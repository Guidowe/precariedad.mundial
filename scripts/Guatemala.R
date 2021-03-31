library(tidyverse)
library(foreign)
# guatemala<- read.spss('../bases/Guatemala/2017 guatemala.sav',
#                       reencode = "UTF-8",use.value.labels = F,
#                       to.data.frame = T) %>% 
#   mutate(periodo = 2017)
#saveRDS(guatemala,file = "Bases/guatemala_2017.RDS")
guatemala<-readRDS(file = "Bases/guatemala_2017.RDS")

####Guatemala####
# table(guatemala$P04C02B_1D)
# table(guatemala$P04C05)
# table(guatemala$AREA)
# table(guatemala$P04C06,useNA = "always")
#table(guatemala$OCUPADOS,useNA = "always")

guatemala.cat <- guatemala %>% 
  mutate(FACTOR = Factor_expansion) %>% 
  filter(AREA == 1) %>%  #area urbana
  filter(OCUPADOS == 1) %>%  #Ocupados
  filter(!(P04C06 %in% c(1,4))) %>% # Spriv s/ serv domestico
  # filter(P04C06 == 2) %>% # Asalariado
  mutate(
    horas.semana = P04C28A+P04C28B+P04C28C+P04C28D+P04C28E+P04C28F+P04C28G,
    seguridad.social =  case_when(P04C25A %in%  1:3 ~ "Si",
                                  P04C25A == 4 ~ "No"),
    registracion =  case_when(P04C07 == 1 ~ "Si",
                              P04C07 == 2 ~ "No"),
    part.time.inv = case_when(horas.semana < 35 & P04C29  %in%  2:4 ~ "Part Involunt",
                              horas.semana < 35 & !(P04C29  %in% 2:4) ~ "Part Volunt",
                              horas.semana >= 35 ~ "Full Time"), 
    tiempo.determinado = case_when(
      P04C08A == 2 ~ "No",
      P04C08A == 1 ~ "Si"),
    grupos.calif =   
      case_when(
        P04C02B_1D %in% 1:3 ~ "Alta",
        P04C02B_1D %in% 4:8 ~ "Media",
        P04C02B_1D %in% 9 ~ "Baja"),
    grupos.tamanio =
      case_when(
        P04C05 %in% 1:9 ~ "Pequeño", # 1 a 9
        P04C05 %in% 10:50 ~ "Mediano", # 10 a 50
        P04C05 >51  ~ "Grande")
  ) 

#summary(guatemala.cat$horas.semana)

guate.ocupados.distrib <-  guatemala.cat  %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    ocupados = sum(FACTOR,na.rm = T),
    asalariados = sum(FACTOR[P04C06 == 2 ],na.rm = T),
    no.asalariados = sum(FACTOR[P04C06 != 2],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = P04C10,
      w = FACTOR,na.rm = T),
    promedio.ing.oc.prin.noasal=weighted.mean(
      x = P04C10[P04C06 != 2],
      w = FACTOR[P04C06 != 2],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = P04C10[P04C06 == 2 ],
      w = FACTOR[P04C06 == 2],na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.no.asal= no.asalariados/sum(no.asalariados))

guat.asalariados.tasas <- guatemala.cat %>% 
  filter(P04C06 == 2) %>% # Asalariado
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    seguridad.social.si = sum(FACTOR[seguridad.social=="Si"],na.rm = T),
    seguridad.social.no = sum(FACTOR[seguridad.social=="No"],na.rm = T),
    registrados =sum(FACTOR[registracion=="Si"],na.rm = T),
    no.registrados =sum(FACTOR[registracion=="No"],na.rm = T),
    empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(FACTOR[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(FACTOR[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(FACTOR[part.time.inv=="Full Time"],na.rm = T),
    tasa.partime.asal = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp.asal = empleo.temporal/(empleo.temporal+
                                        empleo.no.temporal))

guate.resultado <- guate.ocupados.distrib %>%
  left_join(guat.asalariados.tasas )%>% 
  mutate(Pais = "Guatemala",
         tamanio.calif = paste0(grupos.tamanio," - ",grupos.calif),
         tamanio.calif = factor(tamanio.calif,
                                levels = 
                                  c("Pequeño - Baja",
                                    "Pequeño - Media",
                                    "Pequeño - Alta",
                                    "Mediano - Baja",
                                    "Mediano - Media", 
                                    "Mediano - Alta",
                                    "Grande - Baja",
                                    "Grande - Media",
                                    "Grande - Alta"))) %>% 
  arrange(tamanio.calif)

saveRDS(guate.resultado,file = "Resultados/Guatemala.RDS")  




