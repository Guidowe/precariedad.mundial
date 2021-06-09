library(tidyverse)
library(foreign)

# guatemala1.2019<- read.spss('../bases/Guatemala/2019 ENEI 1 Mayo - Personas.sav',
#                       reencode = "UTF-8",use.value.labels = F,
#                       to.data.frame = T) %>%
#   mutate(periodo = 201901)
# guatemala2.2019<- read.spss('../bases/Guatemala/2019 ENEI 2 Noviembre - Personas.sav',
#                       reencode = "UTF-8",use.value.labels = F,
#                       to.data.frame = T) %>%
#   mutate(periodo = 201902) %>%
#   mutate(AREA = .REA)
# 
# guatemala <- bind_rows(guatemala1.2019,guatemala2.2019)
# saveRDS(guatemala, "Bases/guatemala_2019.RDS")
guatemala<-readRDS(file = "Bases/guatemala_2019.RDS")

####Guatemala####
# table(guatemala$P04C02B_1D)
# table(guatemala$P04C05)
# table(guatemala$AREA)
# table(guatemala$P04C06,useNA = "always")
#table(guatemala$OCUPADOS,useNA = "always")

guatemala.cat <- guatemala %>% 
#  mutate(FACTOR = Factor_expansion) %>% 
  filter(AREA == 1) %>%  #area urbana
  filter(OCUPADOS == 1) %>%  #Ocupados
  filter(P04C06 %in% c(2,5)) %>% # Spriv s/ serv domestico, Asal y TCP (no agrícola)
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
        P04C05 >51  ~ "Grande"),
    grupos.tamanio = case_when(P04C06 == 5 ~ "Pequeño",
                               TRUE ~ grupos.tamanio)
  ) 

#summary(guatemala.cat$horas.semana)

guate.ocupados.distrib <-  guatemala.cat  %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    ocupados = sum(FACTOR,na.rm = T),
    asalariados = sum(FACTOR[P04C06 == 2 ],na.rm = T),
    tcp = sum(FACTOR[P04C06 != 2],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = P04C10,
      w = FACTOR,na.rm = T),
    promedio.ing.oc.prin.tcp=weighted.mean(
      x = P04C22[P04C06 != 2],
      w = FACTOR[P04C06 != 2],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = P04C10[P04C06 == 2 ],
      w = FACTOR[P04C06 == 2],na.rm = T)
  ) %>% 
  group_by(periodo) %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.tcp= tcp/sum(tcp)) %>% 
  ungroup()


guate.ocupados.distrib.agregado <-  guatemala.cat  %>% 
  group_by(periodo) %>% 
  summarise(
    ocupados = sum(FACTOR,na.rm = T),
    asalariados = sum(FACTOR[P04C06 == 2 ],na.rm = T),
    tcp = sum(FACTOR[P04C06 != 2],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean( ##Erronea, o 1 u otra variable###
      x = P04C10,
      w = FACTOR,na.rm = T),
    promedio.ing.oc.prin.tcp=weighted.mean(
      x = P04C22[P04C06 != 2],
      w = FACTOR[P04C06 != 2],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = P04C10[P04C06 == 2 ],
      w = FACTOR[P04C06 == 2],na.rm = T)
  ) %>% 
  group_by(periodo) %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.tcp = tcp/sum(tcp)) %>% 
  ungroup()


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
    tasa.partime = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp = empleo.temporal/(empleo.temporal+
                                        empleo.no.temporal)) %>% 
    ungroup() %>% 
    rename_with(~str_c(.,".asal"), .cols = 4:ncol(.))

guat.tcp.tasas <- guatemala.cat %>% 
  filter(P04C06 != 2) %>% # TCP
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
    tasa.partime = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp = empleo.temporal/(empleo.temporal+
                                        empleo.no.temporal))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".tcp"), .cols = 4:ncol(.))


guat.asalariados.tasas.agregado <- guatemala.cat %>% 
  filter(P04C06 == 2) %>% # Asalariado
  group_by(periodo) %>% 
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
    tasa.partime = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp = empleo.temporal/(empleo.temporal+
                                        empleo.no.temporal)) %>% 
  rename_with(~str_c(.,".asal"), .cols = 2:ncol(.))


guat.tcp.tasas.agregado <- guatemala.cat %>% 
  filter(P04C06 != 2) %>% # TCP
  group_by(periodo) %>% 
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
    tasa.partime = part.involun/(part.involun+
                                   part.volunt+
                                   full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp = empleo.temporal/(empleo.temporal+
                                   empleo.no.temporal)) %>% 
  rename_with(~str_c(.,".tcp"), .cols = 2:ncol(.))


guate.resultado <- guate.ocupados.distrib %>%
  left_join(guat.asalariados.tasas) %>% 
  left_join(guat.tcp.tasas) %>% 
  ungroup() %>% 
  group_by(grupos.calif,grupos.tamanio) %>% 
  summarise(periodo = 2019, 
            across(.cols = 4:ncol(.)-2,
                   .fns = mean,na.rm = TRUE))  %>% 
  ungroup() %>% 
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

guate.resultado.agregado <- guate.ocupados.distrib.agregado %>%
  left_join(guat.asalariados.tasas.agregado) %>% 
  left_join(guat.tcp.tasas.agregado) %>% 
  summarise(periodo = 2019, 
            across(.cols = 2:ncol(.),.fns = mean))  %>% 
  ungroup() %>% 
  mutate(Pais = "Guatemala")

saveRDS(guate.resultado,file = "Resultados/Guatemala.RDS")  
saveRDS(guate.resultado.agregado,file = "Resultados/Guatemala_agregado.RDS")  




