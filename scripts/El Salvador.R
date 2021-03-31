library(tidyverse)
library(foreign)
# elsalvador <- read.spss("../bases/El Salvador/El Salvador 2016.sav",
#                         reencode = "UTF-8",use.value.labels = F,
#                         to.data.frame = T)
# 
# saveRDS(elsalvador,file = "Bases/elsalvador_2016.RDS")
elsalvador<-readRDS(file = "Bases/elsalvador_2016.RDS")

####El Salvador####
# eph::calculate_tabulates(base = elsalvador,
#                          x = "segm",weights = "fac00",add.totals = "row")  
# eph::calculate_tabulates(base = elsalvador,
#                          x = "r421",weights = "fac00",add.totals = "row")  
# table(elsalvador$r420)
# table(elsalvador$r418)
# table(elsalvador$actpr2012)
# table(elsalvador$area)

el.salvador.cat<- elsalvador %>% 
  filter(area == 1) %>%  #Urbano
  filter(actpr2012 == 10) %>%  #Ocupados
  filter(r418 != 9,r420 == 1) %>% # Spriv s/ serv domestico
  # filter(r418 == 6:7) %>% # Asalariado
  mutate(
    periodo = edicion,
    FACTOR = fac00,
    horas.semana = r412a + r412d,
    seguridad.social =  case_when(r422a %in%  1:2 ~ "Si",
                                  r422a == 3 ~ "No"),
    registracion =  case_when(r419 %in% 1:6 ~ "Si",
                              r419 %in% 7:8 ~ "No"),
    part.time.inv = case_when(horas.semana < 35 & r413  %in%  2:4 ~ "Part Involunt",
                              horas.semana < 35 & !(r413  %in% 2:4) ~ "Part Volunt",
                              horas.semana >= 35 ~ "Full Time"), 
    tiempo.determinado = case_when(
      r419 == 1 ~ "No",
      r419 != 1 ~ "Si"),
    grupos.calif =   
      case_when(
        ciuo414 %in% 1:3 ~ "Alta",
        ciuo414 %in% 4:8 ~ "Media",
        ciuo414 %in% 9 ~ "Baja"),
    grupos.tamanio =
      case_when(
        r421 %in% 1:9 ~ "Pequeño", # 1 a 9
        r421 %in% 10:50 ~ "Mediano", # 10 a 50
        r421 %in% 51:997  ~ "Grande"
      )
  ) 


# unique(el.salvador.cat$part.time.inv)
# table(el.salvador.cat$r413)
#summary(guatemala.cat$horas.semana)

elsa.ocupados.distrib <-  el.salvador.cat  %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    ocupados = sum(FACTOR,na.rm = T),
    asalariados = sum(FACTOR[r418 %in%  6:7],na.rm = T),
    no.asalariados = sum(FACTOR[!(r418 %in%  6:7)],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = money,
      w = FACTOR,na.rm = T),
    promedio.ing.oc.prin.noasal=weighted.mean(
      x = money[!(r418 %in%  6:7)],
      w = FACTOR[!(r418 %in%  6:7)],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = money[r418 %in%  6:7],
      w = FACTOR[r418 %in%  6:7],na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.no.asal= no.asalariados/sum(no.asalariados))



elsa.asalariados.tasas <- el.salvador.cat %>% 
  filter(r418 %in%  6:7) %>% # Asalariado
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

elsa.resultado <- elsa.ocupados.distrib %>%
  left_join(elsa.asalariados.tasas)%>% 
  mutate(Pais = "El Salvador",
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

saveRDS(elsa.resultado,file = "Resultados/El Salvador.RDS")  



