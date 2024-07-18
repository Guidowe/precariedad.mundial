library(tidyverse)
library(foreign)
# elsalvador <- read.spss("../bases/El Salvador/El Salvador 2016.sav",
#                         reencode = "UTF-8",use.value.labels = F,
#                         to.data.frame = T)
# 
# saveRDS(elsalvador,file = "Bases/elsalvador_2016.RDS")

elsalvador <- read.spss("../bases/El Salvador/EHPM 2019.sav",
                        reencode = "UTF-8",use.value.labels = F,
                        to.data.frame = T)
#elsalvador<-readRDS(file = "Bases/elsalvador_2016.RDS")

####El Salvador####
# eph::calculate_tabulates(base = elsalvador,
#                          x = "segm",weights = "fac00",add.totals = "row")  
# eph::calculate_tabulates(base = elsalvador,
#                          x = "r421",weights = "fac00",add.totals = "row")  
# table(elsalvador$r420)
 table(elsalvador$r418)

# table(elsalvador$actpr2012)
# table(elsalvador$area)
#table(elsalvador$r420,elsalvador$r418) #La pregunta de sector solo se hace a asalariados
##Base Homog ####
base_homog<- elsalvador %>% 
  filter(area == 1) %>%  #Urbano
  filter(actpr2012 == 10) %>%  #Ocupados
#  filter(r418  %in%  c(2,3,6,7,8)) %>% #  TCP y asal s/ serv domestico
#  filter(!((r418 %in%  6:7) & (r420 != 1))) %>% #  Saco S.Pub para asal
  # filter(r418 == 6:7) %>% # Asalariado
  mutate(
    ING = money,
    WEIGHT = fac00,
    PERIODO = 2019,
    CATOCUP = case_when(r418 %in% 6:9~ "Asalariados",
                        r418 %in% c(2,3)~ "Cuenta Propia",
                        TRUE ~ "Resto"),
    SECTOR = case_when(r420 %in% 2~ "Pub",
                       r420 %in% 1 & r418!=9  ~ "Priv",
                       r418 == 9 ~ "SD"),
    PRECASALUD = NA,
    COND = "Ocupado",
    PAIS = "El Salvador",
    ANO = 2019,
    SEXO = case_when(r104 == 1 ~ "Varon",
                     r104 == 2 ~ "Mujer"),
    EDAD = r106,
    EDUC = case_when(r215a %in% c(8,0:2) ~ "Primaria",
                     r217 %in% c(3) ~ "Secundaria",
                     r217 %in% c(4:5) ~ "Terciaria"),
    periodo = edicion,
    FACTOR = fac00,
    horas.semana = r412a + r412d,
    PRECASEG =  case_when(r422a %in%  1:2 ~ 0,
                                  r422a == 3 ~ 1),
    PRECAREG =  case_when(r419 %in% 1:6 ~ 0,
                              r419 %in% 7 ~ 1),
    part.time.inv = case_when(horas.semana < 35 & r413  %in%  2:4 ~ "Part Involunt",
                              horas.semana < 35 & !(r413  %in% 2:4) ~ "Part Volunt",
                              horas.semana >= 35 ~ "Full Time"), 
    PRECAPT = case_when(part.time.inv == "Part Involunt"~1,
                        part.time.inv %in%  c("Part Volunt","Full Time")~0),
    PRECATEMP = case_when(
      r419 == 1 ~ 0,
      r419 %in% 2:7 ~ 1),
    CALIF =   
      case_when(
        ciuo414 %in% 1:3 ~ "Alta",
        ciuo414 %in% 4:8 ~ "Media",
        ciuo414 %in% 9 ~ "Baja"),
    TAMA =
      case_when(
        r421 %in% 1:9 ~ "Pequeño", # 1 a 9
        r421 %in% 10:50 ~ "Mediano", # 10 a 50
        r421 %in% 51:997  ~ "Grande"),
    TAMA = 
      case_when(r418 %in% 2:3 ~ "Pequeño",
                TRUE ~ TAMA)
  )

variables<- c("PAIS","ANO","PERIODO","WEIGHT","SEXO","EDAD",
              "CATOCUP","COND","SECTOR","PRECAPT","EDUC",
              "PRECAREG","PRECATEMP","PRECASALUD","PRECASEG","TAMA","CALIF","ING") 

base_homog_final <- base_homog %>% 
  select(all_of(variables))

saveRDS(base_homog_final,file = "bases_homog/el_salvador.rds")

##Base cat####
el.salvador.cat<- elsalvador %>% 
  filter(area == 1) %>%  #Urbano
  filter(actpr2012 == 10) %>%  #Ocupados
  filter(r418  %in%  c(2,3,6,7,8)) %>% #  TCP y asal s/ serv domestico
  filter(!((r418 %in%  6:7) & (r420 != 1))) %>% #  Saco S.Pub para asal
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
        r421 %in% 51:997  ~ "Grande"),
    grupos.tamanio = 
      case_when(r418 %in% 2:3 ~ "Pequeño",
                TRUE ~ grupos.tamanio)
      )
   


# unique(el.salvador.cat$part.time.inv)
# table(el.salvador.cat$r413)
#summary(guatemala.cat$horas.semana)

elsa.ocupados.distrib <-  el.salvador.cat  %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    total.casos = n(),
    total.asalariados = sum(r418 %in%  6:8),
    ocupados = sum(FACTOR,na.rm = T),
    asalariados = sum(FACTOR[r418 %in%  6:8],na.rm = T),
    tcp = sum(FACTOR[!(r418 %in%  6:8)],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = money,
      w = FACTOR,na.rm = T),
    promedio.ing.oc.prin.tcp=weighted.mean(
      x = money[!(r418 %in%  6:8)],
      w = FACTOR[!(r418 %in%  6:8)],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = money[r418 %in%  6:8],
      w = FACTOR[r418 %in%  6:8],na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.tcp = tcp/sum(tcp))

elsa.ocupados.distrib.agregado <-  el.salvador.cat  %>% 
  group_by(periodo) %>% 
  summarise(
    ocupados = sum(FACTOR,na.rm = T),
    asalariados = sum(FACTOR[r418 %in%  6:8],na.rm = T),
    tcp = sum(FACTOR[!(r418 %in%  6:8)],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = money,
      w = FACTOR,na.rm = T),
    promedio.ing.oc.prin.tcp=weighted.mean(
      x = money[!(r418 %in%  6:8)],
      w = FACTOR[!(r418 %in%  6:8)],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = money[r418 %in%  6:8],
      w = FACTOR[r418 %in%  6:8],na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.tcp= tcp/sum(tcp))


elsa.asalariados.tasas <- el.salvador.cat %>% 
  filter(r418 %in%  6:8) %>% # Asalariado
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
  rename_with(~str_c(.,".asal"), .cols = 4:ncol(.))


elsa.tcp.tasas <- el.salvador.cat %>% 
  filter(!(r418 %in%  6:8)) %>% # TCP
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


elsa.asalariados.tasas.agregado <- el.salvador.cat %>% 
  filter(r418 %in%  6:8) %>% # Asalariado
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
                                        empleo.no.temporal))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".asal"), .cols = 2:ncol(.))


elsa.tcp.tasas.agregado <- el.salvador.cat %>% 
  filter(!(r418 %in%  6:8)) %>% # TCP
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
                                   empleo.no.temporal))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".tcp"), .cols = 2:ncol(.))

elsa.resultado <- elsa.ocupados.distrib %>%
  left_join(elsa.asalariados.tasas)%>% 
  left_join(elsa.tcp.tasas)%>% 
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

elsa.resultado.agregado <- elsa.ocupados.distrib.agregado %>%
  left_join(elsa.asalariados.tasas.agregado)%>% 
  left_join(elsa.tcp.tasas.agregado)%>% 
  mutate(Pais = "El Salvador")

saveRDS(elsa.resultado,file = "Resultados/El Salvador.RDS")  
saveRDS(elsa.resultado.agregado,file = "Resultados/El Salvador_agregado.RDS")  



