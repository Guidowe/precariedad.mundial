library(tidyverse)
library(foreign)
###Levanto bases 2019###
# canada <- read.csv("../bases/Canada/pub1219.csv")
#canada<-readRDS(file = "Bases/canada_122019.RDS")

archivos<- list.files("../bases/Canada/")

rutas <- data.frame(
  ruta = list.files("../bases/Canada/",recursive = T))

rutas.base.2019 <- rutas %>% 
  filter(str_detect(ruta,pattern = "19"),
         str_detect(ruta,pattern = "pub"),
         str_detect(ruta,pattern = "csv"))

canada2019 <- data.frame()
canada2019.agregado <- data.frame()
for(base in rutas.base.2019$ruta){


canada<- read.csv(file = paste0('../bases/Canada/',base))






####Canada####
canada.cat<- canada %>% 
  filter(LFSSTAT  %in%  1:2) %>%  #Ocupados
  filter(COWMAIN %in% 2:6) %>% # Asal Spriv y TCP
  # filter(COWMAIN == 2) %>% # Asalariado
  mutate(
    periodo = as.double(paste0(SURVYEAR,SURVMNTH)),
    FACTOR = FINALWT,
    # registracion =  case_when(r419 %in% 1:6 ~ "Si",
    #                           r419 %in% 7:8 ~ "No"),
    registracion = NA,
    UHRSMAIN = UHRSMAIN/10,
    HRLYEARN = HRLYEARN/100,
    ing.mensual =  HRLYEARN*UHRSMAIN*4.25,
    part.time.inv = case_when(UHRSMAIN < 35 & WHYPT  %in%  6:7 ~ "Part Involunt",
                              UHRSMAIN < 35 & !(WHYPT  %in% 6:7) ~ "Part Volunt",
                              UHRSMAIN >= 35 ~ "Full Time"), 
    tiempo.determinado = case_when(
      PERMTEMP == 1 ~ "No",
      PERMTEMP != 1 ~ "Si"),
    grupos.calif =   
      case_when(
        NOC_10 %in% 1:3 ~ "Alta",
        NOC_10 %in% 4:8 ~ "Media",
        NOC_10 %in% 9 ~ "Baja"),
    grupos.tamanio =
      case_when(
        ESTSIZE %in% 1 ~ "Pequeño", # 1 a 20
        ESTSIZE %in% 2 ~ "Mediano", # 20 a 99
        ESTSIZE %in% 3:4  ~ "Grande"),# 100 a 500 y mas 500
    grupos.tamanio =
      case_when(
        COWMAIN %in% 3:6 ~ "Pequeño", 
        TRUE   ~ grupos.tamanio)
    )
  



canada.ocupados.distrib <-  canada.cat %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    ocupados = sum(FACTOR,na.rm = T),
    asalariados = sum(FACTOR[COWMAIN == 2],na.rm = T),
    tcp = sum(FACTOR[COWMAIN != 2],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = ing.mensual,
      w = FACTOR,na.rm = T),
    promedio.ing.oc.prin.tcp=weighted.mean(
      x = ing.mensual[COWMAIN != 2],
      w = FACTOR[COWMAIN != 2],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = ing.mensual[COWMAIN == 2],
      w = FACTOR[COWMAIN == 2],na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.tcp= tcp/sum(tcp))

canada.ocupados.distrib.agregado <-  canada.cat %>% 
#  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(periodo) %>% 
  summarise(
    ocupados = sum(FACTOR,na.rm = T),
    asalariados = sum(FACTOR[COWMAIN == 2],na.rm = T),
    tcp = sum(FACTOR[COWMAIN != 2],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = ing.mensual,
      w = FACTOR,na.rm = T),
    promedio.ing.oc.prin.tcp=weighted.mean(
      x = ing.mensual[COWMAIN != 2],
      w = FACTOR[COWMAIN != 2],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = ing.mensual[COWMAIN == 2],
      w = FACTOR[COWMAIN == 2],na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.tcp= tcp/sum(tcp))

canada.asalariados.tasas <- canada.cat %>% 
  filter(COWMAIN == 2) %>% # Asalariado
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
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
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp = empleo.temporal/(empleo.temporal+
                                        empleo.no.temporal))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".asal"), .cols = 4:ncol(.))

canada.tcp.tasas <- canada.cat %>% 
  filter(COWMAIN != 2) %>% # TCP
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
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
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp = empleo.temporal/(empleo.temporal+
                                   empleo.no.temporal))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".tcp"), .cols = 4:ncol(.))

canada.asalariados.tasas.agregado <- canada.cat %>% 
  filter(COWMAIN == 2) %>% # Asalariado
#  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(periodo) %>% 
  summarise(
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
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp = empleo.temporal/(empleo.temporal+
                                        empleo.no.temporal))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".asal"), .cols = 2:ncol(.))

canada.tcp.tasas.agregado <- canada.cat %>% 
  filter(COWMAIN != 2) %>% # tcp
  #  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(periodo) %>% 
  summarise(
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
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp = empleo.temporal/(empleo.temporal+
                                   empleo.no.temporal))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".tcp"), .cols = 2:ncol(.))

canada.resultado <- canada.ocupados.distrib %>%
  left_join(canada.asalariados.tasas) %>% 
  left_join(canada.tcp.tasas) 

canada.resultado.agregado <- canada.ocupados.distrib.agregado %>%
  left_join(canada.asalariados.tasas.agregado) %>% 
  left_join(canada.tcp.tasas.agregado) 

canada2019 <- bind_rows(canada2019,canada.resultado)
canada2019.agregado <- bind_rows(canada2019.agregado,canada.resultado.agregado)

}

#canada2019 <- readRDS("Resultados/Canada_trimestral.RDS")

canada2019 <- canada2019 %>%
  group_by(grupos.calif,grupos.tamanio) %>% 
  summarise(periodo = 2019, 
            across(.cols = 4:ncol(.)-2,.fns = mean))  %>% 
  ungroup() %>% 
  mutate(Pais = "Canada",
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


canada2019.agregado <- canada2019.agregado %>%
  #group_by(grupos.calif,grupos.tamanio) %>% 
  summarise(periodo = 2019, 
            across(.cols = 2:ncol(.),.fns = mean))  %>% 
  ungroup() %>% 
  mutate(Pais = "Canada") 

saveRDS(canada2019,file = "Resultados/Canada.RDS")  
saveRDS(canada2019.agregado,file = "Resultados/Canada_agregado.RDS")  

