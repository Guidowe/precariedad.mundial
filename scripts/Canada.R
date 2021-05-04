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
for(base in rutas.base.2019$ruta){


canada<- read.csv(file = paste0('../bases/Canada/',base))






####Canada####
canada.cat<- canada %>% 
  filter(LFSSTAT  %in%  1:2) %>%  #Ocupados
  filter(COWMAIN != 1) %>% # Spriv 
  # filter(COWMAIN == 2) %>% # Asalariado
  mutate(
    periodo = as.double(paste0(SURVYEAR,SURVMNTH)),
    FACTOR = FINALWT,
    # registracion =  case_when(r419 %in% 1:6 ~ "Si",
    #                           r419 %in% 7:8 ~ "No"),
    registracion = NA,
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
        ESTSIZE %in% 3:4  ~ "Grande"# 100 a 500 y mas 500
      )
  ) 



canada.ocupados.distrib <-  canada.cat %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    ocupados = sum(FACTOR,na.rm = T),
    asalariados = sum(FACTOR[COWMAIN == 2],na.rm = T),
    no.asalariados = sum(FACTOR[COWMAIN != 2],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = ing.mensual,
      w = FACTOR,na.rm = T),
    promedio.ing.oc.prin.noasal=weighted.mean(
      x = ing.mensual[COWMAIN != 2],
      w = FACTOR[COWMAIN != 2],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = ing.mensual[COWMAIN == 2],
      w = FACTOR[COWMAIN == 2],na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.no.asal= no.asalariados/sum(no.asalariados))



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
    tasa.partime.asal = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp.asal = empleo.temporal/(empleo.temporal+
                                        empleo.no.temporal))

canada.resultado <- canada.ocupados.distrib %>%
  left_join(canada.asalariados.tasas) 


canada2019 <- bind_rows(canada2019,canada.resultado)

}

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

saveRDS(canada2019,file = "Resultados/Canada.RDS")  

