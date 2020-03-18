library(tidyverse)
library(ipumsr)
library(eph)
cps_ddi_file <- "cps_00002.xml"
cps_data_file <- "cps_00002.dat"
cps_ddi <- read_ipums_ddi(cps_ddi_file) # Contains metadata, nice to have as separate object
Base <- ipumsr::read_ipums_micro(ddi = cps_ddi_file,data_file =  cps_data_file)
Ver <- cps_ddi[["var_info"]]

tamanios.cod <- ipums_val_labels(cps_ddi, var = FIRMSIZE)
educacion.cod <- ipums_val_labels(cps_ddi, var = EDUC)
labforce.cod <- ipums_val_labels(cps_ddi, var = LABFORCE)
stat.cod <- ipums_val_labels(cps_ddi, var = EMPSTAT)
workly.cod <- ipums_val_labels(cps_ddi, var = WORKLY)

####Ver la descripción de una variable####
Ver$var_desc[Ver$var_name == "CLASSWLY"]

colapse 
paste0(tamanios.cod$lbl,collapse = " / ")
####Total ocupados en Marzo 2019 y salario promedio####
 # Ocupados  <- Base %>% 
 # filter(ASECFLAG ==1,WORKLY ==2) %>% 
 # summarise(casos = n(),
 #          casos.pond = sum(ASECWT),
 #          salario.anual.prom = weighted.mean(INCWAGE,ASECWT),
 #          salario.anual.mediana = median(INCWAGE),
 #          salario.mensual.mediana = median(INCWAGE)/12,
 #          salario.mensual.prom = salario.anual.prom/12,
 #          horas.prom = weighted.mean(UHRSWORKLY,ASECWT))


#NIU (not in universe)
Enc_anual <- Base %>% 
  filter(ASECFLAG ==1,WORKLY ==2) %>% 
  mutate(tamaño.estrat = case_when(FIRMSIZE==1~"1-10",
                                   FIRMSIZE %in% 2:4~"10-49",
                                   FIRMSIZE %in% 5:9~"50 +",
                                   FIRMSIZE %in% 0 ~ "ns/nr"),
         educacion = case_when(EDUC %in% 2:72~ "baja",
                               EDUC %in% 73:110~ "media",
                               EDUC %in% 111:125~ "alta",
                               TRUE ~ "ns/nr"))


grupos <- Enc_anual %>% 
  group_by(educacion,tamaño.estrat) %>% 
  summarise(casos = n(),
            casos.pond = sum(ASECWT),
            salario.anual.prom = weighted.mean(INCWAGE,ASECWT),
            salario.mensual.prom = salario.anual.prom/12,
            salario.anual.mediana = median(INCWAGE),
            salario.mensual.mediana = median(INCWAGE)/12,
            horas.prom = weighted.mean(UHRSWORKLY,ASECWT)) %>% 
  arrange(salario.anual.prom)

openxlsx::write.xlsx(grupos,file = "Resultados/indicadores_grupos.xlsx")
