library(tidyverse)
library(ipumsr)
library(eph)
cps_ddi_file <- "data/cps_00002.xml"
cps_data_file <- "data/cps_00002.dat"
cps_ddi <- read_ipums_ddi(cps_ddi_file) # Contains metadata, nice to have as separate object
Base <- ipumsr::read_ipums_micro(ddi = cps_ddi_file,
                                 data_file =  cps_data_file)

####Variables y categorias####
listado.variables <- cps_ddi[["var_info"]]
ipums_val_labels(cps_ddi, var = FIRMSIZE)
educacion <- ipums_val_labels(cps_ddi, var = "EDUC")
ipums_val_labels(cps_ddi, var = OCCLY)
cod_ocupacion <- ipums_val_labels(cps_ddi, var = "OCC")
ipums_val_labels(cps_ddi, var = "EMPSTAT")
ipums_val_labels(cps_ddi, var = "WHYPTLWK")

####Descripcion variables####
listado.variables$var_desc[listado.variables$var_name=="FIRMSIZE"]
#NIU (not in universe)

####Procesamiento####

Enc_anual <- Base %>% 
  filter(ASECFLAG ==1) %>% 
  mutate(tamaño.estrat = case_when(FIRMSIZE==1~"1-10",
                                   FIRMSIZE %in% 2:4~"10-49",
                                   FIRMSIZE %in% 5:9~"50 +",
                                   FIRMSIZE %in% 0 ~ "ns/nr"),
         educacion = factor(case_when(EDUC %in% 2:72~ "bajo",
                               EDUC %in% 73:110~ "medio",
                               EDUC %in% 111:125~ "alto",
                               TRUE ~ "ns/nr"),
                            levels = c("bajo","medio","alto","ns/nr")))

Desempleo <- Enc_anual %>% 
  group_by(YEAR,educacion) %>% 
  summarise(PEA = sum(ASECWT[EMPSTAT %in% 1:22],na.rm = TRUE),
            desocupados = sum(ASECWT[EMPSTAT %in% 20:22],na.rm = TRUE),
            tasa.desocup = desocupados/PEA)

grupos <- Enc_anual %>% 
  filter(educacion != "ns/nr",tamaño.estrat != "ns/nr") %>% 
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
