library(tidyr)
library(dplyr)

Carpeta <- "F:/LFS/"                                                                              # Carpeta con bases originales
CarpetaRdos <- "C:/Users/facun/Documents/Investigación/1. LFS 2020/Resultados/"                   # Carpeta de resultados
CarpetaBasesUnificadas <- "C:/Users/facun/Documents/Investigación/1. LFS 2020/BasesUnificadas/"   # Carpeta con bases por país unificadas para todos los años


# Base <- readRDS(paste0(CarpetaBasesUnificadas, "SeleccionPaises.2014.Rda"))


# Script para usar con la base unificada
  
NivelEduc <-  Base                                                   %>%
  filter(EDUC!=0)                                                    %>%
  group_by(COUNTRY, EDUC)                                            %>%
  summarise("Desocupados"  = sum(WEIGHT[COND==2], na.rm=TRUE), 
            "Ocupados"     = sum(WEIGHT[COND==1], na.rm=TRUE),
            "Tasa Desocup" = Desocupados/Ocupados)

  
Tama      <-  Base                                       %>%
  filter(COND==1 & TAMA!=0)                              %>%
  group_by(COUNTRY, TAMA)                                %>%
  summarise("Casos"      = n(),
            "Ponderados" = sum(WEIGHT, na.rm=TRUE))

Calif     <-  Base                                       %>%
  filter(COND==1 & CALIF!=0)                             %>%
  group_by(COUNTRY, CALIF)                               %>%
  summarise("Casos"      = n(),
            "Ponderados" = sum(WEIGHT, na.rm=TRUE))


EducTama      <-  Base                                   %>%
  filter(COND==1 & TAMA!=0 & EDUC!=0)                    %>%
  group_by(COUNTRY, TAMA, EDUC)                          %>%
  summarise("Ocupados" = sum(WEIGHT, na.rm=TRUE))

EducTama <- spread(EducTama, COUNTRY, Ocupados)

CalifTama      <-  Base                                  %>%
  filter(COND==1 & TAMA!=0 & CALIF!=0)                   %>%
  group_by(COUNTRY, TAMA, CALIF)                         %>%
  summarise("Ocupados" = sum(WEIGHT, na.rm=TRUE))
  
#Tasa de precareidad segun perfiles (porcentaje sobre el total de cada perfil)
  
PrecaPorTamayCalif  <-  Base                                       %>%
  filter(COND==1 & STAPRO==3 & TAMA!=0 & CALIF!=0)                 %>%
  group_by(COUNTRY, TAMA, CALIF)                                   %>%
  summarise('Precariedad'        = sum(WEIGHT[PRECA==1], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE))       

PrecaPorTamayEduc  <-  Base                                        %>%
  filter(COND==1 & STAPRO==3 & TAMA!=0 & EDUC!=0)                  %>%
  group_by(COUNTRY, TAMA, EDUC)                                    %>%
  summarise('Precariedad'        = sum(WEIGHT[PRECA==1], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE))

# Tasa de asalarización

AsalPorTamayCalif  <-  Base                                       %>%
  filter(COND==1 & TAMA!=0 & CALIF!=0)                            %>%
  group_by(COUNTRY, TAMA, CALIF)                                  %>%
  summarise('Asalarizacion'        = sum(WEIGHT[STAPRO==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE))

AsalPorTamayEduc  <-  Base                                       %>%
  filter(COND==1 & TAMA!=0 & EDUC!=0)                            %>%
  group_by(COUNTRY, TAMA, EDUC)                                  %>%
  summarise('Asalarizacion'        = sum(WEIGHT[STAPRO==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE))

AsalPorTamayEduc <- spread(AsalPorTamayEduc, COUNTRY, Asalarizacion)

DecilPorTamayCalif  <- Base                                        %>%
  filter(STAPRO==3 & COND==1 & INCDECIL!=99  & TAMA!=0 & CALIF!=0) %>%
  group_by(COUNTRY, TAMA, CALIF)                                   %>%
  summarise('PromedioDecil'= weighted.mean(INCDECIL, WEIGHT))


  
  
  