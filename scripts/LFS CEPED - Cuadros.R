library(tidyverse)
library(dplyr)
library(xlsx)

Carpeta <- "F:/LFS/"                                                                    
CarpetaRdos <- "C:/Users/facun/Documents/Investigación/1. LFS 2020 - CEPED Precariedad mundial/Resultados/"
CarpetaBasesUnificadas <- "F:/LFS/BasesUnificadas/" 

                    ### CALCULO PARA SELECCION DE PAISES 2018 ####

Base <- readRDS(paste0(CarpetaBasesUnificadas, "SeleccionPaises2018.Rda"))
## Sacar Sector público y empleo doméstico
Base <- Base           %>%
  filter(NACE1D!="O")  %>%
  filter(NACE1D!="T")

## Cuadro Ocupados por Calificacion
OcupadosCalif <- Base                                 %>%
  filter(COND=="Ocupado" & CALIF!="Ns/Nc" & TAMA!="Ns/Nc")   %>%
  group_by(COUNTRY, TAMA, CALIF)                             %>%
  summarise('Total'                  = sum(WEIGHT, na.rm=TRUE),
            'tasa.asalarizacion'          = sum(WEIGHT[STAPRO==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'tasa.part.invol' = sum(WEIGHT[PRECAPT==1], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'tasa.cooc.part.invol.temp'       = sum(WEIGHT[PRECAPT==1 & PRECATEMP==1], na.rm=TRUE)/ sum(WEIGHT[PRECAPT==1], na.rm=TRUE), 
            'tasa.cooc.part.invol.subocup.invol'   = sum(WEIGHT[PRECAPT==1 & PRECAHORA==1], na.rm=TRUE)/ sum(WEIGHT[PRECAPT==1], na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(COUNTRY) %>%
  mutate('Particip_emp'          = Total/sum(Total))
#Saco el valor NaN de Rumania
OcupadosCalif$`tasa.cooc.part.invol.temp`[is.nan(OcupadosCalif$`tasa.cooc.part.invol.temp`)]<-0
OcupadosCalif$`tasa.cooc.part.invol.subocup.invol`[is.nan(OcupadosCalif$`tasa.cooc.part.invol.subocup.invol`)]<-0
# Le doy formato para igualar a Fantom
OcupadosCalif <- OcupadosCalif          %>%
  mutate(Pais=substr(COUNTRY, 1, 2), 
         ANO4=substr(COUNTRY, 3, 7))    %>% 
  ungroup()                             %>% 
  select(-COUNTRY)
colnames(OcupadosCalif)[1] <- "grupos.tamanio"
colnames(OcupadosCalif)[2] <- "grupos.calif"
OcupadosCalif <- OcupadosCalif[,c(10, 9, 1, 2, 3, 4, 5, 6, 7, 8)]

## Cuadro Ocupados por Educacion

OcupadosEduc <- Base                                 %>%
  filter(COND=="Ocupado" & EDUC!="Ns/Nc" & TAMA!="Ns/Nc")   %>%
  group_by(COUNTRY, TAMA, EDUC)                             %>%
  summarise('Total'                  = sum(WEIGHT, na.rm=TRUE),
            'tasa.asalarizacion'          = sum(WEIGHT[STAPRO==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'tasa.part.invol' = sum(WEIGHT[PRECAPT==1], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'tasa.cooc.part.invol.temp'       = sum(WEIGHT[PRECAPT==1 & PRECATEMP==1], na.rm=TRUE)/ sum(WEIGHT[PRECAPT==1], na.rm=TRUE), 
            'tasa.cooc.part.invol.subocup.invol'   = sum(WEIGHT[PRECAPT==1 & PRECAHORA==1], na.rm=TRUE)/ sum(WEIGHT[PRECAPT==1], na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(COUNTRY) %>%
  mutate('Particip_emp'          = Total/sum(Total))
#Saco el valor NaN de Rumania
OcupadosEduc$`tasa.cooc.part.invol.temp`[is.nan(OcupadosEduc$`tasa.cooc.part.invol.temp`)]<-0
OcupadosEduc$`tasa.cooc.part.invol.subocup.invol`[is.nan(OcupadosEduc$`tasa.cooc.part.invol.subocup.invol`)]<-0
# Le doy formato para igualar a Fantom
OcupadosEduc <- OcupadosEduc          %>%
  mutate(Pais=substr(COUNTRY, 1, 2), 
         ANO4=substr(COUNTRY, 3, 7))    %>% 
  ungroup()                             %>% 
  select(-COUNTRY)
colnames(OcupadosEduc)[1] <- "grupos.tamanio"
colnames(OcupadosEduc)[2] <- "grupos.nivel.ed"
OcupadosEduc <- OcupadosEduc[,c(10, 9, 1, 2, 3, 4, 5, 6, 7, 8)]

## Cuadro Asalariados por Calificacion

AsalaCalif <- Base                                 %>%
  filter(COND=="Ocupado" & STAPRO==3 & CALIF!="Ns/Nc" & TAMA!="Ns/Nc")   %>%
  group_by(COUNTRY, TAMA, CALIF)                             %>%
  summarise('Total'                  = sum(WEIGHT, na.rm=TRUE),
            'PromedioDecil'   = weighted.mean(INCDECIL, WEIGHT, na.rm=TRUE),
            'Salario.prom.SES'= weighted.mean(salario, WEIGHT, na.rm = TRUE),
            'Salario.horario.prom.SES'= weighted.mean(salariohorario, WEIGHT, na.rm = TRUE), 
            'tasa.part.invol' = sum(WEIGHT[PRECAPT==1], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'tasa.cooc.part.invol.temp'       = sum(WEIGHT[PRECAPT==1 & PRECATEMP==1], na.rm=TRUE)/ sum(WEIGHT[PRECAPT==1], na.rm=TRUE), 
            'tasa.cooc.part.invol.subocup.invol'   = sum(WEIGHT[PRECAPT==1 & PRECAHORA==1], na.rm=TRUE)/ sum(WEIGHT[PRECAPT==1], na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(COUNTRY) %>%
  mutate('Particip_emp'          = Total/sum(Total))
#Saco el valor NaN de Rumania
AsalaCalif$`tasa.cooc.part.invol.temp`[is.nan(AsalaCalif$`tasa.cooc.part.invol.temp`)]<-0
AsalaCalif$`tasa.cooc.part.invol.subocup.invol`[is.nan(AsalaCalif$`tasa.cooc.part.invol.subocup.invol`)]<-0
# Le doy formato para igualar a Fantom

AsalaCalif <- AsalaCalif          %>%
  mutate(Pais=substr(COUNTRY, 1, 2), 
         ANO4=substr(COUNTRY, 3, 7))    %>% 
  ungroup()                             %>% 
  select(-COUNTRY)
colnames(AsalaCalif)[1] <- "grupos.tamanio"
colnames(AsalaCalif)[2] <- "grupos.calif"
AsalaCalif <- AsalaCalif[,c(12, 11, 1, 2, 3, 10, 4, 5, 6, 7, 8, 9)]

## Cuadro Asalariados por Educacion

AsalaEduc <- Base                                 %>%
  filter(COND=="Ocupado" & STAPRO==3 & EDUC!="Ns/Nc" & TAMA!="Ns/Nc")   %>%
  group_by(COUNTRY, TAMA, EDUC)                             %>%
  summarise('Total'                  = sum(WEIGHT, na.rm=TRUE),
            'PromedioDecil'= weighted.mean(INCDECIL, WEIGHT, na.rm=TRUE),
            'Salario.prom.SES'= weighted.mean(salario, WEIGHT, na.rm = TRUE),
            'Salario.horario.prom.SES'= weighted.mean(salariohorario, WEIGHT, na.rm = TRUE), 
            'tasa.part.invol' = sum(WEIGHT[PRECAPT==1], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'tasa.cooc.part.invol.temp'       = sum(WEIGHT[PRECAPT==1 & PRECATEMP==1], na.rm=TRUE)/ sum(WEIGHT[PRECAPT==1], na.rm=TRUE), 
            'tasa.cooc.part.invol.subocup.invol'   = sum(WEIGHT[PRECAPT==1 & PRECAHORA==1], na.rm=TRUE)/ sum(WEIGHT[PRECAPT==1], na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(COUNTRY) %>%
  mutate('Particip_emp'          = Total/sum(Total))
#Saco el valor NaN de Rumania
AsalaEduc$`tasa.cooc.part.invol.temp`[is.nan(AsalaEduc$`tasa.cooc.part.invol.temp`)]<-0
AsalaEduc$`tasa.cooc.part.invol.subocup.invol`[is.nan(AsalaEduc$`tasa.cooc.part.invol.subocup.invol`)]<-0
# Le doy formato para igualar a Fantom
AsalaEduc <- AsalaEduc          %>%
  mutate(Pais=substr(COUNTRY, 1, 2), 
         ANO4=substr(COUNTRY, 3, 7))    %>% 
  ungroup()                             %>% 
  select(-COUNTRY)
colnames(AsalaEduc)[1] <- "grupos.tamanio"
colnames(AsalaEduc)[2] <- "grupos.nivel.ed"
AsalaEduc <- AsalaEduc[,c(12, 11, 1, 2, 3, 10, 4, 5, 6, 7, 8, 9)]


## Cuadro Ocupados Educacion-Calificacion

OcupadosEducCalif <- Base                                    %>%
  filter(COND=="Ocupado" & EDUC!="Ns/Nc" & CALIF!="Ns/Nc")   %>%
  group_by(COUNTRY, EDUC)                                    %>%
  summarise('Total'                       = sum(WEIGHT, na.rm=TRUE), 
            'CalifBaja'                  = sum(WEIGHT[CALIF=="Baja"], na.rm=TRUE), 
            'CalifMedia'                 = sum(WEIGHT[CALIF=="Media"], na.rm=TRUE), 
            'CalifAlta'                  = sum(WEIGHT[CALIF=="Alta"], na.rm=TRUE))   %>%
  ungroup() %>%
  group_by(COUNTRY) %>%
  mutate('Calif Baja Part'   = CalifBaja/sum(Total), 
         'Calif Media Part'  = CalifMedia/sum(Total), 
         'Calif Alta Part'   = CalifAlta/sum(Total))

#Lo guardo en Excel
write.xlsx(as.data.frame(OcupadosCalif), paste0(CarpetaRdos, "RestultadosLFS.xlsx"), sheetName = "Ocupados.calif", append = TRUE, row.names = FALSE)
write.xlsx(as.data.frame(OcupadosEduc), paste0(CarpetaRdos, "RestultadosLFS.xlsx"), sheetName = "Ocupados.nivel.ed", append = TRUE, row.names = FALSE)
write.xlsx(as.data.frame(AsalaCalif), paste0(CarpetaRdos, "RestultadosLFS.xlsx"), sheetName = "Asalariados.nivel.ed", append = TRUE, row.names = FALSE)
write.xlsx(as.data.frame(AsalaEduc), paste0(CarpetaRdos, "RestultadosLFS.xlsx"), sheetName = "Asalariados.calif", append = TRUE, row.names = FALSE)
write.xlsx(as.data.frame(OcupadosEducCalif), paste0(CarpetaRdos, "RestultadosLFS.xlsx"), sheetName = "OcupadosEducCalif", append = TRUE, row.names = FALSE)


##Comparación part-time y full-time

PTFTPromedioDeciles <- Base        %>%
  filter(COND=="Ocupado" & STAPRO==3 & CALIF!="Ns/Nc")   %>%
  group_by(COUNTRY, PRECAPT)   %>%
  summarise('PromedioDecil'= weighted.mean(INCDECIL, WEIGHT, na.rm=TRUE))

PTFTPromedioDecilesCalif <- Base        %>%
  filter(COND=="Ocupado" & STAPRO==3 & CALIF!="Ns/Nc")   %>%
  group_by(COUNTRY, CALIF, PRECAPT)   %>%
  summarise('PromedioDecil'= weighted.mean(INCDECIL, WEIGHT, na.rm=TRUE))
  
write.xlsx(as.data.frame(PTFTPromedioDeciles), paste0(CarpetaRdos, "LFSComparacionFTPT.xlsx"), sheetName = "PromedioDeciles", append = TRUE, row.names = FALSE)
write.xlsx(as.data.frame(PTFTPromedioDecilesCalif), paste0(CarpetaRdos, "LFSComparacionFTPT.xlsx"), sheetName = "PromedioDecilesCalif", append = TRUE, row.names = FALSE)


##Desocupacion por calificacion

Desocup.Calif <- Base                               %>%
  filter(COND!="Inactivo" & CALIFANT!="Ns/Nc")       %>%
  group_by(COUNTRY, CALIFANT)                        %>%
  summarise('DesocupadosConCalifAnt' = sum(WEIGHT, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(COUNTRY) %>%
  mutate('Por.Desocup'  = DesocupadosConCalifAnt/sum(DesocupadosConCalifAnt, na.rm=TRUE))%>% 
  rename(CALIF=CALIFANT)

Ocup.Calif <- Base                                 %>%
  filter(COND=="Ocupado" & CALIF!="Ns/Nc")   %>%
  group_by(COUNTRY, CALIF)  %>%
  summarise('Ocupados' = sum(WEIGHT, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(COUNTRY) %>%
  mutate('Por.Ocup'  = Ocupados/sum(Ocupados, na.rm=TRUE))

Desocup.Calif <- inner_join(Desocup.Calif, Ocup.Calif, by=c("COUNTRY", "CALIF")) %>%
  mutate(brecha=Por.Desocup/Por.Ocup)

remove(Ocup.Calif)

write.xlsx(as.data.frame(Desocup.Calif), paste0(CarpetaRdos, "RestultadosLFS.xlsx"), sheetName = "Desocup.Calif", append = TRUE, row.names = FALSE)

                              ### CALCULO DE SERIES 2008-2018 ####  

countries <- c("ES", "FR", "UK", "DE", "GR", "IT", "PT", "DK", "SE", "BG", "RO")

i <- 1

while (i < length(countries) + 1) {

  Base <- readRDS(paste0(CarpetaBasesUnificadas, countries[i], "2008-2018.Rda"))

  ## Sacar Sector público y empleo doméstico
  
  Base <- Base           %>%
    filter(NACE1D!="O")  %>%
    filter(NACE1D!="T")
  
  ## Cuadro Ocupados por Calificacion
  OcupadosCalif <- Base                                 %>%
    filter(COND=="Ocupado" & CALIF!="Ns/Nc" & TAMA!="Ns/Nc")   %>%
    group_by(YEAR, TAMA, CALIF)                             %>%
    summarise('Pais'                                 = countries[i],
              'Total'                                = sum(WEIGHT, na.rm=TRUE),
              'tasa.asalarizacion'                   = sum(WEIGHT[STAPRO==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
              'tasa.part.invol'                      = sum(WEIGHT[PRECAPT==1], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
              'tasa.cooc.part.invol.temp'            = sum(WEIGHT[PRECAPT==1 & PRECATEMP==1], na.rm=TRUE)/ sum(WEIGHT[PRECAPT==1], na.rm=TRUE), 
              'tasa.cooc.part.invol.subocup.invol'   = sum(WEIGHT[PRECAPT==1 & PRECAHORA==1], na.rm=TRUE)/ sum(WEIGHT[PRECAPT==1], na.rm=TRUE)) %>%
    ungroup() %>%
    group_by(YEAR) %>%
    mutate('Particip_emp'          = Total/sum(Total))
  #Saco el valor NaN de Rumania
  OcupadosCalif$`tasa.cooc.part.invol.temp`[is.nan(OcupadosCalif$`tasa.cooc.part.invol.temp`)]<-0
  OcupadosCalif$`tasa.cooc.part.invol.subocup.invol`[is.nan(OcupadosCalif$`tasa.cooc.part.invol.subocup.invol`)]<-0
  # Formato
  colnames(OcupadosCalif)[1] <- "ANO4"
  colnames(OcupadosCalif)[2] <- "grupos.tamanio"
  colnames(OcupadosCalif)[3] <- "grupos.calif"
  OcupadosCalif <-  OcupadosCalif[,c(4, 1, 2, 3, 5:10)]
  
  ## Cuadro Ocupados por Educacion
  
  OcupadosEduc <- Base                                 %>%
    filter(COND=="Ocupado" & EDUC!="Ns/Nc" & TAMA!="Ns/Nc")   %>%
    group_by(YEAR, TAMA, EDUC)                             %>%
    summarise('Pais'                                 = countries[i],
              'Total'                                = sum(WEIGHT, na.rm=TRUE),
              'tasa.asalarizacion'                   = sum(WEIGHT[STAPRO==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
              'tasa.part.invol'                      = sum(WEIGHT[PRECAPT==1], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
              'tasa.cooc.part.invol.temp'            = sum(WEIGHT[PRECAPT==1 & PRECATEMP==1], na.rm=TRUE)/ sum(WEIGHT[PRECAPT==1], na.rm=TRUE), 
              'tasa.cooc.part.invol.subocup.invol'   = sum(WEIGHT[PRECAPT==1 & PRECAHORA==1], na.rm=TRUE)/ sum(WEIGHT[PRECAPT==1], na.rm=TRUE)) %>%
    ungroup() %>%
    group_by(YEAR) %>%
    mutate('Particip_emp'          = Total/sum(Total))
  #Saco el valor NaN de Rumania
  OcupadosEduc$`tasa.cooc.part.invol.temp`[is.nan(OcupadosEduc$`tasa.cooc.part.invol.temp`)]<-0
  OcupadosEduc$`tasa.cooc.part.invol.subocup.invol`[is.nan(OcupadosEduc$`tasa.cooc.part.invol.subocup.invol`)]<-0
  # Formato
  colnames(OcupadosCalif)[1] <- "ANO4"
  colnames(OcupadosCalif)[2] <- "grupos.tamanio"
  colnames(OcupadosCalif)[3] <- "grupos.nivel.ed"
  OcupadosEduc <-  OcupadosEduc[,c(4, 1, 2, 3, 5:10)]
  
  ## Cuadro Asalariados por Calificacion
  
  AsalaCalif <- Base                                 %>%
    filter(COND=="Ocupado" & STAPRO==3 & CALIF!="Ns/Nc" & TAMA!="Ns/Nc")   %>%
    group_by(YEAR, TAMA, CALIF)                             %>%
    summarise('Pais'                                 = countries[i],
              'Total'                                = sum(WEIGHT, na.rm=TRUE),
              'PromedioDecil'                        = weighted.mean(INCDECIL, WEIGHT, na.rm=TRUE),
              'tasa.part.invol'                      = sum(WEIGHT[PRECAPT==1], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
              'tasa.cooc.part.invol.temp'            = sum(WEIGHT[PRECAPT==1 & PRECATEMP==1], na.rm=TRUE)/ sum(WEIGHT[PRECAPT==1], na.rm=TRUE), 
              'tasa.cooc.part.invol.subocup.invol'   = sum(WEIGHT[PRECAPT==1 & PRECAHORA==1], na.rm=TRUE)/ sum(WEIGHT[PRECAPT==1], na.rm=TRUE)) %>%
    ungroup() %>%
    group_by(YEAR) %>%
    mutate('Particip_emp'          = Total/sum(Total))
  #Saco el valor NaN de Rumania
  AsalaCalif$`tasa.cooc.part.invol.temp`[is.nan(AsalaCalif$`tasa.cooc.part.invol.temp`)]<-0
  AsalaCalif$`tasa.cooc.part.invol.subocup.invol`[is.nan(AsalaCalif$`tasa.cooc.part.invol.subocup.invol`)]<-0
  # Formato
  colnames(OcupadosCalif)[1] <- "ANO4"
  colnames(OcupadosCalif)[2] <- "grupos.tamanio"
  colnames(OcupadosCalif)[3] <- "grupos.calif"
  AsalaCalif <-  AsalaCalif[,c(4, 1, 2, 3, 5:10)]
  
  ## Cuadro Asalariados por Educacion
  
  AsalaEduc <- Base                                 %>%
    filter(COND=="Ocupado" & STAPRO==3 & EDUC!="Ns/Nc" & TAMA!="Ns/Nc")   %>%
    group_by(YEAR, TAMA, EDUC)                             %>%
    summarise('Pais'                                 = countries[i],
              'Total'                                = sum(WEIGHT, na.rm=TRUE),
              'PromedioDecil'                        = weighted.mean(INCDECIL, WEIGHT, na.rm=TRUE),
              'tasa.part.invol'                      = sum(WEIGHT[PRECAPT==1], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
              'tasa.cooc.part.invol.temp'            = sum(WEIGHT[PRECAPT==1 & PRECATEMP==1], na.rm=TRUE)/ sum(WEIGHT[PRECAPT==1], na.rm=TRUE), 
              'tasa.cooc.part.invol.subocup.invol'   = sum(WEIGHT[PRECAPT==1 & PRECAHORA==1], na.rm=TRUE)/ sum(WEIGHT[PRECAPT==1], na.rm=TRUE)) %>%
    ungroup() %>%
    group_by(YEAR) %>%
    mutate('Particip_emp'          = Total/sum(Total))
  #Saco el valor NaN de Rumania
  AsalaEduc$`tasa.cooc.part.invol.temp`[is.nan(AsalaEduc$`tasa.cooc.part.invol.temp`)]<-0
  AsalaEduc$`tasa.cooc.part.invol.subocup.invol`[is.nan(AsalaEduc$`tasa.cooc.part.invol.subocup.invol`)]<-0
  # Formato
  colnames(OcupadosCalif)[1] <- "ANO4"
  colnames(OcupadosCalif)[2] <- "grupos.tamanio"
  colnames(OcupadosCalif)[3] <- "grupos.nivel.ed"
  AsalaEduc <-  AsalaEduc[,c(4, 1, 2, 3, 5:10)]
  
  #Lo guardo en Excel
  write.xlsx(as.data.frame(OcupadosCalif), paste0(CarpetaRdos, countries[i], ".xlsx"), sheetName = "Ocupados.calif", append = TRUE, row.names = FALSE)
  write.xlsx(as.data.frame(OcupadosEduc), paste0(CarpetaRdos, countries[i], ".xlsx"), sheetName = "Ocupados.nivel.ed", append = TRUE, row.names = FALSE)
  write.xlsx(as.data.frame(AsalaCalif), paste0(CarpetaRdos, countries[i], ".xlsx"), sheetName = "Asalariados.nivel.ed", append = TRUE, row.names = FALSE)
  write.xlsx(as.data.frame(AsalaEduc), paste0(CarpetaRdos, countries[i], ".xlsx"), sheetName = "Asalariados.calif", append = TRUE, row.names = FALSE)

  
  
  i <- i+1
}
  