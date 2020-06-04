
library(dplyr)

Carpeta <- "F:/LFS/"                                                                    
CarpetaRdos <- "C:/Users/facun/Documents/Investigación/1. LFS 2020 - CEPED Precariedad mundial/Resultados/"
CarpetaBasesUnificadas <- "F:/LFS/BasesUnificadas/" 

                            ### CALCULO DE SERIES 2008-2018 ####  

countries <- c("ES", "FR", "UK", "DE", "GR", "IT", "PT", "DK", "BG", "RO")

i <- 1

Resultados_bind <- data.frame()

while (i < length(countries) + 1) {

  Base <- readRDS(paste0(CarpetaBasesUnificadas, countries[i], "2008-2018.Rda"))
  
  ## Sacar Sector público, servicio doméstico y trabajadores familiares
  
  Base <- Base           %>%
    filter(NACE1D!="O")  %>%
    filter(NACE1D!="T")  %>%
    filter(STAPRO!=4)
  
  ## Armo cuadro
  Resultados <- Base                                 %>%
    filter(COND=="Ocupado" & CALIF!="Ns/Nc" & TAMA!="Ns/Nc")   %>%
    group_by(YEAR, TAMA, CALIF)                             %>%
    summarise('pais'                                 = countries[i],
              'total.ocupados'                       = sum(WEIGHT, na.rm=TRUE),
              'tasa.asalarizacion'                   = sum(WEIGHT[STAPRO==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
              'tasa.1.ocupados'                      = sum(WEIGHT[PRECACOUNT==1 | PRECACOUNT==2 | PRECACOUNT==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
              'total.asal'                           = sum(WEIGHT[STAPRO==3], na.rm=TRUE),
              'promedio.decil'                       = weighted.mean(INCDECIL[STAPRO==3], WEIGHT[STAPRO==3], na.rm=TRUE),            
              'tasa.1.asalariados'                   = sum(WEIGHT[PRECACOUNT==1 | PRECACOUNT==2 | PRECACOUNT==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
              'tasa.2.asalariados'                   = sum(WEIGHT[PRECACOUNT==2 | PRECACOUNT==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),              
              'tasa.3.asalariados'                   = sum(WEIGHT[PRECACOUNT==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
              'total.tcp'                            = sum(WEIGHT[STAPRO==0], na.rm=TRUE),              
              'tasa.parttime.tcp'                    = sum(WEIGHT[PRECAPT==1 & STAPRO==0], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE))  %>%
    ungroup() %>%
    group_by(YEAR) %>%
    mutate('particip.ocup'          = total.ocupados/sum(total.ocupados), 
           'particip.asal'          = total.asal/sum(total.ocupados), 
           'particip.tcp'           = total.tcp/sum(total.ocupados))

  # Formato
  colnames(Resultados)[1] <- "ANO4"
  colnames(Resultados)[2] <- "grupos.tamanio"
  colnames(Resultados)[3] <- "grupos.calif"
  Resultados <-  Resultados[,c(4, 1, 2, 3, 5, 15, 6:8, 16, 9:13, 17, 14)]
  
  
  Resultados_bind <-   bind_rows(Resultados, Resultados_bind)
  
  i <- i+1
}

save(Resultados_bind,
     file = paste0(CarpetaRdos,"EUROPA.RDATA"))


