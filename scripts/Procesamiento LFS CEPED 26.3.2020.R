library(tidyverse)
library(dplyr)
library(haven)
library(psych)
library(xlsx)

#Carpeta <- "C:/Users/facun/Documents/Investigación/1. LFS 2020/Bases/"
Carpeta <- "F:/LFS/"                                                                              # Carpeta con bases originales
CarpetaRdos <- "C:/Users/facun/Documents/Investigación/1. LFS 2020/Resultados/"                   # Carpeta de resultados
CarpetaBasesUnificadas <- "C:/Users/facun/Documents/Investigación/1. LFS 2020/BasesUnificadas/"   # Carpeta con bases por país unificadas para todos los años

###ARMADO DE BASES POR PAIS ###

countries <- c("ES", "FR", "UK", "DE", "GR", "IT", "NL", "NO", "PL", "PT", "RO", "SE", "SI")
#countries <- c("ES", "FR")

### Selecciono distintas variables (use una seleccion mas amplia de variables que había armado antes)

variables <- c("YEAR", "COEFF", "WSTATOR", "SEX", "AGE", "COUNTRYB", "SEEKWORK", "AVAILBLE", "STAPRO", "NACE1D", "SIZEFIRM", "ISCO1D", "ISCO2D", 
               "ISCO3D", "IS881D", "YSTARTWK", "MSTARTWK", "EXIST2J", "FTPT", "FTPTREAS", "TEMP", "TEMPREAS", "WISHMORE", "HWACTUAL", "HATLEV1D", 
               "COUNTRYB", "DEGURBA", "ILOSTAT", "INCDECIL")

### Este loop arma bases unificadas por país y crea variables derivadas de las originales para hacer los calculos

i <- 1

while (i < length(countries) + 1) {
  
  for (val in 2002:2014) {
    Base <- read.csv(paste0(Carpeta, countries[i], val, "_y", ".csv"))
    Base <- Base %>% 
      select(one_of(variables))
    nombre <- val
    assign(paste(nombre), Base)
  }
  
  JoinBase <- rbind.data.frame(`2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`)
  
  JoinBase <- JoinBase         %>%
    mutate(
      #Ponderador
      WEIGHT=COEFF*1000, 
      
      #Condicion de actividad
      COND= case_when(# 1. Ocupado (ILOSTAT 1. Employed, 4. Compulsory military service)
                      ILOSTAT==1 | ILOSTAT==4     ~ 1,
                      # 2. Desocupados que encontraron trabajo y estan disponibles para trabajar "inmediatamente" (en 2 semanas)
                      ILOSTAT==2                  ~ 2,
                      # 3. Inactivos (3.Inactive, 9. Persons less than 15 years old)
                      ILOSTAT==3 | ILOSTAT==9     ~ 3,
                      TRUE                        ~ 0),
      
      #Precariedad
      PRECA= case_when(#1. Part-time job and Person could not find a full-time job
                        FTPT==2 & FTPTREAS==5       ~ 1, 
                        #1. Person has temporary job/work contract of limited duration and person could not find a permanent job
                        TEMP==2 & TEMPREAS==2       ~ 1,
                        #1. Person Wish to work usually more than the current number of hours +  HWACTUAL < 35: la persona trabaja menos de 35 horas semanales
                        WISHMORE==1 & HWACTUAL < 35 ~ 1,
                        TRUE                        ~ 0),
      
      #Tamaño establecimiento
      TAMA= case_when( #1. 10 o menos
                      SIZEFIRM==10 | SIZEFIRM==14 ~ 1,
                      #2. 11 a 49
                      SIZEFIRM==11 | SIZEFIRM==12 ~ 2,
                      #3. Mas de 50
                      SIZEFIRM==13                ~ 3,
                      TRUE                        ~ 0),
      
      #Educación
      EDUC= case_when(#1. Baja
                       HATLEV1D=="L"     ~ 1, 
                       #1. Media
                       HATLEV1D=="M"     ~ 2,
                       #1. Alta
                       HATLEV1D=="H"     ~ 3, 
                       TRUE              ~ 0),
      
      #Calificación del puesto
      CALIF= case_when( #1. Alta
        ISCO1D %in% c(100, 200, 300)           | IS881D  %in% c(100, 200, 300)           ~ 1, 
        #2. Media
        ISCO1D %in% c(400, 500, 600, 700, 800) | IS881D  %in% c(400, 500, 600, 700, 800) ~ 2, 
        #3. Baja
        ISCO1D == 900                          | IS881D == 900                           ~ 3, 
        TRUE                                                                             ~ 0))
  
  assign(countries[i], JoinBase)
  
  remove(`2002`, `2003`, `2004`, `2005`, `2006`, `2007`, `2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `JoinBase`, `Base`)
  
  saveRDS(get(countries[i]), paste0(CarpetaBasesUnificadas, countries[i], ".Rda"))
  
  i <- i+1
  
}

### Loop para abrir las bases unificadas por país. Cambiando el vector countries se puede hacer una seleccion de paises

countries <- c("FR", "UK", "DE", "GR", "IT")

# countries <- c("ES", "FR", "UK", "DE", "GR", "IT", "NL", "NO", "PL", "PT", "RO", "SE", "SI")

i <- 1

while (i < length(countries) + 1) {
  
  Base <- readRDS(paste0(CarpetaBasesUnificadas, countries[i], ".Rda"))
  assign(countries[i], Base)
  i <- i+1
  
}

remove(`Base`)


#### PRIMERAS ESTIMACIONES 26.3.2020

countries <- c("FR", "UK", "DE", "GR", "IT")

# countries <- c("ES", "FR", "UK", "DE", "GR", "IT", "NL", "NO", "PL", "PT", "RO", "SE", "SI")

i <- 1

while (i < length(countries) + 1) {
  
  
  NivelEduc <-  get(countries[i])                                       %>%
    filter(YEAR==2014)                                                  %>%
    group_by(HATLEV1D)                                                  %>%
    summarise("Desocupados" = sum(WEIGHT[COND==2], na.rm=TRUE), 
              "Ocupados" = sum(WEIGHT[COND==1]),
              "Tasa Desocup" = Desocupados/Ocupados)
  
  
  assign(paste0(countries[i], ".NivelEduc "), NivelEduc)
  
  write.xlsx(as.data.frame(get(paste0(countries[i], ".NivelEduc "))), paste0(CarpetaRdos,"ResultadosNivelEduc.xlsx"),
             sheetName = paste0(countries[i]), append = TRUE, row.names = FALSE)
  
  
  Tama      <-  get(countries[i])                                       %>%
    filter(YEAR==2014 & COND==1)                                        %>%
    group_by(TAMA)                                                      %>%
    summarise("Casos"      = n(),
              "Ponderados" = sum(WEIGHT, na.rm=TRUE))
  
  assign(paste0(countries[i], ".Tama"), Tama)
  
  write.xlsx(as.data.frame(get(paste0(countries[i], ".Tama"))), paste0(CarpetaRdos,"ResultadosTama.xlsx"),
             sheetName = paste0(countries[i]), append = TRUE, row.names = FALSE)
  
  
  ISCO      <-  get(countries[i])                                       %>%
    filter(YEAR==2014 & COND==1)                                        %>%
    group_by(CALIF)                                                      %>%
    summarise("Casos"      = n(),
              "Ponderados" = sum(WEIGHT, na.rm=TRUE))
  
  assign(paste0(countries[i], ".ISCO"), ISCO)
  
  write.xlsx(as.data.frame(get(paste0(countries[i], ".ISCO"))), paste0(CarpetaRdos,"ResultadosISCO.xlsx"),
             sheetName = paste0(countries[i]), append = TRUE, row.names = FALSE)
  
  EducTama      <-  get(countries[i])                                   %>%
    filter(YEAR==2014 & COND==1)                                        %>%
    group_by(TAMA, HATLEV1D)                                            %>%
    summarise("Ocupados" = sum(WEIGHT, na.rm=TRUE))
  
  assign(paste0(countries[i], ".EducTama"), EducTama)
  
  write.xlsx(as.data.frame(get(paste0(countries[i], ".EducTama"))), paste0(CarpetaRdos,"ResultadosEducTama.xlsx"),
             sheetName = paste0(countries[i]), append = TRUE, row.names = FALSE)
  
  
  CalifTama      <-  get(countries[i])                                  %>%
    filter(YEAR==2014 & COND==1)                                        %>%
    group_by(TAMA, CALIF)                                               %>%
    summarise("Ocupados" = sum(WEIGHT, na.rm=TRUE))
  
  assign(paste0(countries[i], ".CalifTama"), CalifTama)
  
  write.xlsx(as.data.frame(get(paste0(countries[i], ".CalifTama"))), paste0(CarpetaRdos,"ResultadosCalifTama.xlsx"),
             sheetName = paste0(countries[i]), append = TRUE, row.names = FALSE)
  
  #TASA DE PRECARIEDAD SEGUN PERFILES POR TAMAÑO DEL ESTABLECIMIENTO Y CALIFICACION (porcentaje sobre el total de cada perfil)
  
  PrecaPorTamayCalif  <- get(countries[i])                            %>%
    filter(COND==1)                                                   %>%
    group_by(YEAR)                                                    %>%
    summarise('Etab chico y bajo calif'        = sum(WEIGHT[TAMA==1 & CALIF==3 & PRECA==1], na.rm=TRUE)/sum(WEIGHT[TAMA==1 & CALIF==3], na.rm=TRUE), 
              'Etab chico y mediana calif'     = sum(WEIGHT[TAMA==1 & CALIF==2 & PRECA==1], na.rm=TRUE)/sum(WEIGHT[TAMA==1 & CALIF==2], na.rm=TRUE),        
              'Etab chico y alta calif'        = sum(WEIGHT[TAMA==1 & CALIF==1 & PRECA==1], na.rm=TRUE)/sum(WEIGHT[TAMA==1 & CALIF==1], na.rm=TRUE),     
              'Etab mediano y bajo calif'      = sum(WEIGHT[TAMA==2 & CALIF==3 & PRECA==1], na.rm=TRUE)/sum(WEIGHT[TAMA==2 & CALIF==3], na.rm=TRUE),       
              'Etab mediano y mediana calif'   = sum(WEIGHT[TAMA==2 & CALIF==2 & PRECA==1], na.rm=TRUE)/sum(WEIGHT[TAMA==2 & CALIF==2], na.rm=TRUE),           
              'Etab mediano y alta calif'      = sum(WEIGHT[TAMA==2 & CALIF==1 & PRECA==1], na.rm=TRUE)/sum(WEIGHT[TAMA==2 & CALIF==1], na.rm=TRUE),        
              'Etab grande y bajo calif'       = sum(WEIGHT[TAMA==3 & CALIF==3 & PRECA==1], na.rm=TRUE)/sum(WEIGHT[TAMA==3 & CALIF==3], na.rm=TRUE),       
              'Etab grande y mediana calif'    = sum(WEIGHT[TAMA==3 & CALIF==2 & PRECA==1], na.rm=TRUE)/sum(WEIGHT[TAMA==3 & CALIF==2], na.rm=TRUE),          
              'Etab grande y alta calif'       = sum(WEIGHT[TAMA==3 & CALIF==1 & PRECA==1], na.rm=TRUE)/sum(WEIGHT[TAMA==3 & CALIF==1], na.rm=TRUE))       
  
  assign(paste0(countries[i], ".PrecaPorTamayCalif"), PrecaPorTamayCalif)
  
  write.xlsx(as.data.frame(get(paste0(countries[i], ".PrecaPorTamayCalif"))), paste0(CarpetaRdos, "ResultadosPrecaPorTamayCalif.xlsx"),
             sheetName = paste0(countries[i]), append = TRUE, row.names = FALSE)
  
  #DECILES SEGUN PERFILES POR TAMAÑO DEL ESTABLECIMIENTO Y CALIFICACION
  
  
  IngPorTamayCalif  <- get(countries[i])                             %>%
    filter(STAPRO==3 & COND==1)                                      %>%
    mutate(INCPONDERADO= INCDECIL*WEIGHT)                            %>%
    group_by(YEAR)                                                   %>%
    summarise('Etab chico y bajo calif'        = sum(INCPONDERADO[TAMA==1 & CALIF==3], na.rm=TRUE)/sum(WEIGHT[TAMA==1 & CALIF==3], na.rm=TRUE), 
              'Etab chico y mediana calif'     = sum(INCPONDERADO[TAMA==1 & CALIF==2], na.rm=TRUE)/sum(WEIGHT[TAMA==1 & CALIF==2], na.rm=TRUE),        
              'Etab chico y alta calif'        = sum(INCPONDERADO[TAMA==1 & CALIF==1], na.rm=TRUE)/sum(WEIGHT[TAMA==1 & CALIF==1], na.rm=TRUE),     
              'Etab mediano y bajo calif'      = sum(INCPONDERADO[TAMA==2 & CALIF==3], na.rm=TRUE)/sum(WEIGHT[TAMA==2 & CALIF==3], na.rm=TRUE),       
              'Etab mediano y mediana calif'   = sum(INCPONDERADO[TAMA==2 & CALIF==2], na.rm=TRUE)/sum(WEIGHT[TAMA==2 & CALIF==2], na.rm=TRUE),           
              'Etab mediano y alta calif'      = sum(INCPONDERADO[TAMA==2 & CALIF==1], na.rm=TRUE)/sum(WEIGHT[TAMA==2 & CALIF==1], na.rm=TRUE),        
              'Etab grande y bajo calif'       = sum(INCPONDERADO[TAMA==3 & CALIF==3], na.rm=TRUE)/sum(WEIGHT[TAMA==3 & CALIF==3], na.rm=TRUE),       
              'Etab grande y mediana calif'    = sum(INCPONDERADO[TAMA==3 & CALIF==2], na.rm=TRUE)/sum(WEIGHT[TAMA==3 & CALIF==2], na.rm=TRUE),          
              'Etab grande y alta calif'       = sum(INCPONDERADO[TAMA==3 & CALIF==1], na.rm=TRUE)/sum(WEIGHT[TAMA==3 & CALIF==1], na.rm=TRUE))       
  
  assign(paste0(countries[i], ".IngPorTamayCalif"), IngPorTamayCalif)
  
  write.xlsx(as.data.frame(get(paste0(countries[i], ".IngPorTamayCalif"))), paste0(CarpetaRdos, "ResultadosIngPorTamayCalif.xlsx"),
             sheetName =  paste0(countries[i]), append = TRUE, row.names = FALSE)
  

  #TASA DE PRECARIEDAD SEGUN PERFILES POR TAMAÑO DEL ESTABLECIMIENTO Y EDUCACION (porcentaje sobre el total de cada perfil)
  
  PrecaPorTamayEduc  <- get(countries[i])                            %>%
    filter(COND==1)                                                   %>%
    group_by(YEAR)                                                    %>%
    summarise('Etab chico y bajo educ'        = sum(WEIGHT[TAMA==1 & EDUC==3 & PRECA==1], na.rm=TRUE)/sum(WEIGHT[TAMA==1 & EDUC==3], na.rm=TRUE), 
              'Etab chico y mediana educ'     = sum(WEIGHT[TAMA==1 & EDUC==2 & PRECA==1], na.rm=TRUE)/sum(WEIGHT[TAMA==1 & EDUC==2], na.rm=TRUE),        
              'Etab chico y alta educ '       = sum(WEIGHT[TAMA==1 & EDUC==1 & PRECA==1], na.rm=TRUE)/sum(WEIGHT[TAMA==1 & EDUC==1], na.rm=TRUE),     
              'Etab mediano y bajo educ'      = sum(WEIGHT[TAMA==2 & EDUC==3 & PRECA==1], na.rm=TRUE)/sum(WEIGHT[TAMA==2 & EDUC==3], na.rm=TRUE),       
              'Etab mediano y mediana educ'   = sum(WEIGHT[TAMA==2 & EDUC==2 & PRECA==1], na.rm=TRUE)/sum(WEIGHT[TAMA==2 & EDUC==2], na.rm=TRUE),           
              'Etab mediano y alta educ'      = sum(WEIGHT[TAMA==2 & EDUC==1 & PRECA==1], na.rm=TRUE)/sum(WEIGHT[TAMA==2 & EDUC==1], na.rm=TRUE),        
              'Etab grande y bajo educ'       = sum(WEIGHT[TAMA==3 & EDUC==3 & PRECA==1], na.rm=TRUE)/sum(WEIGHT[TAMA==3 & EDUC==3], na.rm=TRUE),       
              'Etab grande y mediana educ'    = sum(WEIGHT[TAMA==3 & EDUC==2 & PRECA==1], na.rm=TRUE)/sum(WEIGHT[TAMA==3 & EDUC==2], na.rm=TRUE),          
              'Etab grande y alta educ'       = sum(WEIGHT[TAMA==3 & EDUC==1 & PRECA==1], na.rm=TRUE)/sum(WEIGHT[TAMA==3 & EDUC==1], na.rm=TRUE))       
  
  assign(paste0(countries[i], ".PrecaPorTamayEduc"), PrecaPorTamayEduc)
  
  write.xlsx(as.data.frame(get(paste0(countries[i], ".PrecaPorTamayEduc"))), paste0(CarpetaRdos, "ResultadosPrecaPorTamayEduc.xlsx"),
             sheetName =  paste0(countries[i]), append = TRUE, row.names = FALSE)

  i <- i+1
  
}
