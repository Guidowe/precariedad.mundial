library(dplyr)
library(beepr)

Carpeta <- "D:/LFS/"                                                                              # Carpeta con bases originales
CarpetaBasesUnificadas <- "D:/LFS/BasesUnificadas/"                                

variables <- c("YEAR", "COEFF", "WSTATOR", "SEX", "AGE", "COUNTRYB", "SEEKWORK", "AVAILBLE", "STAPRO", "NACE1D", 
               "SIZEFIRM", "ISCO1D", "ISCO2D", "ISCO3D", "IS881D", "YSTARTWK", "MSTARTWK", "EXIST2J", "FTPT", 
               "FTPTREAS", "TEMP", "TEMPREAS", "TEMPAGCY", "WISHMORE", "HAT11LEV", "HAT97LEV", "HATLEV1D", "COUNTRYB", 
               "DEGURBA", "ILOSTAT", "INCDECIL", "EXISTPR", "NACEPR2D", "NACE11PR2D", "ISCOPR3D", "HWUSUAL", "HWACTUAL")

              ##########   GENERACION BASES UNIFICADAS POR PAIS 2008-2018    ##########

countries <- c("ES", "FR", "UK", "DE", "GR", "IT", "PT", "DK", "BG", "RO")

i <- 1
while (i < length(countries) + 1) {
  for (val in 2008:2018) {
    
  print(paste0('Procesando ', countries[i], ' para el año ', val ))
    
  Base <- read.csv(paste0(Carpeta, countries[i], val, "_y", ".csv"))
  Base <- Base                               %>% 
    select(one_of(variables))                %>%
    mutate(COUNTRY= countries[i])   
  assign(paste0(val), Base)}
  
  Base <- rbind.data.frame(`2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`)
  remove(`2008`, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`)
  
  
  Base <- Base         %>%
    mutate(
      #Ponderador
      WEIGHT=COEFF*1000, 
      #Condicion de actividad
      COND= factor(case_when(# 1. Ocupado (ILOSTAT 1. Employed, 4. Compulsory military service)
        ILOSTAT==1 | ILOSTAT==4     ~ "Ocupado",
        # 2. Desocupados que encontraron trabajo y estan disponibles para trabajar "inmediatamente" (en 2 semanas)
        ILOSTAT==2                  ~ "Desocupado",
        # 3. Inactivos (3.Inactive, 9. Persons less than 15 years old)
        ILOSTAT==3 | ILOSTAT==9     ~ "Inactivo",
        TRUE                        ~ "Ns/Nc"),
        levels= c("Ocupado", "Desocupado", "Inactivo", "Ns/Nc")),
      #Precariedad por trabajo part-time
      PRECAPT= case_when(FTPT==2 & FTPTREAS==5       ~ 1,
                         TRUE                        ~ 0),
      #Precariedad por contrato de tiempo limitado
      PRECATEMP= case_when(TEMP==2 & TEMPREAS==2       ~ 1,
                           TRUE                        ~ 0),
      PRECACOUNT= PRECAPT + PRECATEMP,
      #Tamaño establecimiento
      TAMA= factor(case_when( #1. 10 o menos
        SIZEFIRM==10 | SIZEFIRM==14 ~ "Pequeño",
        #2. 11 a 49
        SIZEFIRM==11 | SIZEFIRM==12 ~ "Mediano",
        #3. Mas de 50
        SIZEFIRM==13                ~ "Grande",
        TRUE                        ~ "Ns/Nc"),
        levels= c("Pequeño", "Mediano", "Grande", "Ns/Nc")),
      #Educación
      EDUC= factor(case_when(#1. Baja                                 
        HAT11LEV %in% 100:299 | HAT97LEV %in% 10:29   ~ "Menor a Secundaria", 
        #1. Media
        HAT11LEV %in% 300:599 | HAT97LEV %in% 30:59   ~ "Secundaria Completa",
        #1. Alta
        HAT11LEV %in% 600:899 | HAT97LEV %in% 60:89   ~ "Superior Completo", 
        TRUE              ~ "Ns/Nc"),
        levels= c("Menor a Secundaria", "Secundaria Completa", "Superior Completo", "Ns/Nc")),
      #Calificación del puesto
      CALIF= factor(case_when( #1. Baja
        ISCO1D == 900                          | IS881D == 900                           ~ "Baja",
        #2. Media
        ISCO1D %in% c(400, 500, 600, 700, 800) | IS881D  %in% c(400, 500, 600, 700, 800) ~ "Media", 
        #3. Alta
        ISCO1D %in% c(100, 200, 300)           | IS881D  %in% c(100, 200, 300)           ~ "Alta", 
        TRUE                                                                             ~  "Ns/Nc"), 
        levels= c("Baja", "Media", "Alta", "Ns/Nc")), 
      CALIFANT= factor(case_when( #1. Baja
        ISCOPR3D %in% 900:998                    ~ "Baja",
        #2. Media
        ISCOPR3D %in% 400:899                    ~ "Media", 
        #3. Alta
        ISCOPR3D %in% c(11:39, 100:399)          ~ "Alta", 
        TRUE                                     ~  "Ns/Nc"), 
        levels= c("Baja", "Media", "Alta", "Ns/Nc")))
  
  saveRDS(Base, paste0(CarpetaBasesUnificadas, countries[i], "2008-2018.Rda"))
  i <- i+1
}



### GENERACION BASES 2014


i <- 2   # Para España va 2015
while (i < length(countries) + 1) {
  Base <- readRDS(paste0(CarpetaBasesUnificadas, countries[i], "2008-2018.Rda")) 
  Base <- Base %>% filter(YEAR==2014)
  assign(paste0(countries[i]), Base)
  i <- i+1
}
  
ES <- readRDS(paste0(CarpetaBasesUnificadas, "ES2008-2018.Rda")) 
ES <- ES %>% filter(YEAR==2015)

Base <- rbind.data.frame(ES, FR, UK, DE, GR, IT, PT, DK, BG, RO)
remove(ES, FR, UK, DE, GR, IT, PT, DK, BG, RO)

saveRDS(Base, paste0(CarpetaBasesUnificadas, "SeleccionPaises2014.Rda"))
