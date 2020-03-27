library(dplyr)

Carpeta <- "F:/LFS/"                                                                              # Carpeta con bases originales
CarpetaRdos <- "C:/Users/facun/Documents/Investigación/1. LFS 2020/Resultados/"                   # Carpeta de resultados
CarpetaBasesUnificadas <- "C:/Users/facun/Documents/Investigación/1. LFS 2020/BasesUnificadas/"   # Carpeta con bases por país unificadas para todos los años

countries <- c("FR", "UK", "DE", "GR", "IT")
#countries <- c("FR", "UK")

variables <- c("YEAR", "COEFF", "WSTATOR", "SEX", "AGE", "COUNTRYB", "SEEKWORK", "AVAILBLE", "STAPRO", "NACE1D", "SIZEFIRM", "ISCO1D", "ISCO2D", 
               "ISCO3D", "IS881D", "YSTARTWK", "MSTARTWK", "EXIST2J", "FTPT", "FTPTREAS", "TEMP", "TEMPREAS", "WISHMORE", "HWACTUAL", "HATLEV1D", 
               "COUNTRYB", "DEGURBA", "ILOSTAT", "INCDECIL")
ano <- 2014

i <- 1

while (i < length(countries) + 1) {
  
    Base <- read.csv(paste0(Carpeta, countries[i], ano, "_y", ".csv"))
    
    Base <- Base                               %>% 
      select(one_of(variables))                %>%
      filter(YEAR== ano)                       %>%
      mutate(COUNTRY= countries[i])            
    
    assign(paste0(countries[i]), Base)
    
    i <- i+1
    
    }
  
Base <- rbind.data.frame(`FR`, `UK`, `DE`, `GR`, `IT`)

remove(`FR`, `UK`, `DE`, `GR`, `IT`)
  
  
Base <- Base         %>%
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
    CALIF= case_when( #1. Baja
                      ISCO1D == 900                          | IS881D == 900                           ~ 1,
                      #2. Media
                      ISCO1D %in% c(400, 500, 600, 700, 800) | IS881D  %in% c(400, 500, 600, 700, 800) ~ 2, 
                      #3. Alta
                      ISCO1D == 900                          | IS881D == 900                           ~ 3, 
                      TRUE                                                                             ~ 0))


saveRDS(Base, paste0(CarpetaBasesUnificadas, "SeleccionPaises.", ano, ".Rda"))
  
  