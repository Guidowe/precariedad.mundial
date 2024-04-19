library(tidyverse)
variables <- c("trimestre", "s2_22", "fact_trim", "s2_18", "condact", "phrs", "s2_57",  
               "s2_21",  "s2_26", "cob_op", "yprilab", "s2_64", "area", "s2_36a")  
variables_homog <- c("PAIS", "WEIGHT", "CATOCUP", "COND", "PRECAPT", "PRECAREG", "PRECATEMP", "PRECASALUD", "PRECASEG", 
                     "PRECAPT_binaria", "PRECAREG_binaria", "PRECATEMP_binaria", "PRECASALUD_binaria", "PRECASEG_binaria",
                     "TAMA", "CALIF", "ING", "ANO", "PRECACOUNT", "PRECACOUNT2", "PERIODO")
Base <- readRDS("Bases/Bolivia_1a4T2019.RDS")
Base$s2_18[is.na(Base$s2_18)] = 0         #Saco NAs de variable para categoria ocupacional
Base$s2_22[is.na(Base$s2_22)] = 0
Base <- Base                                 %>% 
  select(variables)                         %>%
  # Filtro sector publico y servicio domestico
  filter(s2_22!=1 & s2_18!=7)         %>%   # Saco Sector Publico y "ewmpleada/o del hogar"
  # Filtro areas rurales
  filter(area!=2)                   %>%
  mutate(
    ANO= 2019,
    PERIODO=trimestre,
    PAIS="Bolivia",
    #Ponderador
    WEIGHT=fact_trim,
    #Categoria Ocupacional                                         
    CATOCUP=factor(case_when(
      s2_18 == 2                        ~ "Cuenta propia", 
      s2_18 %in%  c(3, 4, 5, 6, 7)      ~ "Resto", 
      s2_18 == 1                        ~ "Asalariados",       
      TRUE                              ~ "Ns/Nc"),
      levels= c("Asalariados", "Cuenta propia", "Resto", "Ns/Nc")),
    #Condicion de actividad
    COND= factor(case_when(
      condact == 1       ~ "Ocupado",
      condact %in% 2:5   ~ "Desocupado", #Categoria 4 y 5 tal vez son inactivos. Para los calculos no afecta porque tomamos solo ocupados
      TRUE               ~ "Ns/Nc"),
      levels= c("Ocupado", "Desocupado", "Ns/Nc")),
    #Precariedad por trabajo part-time
    PRECAPT= factor(case_when(phrs<35 & phrs>0 & s2_57==1  ~ "Part-time involuntario",         
                              phrs<35 & phrs>0 & s2_57==2  ~ "Part-time voluntario", 
                              phrs>34                      ~ "Tiempo completo",                                                
                              TRUE                         ~ "Ns/Nc"),                      
                    levels= c("Part-time involuntario", "Part-time voluntario", "Tiempo completo","Ns/Nc")),
    #Precariedad por contrato de tiempo limitado
    PRECATEMP= factor(case_when( s2_21 %in% 1:2     ~ "Temporal",              
                                 s2_21 %in% 3:5            ~ "No temporal",
                                 TRUE  ~ "Ns/Nc"), 
                      levels= c("Temporal", "No temporal", "Ns/Nc")),  
    #Precariedad por registracion
    PRECAREG= factor(case_when( s2_21  %in% c(2, 3, 5)     ~ "No registrado",        
                                s2_21  %in% c(1, 4)          ~ "Registrado", 
                                TRUE                      ~  "Ns/Nc"),
                     levels=c("No registrado", "Registrado", "Ns/Nc")),  
    #Precariedad por aportes a la seguridad social
    PRECASEG= factor(case_when( s2_64==2 ~ "Sin aportes",               
                                s2_64==1  ~ "Con aportes",             
                                TRUE      ~  "Ns/Nc"),
                     levels=c( "Sin aportes", "Con aportes", "Ns/Nc")),                  
    #Precariedad por acceso al sistema de salud
    PRECASALUD= factor(case_when( s2_36a==2 ~ "Sin cobertura",              
                                  s2_36a==1 ~ "Con cobertura",  
                                  TRUE   ~  "Ns/Nc"), 
                       levels=c("Sin cobertura", "Con cobertura", "Ns/Nc")),                                     
    #Conteo de expersiones de precariedad con generacion de binarias
    PRECAPT_binaria= case_when(PRECAPT=="Part-time involuntario"~ 1 ,         
                               TRUE   ~ 0), 
    PRECATEMP_binaria= case_when(PRECATEMP=="Temporal"~ 1 ,         
                                 TRUE   ~ 0), 
    PRECAREG_binaria= case_when(PRECAREG=="No registrado"~ 1 ,         
                                TRUE   ~ 0), 
    PRECASALUD_binaria= case_when(PRECASALUD== "Sin cobertura"  ~ 1 ,         
                                  TRUE   ~ 0), 
    PRECASEG_binaria=  case_when(PRECASEG== "Sin aportes"  ~ 1 ,         
                                 TRUE   ~ 0),
    PRECACOUNT= PRECAPT_binaria + PRECATEMP_binaria + PRECAREG_binaria + PRECASEG_binaria,
    PRECACOUNT2= PRECAPT_binaria + PRECASALUD_binaria,
    #Tamaño establecimiento
    TAMA= factor(case_when( 
      #1. 10 o menos
      s2_26 %in% 1:10    ~ "Pequeño",
      #2. 11 a 50
      s2_26  %in% 11:50    ~ "Mediano",
      #3. Mas de 50
      s2_26 > 50        ~ "Grande",
      TRUE            ~ "Ns/Nc"),
      levels= c("Pequeño", "Mediano", "Grande", "Ns/Nc")),
    #Calificacion del puesto
    CALIF= factor(case_when( #1. Baja
      cob_op == 9              ~ "Baja",
      #2. Media
      cob_op  %in% 4:8         ~ "Media", 
      #3. Alta
      cob_op  %in% 1:3         ~ "Alta", 
      TRUE                    ~  "Ns/Nc"), 
      levels= c("Baja", "Media", "Alta", "Ns/Nc")), 
    #Ingreso de la ocupacion principal
    ING=yprilab, 
    ING=case_when(
      ING==0 ~ NA_real_, 
      TRUE   ~ ING))              %>% 
  select(variables_homog)   

save(Base, file= "Bases_homog/Bolivia.Rdata")

