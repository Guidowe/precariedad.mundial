library(tidyverse)
Base <- read.csv("Bases/chile_2019.csv", sep=";")    #Es una sola base anual que se levanta entre octubre y diciembre

variables <- c("fact_cal_esi", "c1", "c10", "c11", "b9", "b7a_1", "b7a_2", "b8", 
               "b15_1", "b1", "cise", "cse_especifico", "habituales", "ing_t_p", "tipo", "d6_1_opcion")
variables_homog <- c("PAIS", "WEIGHT", "CATOCUP", "COND", "PRECAPT", "PRECAREG", "PRECATEMP", "PRECASALUD", "PRECASEG", 
                     "PRECAPT_binaria", "PRECAREG_binaria", "PRECATEMP_binaria", "PRECASALUD_binaria", "PRECASEG_binaria",
                     "TAMA", "CALIF", "ING", "ANO", "PRECACOUNT", "PRECACOUNT2", "PERIODO")

Base <- Base                                  %>% 
  select(variables)                         %>%
  #  Filtro sector publico y servicio domestico
  filter( cise!=4 & cise!=5 &  cise!=6)      %>%
  # Filtro areas rurales
  filter(tipo!=3)                            %>%
  mutate(
    ANO= 2019,
    PERIODO= 1, 
    PAIS="Chile",
    #Ponderador
    WEIGHT=as.numeric(sub(",", ".", fact_cal_esi)),
    #Categoria Ocupacional                                        
    CATOCUP=factor(case_when(
      cise %in% c(1, 7)        ~ "Resto",
      cise == 2                ~ "Cuenta propia",
      cise == 3                ~ "Asalariados",       
      TRUE                     ~ "Ns/Nc"),
      levels= c("Asalariados", "Cuenta propia", "Resto", "Ns/Nc")),
    #Condicion de actividad
    COND= factor(case_when(
      cse_especifico %in% 1:7          ~ "Ocupado",
      cse_especifico %in% 8:9          ~ "Desocupado",
      cse_especifico %in% c(0, 10:28)   ~ "Inactivo"),
      levels= c("Ocupado", "Desocupado", "Inactivo", "Ns/Nc")),
    #Precariedad por trabajo part-time
    PRECAPT= factor(case_when(habituales<35 & habituales>0 & c10==1           ~ "Part-time involuntario", 
                              habituales<35 & habituales>0 & c10==2          ~ "Part-time voluntario", 
                              habituales %in% (35:98)         ~ "Tiempo completo",
                              habituales==99                  ~  "Ns/Nc", 
                              TRUE                            ~  "Ns/Nc"),
                    levels= c("Part-time involuntario", "Part-time voluntario", "Tiempo completo","Ns/Nc")),
    #Precariedad por contrato de tiempo limitado
    PRECATEMP= factor(case_when(b9== 1            ~ "Temporal",
                                b9== 2            ~ "No temporal",
                                TRUE              ~ "Ns/Nc"), 
                      levels= c("Temporal", "No temporal", "Ns/Nc")),             
    #Precariedad por registracion del contrato
    PRECAREG= factor(case_when(b8== 2            ~ "No registrado", 
                               b8== 1            ~ "Registrado", 
                               TRUE              ~  "Ns/Nc"),
                     levels=c("No registrado", "Registrado", "Ns/Nc")),  
    #Precariedad por aportes a la seguridad social
    PRECASEG= factor(case_when(b7a_1== 2                      ~ "Sin aportes",           #Solo para asalariados
                               b7a_1== 1                             ~ "Con aportes",
                               b7a_1==88 | b7a_1==99 | is.na(b7a_1)  ~ "Ns/Nc"),
                     levels=c( "Sin aportes", "Con aportes", "Ns/Nc")),                  
    #Precariedad por cobertura de salud
    PRECASALUD=  factor(case_when(d6_1_opcion== 2                             ~ "Sin cobertura",              # Tiene ISAPRE o FONASA?
                                  d6_1_opcion== 1                             ~ "Con cobertura",
                                  d6_1_opcion==88 | d6_1_opcion==99 | is.na(d6_1_opcion)  ~ "Ns/Nc"),
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
      b15_1 %in% 1:2     ~ "Pequeño",
      #2. 11 a 49
      b15_1==3           ~ "Mediano",
      #3. Mas de 50
      b15_1 %in% 4:5     ~ "Grande",
      TRUE               ~ "Ns/Nc"),
      levels= c("Pequeño", "Mediano", "Grande", "Ns/Nc")),
    #Calificacion del puesto
    CALIF= factor(case_when( #1. Baja
      b1==9              ~ "Baja",
      #2. Media
      b1 %in% 4:8        ~ "Media", 
      #3. Alta
      b1 %in% 1:3        ~ "Alta", 
      TRUE               ~  "Ns/Nc"), 
      levels= c("Baja", "Media", "Alta", "Ns/Nc")), 
    #Ingreso de la ocupacion principal
    ING=as.numeric(as.numeric(gsub(",", ".", gsub("\\.", "", ing_t_p)))), 
    #Limpio ruido
    ING=case_when(
      ING==0 ~ NA_real_, 
      ING>14000000 ~ NA_real_, 
      TRUE   ~ ING))                            %>%
  select(variables_homog)      

save(Base, file= "Bases_homog/Chile.Rdata")