library(tidyverse)
Base <- read.csv("Bases/chile_2019.csv", sep=";")   
variables<- c("PAIS","ANO","PERIODO","WEIGHT","SEXO","EDAD",
              "CATOCUP","SECTOR","PRECAPT","EDUC",
              "PRECAREG","PRECATEMP","PRECASALUD","PRECASEG",
              "TAMA","CALIF","ING") 

Base <- Base %>% 
  filter(cse_especifico %in% 1:7)  %>%
  filter(tipo!=3)   %>%
  mutate(
    ANO= 2019,
    PERIODO= 1, 
    PAIS="Chile",
    SEXO= case_when(
      sexo == 1 ~ "Varon", 
      sexo == 2 ~ "Mujer"),
    EDAD= edad, 
    EDUC= case_when(
      nivel %in% c(0:3) ~ "Primaria", 
      nivel %in% c(4:5) ~ "Secundaria",       
      nivel %in% c(6:14) ~ "Terciaria"),
    WEIGHT=as.numeric(sub(",", ".", fact_cal_esi)),
    CATOCUP=case_when(
      cise ==1                   ~ "Patron",
      cise == 2                  ~ "Cuenta propia",
      cise %in% 3:6              ~ "Asalariados", 
      cise == 7                  ~ "Resto"),
    SECTOR= case_when(
      cise %in% c(1,2,3,5)  ~ "Priv", 
      cise == 4             ~ "Pub", 
      cise %in% 5:6         ~ "SD"),
    PRECAPT= case_when(
      habituales<35 & habituales>0 & c10==1  ~ 1, 
      habituales<35 & habituales>0 & c10==2  ~ 0, 
      habituales %in% (35:98)                ~ 0),
    PRECATEMP= case_when(
      b9== 1            ~ 1,
      b9== 2            ~ 0), 
    PRECAREG= case_when(
      b8== 2            ~ 1, 
      b8== 1            ~ 0),
    PRECASEG= case_when(
      b7a_1== 2    ~ 1,          
      b7a_1== 1    ~ 0),  
    PRECASALUD=  case_when(
      d6_1_opcion== 2     ~ 1,              
      d6_1_opcion== 1   ~ 0),    
    TAMA= case_when( 
      b15_1 %in% 1:2     ~ "Pequeño",
      b15_1==3           ~ "Mediano",
      b15_1 %in% 4:5     ~ "Grande"),
    CALIF= case_when( 
      b1==9              ~ "Baja",
      b1 %in% 4:8        ~ "Media", 
      b1 %in% 1:3        ~ "Alta"), 
    ING=as.numeric(as.numeric(gsub(",", ".", gsub("\\.", "", ing_t_p)))), 
    ING=case_when(
      ING==0 ~ NA_real_, 
      ING>14000000 ~ NA_real_, 
      TRUE   ~ ING))                            %>%
  select(variables)      

saveRDS(Base, "bases_homog/chile.rds")
