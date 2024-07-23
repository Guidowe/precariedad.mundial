library(tidyverse)
library(haven)

variables<- c("PAIS","ANO","PERIODO","WEIGHT","SEXO","EDAD",
              "CATOCUP","SECTOR","PRECAPT","EDUC",
              "PRECAREG","PRECATEMP","PRECASALUD","PRECASEG",
              "TAMA","CALIF","ING") 

PAR1 <- read_sav("Bases/Paraguay_T12019.SAV")
PAR1 <- PAR1 %>% mutate(PERIODO=1)
PAR2 <-  read_sav("Bases/Paraguay_T22019.SAV")
PAR2 <- PAR2 %>% mutate(PERIODO=2)
PAR3 <-  read_sav("Bases/Paraguay_T32019.SAV")
PAR3 <- PAR3 %>% mutate(PERIODO=3)
PAR4 <-  read_sav("Bases/Paraguay_T42019.SAV")
PAR4 <- PAR4 %>% mutate(PERIODO=4)

PAR <- bind_rows(PAR1, PAR2, PAR3, PAR4)
remove(PAR1, PAR2, PAR3, PAR4)
Base <- PAR                                 %>% 
  filter(AREA!=6)                           %>%
  mutate(
    ANO= 2019,
    PAIS="Paraguay",
    WEIGHT=FEX,
    SEXO= case_when(
      P06 ==1 ~ "Varon", 
      P06 ==6 ~ "Mujer"),
    EDAD=P02, 
    EDUC= NA, 
    CATOCUP=case_when(
      CATE_PEA ==   4          ~ "Cuenta propia", 
      CATE_PEA %in% 1:2        ~ "Asalariados", 
      CATE_PEA %in% c(3, 5, 6) ~ "Resto"),
    SECTOR= case_when(
      CATE_PEA==1 ~ "Pub", 
      CATE_PEA %in% 2:5~ "Priv",       
      CATE_PEA==6 ~ "SD"),
    PRECAPT= case_when( 
      HORAB < 35 & HORAB>0 & D03 %in% 1:3  ~ 1,
      HORAB < 35 & HORAB>0 & D03==6        ~ 0,
      HORAB > 34                           ~ 0),
    PRECATEMP= case_when( 
      B26 %in% 2:3       ~ 1, 
      B26 %in% c(1, 4)   ~ 0),  
    PRECASEG= case_when(
      B10==6  ~ 1,    
      B10==1  ~ 0),  
    PRECAREG= case_when( 
      B26==4        ~ 1,   
      B26 %in% 1:3  ~ 0),  
    PRECASALUD= NA,                 
    TAMA= case_when( 
      TAMA_PEA %in% 1:3    ~ "Pequeño",
      TAMA_PEA %in% 4:6    ~ "Mediano",
      TAMA_PEA %in% 7:9    ~ "Grande"),
    #Calificacion del puesto
    CALIF= case_when( 
      B01REC == 9          ~ "Baja",
      B01REC %in% 4:8       ~ "Media", 
      B01REC %in% 1:3       ~ "Alta"),
    #Ingreso de la ocupacion principal
    ING=as.numeric(E01AIMDE), 
    ING=case_when(
      ING==0 ~ NA_real_, 
      ING > 100000000 ~ NA_real_, 
      TRUE   ~ ING))                                 %>% 
  select(variables) 

saveRDS(Base, "bases_homog/paraguay.rds")
