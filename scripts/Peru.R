library(tidyverse)
library(haven)
variables<- c("PAIS","ANO","PERIODO","WEIGHT","SEXO","EDAD",
              "CATOCUP","SECTOR","PRECAPT","EDUC",
              "PRECAREG","PRECATEMP","PRECASALUD","PRECASEG",
              "TAMA","CALIF","ING") 

PER1 <- read_dta("Bases/Peru_1T2019.dta") %>% mutate(PERIODO=1)
PER2 <- read_dta("Bases/Peru_2T2019.dta")  %>% mutate(PERIODO=2)
PER3 <- read_dta("Bases/Peru_3T2019.dta") %>% mutate(PERIODO=3)
PER4 <- read_dta("Bases/Peru_4T2019.dta") %>% mutate(PERIODO=4)
PER <- bind_rows(PER1, PER2, PER3, PER4)
remove(PER1, PER2, PER3, PER4)

PER$p507[is.na(PER$p507)] = 0  
PER$p510[is.na(PER$p510)] = 0         #Saco NA de variable p510 para no perder a los cuentapropistas cuando cruzo p507 y p510 en los filter

Base<- PER                                  %>% 
  # Filtro areas rurales 
  filter(estrato!=7 &  estrato!=8)         %>%   
  # Filtro ocupados
  filter(ocu500== 1) %>% 
  mutate(                                             
    ANO= 2019,
    PAIS="Peru",
    #Ponderador
    WEIGHT=fac500,
    EDAD = p208a, 
    SEXO = case_when(
      p207 == 1 ~ "Varon", 
      p207 == 2 ~ "Mujer"), 
    EDUC = case_when(
      p301a %in% c(2:5) ~ "Primaria", 
      p301a %in% c(6:7, 9) ~ "Secundaria", 
      p301a %in% c(8, 10, 11) ~ "Terciaria"),
    CATOCUP=case_when(
      p507 == 2                  ~ "Cuenta propia", 
      p507 %in% c(1, 5, 7)       ~ "Resto", 
      p507 %in% c(3, 4, 6)        ~ "Asalariados"),
    SECTOR = case_when(
      p510 %in% c(1, 2) ~ "Pub",
      p507 == 6 ~ "SD", 
      TRUE ~ "Priv"),
    PRECAPT= case_when(p513t<35 & p513t>0 & p521==1   ~ 1,     
                              p513t<35 & p513t>0 & p521==2   ~ 0, 
                              p513t>34                       ~ 0),
    PRECATEMP= factor(case_when( p511a %in% c(2,6)              ~ 1,
                                 p511a %in% c(1, 3, 4, 5, 7, 8) ~ 0)),
    PRECASEG= factor(case_when( p558a5==5                                   ~ 1, 
                                p558a1==1 | p558a2==2 | p558a3==3 | p558a4==4 ~ 0)), 
    PRECAREG= factor(case_when( p511a==7 ~ 1,
                                p511a %in% c(1:6, 8)  ~ 0)),
    PRECASALUD= NA,                                
    #Tamaño establecimiento
    TAMA= case_when( 
      p512a==1 & p512b<11         ~ "Pequeño",              
      p512a==1 & p512b %in% 11:20 ~ "Mediano",
      p512a==2                    ~ "Mediano",                
      p512a %in% 3:5              ~ "Grande"),
    CALIF= case_when( 
      p505r4 %in% 900:999          ~ "Baja", 
      p505r4 %in% 9000:9998        ~ "Baja",                         
      p505r4 %in% 400:899          ~ "Media",                 
      p505r4 %in% 4000:8999        ~ "Media", 
      p505r4 %in% 100:399          ~ "Alta", 
      p505r4 %in% 1000:3999        ~ "Alta", 
      TRUE                     ~  "Ns/Nc"), 
    ING=case_when(
      CATOCUP=="Asalariados" & p523==1       ~ p524e1 * 20,                      # ASALARIADOS: el dato de ingreso de ocupacion principal esta en jornal, semana, quincenal o mes
      CATOCUP=="Asalariados" & p523==2       ~ p524e1 * 4,                       # dependiendo como cobre el encuestado. Se mensualiza suponiendo que la persona trabaja todo el mes
      CATOCUP=="Asalariados" & p523==3       ~ p524e1 * 2,                       # lo mismo que trabajo en la semana de referencia
      CATOCUP=="Asalariados" & p523==4       ~ p524e1, 
      CATOCUP!="Asalariados"                 ~ p530a )) %>%                       
  select(variables) 

saveRDS(Base, "bases_homog/peru.rds")
