library(tidyverse)
variables<- c("PAIS","ANO","PERIODO","WEIGHT","SEXO","EDAD",
              "CATOCUP","COND","SECTOR","PRECAPT","EDUC",
              "PRECAREG","PRECATEMP","PRECASALUD","PRECASEG",
              "TAMA","CALIF","ING") 

Base <- readRDS("bases/Bolivia_1a4T2019.RDS")
Base$s2_18[is.na(Base$s2_18)] = 0         #Saco NAs de variable para categoria ocupacional
Base$s2_22[is.na(Base$s2_22)] = 0
Base <- Base %>% 
  # Filtro areas rurales
  filter(area != 2) %>%
  # Filtro PEA
  filter(condact == 1 | pead ==1) %>%
  mutate(
    ANO = 2019,
    PERIODO = trimestre,
    PAIS = "Bolivia",
    WEIGHT = fact_trim,
    SEXO = case_when(
      s1_02 == 1 ~ "Varon", 
      s1_02 == 2 ~ "Mujer"),
    EDAD = s1_03a,
    EDUC = case_when(
      niv_ed %in% 0:1 ~ "Sin instruccion", 
      niv_ed %in% 2:3 ~ "Primaria", 
      niv_ed  == 4 ~ "Secundaria", 
      niv_ed  == 5 ~ "Terciaria"),
    CATOCUP = case_when(
      s2_18 == 2 ~ "Cuenta propia", 
      s2_18 %in% 3:7 ~ "Resto", 
      s2_18 == 1 ~ "Asalariados",       
      TRUE ~ "Resto"),
    SECTOR = case_when(
      s2_22 == 1 ~ "Pub", 
      s2_22 %in% 2:6 ~ "Priv", 
      s2_18 == 7 ~ "SD"),
    COND = case_when(
      condact == 1 ~ "Ocupado", 
      pead ==1 ~ "Desocupado"),
    # COND = factor(case_when(
    #   condact == 1 ~ "Ocupado",
    #   condact %in% 2:5 ~ "Desocupado",
    #   TRUE ~ "Ns/Nc"),
    # levels = c("Ocupado", "Desocupado", "Ns/Nc")),
    PRECAPT = case_when(
      phrs < 35 & phrs > 0 & s2_57 == 1 ~ 1,         
      phrs < 35 & phrs > 0 & s2_57 == 2 ~ 0, #Part-time voluntario
      phrs > 34 ~ 0), #Tiempo completo
    PRECATEMP = case_when(
      s2_21 %in% 1:2 ~ 1,              
      s2_21 %in% 3:5 ~ 0),
    PRECAREG = case_when(
      s2_21 %in% c(2, 3, 5) ~ 1,        
      s2_21 %in% c(1, 4) ~ 0),
    PRECASEG = case_when(
      s2_64 == 2 ~ 1,               
      s2_64 == 1 ~ 0),
    PRECASALUD = case_when(
      s2_36a == 2 ~ 1,              
      s2_36a == 1 ~ 0),                                     
    TAMA = case_when( 
      s2_26 %in% 1:10 ~ "Pequeño",
      s2_26 %in% 11:50 ~ "Mediano",
      s2_26 > 50 ~ "Grande"),
    CALIF = case_when( 
      cob_op == 9 ~ "Baja",
      cob_op %in% 4:8 ~ "Media", 
      cob_op %in% 1:3 ~ "Alta"), 
    ING = yprilab, 
    ING = case_when(
      ING == 0 ~ NA_real_, 
      TRUE ~ ING)) %>% 
  select(variables)

saveRDS(Base, "bases_homog/bolivia.rds")


