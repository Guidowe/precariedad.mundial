library(tidyverse)
variables<- c("PAIS","ANO","PERIODO","WEIGHT","SEXO","EDAD",
              "CATOCUP","SECTOR","PRECAPT","EDUC",
              "PRECAREG","PRECATEMP","PRECASALUD","PRECASEG",
              "TAMA","CALIF","ING") 
URU <- readRDS("Bases/uruguay_2019.RDS")    # Version liviana de la base original en este repositorio
Base <- URU %>%
  # Filtro areas rurales
  filter(region_4 != 4) %>%
  # Filtro Ocupados
  filter(pobpcoac ==2) %>%
  mutate(
    ANO = 2019,
    PERIODO = 1, 
    PAIS = "Uruguay",
    WEIGHT = pesoano,
    SEXO = case_when(
      e26 == 1 ~  "Varon", 
      e26 == 2 ~ "Mujer"
    ),
    EDAD = e27,
    EDUC = case_when(
      e49 == 2 ~ "Primaria",
      e197_1 == 1 ~ "Primaria", 
      e201_1 == 1 ~ "Secundaria", 
      e212_1 == 1 ~ "Terciaria", 
      e215_1 == 1 ~ "Terciaria", 
      e218_1 == 1 ~ "Terciaria", 
      e221_1 == 1 ~ "Terciaria", 
      e224_1 == 1 ~ "Terciaria"
    ),
    CATOCUP = case_when(
      f71_2 == "9111" ~ "Asalariados",
      f73 %in% 5:6 ~ "Cuenta propia",
      f73 ==4 ~ "Patron",
      f73 %in% c(7, 8) ~ "Resto", 
      f73 %in% 1:3 ~ "Asalariados"
    ),
    SECTOR = case_when(
      f71_2 == "9111" ~ "SD",
      f73 %in% c(1, 3, 4, 5, 6, 7) ~ "Priv", 
      f73 %in% c(2, 8) ~ "Pub"

    ),
    PRECAPT = case_when(
      f85 < 35 & f85 > 0 & f102 == 1 ~ 1,
      f85 < 35 & f85 > 0 & f102 == 2 ~ 0, 
      f85 > 34 ~ 0
    ),
    PRECATEMP = NA,                  
    PRECASEG = case_when( 
      f82 == 2 & CATOCUP == "Asalariados" ~ 1,       
      f82 == 1 & CATOCUP == "Asalariados" ~ 0,   
      f263 == 2 & CATOCUP == "Cuenta propia" ~ 1,    
      f263 == 1 & CATOCUP == "Cuenta propia" ~ 0
    ),  
    PRECAREG = NA,
    PRECASALUD = case_when(
      e45_1 == 2 & e45_2 == 2 & e45_3 == 2 & e45_4 == 2 & e45_5 == 2 & e45_6 == 2 & e45_7 == 2 ~ 1, 
      e45_1 == 1 | e45_2 == 1 | e45_3 == 1 | e45_4 == 1 | e45_5 == 1 | e45_6 == 1 | e45_7 == 1 ~ 0
    ), 
    TAMA = case_when( 
      f77 %in% 1:3 ~ "Pequeño", 
      f77 %in% 6:7 ~ "Mediano", 
      f77 == 5 ~ "Grande",  
      TRUE ~ "Ns/Nc"
    ),
    CALIF = case_when( 
      f71_2 %in% 9000:9999 ~ "Baja",
      f71_2 %in% 4000:8999 ~ "Media", 
      f71_2 %in% 1000:3999 ~ "Alta", 
      TRUE ~ "Ns/Nc"
    ), 
    ING = as.numeric(sub(",", ".", PT2)), 
    ING = case_when(
      ING == 0 ~ NA_real_,
      ING > 750000 ~ NA_real_,
      TRUE ~ ING
    )
  ) %>%
  select(variables)

saveRDS(Base, "bases_homog/uruguay.RDS")

