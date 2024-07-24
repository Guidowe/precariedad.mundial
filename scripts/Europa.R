library(dplyr)
library(tidyverse)
library(Hmisc)

# Define variables and settings
variables <- c("PAIS", "ANO", "PERIODO", "WEIGHT", "SEXO", "EDAD",
               "CATOCUP", "SECTOR", "PRECAPT", "EDUC",
               "PRECAREG", "PRECATEMP", "PRECASALUD", "PRECASEG",
               "TAMA", "CALIF", "ING")

countries <- c("AT2018", "BG2018", "DE2017", "DK2018", "ES2018", "FR2018", "GR2018", 
               "IT2018", "NL2018", "NO2018", "PL2018", "PT2018", "RO2018", "SE2018", "UK2018")

country_names <- c(
  "AT2018" = "Austria", "BG2018" = "Bulgaria", "DE2017" = "Alemania", "DK2018" = "Dinamarca", 
  "ES2018" = "España", "FR2018" = "Francia", "GR2018" = "Grecia", "IT2018" = "Italia", 
  "NL2018" = "Países Bajos", "NO2018" = "Noruega", "PL2018" = "Polonia", "PT2018" = "Portugal", 
  "RO2018" = "Rumanía", "SE2018" = "Suecia", "UK2018" = "Reino Unido")

Base_LFS <- data.frame()

# Iterate over countries
for (country in countries) {
  print(paste("Procesando LFS para", country_names[country]))
  file_path <- paste0("bases/LFS/", country, "_y.csv")
  temp_data <- read.csv(file_path)
  temp_data <- temp_data %>% 
    filter(ILOSTAT %in% c(1, 4)) %>%
    mutate(
      ANO = ifelse(country == "DE2017", 2017, 2018),  #Usamos 2017 para alemania por problemas en variable de tamanio
      PERIODO = 1,
      PAIS = country_names[country],
      WEIGHT = COEFF * 1000,
      SEXO = case_when(
        SEX == 1 ~ "Varon",
        SEX == 2 ~ "Mujer"),
      EDAD = AGE,
      EDUC = case_when(
        HAT11LEV %in% 100:299 | HAT97LEV %in% 10:29 ~ "Primaria",
        HAT11LEV %in% 300:599 | HAT97LEV %in% 30:59 ~ "Secundaria",
        HAT11LEV %in% 600:899 | HAT97LEV %in% 60:89 ~ "Terciaria"),
      CATOCUP = case_when(
        STAPRO == 0 ~ "Cuenta propia",
        STAPRO == 3 ~ "Asalariados",
        TRUE ~ "Resto"),
      SECTOR = case_when(
        NACE1D == "O" ~ "Pub",
        NACE1D == "T" ~ "SD",
        TRUE ~ "Priv"),
      PRECAPT = case_when(
        FTPT == 2 & FTPTREAS == 5 ~ 1,
        TRUE ~ 0),
      PRECATEMP = case_when(
        TEMP == 2 & TEMPREAS == 2 ~ 1,
        TRUE ~ 0),
      PRECAREG = NA,
      PRECASEG = NA, 
      PRECASALUD = NA,
      TAMA = case_when(
        SIZEFIRM == 10 | SIZEFIRM == 14 ~ "Pequeño",
        SIZEFIRM == 11 | SIZEFIRM == 12 ~ "Mediano",
        SIZEFIRM == 13 ~ "Grande"),
      CALIF = case_when(
        ISCO1D == 900 | IS881D == 900 ~ "Baja",
        ISCO1D %in% c(400, 500, 600, 700, 800) | IS881D %in% c(400, 500, 600, 700, 800) ~ "Media",
        ISCO1D %in% c(100, 200, 300) | IS881D %in% c(100, 200, 300) ~ "Alta"), 
      ING = NA) %>% 
    select(variables, 'INCDECIL')
  
  Base_LFS <- bind_rows(Base_LFS, temp_data)
  
  rm(temp_data)
}

#Imputacion con SES

Carpeta <- "D:/SES/"

paises_SES <- c("ES2014", "FR2014", "UK2014", "DE2014", "IT2014", "PT2014", "DK2014", "BG2014", "RO2014")

country_names_SES <- c(
  "DE2014" = "Alemania", "DK2014" = "Dinamarca", "BG2014" = "Bulgaria",
  "ES2014" = "España", "FR2014" = "Francia",  "IT2014" = "Italia", 
  "PT2014" = "Portugal", "RO2014" = "Rumanía", "UK2014" = "Reino Unido")

Base_SES <- data.frame()

i <- 1
while (i < length(paises_SES) + 1) {
  print(paste("Procesando SES para", country_names_SES[paises_SES[i]]))
  Base <- readRDS(paste0(Carpeta, paises_SES[i], ".Rda"))
  
  Base <- Base     %>% 
    filter(nace!="XO")   %>%
    filter(nace!="XT")   %>%
    mutate(
      PAIS=country_names_SES[paises_SES[i]],
      WEIGHT=as.numeric(as.character(B52)), 
      CALIF= case_when(
        B23 %in% c(90:99, 900:999)  ~ "Baja",
        B23 %in% c(40:89, 400:899) ~ "Media", 
        B23 %in% c(10:39, 100:399) ~ "Alta"),                                                                   
      SALARIODB = as.numeric(as.character(B42))) %>% 
    select(PAIS, WEIGHT, CALIF, A12_CLASS, SALARIODB)

  #Calculo el límite de los deciles ponderados
  q <- wtd.quantile(Base$SALARIODB, probs = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE, weight = Base$WEIGHT)
  
  #Asigno decil a los casos
  Base <- Base %>%
    mutate(decil=case_when(   SALARIODB <= q[1]                    ~ 1, 
                              SALARIODB > q[1] & SALARIODB <= q[2] ~ 2,
                              SALARIODB > q[2] & SALARIODB <= q[3] ~ 3, 
                              SALARIODB > q[3] & SALARIODB <= q[4] ~ 4, 
                              SALARIODB > q[4] & SALARIODB <= q[5] ~ 5, 
                              SALARIODB > q[5] & SALARIODB <= q[6] ~ 6, 
                              SALARIODB > q[6] & SALARIODB <= q[7] ~ 7, 
                              SALARIODB > q[7] & SALARIODB <= q[8] ~ 8, 
                              SALARIODB > q[8] & SALARIODB <= q[9] ~ 9, 
                              SALARIODB > q[9] & SALARIODB <= q[10] ~ 10))
  
  Base_SES <- bind_rows(Base_SES, Base)
  
  i <- i + 1
  
}

PromedioDeciles <- Base_SES                                              %>%
  group_by(PAIS, decil)                                                  %>%
  summarise(promedio = weighted.mean(SALARIODB, WEIGHT, na.rm = TRUE)) %>%
  filter(!is.na(decil))  %>% 
  rename(INCDECIL = decil)

# Imputo promedio del decil del ingreso
Base_LFS <- Base_LFS %>%
  left_join(PromedioDeciles, by = c("PAIS", "INCDECIL")) %>% 
  mutate(ING = promedio) %>% 
  select(variables)

saveRDS(Base_LFS, "bases_homog/europa.rds")