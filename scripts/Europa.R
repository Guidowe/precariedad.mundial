library(dplyr)
library(tidyverse)
library(Hmisc)
library(haven)

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

# Itera sobre las bases de la LFS
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

Carpeta <- "bases/SES/"

# Set de paises disponibles en SES (no hay data para Austria, Belgica, Grecia y UK)
paises_SES <- c("SES2018_DE.dta", "SES2018_DK.dta", "SES2018_ES.dta", "SES2018_FR.dta", "SES2018_IT.dta", "SES2018_NL.dta", "SES2018_NO.dta", "SES2018_PL.dta", "SES2018_PT.dta", "SES2018_RO.dta", "SES2018_SE.dta")

# Mapeo de nombre de paises
country_names_SES <- c(
  "SES2018_DE.dta" = "Alemania",
  "SES2018_DK.dta" = "Dinamarca",
  "SES2018_ES.dta" = "España",
  "SES2018_FR.dta" = "Francia",
  "SES2018_IT.dta" = "Italia",
  "SES2018_NL.dta" = "Países Bajos",
  "SES2018_NO.dta" = "Noruega",
  "SES2018_PL.dta" = "Polonia",
  "SES2018_PT.dta" = "Portugal",
  "SES2018_RO.dta" = "Rumanía",
  "SES2018_SE.dta" = "Suecia"
)

Base_SES <- data.frame()

i <- 1
while (i < length(paises_SES) + 1) {
  print(paste("Procesando SES para", country_names_SES[paises_SES[i]]))
  Base <- read_dta(paste0(Carpeta, paises_SES[i]))
  
  Base <- Base     %>% 
    filter(nace!="XO")   %>%
    filter(nace!="XT")   %>%
    mutate(
      PAIS=country_names_SES[paises_SES[i]],
      WEIGHT=as.numeric(as.character(b52)), 
      CALIF= case_when(
        b23 %in% c(90:99, 900:999)  ~ "Baja",
        b23 %in% c(40:89, 400:899) ~ "Media", 
        b23 %in% c(10:39, 100:399) ~ "Alta"),                                                                   
      SALARIODB = as.numeric(as.character(b42))) %>% 
    select(PAIS, WEIGHT, CALIF, a12_class, SALARIODB)

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