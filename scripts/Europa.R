library(dplyr)

# Define variables and settings
variables <- c("PAIS", "ANO", "PERIODO", "WEIGHT", "SEXO", "EDAD",
               "CATOCUP", "COND", "SECTOR", "PRECAPT", "EDUC",
               "PRECAREG", "PRECATEMP", "PRECASALUD", "PRECASEG",
               "TAMA", "CALIF", "ING")

countries <- c("AT2018", "BG2018", "DE2017", "DK2018", "ES2018", "FR2018", "GR2018", 
               "IT2018", "NL2018", "NO2018", "PL2018", "PT2018", "RO2018", "SE2018", "UK2018")
variables_raw <- c("COEFF", "ILOSTAT", "SEX", "AGE", "STAPRO", "NACE1D", "FTPT", 
                   "FTPTREAS", "TEMP", "TEMPREAS", "SIZEFIRM", "HAT11LEV", "HAT97LEV", "ISCO1D", "IS881D")

country_names <- c(
  "AT2018" = "Austria", "BG2018" = "Bulgaria", "DE2017" = "Alemania", "DK2018" = "Dinamarca", 
  "ES2018" = "España", "FR2018" = "Francia", "GR2018" = "Grecia", "IT2018" = "Italia", 
  "NL2018" = "Países Bajos", "NO2018" = "Noruega", "PL2018" = "Polonia", "PT2018" = "Portugal", 
  "RO2018" = "Rumanía", "SE2018" = "Suecia", "UK2018" = "Reino Unido")

# Initialize an empty dataframe to store results
Base <- data.frame()

# Iterate over countries
for (country in countries) {
  print(paste("Procesando país:", country_names[country]))
  file_path <- paste0("bases/LFS/", country, "_y.csv")
  temp_data <- read.csv(file_path)
  temp_data <- temp_data %>% 
    select(all_of(variables_raw)) %>%
    filter(ILOSTAT %in% c(1, 2, 4)) %>%
    mutate(
      ANO = 2018,
      PERIODO = NA,
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
      COND = case_when(
        ILOSTAT == 1 | ILOSTAT == 4 ~ "Ocupado",
        ILOSTAT == 2 ~ "Desocupado"),
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
    select(variables)
  
  Base <- bind_rows(Base, temp_data)
  
  rm(temp_data)
}

saveRDS(Base, "bases_homog/europa.rds")



