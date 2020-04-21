library(tidyverse)
library(dplyr)

Carpeta <- "F:/LFS/" 

#Abro una base cualquiera (DE2017)
Base <- read.csv(paste0(Carpeta, "DE2017_y", ".csv"))

#Genero variables con factor y levels

variables <- c("YEAR", "COEFF", "WSTATOR", "SEX", "AGE", "COUNTRYB", "SEEKWORK", "AVAILBLE", "STAPRO", "NACE1D", 
               "SIZEFIRM", "ISCO1D", "ISCO2D", "ISCO3D", "IS881D", "YSTARTWK", "MSTARTWK", "EXIST2J", "FTPT", 
               "FTPTREAS", "TEMP", "TEMPREAS", "WISHMORE", "HWACTUAL", "HAT11LEV", "HAT97LEV", "HATLEV1D", "COUNTRYB", 
               "DEGURBA", "ILOSTAT", "INCDECIL")
Base <- Base                               %>% 
  select(one_of(variables))  

Base <- Base         %>%
  mutate(
    #Ponderador
    WEIGHT=COEFF*1000, 
    #Condicion de actividad
    COND= factor(case_when(# 1. Ocupado (ILOSTAT 1. Employed, 4. Compulsory military service)
      ILOSTAT==1 | ILOSTAT==4     ~ "Ocupado",
      # 2. Desocupados que encontraron trabajo y estan disponibles para trabajar "inmediatamente" (en 2 semanas)
      ILOSTAT==2                  ~ "Desocupado",
      # 3. Inactivos (3.Inactive, 9. Persons less than 15 years old)
      ILOSTAT==3 | ILOSTAT==9     ~ "Inactivo",
      TRUE                        ~ "Ns/Nc"),
      levels= c("Ocupado", "Desocupado", "Inactivo", "Ns/Nc")),
    #Tamaño establecimiento
    TAMA= factor(case_when( #1. 10 o menos
      SIZEFIRM==10 | SIZEFIRM==14 ~ "10 o menos",
      #2. 11 a 49
      SIZEFIRM==11 | SIZEFIRM==12 ~ "11 a 49",
      #3. Mas de 50
      SIZEFIRM==13                ~ "Mas de 50",
      TRUE                        ~ "Ns/Nc"),
      levels= c("10 o menos", "11 a 49", "Mas de 50", "Ns/Nc")),
    #Educación
    EDUC= factor(case_when(#1. Baja                                 
      HAT11LEV %in% 100:299 | HAT97LEV %in% 100:299   ~ "Menor a secundaria", 
      #1. Media
      HAT11LEV %in% 300:599 | HAT97LEV %in% 300:599   ~ "Secundaria completa",
      #1. Alta
      HAT11LEV %in% 600:899 | HAT97LEV %in% 600:899   ~ "Superior completo", 
      TRUE              ~ "Ns/Nc"),
      levels= c("Menor a secundaria", "Secundaria completa", "Superior completo", "Ns/Nc")),
    #Calificación del puesto
    CALIF= factor(case_when( #1. Baja
      ISCO1D == 900                          | IS881D == 900                           ~ "Baja",
      #2. Media
      ISCO1D %in% c(400, 500, 600, 700, 800) | IS881D  %in% c(400, 500, 600, 700, 800) ~ "Media", 
      #3. Alta
      ISCO1D %in% c(100, 200, 300)           | IS881D  %in% c(100, 200, 300)           ~ "Alta", 
      TRUE                                                                             ~  "Ns/Nc"), 
      levels= c("Baja", "Media", "Alta", "Ns/Nc")))


### Trato de calcular cualquier cosa y me da error de implicit NA

EducCalif      <-  Base                                %>%
  filter(COND==1)    %>%
  group_by(EDUC, CALIF)                                %>%
  summarise("Casos"      = sum(WEIGHT))


