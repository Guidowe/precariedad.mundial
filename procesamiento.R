# Importacion ####
options(scipen=99)
library(tidyverse)
library(openxlsx)

rutas <- list.files("Bases_homog/",full.names = T,pattern = ".rds")
Base <- data.frame()
for(i in rutas){
  base_temp<- readRDS(i) %>%
    mutate(PERIODO = as.character(PERIODO),
           EDAD = as.numeric(EDAD),
           ING = as.numeric(ING))
Base <-   bind_rows(Base,base_temp)
print(i)
}
table(Base$ANO,Base$PAIS)

# Fuentes complementarias ####
Paises <- read.xlsx("Fuentes Complementarias/Prod y Salarios.xlsx",
                    sheet = "Paises") 
IPC_2005 <- read.xlsx("Fuentes Complementarias/Prod y Salarios.xlsx",
                      sheet = "IPC (2005)") %>%
  rename(ANO4 = X1)%>%
  pivot_longer(cols = 2:ncol(.),
               names_to = "nombre.pais",
               values_to = "IPC_2005") %>%
  mutate(nombre.pais = str_replace_all(nombre.pais,"[[:punct:] ]+",replacement = " "))

PPA_WB <- read.csv("Fuentes Complementarias/PPA.csv") %>%
  rename(COD.OCDE = Country.Code) %>%
  filter(Classification.Code == "PPPGlob",Series.Code == 9020000) %>%
  pivot_longer(cols = 7:ncol(.),
               names_to = "Año",
               values_to = "PPA.BENCHMARK.2017") %>%
  mutate(ANO4 = as.numeric(str_extract(Año,"[[:digit:]]{4}")))

PPA_WB_IPC <- IPC_2005 %>% 
  left_join(Paises) %>% 
  left_join(PPA_WB %>% select(COD.OCDE,ANO4,PPA.BENCHMARK.2017))  %>%
  mutate(PPA.BENCHMARK.2017 = as.numeric(as.character(PPA.BENCHMARK.2017))) %>%
  group_by(ANO4) %>%
  mutate(IPC_2005_USA = IPC_2005[nombre.pais == "Estados Unidos"]) %>%
  group_by(nombre.pais) %>%
  mutate(PPA.BENCHMARK.2017.EXTRAPOLADO =
           case_when(!is.na(PPA.BENCHMARK.2017)~PPA.BENCHMARK.2017,
                     is.na(PPA.BENCHMARK.2017)~
                       PPA.BENCHMARK.2017[ANO4 == 2017]*
                       (IPC_2005/IPC_2005[ANO4 == 2017])/
                       (IPC_2005_USA/IPC_2005_USA[ANO4 == 2017])
           )) %>% 
  ungroup()

# Chequeos ####
#Cosas a resolver - CATOCUP y SECTOR
table(Base$CATOCUP,Base$SECTOR,useNA = "always")
table(Base$CATOCUP,Base$PAIS,useNA = "always")

for (pais in unique(Base$PAIS)){
  subset_data <- Base %>% filter(PAIS == pais)
  cat("PAIS:", pais, "\n")
  print(table(subset_data$CATOCUP, subset_data$SECTOR, useNA = "always"))}

Base$CATOCUP[Base$CATOCUP=="Asalariados"] <- "Asalariado"
Base$CATOCUP[Base$CATOCUP=="Cuenta Propia"] <- "Cuenta propia"

chequeo <- Base %>%
  group_by(PAIS,CATOCUP,SECTOR) %>% 
  count()

#En algunos países los CP no quedan dentro del sector privado

# Base PPA ####
Base_PPA<- Base %>% 
#  select(-PPA,-ING_PPA) %>% 
  left_join(
    PPA_WB_IPC %>%
      select(PAIS = nombre.pais,
             PPA= PPA.BENCHMARK.2017.EXTRAPOLADO,
             ANO = ANO4
      )) %>% 
  mutate(ING_PPA = ING/PPA)

## Chequeos ####
sin_data_ingresos <- Base_PPA %>% 
  group_by(PAIS) %>% 
  summarise(sin_ingresos = all(is.na(ING)))

ingprom <- Base_PPA %>% 
  group_by(ANO,PAIS) %>% 
  summarise(PPA_mean =mean(ING_PPA,na.rm = T),
            PPA_median = median(ING_PPA,na.rm = T))

#Para levantar una base en particular y unirla
# Base_PPA<- readRDS("base_homogenea.RDS")
# Base_PPA <- base_truco %>%
#   bind_rows(Base_PPA)

#Exportacion base ####
saveRDS(object = Base_PPA,"base_homogenea.RDS")
