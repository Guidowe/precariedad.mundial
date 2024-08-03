europa <- readRDS('bases_homog/europa.rds')
library(tidyverse)

tabla <- europa %>% filter(SECTOR=='Priv') %>% 
  group_by(PAIS, TAMA, CALIF) %>% 
  summarise(promedio = weighted.mean(ING, WEIGHT, na.rm = TRUE))
