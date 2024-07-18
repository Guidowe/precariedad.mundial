options(scipen=99)
library(tidyverse)

europa <- readRDS('bases_homog/europa.rds')
bolivia <- readRDS('bases_homog/bolivia.rds')
uruguay <- readRDS('bases_homog/uruguay.rds')
brasil <- readRDS('bases_homog/brasil.rds')

Base <- rbind(europa, bolivia, uruguay, brasil)

#Remuevo todo excepto Base
rm(list = setdiff(ls(), "Base"))

#Resultados por periodo (sin agregar anualmente)
Resultados <- Base                                 %>%
  filter(SECTOR == "Priv", !is.na(CALIF), !is.na(TAMA))   %>%
  group_by(PAIS, PERIODO, TAMA, CALIF)                             %>%
  summarise('total.ocupados'                       = sum(WEIGHT, na.rm=TRUE),
            'tasa.asalarizacion'                   = sum(WEIGHT[CATOCUP=="Asalariados"], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'total.asal'                           = sum(WEIGHT[CATOCUP=="Asalariados"], na.rm=TRUE),
            'tasa.partime.asal'                    = sum(WEIGHT[PRECAPT==1 & CATOCUP=="Asalariados"], na.rm=TRUE)/sum(WEIGHT[CATOCUP=="Asalariados"], na.rm=TRUE),         
            'tasa.temp.asal'                       = sum(WEIGHT[PRECATEMP==1 & CATOCUP=="Asalariados"], na.rm=TRUE)/sum(CATOCUP=="Asalariados", na.rm=TRUE), 
            'tasa.1.asalariados'                   = sum(WEIGHT[PRECAPT==1 | PRECATEMP==1], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'tasa.2.asalariados'                   = sum(WEIGHT[PRECAPT==1 & PRECATEMP==1], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'total.tcp'                            = sum(WEIGHT[CATOCUP=="Cuenta propia"], na.rm=TRUE),       
            'tasa.parttime.tcp'                    = sum(WEIGHT[PRECAPT==1 & CATOCUP=="Cuenta propia"], na.rm=TRUE)/sum(WEIGHT[CATOCUP=="Cuenta propia"], na.rm=TRUE))  %>%
  ungroup() %>%
  group_by(PAIS) %>%
  mutate('particip.ocup'          = total.ocupados/sum(total.ocupados),     
         'particip.asal'          = total.asal/sum(total.ocupados), 
         'particip.tcp'           = total.tcp/sum(total.ocupados))

