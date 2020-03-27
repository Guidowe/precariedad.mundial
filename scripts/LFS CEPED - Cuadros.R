library(dplyr)
  
# Script para usar con la base unificada
  
NivelEduc <-  Base                                                   %>%
  filter(EDUC!=0)                                                    %>%
  group_by(COUNTRY, EDUC)                                            %>%
  summarise("Desocupados"  = sum(WEIGHT[COND==2], na.rm=TRUE), 
            "Ocupados"     = sum(WEIGHT[COND==1], na.rm=TRUE),
            "Tasa Desocup" = Desocupados/Ocupados)

  
Tama      <-  Base                                       %>%
  filter(COND==1 & TAMA!=0)                              %>%
  group_by(COUNTRY, TAMA)                                %>%
  summarise("Casos"      = n(),
            "Ponderados" = sum(WEIGHT, na.rm=TRUE))

Calif     <-  Base                                       %>%
  filter(COND==1 & CALIF!=0)                             %>%
  group_by(COUNTRY, CALIF)                               %>%
  summarise("Casos"      = n(),
            "Ponderados" = sum(WEIGHT, na.rm=TRUE))


EducTama      <-  Base                                   %>%
  filter(COND==1 & TAMA!=0 & EDUC!=0)                    %>%
  group_by(COUNTRY, TAMA, EDUC)                          %>%
  summarise("Ocupados" = sum(WEIGHT, na.rm=TRUE))

CalifTama      <-  Base                                  %>%
  filter(COND==1 & TAMA!=0 & CALIF!=0)                   %>%
  group_by(COUNTRY, TAMA, CALIF)                         %>%
  summarise("Ocupados" = sum(WEIGHT, na.rm=TRUE))
  
#Tasa de precareidad segun perfiles (porcentaje sobre el total de cada perfil)
  
PrecaPorTamayCalif  <-  Base                                       %>%
  filter(COND==1 & STAPRO==3 & TAMA!=0 & CALIF!=0)                 %>%
  group_by(COUNTRY, TAMA, CALIF)                                   %>%
  summarise('Precariedad'        = sum(WEIGHT[PRECA==1], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE))       

PrecaPorTamayEduc  <-  Base                                        %>%
  filter(COND==1 & STAPRO==3 & TAMA!=0 & EDUC!=0)                  %>%
  group_by(COUNTRY, TAMA, EDUC)                                    %>%
  summarise('Precariedad'        = sum(WEIGHT[PRECA==1], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE))    
 
