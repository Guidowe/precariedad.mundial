library(tidyverse)
library(reldist)
library(xlsx)

#Carpeta <- "D:/SES/"
Carpeta <- "F:/SES/"
CarpetaRdos <- "C:/Users/facun/Documents/Investigación/1. LFS 2020 - CEPED Precariedad mundial/Resultados/"


variables <- c("B52", "A12", "A12_CLASS", "KEY_L", "B23", "A13", "B21", "B22_CLASS",
                "B25", "B27", "B28", "B42", "B43", "nace")

paises <- c("ES2014", "FR2014", "UK2014", "DE2014", "IT2014", "PT2014", "DK2014", "SE2014", "BG2014", "RO2014")

BaseUnificada <- data.frame()

i <- 1
while (i < length(paises) + 1) {

Base <- readRDS(paste0(Carpeta, paises[i], ".Rda"))
  
Base <- Base     %>% 
  select(one_of(variables))  %>%
  filter(nace!="XO")   %>%
  filter(nace!="XT")   %>%
  mutate(
    #AnoPais
    PAIS=paises[i],
    #Ponderador
    WEIGHT=B52, 
    #Educación
    EDUC= factor(case_when(#1. Baja                                 
      B25 =="G1"              ~ "Menor a secundaria", 
      #1. Media
      B25 %in% c("G2", "G3")  ~ "Secundaria completa",
      #1. Alta
      B25=="G4"               ~ "Superior completo", 
      TRUE              ~ "Ns/Nc"),
      levels= c("Menor a secundaria", "Secundaria completa", "Superior completo", "Ns/Nr")),
    #Calificación del puesto
    CALIF= factor(case_when( #1. Baja
      B23 %in% c(90:99, 900:999)  ~ "Baja",
      #2. Media
      B23 %in% c(40:89, 400:899) ~ "Media", 
      #3. Alta
      B23 %in% c(10:39, 100:399) ~ "Alta", 
      TRUE           ~ "Ns/Nr"),                                                                   
      levels= c("Baja", "Media", "Alta", "Ns/Nr")))

Base$WEIGHT <- as.numeric(as.character(Base$WEIGHT))
Base$SALARIODB <- as.numeric(as.character(Base$B42))
Base$SALARIOHORARIODB <- as.numeric(as.character(Base$B43))


Base <- Base %>% select(PAIS, WEIGHT, EDUC, CALIF, A12_CLASS, SALARIODB, SALARIOHORARIODB)

q <- wtd.quantile (Base$SALARIODB, q=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE, weight=Base$WEIGHT)

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

q <- wtd.quantile (Base$SALARIOHORARIODB, q=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE, weight=Base$WEIGHT)

Base <- Base %>%
  mutate(decilhorario=case_when(   SALARIOHORARIODB <= q[1]                    ~ 1, 
                            SALARIOHORARIODB > q[1] & SALARIOHORARIODB <= q[2] ~ 2,
                            SALARIOHORARIODB > q[2] & SALARIOHORARIODB <= q[3] ~ 3, 
                            SALARIOHORARIODB > q[3] & SALARIOHORARIODB <= q[4] ~ 4, 
                            SALARIOHORARIODB > q[4] & SALARIOHORARIODB <= q[5] ~ 5, 
                            SALARIOHORARIODB > q[5] & SALARIOHORARIODB <= q[6] ~ 6, 
                            SALARIOHORARIODB > q[6] & SALARIOHORARIODB <= q[7] ~ 7, 
                            SALARIOHORARIODB > q[7] & SALARIOHORARIODB <= q[8] ~ 8, 
                            SALARIOHORARIODB > q[8] & SALARIOHORARIODB <= q[9] ~ 9, 
                            SALARIOHORARIODB > q[9] & SALARIOHORARIODB <= q[10] ~ 10))

BaseUnificada <- bind_rows(BaseUnificada, Base)

i <- i + 1

}

saveRDS(BaseUnificada, paste0(Carpeta, "BaseUnificada.Rda"))
Base <- BaseUnificada
remove(BaseUnificada)


### ESTIMACION DE RESULTADOS

Base <- readRDS(paste0(Carpeta, "BaseUnificada.Rda"))           

CasosTamaCalif <- Base                                                       %>% 
  filter(CALIF!="Ns/Nr", EDUC!="Ns/Nr")                                      %>%
  group_by(PAIS, A12_CLASS, CALIF)                                           %>%
  summarise('Casos'      =n(), 
            'Ponderados' =sum(WEIGHT, na.rm = TRUE), 
            'Salario'    =weighted.mean(SALARIODB, WEIGHT, na.rm = TRUE))    %>% 
  mutate('Salario'= case_when(PAIS=="UK2014" ~ Salario/0.80612, 
                             PAIS=="DK2014" ~ Salario/7.4533,
                             PAIS=="SE2014" ~ Salario/9.0985, 
                             PAIS=="BG2014" ~ Salario/1.9558, 
                             PAIS=="RO2017" ~ Salario/4.4437, 
                             TRUE           ~ Salario))  

CasosTamaEduc <- Base                                                       %>% 
  filter(CALIF!="Ns/Nr", EDUC!="Ns/Nr")                                     %>%
  group_by(PAIS, A12_CLASS, EDUC)                                           %>%
  summarise('Casos'      =n(), 
            'Ponderados' =sum(WEIGHT, na.rm = TRUE), 
            'Salario'    =weighted.mean(SALARIODB, WEIGHT, na.rm = TRUE))   %>% 
  mutate('Salario'= case_when(PAIS=="UK2014" ~ Salario/0.80612, 
                             PAIS=="DK2014" ~ Salario/7.4533,
                             PAIS=="SE2014" ~ Salario/9.0985, 
                             PAIS=="BG2014" ~ Salario/1.9558, 
                             PAIS=="RO2017" ~ Salario/4.4437, 
                             TRUE           ~ Salario))  

### CALCULO DE DECILES PARA ASALARIADOS

paises <- c("ES2014", "FR2014", "UK2014", "DE2014", "IT2014", "PT2014", "DK2014", "SE2014", "BG2014", "RO2014")

#LIMITES DECILES

i <- 1
Deciles         <- data.frame(row.names = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"))
while (i < length(paises) + 1) {

BaseP <- Base %>% filter(PAIS==paises[i])
q <- wtd.quantile (BaseP$SALARIODB, q=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE, weight=BaseP$WEIGHT)
Deciles[paises[i]] <- q
i <- i + 1
}

Deciles <- Deciles %>%
  mutate(UK2014=UK2014/0.80612,
         DK2014=DK2014/7.4533,
         SE2014=SE2014/9.0985, 
         BG2014=BG2014/1.9558, 
         RO2014=RO2014/4.4437)  

#LIMITES DECILES HORARIOS

i <- 1
DecilesHorario <- data.frame(row.names = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"))
while (i < length(paises) + 1) {
    BaseP <- Base %>% filter(PAIS==paises[i])
  q <- wtd.quantile (BaseP$SALARIOHORARIODB, q=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), na.rm = TRUE, weight=BaseP$WEIGHT)
  DecilesHorario[paises[i]] <- q
  i <- i + 1
}

DecilesHorario <- DecilesHorario %>%
  mutate(UK2014=UK2014/0.80612,
         DK2014=DK2014/7.4533,
         SE2014=SE2014/9.0985, 
         BG2014=BG2014/1.9558, 
         RO2014=RO2014/4.4437) 

### PROMEDIO DE CADA DECIL

PromedioDeciles <- Base                                                  %>%
  group_by(PAIS, decil)                                                  %>%
  summarise('Promedio' = weighted.mean(SALARIODB, WEIGHT, na.rm = TRUE)) %>%
  filter(!is.na(decil))                                                  %>% 
  mutate(Promedio= case_when(PAIS=="UK2014" ~ Promedio/0.80612, 
                             PAIS=="DK2014" ~ Promedio/7.4533,
                             PAIS=="SE2014" ~ Promedio/9.0985, 
                             PAIS=="BG2014" ~ Promedio/1.9558, 
                             PAIS=="RO2017" ~ Promedio/4.4437, 
                             TRUE           ~ Promedio))                 %>% 
  spread(PAIS, Promedio)                                                 %>% 
  select(-decil)

PromedioHorarioDeciles <- Base                                                  %>%
  group_by(PAIS, decilhorario)                                                  %>%
  summarise('Promedio' = weighted.mean(SALARIOHORARIODB, WEIGHT, na.rm = TRUE)) %>%
  filter(!is.na(decilhorario))                                                  %>% 
  mutate(Promedio= case_when(PAIS=="UK2014" ~ Promedio/0.80612, 
                             PAIS=="DK2014" ~ Promedio/7.4533,
                             PAIS=="SE2014" ~ Promedio/9.0985, 
                             PAIS=="BG2014" ~ Promedio/1.9558, 
                             PAIS=="RO2017" ~ Promedio/4.4437, 
                             TRUE           ~ Promedio))                        %>% 
  spread(PAIS, Promedio)                                                        %>% 
  select(-decilhorario)


write.xlsx(as.data.frame(PromedioHorarioDeciles), paste0(CarpetaRdos, "ResultadosSES.xlsx"), sheetName = "PromedioHorarioDeciles", append=TRUE, row.names = FALSE )
write.xlsx(as.data.frame(PromedioDeciles), paste0(CarpetaRdos, "ResultadosSES.xlsx"), sheetName = "PromedioDeciles", append=TRUE, row.names = FALSE )
write.xlsx(as.data.frame(Deciles), paste0(CarpetaRdos, "ResultadosSES.xlsx"), sheetName = "LimitesDeciles", append=TRUE, row.names = FALSE)
write.xlsx(as.data.frame(CasosTamaCalif), paste0(CarpetaRdos, "ResultadosSES.xlsx"), sheetName = "PerfilesTamaCalif", append=TRUE, row.names = FALSE)
write.xlsx(as.data.frame(CasosTamaEduc), paste0(CarpetaRdos, "ResultadosSES.xlsx"), sheetName = "PerfilesTamaEduc", append=TRUE, row.names = FALSE)
