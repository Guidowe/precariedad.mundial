library(tidyverse)

CarpetaBasesUnificadas <- "F:/LFS/BasesUnificadas/" 

### PROMEDIO DE CADA DECIL

SES <- readRDS("F:/SES/BaseUnificada.Rda")  

PromedioDeciles <- SES                                                   %>%
  group_by(PAIS, decil)                                                  %>%
  summarise('Promedio' = weighted.mean(SALARIODB, WEIGHT, na.rm = TRUE)) %>%
  filter(!is.na(decil))                                                  %>% 
  spread(PAIS, Promedio)                                                 %>% 
  select(-decil)

PromedioHorarioDeciles <- SES                                                   %>%
  group_by(PAIS, decilhorario)                                                  %>%
  summarise('Promedio' = weighted.mean(SALARIOHORARIODB, WEIGHT, na.rm = TRUE)) %>%
  filter(!is.na(decilhorario))                                                  %>% 
  spread(PAIS, Promedio)                                                        %>% 
  select(-decilhorario)

remove(SES)

#Saco a SE sin datos para deciles
PromedioDeciles <- PromedioDeciles %>% select(-SE2014)
PromedioHorarioDeciles <- PromedioHorarioDeciles %>% select(-SE2014)
#No incluyo a Grecia que no está en SES
countries <- c("BG", "DE", "DK", "ES", "FR", "IT", "PT",  "RO", "UK")
PPP <- c(1, 0.834523744, 8.552694151, 0.738510185, 0.832556816, 0.853587053, 0.687558871, 1.217568971, 0.656065473)


i <- 1
while (i < length(countries) + 1) {


Base <- readRDS(paste0(CarpetaBasesUnificadas, countries[i], "2008-2018.Rda"))

Base <- Base  %>%
  mutate(salarioNAC= case_when(
                            INCDECIL==1  ~ PromedioDeciles[[1,i]],
                            INCDECIL==2  ~ PromedioDeciles[[2,i]],
                            INCDECIL==3  ~ PromedioDeciles[[3,i]],
                            INCDECIL==4  ~ PromedioDeciles[[4,i]],
                            INCDECIL==5  ~ PromedioDeciles[[5,i]],
                            INCDECIL==6  ~ PromedioDeciles[[6,i]],
                            INCDECIL==7  ~ PromedioDeciles[[7,i]],
                            INCDECIL==8  ~ PromedioDeciles[[8,i]],
                            INCDECIL==9  ~ PromedioDeciles[[9,i]],
                            INCDECIL==10 ~ PromedioDeciles[[10,i]]))

Base <- Base  %>%
  mutate(salariohorarioNAC= case_when(
    INCDECIL==1  ~ PromedioHorarioDeciles[[1,i]],
    INCDECIL==2  ~ PromedioHorarioDeciles[[2,i]],
    INCDECIL==3  ~ PromedioHorarioDeciles[[3,i]],
    INCDECIL==4  ~ PromedioHorarioDeciles[[4,i]],
    INCDECIL==5  ~ PromedioHorarioDeciles[[5,i]],
    INCDECIL==6  ~ PromedioHorarioDeciles[[6,i]],
    INCDECIL==7  ~ PromedioHorarioDeciles[[7,i]],
    INCDECIL==8  ~ PromedioHorarioDeciles[[8,i]],
    INCDECIL==9  ~ PromedioHorarioDeciles[[9,i]],
    INCDECIL==10 ~ PromedioHorarioDeciles[[10,i]]))

Base <- Base %>%
  mutate('salario'=  salarioNAC/PPP[i], 
         'salariohorario'= salariohorarioNAC/PPP[i])
    
saveRDS(Base, paste0(CarpetaBasesUnificadas, countries[i], "2008-2018.Rda"))    

i <- i + 1

}


