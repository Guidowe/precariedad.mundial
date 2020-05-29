


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


Base <- readRDS("F:/LFS/BasesUnificadas/SeleccionPaises2014.Rda")

BaseImputada <- data.frame()

#Saco a SE sin datos para deciles
PromedioDeciles <- PromedioDeciles %>% select(-SE2014)
PromedioHorarioDeciles <- PromedioHorarioDeciles %>% select(-SE2014)
#No incluyo a Grecia que no está en SES
countries <- c("BG", "DE", "DK", "ES", "FR", "IT", "PT",  "RO", "UK")

#Genero variable para identificar el pais sin ano
Base <-  Base                        %>%
  mutate(pais=substr(COUNTRY, 1, 2))   


i <- 1
while (i < length(countries) + 1) {

#Imputo deciles pais por pais

Basep <- Base %>%
  filter(pais==countries[i])

Basep <- Basep  %>%
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

Basep <- Basep  %>%
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

BaseImputada <- bind_rows(BaseImputada, Basep)

i <- i + 1

}


### PASO A PPP

GR <- Base %>% filter(pais=="GR")
SE <- Base %>% filter(pais=="SE")

BaseImputada <- bind_rows(BaseImputada, GR, SE)

BaseImputada <- BaseImputada %>%
  mutate('salario'= case_when( COUNTRY=="ES" ~ salarioNAC/0.738510185, 
                             COUNTRY=="FR" ~ salarioNAC/0.832556816    ,
                             COUNTRY=="IT" ~ salarioNAC/0.853587053    ,
                             COUNTRY=="PT" ~ salarioNAC/0.687558871    ,
                             COUNTRY=="UK" ~ salarioNAC/0.656065473    ,
                             COUNTRY=="RO" ~ salarioNAC/1.217568971    ,
                             COUNTRY=="SE" ~ salarioNAC/8.447913694    ,
                             COUNTRY=="DE" ~ salarioNAC/0.834523744    ,
                             COUNTRY=="DK" ~ salarioNAC/8.552694151    ,
                             COUNTRY=="BG"  ~ 0), 
         'salariohorario'= case_when( COUNTRY=="ES" ~ salariohorarioNAC/0.738510185, 
                                    COUNTRY=="FR" ~ salariohorarioNAC/0.832556816    ,
                                    COUNTRY=="IT" ~ salariohorarioNAC/0.853587053    ,
                                    COUNTRY=="PT" ~ salariohorarioNAC/0.687558871    ,
                                    COUNTRY=="UK" ~ salariohorarioNAC/0.656065473    ,
                                    COUNTRY=="RO" ~ salariohorarioNAC/1.217568971    ,
                                    COUNTRY=="SE" ~ salariohorarioNAC/8.447913694    ,
                                    COUNTRY=="DE" ~ salariohorarioNAC/0.834523744    ,
                                    COUNTRY=="DK" ~ salariohorarioNAC/8.552694151    ,
                                    COUNTRY=="BG"  ~ 0)) 

saveRDS(BaseImputada, "F:/LFS/BasesUnificadas/SeleccionPaises2014.Rda")
