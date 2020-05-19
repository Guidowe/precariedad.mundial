

Base <- readRDS("F:/LFS/BasesUnificadas/SeleccionPaises2018.Rda")

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
  mutate(salario= case_when(
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
  mutate(salariohorario= case_when(
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

GR <- Base %>% filter(pais=="GR")
SE <- Base %>% filter(pais=="SE")

BaseImputada <- bind_rows(BaseImputada, GR, SE)

saveRDS(BaseImputada, "F:/LFS/BasesUnificadas/SeleccionPaises2018.Rda")
