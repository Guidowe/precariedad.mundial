library(tidyverse)
variables <- c("p507", "p510", "fac500", "p507", "ocu500", "p513t", "p521a", "p510a1", "p521",
               "p511a", "p512a", "p512b", "p505", "p505r4", "p523", "p524e1", "estrato", "p558a1", "p558a2", 
               "p558a3", "p558a4", "p558a5", "p530a") 
variables_homog <- c("PAIS", "WEIGHT", "CATOCUP", "COND", "PRECAPT", "PRECAREG", "PRECATEMP", "PRECASALUD", "PRECASEG", 
                     "PRECAPT_binaria", "PRECAREG_binaria", "PRECATEMP_binaria", "PRECASALUD_binaria", "PRECASEG_binaria",
                     "TAMA", "CALIF", "ING", "ANO", "PRECACOUNT", "PRECACOUNT2", "PERIODO")

PER1 <- read_dta("Bases/Peru_1T2019.dta")
PER1 <- PER1 %>% select(variables) %>%mutate(PERIODO=1)
PER2 <- read_dta("Bases/Peru_2T2019.dta")
PER2 <- PER2 %>% select(variables) %>%mutate(PERIODO=2)
PER3 <- read_dta("Bases/Peru_3T2019.dta")
PER3 <- PER3 %>% select(variables) %>%mutate(PERIODO=3)
PER4 <- read_dta("Bases/Peru_4T2019.dta")
PER4 <- PER4 %>% select(variables) %>%mutate(PERIODO=4)
PER <- bind_rows(PER1, PER2, PER3, PER4)
remove(PER1, PER2, PER3, PER4)

PER$p507[is.na(PER$p507)] = 0  
PER$p510[is.na(PER$p510)] = 0         #Saco NA de variable p510 para no perder a los cuentapropistas cuando cruzo p507 y p510 en los filter

Base<- PER                                  %>% 
  # Filtro sector publico y servicio domestico
  filter(p507!=6 &  p510!=1 & p510!=2 )    %>%      
  # Filtro areas rurales 
  filter(estrato!=7 &  estrato!=8)         %>%    
  mutate(                                             
    ANO= 2019,
    PAIS="Peru",
    #Ponderador
    WEIGHT=fac500,
    #Categoria ocupacional
    CATOCUP=factor(case_when(
      p507 == 2                  ~ "Cuenta propia", 
      p507 %in% c(1, 5, 7)       ~ "Resto", 
      p507 %in% 3:4              ~ "Asalariados",       
      TRUE                       ~ "Ns/Nc"),
      levels= c("Asalariados", "Cuenta propia", "Resto", "Ns/Nc")),
    #Condicion de actividad
    COND= factor(case_when(
      ocu500== 1        ~ "Ocupado",
      ocu500 %in% 2:3   ~ "Desocupado",
      ocu500==4         ~ "Inactivo",       
      TRUE               ~ "Ns/Nc"),
      levels= c("Ocupado", "Desocupado", "Inactivo", "Ns/Nc")),
    #Precariedad por trabajo part-time
    PRECAPT= factor(case_when(p513t<35 & p513t>0 & p521==1   ~ "Part-time involuntario",     #Menos de 35 horas, desea trabajar mas horas  
                              p513t<35 & p513t>0 & p521==2   ~ "Part-time voluntario", 
                              p513t>34                       ~ "Tiempo completo",                                                
                              TRUE                           ~ "Ns/Nc"),                      
                    levels= c("Part-time involuntario", "Part-time voluntario", "Tiempo completo","Ns/Nc")),
    #Precariedad por contrato de tiempo limitado
    PRECATEMP= factor(case_when( p511a %in% c(2,6)              ~ "Temporal",
                                 p511a %in% c(1, 3, 4, 5, 7, 8)          ~ "No temporal",
                                 TRUE                          ~ "Ns/Nc"), 
                      levels= c("Temporal", "No temporal", "Ns/Nc")),
    #Precariedad por aportes a la seguridad social
    PRECASEG= factor(case_when( p558a5==5                                   ~ "Sin aportes", 
                                p558a1==1 | p558a2==2 | p558a3==3 | p558a4==4 ~ "Con aportes",             
                                TRUE              ~  "Ns/Nc"),
                     levels=c( "Sin aportes", "Con aportes", "Ns/Nc")), 
    #Precariedad por registracion
    PRECAREG= factor(case_when( p511a==7 ~ "No registrado",
                                p511a %in% c(1:6, 8)  ~ "Registrado",                    # p551a==7 : No tiene contrato
                                TRUE      ~  "Ns/Nc"),
                     levels=c("No registrado", "Registrado", "Ns/Nc")),
    #Precariedad por acceso al sistema de salud
    PRECASALUD= "Ns/Nc",                                
    #Conteo de expersiones de precariedad con generacion de binarias
    PRECAPT_binaria= case_when(PRECAPT=="Part-time involuntario"~ 1 ,         
                               TRUE   ~ 0), 
    PRECATEMP_binaria= case_when(PRECATEMP=="Temporal"~ 1 ,         
                                 TRUE   ~ 0), 
    PRECAREG_binaria= case_when(PRECAREG=="No registrado"~ 1 ,         
                                TRUE   ~ 0), 
    PRECASALUD_binaria= case_when(PRECASALUD== "Sin cobertura"  ~ 1 ,         
                                  TRUE   ~ 0), 
    PRECASEG_binaria=  case_when(PRECASEG== "Sin aportes"  ~ 1 ,         
                                 TRUE   ~ 0),
    PRECACOUNT= PRECAPT_binaria + PRECATEMP_binaria + PRECAREG_binaria + PRECASEG_binaria,
    PRECACOUNT2= PRECAPT_binaria + PRECASALUD_binaria,
    #Tamaño establecimiento
    TAMA= factor(case_when( 
      #1. 10 o menos
      p512a==1 & p512b<11         ~ "Pequeño",              
      #2. 11 a 49                                             
      p512a==1 & p512b %in% 11:20 ~ "Mediano",
      p512a==2                    ~ "Mediano",                
      #3. Mas de 50
      p512a %in% 3:5              ~ "Grande",
      TRUE                        ~ "Ns/Nc"),
      levels= c("Pequeño", "Mediano", "Grande", "Ns/Nc")),
    #Calificacion del puesto
    CALIF= factor(case_when( 
      #1. Baja
      p505r4 %in% 900:999          ~ "Baja", 
      p505r4 %in% 9000:9998        ~ "Baja",                         
      #2. Media
      p505r4 %in% 400:899          ~ "Media",                 
      p505r4 %in% 4000:8999        ~ "Media", 
      #3. Alta
      p505r4 %in% 100:399          ~ "Alta", 
      p505r4 %in% 1000:3999        ~ "Alta", 
      TRUE                     ~  "Ns/Nc"), 
      levels= c("Baja", "Media", "Alta", "Ns/Nc")), 
    #Ingreso de la ocupacion principal
    ING=case_when(
      CATOCUP=="Asalariados" & p523==1       ~ p524e1 * 20,                      # ASALARIADOS: el dato de ingreso de ocupacion principal esta en jornal, semana, quincenal o mes
      CATOCUP=="Asalariados" & p523==2       ~ p524e1 * 4,                       # dependiendo como cobre el encuestado. Se mensualiza suponiendo que la persona trabaja todo el mes
      CATOCUP=="Asalariados" & p523==3       ~ p524e1 * 2,                       # lo mismo que trabajo en la semana de referencia
      CATOCUP=="Asalariados" & p523==4       ~ p524e1, 
      CATOCUP!="Asalariados"                 ~ p530a )) %>%                       
  select(variables_homog) 

save(Base, file= "Bases_homog/Peru.Rdata")
