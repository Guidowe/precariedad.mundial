library(tidyverse)
library(haven)
variables <- c("CATE_PEA", "FEX", "PEAA", "B10", "B26", "A16", "TAMA_PEA", "B01REC", 
               "E01AIMDE", "HORAB", "D03", "AREA") 
variables_homog <- c("PAIS", "WEIGHT", "CATOCUP", "COND", "PRECAPT", "PRECAREG", "PRECATEMP", "PRECASALUD", "PRECASEG", 
                     "PRECAPT_binaria", "PRECAREG_binaria", "PRECATEMP_binaria", "PRECASALUD_binaria", "PRECASEG_binaria",
                     "TAMA", "CALIF", "ING", "ANO", "PRECACOUNT", "PRECACOUNT2", "PERIODO")

PAR1 <- read_sav("Bases/Paraguay_T12019.SAV")
PAR1 <- PAR1  %>% select(variables) %>% mutate(PERIODO=1)
PAR2 <-  read_sav("Bases/Paraguay_T22019.SAV")
PAR2 <- PAR2  %>% select(variables) %>% mutate(PERIODO=2)
PAR3 <-  read_sav("Bases/Paraguay_T32019.SAV")
PAR3 <- PAR3  %>% select(variables) %>% mutate(PERIODO=3)
PAR4 <-  read_sav("Bases/Paraguay_T42019.SAV")
PAR4 <- PAR4  %>% select(variables) %>% mutate(PERIODO=4)

Base <- bind_rows(PAR1, PAR2, PAR3, PAR4)
remove(PAR1, PAR2, PAR3, PAR4)
Base <- Base                                  %>% 
  # Filtro sector publico y servicio domestico
  filter(CATE_PEA!=1 & CATE_PEA!=6 )        %>% 
  # Filtro areas rurales
  filter(AREA!=6)                           %>%
  mutate(
    ANO= 2019,
    PAIS="Paraguay",
    #Ponderador
    WEIGHT=FEX,
    #Categoria Ocupacional                                         
    CATOCUP=factor(case_when(
      CATE_PEA ==   4       ~ "Cuenta propia", 
      CATE_PEA ==   2       ~ "Asalariados", 
      CATE_PEA %in% c(3, 5) ~ "Resto", 
      TRUE                  ~ "Ns/Nc"),
      levels= c("Asalariados", "Cuenta propia", "Resto", "Ns/Nc")),
    #Condicion de actividad
    COND= factor(case_when(
      PEAA == 1       ~ "Ocupado",
      PEAA == 2       ~ "Desocupado",
      PEAA == 3       ~ "Inactivo",       
      TRUE  ~ "Ns/Nc"),
      levels= c("Ocupado", "Desocupado", "Inactivo", "Ns/Nc")),
    PRECAPT= factor(case_when( HORAB < 35 & HORAB>0 & D03 %in% 1:3  ~ "Part-time involuntario",
                               HORAB < 35 & HORAB>0 & D03==6               ~ "Part-time voluntario",
                               HORAB > 34                                  ~ "Tiempo completo",                                                
                               TRUE                                        ~ "Ns/Nc"),
                    levels= c("Part-time involuntario", "Part-time voluntario", "Tiempo completo","Ns/Nc")),
    #Precariedad por contrato de tiempo limitado
    PRECATEMP= factor(case_when( B26 %in% 2:3       ~ "Temporal", 
                                 B26 %in% c(1, 4)          ~ "No temporal", 
                                 TRUE               ~ "Ns/Nc"), 
                      levels= c("Temporal", "No temporal", "Ns/Nc")),  
    
    PRECASEG= factor(case_when(B10==6                             ~ "Sin aportes",    
                               B10==1                             ~ "Con aportes",
                               TRUE                               ~ "Ns/Nc"),
                     levels=c( "Sin aportes", "Con aportes", "Ns/Nc")),    
    #Precariedad por registracion
    PRECAREG= factor(case_when( B26==4        ~ "No registrado",   
                                B26 %in% 1:3  ~ "Registrado", 
                                TRUE      ~  "Ns/Nc"),
                     levels=c("No registrado", "Registrado", "Ns/Nc")),  
    #Precariedad por acceso al sistema de salud
    PRECASALUD= "Ns/Nc" ,                 
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
      TAMA_PEA %in% 1:3    ~ "Pequeño",
      #2. 11 a 50
      TAMA_PEA %in% 4:6    ~ "Mediano",
      #3. Mas de 51
      TAMA_PEA %in% 7:9    ~ "Grande",       
      TRUE  ~ "Ns/Nc"),
      levels= c("Pequeño", "Mediano", "Grande", "Ns/Nc")),
    #Calificacion del puesto
    CALIF= factor(case_when( #1. Baja
      B01REC == 9          ~ "Baja",
      #2. Media
      B01REC %in% 4:8       ~ "Media", 
      #3. Alta
      B01REC %in% 1:3       ~ "Alta",       
      TRUE  ~ "Ns/Nc"),
      levels= c("Baja", "Media", "Alta", "Ns/Nc")), 
    #Ingreso de la ocupacion principal
    ING=as.numeric(E01AIMDE), 
    ING=case_when(
      ING==0 ~ NA_real_, 
      ING > 100000000 ~ NA_real_, 
      TRUE   ~ ING))                                 %>% 
  select(variables_homog)    

save(Base, file= "Bases_homog/Paraguay.Rdata")
