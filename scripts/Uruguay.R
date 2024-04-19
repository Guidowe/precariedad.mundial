library(tidyverse)
URU <- readRDS("Bases/uruguay_2019.RDS")    #La base original pesaba mucho asi que guarde version liviana en el repositorio
variables <- c("f73", "pesomen", "pobpcoac", "f82", "f77", "f71_2", "PT2", "f85", "f102", "f103", "region_4",
               "e45_1", "e45_2", "e45_3", "e45_4", "e45_5", "e45_6", "e45_7", "f263")
variables_homog <- c("PAIS", "WEIGHT", "CATOCUP", "COND", "PRECAPT", "PRECAREG", "PRECATEMP", "PRECASALUD", "PRECASEG", 
                 "PRECAPT_binaria", "PRECAREG_binaria", "PRECATEMP_binaria", "PRECASALUD_binaria", "PRECASEG_binaria",
                 "TAMA", "CALIF", "ING", "ANO", "PRECACOUNT", "PRECACOUNT2", "PERIODO")
Base <- URU                                          %>% 
  select(variables)                                 %>%  
  # Filtro sector publico y servicio domestico
  filter(f73!=2 & f73!=8 & f71_2!=9111)             %>%   # Asalariado Sector Publico, programa social de empleo, servicio domestico segun CIOU
  # Filtro areas rurales
  filter(region_4!=4)                               %>%
  mutate(
    ANO= 2019,
    PERIODO=1, 
    PAIS="Uruguay",
    #Ponderador
    WEIGHT=pesomen,
    #Categoria Ocupacional                                         
    CATOCUP=factor(case_when(
      f73==5 | f73==6           ~ "Cuenta propia",
      f73==3 | f73==4 | f73==7 | f73==8    ~ "Resto", 
      f73==1 | f73==2          ~ "Asalariados",       
      TRUE  ~ "Ns/Nc"),
      levels= c("Asalariados", "Cuenta propia", "Resto", "Ns/Nc")),
    #Condicion de actividad
    COND= factor(case_when(
      pobpcoac == 2       ~ "Ocupado",
      pobpcoac %in% 3:5   ~ "Desocupado",
      pobpcoac %in% 6:11 ~ "Inactivo",       
      TRUE  ~ "Ns/Nc"),
      levels= c("Ocupado", "Desocupado", "Inactivo", "Ns/Nc")),
    #Precariedad por trabajo part-time
    PRECAPT= factor(case_when(f85<35 & f85>0 & f102==1  ~ "Part-time involuntario",     #Menos de 35 horas, desea trabajar mas horas 
                              f85<35 & f85>0 & f102==2  ~ "Part-time voluntario", 
                              f85>34            ~ "Tiempo completo",                                                
                              TRUE              ~ "Ns/Nc"),                      
                    levels= c("Part-time involuntario", "Part-time voluntario", "Tiempo completo","Ns/Nc")),
    #Precariedad por contrato de tiempo limitado
    PRECATEMP= "Ns/Nc",                   
    #Precariedad por aportes a la seguridad social
    PRECASEG= factor(case_when( f82==2 & CATOCUP=="Asalariados"  ~ "Sin aportes",       # No aporta a caja de jubilaciones (PARA ASALARIADOS)
                                f82==1 & CATOCUP=="Asalariados"  ~  "Con aportes",   
                                f263==2 & CATOCUP=="Cuenta propia" ~ "Sin aportes",    # Negocio registrado en impuestos o seguridad social
                                f263==1 & CATOCUP=="Cuenta propia" ~ "Con aportes",                         
                                TRUE      ~  "Ns/Nc"),
                     levels=c( "Sin aportes", "Con aportes", "Ns/Nc")),  
    
    #Precariedad por registracion
    PRECAREG= "Ns/Nc",
    #Precariedad por acceso al sistema de salud
    PRECASALUD= factor(case_when(e45_1==2 & e45_2==2 & e45_3==2 & e45_4==2 & e45_5==2 & e45_6==2 & e45_7==2   ~ "Sin cobertura",            # Varias preguntas sobre "derechos vigentes para antender la salud"
                                 e45_1==1 | e45_2==1 | e45_3==1 | e45_4==1 | e45_5==1 | e45_6==1 | e45_7==1    ~ "Con cobertura",
                                 TRUE   ~  "Ns/Nc"), 
                       levels=c("Sin cobertura", "Con cobertura", "Ns/Nc")),  
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
      f77 %in% 1:3    ~ "Pequeño",
      #2. 11 a 49
      f77 %in% 6:7    ~ "Mediano",
      #3. Mas de 50
      f77 == 5        ~ "Grande",
      TRUE            ~ "Ns/Nc"),
      levels= c("Pequeño", "Mediano", "Grande", "Ns/Nc")),
    #Calificacion del puesto
    CALIF= factor(case_when( #1. Baja
      f71_2 %in% 9000:9999            ~ "Baja",
      #2. Media
      f71_2 %in% 4000:8999        ~ "Media", 
      #3. Alta
      f71_2 %in% 1000:3999        ~ "Alta", 
      TRUE                        ~  "Ns/Nc"), 
      levels= c("Baja", "Media", "Alta", "Ns/Nc")), 
    #Ingreso de la ocupacion principal
    ING=as.numeric(sub(",", ".", PT2)), 
    ING=case_when(
      ING==0 ~ NA_real_,
      ING>750000 ~ NA_real_,
      TRUE   ~ ING))  %>% 
  select(variables_homog)

save(Base, file= "Bases_homog/Uruguay.Rdata")

