
#ANTES DE EMPEZAR CHEQUEAR COMPATIBILDIAD CON ENCODING

#  ññññ ó á é í ó ú

#### Intro ####

library(dplyr)
library(haven)
library(xlsx)
library(ggplot2)
library(ggthemes)

options(scipen = 999) 

setwd("~/GitHub/precariedad.mundial")

variables2  <- c("PAIS", "WEIGHT", "CATOCUP", "COND", "PRECAPT", "PRECAREG", "PRECATEMP", "PRECASALUD", "PRECASEG", 
                 "PRECAPT_binaria", "PRECAREG_binaria", "PRECATEMP_binaria", "PRECASALUD_binaria", "PRECASEG_binaria",
                 "TAMA", "CALIF", "ING", "ANO", "PRECACOUNT", "PRECACOUNT2", "PERIODO")

#### Chile ####


CHI <- read.csv("Bases/chile_2019.csv", sep=";")    #Es una sola base anual que se levanta entre octubre y diciembre



variables <- c("fact_cal_esi", "c1", "c10", "c11", "b9", "b7a_1", "b7a_2", "b8", 
               "b15_1", "b1", "cise", "cse_especifico", "habituales", "ing_t_p", "tipo", "d6_1_opcion")   

CHI <- CHI                                  %>% 
  select(variables)                         %>%
  
  #  Filtro sector publico y servicio domestico
    filter( cise!=4 & cise!=5 &  cise!=6)      %>%     # Asal sector publico, empleo domestico puertas afuera y puertas adentro
  
  # Filtro areas rurales
    filter(tipo!=3)                            %>%
  
  mutate(
    
    ANO= 2019,
    PERIODO= 1, 
    PAIS="Chile",
    
    #Ponderador
    WEIGHT=as.numeric(sub(",", ".", fact_cal_esi)),
    
        #Categoria Ocupacional                                        
    CATOCUP=factor(case_when(
      cise %in% c(1, 7)        ~ "Resto",
      cise == 2                ~ "Cuenta propia",
      cise == 3                ~ "Asalariados",       
      TRUE                     ~ "Ns/Nc"),
      levels= c("Asalariados", "Cuenta propia", "Resto", "Ns/Nc")),
    
    #Condicion de actividad
    COND= factor(case_when(
      cse_especifico %in% 1:7          ~ "Ocupado",
      cse_especifico %in% 8:9          ~ "Desocupado",
      cse_especifico %in% c(0, 10:28)   ~ "Inactivo"),
      levels= c("Ocupado", "Desocupado", "Inactivo", "Ns/Nc")),
    
    #Precariedad por trabajo part-time
    PRECAPT= factor(case_when(habituales<35 & habituales>0 & c10==1           ~ "Part-time involuntario",  #Horas habituales trabajadas menor a 35 / Si de usted dependiera, ¿trabajaría habitualmente mas horas de las que trabaja en la actualidad?
                               habituales<35 & habituales>0 & c10==2          ~ "Part-time voluntario", 
                               habituales %in% (35:98)         ~ "Tiempo completo",
                               habituales==99                  ~  "Ns/Nc", 
                               TRUE                            ~  "Ns/Nc"),
                    levels= c("Part-time involuntario", "Part-time voluntario", "Tiempo completo","Ns/Nc")),
    
    
    #Precariedad por contrato de tiempo limitado
    PRECATEMP= factor(case_when(b9== 1            ~ "Temporal",
                                b9== 2            ~ "No temporal",
                                TRUE              ~ "Ns/Nc"), 
                      levels= c("Temporal", "No temporal", "Ns/Nc")),             
    
    #Precariedad por registracion del contrato
    
    PRECAREG= factor(case_when(b8== 2            ~ "No registrado", 
                               b8== 1            ~ "Registrado", 
                               TRUE              ~  "Ns/Nc"),
    levels=c("No registrado", "Registrado", "Ns/Nc")),  
    
    #Precariedad por aportes a la seguridad social
    PRECASEG= factor(case_when(b7a_1== 2                      ~ "Sin aportes",           #Solo para asalariados
                        b7a_1== 1                             ~ "Con aportes",
                        b7a_1==88 | b7a_1==99 | is.na(b7a_1)  ~ "Ns/Nc"),
                     levels=c( "Sin aportes", "Con aportes", "Ns/Nc")),                  
    
    #Precariedad por cobertura de salud
    
    PRECASALUD=  factor(case_when(d6_1_opcion== 2                             ~ "Sin cobertura",              # Tiene ISAPRE o FONASA?
                                  d6_1_opcion== 1                             ~ "Con cobertura",
                                  d6_1_opcion==88 | d6_1_opcion==99 | is.na(d6_1_opcion)  ~ "Ns/Nc"),
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
      b15_1 %in% 1:2     ~ "Pequeño",
      #2. 11 a 49
      b15_1==3           ~ "Mediano",
      #3. Mas de 50
      b15_1 %in% 4:5     ~ "Grande",
      TRUE               ~ "Ns/Nc"),
      levels= c("Pequeño", "Mediano", "Grande", "Ns/Nc")),
    
    #Calificacion del puesto
    CALIF= factor(case_when( #1. Baja
      b1==9              ~ "Baja",
      #2. Media
      b1 %in% 4:8        ~ "Media", 
      #3. Alta
      b1 %in% 1:3        ~ "Alta", 
      TRUE               ~  "Ns/Nc"), 
      levels= c("Baja", "Media", "Alta", "Ns/Nc")), 
    
    #Ingreso de la ocupacion principal
    ING=as.numeric(as.numeric(gsub(",", ".", gsub("\\.", "", ing_t_p)))), 
    #Limpio ruido
    ING=case_when(
          ING==0 ~ NA_real_, 
          ING>14000000 ~ NA_real_, 
          TRUE   ~ ING))                            %>%
  
    select(variables2)                          

#### Uruguay ####

URU <- readRDS("Bases/uruguay_2019.RDS")    #Una sola basa anual

## OJO: La base original pesaba mucho asi que guarde version liviana en el repositorio

variables <- c("f73", "pesomen", "pobpcoac", "f82", "f77", "f71_2", "PT2", "f85", "f102", "f103", "region_4",
               "e45_1", "e45_2", "e45_3", "e45_4", "e45_5", "e45_6", "e45_7", "f263")                                            
 
URU <- URU                                          %>% 
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
  select(variables2)    


#### Brasil  ####

variables <- c("V1028", "VD4002", "V4018", "VD4009", "VD4011", "VD4012", "VD4004A", "V4064A", "VD4016", "V1022", "VD4031", "V4063A")                                            

BRA1 <- readRDS("Bases/Brasil_T12019.Rda")
BRA1 <- BRA1  %>% select(variables) %>% mutate(PERIODO=1)

BRA2 <- readRDS("Bases/Brasil_T22019.Rda")
BRA2 <- BRA2  %>% select(variables) %>% mutate(PERIODO=2)

BRA3 <- readRDS("Bases/Brasil_T32019.Rda")
BRA3 <- BRA3  %>% select(variables) %>% mutate(PERIODO=3)

BRA4 <- readRDS("Bases/Brasil_T42019.Rda")
BRA4 <- BRA4  %>% select(variables) %>% mutate(PERIODO=4)

BRA <- bind_rows(BRA1, BRA2, BRA3, BRA4)
remove(BRA1, BRA2, BRA3, BRA4)

BRA <- BRA                                              %>% 
 
  # Filtro sector publico y servicio domestico
  
   filter(VD4009!="Trabalhador doméstico com carteira de trabalho assinada" &
         VD4009!= "Trabalhador doméstico sem carteira de trabalho assinada" &
         VD4009!= "Empregado no setor público com carteira de trabalho assinada" &
         VD4009!= "Empregado no setor público sem carteira de trabalho assinada" &
         VD4009!= "Militar e servidor estatutário")      %>% 
  
  # Filtro areas rurales
  
  filter(V1022!=2)                                       %>% 
  
  mutate(
    
    ANO= 2019,
    
    PAIS="Brasil",
    
    #Ponderador
    WEIGHT=V1028,
    
    #Categoria Ocupacional                                         
    CATOCUP=factor(case_when(
      VD4009== "Conta-própria"                ~ "Cuenta propia", 
      VD4009== "Trabalhador familiar auxiliar" |
      VD4009== "Empregador"                          ~ "Resto", 

      VD4009=="Empregado no setor privado com carteira de trabalho assinada" |
      VD4009=="Empregado no setor privado sem carteira de trabalho assinada"    ~ "Asalariados",       
      TRUE  ~ "Ns/Nc"),
      levels= c("Asalariados", "Cuenta propia", "Resto", "Ns/Nc")),
    
    #Condicion de actividad
    COND= factor(case_when(                             
      VD4002 == "Pessoas ocupadas"     ~ "Ocupado",
      VD4002 == "Pessoas desocupadas"  ~ "Desocupado",
      TRUE  ~ "Ns/Nc"),
      levels= c("Ocupado", "Desocupado", "Inactivo", "Ns/Nc")),
    
    #Precariedad por trabajo part-time
    PRECAPT= factor(case_when( VD4031 < 35 & VD4031>0 & V4063A=="Sim"   ~ "Part-time involuntario",                # VD4031: Horas habitualmente trabalhadas por semana em todos os trabalhos para pessoas de 14 anos ou mais de idade
                               VD4031 < 35 & VD4031>0 & V4063A=="Não"   ~ "Part-time voluntario", 
                               VD4031 > 34                   ~ "Tiempo completo", 
                               TRUE              ~ "Ns/Nc"),                                            # V4063A: ... gostaria de trabalhar mais horas do que as ... (soma das horas declaradas nos quesitos 39, 56 e 62) horas que normalmente trabalhava no(s) trabalho(s) que tinha na semana de referência?
             levels= c("Part-time involuntario", "Part-time voluntario", "Tiempo completo","Ns/Nc")),
    
    #Precariedad por contrato de tiempo limitado
    PRECATEMP= "Ns/Nc",        
    
    #Precariedad por aportes a la seguridad social
    PRECASEG= factor(case_when(VD4012== "Não contribuinte"                     ~ "Sin aportes",          
                               VD4012== "Contribuinte"                         ~ "Con aportes",
                               TRUE                                            ~ "Ns/Nc"),
                     levels=c( "Sin aportes", "Con aportes", "Ns/Nc")),                  
    
    #Precariedad por registracion
    PRECAREG= factor(case_when( VD4009=="Empregado no setor privado sem carteira de trabalho assinada"  ~ "No registrado",
                         VD4009=="Empregado no setor privado com carteira de trabalho assinada"  ~ "Registrado",
                         TRUE   ~ "Ns/Nc"), 
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
      V4018== "1 a 5 pessoas" | V4018== "6 a 10 pessoas"     ~ "Pequeño",
      #2. 11 a 50
      V4018== "11 a 50 pessoas"                              ~ "Mediano",
      #3. 51 0 mas
      V4018== "51 ou mais pessoas"                           ~ "Grande",
      TRUE            ~ "Ns/Nc"),
      levels= c("Pequeño", "Mediano", "Grande", "Ns/Nc")),
    
    
    #Calificacion del puesto
    CALIF= factor(case_when( 
      #1. Baja
      VD4011== "Ocupações elementares"                                         ~ "Baja",
      #2. Media
      VD4011 == "Operadores de instalações e máquinas e montadores"  |
      VD4011 == "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios"  |
      VD4011 == "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca"  |                
      VD4011 == "Trabalhadores dos serviços, vendedores dos comércios e mercados"  |
      VD4011 == "Trabalhadores de apoio administrativo"                        ~ "Media", 
      #3. Alta
      VD4011 == "Técnicos e profissionais de nível médio"  |
      VD4011 == "Profissionais das ciências e intelectuais"  |
      VD4011 == "Diretores e gerentes"                                         ~ "Alta", 
      TRUE                                                                     ~ "Ns/Nc"), 
      levels= c("Baja", "Media", "Alta", "Ns/Nc")), 
    
    #Ingreso de la ocupacion principal
    ING=VD4016
      
  )                                 %>% 
     select(variables2)    


#### Paraguay ####


variables <- c("CATE_PEA", "FEX", "PEAA", "B10", "B26", "A16", "TAMA_PEA", "B01REC", 
               "E01AIMDE", "HORAB", "D03", "AREA") 

PAR1 <- read_sav("Bases/Paraguay_T12019.SAV")
PAR1 <- PAR1  %>% select(variables) %>% mutate(PERIODO=1)

PAR2 <-  read_sav("Bases/Paraguay_T22019.SAV")
PAR2 <- PAR2  %>% select(variables) %>% mutate(PERIODO=2)

PAR3 <-  read_sav("Bases/Paraguay_T32019.SAV")
PAR3 <- PAR3  %>% select(variables) %>% mutate(PERIODO=3)

PAR4 <-  read_sav("Bases/Paraguay_T42019.SAV")
PAR4 <- PAR4  %>% select(variables) %>% mutate(PERIODO=4)

PAR <- bind_rows(PAR1, PAR2, PAR3, PAR4)
remove(PAR1, PAR2, PAR3, PAR4)

PAR <- PAR                                  %>% 

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
    
    PRECAPT= factor(case_when( HORAB < 35 & HORAB>0 & D03 %in% 1:3  ~ "Part-time involuntario",     #Menos de 35horas y desea cambiar, mejorar o adicionar ocupacion 
                        HORAB < 35 & HORAB>0 & D03==6               ~ "Part-time voluntario",       #Menos de 35horas y no desea cambiar ocupacion
                        HORAB > 34                                  ~ "Tiempo completo",                                                
                        TRUE                                        ~ "Ns/Nc"),
                    levels= c("Part-time involuntario", "Part-time voluntario", "Tiempo completo","Ns/Nc")),
  
    #Precariedad por contrato de tiempo limitado
    PRECATEMP= factor(case_when( B26 %in% 2:3       ~ "Temporal",         # Contrato 'definido temporal' (con o sin factura legal)  
                          B26 %in% c(1, 4)          ~ "No temporal",        # Aqui incluyo a los que tienen 'contrato verbal' (que son PRECAREG)
                          TRUE               ~ "Ns/Nc"), 
                 levels= c("Temporal", "No temporal", "Ns/Nc")),  

    PRECASEG= factor(case_when(B10==6                             ~ "Sin aportes",        #Aporta a una caja de jubilacion por esa ocupacion?
                               B10==1                             ~ "Con aportes",
                               TRUE                               ~ "Ns/Nc"),
                     levels=c( "Sin aportes", "Con aportes", "Ns/Nc")),    
    
   #Precariedad por registracion
    PRECAREG= factor(case_when( B26==4        ~ "No registrado",        # Se incluyen a los que responden "contrato verbal"
                                B26 %in% 1:3  ~ "Registrado", 
                                TRUE      ~  "Ns/Nc"),
               levels=c("No registrado", "Registrado", "Ns/Nc")),  
    
   #Precariedad por acceso al sistema de salud
    PRECASALUD= "Ns/Nc" ,                                   #No encontre nada de salud en la EPHC trimestral 2019
    
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
  
  select(variables2)    



#### Bolivia #### 


variables <- c("trimestre", "s2_22", "fact_trim", "s2_18", "condact", "phrs", "s2_57",  
               "s2_21",  "s2_26", "cob_op", "yprilab", "s2_64", "area", "s2_36a")  

BOL <- readRDS("Bases/Bolivia_1a4T2019.RDS")

BOL$s2_18[is.na(BOL$s2_18)] = 0         #Saco NAs de variable para categoria ocupacional
BOL$s2_22[is.na(BOL$s2_22)] = 0

BOL <- BOL                                 %>% 
  select(variables)                         %>%
  
  # Filtro sector publico y servicio domestico
  
  filter(s2_22!=1 & s2_18!=7)         %>%   # Saco Sector Publico y "ewmpleada/o del hogar"
  
  # Filtro areas rurales
  
  filter(area!=2)                   %>%
  
  mutate(
    
    ANO= 2019,
    PERIODO=trimestre,
    
    PAIS="Bolivia",
    
    #Ponderador
    WEIGHT=fact_trim,
    
    #Categoria Ocupacional                                         
    CATOCUP=factor(case_when(
      s2_18 == 2                        ~ "Cuenta propia", 
      s2_18 %in%  c(3, 4, 5, 6, 7)      ~ "Resto", 
      s2_18 == 1                        ~ "Asalariados",       
      TRUE                              ~ "Ns/Nc"),
      levels= c("Asalariados", "Cuenta propia", "Resto", "Ns/Nc")),
    
    #Condicion de actividad
    COND= factor(case_when(
      condact == 1       ~ "Ocupado",
      condact %in% 2:5   ~ "Desocupado",# Revisar que categoría 4 y 5 parecen ser inactivos (tabular con variable pea). Para los calculos no afecta porque tomamos solo ocupados
      TRUE               ~ "Ns/Nc"),
      levels= c("Ocupado", "Desocupado", "Ns/Nc")),
    
    #Precariedad por trabajo part-time
    PRECAPT= factor(case_when(phrs<35 & phrs>0 & s2_57==1  ~ "Part-time involuntario",         #Menos de 35hs y desea trabajar mas horas
                              phrs<35 & phrs>0 & s2_57==2  ~ "Part-time voluntario", 
                              phrs>34                      ~ "Tiempo completo",                                                
                              TRUE                         ~ "Ns/Nc"),                      
                        levels= c("Part-time involuntario", "Part-time voluntario", "Tiempo completo","Ns/Nc")),
    
    
    #Precariedad por contrato de tiempo limitado
    PRECATEMP= factor(case_when( s2_21 %in% 1:2     ~ "Temporal",              #'Firmo contrato con fecha de vencimiento o termino' o 'No firmo contrato pero tiene compromiso por obra o trabajo terminado'
                                 s2_21 %in% 3:5            ~ "No temporal",
                                 TRUE  ~ "Ns/Nc"), 
                      levels= c("Temporal", "No temporal", "Ns/Nc")),  
    
    #Precariedad por registracion
    PRECAREG= factor(case_when( s2_21  %in% c(2, 3, 5)     ~ "No registrado",        
                                s2_21  %in% c(1, 4)          ~ "Registrado", 
                                TRUE                      ~  "Ns/Nc"),
                     levels=c("No registrado", "Registrado", "Ns/Nc")),  
    
    #Precariedad por aportes a la seguridad social
    PRECASEG= factor(case_when( s2_64==2 ~ "Sin aportes",                   #Esta afiliado a una AFP
                                s2_64==1  ~ "Con aportes",             
                                TRUE      ~  "Ns/Nc"),
                     levels=c( "Sin aportes", "Con aportes", "Ns/Nc")),                  
    
    #Precariedad por acceso al sistema de salud
    PRECASALUD= factor(case_when( s2_36a==2 ~ "Sin cobertura",                #En su ocupacion usted tiene: Seguro de Salud
                                  s2_36a==1 ~ "Con cobertura",     ## OJO: la variable solo esta disponible para asalariados asi qeu no sirve
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
     s2_26 %in% 1:10    ~ "Pequeño",
      #2. 11 a 50
     s2_26  %in% 11:50    ~ "Mediano",
      #3. Mas de 50
     s2_26 > 50        ~ "Grande",
      TRUE            ~ "Ns/Nc"),
      levels= c("Pequeño", "Mediano", "Grande", "Ns/Nc")),
    
    #Calificacion del puesto
    CALIF= factor(case_when( #1. Baja
     cob_op == 9              ~ "Baja",
      #2. Media
     cob_op  %in% 4:8         ~ "Media", 
      #3. Alta
     cob_op  %in% 1:3         ~ "Alta", 
      TRUE                    ~  "Ns/Nc"), 
      levels= c("Baja", "Media", "Alta", "Ns/Nc")), 
     
     #Ingreso de la ocupacion principal
     ING=yprilab, 
     ING=case_when(
      ING==0 ~ NA_real_, 
      TRUE   ~ ING))              %>% 
      select(variables2)    

=#### Peru ####

variables <- c("p507", "p510", "fac500", "p507", "ocu500", "p513t", "p521a", "p510a1", "p521",
               "p511a", "p512a", "p512b", "p505", "p505r4", "p523", "p524e1", "estrato", "p558a1", "p558a2", 
               "p558a3", "p558a4", "p558a5", "p530a") 

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

PER <- PER                                  %>% 
  # Filtro sector publico y servicio domestico
    filter(p507!=6 &  p510!=1 & p510!=2 )    %>%       # Trabajador del hogar, FFAA y Administracion publica. 
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
      p512a==1 & p512b<11         ~ "Pequeño",               ## ATENCION: para Tamaño Pequeño hay que subdividir el primer estrato entre mayores y menores de 20 personas usando la variable p512b
      #2. 11 a 49                                             # La variable p512b no tiene muchos NA asi que todo bien
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
      p505r4 %in% 9000:9998        ~ "Baja",                  ## Clasificador nacional de ocupaciones 2015 a 4 digitos (basado en CIOU08)         
      #2. Media
      p505r4 %in% 400:899          ~ "Media",                  # Tiene pocos casos con 3 digitos
      p505r4 %in% 4000:8999        ~ "Media", 
      #3. Alta
      p505r4 %in% 100:399          ~ "Alta", 
      p505r4 %in% 1000:3999        ~ "Alta", 
      TRUE                     ~  "Ns/Nc"), 
      levels= c("Baja", "Media", "Alta", "Ns/Nc")), 
    
    #Ingreso de la ocupacion principal
    ING=case_when(
      CATOCUP=="Asalariados" & p523==1       ~ p524e1 * 20,                      # ASALARIADOS: el dato de ingreso de ocupacion principal esta en jornal, semana, quincenal o mes
      CATOCUP=="Asalariados" & p523==2       ~ p524e1 * 4,                       # dependiendo como cobre el encuestado. Lo mensualice suponiendo que la persona trabaja todo el mes
      CATOCUP=="Asalariados" & p523==3       ~ p524e1 * 2,                       # lo mismo que trabajo en la semana de referencia
      CATOCUP=="Asalariados" & p523==4       ~ p524e1, 
      CATOCUP!="Asalariados"                 ~ p530a )) %>%                       
  select(variables2) 

#### Join de todas las bases y modificaciones generales ####

Base <-   bind_rows(CHI, URU, BRA, PAR, BOL, PER)

#Paso todos los TCP a Tamaño Pequeño

Base <- Base %>% 
  mutate(
    TAMA=factor(case_when(
      CATOCUP== "Cuenta propia" ~ "Pequeño", 
      TRUE                      ~ as.character(TAMA)), 
      levels= c("Pequeño", "Mediano", "Grande", "Ns/Nc")))


# Guardo la base

save(Base, file = "Bases/Bases_Latam_procesadas_Facu.Rds")
    
#### Resultados ####

#Si es necesario abro la base ya procesada
load("Bases/Bases_Latam_procesadas_Facu.Rds")

Resultados <- Base                                          %>%      
  filter(COND=="Ocupado" & CALIF!="Ns/Nc" & TAMA!="Ns/Nc" & CATOCUP!="Resto" & CATOCUP!= "Ns/Nc")   %>%
  group_by(PAIS, PERIODO, TAMA, CALIF)                     %>%
  summarise('periodo'                              = mean(ANO),         ## Ojo, hay una variable 'periodo' y otra 'PERIODO' que luego se descarta
            'casos'                                = n(),
            'ocupados'                             = sum(WEIGHT, na.rm=TRUE),
            'asalariados'                          = sum(WEIGHT[CATOCUP=="Asalariados"], na.rm=TRUE),
            'tcp'                                  = sum(WEIGHT[CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'tasa.asalarizacion'                   = sum(WEIGHT[CATOCUP=="Asalariados"], na.rm=TRUE)/sum(WEIGHT[CATOCUP=="Asalariados" | CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'seguridad.social.si.asal'             = sum(WEIGHT[PRECASEG=="Con aportes" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'seguridad.social.no.asal'             = sum(WEIGHT[PRECASEG=="Sin aportes" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'registrados.asal'                     = sum(WEIGHT[PRECAREG=="Registrado" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'no.registrados.asal'                  = sum(WEIGHT[PRECAREG=="No registrado" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'empleo.temporal.asal'                 = sum(WEIGHT[PRECATEMP=="Temporal" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'empleo.no.temporal.asal'              = sum(WEIGHT[PRECATEMP=="No temporal" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'part.involun.asal'                    = sum(WEIGHT[PRECAPT=="Part-time involuntario" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'part.volunt.asal'                     = sum(WEIGHT[PRECAPT=="Part-time voluntario" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'full.time.asal'                       = sum(WEIGHT[PRECAPT=="Tiempo completo" & CATOCUP=="Asalariados"], na.rm=TRUE),
            
            'seguridad.social.si.tcp'              = sum(WEIGHT[PRECASEG=="Con aportes" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'seguridad.social.no.tcp'              = sum(WEIGHT[PRECASEG=="Sin aportes" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'registrados.tcp'                      = sum(WEIGHT[PRECAREG=="Registrado" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'no.registrados.tcp'                   = sum(WEIGHT[PRECAREG=="No registrado" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'empleo.temporal.tcp'                  = sum(WEIGHT[PRECATEMP=="Temporal" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'empleo.no.temporal.tcp'               = sum(WEIGHT[PRECATEMP=="No temporal" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'part.involun.tcp'                     = sum(WEIGHT[PRECAPT=="Part-time involuntario" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'part.volunt.tcp'                      = sum(WEIGHT[PRECAPT=="Part-time voluntario" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'full.time.tcp'                        = sum(WEIGHT[PRECAPT=="Tiempo completo" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            
            'tasa.1.asalariados'                   = sum(WEIGHT[PRECACOUNT==1 | PRECACOUNT==2 | PRECACOUNT==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'tasa.2.asalariados'                   = sum(WEIGHT[PRECACOUNT==2 | PRECACOUNT==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'tasa.3.asalariados'                   = sum(WEIGHT[PRECACOUNT==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'promedio.ing.oc.prin'                 = weighted.mean(ING[CATOCUP=="Asalariados" | CATOCUP=="Cuenta propia"], WEIGHT[CATOCUP=="Asalariados" | CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'promedio.ing.oc.prin.asal'            = weighted.mean(ING[CATOCUP=="Asalariados"], WEIGHT[CATOCUP=="Asalariados"], na.rm=TRUE),
            'promedio.ing.oc.prin.tcp'             = weighted.mean(ING[CATOCUP=="Cuenta propia"], WEIGHT[CATOCUP=="Cuenta propia"], na.rm=TRUE))  %>%
  ungroup()                                                                    %>%
  mutate(   'tasa.partime.asal'                    = part.involun.asal/(part.involun.asal + part.volunt.asal + full.time.asal), 
            'tasa.seguridad.social.asal'           = seguridad.social.no.asal/(seguridad.social.si.asal+seguridad.social.no.asal),
            'tasa.no.registro.asal'                = no.registrados.asal/(no.registrados.asal+registrados.asal),
            'tasa.temp.asal'                       = part.involun.asal/(part.involun.asal+part.volunt.asal+part.volunt.asal),
         
            'tasa.partime.tcp'                     = part.involun.tcp/(part.involun.tcp + part.volunt.tcp + full.time.tcp), 
            'tasa.seguridad.social.tcp'            = seguridad.social.no.tcp/(seguridad.social.si.tcp+seguridad.social.no.tcp),
            'tasa.no.registro.tcp'                 = no.registrados.tcp/(no.registrados.tcp+registrados.tcp),
            'tasa.temp.tcp'                        = part.involun.tcp/(part.involun.tcp+part.volunt.tcp+part.volunt.tcp)) %>%          
  group_by(PAIS, PERIODO)                                                      %>%                         
  mutate('particip.ocup'           = ocupados/sum(ocupados),
         'particip.asal'           = asalariados/sum(asalariados),
         'particip.tcp'            = tcp/sum(tcp))                             %>%  
  ungroup()                                                                    %>%
  group_by(PAIS, TAMA, CALIF)                                                  %>% 
  mutate('total.casos'             = sum(casos))                               %>% 
  rename(Pais=PAIS, 
    grupos.tamanio=TAMA, 
    grupos.calif=CALIF)

#Resultados <-  Resultados[,c(1:5, 16, 17, 6:15, 18)]
Resultados[is.na(Resultados)] <- 0

Resultados <- Resultados %>%                        
  group_by(Pais, grupos.tamanio, grupos.calif) %>%
  summarise_each(funs(mean)) %>%
  select(-PERIODO)


Resultados   <-  Resultados  %>% 
  mutate(tamanio.calif= paste(grupos.tamanio, " - ", grupos.calif, sep=""),
         tamanio.calif2 = case_when(
           tamanio.calif == "Pequeño - Baja"  ~ "1) Pequeño - Baja",
           tamanio.calif == "Pequeño - Media" ~ "2) Pequeño - Media",
           tamanio.calif == "Pequeño - Alta" ~ "3) Pequeño - Alta",
           tamanio.calif == "Mediano - Baja" ~ "4) Mediano - Baja",
           tamanio.calif == "Mediano - Media" ~ "5) Mediano - Media",
           tamanio.calif == "Mediano - Alta" ~ "6) Mediano - Alta",
           tamanio.calif == "Grande - Baja" ~ "7) Grande - Baja",
           tamanio.calif == "Grande - Media" ~ "8) Grande - Media",
           tamanio.calif == "Grande - Alta" ~ "9) Grande - Alta"))


save(Resultados, file = "Resultados/ResultadosFacu.RDATA")


#### Resultados sin desagregar por perfiles ####


Resultados2 <- Base                                          %>%      
  filter(COND=="Ocupado" & CALIF!="Ns/Nc" & TAMA!="Ns/Nc")   %>%
  group_by(PAIS, PERIODO)                                %>%
  summarise('periodo'                              = mean(ANO),
            'ocupados'                             = sum(WEIGHT, na.rm=TRUE),
            'asalariados'                          = sum(WEIGHT[CATOCUP=="Asalariados"], na.rm=TRUE),
            'tcp'                                  = sum(WEIGHT[CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'tasa.asalarizacion'                   = sum(WEIGHT[CATOCUP=="Asalariados"], na.rm=TRUE)/sum(WEIGHT[CATOCUP=="Asalariados" | CATOCUP=="Cuenta propia"], na.rm=TRUE),
            
            'seguridad.social.si.asal'             = sum(WEIGHT[PRECASEG=="Con aportes" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'seguridad.social.no.asal'             = sum(WEIGHT[PRECASEG=="Sin aportes" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'registrados.asal'                     = sum(WEIGHT[PRECAREG=="Registrado" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'no.registrados.asal'                  = sum(WEIGHT[PRECAREG=="No registrado" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'empleo.temporal.asal'                 = sum(WEIGHT[PRECATEMP=="Temporal" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'empleo.no.temporal.asal'              = sum(WEIGHT[PRECATEMP=="No temporal" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'part.involun.asal'                    = sum(WEIGHT[PRECAPT=="Part-time involuntario" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'part.volunt.asal'                     = sum(WEIGHT[PRECAPT=="Part-time voluntario" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'full.time.asal'                       = sum(WEIGHT[PRECAPT=="Tiempo completo" & CATOCUP=="Asalariados"], na.rm=TRUE),
            
            'seguridad.social.si.tcp'              = sum(WEIGHT[PRECASEG=="Con aportes" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'seguridad.social.no.tcp'              = sum(WEIGHT[PRECASEG=="Sin aportes" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'registrados.tcp'                      = sum(WEIGHT[PRECAREG=="Registrado" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'no.registrados.tcp'                   = sum(WEIGHT[PRECAREG=="No registrado" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'empleo.temporal.tcp'                  = sum(WEIGHT[PRECATEMP=="Temporal" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'empleo.no.temporal.tcp'               = sum(WEIGHT[PRECATEMP=="No temporal" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'part.involun.tcp'                     = sum(WEIGHT[PRECAPT=="Part-time involuntario" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'part.volunt.tcp'                      = sum(WEIGHT[PRECAPT=="Part-time voluntario" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'full.time.tcp'                        = sum(WEIGHT[PRECAPT=="Tiempo completo" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            
            'tasa.1.asalariados'                   = sum(WEIGHT[PRECACOUNT==1 | PRECACOUNT==2 | PRECACOUNT==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'tasa.2.asalariados'                   = sum(WEIGHT[PRECACOUNT==2 | PRECACOUNT==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'tasa.3.asalariados'                   = sum(WEIGHT[PRECACOUNT==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'promedio.ing.oc.prin'                 = weighted.mean(ING[CATOCUP=="Asalariados" | CATOCUP=="Cuenta propia"], WEIGHT[CATOCUP=="Asalariados" | CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'promedio.ing.oc.prin.asal'            = weighted.mean(ING[CATOCUP=="Asalariados"], WEIGHT[CATOCUP=="Asalariados"], na.rm=TRUE),
            'promedio.ing.oc.prin.tcp'             = weighted.mean(ING[CATOCUP=="Cuenta propia"], WEIGHT[CATOCUP=="Cuenta propia"], na.rm=TRUE))  %>%
  ungroup()                                                                    %>%
  mutate(   'tasa.partime.asal'                    = part.involun.asal/(part.involun.asal + part.volunt.asal + full.time.asal), 
            'tasa.seguridad.social.asal'           = seguridad.social.no.asal/(seguridad.social.si.asal+seguridad.social.no.asal),
            'tasa.no.registro.asal'                = no.registrados.asal/(no.registrados.asal+registrados.asal),
            'tasa.temp.asal'                       = part.involun.asal/(part.involun.asal+part.volunt.asal+part.volunt.asal),
            
            'tasa.partime.tcp'                     = part.involun.tcp/(part.involun.tcp + part.volunt.tcp + full.time.tcp), 
            'tasa.seguridad.social.tcp'            = seguridad.social.no.tcp/(seguridad.social.si.tcp+seguridad.social.no.tcp),
            'tasa.no.registro.tcp'                 = no.registrados.tcp/(no.registrados.tcp+registrados.tcp),
            'tasa.temp.tcp'                        = part.involun.tcp/(part.involun.tcp+part.volunt.tcp+part.volunt.tcp)) %>% 
  rename(Pais=PAIS)

Resultados2[is.na(Resultados2)] <- 0

Resultados2 <- Resultados2 %>%                        
  group_by(Pais) %>%
  summarise_each(funs(mean)) %>%
  select(-PERIODO)

save(Resultados2, file = "Resultados/Resultados_agregados_Facu.RDATA")


#### Cuento cantidad de casos sin ponderar en cada perfil ####

Casos  <- Base                                               %>%      
  filter(COND=="Ocupado" & CALIF!="Ns/Nc" & TAMA!="Ns/Nc")   %>%
  group_by(PAIS, PERIODO, TAMA, CALIF)                       %>%
  summarise('casos' = n())                                   %>%
  ungroup()                                                  %>% 
  group_by(PAIS, TAMA, CALIF)                                %>%  
  summarise_each(funs(mean))                                 %>%
  ungroup()                                                  %>% 
  select(-PERIODO)                                           %>% 
  group_by(PAIS)                                             %>%                         
  mutate('porcentaje'           = casos/sum(casos))          %>% 
  ungroup()                                                  %>% 
  mutate(tamanio.calif= paste(TAMA, " - ", CALIF, sep=""),
         tamanio.calif = case_when(
           tamanio.calif == "Pequeño - Baja"  ~ "1) Pequeño - Baja",
           tamanio.calif == "Pequeño - Media" ~ "2) Pequeño - Media",
           tamanio.calif == "Pequeño - Alta" ~ "3) Pequeño - Alta",
           tamanio.calif == "Mediano - Baja" ~ "4) Mediano - Baja",
           tamanio.calif == "Mediano - Media" ~ "5) Mediano - Media",
           tamanio.calif == "Mediano - Alta" ~ "6) Mediano - Alta",
           tamanio.calif == "Grande - Baja" ~ "7) Grande - Baja",
           tamanio.calif == "Grande - Media" ~ "8) Grande - Media",
           tamanio.calif == "Grande - Alta" ~ "9) Grande - Alta"))   %>% 
  filter(PAIS!="Brasil")

ggplot(Casos, aes(x=tamanio.calif, y=casos)) +
  geom_col(position = "dodge") +
  facet_wrap(~PAIS) +
  labs(title = "Cantidad de casos sin ponderar")
  





####Graficos ####


#Paleta de colores#
azul <- colorspace::diverge_hcl(n = 12,h = c(255,330),
                                l = c(40,90))[c(4,2,1)]
verde <- colorspace::diverge_hcl(n = 12,h = c(130,43),
                                 c = 100,
                                 l = c(70,90))[c(4,2,1)]

naranja <- colorspace::diverge_hcl(n = 12,h = c(130,43),
                                   c = 100,
                                   l = c(70,90))[c(10,11,12)]
paleta <- c(azul,
            naranja,
            verde)

paleta3 <- c(azul[1],
             naranja[1],
             verde[1])

Grafico <-  Resultados  %>%  
  ggplot(.,
         aes(x = PAIS, y = particip.ocup,
             fill = tamanio.calif2, group = tamanio.calif2,
             label = scales::percent(particip.ocup))) +
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5),size=3)+
  labs(title = "Distribución del empleo según grupos")+
  theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 14),
        axis.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 45),
        #panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta)+
  scale_y_continuous(labels = scales::percent)+
  guides(fill=guide_legend(title="Tamaño - Calificacion")) +
  ggsave(paste0(Carpeta, "grupos_calificacion_tamanio.jpg"),width = 10,height = 8)



