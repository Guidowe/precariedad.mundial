library(tidyverse)
variables <- c("V1028", "VD4002", "V4018", "VD4009", "VD4011", "VD4012", "VD4004A", "V4064A", "VD4016", "V1022", "VD4031", "V4063A")                                            
variables_homog <- c("PAIS", "WEIGHT", "CATOCUP", "COND", "PRECAPT", "PRECAREG", "PRECATEMP", "PRECASALUD", "PRECASEG", 
                     "PRECAPT_binaria", "PRECAREG_binaria", "PRECATEMP_binaria", "PRECASALUD_binaria", "PRECASEG_binaria",
                     "TAMA", "CALIF", "ING", "ANO", "PRECACOUNT", "PRECACOUNT2", "PERIODO")

BRA1 <- readRDS("Bases/Brasil_T12019.Rda")
BRA1 <- BRA1  %>% select(variables) %>% mutate(PERIODO=1)
BRA2 <- readRDS("Bases/Brasil_T22019.Rda")
BRA2 <- BRA2  %>% select(variables) %>% mutate(PERIODO=2)
BRA3 <- readRDS("Bases/Brasil_T32019.Rda")
BRA3 <- BRA3  %>% select(variables) %>% mutate(PERIODO=3)
BRA4 <- readRDS("Bases/Brasil_T42019.Rda")
BRA4 <- BRA4  %>% select(variables) %>% mutate(PERIODO=4)
Base <- bind_rows(BRA1, BRA2, BRA3, BRA4)
remove(BRA1, BRA2, BRA3, BRA4)

Base <- Base                                              %>% 
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
    ING=VD4016)            %>% 
  select(variables_homog)

save(Base, file= "Bases_homog/Brasil.Rdata")