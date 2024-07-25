library(tidyverse)
variables<- c("PAIS","ANO","PERIODO","WEIGHT","SEXO","EDAD",
              "CATOCUP","SECTOR","PRECAPT","EDUC",
              "PRECAREG","PRECATEMP","PRECASALUD","PRECASEG",
              "TAMA","CALIF","ING") 

BRA1 <- readRDS("Bases/Brasil_T12019.Rda")
BRA1 <- BRA1 %>% mutate(PERIODO=1)
BRA2 <- readRDS("Bases/Brasil_T22019.Rda")
BRA2 <- BRA2 %>% mutate(PERIODO=2)
BRA3 <- readRDS("Bases/Brasil_T32019.Rda")
BRA3 <- BRA3  %>% mutate(PERIODO=3)
BRA4 <- readRDS("Bases/Brasil_T42019.Rda")
BRA4 <- BRA4 %>% mutate(PERIODO=4)
Base <- bind_rows(BRA1, BRA2, BRA3, BRA4)
remove(BRA1, BRA2, BRA3, BRA4)

Base <- Base %>% 
  filter(V1022 != 2) %>% 
  filter(VD4002 == "Pessoas ocupadas") %>% 
  mutate(
    ANO = 2019,
    PAIS = "Brasil",
    WEIGHT = V1028,
    SEXO = case_when(V2007 == "Homem" ~ "Varon", 
                     V2007 == "Mulher" ~ "Mujer"), 
    EDAD= V2009,
    EDUC = case_when(
      VD3004 %in% c("Sem instrução e menos de 1 ano de estudo", 
                    "Fundamental incompleto ou equivalente", 
                    "Fundamental completo ou equivalente", 
                    "Médio incompleto ou equivalente")~ "Primaria", 
      VD3004 %in% c("Médio completo ou equivalente",
                    "Superior incompleto ou equivalente")~ "Secundaria", 
      VD3004 %in% c("Superior completo")~ "Terciaria"),
    CATOCUP = case_when(
      VD4009 == "Conta-própria" ~ "Cuenta propia", 
      VD4009 == "Trabalhador familiar auxiliar"~ "Resto", 
      VD4009 == "Empregador" ~ "Resto", 
      VD4009 == "Empregado no setor privado com carteira de trabalho assinada" ~ "Asalariado", 
      VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" ~ "Asalariado", 
      VD4009 == "Empregado no setor público com carteira de trabalho assinada" ~ "Asalariado", 
      VD4009 == "Empregado no setor público sem carteira de trabalho assinada" ~ "Asalariado", 
      VD4009 == "Trabalhador doméstico com carteira de trabalho assinada" ~ "Asalariado",
      VD4009 == "Trabalhador doméstico sem carteira de trabalho assinada"~ "Asalariado", 
      VD4009 == "Militar e servidor estatutário" ~ "Asalariado"),
    SECTOR = case_when(
      VD4009 %in% c("Empregado no setor público com carteira de trabalho assinada",
                    "Empregado no setor público sem carteira de trabalho assinada",
                    "Militar e servidor estatutário") ~ "Pub", 
      VD4009 %in% c("Trabalhador doméstico com carteira de trabalho assinada",
                    "Trabalhador doméstico sem carteira de trabalho assinada") ~ "SD", 
      VD4009 %in% c("Empregado no setor privado com carteira de trabalho assinada", 
                    "Empregado no setor privado sem carteira de trabalho assinada", 
                    "Empregador", "Conta-própria", "Trabalhador familiar auxiliar") ~ "Priv"),
    PRECAPT = case_when( 
      VD4031 < 35 & VD4031 > 0 & V4063A == "Sim" ~ 1,               
      VD4031 < 35 & VD4031 > 0 & V4063A == "Não" ~ 0, 
      VD4031 > 34 ~ 0),
    PRECATEMP = NA,        
    PRECASEG = case_when(
      VD4012 == "Não contribuinte" ~ 1,          
      VD4012 == "Contribuinte" ~ 0),
    PRECAREG = case_when( 
      VD4009 == "Empregado no setor privado sem carteira de trabalho assinada" ~ 1,
      VD4009 == "Empregado no setor privado com carteira de trabalho assinada" ~ 0), 
    PRECASALUD = NA, 
    TAMA = case_when( 
      V4018 %in% c("1 a 5 pessoas", "6 a 10 pessoas") ~ "Pequeño",
      V4018 == "11 a 50 pessoas" ~ "Mediano",
      V4018 == "51 ou mais pessoas" ~ "Grande"),
    CALIF = case_when( 
      VD4011 == "Ocupações elementares" ~ "Baja",
      VD4011 %in% c("Operadores de instalações e máquinas e montadores",
                    "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios",
                    "Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca",                
                    "Trabalhadores dos serviços, vendedores dos comércios e mercados",
                    "Trabalhadores de apoio administrativo") ~ "Media", 
      VD4011 %in% c("Técnicos e profissionais de nível médio",
                    "Profissionais das ciências e intelectuais",
                    "Diretores e gerentes") ~ "Alta"), 
    ING = VD4016) %>% 
  select(variables)

saveRDS(Base, "bases_homog/brasil.rds")
