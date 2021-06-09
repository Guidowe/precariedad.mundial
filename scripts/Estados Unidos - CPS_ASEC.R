####funciones y librerias#####
library(ipumsr)
library(eph)
library(tidyverse)
library(openxlsx)
library(stringr)

sample.isco <- function(df) {
  sample(df$ISCO.1.digit,size = 1)
}

#######USA#####

# ocup_usa <- read.xlsx("data/Codigos Ocup USA.xlsx",sheet = "OCC 2011a2019")
# ramas_usa <- read.xlsx("data/Codigos Ocup USA.xlsx",sheet = "IND 2014a2019")
# soc_census <- read.xlsx("data/Codigos Ocup USA.xlsx",sheet = "SOC CENSO cross para R")
# soc_isco <- read.xlsx("data/Codigos Ocup USA.xlsx",sheet = "SOC ISCO cross para R")
# skills_isco <- read.xlsx("data/Codigos Ocup USA.xlsx",sheet = "Skill levels ISCO")


# cps_ddi_file <- "../bases/cps_00004.xml"
# cps_data_file <- "../bases/cps_00004.dat"
# cps_ddi <- read_ipums_ddi(cps_ddi_file)
# # Base_USA <- ipumsr::read_ipums_micro(ddi = cps_ddi_file,
# #                                  data_file =  cps_data_file) %>%
# #   filter(YEAR %in% 2011:2019)%>%
# #   filter(ASECFLAG==1)
# Base_USA <- readRDS("../bases/Base_USA2011_2019.RDS")

####USA Variables####
#  Variables.USA <- c("FIRMSIZE","EDUC","LABFORCE","EMPSTAT","YEAR","MONTH",
#                     "CLASSWKR","CLASSWLY","WORKLY",
#                     "WKSTAT","FULLPART",
#                     "WHYPTLY","WHYPTLWK",
#                     "IND","INDLY",
#                     "OCC","OCCLY",
#                     "EARNWEEK","PENSION",
#                     "INCWAGE","INCBUS","INCLONGJ","SRCEARN",
#                     "INCTOT","EARNWEEK","INCFARM","OINCFARM",
#                     "OINCWAGE","WKSWORK1","UHRSWORKLY",
#                     "ASECWT","EARNWT","ASECFLAG") 
# 
# table(Base_USA$UHRSWORKLY)
####Crosswalk USA#####
#   
# cross.census.a.soc <- ocup_usa %>% 
#   full_join(soc_census %>% mutate(OCC=as.numeric(`2010.Census.Code`))) %>% 
#   select(Census = OCC,Census.title = Description,
#          SOC.title = `2010.Occupation.Title`,
#          SOC = `2010.SOC.Code`) %>% 
#   mutate(SOC= stringr::str_trim(string = SOC,side = "both"),
#          Census= stringr::str_trim(string = Census,side = "both"),
#          SOC.title = case_when(is.na(SOC.title)~Census.title,
#                                !is.na(SOC.title)~SOC.title),
#          SOC = case_when(Census %in%  c(9840,9830)~"55-3010",
#                          Census== 4550~"39-7010",
#                          Census== 1000~"15-1131",
#                          TRUE~SOC)) %>% 
#   filter(!is.na(Census))
#   
# cross.soc.a.isco <-  soc_isco %>% 
#   select(SOC = `2010.SOC.Code`,SOC.title = `2010.SOC.Title`,part, 
#          ISCO = `ISCO-08.Code`,ISCO.title = `ISCO-08.Title.EN`) %>% 
#   mutate(SOC= stringr::str_trim(string = SOC,side = "both"),
#          ISCO= stringr::str_trim(string = ISCO,side = "both"))
# 
# 
# cross.census.a.soc.a.isco <- cross.census.a.soc %>% 
#   mutate(SOC.JOIN = case_when(substring(SOC, nchar(SOC)) %in% c("0","X")~
#                          paste0(substring(SOC,1,nchar(SOC)-1),1),
#                          TRUE~ SOC),
#          SOC.JOIN = case_when(substring(SOC.JOIN,nchar(SOC.JOIN)-1,nchar(SOC.JOIN)-1) == "X"~
#                                 paste0(substring(SOC.JOIN,1,nchar(SOC.JOIN)-2),99),
#                               TRUE~ SOC.JOIN),
#          SOC.JOIN = case_when(SOC.JOIN == "25-1001"~"25-1011",
#                               SOC.JOIN == "25-3001"~"25-3099",
#                               SOC.JOIN == "29-9001"~"29-9099",
#                               SOC.JOIN == "39-4099"~"39-4011",
#                               SOC.JOIN == "53-1001"~"53-1031",
#                               SOC.JOIN == "53-1001"~"53-1031",
#                               TRUE~ SOC.JOIN)) %>% 
#   left_join(cross.soc.a.isco %>% rename(SOC.title.2 = SOC.title,
#                                         SOC.JOIN = SOC))
# 
# 
# 
# 
# 
# ###Cross a 1 digito sampleado###
# cross.census.a.soc.a.isco.1.dig  <- cross.census.a.soc.a.isco %>% 
#   mutate(ISCO.1.digit = substr(ISCO,1,1),
#          Census = as.numeric(Census)) %>% 
#   left_join(skills_isco %>% mutate(ISCO.1.digit = as.character(ISCO.1.digit))) 
#   
# 
# casos.analizables  <- cross.census.a.soc.a.isco.1.dig %>% 
#    select(SOC,SOC.title,subjetividad) %>% 
#    unique() %>% 
#    group_by(SOC) %>% 
#    summarise(soc.a.isco.1.dig.distintos = n()) %>% 
#    filter(soc.a.isco.1.dig.distintos>1,!is.na(SOC)) %>% 
#    left_join(cross.census.a.soc.a.isco.1.dig %>% 
#                select(SOC,SOC.title.2,ISCO.1.digit,ISCO,ISCO.title))
# 
# 
# cross.census.a.soc.a.isco.nested <-  cross.census.a.soc.a.isco.1.dig %>% 
#   group_by(Census,SOC,SOC.title) %>% 
#   nest()
# 
# ####Aplico Cross####
# Base_Usa_cruzada <- Base_USA %>% 
#   filter(ASECFLAG==1) %>% 
#   select(Variables.USA) %>% 
#   rename(Census = OCCLY) %>%  
#   left_join(cross.census.a.soc.a.isco.nested)
# 
# #Sorteo ocupaciones
# set.seed(9999)
# Base_Usa_sampleada <- Base_Usa_cruzada %>% 
#   mutate(ISCO.1.digit = map(data, sample.isco)) %>% 
#   select(-data) %>% 
#   mutate(ISCO.1.digit = as.numeric(ISCO.1.digit)) %>% 
#   left_join(skills_isco) 
# 
# rm(list = c("Base_USA","Base_Usa_cruzada","cps_ddi"))
# gc()

#saveRDS(Base_Usa_sampleada,"../bases/Base_Usa_sampleada.RDS")
Base_Usa_sampleada<- readRDS("../bases/Estados Unidos/Base_Usa_sampleada.RDS")

# table(Base_Usa_sampleada$CLASSWKR)
ver <- eph::calculate_tabulates(Base_Usa_sampleada,"CLASSWLY","FIRMSIZE")
####USA Categorias####
Base_USA.cat <- Base_Usa_sampleada %>% 
  filter(ASECFLAG==1,
         WORKLY ==  2,
         INDLY <9370,#sin Sector publico
         INDLY <9290,#sin Sector publico ni S. doméstico 
         CLASSWLY %in% 10:28) %>% # ASAL y TCP
  mutate(
    grupos.calif = factor(subjetividad,levels = c("Baja","Media","Alta")),
    grupos.nivel.ed = factor(case_when(
      EDUC %in% 2:72~ "Menor a Secundaria",
      EDUC %in% 73:110~ "Secundaria Completa",
      EDUC %in% 111:125~ "Superior Completo",
      TRUE ~ "Ns/Nr"),
      c("Menor a Secundaria","Secundaria Completa","Superior Completo")),
    Categoria =  case_when(
      CLASSWLY %in% 10:19 ~ "Patrones y CP",
      CLASSWLY  %in% 20:28 ~ "Asalariados",
      CLASSWLY == 29 ~ "TFSR",
      CLASSWLY == 99 ~ "Ns/Nr"),
    Categoria =  case_when(CLASSWKR %in% 10:19 ~ "Self employed",
                           CLASSWKR  %in% 20:28 ~ "Asalariados"),
    grupos.tamanio =case_when(
      FIRMSIZE==1~"Pequeño",
      FIRMSIZE %in% 2:4~"Mediano",
      FIRMSIZE %in% 5:9~"Grande"),
    grupos.tamanio =case_when(CLASSWKR %in% 10:19 ~"Pequeño",
                              TRUE ~ grupos.tamanio),
    seguridad.social = case_when(
      PENSION %in% 1:2 ~ "No",
      PENSION %in% 3 ~ "Si",
      PENSION %in% 0 ~ "NIU"),
    part.time.inv = case_when(
      FULLPART == 2 & WHYPTLY %in% c(1,3,4)~"Part Involunt",
      FULLPART == 2 & WHYPTLY %in% c(2)~"Part Volunt",
      FULLPART == 1 ~"Full Time",
      TRUE ~ "Otros"),
    sobreocup = case_when(UHRSWORKLY %in%  46:99~"Si",
                          UHRSWORKLY %in%  1:45~"No"))


##Filtros USA #####
usa.cat <- Base_USA.cat %>% 
  mutate(periodo = YEAR -1,
         FACTOR = ASECWT)


usa.ocupados.distrib <-  usa.cat  %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    ocupados = sum(FACTOR,na.rm = T),
    asalariados = sum(FACTOR[Categoria  == "Asalariados"],na.rm = T),
    tcp = sum(FACTOR[Categoria  != "Asalariados"],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados) %>% 
  group_by(periodo) %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.tcp= tcp/sum(tcp))

usa.ocupados.distrib.agregado <-  usa.cat  %>% 
  group_by(periodo) %>% 
  summarise(
    ocupados = sum(FACTOR,na.rm = T),
    asalariados = sum(FACTOR[Categoria  == "Asalariados"],na.rm = T),
    tcp = sum(FACTOR[Categoria  != "Asalariados"],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados) %>% 
  group_by(periodo) %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.tcp= tcp/sum(tcp))

usa.asalariados.tasas <- usa.cat %>% 
  filter(Categoria  == "Asalariados") %>% # Asalariado
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    seguridad.social.si = sum(FACTOR[seguridad.social=="Si"],na.rm = T),
    seguridad.social.no = sum(FACTOR[seguridad.social=="No"],na.rm = T),
    # registrados =sum(FACTOR[registracion=="Si"],na.rm = T),
    # no.registrados =sum(FACTOR[registracion=="No"],na.rm = T),
    # empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    # empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(FACTOR[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(FACTOR[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(FACTOR[part.time.inv=="Full Time"],na.rm = T),
    tasa.partime = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".asal"), .cols = 4:ncol(.))

usa.tcp.tasas <- usa.cat %>% 
  filter(Categoria  != "Asalariados") %>% # TCP
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    seguridad.social.si = sum(FACTOR[seguridad.social=="Si"],na.rm = T),
    seguridad.social.no = sum(FACTOR[seguridad.social=="No"],na.rm = T),
    # registrados =sum(FACTOR[registracion=="Si"],na.rm = T),
    # no.registrados =sum(FACTOR[registracion=="No"],na.rm = T),
    # empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    # empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(FACTOR[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(FACTOR[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(FACTOR[part.time.inv=="Full Time"],na.rm = T),
    tasa.partime = part.involun/(part.involun+
                                   part.volunt+
                                   full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".tcp"), .cols = 4:ncol(.))

usa.asalariados.tasas.agregado <- usa.cat %>% 
  filter(Categoria  == "Asalariados") %>% # Asalariado
  group_by(periodo) %>% 
  summarise(
    seguridad.social.si = sum(FACTOR[seguridad.social=="Si"],na.rm = T),
    seguridad.social.no = sum(FACTOR[seguridad.social=="No"],na.rm = T),
    # registrados =sum(FACTOR[registracion=="Si"],na.rm = T),
    # no.registrados =sum(FACTOR[registracion=="No"],na.rm = T),
    # empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    # empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(FACTOR[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(FACTOR[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(FACTOR[part.time.inv=="Full Time"],na.rm = T),
    tasa.partime = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".asal"), .cols = 2:ncol(.))

usa.tcp.tasas.agregado <- usa.cat %>% 
  filter(Categoria  != "Asalariados") %>% # Asalariado
  group_by(periodo) %>% 
  summarise(
    seguridad.social.si = sum(FACTOR[seguridad.social=="Si"],na.rm = T),
    seguridad.social.no = sum(FACTOR[seguridad.social=="No"],na.rm = T),
    # registrados =sum(FACTOR[registracion=="Si"],na.rm = T),
    # no.registrados =sum(FACTOR[registracion=="No"],na.rm = T),
    # empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    # empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(FACTOR[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(FACTOR[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(FACTOR[part.time.inv=="Full Time"],na.rm = T),
    tasa.partime = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".tcp"), .cols = 2:ncol(.))


usa.ingresos.asal <- usa.cat %>% 
  filter(INCWAGE>0|INCBUS>0,
         INCWAGE!=99999999|INCBUS!=99999999,
         INCWAGE==INCLONGJ|INCBUS==INCLONGJ,
#         Categoria=="Asalariados",
         WORKLY==2) %>% 
  mutate(ingreso.horario = INCLONGJ/WKSWORK1/UHRSWORKLY,
         ingreso.mensual = INCLONGJ/WKSWORK1*4)

usa.ingresos.asal.perfiles <- usa.ingresos.asal%>% 
  group_by(periodo,grupos.calif,grupos.tamanio) %>% 
  summarise(promedio.ing.oc.prin = weighted.mean(x = ingreso.mensual,
                                                      w = FACTOR,
                                                      na.rm = TRUE),
            promedio.ing.oc.prin.asal = weighted.mean(x = ingreso.mensual[ Categoria=="Asalariados"],
                                                      w = FACTOR[Categoria=="Asalariados"],
                                                      na.rm = TRUE),
            promedio.ing.oc.prin.tcp = weighted.mean(x = ingreso.mensual[Categoria!="Asalariados"],
                                                      w = FACTOR[Categoria!="Asalariados"],
                                                      na.rm = TRUE))
                     
usa.ingresos.asal.agregado <- usa.ingresos.asal%>% 
  group_by(periodo) %>% 
  summarise(promedio.ing.oc.prin = weighted.mean(x = ingreso.mensual,
                                                 w = FACTOR,
                                                 na.rm = TRUE),
            promedio.ing.oc.prin.asal = weighted.mean(x = ingreso.mensual[ Categoria=="Asalariados"],
                                                      w = FACTOR[Categoria=="Asalariados"],
                                                      na.rm = TRUE),
            promedio.ing.oc.prin.tcp = weighted.mean(x = ingreso.mensual[Categoria!="Asalariados"],
                                                     w = FACTOR[Categoria!="Asalariados"],
                                                     na.rm = TRUE))

usa.resultado <- usa.ocupados.distrib %>%
  left_join(usa.asalariados.tasas)%>% 
  left_join(usa.tcp.tasas)%>% 
  left_join(usa.ingresos.asal.perfiles)%>% 
  mutate(Pais = "Estados Unidos",
         tamanio.calif = paste0(grupos.tamanio," - ",grupos.calif),
         tamanio.calif = factor(tamanio.calif,
                                levels = 
                                  c("Pequeño - Baja",
                                    "Pequeño - Media",
                                    "Pequeño - Alta",
                                    "Mediano - Baja",
                                    "Mediano - Media", 
                                    "Mediano - Alta",
                                    "Grande - Baja",
                                    "Grande - Media",
                                    "Grande - Alta"))) %>% 
  arrange(tamanio.calif)

usa.resultado.agregado <- usa.ocupados.distrib.agregado %>%
  left_join(usa.asalariados.tasas.agregado)%>% 
  left_join(usa.tcp.tasas.agregado)%>% 
  left_join(usa.ingresos.asal.agregado)%>% 
  mutate(Pais = "Estados Unidos")

saveRDS(usa.resultado,file = "Resultados/Estados Unidos.RDS")  
saveRDS(usa.resultado.agregado,file = "Resultados/Estados Unidos_agregado.RDS")  


