####funciones y librerias#####
  library(ipumsr)
  library(eph)
  library(ggthemes)
  library(ggalt)
  library(tidyverse)
  library(kableExtra)
  library(formattable)
  library(openxlsx)
  library(Weighted.Desc.Stat)
  library(stringr)
  # Funcion de redondeo para presentación (queda como character)
  formato_porc <- function(numero, dec = 1){
    format(round(numero, digits = dec), nsmall = dec, decimal.mark = ",")
  }
  
  formato_pesos <- function(numero, dec = 2){
    paste0("$", format(round(numero, digits = dec), nsmall = dec, big.mark = ".", decimal.mark = ","))
  }
  
  formato_cantidad <- function(numero, dec = 0){
    format(round(numero, digits = dec), nsmall = dec, big.mark = ".", decimal.mark = ",")
  }
  
  sample.isco <- function(df) {
    sample(df$ISCO.1.digit,size = 1)
  }
  
####bases de datos#####
####ARGENTINA#####
Base_ARG0814 <- readRDS("../bases/EPH2008_2014.RDS")  
  Base_ARG1719 <- readRDS("../bases/EPH2016_2019.RDS")  
  
####ARG Variables####
Variables1719  <- c("CODUSU","NRO_HOGAR","COMPONENTE","ANO4","TRIMESTRE" ,"AGLOMERADO","H15",
    "CH04", "CH06", "CH12","CH13","CH14","CH15","ESTADO","CAT_OCUP","INTENSI",
    "PP04A", "PP04B_COD","PP07H","P21","PONDERA","PP04D_COD","PP04C",
    "PP07A","PP07C","PP05B2_ANO","PP04B3_ANO","PP07E","NIVEL_ED","PONDIIO","DECOCUR",
    "PP11D_COD","PP04C","PP04C99","PP03G","PP3E_TOT")
  
  
Variables0814  <- c("CODUSU","NRO_HOGAR","COMPONENTE","ANO4","TRIMESTRE" ,"AGLOMERADO","H15",
                      "CH04", "CH06", "CH12","CH13","CH14","CH15","ESTADO","CAT_OCUP","INTENSI",
                      "PP04A", "PP04B_COD","PP07H","P21","PONDERA","PP04D_COD","PP04B_CAES","PP04C",
                      "PP07A","PP07C","PP05B2_ANO","PP04B3_ANO","PP07E","NIVEL_ED","DECOCUR",
                      "PP11D_COD","PP04C","PP04C99","PP03G","PP3E_TOT")

bases_bind <- bind_rows(
Base_ARG0814 %>% select(Variables0814),
Base_ARG1719 %>% select(Variables1719)) %>% 
  mutate(PP04B_COD = as.character(PP04B_COD), 
         PP04B_COD = dplyr::case_when(
           nchar(PP04B_COD) == 1 ~ paste0("0", PP04B_COD),
           nchar(PP04B_COD) == 2 ~ PP04B_COD, 
           nchar(PP04B_COD) == 3 ~ paste0("0", PP04B_COD),
           nchar(PP04B_COD) == 4 ~ PP04B_COD)) %>% 
  mutate(PP04B_COD = case_when(ANO4 %in% 2011:2015 ~ PP04B_CAES,
                               TRUE~ PP04B_COD))

#Check <- eph::calculate_tabulates(bases_bind,x = "PP04B_COD",y = "ANO4",weights = "PONDERA")


rm(list = c("Base_ARG1719","Base_ARG0814"))
gc()
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
Base_Usa_sampleada<- readRDS("../bases/Base_Usa_sampleada.RDS")

# Chequeo.todo.joya <- Base_Usa_sampleada %>% 
#   filter(Census!= 0,!(ISCO.1.digit %in% 0:10))

####USA Categorias####
Base_USA.cat <- Base_Usa_sampleada %>% 
  mutate(
    grupos.calif = factor(subjetividad,levels = c("Baja","Media","Alta")),
    grupos.tamanio = factor(case_when(
      FIRMSIZE==1~"Pequeño",
      FIRMSIZE %in% 2:4~"Mediano",
      FIRMSIZE %in% 5:9~"Grande",
      FIRMSIZE %in% 0 ~ "Ns/Nr"),
      levels = c("Pequeño","Mediano","Grande","Ns/Nr")),
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
         # Categoria =  case_when(CLASSWKR %in% 10:19 ~ "Patrones y CP",
         #                        CLASSWKR  %in% 20:28 ~ "Asalariados",
         #                        CLASSWKR == 29 ~ "TFSR",
         #                        CLASSWKR == 99 ~ "Ns/Nr"),
    pension = case_when(
      PENSION %in% 1:2 ~ "No",
      PENSION %in% 3 ~ "Si",
      PENSION %in% 0 ~ "NIU"),
         # part.time.inv = case_when(WKSTAT  %in%  20:42 &
         #                             WHYPTLWK %in% c(10:40,
         #                                             52,60,17:81,
         #                                             4)~"Involunt",
         #                           TRUE ~ "Resto"),
    part.time.inv = case_when(
      FULLPART == 2 & WHYPTLY %in% c(1,3,4)~"Part Involunt",
      FULLPART == 2 & WHYPTLY %in% c(2)~"Part Volunt",
      FULLPART == 1 ~"Full Time",
      TRUE ~ "Otros"),
    sobreocup = case_when(UHRSWORKLY %in%  46:99~"Si",
                          UHRSWORKLY %in%  1:45~"No"))

#table(Base_USA.cat$sobreocup,useNA = "always")

####ARG categorias####
Base_EPH.cat <- bases_bind %>%
  eph::organize_cno() %>% 
  #mutate(PP04B_COD = PP04B_CAES) %>% 
  mutate(
    cno.anterior.desocup = stringr::str_pad(PP11D_COD,5,
                                              side = "left", 
                                              pad = "0"),
    calif.anterior.desocup = substr(cno.anterior.desocup, 5, 5),
    grupos.calif.desocup = factor (
      case_when(
        calif.anterior.desocup %in% 1:2~ "Alta",
        calif.anterior.desocup ==   3 ~ "Media",
        calif.anterior.desocup ==   4 ~ "Baja",
        TRUE ~ "Ns/Nr"),
      levels = c("Baja","Media","Alta","Ns/Nr")), 
    grupos.calif = factor(
      case_when(
        CALIFICACION %in% c("Profesionales","Técnicos") ~ "Alta",
        CALIFICACION ==   "Operativos" ~ "Media",
        CALIFICACION ==   "No calificados" ~ "Baja"),
      levels = c("Baja","Media","Alta")),
    grupos.nivel.ed = factor(
      case_when(NIVEL_ED %in% c(7,1,2,3) ~ "Menor a Secundaria",
                NIVEL_ED %in% c(4,5) ~ "Secundaria Completa",
                NIVEL_ED == 6 ~ "Superior Completo",
                TRUE ~ "Ns/Nr"),
      levels = c("Menor a Secundaria","Secundaria Completa","Superior Completo")),
    grupos.tamanio = factor(
      case_when(PP04C %in% 1:6  |(PP04C %in% 99 & PP04C99 == 1)~ "Pequeño",
                PP04C %in% 7:8  ~ "Mediano",
                PP04C %in% 9:12 |(PP04C %in% 99 & PP04C99 == 3)~ "Grande",
                PP04C %in% 99 & PP04C99 %in% c(2,9) ~ "Ns/Nr"),
      levels = c("Pequeño","Mediano","Grande","Ns/Nr")),
    descuento_jubil = case_when(PP07H == 1 ~ "Si",
                                PP07H == 2 ~ "No",
                                PP07H == 0 ~ "0"),
    Categoria =  case_when(CAT_OCUP == 1 ~ "Patrones",
                           CAT_OCUP == 2 ~ "TCP",
                           CAT_OCUP == 3 ~ "Asalariados",
                           CAT_OCUP == 4 ~ "TFSR",
                           CAT_OCUP == 9 ~ "Ns/Nr"),
    part.time.inv = case_when(ESTADO == 1 & PP3E_TOT < 35 & PP03G == 1 ~ "Part Involunt",
                              ESTADO == 1 & PP3E_TOT < 35 & PP03G == 2 ~"Part Volunt",
                              ESTADO == 1 & PP3E_TOT >= 35  ~"Full Time",
                              TRUE ~ "Otros"),
    sobreocup = case_when(PP3E_TOT >=  46 & PP3E_TOT <=168 ~"Si",
                          PP3E_TOT >=  1 & PP3E_TOT <=45  ~ "No"),
    tiempo.determinado = case_when(PP07C ==  1 ~"Si",
                                   PP07C ==  2 ~ "No"))
##ARG insercion ----
insercion.niveles.arg.trim <- Base_EPH.cat %>% 
  group_by(ANO4,TRIMESTRE,grupos.nivel.ed) %>% 
  summarise(PEA = sum(PONDERA[ESTADO %in% 1:2],na.rm = TRUE),
            ocupados = sum(PONDERA[ESTADO %in% 1],na.rm = TRUE),
            desocupados = sum(PONDERA[ESTADO %in% 2],na.rm = TRUE),
            tasa.desocup = desocupados/PEA) %>%
  group_by(ANO4,TRIMESTRE) %>% 
  mutate(PEA.porc = PEA/sum(PEA),
         ocup.porc = ocupados/sum(ocupados)) %>% 
  ungroup() %>% 
  select(-PEA,-ocupados,-desocupados)

desocup.calif.ant.arg <- Base_EPH.cat %>%
  filter(grupos.calif.desocup!= "Ns/Nr") %>% 
  group_by(ANO4,TRIMESTRE,grupos.calif.desocup) %>% 
  summarise(desocupados = sum(PONDERA[ESTADO %in% 2],na.rm = TRUE)) %>% 
  group_by(ANO4,TRIMESTRE) %>% 
  mutate(distribucion = desocupados/sum(desocupados))

insercion.niveles.arg <- insercion.niveles.arg.trim %>% 
  group_by(grupos.nivel.ed,ANO4) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  select(-TRIMESTRE) 
  

##USA insercion ----
insercion.niveles.usa <- Base_USA.cat %>% 
  filter(grupos.nivel.ed != "ns/nr") %>% 
  group_by(YEAR,grupos.nivel.ed) %>% 
  summarise(PEA = sum(ASECWT[EMPSTAT %in% 1:22],na.rm = TRUE),
            ocupados = sum(ASECWT[EMPSTAT %in% 1:12],na.rm = TRUE),
            desocupados = sum(ASECWT[EMPSTAT %in% 20:22],na.rm = TRUE),
            tasa.desocup = desocupados/PEA)%>%
  group_by(YEAR) %>% 
  mutate(PEA.porc = PEA/sum(PEA),
         ocup.porc = ocupados/sum(ocupados))%>% 
  select(-PEA,-ocupados,-desocupados)


desocup.calif.ant.usa <- Base_USA.cat %>%
  filter(grupos.calif!= "Ns/Nr") %>% 
  group_by(YEAR,grupos.calif) %>% 
  summarise(desocupados = sum(ASECWT[EMPSTAT %in% 20:22],na.rm = TRUE)) %>% 
  group_by(YEAR) %>% 
  mutate(distribucion = desocupados/sum(desocupados))


##Filtros ARG##### 
eph.ocup.privados <- Base_EPH.cat %>% 
  filter(!(ANO4 %in% 2011:2020 & PP04B_COD %in% c(83:84,8300:8499,#Adm Publica
                                                  97:98,9700:9899)),#Serv Domestico
         !(ANO4 %in% 2008:2010 & PP04B_COD %in% c(75,7500:7599,#Adm Publica
                                                  95,9500:9599)),#Serv Domestico

         ESTADO == 1) %>% 
  mutate(Pais = "ARG",
         PONDERA_SALARIOS = case_when(ANO4  %in%  2016:2020 ~ as.integer(PONDIIO),
                                      ANO4  %in%  2003:2015 ~ as.integer(PONDERA))) %>% 
  rename(ingreso.mensual = P21)

##Filtros USA #####
usa.ocup.privados <- Base_USA.cat %>% 
  filter(INDLY <9370,#sin Sector publico
         INDLY <9290,#sin Sector publico ni S. doméstico 
         WORKLY ==  2)%>% 
  mutate(Pais = "USA",
         ANO4 = YEAR,
         PONDERA = ASECWT,
         TRIMESTRE = 1)

##Base unificada (No ingresos)####
variables_comunes <- 
  c("ANO4","TRIMESTRE","Pais","grupos.nivel.ed","grupos.tamanio",
    "Categoria","grupos.calif","PONDERA","part.time.inv","sobreocup")

base.unica <- bind_rows(
  eph.ocup.privados %>%select(variables_comunes,descuento_jubil,tiempo.determinado),
  usa.ocup.privados %>% select(variables_comunes,pension)) %>% 
  rownames_to_column(var = "Id") %>% 
  filter(grupos.tamanio != "Ns/Nr",!is.na(grupos.tamanio))
####Cruce Educacion calificacion####
calificacion.educacion <- base.unica %>% 
  filter(grupos.calif != "Ns/Nr",!is.na(grupos.calif)) %>% 
  group_by(grupos.nivel.ed,grupos.calif,Pais,ANO4,TRIMESTRE) %>% 
  summarise(total = sum(PONDERA,na.rm = TRUE))

calificacion.educacion.anual <- calificacion.educacion %>% 
  group_by(grupos.nivel.ed,grupos.calif,Pais,ANO4) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  select(-TRIMESTRE) %>% 
  arrange(ANO4,Pais,grupos.calif) %>% 
  group_by(grupos.nivel.ed,Pais,ANO4) %>% 
  mutate(Porcentaje = total/sum(total))


cruces.educ.calif.abs <- calificacion.educacion.anual %>% 
  select(-Porcentaje) %>% 
  pivot_wider(names_from = grupos.calif,values_from = total)

cruces.educ.calif.perc <- calificacion.educacion.anual %>% 
  select(-total) %>% 
  pivot_wider(names_from = grupos.calif,values_from = Porcentaje)

###RESULTADOS caract####
# write.xlsx(x = list("Insercion NIVEL ED ARG" = insercion.niveles.arg,
#                     "Insercion NIVEL ED USA" = insercion.niveles.usa,
#                     "Educ calif absolutos" = cruces.educ.calif.abs,
#                     "Educ calif porcentaje" = cruces.educ.calif.perc,
#                     "Desocup calif ant ARG" = desocup.calif.ant.arg,
#                     "Desocup calif ant USA" = desocup.calif.ant.usa),
#            file = "Resultados/Educacion y Calificacion.xlsx")  

####Perfiles (Sin ingresos)####

#########Ocup.calif#############################################
perfiles.ocupados.calif <- base.unica %>% 
  filter(grupos.tamanio != "Ns/Nr",grupos.calif %in% c("Alta","Media","Baja")) %>% 
  group_by(grupos.calif,grupos.tamanio,Pais,ANO4,TRIMESTRE) %>% 
  summarise(casos.muestrales=n(),
            total = sum(PONDERA,na.rm = TRUE),
            asalariados = sum(PONDERA[Categoria=="Asalariados"]),
            tasa.asalarizacion = asalariados/total,
            part.involun = sum(PONDERA[part.time.inv=="Part Involunt"]),
            part.volunt = sum(PONDERA[part.time.inv=="Part Volunt"]),
            sobreocupados = sum(PONDERA[sobreocup=="Si"],na.rm = TRUE),
            no.sobreocupados = sum(PONDERA[sobreocup=="No"],na.rm = TRUE),
            full.time = sum(PONDERA[part.time.inv=="Full Time"]),
            resto = sum(PONDERA[part.time.inv=="Otros"]),
            tasa.part.invol = part.involun/(part.volunt+part.involun+full.time),
            tasa.sobreocup = sobreocupados/(sobreocupados+no.sobreocupados),
            s_desc_jubilat =sum(PONDERA[descuento_jubil=="No"],na.rm = T),
            c_desc_jubilat =sum(PONDERA[descuento_jubil=="Si"],na.rm = T),
            s_pension =sum(PONDERA[pension=="No"],na.rm = T),
            c_pension =sum(PONDERA[pension=="Si"],na.rm = T),
            tasa.s.desc.jubil = s_desc_jubilat/(c_desc_jubilat+s_desc_jubilat),
            tasa.s.pension = s_pension/(c_pension+s_pension),
            part.involun.s_desc = sum(PONDERA[part.time.inv=="Part Involunt" & descuento_jubil=="No"],na.rm = T),
            part.involun.c_desc =sum(PONDERA[part.time.inv=="Part Involunt" & descuento_jubil=="Si"],na.rm = T),
            tasa.cooc.invol.s.desc = part.involun.s_desc/(part.involun.s_desc+part.involun.c_desc),
            no.part.involun.s_desc =sum(PONDERA[part.time.inv!="Part Involunt" & descuento_jubil=="No"],na.rm = T),
            no.part.part.involun.c_desc =sum(PONDERA[part.time.inv!="Part Involunt" & descuento_jubil=="Si"],na.rm = T),
            tasa.cooc.no.invol.s.desc = no.part.involun.s_desc/(no.part.involun.s_desc+no.part.part.involun.c_desc),
            part.involun.s_pension = sum(PONDERA[part.time.inv=="Part Involunt" & pension=="No"],na.rm = T),
            part.involun.c_pension =sum(PONDERA[part.time.inv=="Part Involunt" & pension=="Si"],na.rm = T),
            tasa.cooc.invol.s.pension = part.involun.s_pension/(part.involun.s_pension+part.involun.c_pension),
            no.part.involun.s_pension =sum(PONDERA[part.time.inv!="Part Involunt" & pension=="No"],na.rm = T),
            no.part.part.involun.c_pension =sum(PONDERA[part.time.inv!="Part Involunt" & pension=="Si"],na.rm = T),
            tasa.cooc.no.invol.s.pension = no.part.involun.s_pension/(no.part.involun.s_pension+no.part.part.involun.c_pension)) %>% 
  group_by(Pais,ANO4,TRIMESTRE) %>% 
  mutate(Particip_emp = total/sum(total)) %>% 
  ungroup()

indicadores.anuales.ocupados.calif <- perfiles.ocupados.calif %>% 
  group_by(grupos.calif,grupos.tamanio,Pais,ANO4) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  select(-TRIMESTRE) %>% 
  arrange(ANO4,Pais,grupos.tamanio)

#########asal.calif#############################################
perfiles.asalariados.calif <- base.unica %>% 
  filter(Categoria == "Asalariados") %>% 
  filter(grupos.tamanio != "Ns/Nr",grupos.calif %in% c("Alta","Media","Baja")) %>% 
  group_by(grupos.calif,grupos.tamanio,Pais,ANO4,TRIMESTRE) %>% 
  summarise(casos.muestrales=n(),
            total = sum(PONDERA,na.rm = TRUE),
            asalariados = sum(PONDERA[Categoria=="Asalariados"]),
            tasa.asalarizacion = asalariados/total,
            part.involun = sum(PONDERA[part.time.inv=="Part Involunt"]),
            part.volunt = sum(PONDERA[part.time.inv=="Part Volunt"]),
            sobreocupados = sum(PONDERA[sobreocup=="Si"],na.rm = TRUE),
            no.sobreocupados = sum(PONDERA[sobreocup=="No"],na.rm = TRUE),
            empleo.temporal = sum(PONDERA[tiempo.determinado=="Si"],na.rm = TRUE),
            empleo.no.temporal = sum(PONDERA[tiempo.determinado=="No"],na.rm = TRUE),
            full.time = sum(PONDERA[part.time.inv=="Full Time"]),
            resto = sum(PONDERA[part.time.inv=="Otros"]),
            s_desc_jubilat =sum(PONDERA[descuento_jubil=="No"],na.rm = T),
            c_desc_jubilat =sum(PONDERA[descuento_jubil=="Si"],na.rm = T),
            s_pension =sum(PONDERA[pension=="No"],na.rm = T),
            c_pension =sum(PONDERA[pension=="Si"],na.rm = T),
            tasa.part.invol = part.involun/(part.volunt+part.involun+full.time),
            tasa.sobreocup = sobreocupados/(sobreocupados+no.sobreocupados),
            tasa.s.desc.jubil = s_desc_jubilat/(c_desc_jubilat+s_desc_jubilat),
            tasa.s.pension = s_pension/(c_pension+s_pension),
            tasa.empleo.temporal = empleo.temporal/(empleo.temporal+empleo.no.temporal),
            part.involun.s_desc = sum(PONDERA[part.time.inv=="Part Involunt" & descuento_jubil=="No"],na.rm = T),
            part.involun.c_desc =sum(PONDERA[part.time.inv=="Part Involunt" & descuento_jubil=="Si"],na.rm = T),
            no.part.involun.s_desc =sum(PONDERA[part.time.inv!="Part Involunt" & descuento_jubil=="No"],na.rm = T),
            no.part.part.involun.c_desc =sum(PONDERA[part.time.inv!="Part Involunt" & descuento_jubil=="Si"],na.rm = T),
            tasa.cooc.invol.s.desc = part.involun.s_desc/(part.involun.s_desc+part.involun.c_desc),
#           tasa.cooc.no.invol.s.desc = no.part.involun.s_desc/(no.part.involun.s_desc+no.part.part.involun.c_desc),
            part.involun.temporal = sum(PONDERA[part.time.inv=="Part Involunt" & tiempo.determinado=="Si"],na.rm = T),
            part.involun.no.temporal = sum(PONDERA[part.time.inv=="Part Involunt" & tiempo.determinado=="No"],na.rm = T),
            no.part.involun.temporal = sum(PONDERA[part.time.inv!="Part Involunt" & tiempo.determinado=="Si"],na.rm = T),
            no.part.involun.no.temporal = sum(PONDERA[part.time.inv!="Part Involunt" & tiempo.determinado=="No"],na.rm = T),
            tasa.O.part.involun.o.temporal = 1-(no.part.involun.no.temporal/(part.involun.temporal+
                                                                    part.involun.no.temporal+
                                                                    no.part.involun.temporal+
                                                                    no.part.involun.no.temporal)),
            tasa.cooc.involun.temporal = part.involun.temporal/(part.involun.temporal+part.involun.no.temporal),
            part.involun.s_pension = sum(PONDERA[part.time.inv=="Part Involunt" & pension=="No"],na.rm = T),
            part.involun.c_pension =sum(PONDERA[part.time.inv=="Part Involunt" & pension=="Si"],na.rm = T),
            tasa.cooc.invol.s.pension = part.involun.s_pension/(part.involun.s_pension+part.involun.c_pension),
            no.part.involun.s_pension =sum(PONDERA[part.time.inv!="Part Involunt" & pension=="No"],na.rm = T),
            no.part.part.involun.c_pension =sum(PONDERA[part.time.inv!="Part Involunt" & pension=="Si"],na.rm = T),
            tasa.cooc.no.invol.s.pension = no.part.involun.s_pension/(no.part.involun.s_pension+no.part.part.involun.c_pension)) %>% 
  group_by(Pais,ANO4,TRIMESTRE) %>% 
  mutate(Particip_emp = total/sum(total)) %>% 
  ungroup()

indicadores.anuales.asalariados.calif <- perfiles.asalariados.calif %>% 
  group_by(grupos.calif,grupos.tamanio,Pais,ANO4) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  select(-TRIMESTRE) %>% 
  arrange(ANO4,Pais,grupos.tamanio)

###################################Res. Perfiles#############################################

 # write.xlsx(x = list("Ocupados.nivel.ed" = indicadores.anuales.ocupados.nivel.ed,
 #                     "Ocupados.calif" = indicadores.anuales.ocupados.calif,
 #                     "Asalariados.nivel.ed" = indicadores.anuales.asalariados.nivel.ed,
 #                     "Asalariados.calif" = indicadores.anuales.asalariados.calif),
 #            file = "Resultados/Arg_USA_2018.xlsx")  


###################################INGRESOS#############################################
###USA####
 Base.USA.ingresos.ASEC  <- usa.ocup.privados %>% 
   filter(MONTH == 3,ASECFLAG==1) %>% 
   filter(INDLY <9370,#sin Sector publico
          INDLY <9290,#sin serv domestico
          INCWAGE>0,
          INCWAGE!=99999999,
          Categoria=="Asalariados",
          WORKLY==2
   )#Solo asalariados

set.seed(999)  
 Base.USA.ingresos.ASEC.decil <- Base.USA.ingresos.ASEC %>% 
   ungroup() %>% 
   mutate(ingresos.no.salario.ppal = INCBUS+OINCFARM+OINCWAGE+INCFARM,
          ingreso.horario = INCWAGE/WKSWORK1/UHRSWORKLY,
          ingreso.mensual = INCWAGE/WKSWORK1*4,
          ingreso.horario.d = ingreso.horario+runif(nrow(.),min = -.01,max =.01),
          ingreso.mens.d = ingreso.mensual+runif(nrow(.),min = -.01,max =.01)) %>% 
   filter(INCWAGE==INCLONGJ,ingresos.no.salario.ppal== 0) %>% 
   group_by(ANO4) %>% 
   mutate(Decil.ing.hora = statar::xtile(ingreso.horario.d,n=10,w = ASECWT),
          Decil.ing.mens = statar::xtile(ingreso.mens.d,n=10,w = ASECWT)) %>% 
   ungroup() 

 
ing.prom.decil.usa <-  Base.USA.ingresos.ASEC.decil %>% 
   group_by(ANO4,Decil.ing.mens) %>% 
   summarise(ing.prom.decil = weighted.mean(x = ingreso.mensual,
                                            w = ASECWT))
 
 nrow(Base.USA.ingresos.ASEC.decil)
 
 ingresos.asec.asalariados.nivel <- Base.USA.ingresos.ASEC.decil %>% 
   filter(grupos.tamanio != "Ns/Nr",grupos.nivel.ed != "Ns/Nr") %>% 
   left_join(ing.prom.decil.usa) %>% 
   group_by(grupos.nivel.ed,grupos.tamanio,Pais,ANO4) %>%
   summarise(casos.muestrales=n(),
             total = sum(ASECWT,na.rm = TRUE),
             ingreso.horario.prom = weighted.mean(x = ingreso.horario,
                                                  w = ASECWT),
             ingreso.horario.mediana = median(ingreso.horario),
             ingreso.h.coef.variacion = w.cv(ingreso.horario,
                                           ASECWT),
             decil.h.promedio = weighted.mean(Decil.ing.hora,ASECWT,na.rm = T),
             ingreso.mensual.prom = weighted.mean(x = ingreso.mensual,
                                                  w = ASECWT),
             ingreso.mensual.via.decil = weighted.mean(x = ing.prom.decil,
                                                  w = ASECWT),
             ingreso.mensual.mediana = median(ingreso.mensual),
             ingreso.m.coef.variacion = w.cv(ingreso.mensual,
                                             ASECWT),
             decil.m.promedio = weighted.mean(Decil.ing.mens,ASECWT,na.rm = T)) %>% 
   ungroup() %>% 
   mutate(Particip_emp = total/sum(total)*100) %>% 
   ungroup()%>% 
   arrange(ANO4,Pais,grupos.tamanio)
 
ingresos.asec.asalariados.calif <- Base.USA.ingresos.ASEC.decil %>% 
   filter(grupos.tamanio != "Ns/Nr",grupos.calif  %in% c("Alta","Media","Baja")) %>% 
  left_join(ing.prom.decil.usa) %>% 
  group_by(grupos.calif,grupos.tamanio,ANO4,Pais) %>% 
  summarise(casos.muestrales=n(),
            total = sum(ASECWT,na.rm = TRUE),
            ingreso.horario.prom = weighted.mean(x = ingreso.horario,
                                                 w = ASECWT),
            ingreso.horario.mediana = median(ingreso.horario),
            ingreso.h.coef.variacion = w.cv(ingreso.horario,
                                            ASECWT),
            decil.h.promedio = weighted.mean(Decil.ing.hora,ASECWT,na.rm = T),
            ingreso.mensual.prom = weighted.mean(x = ingreso.mensual,
                                                 w = ASECWT),
            ingreso.mensual.via.decil = weighted.mean(x = ing.prom.decil,
                                                      w = ASECWT),
            ingreso.mensual.mediana = median(ingreso.mensual),
            ingreso.m.coef.variacion = w.cv(ingreso.mensual,
                                            ASECWT),
            decil.m.promedio = weighted.mean(Decil.ing.mens,ASECWT,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Particip_emp = total/sum(total)*100) %>% 
  ungroup() %>% 
  arrange(ANO4,Pais,grupos.tamanio)


###ARG####
eph.ocup.privados$PP3E_TOT[eph.ocup.privados$PP3E_TOT == 999] <- NA 
eph.ocup.privados$PP3E_TOT[eph.ocup.privados$PP3E_TOT == 0] <- NA 

set.seed(68150)
EPH.ingresos.deciles <- eph.ocup.privados %>%
  filter(Categoria =="Asalariados", ingreso.mensual>0) %>% 
  mutate(ingreso.horario = ingreso.mensual/30*7/PP3E_TOT,
         ingreso.mensual.d = ingreso.mensual+runif(nrow(.),min = -.01,max =.01),
         ingreso.horario.d = ingreso.horario+runif(nrow(.),min = -.01,max =.01)) %>% 
  group_by(Pais,ANO4,TRIMESTRE) %>% 
  mutate(Decil.ing.mens = statar::xtile(ingreso.mensual.d,n=10,w = PONDERA_SALARIOS),
         Decil.ing.hora = statar::xtile(ingreso.horario.d,
                                           n=10,
                                           w = PONDERA_SALARIOS))

ing.prom.decil.arg <-  EPH.ingresos.deciles %>% 
  group_by(ANO4,TRIMESTRE,Decil.ing.mens) %>% 
  summarise(ing.prom.decil = weighted.mean(x = ingreso.mensual,
                                           w = PONDERA_SALARIOS))


ingresos.eph.asalariados.calif <- EPH.ingresos.deciles %>% 
  filter(grupos.tamanio != "Ns/Nr",grupos.calif  %in% c("Alta","Media","Baja")) %>% 
  left_join(ing.prom.decil.arg) %>% 
  group_by(grupos.calif,grupos.tamanio,Pais,ANO4) %>% 
  summarise(casos.muestrales=n(),
            total = sum(PONDERA_SALARIOS,na.rm = TRUE),
            ingreso.horario.prom = weighted.mean(x = ingreso.horario,
                                                 w = PONDERA_SALARIOS,na.rm = T),
            ingreso.horario.mediana = median(ingreso.horario,na.rm = T),
            ingreso.h.coef.variacion = w.cv(ingreso.horario[!is.na(ingreso.horario)],
                                            PONDERA_SALARIOS[!is.na(ingreso.horario)]),
            decil.h.promedio = weighted.mean(Decil.ing.hora,
                                             PONDERA_SALARIOS,na.rm = T),
            ingreso.mensual.prom = weighted.mean(x = ingreso.mensual,
                                                 w = PONDERA_SALARIOS,na.rm = T),
            ingreso.mensual.via.decil = weighted.mean(x = ing.prom.decil,
                                                      w = PONDERA_SALARIOS,na.rm = T),
            ingreso.mensual.mediana = median(ingreso.mensual,na.rm = T),
            ingreso.m.coef.variacion = w.cv(ingreso.mensual,
                                            PONDERA_SALARIOS/1000000),
            decil.m.promedio = weighted.mean(Decil.ing.mens,
                                             PONDERA_SALARIOS,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Particip_emp = total/sum(total)*100) %>% 
  ungroup() %>% 
  arrange(ANO4,Pais,grupos.tamanio)

set.seed(68150)
EPH.ingresos.deciles.ocupados <- eph.ocup.privados %>%
  filter(ingreso.mensual>0) %>% 
  mutate(ingreso.horario = ingreso.mensual/30*7/PP3E_TOT,
         ingreso.mensual.d = ingreso.mensual+runif(nrow(.),min = -.01,max =.01),
         ingreso.horario.d = ingreso.horario+runif(nrow(.),min = -.01,max =.01)) %>% 
  group_by(Pais,ANO4,TRIMESTRE) %>% 
  mutate(Decil.ing.mens = statar::xtile(ingreso.mensual.d,n=10,w = PONDERA_SALARIOS),
         Decil.ing.hora = statar::xtile(ingreso.horario.d,
                                        n=10,
                                        w = PONDERA_SALARIOS))



ingresos.eph.ocupados.calif <- EPH.ingresos.deciles.ocupados %>% 
  filter(grupos.tamanio != "Ns/Nr",grupos.calif  %in% c("Alta","Media","Baja")) %>% 
  group_by(grupos.calif,grupos.tamanio,Pais,ANO4) %>% 
  summarise(casos.muestrales=n(),
            total = sum(PONDERA_SALARIOS,na.rm = TRUE),
            ingreso.horario.prom = weighted.mean(x = ingreso.horario,
                                                 w = PONDERA_SALARIOS,na.rm = T),
            ingreso.horario.mediana = median(ingreso.horario,na.rm = T),
            ingreso.h.coef.variacion = w.cv(ingreso.horario[!is.na(ingreso.horario)],
                                            PONDERA_SALARIOS[!is.na(ingreso.horario)]),
            decil.h.promedio = weighted.mean(Decil.ing.hora,
                                             PONDERA_SALARIOS,na.rm = T),
            ingreso.mensual.prom = weighted.mean(x = ingreso.mensual,
                                                 w = PONDERA_SALARIOS,na.rm = T),
            ingreso.mensual.mediana = median(ingreso.mensual,na.rm = T),
            ingreso.m.coef.variacion = w.cv(ingreso.mensual,
                                            PONDERA_SALARIOS/1000000),
            decil.m.promedio = weighted.mean(Decil.ing.mens,
                                             PONDERA_SALARIOS,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Particip_emp = total/sum(total)*100) %>% 
  ungroup() %>% 
  arrange(ANO4,Pais,grupos.tamanio)

###################################Res. ingresos#############################################
write.xlsx(x = list("USA NIVEL ED" = ingresos.asec.asalariados.nivel,
                    "USA CALIF" = ingresos.asec.asalariados.calif,
                    "ARG NIVEL ED" = ingresos.eph.asalariados.nivel ,
                    "ARG CALIF" =ingresos.eph.asalariados.calif),
           file = "Resultados/INGRESOS_Arg_USA_2018.xlsx")  

save(insercion.niveles.arg,
  insercion.niveles.usa,
  cruces.educ.calif.abs,
  desocup.calif.ant.usa,
  desocup.calif.ant.arg,
  indicadores.anuales.ocupados.calif,
  indicadores.anuales.asalariados.calif,
  ingresos.asec.asalariados.calif,
  ingresos.eph.asalariados.calif,
  ingresos.eph.ocupados.calif,
  file = "Resultados/ARG_USA.RDATA")

  