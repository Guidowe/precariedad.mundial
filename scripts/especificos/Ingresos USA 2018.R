####funciones y librerias#####
  knitr::opts_chunk$set(echo = FALSE,warning = FALSE,message = FALSE)
  knitr::opts_chunk$set(fig.width = 10)
  
  
  
  library(ipumsr)
  library(eph)
  library(ggthemes)
  library(ggalt)
  library(tidyverse)
  library(kableExtra)
  library(formattable)
  library(openxlsx)
  library(Weighted.Desc.Stat)
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
  
####bases de datos#####
  
  ocup_usa <- read.xlsx("data/Codigos Ocup USA.xlsx",sheet = "OCC 2011a2019")
  ramas_usa <- read.xlsx("data/Codigos Ocup USA.xlsx",sheet = "IND 2014a2019")
  soc_census <- read.xlsx("data/Codigos Ocup USA.xlsx",sheet = "SOC CENSO cross para R")
  soc_isco <- read.xlsx("data/Codigos Ocup USA.xlsx",sheet = "SOC ISCO cross para R")
  skills_isco <- read.xlsx("data/Codigos Ocup USA.xlsx",sheet = "Skill levels ISCO")
  
  
  cps_ddi_file <- "../bases/cps_00005.xml"
  cps_data_file <- "../bases/cps_00005.dat"
  cps_ddi <- read_ipums_ddi(cps_ddi_file) 
  Base_USA <- ipumsr::read_ipums_micro(ddi = cps_ddi_file,
                                   data_file =  cps_data_file)



cross.census.a.soc <- ocup_usa %>% 
  full_join(soc_census %>% mutate(OCC=as.numeric(`2010.Census.Code`))) %>% 
  select(Census = OCC,Census.title = Description,
         SOC.title = `2010.Occupation.Title`,
         SOC = `2010.SOC.Code`) %>% 
  mutate(SOC.title = case_when(is.na(SOC.title)~Census.title,
                               !is.na(SOC.title)~SOC.title),
         SOC = case_when(Census %in%  c(9840,9830)~"55-3010",
                         Census== 4550~"39-7010",
                         Census== 1000~"15-1131",
                         TRUE~SOC)) %>% 
  filter(!is.na(Census))
  
cross.soc.a.isco <-  soc_isco %>% 
  select(SOC = `2010.SOC.Code`,SOC.title = `2010.SOC.Title`,part, 
         ISCO = `ISCO-08.Code`,ISCO.title = `ISCO-08.Title.EN`) %>% 
  mutate(ISCO.1.digit = substr(ISCO,1,1)) %>% 
  left_join(skills_isco %>% mutate(ISCO.1.digit = as.character(ISCO.1.digit)))

# cross.soc.a.isco.1.digit  <- cross.soc.a.isco %>%
#   select(SOC,SOC.title,subjetividad) %>% 
#   unique()

duplicados <- cross.soc.a.isco %>% 
  group_by(SOC) %>% 
  summarise(Casos.distintos = n()) %>% 
  filter(Casos.distintos>1)

####Intento crosswalk para 2018#####
Base_Usa_census <- head(Base_USA %>% filter(ASECFLAG==1)) %>% 
  rename(Census = OCCLY) %>%  
  left_join(cross.census.a.soc)

####Analisis suplementos y tamaños de muestra #####
#1 La variable de FIRMSIZE solo esta en ASEC (Marzo de cada año)
#2 Reduciendo el universo a ASEC hay 2 variables de ingresos:INCLONG y EARNWEEK
#3 INCLONG: pregunta por el ingreso total durante el año calendario anterior
 #del trabajo que tuvo por más tiempo
#4 EARNWEEK: pregunta por el ingreso semanal 
  #que usualmente gana por el trabajo actual (before deductions)  

# La muestra mensual tiene 125 mil casos
nrow(Base_USA %>% filter(MONTH == 1))
# De los cuales un 10% aprox (13mil) etran en el Outgoing Rotation Group
nrow(Base_USA %>% filter(MONTH == 1,ELIGORG==1))

# Una muestra de ASEC tiene 180 mil casos
nrow(Base_USA %>% filter(MONTH == 3,ASECFLAG==1))

# Acotando ASEC a asalariados 79.600 casos, que ponderados son 139 millones
Base_USA %>% filter(MONTH == 3,ASECFLAG==1,CLASSWKR %in% 21:28,
                    WKSTAT %in% 11:42) %>% 
    summarise(sum(ASECWT))  
# Acotando la muestra de ASEC al Outgoing Rotation tenemos 18 mil casos (10%)
nrow(Base_USA %>% filter(MONTH == 3,ASECFLAG==1,ELIGORG == 1))

outgoing_rotation  <- Base_USA.c %>% 
  filter(MONTH == 3,ELIGORG == 1,ASECFLAG==1)%>% 
  select(Categoria,WKSTAT,CLASSWKR,EARNWT,EARNWEEK)  
# Acotando la muestra de ASEC al Outgoing Rotation e ingresos
## respondidos son 12 mil casos
nrow(Base_USA %>% filter(MONTH == 3,ASECFLAG==1,ELIGORG == 1,EARNWEEK!= 9999.99))## respondidos son 12 mil casos

## Estos 12 mil casos ponderan por 128,5 millones de personas 
#(Total de asalariados!!!)
  Base_USA %>% 
    filter(MONTH == 3,ASECFLAG==1,ELIGORG == 1,EARNWEEK!= 9999.99) %>% 
    summarise(sum(EARNWT))

###  Total de Población Estados UNIDOS. Ponderaciones ASECWT
###  Total de Asalariados Estados UNIDOS. Ponderaciones ASECWT
Base_USA %>% 
    filter(MONTH == 3,ASECFLAG==1) %>% 
    summarise(sum(ASECWT))

Base_USA %>% 
  filter(MONTH == 3,ASECFLAG==1,CLASSWLY  %in% 20:28) %>% 
  summarise(sum(ASECWT))

Base_USA %>% 
  filter(MONTH == 3) %>% 
  summarise(sum(WTFINL,na.rm = T))

####Variables y categorias #####

  
listado.variables.USA <- cps_ddi[["var_info"]]
variables.categorias.USA<- unnest(listado.variables.USA,val_labels)
  
  Variables.USA <- c("FIRMSIZE","EDUC","LABFORCE","EMPSTAT",
                     "CLASSWKR","CLASSWLY",
                     "WKSTAT","FULLPART",
                     "WHYPTLY","WHYPTLWK",
                     "IND","INDLY",
                     "OCC","OCCLY",
                     "EARNWEEK","PENSION",
                     "INCWAGE","INCBUS","INCLONGJ","SRCEARN",
                     "INCTOT","EARNWEEK",
                     "ASECWT","EARNWT") 
    
  var.cat.utilizadas.USA <- variables.categorias.USA %>% 
    filter(var_name %in% Variables.USA)
  
  var.utilizadas.USA <- listado.variables.USA %>% 
    filter(var_name %in% Variables.USA)
  
####Lineas para ver descripción y categorias de las variables####
  estadousa <- ipums_val_labels(cps_ddi, var = "WHYPTLY")
#listado.variables$var_desc[listado.variables$var_name=="FIRMSIZE"]
  
  ###EPH##
  #Bases_eph <- readRDS("../bases/eph2019.RDS")
  Bases_eph <- eph::get_microdata(year = 2018,trimester = 1:4)
  
  bases_bind <- Bases_eph %>%
    dplyr::select(microdata) %>%
    tidyr::unnest(cols = c(microdata))
  
####Reclas USA####
  Base_USA.c <- Base_USA %>% 
    mutate(grupos.tamanio = factor(case_when(FIRMSIZE==1~"Pequeño",
                                     FIRMSIZE %in% 2:4~"Mediano",
                                     FIRMSIZE %in% 5:9~"Grande",
                                     FIRMSIZE %in% 0 ~ "Ns/Nr"),
                          levels = c("Pequeño","Mediano","Grande","Ns/Nr")),
           grupos.nivel.ed = factor(case_when(EDUC %in% 2:72~ "Menor a Secundaria",
                                 EDUC %in% 73:110~ "Secundaria Completa",
                                 EDUC %in% 111:125~ "Superior Completo",
                                 TRUE ~ "Ns/Nr"),
                              c("Menor a Secundaria","Secundaria Completa","Superior Completo")),
            Categoria.ly =  case_when(CLASSWLY %in% 10:19 ~ "Patrones y CP",
                             CLASSWLY  %in% 20:28 ~ "Asalariados",
                             CLASSWLY == 29 ~ "TFSR",
                             CLASSWLY == 99 ~ "Ns/Nr"),
           Categoria =  case_when(CLASSWKR %in% 10:19 ~ "Patrones y CP",
                                  CLASSWKR  %in% 20:28 ~ "Asalariados",
                                  CLASSWKR == 29 ~ "TFSR",
                                  CLASSWKR == 99 ~ "Ns/Nr"),
           pension = case_when(PENSION %in% 1:2 ~ "No",
                               PENSION %in% 3 ~ "Si",
                               PENSION %in% 0 ~ "NIU"),
           part.time.inv = case_when(WKSTAT  %in%  20:42 &
                                     WHYPTLWK %in% c(10:40,
                                                     52,60,17:81,
                                                     4)~"Involunt",
                                     TRUE ~ "Resto"),
           precario.part.ly = case_when(FULLPART == 2 & WHYPTLY %in% c(1,3,4)~"Precario",
                                     TRUE ~ "Resto"))

Base.USA.ingresos  <- Base_USA.c %>% 
    filter(MONTH == 3,ASECFLAG==1,ELIGORG==1,EARNWEEK!= 9999.99) %>% 
  filter(INDLY <9370,#sin Sector publico
                  INDLY <9290)#sin Sector publico ni S. doméstico 

# Actuales <- Base.USA.ingresos %>% 
#   select(Categoria,WKSTAT,USFTPTLW,WHYPTLWK,CLASSWKR,FIRMSIZE,EARNWEEK,EDUC,EARNWT,precario.part,
#          grupos.nivel.ed,grupos.tamanio)  
# 
# table(Actuales$FIRMSIZE)
# table(Actuales$WHYPTLWK)
  
Base.USA.ingresos.decil <- Base.USA.ingresos %>%
  filter(EARNWEEK>0) %>% 
  mutate(EARNWEEK_d = EARNWEEK+runif(nrow(.),min = -.01,max =.01)) %>% 
  mutate(Decil = statar::xtile(EARNWEEK_d,n=10,w = EARNWT)) 
  
  ingresos.aslariados <- Base.USA.ingresos.decil %>% 
    filter(grupos.tamanio != "Ns/Nr",grupos.nivel.ed != "Ns/Nr") %>% 
    group_by(grupos.nivel.ed,grupos.tamanio) %>% 
    summarise(casos.muestrales=n(),
              total = sum(EARNWT,na.rm = TRUE),
              # asalariados = sum(EARNWT[Categoria=="Asalariados"]),
              # tasa.asalarizacion = asalariados/total,
              # subocup.horaria = sum(PONDERA[precario.part=="Precario"]),
              # tasa.subocup.horaria = subocup.horaria/total*100,
              # s_desc_jubilat =sum(PONDERA[descuento_jubil=="No"],na.rm = T),
              # c_desc_jubilat =sum(PONDERA[descuento_jubil=="Si"],na.rm = T),
              # s_pension =sum(PONDERA[pension=="No"],na.rm = T),
              # c_pension =sum(PONDERA[pension=="Si"],na.rm = T),
              # tasa.s.desc.jubil = s_desc_jubilat/(c_desc_jubilat+s_desc_jubilat),
              # tasa.s.pension = s_pension/(c_pension+s_pension),
              ingreso.semanal.prom = weighted.mean(EARNWEEK,
                                                   EARNWT),
              ingreso.seumanal.mediana = median(EARNWEEK),
              ingreso.coef.variacion = w.cv(EARNWEEK,
                                            EARNWT),
              decil.promedio = weighted.mean(Decil,EARNWT,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Particip_emp = total/sum(total)*100) %>% 
  ungroup()


##############
  Base.USA.ingresos.ASEC  <- Base_USA.c %>% 
  filter(MONTH == 3,ASECFLAG==1) %>% 
  filter(INDLY <9370,#sin Sector publico
         INDLY <9290,#sin serv domestico
         INCWAGE>0,
         INCWAGE!=99999999,
         Categoria.ly=="Asalariados",
         WORKLY==2
         )#Solo asalariados

Base.USA.ingresos.ASEC.decil <- Base.USA.ingresos.ASEC %>% 
  ungroup() %>% 
  mutate(ingresos.no.salario.ppal = INCBUS+OINCFARM+OINCWAGE+INCFARM,
         ingreso.horario = INCWAGE/WKSWORK1/UHRSWORKLY,
         ingreso.horario.d = ingreso.horario+runif(nrow(.),min = -.01,max =.01)) %>% 
  filter(INCWAGE==INCLONGJ,ingresos.no.salario.ppal== 0) %>% 
  mutate(Decil = statar::xtile(ingreso.horario.d,n=10,w = ASECWT)) %>% 
  select(INCTOT,INCWAGE,INCBUS,INCFARM,ingresos.no.salario.ppal,INCLONGJ,OINCFARM,OINCWAGE,WKSWORK1,UHRSWORKLY,ingreso.horario.d,ingreso.horario,
         grupos.tamanio,grupos.nivel.ed,ASECWT,INDLY,Decil)
nrow(Base.USA.ingresos.ASEC.decil)

ingresos.asec.asalariados <- Base.USA.ingresos.ASEC.decil %>% 
  filter(grupos.tamanio != "Ns/Nr",grupos.nivel.ed != "Ns/Nr") %>% 
  group_by(grupos.nivel.ed,grupos.tamanio) %>% 
  summarise(casos.muestrales=n(),
            total = sum(ASECWT,na.rm = TRUE),
            ingreso.horario.prom = weighted.mean(x = ingreso.horario,
                                            w = ASECWT),
            ingreso.horario.mediana = median(ingreso.horario),
            ingreso.coef.variacion = w.cv(ingreso.horario,
                                          ASECWT),
            decil.promedio = weighted.mean(Decil,ASECWT,na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Particip_emp = total/sum(total)*100) %>% 
  ungroup()

