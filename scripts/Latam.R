library(eph)
library(xml2)
library(tidyverse)
library(ipumsr)
library(foreign)
####Lectura bases original####
# costarica<- read.spss('../bases/Costa Rica/ENAHO 2019.sav',
#                       reencode = "UTF-8",use.value.labels = F,
#                       to.data.frame = T) %>% 
#   mutate(periodo = 2019)
#saveRDS(costarica,file = "Bases/costarica_2019.RDS")


# colombia<- read.spss('../bases/Colombia/Diciembre.spss/Cabecera - Ocupados.sav',
#                       reencode = "UTF-8",use.value.labels = F,
#                       to.data.frame = T) %>% 
#   mutate(periodo = 122019)
#saveRDS(colombia,file = "Bases/colombia_122019.RDS")

# guatemala<- read.spss('../bases/Guatemala/2017 guatemala.sav',
#                       reencode = "UTF-8",use.value.labels = F,
#                       to.data.frame = T) %>% 
#   mutate(periodo = 2017)
#saveRDS(guatemala,file = "Bases/guatemala_2017.RDS")

# ecuador <- read.spss("../bases/Ecuador/enemdu_persona_201912.sav",
#                      reencode = "UTF-8",use.value.labels = F,
#                      to.data.frame = T)
# saveRDS(ecuador,file = "Bases/ecuador_122019.RDS")

# elsalvador <- read.spss("../bases/El Salvador/El Salvador 2016.sav",
#                         reencode = "UTF-8",use.value.labels = F,
#                         to.data.frame = T)
# 
# saveRDS(elsalvador,file = "Bases/elsalvador_2016.RDS")

# mexico.basico <- read_csv("../bases/Mexico/SDEMT319.csv.CSV")
# mexico.ampliado <- read_csv("../bases/Mexico/COE1T319.csv.CSV")
# mexico.ampliado2 <- read_csv("../bases/Mexico/COE2T319.csv.CSV")
# mex.variables.ampliado <- mexico.ampliado %>%
#   mutate(eda = as.character(eda)) %>%
#   select(1:16,p3)
# mex.variables.ampliado2 <- mexico.ampliado2 %>%
#   mutate(eda = as.character(eda)) %>%
#   select(1:16,p6b2)
#  mexico <- mexico.basico %>% 
#    left_join(mex.variables.ampliado) %>% 
#    left_join(mex.variables.ampliado2)  
#saveRDS(mexico,file = "Bases/mexico_T32019.RDS")


# canada <- read.csv("../bases/Canada/pub1219.csv")
#  saveRDS(canada,file = "Bases/canada_122019.RDS")
####Bases RDS####
costarica<-readRDS(file = "Bases/costarica_2019.RDS")
colombia<-readRDS(file = "Bases/colombia_122019.RDS")
guatemala<-readRDS(file = "Bases/guatemala_2017.RDS")
ecuador<-readRDS(file = "Bases/ecuador_122019.RDS")
mexico<-readRDS(file = "Bases/mexico_T32019.RDS")
elsalvador<-readRDS(file = "Bases/elsalvador_2016.RDS")
canada<-readRDS(file = "Bases/canada_122019.RDS")
####Colombia####
##Miro variables##
# table(colombia$CLASE)
# table(colombia$P6430)
# table(colombia$P6460)
# table(colombia$P6440)
# table(colombia$P6450)
# table(colombia$OFICIO)
##Proceso##
co.categ <- colombia %>% 
  rename(fexp = fex_c_2011) %>% 
  filter(CLASE == 1) %>% # Ubana
  filter(! P6430  %in%  c(2,3)) %>% # saco S.Pub y S.Dom
   #   filter(P6430 == 1) %>% # Asalariad
  mutate(
    registracion =  case_when(P6440 == 1 & P6450 == 2 ~ "Si",
                              TRUE ~ "No"),
    part.time.inv = case_when(P6800 < 35 & P6810 == 1 ~ "Part Involunt",
                              P6800 < 35 & P6810 != 1 ~ "Part Volunt",
                              P6800 >= 35 ~ "Full Time"), 
    tiempo.determinado = case_when(
      P6460 == 1 ~ "No",
      P6460 == 2  ~ "Si"),
    grupos.calif =   case_when(
      substr(OFICIO,1,1) == 0|
      substr(OFICIO,2,2) %in%  1:2 ~ "Alta",
      substr(OFICIO,1,1) != 0 &
      substr(OFICIO,2,2) %in% 3:5 ~ "Media",
      substr(OFICIO,1,1) != 0 &
        substr(OFICIO,2,2) %in% 6:9 ~ "Baja"),
    grupos.tamanio =
      case_when(
        P6870  %in% 1:4 ~ "Pequeño", # 1 a 10
        P6870  %in% 5:7 ~ "Mediano", # 11 a 50
        P6870  %in% 8:9  ~ "Grande")#  51 +
  ) 

# ver <- ec.categ %>% 
#   group_by(part.time.inv,grupos.tamanio,grupos.calif) %>% 
#   summarise(casos = n())
# 
# table(ec.categ$ISCO.1.digit)

co.tasa.asalariz <- co.categ %>% 
  summarise(total.ocupados = sum(fexp,na.rm = T),
            asalariados = sum(fexp[P6430  %in%  1:2]),
            tasa.asalarizacion = asalariados/total.ocupados) 


co.ocupados.distrib  <- co.categ %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(ocupados = sum(fexp,na.rm = T),
            asalariados = sum(fexp[P6430 %in% 1],na.rm = T),
            no.asalariados = sum(fexp[P6430 != 1],na.rm = T),
            tasa.asalarizacion = asalariados/ocupados,
            promedio.ing.oc.prin=weighted.mean(
              x = INGLABO,
              w = fexp,na.rm = T),
            promedio.ing.oc.prin.noasal=weighted.mean(
              x = INGLABO[P6430 != 1],
              w = fexp[P6430 != 1],na.rm = T),
            promedio.ing.oc.prin.asal=weighted.mean(
              x = INGLABO[P6430 == 1],
              w = fexp[P6430 == 1],na.rm = T)
            ) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.no.asal= no.asalariados/sum(no.asalariados))

co.asalariados.tasas <- co.categ %>% 
  filter(P6430 %in% 1) %>% # Asalariad S.priv
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    registrados =sum(fexp[registracion=="Si"],na.rm = T),
    no.registrados =sum(fexp[registracion=="No"],na.rm = T),
    empleo.temporal =sum(fexp[tiempo.determinado=="Si"],na.rm = T),
    empleo.no.temporal =sum(fexp[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(fexp[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(fexp[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(fexp[part.time.inv=="Full Time"],na.rm = T),
    tasa.partime.asal = part.involun/(part.involun+
                                      part.volunt+
                                      full.time),
    tasa.temp.asal = empleo.temporal/(empleo.temporal+
                                      empleo.no.temporal),
    tasa.no.registro = no.registrados/(registrados+
                                       no.registrados)) 

co.resultado <- co.ocupados.distrib %>% 
  left_join(co.asalariados.tasas) 

####Ecuador####
##Miro variables##
 # table(ecuador$area)
 # table(ecuador2$area)
 # table(ecuador$p27)
 # table(ecuador2$p27)
# table(ecuador$p44f)
# table(ecuador$p24)
# table(ecuador$p27,useNA = "always")
# table(ecuador$p41)
# table(ecuador$p47a)
# table(ecuador$p47b)
# aaa <- data.frame(table(ecuador$p47b,ecuador$p47a,useNA = "always"))
# ver <- ecuador %>% 
#   filter(condact %in% 2:7) %>% #Ocupados
#   filter(p42 %in% c(2,4,5,6)) %>% 
#   group_by(p27) %>% 
#   summarise(casos = n())

##Proceso##

ec.categ <- ecuador %>% 
  filter(area == 1) %>% #Urbanos
  filter(condact %in% 2:7) %>% #Ocupados
  filter(p42 %in% c(2,4,5,6)) %>% # CP, PaAtron y SPriv sin S/D
  #   filter(p42 == 2) %>% # Asalariad
  mutate(
    registracion =  case_when(p44f == 1 ~ "Si",
                              p44f == 2 ~ "No"),
    part.time.inv = case_when(p24 < 35 & p27%in% 1:3 ~ "Part Involunt",
                              p24 < 35 & p27 %in% 4 ~ "Part Volunt",
                              p24 >= 35 ~ "Full Time"), 
    # tiempo.determinado = case_when(
    #   Estabili == 10 ~ "No",
    #   Estabili != 10 ~ "Si"),
    ISCO.1.digit = str_sub(p41,1,1), 
    grupos.calif =   case_when(
      ISCO.1.digit %in% 1:3 ~ "Alta",
      ISCO.1.digit %in% 4:8 ~ "Media",
      ISCO.1.digit %in% 9 ~ "Baja"),
    grupos.tamanio =
      case_when(
        p47b %in% 1:9 ~ "Pequeño", # 1 a 9
        p47b %in% 10:50 ~ "Mediano", # 10 a 50
        p47b > 50 | p47a == 2 ~ "Grande")#  + de  100
    ) 

# ver <- ec.categ %>% 
#   group_by(part.time.inv,grupos.tamanio,grupos.calif) %>% 
#   summarise(casos = n())
# 
# table(ec.categ$ISCO.1.digit)

ec.tasa.asalariz <- ec.categ %>% 
  summarise(total.ocupados = sum(fexp,na.rm = T),
            asalariados = sum(fexp[p42 == 2]),
            tasa.asalarizacion = asalariados/total.ocupados) 


ec.ocupados.distrib  <- ec.categ %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    ocupados = sum(fexp,na.rm = T),
    asalariados = sum(fexp[p42 == 2],na.rm = T),
    no.asalariados = sum(fexp[p42 != 2],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
            x = ingrl,
            w = fexp,na.rm = T),
          promedio.ing.oc.prin.noasal=weighted.mean(
            x = ingrl[p42!= 2],
            w = fexp[p42!= 2],na.rm = T),
          promedio.ing.oc.prin.asal=weighted.mean(
            x = p66[p42== 2],
            w = fexp[p42== 2],na.rm = T)
) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.no.asal= no.asalariados/sum(no.asalariados))


# ec.asalariados.salario <- cr.prueba %>% 
#   filter(PosiEmpPri == 12) %>% # Asalariad
#   filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
#   filter(E12A > 2, E12A!=99999999) %>% 
#   filter(E12B == 1) %>% #salario bruto
#   group_by(grupos.calif,grupos.tamanio) %>% 
#   summarise(
#     sal.prom = weighted.mean(E12A,FACTOR,na.rm = T))

ec.asalariados.tasas <- ec.categ %>% 
  filter(p42 == 2) %>% # Asalariad
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    registrados =sum(fexp[registracion=="Si"],na.rm = T),
    no.registrados =sum(fexp[registracion=="No"],na.rm = T),
    # empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    # empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(fexp[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(fexp[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(fexp[part.time.inv=="Full Time"],na.rm = T),
    tasa.partime.asal = part.involun/(part.involun+
                                      part.volunt+
                                      full.time),
    tasa.no.registro = no.registrados/(registrados+
                                      no.registrados))

ec.resultado <- ec.ocupados.distrib %>% 
  left_join(ec.asalariados.tasas) 
  
####Mexico####
#Miro algunas variables#
# codigos.ocup <- data.frame(unique(mexico$p3))
# 
# asalad.mex <- mexico %>% 
#   filter(clase2 == 1) %>% 
#   filter(pos_ocu == 1)  
#ver <- calculate_tabulates(mexico,"t_loc","cd_a")
# table(asalad.mex$tip_con)
# table(asalad.mex$dur_est)
# table(asalad.mex$hrsocup)
# si<- calculate_tabulates(asalad.mex,"sub_o","dur_est")
# si<- calculate_tabulates(asalad.mex,"s_clasifi","dur_est")
# 
# si <- data.frame(table(mexico$tue1,mexico$tue2))
# check <- data.frame(table(mexico$tip_con,mexico$pre_asa),use_na = "always")

#Proceso#
mex.cat <- mexico %>% 
  filter(t_loc != 4) %>%  #Localidades mayores a 2500
  filter(clase2 == 1) %>%  #Ocupados
  filter((tue1  %in%  c(1,4))   |
         (tue1 == 2 & tue2 == 3)|
         (tue1 == 3 & tue3 == 5)) %>% # Spriv s/ serv domestico
  # filter(pos_ocu == 1) %>% # Asalariado %>% 
   mutate(
     periodo = per,
     FACTOR  = fac,
     grupos.tamanio = 
       case_when(
       emple7c %in% 1:3 ~ "Pequeño", # 1 a 10
       emple7c %in% 4:5 ~ "Mediano", # 11 a 50
       emple7c %in% 6:5 ~ "Grande"), # + 51 
    registracion =  case_when(pre_asa %in% 1 ~ "Si",
                              TRUE ~ "No"),
    part.time.inv = case_when(dur_est %in%  2:3 & sub_o == 1 ~ "Part Involunt",
                              dur_est %in%  2:3 & sub_o == 0 ~ "Part Volunt",
                              dur_est %in% 4:5 ~ "Full Time"), 
    tiempo.determinado = case_when(
      tip_con %in% 1:3 ~ "No",
      TRUE   ~ "Si"),
    grupos.calif =   ### CORREGIR
      case_when(
        substr(p3,1,1) %in% 1:3 ~ "Alta",
        substr(p3,1,1) %in% 4:8 ~ "Media",
        substr(p3,1,1) %in% 9 ~ "Baja")
      ) 
mex.ocupados.distrib <-  mex.cat  %>% 
  mutate(p6b2 = as.numeric(p6b2)) %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
  ocupados = sum(FACTOR,na.rm = T),
  asalariados = sum(FACTOR[pos_ocu == 1],na.rm = T),
  no.asalariados = sum(FACTOR[pos_ocu != 1],na.rm = T),
  tasa.asalarizacion = asalariados/ocupados,
  promedio.ing.oc.prin=weighted.mean(
    x = p6b2[p6b2 != 999999],
    w = FACTOR[p6b2 != 999999],na.rm = T),
  promedio.ing.oc.prin.noasal=weighted.mean(
    x = p6b2[pos_ocu!= 1 & p6b2!= 999999],
    w = FACTOR[pos_ocu!= 1 & p6b2!= 999999],na.rm = T),
  promedio.ing.oc.prin.asal=weighted.mean(
    x = p6b2[pos_ocu== 1 & p6b2!= 999999],
    w = FACTOR[pos_ocu== 1 & p6b2!= 999999],na.rm = T)
) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.no.asal= no.asalariados/sum(no.asalariados))


mex.asalariados.tasas <- mex.cat %>% 
  filter(pos_ocu == 1) %>% # Asalariado
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    registrados =sum(FACTOR[registracion=="Si"],na.rm = T),
    no.registrados =sum(FACTOR[registracion=="No"],na.rm = T),
    empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(FACTOR[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(FACTOR[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(FACTOR[part.time.inv=="Full Time"],na.rm = T),
    tasa.partime.asal = part.involun/(part.involun+
                                      part.volunt+
                                      full.time),
    tasa.no.registro = no.registrados/(registrados+
                                       no.registrados),
    tasa.temp.asal = empleo.temporal/(empleo.temporal+
                                     empleo.no.temporal))

mex.resultado <- mex.ocupados.distrib %>%
  left_join(mex.asalariados.tasas)


####Costa Rica####
# table(costarica$OcupEmpPri)
# table(costarica$C10)
# table(costarica$CondAct)
# table(costarica$PosiEmpPri)
# table(costarica$E9A)
# table(costarica$C2A1)
# table(costarica$E10A)
# table(costarica$Estabili)
# weighted.mean(costarica$C2A1,costarica$FACTOR,na.rm = T) #horas

cr.categ <- costarica %>% 
    filter(ZONA == 1) %>% #Urbano
    filter(CondAct %in% 1) %>% #Ocupados
    filter(SecInsPri %in% 3) %>% # Spriv
    filter(PosiEmpPri != 11) %>% # No serv domestico
#    filter(PosiEmpPri == 12) %>% # Asalariad
    mutate(
      registracion =  case_when(E9A == 1 ~ "Si",
                                E9A == 2 ~ "No"),
      part.time.inv = case_when(C2A1 < 35 & C3 == 1 ~ "Part Involunt",
                                C2A1 < 35 & C3 == 2 ~ "Part Volunt",
                                C2A1 >= 35 ~ "Full Time"), 
      tiempo.determinado = case_when(
          Estabili == 10 ~ "No",
          Estabili != 10 ~ "Si"),
      grupos.calif =   case_when(
        OcupEmpPri %in% 1:3 ~ "Alta",
        OcupEmpPri %in% 4:8 ~ "Media",
        OcupEmpPri %in% 9 ~ "Baja"),
      grupos.tamanio =
        case_when(
          C10 %in% 1:9 ~ "Pequeño", # 1 a 9
          C10 %in% 10:11 ~ "Mediano", # 10 a 30
          C10 %in% 12 ~ "Mediano", #  30 a 100
          C10 %in% 13 ~ "Grande")) #  + de  100

cr.tasa.asalariz <- cr.categ %>% 
  summarise(total.ocupados = sum(FACTOR,na.rm = T),
            total.asal = sum(FACTOR[PosiEmpPri == 12]),
            tasa.asalarizacion = total.asal/total.ocupados) 
  
cr.ocupados.distrib  <- cr.categ %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
  ocupados = sum(FACTOR,na.rm = T),
  asalariados = sum(FACTOR[PosiEmpPri == 12],na.rm = T),
  no.asalariados = sum(FACTOR[PosiEmpPri != 12],na.rm = T),
  tasa.asalarizacion = asalariados/ocupados,
  promedio.ing.oc.prin=weighted.mean(
    x = ipnt,
    w = FACTOR,na.rm = T),
  promedio.ing.oc.prin.noasal=weighted.mean(
    x = ipnt[PosiEmpPri!= 12],
    w = FACTOR[PosiEmpPri!= 12],na.rm = T),
  promedio.ing.oc.prin.asal=weighted.mean(
    x = ipnt[PosiEmpPri== 12 ],
    w = FACTOR[PosiEmpPri== 12 ],na.rm = T)
) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.no.asal= no.asalariados/sum(no.asalariados))

cr.asalariados.tasas <- cr.categ %>% 
  filter(PosiEmpPri == 12) %>% # Asalariad
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    registrados =sum(FACTOR[registracion=="Si"],na.rm = T),
    no.registrados =sum(FACTOR[registracion=="No"],na.rm = T),
    empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(FACTOR[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(FACTOR[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(FACTOR[part.time.inv=="Full Time"],na.rm = T),
    tasa.partime.asal = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp.asal = empleo.temporal/(empleo.temporal+
                                        empleo.no.temporal))

cr.resultado <- cr.ocupados.distrib %>%
  left_join(cr.asalariados.tasas)
# openxlsx::write.xlsx(x = list(cr.tasa.asalariz,
#                               cr.ocupados.distrib,
#                               cr.asalariados.tasas),
#                      file = "Resultados/series_ejemplo.xlsx")
  
####Guatemala####
# table(guatemala$P04C02B_1D)
# table(guatemala$P04C05)
# table(guatemala$AREA)
# table(guatemala$P04C06,useNA = "always")
#table(guatemala$OCUPADOS,useNA = "always")

guatemala.cat <- guatemala %>% 
  mutate(FACTOR = Factor_expansion) %>% 
  filter(AREA == 1) %>%  #area urbana
  filter(OCUPADOS == 1) %>%  #Ocupados
  filter(!(P04C06 %in% c(1,4))) %>% # Spriv s/ serv domestico
 # filter(P04C06 == 2) %>% # Asalariado
  mutate(
    horas.semana = P04C28A+P04C28B+P04C28C+P04C28D+P04C28E+P04C28F+P04C28G,
    registracion =  case_when(P04C07 == 1 ~ "Si",
                              P04C07 == 2 ~ "No"),
    part.time.inv = case_when(horas.semana < 35 & P04C29  %in%  2:4 ~ "Part Involunt",
                              horas.semana < 35 & !(P04C29  %in% 2:4) ~ "Part Volunt",
                              horas.semana >= 35 ~ "Full Time"), 
    tiempo.determinado = case_when(
      P04C08A == 2 ~ "No",
      P04C08A == 1 ~ "Si"),
    grupos.calif =   
      case_when(
      P04C02B_1D %in% 1:3 ~ "Alta",
      P04C02B_1D %in% 4:8 ~ "Media",
      P04C02B_1D %in% 9 ~ "Baja"),
    grupos.tamanio =
      case_when(
        P04C05 %in% 1:9 ~ "Pequeño", # 1 a 9
        P04C05 %in% 10:50 ~ "Mediano", # 10 a 50
        P04C05 >51  ~ "Grande")
    ) 

#summary(guatemala.cat$horas.semana)

guate.ocupados.distrib <-  guatemala.cat  %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    ocupados = sum(FACTOR,na.rm = T),
    asalariados = sum(FACTOR[P04C06 == 2 ],na.rm = T),
    no.asalariados = sum(FACTOR[P04C06 != 2],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = P04C10,
      w = FACTOR,na.rm = T),
    promedio.ing.oc.prin.noasal=weighted.mean(
      x = P04C10[P04C06 != 2],
      w = FACTOR[P04C06 != 2],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = P04C10[P04C06 == 2 ],
      w = FACTOR[P04C06 == 2],na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.no.asal= no.asalariados/sum(no.asalariados))


guat.asalariados.tasas <- guatemala.cat %>% 
  filter(P04C06 == 2) %>% # Asalariado
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    registrados =sum(FACTOR[registracion=="Si"],na.rm = T),
    no.registrados =sum(FACTOR[registracion=="No"],na.rm = T),
    empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(FACTOR[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(FACTOR[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(FACTOR[part.time.inv=="Full Time"],na.rm = T),
    tasa.partime.asal = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp.asal = empleo.temporal/(empleo.temporal+
                                        empleo.no.temporal))

guate.resultado <- guate.ocupados.distrib %>%
  left_join(guat.asalariados.tasas )


####El Salvador####
# eph::calculate_tabulates(base = elsalvador,
#                          x = "segm",weights = "fac00",add.totals = "row")  
# eph::calculate_tabulates(base = elsalvador,
#                          x = "r421",weights = "fac00",add.totals = "row")  
table(elsalvador$r420)
table(elsalvador$r418)
table(elsalvador$actpr2012)
table(elsalvador$area)

el.salvador.cat<- elsalvador %>% 
  filter(area == 1) %>%  #Urbano
  filter(actpr2012 == 10) %>%  #Ocupados
  filter(r418 != 9,r420 == 1) %>% # Spriv s/ serv domestico
  # filter(r418 == 6:7) %>% # Asalariado
  mutate(
    periodo = edicion,
    FACTOR = fac00,
    horas.semana = r412a + r412d,
    registracion =  case_when(r419 %in% 1:6 ~ "Si",
                              r419 %in% 7:8 ~ "No"),
    part.time.inv = case_when(horas.semana < 35 & r413  %in%  2:4 ~ "Part Involunt",
                              horas.semana < 35 & !(r413  %in% 2:4) ~ "Part Volunt",
                              horas.semana >= 35 ~ "Full Time"), 
    tiempo.determinado = case_when(
      r419 == 1 ~ "No",
      r419 != 1 ~ "Si"),
    grupos.calif =   
      case_when(
        ciuo414 %in% 1:3 ~ "Alta",
        ciuo414 %in% 4:8 ~ "Media",
        ciuo414 %in% 9 ~ "Baja"),
    grupos.tamanio =
      case_when(
        r421 %in% 1:9 ~ "Pequeño", # 1 a 9
        r421 %in% 10:50 ~ "Mediano", # 10 a 50
        r421 %in% 51:997  ~ "Grande"
        )
  ) 


# unique(el.salvador.cat$part.time.inv)
# table(el.salvador.cat$r413)
#summary(guatemala.cat$horas.semana)

elsa.ocupados.distrib <-  el.salvador.cat  %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    ocupados = sum(FACTOR,na.rm = T),
    asalariados = sum(FACTOR[r418 %in%  6:7],na.rm = T),
    no.asalariados = sum(FACTOR[!(r418 %in%  6:7)],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = money,
      w = FACTOR,na.rm = T),
    promedio.ing.oc.prin.noasal=weighted.mean(
      x = money[!(r418 %in%  6:7)],
      w = FACTOR[!(r418 %in%  6:7)],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = money[r418 %in%  6:7],
      w = FACTOR[r418 %in%  6:7],na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.no.asal= no.asalariados/sum(no.asalariados))


    
elsa.asalariados.tasas <- el.salvador.cat %>% 
  filter(r418 %in%  6:7) %>% # Asalariado
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    registrados =sum(FACTOR[registracion=="Si"],na.rm = T),
    no.registrados =sum(FACTOR[registracion=="No"],na.rm = T),
    empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(FACTOR[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(FACTOR[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(FACTOR[part.time.inv=="Full Time"],na.rm = T),
    tasa.partime.asal = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp.asal = empleo.temporal/(empleo.temporal+
                                        empleo.no.temporal))

elsa.resultado <- elsa.ocupados.distrib %>%
  left_join(elsa.asalariados.tasas)
  


####Canada####
canada.cat<- canada %>% 
  filter(LFSSTAT  %in%  1:2) %>%  #Ocupados
  filter(COWMAIN != 1) %>% # Spriv 
  # filter(COWMAIN == 2) %>% # Asalariado
  mutate(
    periodo = as.double(paste0(SURVYEAR,SURVMNTH)),
    FACTOR = FINALWT,
    # registracion =  case_when(r419 %in% 1:6 ~ "Si",
    #                           r419 %in% 7:8 ~ "No"),
    registracion = NA,
    ing.mensual =  HRLYEARN*UHRSMAIN*4.25,
    part.time.inv = case_when(UHRSMAIN < 35 & WHYPT  %in%  6:7 ~ "Part Involunt",
                              UHRSMAIN < 35 & !(WHYPT  %in% 6:7) ~ "Part Volunt",
                              UHRSMAIN >= 35 ~ "Full Time"), 
    tiempo.determinado = case_when(
      PERMTEMP == 1 ~ "No",
      PERMTEMP != 1 ~ "Si"),
    grupos.calif =   
      case_when(
        NOC_10 %in% 1:3 ~ "Alta",
        NOC_10 %in% 4:8 ~ "Media",
        NOC_10 %in% 9 ~ "Baja"),
    grupos.tamanio =
      case_when(
        ESTSIZE %in% 1 ~ "Pequeño", # 1 a 20
        ESTSIZE %in% 2 ~ "Mediano", # 20 a 99
        ESTSIZE %in% 3:4  ~ "Grande"# 100 a 500 y mas 500
      )
  ) 



canada.ocupados.distrib <-  canada.cat %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    ocupados = sum(FACTOR,na.rm = T),
    asalariados = sum(FACTOR[COWMAIN == 2],na.rm = T),
    no.asalariados = sum(FACTOR[COWMAIN != 2],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = ing.mensual,
      w = FACTOR,na.rm = T),
    promedio.ing.oc.prin.noasal=weighted.mean(
      x = ing.mensual[COWMAIN != 2],
      w = FACTOR[COWMAIN != 2],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = ing.mensual[COWMAIN == 2],
      w = FACTOR[COWMAIN == 2],na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.no.asal= no.asalariados/sum(no.asalariados))



canada.asalariados.tasas <- canada.cat %>% 
  filter(COWMAIN == 2) %>% # Asalariado
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    registrados =sum(FACTOR[registracion=="Si"],na.rm = T),
    no.registrados =sum(FACTOR[registracion=="No"],na.rm = T),
    empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(FACTOR[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(FACTOR[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(FACTOR[part.time.inv=="Full Time"],na.rm = T),
    tasa.partime.asal = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp.asal = empleo.temporal/(empleo.temporal+
                                        empleo.no.temporal))

canada.resultado <- canada.ocupados.distrib %>%
  left_join(canada.asalariados.tasas)

####Union Paises####
resultados <- 
            ec.resultado %>% mutate(Pais = "Ecuador") %>% 
  bind_rows(co.resultado  %>% mutate(Pais = "Colombia")) %>% 
  bind_rows(cr.resultado  %>% mutate(Pais = "Costa Rica")) %>% 
  bind_rows(mex.resultado %>% mutate(Pais = "Mexico")) %>% 
  bind_rows(guate.resultado %>% mutate(Pais = "Guatemala")) %>% 
  bind_rows(elsa.resultado  %>% mutate(Pais = "El Salvador")) %>% 
  bind_rows(canada.resultado  %>% mutate(Pais = "Canada")) %>% 
  select(Pais,everything())

save(resultados,
     file = "Resultados/ResultadosGuido.RDATA")

