library(tidyverse)
library(foreign)
# costarica<- read.spss('../bases/Costa Rica/ENAHO 2019.sav',
#                       reencode = "UTF-8",use.value.labels = F,
#                       to.data.frame = T) %>% 
#   mutate(periodo = 2019)
#saveRDS(costarica,file = "Bases/costarica_2019.RDS")
costarica<-readRDS(file = "Bases/costarica_2019.RDS")

####Costa Rica####
 table(costarica$Estabili)
# table(costarica$C10)
# table(costarica$CondAct)
# table(costarica$PosiEmpPri)
# table(costarica$E9A)
# table(costarica$C2A1)
# table(costarica$E10A)
# table(costarica$Estabili)
# weighted.mean(costarica$C2A1,costarica$FACTOR,na.rm = T) #horas
##Base homog ####
base_homog <- costarica %>% 
  filter(ZONA == 1) %>% #Urbano
  filter(CondAct %in% 1) %>% #Ocupados
#  filter(SecInsPri %in% 3) %>% # Spriv
#  filter(PosiEmpPri %in% c(12,22)) %>% # Asalariados y TCP sin  serv domestico
  #    filter(PosiEmpPri == 12) %>% # Asalariad
  mutate(
    PERIODO = periodo,
    CATOCUP = case_when(PosiEmpPri %in% 11:12~ "Asalariados",
                         PosiEmpPri %in% 22~ "Cuenta Propia",
                         PosiEmpPri %in% 21~ "Patron",
                         TRUE ~ "Resto"),
    SECTOR = case_when(SecInsPri %in% 1:2~ "Pub",
                       SecInsPri %in% 3 & PosiEmpPri != 11~ "Priv",
                       PosiEmpPri == 11 ~ "SD"),
    PRECASALUD = case_when(IPM_S1 == 0 ~ 0,
                           IPM_S1 == 1 ~ 1),
    COND = "Ocupado",
    WEIGHT  = FACTOR,
    PAIS = "Costa Rica",
    ANO = 2019,
    SEXO = case_when(A4 == 1 ~ "Varon",
                     A4 == 2 ~ "Mujer"),
    EDAD = A5,
    
    PRECASEG =  case_when(E10A == 1 ~ 0,
                                  E10A == 2 ~ 1),
    PRECAREG =  case_when(E10A == 1 ~ 0,
                          E10A == 2 ~ 1),
    part.time.inv = case_when(C2A1 < 35 & C3 == 1 ~ "Part Involunt",
                              C2A1 < 35 & C3 == 2 ~ "Part Volunt",
                              C2A1 >= 35 ~ "Full Time"), 
    PRECAPT = case_when(part.time.inv == "Part Involunt"~1,
                        part.time.inv %in%  c("Part Volunt","Full Time")~0),
    PRECATEMP = case_when(
      Estabili == 10 ~ 0,
      Estabili != 10 ~ 1),
    CALIF =   case_when(
      OcupEmpPri %in% 1:3 ~ "Alta",
      OcupEmpPri %in% 4:8 ~ "Media",
      OcupEmpPri %in% 9 ~ "Baja"),
    TAMA =
      case_when(
        C10 %in% 1:9 ~ "Pequeño", # 1 a 9
        C10 %in% 10:11 ~ "Mediano", # 10 a 30
        C10 %in% 12 ~ "Mediano", #  30 a 100
        C10 %in% 13 ~ "Grande"), #  + de  100
    TAMA =
      case_when( PosiEmpPri == 22 ~ "Pequeño",
                 TRUE ~ TAMA),
    ING = ipnt,
    EDUC = case_when(NivInst %in% c(1:3,5) ~ "Primaria",
                     NivInst %in% c(4,6) ~ "Secundaria",
                     NivInst %in% 7:8 ~ "Terciaria")
    ) 

variables<- c("PAIS","ANO","PERIODO","WEIGHT","SEXO","EDAD",
              "CATOCUP","COND","SECTOR","PRECAPT","EDUC",
              "PRECAREG","PRECATEMP","PRECASALUD","PRECASEG","TAMA","CALIF","ING") 

base_homog_final <- base_homog %>% 
  select(all_of(variables))

saveRDS(base_homog_final,file = "bases_homog/costa_rica.rds")

## Base trabajo ####

cr.categ <- costarica %>% 
  filter(ZONA == 1) %>% #Urbano
  filter(CondAct %in% 1) %>% #Ocupados
  filter(SecInsPri %in% 3) %>% # Spriv
  filter(PosiEmpPri %in% c(12,22)) %>% # Asalariados y TCP sin  serv domestico
  #    filter(PosiEmpPri == 12) %>% # Asalariad
  mutate(
    seguridad.social =  case_when(E10A == 1 ~ "Si",
                                  E10A == 2 ~ "No"),
    registracion =  case_when(E10A == 1 ~ "Si",
                              E10A == 2 ~ "No"),
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
        C10 %in% 13 ~ "Grande"), #  + de  100
    grupos.tamanio =
      case_when( PosiEmpPri == 22 ~ "Pequeño",
                 TRUE ~ grupos.tamanio))

cr.ocupados.distrib  <- cr.categ %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    total.casos = n(),
    total.asalariados = sum(PosiEmpPri == 12),
    ocupados = sum(FACTOR,na.rm = T),
    asalariados = sum(FACTOR[PosiEmpPri == 12],na.rm = T),
    tcp = sum(FACTOR[PosiEmpPri != 12],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = ipnt,
      w = FACTOR,na.rm = T),
    promedio.ing.oc.prin.tcp=weighted.mean(
      x = ipnt[PosiEmpPri!= 12],
      w = FACTOR[PosiEmpPri!= 12],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = ipnt[PosiEmpPri== 12 ],
      w = FACTOR[PosiEmpPri== 12 ],na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.tcp= tcp/sum(tcp))


cr.ocupados.distrib.agregado  <- cr.categ %>% 
  group_by(periodo) %>% 
  summarise(
    ocupados = sum(FACTOR,na.rm = T),
    asalariados = sum(FACTOR[PosiEmpPri == 12],na.rm = T),
    tcp = sum(FACTOR[PosiEmpPri != 12],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = ipnt,
      w = FACTOR,na.rm = T),
    promedio.ing.oc.prin.tcp=weighted.mean(
      x = ipnt[PosiEmpPri!= 12],
      w = FACTOR[PosiEmpPri!= 12],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = ipnt[PosiEmpPri== 12 ],
      w = FACTOR[PosiEmpPri== 12 ],na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.tcp= tcp/sum(tcp))


cr.asalariados.tasas <- cr.categ %>% 
  filter(PosiEmpPri == 12) %>% # Asalariad
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    seguridad.social.si = sum(FACTOR[seguridad.social=="Si"],na.rm = T),
    seguridad.social.no = sum(FACTOR[seguridad.social=="No"],na.rm = T),
    registrados =sum(FACTOR[registracion=="Si"],na.rm = T),
    no.registrados =sum(FACTOR[registracion=="No"],na.rm = T),
    empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(FACTOR[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(FACTOR[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(FACTOR[part.time.inv=="Full Time"],na.rm = T),
    tasa.partime = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp = empleo.temporal/(empleo.temporal+
                                        empleo.no.temporal))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".asal"), .cols = 4:ncol(.))


cr.tcp.tasas <- cr.categ %>% 
  filter(PosiEmpPri == 22) %>% # TCP
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    seguridad.social.si = sum(FACTOR[seguridad.social=="Si"],na.rm = T),
    seguridad.social.no = sum(FACTOR[seguridad.social=="No"],na.rm = T),
    registrados =sum(FACTOR[registracion=="Si"],na.rm = T),
    no.registrados =sum(FACTOR[registracion=="No"],na.rm = T),
    empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(FACTOR[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(FACTOR[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(FACTOR[part.time.inv=="Full Time"],na.rm = T),
    tasa.partime = part.involun/(part.involun+
                                   part.volunt+
                                   full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp = empleo.temporal/(empleo.temporal+
                                   empleo.no.temporal))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".tcp"), .cols = 4:ncol(.))

cr.asalariados.tasas.agregado <- cr.categ %>% 
  filter(PosiEmpPri == 12) %>% # Asalariad
#  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(periodo) %>% 
  summarise(
    seguridad.social.si = sum(FACTOR[seguridad.social=="Si"],na.rm = T),
    seguridad.social.no = sum(FACTOR[seguridad.social=="No"],na.rm = T),
    registrados =sum(FACTOR[registracion=="Si"],na.rm = T),
    no.registrados =sum(FACTOR[registracion=="No"],na.rm = T),
    empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(FACTOR[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(FACTOR[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(FACTOR[part.time.inv=="Full Time"],na.rm = T),
    tasa.partime = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp = empleo.temporal/(empleo.temporal+
                                        empleo.no.temporal))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".asal"), .cols = 2:ncol(.))


cr.tcp.tasas.agregado <- cr.categ %>% 
  filter(PosiEmpPri == 22) %>% # TCP
  #  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(periodo) %>% 
  summarise(
    seguridad.social.si = sum(FACTOR[seguridad.social=="Si"],na.rm = T),
    seguridad.social.no = sum(FACTOR[seguridad.social=="No"],na.rm = T),
    registrados =sum(FACTOR[registracion=="Si"],na.rm = T),
    no.registrados =sum(FACTOR[registracion=="No"],na.rm = T),
    empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(FACTOR[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(FACTOR[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(FACTOR[part.time.inv=="Full Time"],na.rm = T),
    tasa.partime = part.involun/(part.involun+
                                   part.volunt+
                                   full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp = empleo.temporal/(empleo.temporal+
                                   empleo.no.temporal))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".tcp"), .cols = 2:ncol(.))


cr.resultado <- cr.ocupados.distrib %>%
  left_join(cr.asalariados.tasas)%>% 
  left_join(cr.tcp.tasas)%>% 
  mutate(Pais = "Costa Rica",
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

cr.resultado.agregado <- cr.ocupados.distrib.agregado %>%
  left_join(cr.asalariados.tasas.agregado)%>% 
  left_join(cr.tcp.tasas.agregado)%>% 
  mutate(Pais = "Costa Rica")

saveRDS(cr.resultado,file = "Resultados/Costa Rica.RDS")  
saveRDS(cr.resultado.agregado,file = "Resultados/Costa Rica_agregado.RDS")  

