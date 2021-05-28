library(tidyverse)
library(foreign)
# costarica<- read.spss('../bases/Costa Rica/ENAHO 2019.sav',
#                       reencode = "UTF-8",use.value.labels = F,
#                       to.data.frame = T) %>% 
#   mutate(periodo = 2019)
#saveRDS(costarica,file = "Bases/costarica_2019.RDS")
costarica<-readRDS(file = "Bases/costarica_2019.RDS")

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


cr.ocupados.distrib.agregado  <- cr.categ %>% 
  group_by(periodo) %>% 
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
    seguridad.social.si = sum(FACTOR[seguridad.social=="Si"],na.rm = T),
    seguridad.social.no = sum(FACTOR[seguridad.social=="No"],na.rm = T),
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
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp.asal = empleo.temporal/(empleo.temporal+
                                        empleo.no.temporal))

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
    tasa.partime.asal = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp.asal = empleo.temporal/(empleo.temporal+
                                        empleo.no.temporal))

cr.resultado <- cr.ocupados.distrib %>%
  left_join(cr.asalariados.tasas)%>% 
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
  mutate(Pais = "Costa Rica")

saveRDS(cr.resultado,file = "Resultados/Costa Rica.RDS")  
saveRDS(cr.resultado.agregado,file = "Resultados/Costa Rica_agregado.RDS")  

