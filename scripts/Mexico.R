library(tidyverse)
library(foreign)
# mexico.basico <- read_csv("../bases/Mexico/1t2019/sdemt119.csv")
# mexico.ampliado <- read_csv("../bases/Mexico/1t2019/coe1t119.csv")
# mexico.ampliado2 <- read_csv("../bases/Mexico/1t2019/coe2t119.csv")
# mex.variables.ampliado <- mexico.ampliado %>%
#   mutate(EDA = as.character(EDA)) %>%
#   select(1:16,P3,P3M4,P3J)
# mex.variables.ampliado2 <- mexico.ampliado2 %>%
#   mutate(EDA = as.character(EDA)) %>%
#   select(1:16,P6B2)
# mexico <- mexico.basico %>%
#    left_join(mex.variables.ampliado) %>%
#    left_join(mex.variables.ampliado2)
# saveRDS(mexico,file = "Bases/mexico_T31019.RDS")

mexico<-readRDS(file = "Bases/mexico_T32019.RDS")

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
#table(mexico$P3M4,useNA = "always")

#Proceso#
mex.cat <- mexico %>% 
  rename_all(.funs = tolower) %>% 
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
    seguridad.social =  case_when(p3m4 %in% 4 ~ "Si",
                              TRUE ~ "No"),
    registracion =  case_when(p3j %in% 1 ~ "Si",
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

mex.resultado <- mex.ocupados.distrib %>%
  left_join(mex.asalariados.tasas) %>% 
  mutate(Pais = "Mexico",
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

saveRDS(mex.resultado,file = "Resultados/Mexico.RDS")  
