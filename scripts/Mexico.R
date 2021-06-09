library(tidyverse)
library(foreign)
########################Carga de datos############################################
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
# saveRDS(mexico,file = "Bases/mexico_T12019.RDS")

mexico<-readRDS(file = "Bases/mexico_T12019.RDS")
load("Fuentes Complementarias/crosstable_sinco2011_isco08.rda")
####################Exploracion bases############################################
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

############################Crosswalk isco#########################################
sample.isco <- function(df) {
  sample(df$cod.destination,size = 1)
}

mexico.cross <- crosstable_sinco2011_isco08 %>% 
  select(P3 = cod.origin,cod.destination) %>%
  dplyr::add_row(P3 = NA) %>% 
  group_by(P3) %>%
  tidyr::nest()

base_join  <- mexico %>% 
 left_join(mexico.cross,by = "P3")

set.seed(9999)
base_join_sample <- base_join %>%
  mutate(isco.08 = purrr::map(data, sample.isco))  %>%
  select(-data) %>% # Elimino la columna creada para el sorteo
  mutate(isco.08 = as.character(isco.08))
#######################Categorias#############################################
mex.cat <- base_join_sample %>% 
  rename_all(.funs = tolower) %>% 
  filter(t_loc != 4) %>%  #Localidades mayores a 2500
  filter(clase2 == 1) %>%  #Ocupados
  filter((tue1  %in%  c(1,4))   |
           (tue1 == 2 & tue2 == 3)|
           (tue1 == 3 & tue3 == 5)) %>% # Spriv s/ serv domestico
  filter(pos_ocu %in%  c(1,3))%>% # Asalariado o TCP 
  mutate(
    periodo = per,
    FACTOR  = fac,
    grupos.tamanio = 
      case_when(
        emple7c %in% 1:3 ~ "Pequeño", # 1 a 10
        emple7c %in% 4:5 ~ "Mediano", # 11 a 50
        emple7c %in% 6:5 ~ "Grande"), # + 51
    grupos.tamanio = 
      case_when(
        pos_ocu %in% 3 ~ "Pequeño",
        TRUE ~  grupos.tamanio),
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
    grupos.calif =   
      case_when(
        str_sub(isco.08,1,1) %in% 1:3 ~ "Alta",
        str_sub(isco.08,1,1) %in% 4:8 ~ "Media",
        str_sub(isco.08,1,1) %in% 9 ~ "Baja")
  ) 


################################Resultados#############################################

mex.ocupados.distrib <-  mex.cat  %>% 
  mutate(p6b2 = as.numeric(p6b2)) %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    ocupados = sum(FACTOR,na.rm = T),
    asalariados = sum(FACTOR[pos_ocu == 1],na.rm = T),
    tcp = sum(FACTOR[pos_ocu != 1],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = p6b2[p6b2 != 999999],
      w = FACTOR[p6b2 != 999999],na.rm = T),
    promedio.ing.oc.prin.tcp=weighted.mean(
      x = p6b2[pos_ocu!= 1 & p6b2!= 999999],
      w = FACTOR[pos_ocu!= 1 & p6b2!= 999999],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = p6b2[pos_ocu== 1 & p6b2!= 999999],
      w = FACTOR[pos_ocu== 1 & p6b2!= 999999],na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.tcp= tcp/sum(tcp))

mex.ocupados.distrib.agregado <-  mex.cat  %>% 
  mutate(p6b2 = as.numeric(p6b2)) %>% 
#  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(periodo) %>% 
  summarise(
    ocupados = sum(FACTOR,na.rm = T),
    asalariados = sum(FACTOR[pos_ocu == 1],na.rm = T),
    tcp = sum(FACTOR[pos_ocu != 1],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = p6b2[p6b2 != 999999],
      w = FACTOR[p6b2 != 999999],na.rm = T),
    promedio.ing.oc.prin.tcp=weighted.mean(
      x = p6b2[pos_ocu!= 1 & p6b2!= 999999],
      w = FACTOR[pos_ocu!= 1 & p6b2!= 999999],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = p6b2[pos_ocu== 1 & p6b2!= 999999],
      w = FACTOR[pos_ocu== 1 & p6b2!= 999999],na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.tcp= tcp/sum(tcp))


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
    tasa.partime = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp = empleo.temporal/(empleo.temporal+
                                        empleo.no.temporal)) %>% 
  ungroup() %>% 
  rename_with(~str_c(.,".asal"), .cols = 4:ncol(.))

mex.tcp.tasas <- mex.cat %>% 
  filter(pos_ocu == 3) %>% # TCP
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
                                   empleo.no.temporal)) %>% 
  ungroup() %>% 
  rename_with(~str_c(.,".tcp"), .cols = 4:ncol(.))



mex.asalariados.tasas.agregado <- mex.cat %>% 
  filter(pos_ocu == 1) %>% # Asalariado
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

mex.tcp.tasas.agregado <- mex.cat %>% 
  filter(pos_ocu == 3) %>% # TCP
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

mex.resultado <- mex.ocupados.distrib %>%
  left_join(mex.asalariados.tasas) %>% 
  left_join(mex.tcp.tasas) %>% 
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

mex.resultado.agregado <- mex.ocupados.distrib.agregado %>% 
  left_join(mex.asalariados.tasas.agregado) %>% 
  left_join(mex.tcp.tasas.agregado) %>% 
  mutate(Pais = "Mexico",
         tamanio.calif = "Total")
################################Exportacion################################

saveRDS(mex.resultado.agregado,file = "Resultados/Mexico_agregado.RDS")  
saveRDS(mex.resultado,file = "Resultados/Mexico.RDS")  
