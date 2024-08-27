library(tidyverse)
library(foreign)
library(stringr)
#############Lectura y pool de bases#########
# archivos<- list.files("../bases/Ecuador/")
# 
# rutas <- data.frame(
#   ruta = list.files("../bases/Ecuador/",recursive = T))
# 
# rutas.base.2019 <- rutas %>%
#   filter(str_detect(ruta,pattern = "sav"))
# 
# ecuador2019 <- data.frame()
# 
# for(base in rutas.base.2019$ruta[1]){
# 
# 
#   ecuador<- read.spss(file = paste0('../bases/Ecuador/',base),
#                      reencode = "UTF-8",use.value.labels = T,
#                                            to.data.frame = T) %>%
#     mutate(ciudad = as.character(ciudad),
#            area = as.character(area),
#            dominio = as.character(dominio))
# 
# ecuador2019 <-   bind_rows(ecuador2019,ecuador)
# 
# 
# }

#saveRDS(ecuador2019,"Bases/ecuador_2019.RDS")
ecuador2019 <- readRDS("Bases/ecuador_2019.RDS")
####Ecuador####
##Miro variables##
# prueba.salario <- ecuador %>%
#   filter(p42 == 2,ingrl!= 0,ingrl!= 999999)
# 
# prueba.salario %>%
#   ggplot(aes(x = ingrl)) +
#   geom_histogram()
# salarios<- data.frame(table(prueba.salario$ingrl))
 table(ecuador$p10a)
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
ver <- ecuador2019 %>%
  filter(condact %in% 2:7,area == 1) %>% #Ocupados
 # filter(p42 %in% c(2,4,5,6)) %>%
  group_by(p42) %>%
  summarise(casos = n())

#Base homogenea ####
table(ecuador2019$periodo)
base_homog <- ecuador2019 %>% 
  mutate(uno = 1) %>% 
  filter(area == 1) %>% #Urbanos
  filter(condact %in% 2:7) %>% #Ocupados
 # filter(p42 %in% c(2,3,4,6)) %>% # CP, y asalariados privados (incluye jornalero y terciarizado)
  #   filter(p42 == 2) %>% # Asalariad
  mutate(
    PAIS = "Ecuador",
    COND = "Ocupado",
    ANO = as.integer(str_sub(periodo,1,4)),
    SEXO = case_when(p02 == 1 ~ "Varon",
                     p02 == 2 ~ "Mujer"),
    EDAD = p03,

    SECTOR = case_when(p42  == 1 ~"Pub",
                         p42  %in%  c(2:6) ~"Priv",
                         p42  == 10 ~"SD"),
    CATOCUP = case_when(p42 %in% c(1,2,3,4,9,10) ~"Asalariados",
                        p42 == 6 ~"Cuenta propia",
                        p42 == 5 ~"Patron",
                        TRUE ~"Resto"),
    PRECASEG =  case_when(p44f == 1 ~ 0,# "Recibe seguo social",
                          p44f == 2 ~ 1),
    PRECAREG = NA,
    PRECASALUD = NA,
    registracion =  NA,
    part.time.inv = case_when(p24 < 35 & p27%in% 1:3 ~ "Part Involunt",
                              p24 < 35 & p27 %in% 4 ~ "Part Volunt",
                              p24 >= 35 ~ "Full Time"), 
    PRECAPT = case_when(part.time.inv == "Part Involunt"~1,
                        part.time.inv %in%  c("Part Volunt","Full Time")~0),
    PRECATEMP = NA,
    # tiempo.determinado = case_when(
    #   Estabili == 10 ~ "No",
    #   Estabili != 10 ~ "Si"),
    ISCO.1.digit = str_sub(p41,1,1), 
    CALIF =   case_when(
      ISCO.1.digit %in% 1:3 ~ "Alta",
      ISCO.1.digit %in% 4:8 ~ "Media",
      ISCO.1.digit %in% 9 ~ "Baja"),
    TAMA =
      case_when(
        p47b %in% 1:9 ~ "Pequeño", # 1 a 9
        p47b %in% 10:50 ~ "Mediano", # 10 a 50
        p47b > 50 | p47a == 2 ~ "Grande"),#  + de  100
    TAMA =case_when(
      p42 == 6   ~ "Pequeño",
      TRUE ~ TAMA),
    ING = case_when(ingrl %in% c(0,999999) ~ NA,
                TRUE ~ ingrl),
    EDUC = case_when(p10a %in% 1:5 ~ "Primaria",
                     p10a %in% 6:7 ~ "Secundaria",
                     p10a %in% 8:10 ~ "Terciaria"
                     ),
  ) %>% 
  rename(
    WEIGHT = fexp,
    PERIODO = periodo,
    ) 

variables<- c("PAIS","ANO","PERIODO","WEIGHT","SEXO","EDAD",
              "CATOCUP","COND","SECTOR","PRECAPT","EDUC",
              "PRECAREG","PRECATEMP","PRECASALUD","PRECASEG","TAMA","CALIF","ING") 
base_homog_final <- base_homog %>% 
  select(all_of(variables))

saveRDS(base_homog_final,file = "bases_homog/ecuador.rds")
##Base Indicadores####
ec.categ <- ecuador2019 %>% 
  mutate(uno = 1) %>% 
  filter(area == 1) %>% #Urbanos
  filter(condact %in% 2:7) %>% #Ocupados
  filter(p42 %in% c(2,3,4,6)) %>% # CP, y asalariados privados (incluye jornalero y terciarizado)
  #   filter(p42 == 2) %>% # Asalariad
  mutate(
    periodo = str_sub(periodo,1,4),
    seguridad.social =  case_when(p44f == 1 ~ "Si",
                                  p44f == 2 ~ "No"),
    registracion =  NA,
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
        p47b > 50 | p47a == 2 ~ "Grande"),#  + de  100
    grupos.tamanio =
      case_when(p42 == 6   ~ "Pequeño",
                TRUE ~ grupos.tamanio)) 


# ver <- ec.categ %>% 
#   group_by(part.time.inv,grupos.tamanio,grupos.calif) %>% 
#   summarise(casos = n())
# 
# table(ec.categ$ISCO.1.digit)

# ec.tasa.asalariz <- ec.categ %>%
#   group_by(periodo) %>% 
#   summarise(total.ocupados = sum(fexp,na.rm = T)/4,
#             asalariados = sum(fexp[p42 == 2])/4,
#             tasa.asalarizacion = asalariados/total.ocupados) 
# 

ec.ocupados.distrib  <- ec.categ %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    total.casos = n(),
    total.asalariados = sum(p42 != 6),
    ocupados = sum(fexp,na.rm = T)/4,
    asalariados = sum(fexp[p42 != 6],na.rm = T)/4,
    asalariados_muestral = sum(uno[p42 != 6],na.rm = T),
    tcp = sum(fexp[p42 == 6],na.rm = T)/4,
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = ingrl[!(ingrl %in% c(0,999999))],
      w = fexp[!(ingrl %in% c(0,999999))],na.rm = T),
    promedio.ing.oc.prin.tcp=weighted.mean(
      x = ingrl[p42 == 6 & (!(ingrl %in% c(0,999999)))],
      w = fexp[p42 == 6 & (!(ingrl %in% c(0,999999)))],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = p66[p42!= 6 & (!(ingrl %in% c(0,999999)))],
      w = fexp[p42!= 6 & (!(ingrl %in% c(0,999999)))],na.rm = T)
  ) %>% 
  group_by() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.tcp= tcp/sum(tcp))

ec.ocupados.distrib.agregado  <- ec.categ %>% 
  #filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(periodo) %>% 
  summarise(
    ocupados = sum(fexp,na.rm = T)/4,
    ocupados_muestral = n(),
    asalariados = sum(fexp[p42 != 6],na.rm = T)/4,
    asalariados_muestral = sum(uno[p42 != 6],na.rm = T),
    tcp = sum(fexp[p42 == 6],na.rm = T)/4,
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = ingrl[!(ingrl %in% c(0,999999))],
      w = fexp[!(ingrl %in% c(0,999999))],na.rm = T),
    promedio.ing.oc.prin.tcp=weighted.mean(
      x = ingrl[p42 == 6 & (!(ingrl %in% c(0,999999)))],
      w = fexp[p42 == 6 & (!(ingrl %in% c(0,999999)))],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = p66[p42!= 6 & (!(ingrl %in% c(0,999999)))],
      w = fexp[p42!= 6 & (!(ingrl %in% c(0,999999)))],na.rm = T)
  ) %>% 
  group_by() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.tcp= tcp/sum(tcp))

# ec.asalariados.salario <- cr.prueba %>% 
#   filter(PosiEmpPri == 12) %>% # Asalariad
#   filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
#   filter(E12A > 2, E12A!=99999999) %>% 
#   filter(E12B == 1) %>% #salario bruto
#   group_by(grupos.calif,grupos.tamanio) %>% 
#   summarise(
#     sal.prom = weighted.mean(E12A,FACTOR,na.rm = T))

ec.asalariados.tasas <- ec.categ %>% 
  filter(p42 != 6) %>% # Asalariad
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    seguridad.social.si = sum(fexp[seguridad.social=="Si"],na.rm = T)/4,
    seguridad.social.no = sum(fexp[seguridad.social=="No"],na.rm = T)/4,
    registrados =sum(fexp[registracion=="Si"],na.rm = T)/4,
    no.registrados =sum(fexp[registracion=="No"],na.rm = T)/4,
    # empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    # empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(fexp[part.time.inv=="Part Involunt"],na.rm = T)/4,
    part.volunt = sum(fexp[part.time.inv=="Part Volunt"],na.rm = T)/4,
    full.time = sum(fexp[part.time.inv=="Full Time"],na.rm = T)/4,
    tasa.partime = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".asal"), .cols = 4:ncol(.))

ec.tcp.tasas <- ec.categ %>% 
  filter(p42 == 6) %>% # TCP
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    seguridad.social.si = sum(fexp[seguridad.social=="Si"],na.rm = T)/4,
    seguridad.social.no = sum(fexp[seguridad.social=="No"],na.rm = T)/4,
    registrados =sum(fexp[registracion=="Si"],na.rm = T)/4,
    no.registrados =sum(fexp[registracion=="No"],na.rm = T)/4,
    # empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    # empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(fexp[part.time.inv=="Part Involunt"],na.rm = T)/4,
    part.volunt = sum(fexp[part.time.inv=="Part Volunt"],na.rm = T)/4,
    full.time = sum(fexp[part.time.inv=="Full Time"],na.rm = T)/4,
    tasa.partime = part.involun/(part.involun+
                                   part.volunt+
                                   full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".tcp"), .cols = 4:ncol(.))



ec.asalariados.tasas.agregado <- ec.categ %>% 
  filter(p42 != 6) %>% # Asalariad
  #filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(periodo) %>% 
  summarise(
    seguridad.social.si = sum(fexp[seguridad.social=="Si"],na.rm = T)/4,
    seguridad.social.no = sum(fexp[seguridad.social=="No"],na.rm = T)/4,
    registrados =sum(fexp[registracion=="Si"],na.rm = T)/4,
    no.registrados =sum(fexp[registracion=="No"],na.rm = T)/4,
    # empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    # empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(fexp[part.time.inv=="Part Involunt"],na.rm = T)/4,
    part.volunt = sum(fexp[part.time.inv=="Part Volunt"],na.rm = T)/4,
    full.time = sum(fexp[part.time.inv=="Full Time"],na.rm = T)/4,
    tasa.partime = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".asal"), .cols = 2:ncol(.))

ec.tcp.tasas.agregado <- ec.categ %>% 
  filter(p42 == 6) %>% # TCP
  #filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(periodo) %>% 
  summarise(
    seguridad.social.si = sum(fexp[seguridad.social=="Si"],na.rm = T)/4,
    seguridad.social.no = sum(fexp[seguridad.social=="No"],na.rm = T)/4,
    registrados =sum(fexp[registracion=="Si"],na.rm = T)/4,
    no.registrados =sum(fexp[registracion=="No"],na.rm = T)/4,
    # empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    # empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(fexp[part.time.inv=="Part Involunt"],na.rm = T)/4,
    part.volunt = sum(fexp[part.time.inv=="Part Volunt"],na.rm = T)/4,
    full.time = sum(fexp[part.time.inv=="Full Time"],na.rm = T)/4,
    tasa.partime = part.involun/(part.involun+
                                   part.volunt+
                                   full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".tcp"), .cols = 2:ncol(.))


ec.resultado <- ec.ocupados.distrib %>% 
  left_join(ec.asalariados.tasas) %>% 
  left_join(ec.tcp.tasas) %>% 
  mutate(Pais = "Ecuador",
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

ec.resultado.agregado <- ec.ocupados.distrib.agregado %>% 
  left_join(ec.asalariados.tasas.agregado) %>% 
  left_join(ec.tcp.tasas.agregado) %>% 
  mutate(Pais = "Ecuador",
         tamanio.calif = "Total")



saveRDS(ec.resultado,file = "Resultados/Ecuador.RDS")  
saveRDS(ec.resultado.agregado,file = "Resultados/Ecuador_agregado.RDS")  




