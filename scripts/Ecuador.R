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
# for(base in rutas.base.2019$ruta){
#   
#   
#   ecuador<- read.spss(file = paste0('../bases/Ecuador/',base),
#                      reencode = "UTF-8",use.value.labels = F,
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
ec.categ <- ecuador2019 %>% 
  mutate(uno = 1) %>% 
  filter(area == 1) %>% #Urbanos
  filter(condact %in% 2:7) %>% #Ocupados
  filter(p42 %in% c(2,4,5,6)) %>% # CP, PaAtron y SPriv sin S/D
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
        p47b > 50 | p47a == 2 ~ "Grande")#  + de  100
  ) 

# ver <- ec.categ %>% 
#   group_by(part.time.inv,grupos.tamanio,grupos.calif) %>% 
#   summarise(casos = n())
# 
# table(ec.categ$ISCO.1.digit)

ec.tasa.asalariz <- ec.categ %>%
  group_by(periodo) %>% 
  summarise(total.ocupados = sum(fexp,na.rm = T)/4,
            asalariados = sum(fexp[p42 == 2])/4,
            tasa.asalarizacion = asalariados/total.ocupados) 


ec.ocupados.distrib  <- ec.categ %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    ocupados = sum(fexp,na.rm = T)/4,
    ocupados_muestral = n(),
    asalariados = sum(fexp[p42 == 2],na.rm = T)/4,
    asalariados_muestral = sum(uno[p42 == 2],na.rm = T),
    no.asalariados = sum(fexp[p42 != 2],na.rm = T)/4,
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = ingrl[!(ingrl %in% c(0,999999))],
      w = fexp[!(ingrl %in% c(0,999999))],na.rm = T),
    promedio.ing.oc.prin.noasal=weighted.mean(
      x = ingrl[p42!= 2 & (!(ingrl %in% c(0,999999)))],
      w = fexp[p42!= 2 & (!(ingrl %in% c(0,999999)))],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = p66[p42== 2 & (!(ingrl %in% c(0,999999)))],
      w = fexp[p42== 2 & (!(ingrl %in% c(0,999999)))],na.rm = T)
  ) %>% 
  group_by() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.no.asal= no.asalariados/sum(no.asalariados))

ec.ocupados.distrib.agregado  <- ec.categ %>% 
  #filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(periodo) %>% 
  summarise(
    ocupados = sum(fexp,na.rm = T)/4,
    ocupados_muestral = n(),
    asalariados = sum(fexp[p42 == 2],na.rm = T)/4,
    asalariados_muestral = sum(uno[p42 == 2],na.rm = T),
    no.asalariados = sum(fexp[p42 != 2],na.rm = T)/4,
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = ingrl[!(ingrl %in% c(0,999999))],
      w = fexp[!(ingrl %in% c(0,999999))],na.rm = T),
    promedio.ing.oc.prin.noasal=weighted.mean(
      x = ingrl[p42!= 2 & (!(ingrl %in% c(0,999999)))],
      w = fexp[p42!= 2 & (!(ingrl %in% c(0,999999)))],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = p66[p42== 2 & (!(ingrl %in% c(0,999999)))],
      w = fexp[p42== 2 & (!(ingrl %in% c(0,999999)))],na.rm = T)
  ) %>% 
  group_by() %>% 
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
    seguridad.social.si = sum(fexp[seguridad.social=="Si"],na.rm = T)/4,
    seguridad.social.no = sum(fexp[seguridad.social=="No"],na.rm = T)/4,
    registrados =sum(fexp[registracion=="Si"],na.rm = T)/4,
    no.registrados =sum(fexp[registracion=="No"],na.rm = T)/4,
    # empleo.temporal =sum(FACTOR[tiempo.determinado=="Si"],na.rm = T),
    # empleo.no.temporal =sum(FACTOR[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(fexp[part.time.inv=="Part Involunt"],na.rm = T)/4,
    part.volunt = sum(fexp[part.time.inv=="Part Volunt"],na.rm = T)/4,
    full.time = sum(fexp[part.time.inv=="Full Time"],na.rm = T)/4,
    tasa.partime.asal = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no))

ec.asalariados.tasas.agregado <- ec.categ %>% 
  filter(p42 == 2) %>% # Asalariad
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
    tasa.partime.asal = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no))

ec.resultado <- ec.ocupados.distrib %>% 
  left_join(ec.asalariados.tasas) %>% 
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
  mutate(Pais = "Ecuador",
         tamanio.calif = "Total")



saveRDS(ec.resultado,file = "Resultados/Ecuador.RDS")  
saveRDS(ec.resultado.agregado,file = "Resultados/Ecuador_agregado.RDS")  




