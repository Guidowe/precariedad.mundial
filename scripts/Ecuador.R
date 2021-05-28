library(tidyverse)
library(foreign)
library(stringr)
# ecuador <- read.spss("../bases/Ecuador/enemdu_persona_201912.sav",
#                      reencode = "UTF-8",use.value.labels = F,
#                      to.data.frame = T)
# saveRDS(ecuador,file = "Bases/ecuador_122019.RDS")
ecuador<-readRDS(file = "Bases/ecuador_122019.RDS")


archivos<- list.files("../bases/Ecuador/")

rutas <- data.frame(
  ruta = list.files("../bases/Ecuador/",recursive = T))

rutas.base.2019 <- rutas %>% 
  filter(str_detect(ruta,pattern = "sav"))

ecuador2019 <- data.frame()
for(base in rutas.base.2019$ruta){
  
  
  ecuador<- read.spss(file = paste0('../bases/Ecuador/',base),
                     reencode = "UTF-8",use.value.labels = F,
                                           to.data.frame = T)
  
  
  



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

ec.categ <- ecuador %>% 
  filter(area == 1) %>% #Urbanos
  filter(condact %in% 2:7) %>% #Ocupados
  filter(p42 %in% c(2,4,5,6)) %>% # CP, PaAtron y SPriv sin S/D
  #   filter(p42 == 2) %>% # Asalariad
  mutate(
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
    seguridad.social.si = sum(fexp[seguridad.social=="Si"],na.rm = T),
    seguridad.social.no = sum(fexp[seguridad.social=="No"],na.rm = T),
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

ecuador2019 <- bind_rows(ecuador2019,ec.resultado)

}

ecuador2019 <- ecuador2019 %>%
  group_by(grupos.calif,grupos.tamanio) %>% 
  summarise(periodo = 2019, 
            across(.cols = 4:ncol(.)-2,.fns = mean))  %>% 
  ungroup() %>% 
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

saveRDS(ecuador2019,file = "Resultados/Ecuador.RDS")  




