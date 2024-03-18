library(tidyverse)
library(foreign)
library(RVerbalExpressions)

# data.2019 <- data.frame(
#   ruta = list.files("../bases/Colombia/2019/",recursive = T))
# 
# hasta.el.punto<- rx() %>% 
#   rx_anything_but(".")
# 
# data.2019.cabecera.ocup <- data.2019 %>% 
#   filter(str_detect(ruta,"Cabecera - Ocupados")) %>% 
#   mutate(mes = str_extract(string = ruta,pattern = hasta.el.punto))
# 
# colombia2019 <- data.frame()
# for(base in data.2019.cabecera.ocup$ruta){
#   
# mes <- str_extract(string = base,pattern = hasta.el.punto)
# 
# colombia<- read.spss(file = paste0('../bases/Colombia/2019/',base),
#                      reencode = "UTF-8",
#                      use.value.labels = F,
#                      to.data.frame = T) %>% 
#   mutate(periodo = paste0(mes, " - ","2019"))
# 
# 
# colombia2019 <- bind_rows(colombia2019,colombia)
# }
# 
# 
# 
# saveRDS(colombia2019,file = "Bases/colombia_2019.RDS")
colombia<-readRDS(file = "Bases/colombia_2019.RDS")
####Colombia####
##Miro variables##
# table(colombia$CLASE)
table(colombia$periodo)
# table(colombia$P6460)
# table(colombia$P6440)
# table(colombia$P6450)
# table(colombia$OFICIO)

##Proceso##
co.categ <- colombia %>% 
  rename(fexp = fex_c_2011) %>% 
  filter(CLASE == 1) %>% # Ubana
  filter( P6430  %in%  c(1,4,8)) %>% # Asal privado, TCP, y jornalero
  mutate(
    seguridad.social =  case_when(P6920 == 1 ~ "Si",
                                  P6920 == 2 ~ "No"),
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
        P6870  %in% 8:9  ~ "Grande"),#  51 +,
    grupos.tamanio = case_when( P6430 == 4  ~ "Pequeño",
                                TRUE ~grupos.tamanio)
    ) 

# ver <- ec.categ %>% 
#   group_by(part.time.inv,grupos.tamanio,grupos.calif) %>% 
#   summarise(casos = n())
# 
# table(ec.categ$ISCO.1.digit)

# co.tasa.asalariz <- co.categ %>%
#   group_by(periodo) %>% 
#   summarise(total.ocupados = sum(fexp,na.rm = T),
#             asalariados = sum(fexp[P6430  %in%  1:2]),
#             tasa.asalarizacion = asalariados/total.ocupados) %>% 
#   ungroup() %>% 
#   summarise(across(.cols = 2:ncol(.),.fns = mean))  



co.ocupados.distrib  <- co.categ %>% 
  mutate(uno = 1) %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(total.casos = n(),
            total.asalariados = sum(P6430 != 4),
            ocupados = sum(fexp,na.rm = T),
            asalariados = sum(fexp[P6430 != 4],na.rm = T),
            ocupados_muestral = n(),
            asalariados_muestral = sum(uno[P6430 != 4],na.rm = T),
            tcp = sum(fexp[P6430 == 4],na.rm = T),
            tasa.asalarizacion = asalariados/ocupados,
            promedio.ing.oc.prin=weighted.mean(
              x = INGLABO,
              w = fexp,na.rm = T),
            promedio.ing.oc.prin.tcp=weighted.mean(
              x = INGLABO[P6430 == 4],
              w = fexp[P6430 == 4],na.rm = T),
            promedio.ing.oc.prin.asal=weighted.mean(
              x = INGLABO[P6430 != 4],
              w = fexp[P6430 != 4],na.rm = T)
  ) %>% 
  group_by(periodo) %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.tcp= tcp/sum(tcp))%>% 
  ungroup() %>% 
  group_by(grupos.calif,grupos.tamanio) %>% 
  summarise(periodo = 2019, 
            total.casos = sum(total.casos),
            total.asalariados = sum(total.asalariados),
            across(.cols = 6:ncol(.)-2,.fns = mean))  

co.ocupados.distrib.agregado  <- co.categ %>% 
  group_by(periodo) %>% 
  summarise(ocupados = sum(fexp,na.rm = T),
            asalariados = sum(fexp[P6430 != 4],na.rm = T),
            ocupados_muestral = n(),
            tcp = sum(fexp[P6430 == 4],na.rm = T),
            tasa.asalarizacion = asalariados/ocupados,
            promedio.ing.oc.prin=weighted.mean(
              x = INGLABO,
              w = fexp,na.rm = T),
            promedio.ing.oc.prin.tcp=weighted.mean(
              x = INGLABO[P6430 == 4],
              w = fexp[P6430 == 4],na.rm = T),
            promedio.ing.oc.prin.asal=weighted.mean(
              x = INGLABO[P6430 != 4],
              w = fexp[P6430 != 4],na.rm = T)
  ) %>% 
  group_by(periodo) %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.tcp= tcp/sum(tcp))%>% 
  ungroup() %>% 
  #group_by(grupos.calif,grupos.tamanio) %>% 
  summarise(periodo = 2019, 
            across(.cols = 2:ncol(.)-2,.fns = mean))  



co.asalariados.tasas <- co.categ %>% 
  filter(P6430 %in% c(1,8)) %>% # Asalariad S.priv
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    seguridad.social.si = sum(fexp[seguridad.social=="Si"],na.rm = T),
    seguridad.social.no = sum(fexp[seguridad.social=="No"],na.rm = T),
    registrados =sum(fexp[registracion=="Si"],na.rm = T),
    no.registrados =sum(fexp[registracion=="No"],na.rm = T),
    empleo.temporal =sum(fexp[tiempo.determinado=="Si"],na.rm = T),
    empleo.no.temporal =sum(fexp[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(fexp[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(fexp[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(fexp[part.time.inv=="Full Time"],na.rm = T),
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
  rename_with(~str_c(.,".asal"), .cols = 4:ncol(.)) %>% 
  group_by(grupos.calif,grupos.tamanio) %>% 
  summarise(periodo = 2019, 
            across(.cols = 4:ncol(.)-2,.fns = mean))  


co.tcp.tasas <- co.categ %>% 
  filter(P6430 %in% 4) %>% # TCP
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,periodo) %>% 
  summarise(
    seguridad.social.si = sum(fexp[seguridad.social=="Si"],na.rm = T),
    seguridad.social.no = sum(fexp[seguridad.social=="No"],na.rm = T),
    registrados =sum(fexp[registracion=="Si"],na.rm = T),
    no.registrados =sum(fexp[registracion=="No"],na.rm = T),
    empleo.temporal =sum(fexp[tiempo.determinado=="Si"],na.rm = T),
    empleo.no.temporal =sum(fexp[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(fexp[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(fexp[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(fexp[part.time.inv=="Full Time"],na.rm = T),
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
  rename_with(~str_c(.,".tcp"), .cols = 4:ncol(.)) %>% 
  group_by(grupos.calif,grupos.tamanio) %>% 
  summarise(periodo = 2019, 
            across(.cols = 4:ncol(.)-2,.fns = mean))  



co.asalariados.tasas.agregado <- co.categ %>% 
  filter(P6430 %in% c(1,8)) %>% # Asalariad S.priv
  #filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(periodo) %>% 
  summarise(
    seguridad.social.si = sum(fexp[seguridad.social=="Si"],na.rm = T),
    seguridad.social.no = sum(fexp[seguridad.social=="No"],na.rm = T),
    registrados =sum(fexp[registracion=="Si"],na.rm = T),
    no.registrados =sum(fexp[registracion=="No"],na.rm = T),
    empleo.temporal =sum(fexp[tiempo.determinado=="Si"],na.rm = T),
    empleo.no.temporal =sum(fexp[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(fexp[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(fexp[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(fexp[part.time.inv=="Full Time"],na.rm = T),
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
  rename_with(~str_c(.,".asal"), .cols = 2:ncol(.)) %>% 
  summarise(periodo = 2019, 
            across(.cols = 2:ncol(.)-2,.fns = mean))


co.tcp.tasas.agregado <- co.categ %>% 
  filter(P6430 %in% 4) %>% # TCP
  #filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(periodo) %>% 
  summarise(
    seguridad.social.si = sum(fexp[seguridad.social=="Si"],na.rm = T),
    seguridad.social.no = sum(fexp[seguridad.social=="No"],na.rm = T),
    registrados =sum(fexp[registracion=="Si"],na.rm = T),
    no.registrados =sum(fexp[registracion=="No"],na.rm = T),
    empleo.temporal =sum(fexp[tiempo.determinado=="Si"],na.rm = T),
    empleo.no.temporal =sum(fexp[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(fexp[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(fexp[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(fexp[part.time.inv=="Full Time"],na.rm = T),
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
  rename_with(~str_c(.,".tcp"), .cols = 2:ncol(.)) %>% 
  summarise(periodo = 2019, 
            across(.cols = 2:ncol(.)-2,.fns = mean))

co.resultado <- co.ocupados.distrib %>% 
  left_join(co.asalariados.tasas) %>% 
  left_join(co.tcp.tasas) %>% 
  mutate(Pais = "Colombia",
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

co.resultado.agregado <- co.ocupados.distrib.agregado %>% 
  left_join(co.asalariados.tasas.agregado) %>% 
  left_join(co.tcp.tasas.agregado) %>% 
  mutate(Pais = "Colombia",
         tamanio.calif = "Total")

saveRDS(co.resultado,file = "Resultados/Colombia.RDS")  
saveRDS(co.resultado.agregado,file = "Resultados/Colombia_agregado.RDS")  

