####funciones y librerias#####
library(eph)
library(tidyverse)
library(openxlsx)

  
####bases de datos#####
####ARGENTINA#####
Base_ARG0814 <- readRDS("../bases/Argentina/EPH2008_2014.RDS")  
Base_ARG1719 <- readRDS("../bases/Argentina/EPH2016_2019.RDS")  
load("Fuentes Complementarias/crosstable_cno2001_isco08.rda")  

####ARG Variables####
Variables1719  <- c("CODUSU","NRO_HOGAR","COMPONENTE","ANO4","TRIMESTRE" ,"AGLOMERADO","H15",
    "CH04", "CH06", "CH12","CH13","CH14","CH15","ESTADO","CAT_OCUP","INTENSI",
    "PP04A", "PP04B_COD","PP07H","P21","PONDERA","PP04D_COD","PP04C",
    "PP07A","PP07C","PP05B2_ANO","PP04B3_ANO","PP07E","NIVEL_ED","PONDIIO","DECOCUR",
    "PP11D_COD","PP04C","PP04C99","PP03G","PP3E_TOT")
  
  
Variables0814  <- c("CODUSU","NRO_HOGAR","COMPONENTE","ANO4","TRIMESTRE" ,"AGLOMERADO","H15",
                      "CH04", "CH06", "CH12","CH13","CH14","CH15","ESTADO","CAT_OCUP","INTENSI",
                      "PP04A", "PP04B_COD","PP07H","P21","PONDERA","PP04D_COD","PP04B_CAES","PP04C",
                      "PP07A","PP07C","PP05B2_ANO","PP04B3_ANO","PP07E","NIVEL_ED","DECOCUR",
                      "PP11D_COD","PP04C","PP04C99","PP03G","PP3E_TOT")

bases_bind <- bind_rows(
Base_ARG0814 %>% select(Variables0814),
Base_ARG1719 %>% select(Variables1719)) %>% 
  mutate(PP04B_COD = as.character(PP04B_COD), 
         PP04B_COD = dplyr::case_when(
           nchar(PP04B_COD) == 1 ~ paste0("0", PP04B_COD),
           nchar(PP04B_COD) == 2 ~ PP04B_COD, 
           nchar(PP04B_COD) == 3 ~ paste0("0", PP04B_COD),
           nchar(PP04B_COD) == 4 ~ PP04B_COD)) %>% 
  mutate(PP04B_COD = case_when(ANO4 %in% 2011:2015 ~ PP04B_CAES,
                               TRUE~ PP04B_COD))


rm(list = c("Base_ARG1719","Base_ARG0814"))
gc()

#####Calificaciones##########
bases_bind <- bases_bind %>% 
  mutate(PP04D_COD = stringr::str_pad(PP04D_COD, 5, side = "left",pad = "0"),
         digito.calificacion = str_sub(PP04D_COD,5,5),
         calificacion = factor(
               case_when(
                 digito.calificacion == 1 ~ "Profesionales",
                 digito.calificacion == 2 ~ "Técnicos",
                 digito.calificacion == 3 ~ "Operativos",
                 digito.calificacion == 4 ~ "No calificados"),
                 levels = c("Profesionales","Técnicos","Operativos","No calificados")
                 ),
         grupos.calif = factor(
                 case_when(
                   calificacion %in% c("Profesionales","Técnicos") ~ "Alta",
                   calificacion ==   "Operativos" ~ "Media",
                   calificacion ==   "No calificados" ~ "Baja"),
                 levels = c("Baja","Media","Alta")),
         cno.anterior.desocup = stringr::str_pad(PP11D_COD,5,
                                                 side = "left", 
                                                 pad = "0"),
         calif.anterior.desocup = substr(cno.anterior.desocup, 5, 5),
         grupos.calif.desocup = factor (
           case_when(
             calif.anterior.desocup %in% 1:2~ "Alta",
             calif.anterior.desocup ==   3 ~ "Media",
             calif.anterior.desocup ==   4 ~ "Baja",
             TRUE ~ "Ns/Nr"),
           levels = c("Baja","Media","Alta","Ns/Nr")))

bases_bind <- bases_bind %>% 
  left_join(crosstable_cno2001_isco08 %>% rename(PP04D_COD = cno.2001.code)) %>% 
  mutate(isco.1.digit = str_sub(isco08.2.digit.code,1,1),
         grupos.calif.isco = factor(
           case_when(
             isco.1.digit %in% 1:3 ~ "Alta",
             isco.1.digit %in% 4:8 ~ "Media",
             isco.1.digit %in% 9 ~ "Baja",),
           levels = c("Baja","Media","Alta")))

comparacion<- bases_bind %>%
  group_by(ANO4,grupos.calif,grupos.calif.isco) %>% 
  summarise(Casos = n())

bases_bind <- bases_bind %>% 
  mutate(grupos.calif = case_when(!is.na(grupos.calif.isco) ~ grupos.calif,
                                         TRUE ~ grupos.calif.isco))

correccion<- bases_bind %>%
  group_by(ANO4,grupos.calif,grupos.calif.isco) %>% 
  summarise(Casos = n())

####ARG categorias####
Base_EPH.cat <- bases_bind %>%
  #mutate(PP04B_COD = PP04B_CAES) %>% 
    mutate(
      grupos.tamanio = factor(
      case_when(PP04C %in% 1:6  |(PP04C %in% 99 & PP04C99 == 1)~ "Pequeño",
                PP04C %in% 7:8  ~ "Mediano",
                PP04C %in% 9:12 |(PP04C %in% 99 & PP04C99 == 3)~ "Grande"),
      levels = c("Pequeño","Mediano","Grande")),
    seguridad.social = case_when(PP07H == 1 ~ "Si",
                             PP07H == 2 ~ "No"),
    registracion = case_when(PP07H == 1 ~ "Si",
                             PP07H == 2 ~ "No"),
    Categoria =  case_when(CAT_OCUP == 1 ~ "Patrones",
                           CAT_OCUP == 2 ~ "TCP",
                           CAT_OCUP == 3 ~ "Asalariados",
                           CAT_OCUP == 4 ~ "TFSR",
                           CAT_OCUP == 9 ~ "Ns/Nr"),
    part.time.inv = case_when(ESTADO == 1 & PP3E_TOT < 35 & PP03G == 1 ~ "Part Involunt",
                              ESTADO == 1 & PP3E_TOT < 35 & PP03G == 2 ~"Part Volunt",
                              ESTADO == 1 & PP3E_TOT >= 35  ~"Full Time",
                              TRUE ~ "Otros"),
    sobreocup = case_when(PP3E_TOT >=  46 & PP3E_TOT <=168 ~"Si",
                          PP3E_TOT >=  1 & PP3E_TOT <=45  ~ "No"),
    tiempo.determinado = case_when(PP07C ==  1 ~"Si",
                                   PP07C ==  2 ~ "No"))

desocup.calif.ant.arg <- Base_EPH.cat %>%
  filter(grupos.calif.desocup!= "Ns/Nr") %>% 
  group_by(ANO4,TRIMESTRE,grupos.calif.desocup) %>% 
  summarise(desocupados = sum(PONDERA[ESTADO %in% 2],na.rm = TRUE)) %>% 
  group_by(ANO4,TRIMESTRE) %>% 
  mutate(distribucion = desocupados/sum(desocupados))

##Tablas Argentina##### 
eph.ocup.privados <- Base_EPH.cat %>% 
  filter(ESTADO == 1) %>% #Ocupados
  filter(!(ANO4 %in% 2011:2020 & PP04B_COD %in% c(83:84,8300:8499,#Adm Publica
                                                  97:98,9700:9899)),#Serv Domestico
         !(ANO4 %in% 2008:2010 & PP04B_COD %in% c(75,7500:7599,#Adm Publica
                                                  95,9500:9599))#Serv Domestico
         ) %>% 

         mutate(Pais = "ARG",
         PONDERA_SALARIOS = case_when(ANO4  %in%  2016:2020 ~ as.integer(PONDIIO),
                                      ANO4  %in%  2003:2015 ~ as.integer(PONDERA)))  



arg.ocupados.distrib <-  eph.ocup.privados %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,ANO4) %>% 
  summarise(
    ocupados = sum(PONDERA,na.rm = T)/4,
    asalariados = sum(PONDERA[CAT_OCUP == 3],na.rm = T)/4,
    no.asalariados = sum(PONDERA[CAT_OCUP != 3],na.rm = T)/4,
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = P21,
      w = PONDERA_SALARIOS,na.rm = T),
    promedio.ing.oc.prin.noasal=weighted.mean(
      x = P21[CAT_OCUP != 3],
      w = PONDERA_SALARIOS[CAT_OCUP != 3],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = P21[CAT_OCUP == 3],
      w = PONDERA_SALARIOS[CAT_OCUP == 3],na.rm = T)
  ) %>% 
  group_by(ANO4) %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.no.asal= no.asalariados/sum(no.asalariados))

arg.ocupados.distrib.agregado <-  eph.ocup.privados %>% 
  group_by(ANO4) %>% 
  summarise(
    ocupados = sum(PONDERA,na.rm = T)/4,
    asalariados = sum(PONDERA[CAT_OCUP == 3],na.rm = T)/4,
    no.asalariados = sum(PONDERA[CAT_OCUP != 3],na.rm = T)/4,
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = P21,
      w = PONDERA_SALARIOS,na.rm = T),
    promedio.ing.oc.prin.noasal=weighted.mean(
      x = P21[CAT_OCUP != 3],
      w = PONDERA_SALARIOS[CAT_OCUP != 3],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = P21[CAT_OCUP == 3],
      w = PONDERA_SALARIOS[CAT_OCUP == 3],na.rm = T)
  ) %>% 
  group_by(ANO4) %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.no.asal= no.asalariados/sum(no.asalariados))

arg.asalariados.tasas <- eph.ocup.privados %>% 
  filter(CAT_OCUP == 3) %>% # Asalariado
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio,ANO4) %>% 
  summarise(
    seguridad.social.si = sum(PONDERA[seguridad.social=="Si"],na.rm = T),
    seguridad.social.no = sum(PONDERA[seguridad.social=="No"],na.rm = T),
    registrados =sum(PONDERA[registracion=="Si"],na.rm = T),
    no.registrados =sum(PONDERA[registracion=="No"],na.rm = T),
    empleo.temporal =sum(PONDERA[tiempo.determinado=="Si"],na.rm = T),
    empleo.no.temporal =sum(PONDERA[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(PONDERA[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(PONDERA[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(PONDERA[part.time.inv=="Full Time"],na.rm = T),
    tasa.partime.asal = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp.asal = empleo.temporal/(empleo.temporal+
                                        empleo.no.temporal))

arg.asalariados.tasas.agregado <- eph.ocup.privados %>% 
  filter(CAT_OCUP == 3) %>% # Asalariado
#  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(ANO4) %>% 
  summarise(
    seguridad.social.si = sum(PONDERA[seguridad.social=="Si"],na.rm = T),
    seguridad.social.no = sum(PONDERA[seguridad.social=="No"],na.rm = T),
    registrados =sum(PONDERA[registracion=="Si"],na.rm = T),
    no.registrados =sum(PONDERA[registracion=="No"],na.rm = T),
    empleo.temporal =sum(PONDERA[tiempo.determinado=="Si"],na.rm = T),
    empleo.no.temporal =sum(PONDERA[tiempo.determinado=="No"],na.rm = T),
    part.involun = sum(PONDERA[part.time.inv=="Part Involunt"],na.rm = T),
    part.volunt = sum(PONDERA[part.time.inv=="Part Volunt"],na.rm = T),
    full.time = sum(PONDERA[part.time.inv=="Full Time"],na.rm = T),
    tasa.partime.asal = part.involun/(part.involun+
                                        part.volunt+
                                        full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp.asal = empleo.temporal/(empleo.temporal+
                                        empleo.no.temporal))

arg.resultado <- arg.ocupados.distrib %>%
  left_join(arg.asalariados.tasas) %>% 
  rename(periodo = ANO4) %>% 
  mutate(Pais = "Argentina",
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

arg.resultado.agregado <- arg.ocupados.distrib.agregado %>% 
  left_join(arg.asalariados.tasas.agregado) %>% 
  mutate(Pais = "Argentina",
         tamanio.calif = "Total")

saveRDS(arg.resultado,file = "Resultados/Argentina.RDS")
saveRDS(arg.resultado.agregado,file = "Resultados/Argentina_agregado.RDS")
#write.xlsx(comparacion,"Resultados/America/Cuadros/argentina_cno_isco.xlsx")
