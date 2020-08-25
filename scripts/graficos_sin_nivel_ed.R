#####Librerias#####
library(openxlsx)
library(tidyverse)
library(stringr)
library(ggrepel)
library(ggthemes)
library(cluster)

########PALETA#####
azul <- colorspace::diverge_hcl(n = 12,h = c(255,330),
                                l = c(40,90))[c(4,2,1)]
verde <- colorspace::diverge_hcl(n = 12,h = c(130,43),
                                 c = 100,
                                 l = c(70,90))[c(4,2,1)]

naranja <- colorspace::diverge_hcl(n = 12,h = c(130,43),
                                   c = 100,
                                   l = c(70,90))[c(10,11,12)]
paleta <- c(azul,
            naranja,
            verde)

paleta3 <- c(azul[1],
             naranja[1],
             verde[1])

#####Carga de datos#####

Paises <- read.xlsx("data/Prod y Salarios2.xlsx",sheet = "Paises")


Salarios_UMN <- read.xlsx("data/Prod y Salarios2.xlsx",
                          sheet = "salario nominal - UMN"
                          ) %>% 
  rename(ANO4 = X1)%>%
  pivot_longer(cols = 2:ncol(.),
               names_to = "nombre.pais",
               values_to = "Salario.UMN") %>% 
  mutate(nombre.pais = str_replace_all(nombre.pais,"[[:punct:] ]+",replacement = " "))

IPC_2005 <- read.xlsx("data/Prod y Salarios2.xlsx",
                          sheet = "IPC (2005)") %>% 
  rename(ANO4 = X1)%>%
  pivot_longer(cols = 2:ncol(.),
               names_to = "nombre.pais",
               values_to = "IPC_2005") %>% 
  mutate(nombre.pais = str_replace_all(nombre.pais,"[[:punct:] ]+",replacement = " "))


PPA_WB <- read.csv("data/PPA.csv") %>%
  rename(COD.OCDE = Country.Code) %>% 
  filter(Classification.Code == "PPPGlob",Series.Code == 9020000) %>%
  pivot_longer(cols = 7:ncol(.),
               names_to = "Año",
               values_to = "PPA.BENCHMARK.2017") %>% 
  mutate(ANO4 = as.numeric(str_extract(Año,"[[:digit:]]{4}")))


Estimacion_GW <- Salarios_UMN %>% 
  left_join(IPC_2005) %>% 
  left_join(Paises) %>% 
  left_join(PPA_WB)  %>% 
  mutate(PPA.BENCHMARK.2017 = as.numeric(as.character(PPA.BENCHMARK.2017))) %>% 
  group_by(ANO4) %>% 
  mutate(IPC_2005_USA = IPC_2005[nombre.pais == "Estados Unidos"]) %>% 
  group_by(nombre.pais) %>% 
  mutate(PPA.BENCHMARK.2017.EXTRAPOLADO = 
           case_when(!is.na(PPA.BENCHMARK.2017)~PPA.BENCHMARK.2017,
                     is.na(PPA.BENCHMARK.2017)~
                       PPA.BENCHMARK.2017[ANO4 == 2017]*
                       (IPC_2005/IPC_2005[ANO4 == 2017])/
                       (IPC_2005_USA/IPC_2005_USA[ANO4 == 2017])
                       )) 
  
  

####Salarios y productividad### Laupa y Dami

Salarios <- read.xlsx("data/Prod y Salarios2.xlsx",sheet = 1) %>% 
  rename(nombre.pais = X1,
         COD.OCDE = LOCATION)


Productividad <- read.xlsx("data/Prod y Salarios2.xlsx",
                           sheet = 2,startRow = 2) %>% 
  rename(COD.OCDE = LOCATION, name = label.x) 


PPA <- read.xlsx("data/Prod y Salarios2.xlsx",
                           sheet = 3) %>% 
  rename(ANO4 = Coeficientes) %>% 
  pivot_longer(cols = 2:ncol(.),
               names_to = "COD.OCDE",
               values_to = "PPA") %>% 
  left_join(Paises) 

#Procesamiento de Encuestas#
#Argentina y USA#
load("Resultados/ARG_USA.RDATA")
load("Resultados/EUROPA.RDATA")

##Europa Ingresos#
asal.Calificacion.europa <- read.xlsx(
  "Resultados/RestultadosLFS 29.5.xlsx",sheet = "Asalariados.nivel.ed")



#Unificacion de Paises####
options(scipen = 999)
EUROPA_USA_ARG <- PRECARIEDAD %>% 
  bind_rows(Resultados_bind %>% rename(Pais = pais)) %>% 
  mutate(
    ANO4 = case_when(Pais == "USA" ~ ANO4-1,
                            TRUE~ANO4),
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
                                  "Grande - Alta")),
    total.tcp = case_when(is.na(total.tcp)~ 0,
                          TRUE ~ total.tcp),
    tasa.parttime.tcp = case_when(is.na(tasa.parttime.tcp)~ 0,
                          TRUE ~ tasa.parttime.tcp),
  tasa.precariedad = 
    (total.asal*tasa.1.asalariados+
    total.tcp*tasa.parttime.tcp)/
  (total.asal+total.tcp))

##tasas prec##
cuadro.tasas.precariedad <- EUROPA_USA_ARG %>% 
  select(ANO4,Pais,grupos.calif,grupos.tamanio,
   tasa.s.desc.jubil,tasa.partime.asal,tasa.temp.asal,tasa.1.asalariados) 

write.xlsx(list("tasas.precariedad" = cuadro.tasas.precariedad),file = "Resultados/tasas precariedad.xlsx")  

#######asalarización######
asalarizacion.tamanio <- EUROPA_USA_ARG %>% 
  group_by(ANO4,Pais,grupos.tamanio) %>% 
  summarise(tasa.asal = sum(total.ocupados*tasa.asalarizacion)/
              sum(total.ocupados),
            tasa.TCP = 1-tasa.asal)

asalarizacion.total <- EUROPA_USA_ARG %>% 
  group_by(ANO4,Pais) %>% 
  summarise(tasa.asal = sum(total.ocupados*tasa.asalarizacion)/
              sum(total.ocupados),
            tasa.TCP = 1-tasa.asal)

write.xlsx(list("asalarizacion por tamanio" = asalarizacion.tamanio,
                "asalarizacion total" = asalarizacion.total),
file = "Resultados/asalarizacion.xlsx")  

####Salarios y productividad agregada#####
# salarios.prod <- Salarios %>% 
#   left_join(Productividad) %>% 
#   left_join(Paises) %>% 
#   select(Salario2018=`2018`,nombre.pais,COD.ESPANIOL,Color,`Prod.relativa.a.EEUU.-.TOTAL`) %>% 
#   #filter(!is.na(Salario2018),!nombre.pais %in%  c("Irlanda","Estonia")) %>%
#   ungroup() %>% 
#   mutate(salario.relativo.usa = 
#            Salario2018/Salario2018[nombre.pais=="Estados Unidos"]*100)
# 
# ggplot(salarios.prod,
#        aes(x=salario.relativo.usa,
#            y=`Prod.relativa.a.EEUU.-.TOTAL`,
#            label = nombre.pais))+
#   geom_smooth(method = 'lm',color = "grey70",se = FALSE)  +
#   geom_point(aes(color = Color))+
#   geom_text_repel(aes(color = Color))+  
#   labs(title = "Salarios promedio y productividad relativa (USA = 100). Año 2018",
#        x = "Salario Relativo (USA = 100)",
#        y = "Productividad Relativa (USA = 100)" )+
#   theme_tufte()+
#   theme(panel.spacing = unit(1,"cm"),
#         legend.position = "none",
#         panel.grid.minor.x = element_line(colour = "grey"),
#         panel.grid.minor.y= element_line(colour = "grey"),
#         panel.grid.major.x = element_line(colour = "grey30"),
#         panel.grid.major.y = element_line(colour = "grey30"),
#         text = element_text(size = 15))+
#   scale_fill_grey()
# 
# ggsave("Resultados/productividad_salarios.png",scale = 2)


##estimacion gw###
salarios.prod.GW <- Estimacion_GW %>% 
  ungroup() %>% 
  filter(!nombre.pais %in%  c("Corea del Sur","Irlanda")) %>% 
  filter(ANO4  %in%  2017:2018) %>% 
  left_join(Productividad %>% select(COD.OCDE,`Prod.relativa.a.EEUU.-.TOTAL`)) %>% 
  mutate(salario.PPA = Salario.UMN/PPA.BENCHMARK.2017.EXTRAPOLADO) %>% 
  filter(!is.na(salario.PPA)) %>% 
  group_by(ANO4) %>% 
  mutate(salario.relativo.usa = salario.PPA/salario.PPA[nombre.pais=="Estados Unidos"]*100)
  

salarios.prod.2017 <- salarios.prod.GW %>% 
  filter(ANO4==2017) %>% 
  select(nombre.pais,Color,Salario.UMN,PPA.BENCHMARK.2017,salario.PPA,salario.relativo.usa,`Prod.relativa.a.EEUU.-.TOTAL`)

ggplot(salarios.prod.2017,
       aes(x=salario.relativo.usa,
           y=`Prod.relativa.a.EEUU.-.TOTAL`,
           label = nombre.pais))+
  geom_smooth(method = 'lm',color = "grey70",se = FALSE)  +
  geom_point(aes(color = Color))+
  geom_text_repel(aes(color = Color))+  
  labs(title = "Salarios promedio y productividad relativa (USA = 100). Año 2017",
       x = "Salario Relativo (USA = 100)",
       y = "Productividad Relativa (USA = 100)" )+
  theme_tufte()+
  theme(panel.spacing = unit(1,"cm"),
        legend.position = "none",
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.minor.y= element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey30"),
        panel.grid.major.y = element_line(colour = "grey30"),
        text = element_text(size = 15))+
  scale_fill_grey()

ggsave("Resultados/productividad_salarios_GW_filtrado.png",scale = 2.5)
write.xlsx(list("salarios.prod.2017" = salarios.prod.2017),
           file = "Resultados/Salarios_Productividad2017_CCNN.xlsx")  


# ggplot(salarios.prod.GW %>% filter(ANO4==2018),
#        aes(x=salario.relativo.usa,
#            y=`Prod.relativa.a.EEUU.-.TOTAL`,
#            label = nombre.pais))+
#   geom_smooth(method = 'lm',color = "grey70",se = FALSE)  +
#   geom_point(aes(color = Color))+
#   geom_text_repel(aes(color = Color))+  
#   labs(title = "Salarios promedio y productividad relativa (USA = 100). Año 2018",
#        x = "Salario Relativo (USA = 100)",
#        y = "Productividad Relativa (USA = 100)" )+
#   theme_tufte()+
#   theme(panel.spacing = unit(1,"cm"),
#         legend.position = "none",
#         panel.grid.minor.x = element_line(colour = "grey"),
#         panel.grid.minor.y= element_line(colour = "grey"),
#         panel.grid.major.x = element_line(colour = "grey30"),
#         panel.grid.major.y = element_line(colour = "grey30"),
#         text = element_text(size = 15))+
#   scale_fill_grey()
# 
# ggsave("Resultados/productividad_salarios_GW_filtrado_2018.png",scale = 2)

####PERFILES####

EUROPA_USA_ARG %>% 
  left_join(Paises %>% rename(Pais = COD.ENCUESTAS)) %>% 
  filter((ANO4 == 2018 & !(Pais %in% c("DE")))|
         ANO4 == 2017 & Pais %in% c("DE")) %>% 
  ggplot(.,
         aes(x = COD.ESPANIOL, y = particip.ocup,
             fill = tamanio.calif,group = tamanio.calif,
             label = scales::percent(particip.ocup))) +
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5),size=3)+
#  labs(title = "Distribución del empleo según grupos")+
  theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_blank(),
        axis.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 45),
        #panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta)+
  scale_y_continuous(labels = scales::percent)

ggsave("Resultados/grupos_calificacion_tamanio.png",width = 10,height = 8)


####Desocupados por calificacion#####
Particip.calif <- EUROPA_USA_ARG %>% 
  ungroup() %>% 
  mutate(grupos.calif = factor(grupos.calif,
                               levels = c("Baja","Media","Alta"))) %>% 
  group_by(grupos.calif,Pais,ANO4) %>% 
  summarise(Particip_emp = sum(particip.ocup))

desocup.arg <- desocup.calif.ant.arg %>% 
  group_by(ANO4, grupos.calif = grupos.calif.desocup) %>% 
  summarise(Pais = "ARG",
            particip.desocup = mean(distribucion)) 

desocup.usa <- desocup.calif.ant.usa %>% 
  mutate(Pais = "USA") %>% 
  select(-desocupados) %>% 
  rename(ANO4 =YEAR,particip.desocup = distribucion)

desocup.europa <- Desocup.Calif %>% 
  select(ANO4,
         Pais,
         grupos.calif = CALIF,
         particip.desocup = Por.Desocup,
         Particip_emp = Por.Ocup) %>% 
  mutate(Pais = case_when(Pais == "UK"~ "GB",
                          T ~ Pais))

desocup.calif <- desocup.arg %>% 
  bind_rows(desocup.usa)%>%
  left_join(Particip.calif) %>% 
  bind_rows(desocup.europa)%>% 
  mutate(Brecha = Particip_emp - particip.desocup) %>% 
  pivot_longer(cols = c(4,5),names_to = "distrib",values_to = "Valor") %>% 
  rename(COD.ENCUESTAS = Pais) %>% 
  left_join(Paises) %>% 
  mutate(Calificación = factor(grupos.calif,
                               levels = c("Baja","Media","Alta")),
         distrib = factor(distrib,
                          labels = c("Ocupados","Desocupados"),
                          levels = c("Particip_emp","particip.desocup"))) 
  

desocup.calif %>% 
  filter(ANO4 == 2018) %>%  
ggplot(.,
       aes(x=distrib,
           y=Valor,
           label = scales::percent(Valor),
           fill = Calificación,
           group = Calificación))+
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5),size=2.5)+
  labs(title = "Distribución de Ocupados y desocupados con empleo anterior según calificación",
       subtitle = "Año 2018")+
  theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        axis.title = element_blank(),
        axis.text.x = element_text(),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta3) +
  facet_wrap(~COD.ESPANIOL,scales = "free_x")
ggsave("Resultados/desocupacion anterior.png",scale = 2)

####Tasas precariedad####
#Graf 4#
graf.todos <- EUROPA_USA_ARG %>%
  left_join(Paises %>% rename(Pais = COD.ENCUESTAS)) %>% 
  filter((ANO4 == 2018 & !(Pais %in% c("DE")))|
           ANO4 == 2017 & Pais %in% c("DE")) %>% 
  ungroup() %>% 
  select(ANO4,COD.ESPANIOL,Pais,tamanio.calif,tasa.s.desc.jubil,tasa.partime.asal,tasa.temp.asal) %>% 
  pivot_longer(cols = 5:ncol(.),
               names_to = "indicador",values_to = "valor") %>% 
  mutate(
    indicador = factor(
      case_when(
        indicador == "tasa.s.desc.jubil" ~ "Sin descuento jubilatorio",
        indicador == "tasa.temp.asal" ~ "Empleo temporal",
        indicador == "tasa.partime.asal" ~ "Part time involuntario"),
      levels = c("Sin descuento jubilatorio",
                 "Empleo temporal",
                 "Part time involuntario"))) %>% 
  ungroup()

graf.todos %>% 
  filter(Pais != "SE") %>% 
  ggplot(.,
         aes(x = COD.ESPANIOL, y = valor,
             fill = indicador,group = indicador,
             label = round(valor,2))) +
  geom_col(position = "dodge")+
  #geom_text(position = position_dodge(),size=2.5,angle = 90)+
  #labs(title = "Expresiones de la precariedad según perfiles y paises. Año 2018")+
  theme_tufte()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(0.2,"cm"),
        #legend.spacing.x  = unit(0.4,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 90,size = 12,vjust = .5),
        axis.ticks.x = element_blank(),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_y_continuous(labels = scales::percent)+
  #scale_fill_manual(values = paleta)+
  facet_wrap(~tamanio.calif,scales = "free_x")

ggsave("Resultados/tasas separadas.png",scale = 3)


#########1 o mas precariedad########
EUROPA_USA_ARG %>% 
  left_join(Paises %>% rename(Pais = COD.ENCUESTAS)) %>% 
  filter((ANO4 == 2018 & !(Pais %in% c("DE")))|
           ANO4 == 2017 & Pais %in% c("DE")) %>% 
  ggplot(.,
         aes(x = tamanio.calif, y = tasa.1.asalariados,
             fill = tamanio.calif,group = tamanio.calif,
             label = scales::percent(tasa.1.asalariados))) +
  geom_col(position = "dodge")+
  geom_text(position = position_dodge(),size=2)+
  labs(title = "Tasa de precariedad. Año 2018",
       subtitle = "Una o más expresiones de precariedad. Total Asalariados")+
  theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        legend.title  = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta)+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~COD.ESPANIOL)
ggsave("Resultados/Precariedad asalariados.png",scale = 2.5)



###Evolucion 1 o mas####
EUROPA_USA_ARG %>% 
  left_join(Paises %>% rename(Pais = COD.ENCUESTAS)) %>% 
  filter(Pais != "SE",ANO4<=2018,
         !(ANO4 == 2018 & Pais %in% c("DE"))) %>% 
  ggplot(.,
         aes(x = as.character(ANO4), y = tasa.1.asalariados,
             color = tamanio.calif,group = tamanio.calif,
             label = round(tasa.1.asalariados,2))) +
  geom_line(size = 1)+
  geom_point(size = 1.2)+
  #geom_text(position = position_dodge(),size=2.5,angle = 90)+
  labs(title = "Porcentaje de asalariados con al menos una expresión de precariedad")+
  theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        axis.title = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.ticks.x = element_blank(),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_color_manual(values = paleta)+
  facet_wrap(~COD.ESPANIOL,scales = "free_y")

#ggsave("Resultados/evol_precariedad.png",scale = 2)

####Argentina Precariedad Asal####
graf.argentina <- EUROPA_USA_ARG %>%
  filter(Pais == "ARG",ANO4<=2018) %>%
  ungroup() %>% 
  select(ANO4,Pais,tamanio.calif,tasa.s.desc.jubil,tasa.partime.asal,tasa.temp.asal,
         tasa.1.asalariados,tasa.2.asalariados,tasa.3.asalariados) %>% 
  pivot_longer(cols = 4:ncol(.),
               names_to = "indicador",values_to = "valor") %>% 
  mutate(
    indicador = factor(
      case_when(
        indicador == "tasa.1.asalariados" ~ "1 o más expresiones",
        indicador == "tasa.2.asalariados" ~ "2 o más expresiones",
        indicador == "tasa.3.asalariados" ~ "3 expresiones",
        indicador == "tasa.s.desc.jubil" ~ "Sin descuento jubilatorio",
        indicador == "tasa.temp.asal" ~ "Empleo temporal",
        indicador == "tasa.partime.asal" ~ "Part time involuntario"),
      levels = c("Sin descuento jubilatorio",
                 "Empleo temporal",
                 "Part time involuntario",
                 "1 o más expresiones",
                 "2 o más expresiones",
                 "3 expresiones"))) %>% 
  ungroup()

ANO4 <- c(2015,2016)
grilla <- tidyr::crossing(
  tamanio.calif = unique(graf.argentina$tamanio.calif),
  indicador = unique(graf.argentina$indicador),
  ANO4)


graf.argentina  %>% 
  bind_rows(grilla) %>% 
  ggplot(.,
         aes(x = as.character(ANO4), y = valor,
             color = tamanio.calif,group = tamanio.calif,
             label = round(valor,1))) +
  geom_line(size = 1)+
  geom_point(size = 1.2)+
  #geom_text(position = position_dodge(),size=2.5,angle = 90)+
  labs(title = "Asalariados según expresiones de la precariedad",
       subtitle = "Argentina")+
  theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        axis.title = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(size = 12),
        axis.ticks.x = element_blank(),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_color_manual(values = paleta)+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~indicador,scales = "free_y")

#ggsave("Resultados/evol_precariedad_arg.png",scale = 2)


###########INGRESOS##################

ingresos.todos <- asal.Calificacion.europa %>% 
  full_join(Resultados_bind %>% rename(Pais = pais)) %>% 
  select(grupos.calif,grupos.tamanio,Pais,ANO4,
         ingreso.mensual.prom = Salario.prom,
         decil.m.promedio = promedio.decil) %>% 
  bind_rows(ingresos.asec.asalariados.calif,
            ingresos.eph.asalariados.calif) %>% 
  mutate(ANO4 = case_when(Pais == "USA" ~ ANO4-1,
                          TRUE~ANO4),
         Pais= case_when(Pais == "UK" ~ "GB",
                         TRUE~Pais)) %>%
  rename(COD.ENCUESTAS = Pais) %>% 
  left_join(PPA) %>% 
  left_join(Estimacion_GW %>% 
              select("COD.ENCUESTAS","COD.ESPANIOL","ANO4","PPA.BENCHMARK.2017")) %>% 
  mutate(ingreso.mensual.ppa = ingreso.mensual.prom/PPA,
         ingreso.mensual.ppa.gw = ingreso.mensual.prom/PPA.BENCHMARK.2017) %>% 
  mutate(tamanio.calif = paste0(grupos.tamanio," - ",grupos.calif),
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
                                    "Grande - Alta")))


####Deciles#####
ingresos.todos %>% 
  filter((ANO4 == 2018 & !(COD.ENCUESTAS %in% c("DE","ES")))|
          ANO4 == 2017 & COD.ENCUESTAS %in% c("DE","ES")) %>% 
  filter(COD.ENCUESTAS != "SE") %>% 
  ggplot(.,
         aes(x = tamanio.calif, y = decil.m.promedio,
             fill = tamanio.calif,group = tamanio.calif,
             label = round(decil.m.promedio,2))) +
  geom_col(position = "dodge")+
  geom_text(position = position_dodge(),size=2.5)+
  labs(title = "Promedio de deciles de pertenencia de los asalariados según grupos. Año 2018")+
  theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta)+
  facet_wrap(~COD.ESPANIOL)

ggsave("Resultados/deciles.png",scale = 3)

########Salarios perfiles###########
data.graf.PPA <- ingresos.todos %>% 
  filter((ANO4 == 2014 & !(COD.ENCUESTAS %in% c("ES")))|
           COD.ENCUESTAS %in% c("ES")) %>% 
  filter(COD.ENCUESTAS != "SE") %>% 
  filter(!is.na(ingreso.mensual.ppa)) %>% 
  #group_by(tamanio.calif) %>% 
  mutate(ing.perfil9.usa.100 = 100*ingreso.mensual.ppa.gw/
      ingreso.mensual.ppa.gw[COD.ENCUESTAS=="USA" & tamanio.calif=="Grande - Alta"]
  ) %>% 
  select(ANO4,nombre.pais,COD.ESPANIOL,tamanio.calif,
         ingreso.mensual.ppa.gw,ing.perfil9.usa.100)

#   ggplot(data.graf.PPA,
#          aes(x = COD.ESPANIOL, y = ing.usa.calif.100,
#              fill = tamanio.calif,group = tamanio.calif,
#              label = round(ing.usa.calif.100,1))) +
#   geom_col(position = "dodge")+
#   geom_text(position = position_dodge(),size=2.5)+
#   labs(title = "Salario relativo en PPA. Año 2014. Perfil 9 de USA=100")+
#   theme_tufte()+
#   theme(legend.position = "none",
#         legend.direction = "vertical",
#         axis.title = element_blank(),
#         axis.text.x = element_text(angle = 90),
#         axis.ticks.x = element_blank(),
#         panel.spacing = unit(1,"cm"),
#         panel.grid.major.y = element_line(colour = "grey"),
#         panel.grid.minor.y = element_line(colour = "grey30"),
#         panel.grid.minor.x = element_line(colour = "grey"),
#         panel.grid.major.x = element_line(colour = "grey"))+
#   scale_fill_manual(values = paleta)+
#   scale_y_continuous(limits = c(0,101),breaks = c(20,40,60,80,100))+
#   facet_wrap(~tamanio.calif)
#   
# ggsave("Resultados/PPA USA 100.png",scale = 2)
  

ggplot(data.graf.PPA,
       aes(x = COD.ESPANIOL, y = ing.perfil9.usa.100,
           fill = tamanio.calif,group = tamanio.calif,
           label = round(ing.perfil9.usa.100,1))) +
  geom_col(position = "dodge")+
  geom_text(position = position_dodge(),size=2.5)+
  labs(title = "Salario relativo en PPA. Año 2014. Perfil 9 de USA=100")+
  theme_tufte()+
  theme(legend.position = "none",
        legend.direction = "vertical",
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.ticks.x = element_blank(),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta)+
  scale_y_continuous(limits = c(0,101),breaks = c(20,40,60,80,100))+
  facet_wrap(~tamanio.calif)

ggsave("Resultados/PPA BENCHMARK 2017_USA 100.png",scale = 2)






cuadro_ingresos <- ingresos.todos %>% 
   select(ANO4,COD.ENCUESTAS,nombre.pais,COD.ESPANIOL,grupos.calif,grupos.tamanio,decil.m.promedio,
         ingreso.mensual.prom,PPA.BENCHMARK.2017,Ingreso.mensual.PPA = ingreso.mensual.ppa.gw)


write.xlsx(list("Grafico 9" = data.graf.PPA,
                "Ingresos Deciles Evol" = cuadro_ingresos),
           file = "Resultados/Salarios Encuestas PPA.xlsx")  


####Animaciones#####
library(gganimate)
library(gifski)

anim <- EUROPA_USA_ARG %>% 
  mutate(ANO4 = as.numeric(round(ANO4))) %>% 
  left_join(Paises %>% rename(Pais = COD.ENCUESTAS)) %>% 
  # filter((ANO4 == 2018 & !(Pais %in% c("DE")))|
  #          ANO4 == 2017 & Pais %in% c("DE")) %>% 
  ggplot(.,
         aes(x = COD.ESPANIOL, y = particip.ocup,
             fill = tamanio.calif,group = tamanio.calif,
             label = scales::percent(particip.ocup))) +
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5),size=3)+
  #  labs(title = "Distribución del empleo según grupos")+
  theme_tufte()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        axis.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 45),
        #panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta)+
  scale_y_continuous(labels = scales::percent)+
  labs(title = 'Year: {round(frame_time)}') +
  transition_time(ANO4)  +
  ease_aes('linear')

a <- animate(anim, nframes = 12, fps = 1,width = 1000, renderer = gifski_renderer())
anim_save("Resultados/Perfiles.gif",animation = a)

salarios.prod.anim <- Salarios_UMN %>% 
   left_join(Paises) %>% 
   left_join(PPA)  %>% 
   filter(!is.na(Salario.UMN),Salario.UMN!= 0,
          !is.na(PPA),PPA != 0) %>% 
   mutate(salario.PPP = Salario.UMN/PPA) %>% 
   group_by(ANO4) %>% 
   mutate(Salario_USA_100 = salario.PPP/salario.PPP[nombre.pais=="Estados Unidos"]*100,
          rank = min_rank(-Salario_USA_100) * 1) %>%
   ungroup()



anim2 <- salarios.prod.anim %>% 
  filter(ANO4 %in% 2015:2016) %>% 
  ungroup() %>% 
  mutate(ANO4 = as.numeric(round(ANO4))) %>% 
  ggplot(.,
         aes(x = desc(rank),
             y = Salario_USA_100,
             fill = nombre.pais,group = nombre.pais,
             label = round(Salario_USA_100,1))) +
  geom_col(position = "dodge")+
  geom_text(aes(y = 0, label = paste(nombre.pais, "")), vjust = 0.2, hjust = 1) +
  labs(title = "Evolución del salario relativo en Paridad de Poder Adquisitivo",
       subtitle = "Estados Unidos  = 100")+
  theme_minimal()+
  theme(
    axis.ticks.y = element_blank(), 
    axis.text.y = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    plot.margin = margin(1,1,1,4, "cm"),
    legend.position = "none")+
  coord_flip(clip = "off", expand = TRUE) +
  labs(title = 'Year: {round(frame_time)}') +
  transition_time(ANO4)  +
  ease_aes("cubic-in-out") +
  view_follow()# permitir que los ejes cambien

  #transition_states(states = ANO4, transition_length = 4, state_length = 1) + 
  #labs(title = 'Year: {round(closest_state)}')
  

 b<- animate(anim2, nframes = 2, fps = 1,duration = 3,
         width = 1000, renderer = gifski_renderer())
anim_save("Resultados/Salario_Nacional.gif",animation = b)
