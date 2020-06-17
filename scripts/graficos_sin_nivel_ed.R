#####Librerias#####
library(openxlsx)
library(tidyverse)
library(stringr)
library(ggrepel)
library(ggthemes)
library(cluster)
#####Carga de datos#####
#Salarios y productividad#
Eurostat <- read_tsv("data/ilc_di01.tsv") 
names(Eurostat)[1] <- "Id"
Eurostat <- Eurostat %>%
  separate(Id, c("Percentil", "Indicador","moneda","Pais"), ",")
  
Paises <- read.xlsx("data/Prod y Salarios.xlsx",sheet = "Paises")

Salarios <- read.xlsx("data/Prod y Salarios.xlsx",sheet = 1) %>% 
  rename(nombre.pais = X1,
         COD.OCDE = LOCATION)
Productividad <- read.xlsx("data/Prod y Salarios.xlsx",
                           sheet = 2,startRow = 2) %>% 
  rename(COD.OCDE = LOCATION, name = label.x)


PPA_WB <- read.csv("data/PPA.csv") %>% 
  filter(Classification.Code == "PPPGlob",Series.Code == 9020000) %>% 
  pivot_longer(cols = 7:ncol(.),
               names_to = "Año",
               values_to = "Valor")  
  

PPA <- read.xlsx("data/Prod y Salarios.xlsx",
                           sheet = 3) %>% 
  rename(ANO4 = Coeficientes) %>% 
  pivot_longer(cols = 2:ncol(.),
               names_to = "COD.OCDE",
               values_to = "PPA") %>% 
  left_join(Paises)

##Europa#


#Procesamiento de Encuestas#
#Argentina y USA#
load("Resultados/ARG_USA.RDATA")
load("Resultados/EUROPA.RDATA")

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

#######ASALARIZACION POR TAMAÑOS######

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

# Calificacion.todos <- indicadores.anuales.ocupados.calif %>% 
#   select(grupos.calif,grupos.tamanio,Particip_emp,Pais,ANO4,everything()) %>% 
#   bind_rows(Calificacion.europa %>% 
#   select(grupos.calif,grupos.tamanio,Particip_emp,Pais,ANO4,everything())) %>% 
#   arrange(desc(grupos.tamanio),grupos.calif)
  
ingresos.todos <- asal.Calificacion.europa  %>%
  select(grupos.calif,grupos.tamanio,Pais,ANO4,
         ingreso.mensual.prom = Salario.prom,
         decil.m.promedio = PromedioDecil) %>% 
  bind_rows(ingresos.asec.asalariados.calif,
            ingresos.eph.asalariados.calif) %>% 
  mutate(ANO4 = case_when(Pais == "USA" ~ ANO4-1,
                          TRUE~ANO4)) %>%
  rename(COD.ENCUESTAS = Pais) %>% 
  left_join(PPA) %>% 
  mutate(ingreso.mensual.ppa = ingreso.mensual.prom/PPA) %>% 
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

salarios.prod <- Salarios %>% 
  left_join(Productividad)

####Salarios y productividad agregada#####
ggplot(salarios.prod,
       aes(x=`2018`,
           y=`Prod.relativa.a.EEUU.-.TOTAL`,
           label = nombre.pais))+
  geom_smooth(method = 'lm',color = "grey70",se = FALSE)  +
  geom_point(aes(color = Color))+
  geom_text_repel(aes(color = Color))+  
  labs(title = "Salarios promedio y productividad relativa (USA = 100). Año 2018",
       x = "Salario promedio en PPA",
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

ggsave("Resultados/productividad_salarios.png",scale = 2)



####Distrib.Calificaciones#####
# calif.graf<- Calificacion.todos %>% 
#   mutate(tamanio.calif = paste0(grupos.tamanio," - ",grupos.calif),
#          tamanio.calif = factor(tamanio.calif,
#                                 levels = 
#                                   c("Pequeño - Baja",
#                                     "Pequeño - Media",
#                                     "Pequeño - Alta",
#                                     "Mediano - Baja",
#                                     "Mediano - Media", 
#                                     "Mediano - Alta",
#                                     "Grande - Baja",
#                                     "Grande - Media",
#                                     "Grande - Alta")))

EUROPA_USA_ARG %>% 
  filter((ANO4 == 2018 & !(Pais %in% c("DE","ES")))|
         ANO4 == 2017 & Pais %in% c("DE","ES")) %>% 
  ggplot(.,
         aes(x = Pais, y = particip.ocup,
             fill = tamanio.calif,group = tamanio.calif,
             label = scales::percent(particip.ocup))) +
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5),size=2.5)+
  labs(title = "Distribución del empleo según grupos")+
  theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45),
        #panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta)

ggsave("Resultados/grupos_calificacion_tamanio.png",width = 10,height = 8)


####Distrib.de. cada.grupo#####
Particip.calif <- EUROPA_USA_ARG %>% 
  ungroup() %>% 
  mutate(grupos.calif = factor(grupos.calif,
                               levels = c("Baja","Media","Alta"))) %>% 
  group_by(grupos.calif,Pais,ANO4) %>% 
  summarise(Particip_emp = sum(particip.ocup))

####Desocupados por calificacion#####
desocup.arg <- desocup.calif.ant.arg %>% 
  group_by(ANO4, grupos.calif = grupos.calif.desocup) %>% 
  summarise(Pais = "ARG",
            particip.desocup = mean(distribucion)) 

desocup.usa <- desocup.calif.ant.usa %>% 
  mutate(Pais = "USA") %>% 
  select(-desocupados) %>% 
  rename(ANO4 =YEAR,particip.desocup = distribucion)

desocup.europa <- Resultados.Calif.Ant %>% 
  select(ANO4,
         Pais,
         grupos.calif = CALIF,
         particip.desocup = Por.Desocup,
         Particip_emp = Por.Ocup) 

desocup.calif <- desocup.arg %>% 
  bind_rows(desocup.usa)%>%
  left_join(Particip.calif) %>% 
  bind_rows(desocup.europa)%>% 
  mutate(Brecha = Particip_emp - particip.desocup) %>% 
  pivot_longer(cols = c(4,5),names_to = "distrib",values_to = "Valor") %>% 
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
  facet_wrap(~Pais,scales = "free_x")


ggsave("Resultados/desocupacion anterior.png",scale = 2)

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
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta)+
  facet_wrap(~COD.ENCUESTAS)

ggsave("Resultados/deciles.png",scale = 3)

########Salarios###########
ingresos.todos %>% 
  filter((ANO4 == 2014 & !(COD.ENCUESTAS %in% c("ES")))|
           COD.ENCUESTAS %in% c("ES")) %>% 
  filter(COD.ENCUESTAS != "SE") %>% 
  ggplot(.,
         aes(x = COD.ENCUESTAS, y = ingreso.mensual.ppa,
             fill = tamanio.calif,group = tamanio.calif,
             label = round(ingreso.mensual.ppa,2))) +
  geom_col(position = "dodge")+
  #geom_text(position = position_dodge(),size=2.5,angle = 90)+
  labs(title = "Ingreso promedio en PPA según perfiles. Año 2014")+
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
  facet_wrap(~tamanio.calif)

ggsave("Resultados/salariosPPA.png",scale = 2)


ingresos.todos %>% 
  filter((ANO4 == 2018 & !(COD.ENCUESTAS %in% c("DE","ES")))|
           ANO4 == 2017 & COD.ENCUESTAS %in% c("DE","ES")) %>% 
  filter(COD.ENCUESTAS != "SE") %>% 
  ggplot(.,
         aes(x = tamanio.calif, y = ingreso.mensual.prom,
             fill = tamanio.calif,group = tamanio.calif,
             label = round(ingreso.mensual.ppa,2))) +
  geom_col(position = "dodge")+
  #geom_text(position = position_dodge(),size=2.5,angle = 90)+
  labs(title = "Ingreso promedio en moneda nacional según perfiles. Año 2018")+
  theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        axis.title = element_blank(),
        #axis.text.x = element_text(angle = 90),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta)+
  facet_wrap(~COD.ENCUESTAS,scales = "free_y")

ggsave("Resultados/salarios_moneda_nac.png",scale = 2)

#####Part Time Involuntario#####
calif.graf %>% 
  filter((ANO4 == 2018 & !(Pais %in% c("DE","ES")))|
           ANO4 == 2017 & Pais %in% c("DE","ES")) %>% 
  ggplot(.,
         aes(x = tamanio.calif, y = tasa.part.invol,
             fill = tamanio.calif,group = tamanio.calif,
             label = scales::percent(tasa.part.invol))) +
  geom_col(position = "dodge")+
  geom_text(position = position_dodge(),size=2.5)+
  labs(title = "Tasa de part time involuntario según grupos. Año 2018")+
  theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
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
  facet_wrap(~Pais)


#########PRECAIREDAD ASALARIADOS########
EUROPA_USA_ARG %>% 
  filter((ANO4 == 2018 & !(Pais %in% c("DE","ES")))|
           ANO4 == 2017 & Pais %in% c("DE","ES")) %>% 
  ggplot(.,
         aes(x = tamanio.calif, y = tasa.1.asalariados,
             fill = tamanio.calif,group = tamanio.calif,
             label = scales::percent(tasa.1.asalariados))) +
  geom_col(position = "dodge")+
  geom_text(position = position_dodge(),size=2.5)+
  labs(title = "Tasa de precariedad. Año 2018",
       subtitle = "Una o más expresiones de precariedad. Total Asalariados")+
  theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
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
  facet_wrap(~Pais)
ggsave("Resultados/Precariedad asalariados.png",scale = 2.5)



###Evolucion Part Time####
EUROPA_USA_ARG %>% 
  filter(Pais != "SE",ANO4<=2018) %>% 
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
  facet_wrap(~Pais,scales = "free_y")

ggsave("Resultados/evol_precariedad.png",scale = 2)

####Argentina Precariedad Asal####
names(argentina.precariedad.asal)
graf.argentina <- argentina.precariedad.asal %>% 
  filter(ANO4<=2018) %>% 
  pivot_longer(cols = 5:ncol(.),
               names_to = "indicador",values_to = "valor") %>% 
  mutate(
    indicador = factor(
      case_when(
      indicador == "tasa.1.asalariados" ~ "1 o más expresiones",
      indicador == "tasa.2.asalariados" ~ "2 o más expresiones",
      indicador == "tasa.3.asalariados" ~ "3 expresiones",
      indicador == "tasa.s.desc.jubil" ~ "Sin descuento jubilatorio",
      indicador == "tasa.empleo.temporal" ~ "Empleo temporal",
      indicador == "tasa.part.invol" ~ "Part time involuntario"),
      levels = c("Sin descuento jubilatorio",
                 "Empleo temporal",
                 "Part time involuntario",
                 "1 o más expresiones",
                 "2 o más expresiones",
                 "3 expresiones")),
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

ggsave("Resultados/evol_precariedad_arg.png",scale = 2)
