#####Librerias#####
library(openxlsx)
library(tidyverse)
library(ggrepel)
library(ggthemes)
#####Carga de datos#####
#Salarios y productividad#
Salarios <- read.xlsx("data/Prod y Salarios.xlsx",sheet = 1)
Productividad <- read.xlsx("data/Prod y Salarios.xlsx",
                           sheet = 2,startRow = 2)

##Arg y USA#
load("Resultados/ARG_USA.RDATA")
##Europa#
Calificacion.europa <- read.xlsx("Resultados/RestultadosLFS 15.5.xlsx",
                                 sheet = "Ocupados.calif") %>% 
  mutate(ANO4 = as.numeric(ANO4))
Educacion.europa <- read.xlsx("Resultados/RestultadosLFS 15.5.xlsx",
                              sheet = "Ocupados.nivel.ed")%>% 
  mutate(ANO4 = as.numeric(ANO4))
asal.Calificacion.europa <- read.xlsx("Resultados/RestultadosLFS 15.5.xlsx",
                                 sheet = "Asalariados.nivel.ed") %>% 
  mutate(ANO4 = as.numeric(ANO4))

desoc.Calificacion.europa <- read.xlsx("Resultados/RestultadosLFS 15.5.xlsx",
                                      sheet = "Desocup.Calif") 





Calificacion.todos <- indicadores.anuales.ocupados.calif %>% 
  select(grupos.calif,grupos.tamanio,Particip_emp,Pais,ANO4,everything()) %>% 
  bind_rows(Calificacion.europa %>% 
  select(grupos.calif,grupos.tamanio,Particip_emp,Pais,ANO4,everything())) %>% 
  arrange(desc(grupos.tamanio),grupos.calif)
  
Educacion.todos <- indicadores.anuales.ocupados.nivel.ed %>% 
  select(grupos.nivel.ed,grupos.tamanio,Particip_emp,Pais,ANO4) %>% 
  bind_rows(Educacion.europa %>% 
            select(grupos.nivel.ed,grupos.tamanio,Particip_emp,Pais,ANO4)) %>% 
  arrange(grupos.tamanio,grupos.nivel.ed)



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
           label = X1,
           color = Color))+
  geom_point()+
  geom_text_repel()+  
  labs(title = "Salarios promedio y productividad relativa (USA = 100). Año 2018",
       x = "Salario promedio en PPA (Ver año)",
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
calif.graf<- Calificacion.todos %>% 
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

calif.graf %>% 
  filter(ANO4 == 2018|Pais %in% c("DE","ES")) %>% 
  ggplot(.,
         aes(x = Pais, y = Particip_emp,
             fill = tamanio.calif,group = tamanio.calif,
             label = scales::percent(Particip_emp))) +
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


####Distrib.Educacion#####
educ.graf<- Educacion.todos %>% 
  mutate(tamanio.educ = paste0(grupos.tamanio," - ",grupos.nivel.ed),
         tamanio.educ = factor(tamanio.educ,
                                levels = unique(tamanio.educ)))


educ.graf %>% 
  filter(ANO4 == 2018|Pais %in% c("DE","ES")) %>% 
  ggplot(.,
         aes(x = Pais, y = Particip_emp,
             fill = tamanio.educ,group = tamanio.educ,
             label = scales::percent(Particip_emp))) +
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5),size=2.5)+
  labs(title = "Distribución del empleo según grupos")+
  theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta)

#ggsave("Resultados/grupos_educacion_tamanio.png",scale = 2)

####Distrib.de. cada.grupo#####
Particip.tamanio <- Educacion.todos %>%
  ungroup() %>% 
  mutate(grupos.tamanio = factor(grupos.tamanio,
                                 levels = c("Pequeño",
                                            "Mediano",
                                            "Grande"))) %>% 
  group_by(grupos.tamanio,Pais,ANO4) %>% 
  summarise(Particip_emp = sum(Particip_emp))


Particip.tamanio %>% 
  filter(ANO4 == 2018|Pais %in% c("DE","ES")) %>% 
  ggplot(.,
         aes(x = Pais, y = Particip_emp,
             fill = grupos.tamanio,group = grupos.tamanio,
             label = scales::percent(Particip_emp))) +
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5),size=2.5)+
  labs(title = "Distribución del empleo según grupos")+
  theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta3)

ggsave("Resultados/particip_tamanio.png",scale = 2)


Particip.nivel <- Educacion.todos %>% 
  group_by(grupos.nivel.ed,Pais,ANO4) %>% 
  summarise(Particip_emp = sum(Particip_emp))

Particip.nivel %>% 
  filter(ANO4 == 2018|Pais %in% c("DE","ES")) %>% 
  ggplot(.,
         aes(x = Pais, y = Particip_emp,
             fill = grupos.nivel.ed,group = grupos.nivel.ed,
             label = scales::percent(Particip_emp))) +
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5),size=2.5)+
  labs(title = "Distribución del empleo según grupos")+
  theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta3)

#ggsave("Resultados/particip_nivel.png",scale = 2)


Particip.calif <- Calificacion.todos %>% 
  ungroup() %>% 
  mutate(grupos.calif = factor(grupos.calif,
                               levels = c("Baja","Media","Alta"))) %>% 
  group_by(grupos.calif,Pais,ANO4) %>% 
  summarise(Particip_emp = sum(Particip_emp))

Particip.calif %>% 
  filter(ANO4 == 2018|Pais %in% c("DE","ES")) %>% 
  ggplot(.,
         aes(x = Pais, y = Particip_emp,
             fill = grupos.calif,group = grupos.calif,
             label = scales::percent(Particip_emp))) +
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5),size=2.5)+
  labs(title = "Distribución del empleo según grupos")+
  theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 45),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta3)

ggsave("Resultados/particip_calif.png",scale = 2)

####Desocupados por calificacion#####
desocup.arg <- desocup.calif.ant.arg %>% 
  group_by(ANO4, grupos.calif = grupos.calif.desocup) %>% 
  summarise(Pais = "ARG",
            particip.desocup = mean(distribucion)) 

desocup.usa <- desocup.calif.ant.usa %>% 
  mutate(Pais = "USA") %>% 
  select(-desocupados) %>% 
  rename(ANO4 =YEAR,particip.desocup = distribucion)

A <- desoc.Calificacion.europa

desocup.calif <- desocup.arg %>% 
  bind_rows(desocup.usa)%>%
  left_join(Particip.calif) %>% 
  pivot_longer(cols = c(4,5),names_to = "distrib",values_to = "Valor") %>% 
  mutate(grupos.calif = factor(grupos.calif,
                               levels = c("Baja","Media","Alta"))) 
  

desocup.calif %>% 
  filter(ANO4 == 2018|Pais %in% c("DE","ES")) %>%  
ggplot(.,
       aes(x=distrib,
           y=Valor,
           label = scales::percent(Valor),
           fill = grupos.calif,
           group = grupos.calif))+
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5),size=2.5)+
  labs(title = "Distribución de desocupados con empleo anterior y participación en el empleo según calificación")+
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
  facet_wrap(~Pais)


ggsave("Resultados/productividad_salarios.png",scale = 2)

####Deciles#####
ingresos.todos <- bind_rows(ingresos.eph.asalariados.calif,
                            ingresos.asec.asalariados.calif,
                            asal.Calificacion.europa %>% 
                              rename(decil.m.promedio = PromedioDecil))%>% 
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


ingresos.todos %>% 
  filter((ANO4 == 2018 & !(Pais %in% c("DE","ES")))|
          ANO4 == 2017 & Pais %in% c("DE","ES")) %>% 
  filter(Pais != "SE") %>% 
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
  facet_wrap(~Pais)

ggsave("Resultados/deciles.png",scale = 3)
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


####PRUEBA DE CLUSTERING#####
set.seed(101)
indic.clusters <- EPH.ingresos.deciles %>%
  filter(grupos.tamanio != "Ns/Nr",grupos.calif  %in% c("Alta","Media","Baja")) %>% 
  filter(TRIMESTRE == 4,ANO4 == 2018) %>% 
  arrange(grupos.tamanio,grupos.calif) %>% 
  mutate(tamanio.calif = paste0(grupos.tamanio," - ",grupos.calif),
         tamanio.calif = factor(tamanio.calif,
                                levels = unique(tamanio.calif))) %>% 
  ungroup() %>% 
  mutate(ingreso.mensual.escalado = scale(ingreso.mensual),
         horas.trabajadas.escaladas=scale(PP3E_TOT)) %>% 
  filter(!is.na(horas.trabajadas.escaladas))

sector_clusters.norm <- kmeans(
  x = indic.clusters[, c("ingreso.mensual.escalado",
                         "horas.trabajadas.escaladas")],
  centers = 3, nstart = 50)

indic.clusters <- indic.clusters %>%
  ungroup() %>% 
  mutate(cluster_norm = sector_clusters.norm$cluster,
         cluster_norm = as.factor(cluster_norm))

options(scipen = 999)
cluster.graf <- ggplot(indic.clusters,
                       aes(x=ingreso.mensual,
                           y=PP3E_TOT,
                           shape = cluster_norm,
                           color = tamanio.calif))+
  geom_point(size = 2)+
  labs(title = "Clasificación de clusers según ingreso y horas trabajadas. 4t 2018",
       x = "Ingreso Ocup ppal",
       y = "Horas trabajadas" )+
  theme_tufte()+
  theme(panel.spacing = unit(1,"cm"),
        legend.position = "left",
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.minor.y= element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey30"),
        panel.grid.major.y = element_line(colour = "grey30"),
        text = element_text(size = 15))+
  scale_fill_grey()+
  scale_x_continuous(limits = c(0,100000))+
  scale_color_manual(values = paleta)

ggsave("Resultados/clustering.png",plot = cluster.graf,scale = 2)


