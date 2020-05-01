library(openxlsx)
library(tidyverse)
library(ggrepel)
library(ggthemes)
Salarios <- read.xlsx("data/Prod y Salarios.xlsx",sheet = 1)
Productividad <- read.xlsx("data/Prod y Salarios.xlsx",sheet = 2,startRow = 2)
Calificacion.arg.usa <- read.xlsx("Resultados/Arg_USA_2018.xlsx",sheet = "Ocupados.calif")
Educacion.arg.usa <- read.xlsx("Resultados/Arg_USA_2018.xlsx",sheet = "Ocupados.nivel.ed")
Calificacion.europa <- read.xlsx("Resultados/RestultadosLFS.xlsx",
                                 sheet = "Ocupados.calif")
Educacion.europa <- read.xlsx("Resultados/RestultadosLFS.xlsx",
                              sheet = "Ocupados.nivel.ed")

Calificacion.todos <- Calificacion.arg.usa %>% 
  select(grupos.calif,grupos.tamanio,Particip_emp,Pais) %>% 
  bind_rows(Calificacion.europa %>% 
  select(grupos.calif,grupos.tamanio,Particip_emp,Pais))


Educacion.todos <- Educacion.arg.usa %>% 
  select(grupos.nivel.ed,grupos.tamanio,Particip_emp,Pais) %>% 
  bind_rows(Educacion.europa %>% 
            select(grupos.nivel.ed,grupos.tamanio,Particip_emp,Pais))


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
                                levels = unique(tamanio.calif)))


calif.graf %>% 
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
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta)

ggsave("Resultados/grupos_calificacion_tamanio.png",scale = 2)


####Distrib.Educacion#####
educ.graf<- Educacion.todos %>% 
  mutate(tamanio.educ = paste0(grupos.tamanio," - ",grupos.nivel.ed),
         tamanio.educ = factor(tamanio.educ,
                                levels = unique(tamanio.educ)))


educ.graf %>% 
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

ggsave("Resultados/grupos_educacion_tamanio.png",scale = 2)

####Distrib.de. cada.grupo#####
Particip.tamanio <- Educacion.todos %>%
  mutate(grupos.tamanio = factor(grupos.tamanio,
                                 levels = unique(grupos.tamanio))) %>% 
  group_by(grupos.tamanio,Pais) %>% 
  summarise(Particip_emp = sum(Particip_emp))


Particip.tamanio %>% 
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
  group_by(grupos.nivel.ed,Pais) %>% 
  summarise(Particip_emp = sum(Particip_emp))

Particip.nivel %>% 
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

ggsave("Resultados/particip_nivel.png",scale = 2)


Particip.calif <- Calificacion.todos %>% 
  mutate(grupos.calif = factor(grupos.calif,
                                 levels = unique(grupos.calif))) %>% 
  group_by(grupos.calif,Pais) %>% 
  summarise(Particip_emp = sum(Particip_emp))

Particip.calif %>% 
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
