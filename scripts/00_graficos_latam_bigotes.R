options(scipen=999)
library(ggplot2)
library(dplyr)

#### Grafico de bigotes ####

data.graf.PPA <- read.xlsx("Resultados/America/Cuadros/salarios_encuestas_ppa.xlsx")

data.graf.bigotes <- data.graf.PPA %>% 
  select(Pais, tamanio.calif, salario.PPA) %>% 
  group_by(Pais) %>% 
  summarise(max.ppa = max(salario.PPA), 
            min.ppa = min(salario.PPA)) %>% 
  ungroup () %>% 
  gather(., max.min, salario.ppa, -Pais) %>% 
  filter(Pais!="Canadá")                      #Saco Canadá porque da muy alto
  
ggplot(data.graf.bigotes, 
       aes(x= fct_reorder(as.factor(Pais), salario.ppa), 
           y = salario.ppa)) +
  geom_point()+
  labs(title = "Promedios mínimos y máximos de salario PPA por perfil. Año 2019")+
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
        panel.grid.major.x = element_line(colour = "grey"))

