library(openxlsx)
library(tidyverse)
library(ggthemes)

#Cargo datos finales a mano en precariedad.mundial ####
america <- readRDS(file = "Resultados/America.RDS")    #guido la llama perfiles y le suma el agregado
load("~/GitHub/precariedad.mundial/Resultados/EUROPA.RDATA")
europa <- Resultados_bind
rm(list = ls()[!ls() %in% c("europa", "america")])
Paises <- read.xlsx("Fuentes Complementarias/Prod y Salarios.xlsx",
                    sheet = "Paises_Latam") 

# Preparo bases
america <- america %>% 
            mutate(
            grupos.calif = factor(grupos.calif,
                                levels = c("Alta","Media","Baja")),
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
                                     "Grande - Alta",
                                     "Total"))) %>% 
            arrange(Pais,tamanio.calif) %>% 
            mutate(tasa.cp = 1- tasa.asalarizacion,
                 ing.tcp.ing.asal = promedio.ing.oc.prin.tcp/promedio.ing.oc.prin.asal) %>% 
            filter(Pais != "Canada") %>% 
  select(Pais, tamanio.calif, particip.ocup)

europa <- europa %>% 
  filter(ANO4==2018) %>% 
  mutate(tamanio.calif=paste0(grupos.tamanio, " - ", grupos.calif)) %>% 
  mutate(grupos.calif = factor(grupos.calif,
                          levels = c("Alta","Media","Baja")),
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
                               "Grande - Alta",
                               "Total"))) %>% 
          arrange(pais,tamanio.calif) %>% 
  select(pais, tamanio.calif, particip.ocup)

#Preparacion para hacer los graficos####

Paises$nombre.pais[Paises$nombre.pais== "México"]<- "Mexico"

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


#Orden de aparicion
orden <- Paises %>% select(nombre.pais, Orden)
america <- america %>% left_join(orden, by=c("Pais" = "nombre.pais")) 

nombres <- Paises %>% select(nombre.pais, COD.ENCUESTAS)
europa <- europa %>% left_join(nombres, by=c("pais" = "COD.ENCUESTAS"))
europa <- europa %>% left_join(orden, by=c("nombre.pais" = "nombre.pais"))
europa <- europa %>% rename('Pais'='nombre.pais') %>% select(-pais)


base <- bind_rows(america, europa)


# Grafico ####


base %>% 
  filter(tamanio.calif!= "Total",Pais!= "Canada") %>% 
  ggplot(.,
         aes(x = reorder(Pais,Orden), y = particip.ocup,
          #aes(x = Pais, y = particip.ocup,
             fill = tamanio.calif,
             group = tamanio.calif,
             label = scales::percent(particip.ocup,decimal.mark = ",",
                                     accuracy = 0.1))) +
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5),size=3)+
  #  labs(title = "Distribución del empleo según grupos")+
  theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 14),
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
  guides(fill=guide_legend(title="Tamaño - Calificación"))