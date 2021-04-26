#####Librerias#####
library(openxlsx)
library(tidyverse)
library(stringr)
library(ggrepel)
library(ggthemes)

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
###############################Data Encuestas#########################
load("Precariedad.app/datashiny.RDATA")

periodos <- unique(tabla[c("Pais","periodo")])

###############################Fuentes complementarias#########################
Paises <- read.xlsx("Fuentes Complementarias/Prod y Salarios.xlsx",
                    sheet = "Paises_Latam")

Productividad <- read.xlsx("Fuentes Complementarias/Prod y Salarios.xlsx",
                           sheet = "Prod Relativa_Total e IND",
                           startRow = 2) %>% 
  rename(COD.OCDE = LOCATION, name = label.x) 

Salarios_UMN <- read.xlsx("Fuentes Complementarias/Prod y Salarios.xlsx",
                          sheet = "salario nominal - UMN"
                          ) %>%
  rename(ANO4 = X1)%>%
  pivot_longer(cols = 2:ncol(.),
               names_to = "nombre.pais",
               values_to = "Salario.UMN") %>%
  mutate(nombre.pais = str_replace_all(nombre.pais,"[[:punct:] ]+",replacement = " "))
 
IPC_2005 <- read.xlsx("Fuentes Complementarias/Prod y Salarios2.xlsx",
                          sheet = "IPC (2005)") %>%
  rename(ANO4 = X1)%>%
  pivot_longer(cols = 2:ncol(.),
               names_to = "nombre.pais",
               values_to = "IPC_2005") %>%
  mutate(nombre.pais = str_replace_all(nombre.pais,"[[:punct:] ]+",replacement = " "))

PPA_WB <- read.csv("Fuentes Complementarias/PPA.csv") %>%
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



####Salarios Y productividad####
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
  labs(x = "Salario Relativo (EEUU = 100)",
       #title = "Salarios promedio y productividad relativa (EEUU = 100). Año 2017",
       y = "Productividad Relativa (EEUU = 100)" )+
  theme_tufte()+
  theme(panel.spacing = unit(1,"cm"),
        legend.position = "none",
        panel.grid.minor.x = element_line(colour = "grey95"),
        panel.grid.minor.y= element_line(colour = "grey95"),
        panel.grid.major.x = element_line(colour = "grey80"),
       panel.grid.major.y = element_line(colour = "grey80"),
        text = element_text(size = 13))+
  scale_fill_grey()

ggsave("Resultados/America/productividad_salarios_latam.jpg",
       width =9,height = 7)
# write.xlsx(list("salarios.prod.2017" = salarios.prod.2017),
#            file = "Resultados/Salarios_Productividad2017_CCNN.xlsx")  


####PERFILES####
options(scipen = 999)
America <- tabla %>% 
  mutate(
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
    tamanio.calif2 = case_when(
      tamanio.calif == "Pequeño - Baja" ~ "1) Pequeño - Baja",
      tamanio.calif == "Pequeño - Media" ~ "2) Pequeño - Media",
      tamanio.calif == "Pequeño - Alta" ~ "3) Pequeño - Alta",
      tamanio.calif == "Mediano - Baja" ~ "4) Mediano - Baja",
      tamanio.calif == "Mediano - Media" ~ "5) Mediano - Media",
      tamanio.calif == "Mediano - Alta" ~ "6) Mediano - Alta",
      tamanio.calif == "Grande - Baja" ~ "7) Grande - Baja",
      tamanio.calif == "Grande - Media" ~ "8) Grande - Media",
      tamanio.calif == "Grande - Alta" ~ "9) Grande - Alta")) %>% 
  arrange(Pais,tamanio.calif)



America %>% 
  ggplot(.,
         aes(x = Pais, y = particip.ocup,
             fill = tamanio.calif,group = tamanio.calif,
             label = scales::percent(particip.ocup,accuracy = 0.01))) +
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

write.xlsx(America,"Resultados/America/Resultados_America.xlsx")
ggsave("Resultados/America/America_calificacion_tamanio.jpg",width = 10,height = 8)


####Tasas precariedad####
America %>%
filter(Pais != "Estados Unidos") %>% 
  ggplot(.,
         aes(x = Pais, y = tasa.seguridad.social,
             label = round(tasa.seguridad.social,2))) +
  geom_col(position = "dodge")+
  #geom_text(position = position_dodge(),size=2.5,angle = 90)+
  labs(title = "Tasa de ausencia de cobertura en la seguridad social")+
  theme_tufte()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.key.size = unit(0.2,"cm"),
        #legend.spacing.x  = unit(0.4,"cm"),
        legend.title = element_blank(),
        #legend.text = element_text(size = 17),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 90,vjust = .5),
        axis.ticks.x = element_blank(),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"),
        text = element_text(size = 17))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = "blue")+
  facet_wrap(~tamanio.calif)+
  guides(fill=guide_legend(keywidth = 0.8))

ggsave("Resultados/America/tasas seguridad social.jpg",width = 15.59,height = 8)

graf.todos %>% 
  filter(indicador == "Empleo temporal") %>% 
  ggplot(.,
         aes(x = Pais, y = valor,
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
        #legend.text = element_text(size = 17),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 90,vjust = .5),
        axis.ticks.x = element_blank(),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"),
        text = element_text(size = 17))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = "blue")+
  facet_wrap(~tamanio.calif)+
  guides(fill=guide_legend(keywidth = 0.8))

ggsave("Resultados/America/Empleo temporal.jpg",width = 15.59,height = 8)


#########1 o mas precariedad########
EUROPA_USA_ARG %>% 
  left_join(Paises %>% rename(Pais = COD.ENCUESTAS)) %>% 
  filter((ANO4 == 2018 & !(Pais %in% c("DE")))|
           ANO4 == 2017 & Pais %in% c("DE")) %>% 
  ggplot(.,
         aes(x = tamanio.calif, y = tasa.1.asalariados,
             fill = tamanio.calif,group = tamanio.calif,
             label = scales::percent(tasa.1.asalariados,accuracy = 1))) +
  geom_col(position = "dodge")+
  geom_text(position = position_dodge(),size=4)+
  # labs(title = "Tasa de precariedad. Año 2018",
  #      subtitle = "Una o más expresiones de precariedad. Total Asalariados")+
  theme_tufte()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        #legend.title  = element_text(size = 8),
        #legend.text  = element_text(size = 8),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 16),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta)+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~reorder(COD.ESPANIOL,desc(Orden)))+
  #guides(fill=guide_legend(title="Tamaño - Calificación"))
  guides(fill=guide_legend(title=str_wrap("Tamaño - Calificación",10),
                           nrow = 1))

ggsave("Resultados/Precariedad asalariados.jpg",width = 15.59,height = 9)



###########INGRESOS##################
America %>%
 ggplot(.,
         aes(x = tamanio.calif, y = prima.salario.medio,
             fill = tamanio.calif,group = tamanio.calif)) +
  geom_col(position = "dodge")+
  geom_hline(mapping = aes(yintercept = 1),size = 1)+
 # labs(title = "Promedio de deciles de pertenencia de los asalariados según grupos. Año 2018")+
  theme_tufte()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        #legend.title  = element_text(size = 8),
        #legend.text  = element_text(size = 8),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 16),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta)+
  facet_wrap(~Pais)

ggsave("Resultados/America/Ingreso Asalariados.png",width = 15.59,height = 9)

########Salarios perfiles###########
 data.graf.PPA <- America %>% 
  select(Pais,tamanio.calif,periodo,promedio.ing.oc.prin) %>% 
  left_join(Paises %>% rename(Pais = nombre.pais)) %>%
  left_join(Estimacion_GW %>% select(periodo = ANO4,
                                     Pais = nombre.pais,
                                     PPA.BENCHMARK.2017.EXTRAPOLADO))  
  #   #group_by(tamanio.calif) %>% 
#   mutate(ing.perfil9.usa.100 = 100*ingreso.mensual.ppa.gw/
#       ingreso.mensual.ppa.gw[COD.ENCUESTAS=="USA" & tamanio.calif=="Grande - Alta"]
#   ) %>% 
#   select(ANO4,nombre.pais,COD.ESPANIOL,Orden,tamanio.calif,
#          ingreso.mensual.ppa.gw,ing.perfil9.usa.100)
# 
# #   ggplot(data.graf.PPA,
# #          aes(x = COD.ESPANIOL, y = ing.usa.calif.100,
# #              fill = tamanio.calif,group = tamanio.calif,
# #              label = round(ing.usa.calif.100,1))) +
# #   geom_col(position = "dodge")+
# #   geom_text(position = position_dodge(),size=2.5)+
# #   labs(title = "Salario relativo en PPA. Año 2014. Perfil 9 de USA=100")+
# #   theme_tufte()+
# #   theme(legend.position = "none",
# #         legend.direction = "vertical",
# #         axis.title = element_blank(),
# #         axis.text.x = element_text(angle = 90),
# #         axis.ticks.x = element_blank(),
# #         panel.spacing = unit(1,"cm"),
# #         panel.grid.major.y = element_line(colour = "grey"),
# #         panel.grid.minor.y = element_line(colour = "grey30"),
# #         panel.grid.minor.x = element_line(colour = "grey"),
# #         panel.grid.major.x = element_line(colour = "grey"))+
# #   scale_fill_manual(values = paleta)+
# #   scale_y_continuous(limits = c(0,101),breaks = c(20,40,60,80,100))+
# #   facet_wrap(~tamanio.calif)
# #   
# # ggsave("Resultados/PPA USA 100.png",scale = 2)
#   
# 
# ggplot(data.graf.PPA,
#        aes(x = reorder(COD.ESPANIOL,desc(Orden)), y = ing.perfil9.usa.100,
#            fill = tamanio.calif,group = tamanio.calif,
#            label = round(ing.perfil9.usa.100,1))) +
#   geom_col(position = "dodge")+
#   geom_text(position = position_dodge(),size=4)+
#   #labs(title = "Salario relativo en PPA. Año 2014. Perfil 9 de USA=100")+
#   theme_tufte()+
#   theme(legend.position = "none",
#         legend.direction = "vertical",
#         axis.title = element_blank(),
#         #axis.text.x = element_text(angle = 90,size = 16,face = "bold"),
#         axis.text.x = element_text(angle = 90,size = 18),
#         axis.ticks.x = element_blank(),
#         panel.spacing = unit(1,"cm"),
#         text = element_text(size = 17),
#         panel.grid.major.y = element_line(colour = "grey"),
#         panel.grid.minor.y = element_line(colour = "grey30"),
#         panel.grid.minor.x = element_line(colour = "grey"),
#         panel.grid.major.x = element_line(colour = "grey"))+
#   scale_fill_manual(values = paleta)+
#   scale_y_continuous(limits = c(0,101),breaks = c(20,40,60,80,100))+
#   facet_wrap(~tamanio.calif)
# 
# ggsave("Resultados/PPA BENCHMARK 2017_USA 100.jpg",width = 15.59,height = 9)
# 
# 
# 
# 
# 
# 
# cuadro_ingresos <- ingresos.todos %>% 
#    select(ANO4,COD.ENCUESTAS,nombre.pais,COD.ESPANIOL,grupos.calif,grupos.tamanio,decil.m.promedio,
#          ingreso.mensual.prom,PPA.BENCHMARK.2017,Ingreso.mensual.PPA = ingreso.mensual.ppa.gw)
# 
# 
# write.xlsx(list("Grafico 9" = data.graf.PPA,
#                 "Ingresos Deciles Evol" = cuadro_ingresos),
#            file = "Resultados/Salarios Encuestas PPA.xlsx")  
# 
# 
# ####Animaciones#####
# library(gganimate)
# library(gifski)
# 
# anim <- EUROPA_USA_ARG %>% 
#   mutate(ANO4 = as.numeric(round(ANO4))) %>% 
#   left_join(Paises %>% rename(Pais = COD.ENCUESTAS)) %>% 
#   # filter((ANO4 == 2018 & !(Pais %in% c("DE")))|
#   #          ANO4 == 2017 & Pais %in% c("DE")) %>% 
#   ggplot(.,
#          aes(x = COD.ESPANIOL, y = particip.ocup,
#              fill = tamanio.calif,group = tamanio.calif,
#              label = scales::percent(particip.ocup))) +
#   geom_col(position = "stack")+
#   geom_text(position = position_stack(vjust = .5),size=3)+
#   #  labs(title = "Distribución del empleo según grupos")+
#   theme_tufte()+
#   theme(legend.position = "bottom",
#         legend.direction = "horizontal",
#         legend.title = element_blank(),
#         axis.title = element_blank(),
#         text = element_text(size = 15),
#         axis.text.x = element_text(angle = 45),
#         #panel.spacing = unit(1,"cm"),
#         panel.grid.major.y = element_line(colour = "grey"),
#         panel.grid.minor.y = element_line(colour = "grey30"),
#         panel.grid.minor.x = element_line(colour = "grey"),
#         panel.grid.major.x = element_line(colour = "grey"))+
#   scale_fill_manual(values = paleta)+
#   scale_y_continuous(labels = scales::percent)+
#   labs(title = 'Year: {round(frame_time)}') +
#   transition_time(ANO4)  +
#   ease_aes('linear')
# 
# a <- animate(anim, nframes = 12, fps = 1,width = 1000, renderer = gifski_renderer())
# anim_save("Resultados/Perfiles.gif",animation = a)
# 
# salarios.prod.anim <- Salarios_UMN %>% 
#    left_join(Paises) %>% 
#    left_join(PPA)  %>% 
#    filter(!is.na(Salario.UMN),Salario.UMN!= 0,
#           !is.na(PPA),PPA != 0) %>% 
#    mutate(salario.PPP = Salario.UMN/PPA) %>% 
#    group_by(ANO4) %>% 
#    mutate(Salario_USA_100 = salario.PPP/salario.PPP[nombre.pais=="Estados Unidos"]*100,
#           rank = min_rank(-Salario_USA_100) * 1) %>%
#    ungroup()
# 
# 
# 
# anim2 <- salarios.prod.anim %>% 
#   filter(ANO4 %in% 2015:2016) %>% 
#   ungroup() %>% 
#   mutate(ANO4 = as.numeric(round(ANO4))) %>% 
#   ggplot(.,
#          aes(x = desc(rank),
#              y = Salario_USA_100,
#              fill = nombre.pais,group = nombre.pais,
#              label = round(Salario_USA_100,1))) +
#   geom_col(position = "dodge")+
#   geom_text(aes(y = 0, label = paste(nombre.pais, "")), vjust = 0.2, hjust = 1) +
#   labs(title = "Evolución del salario relativo en Paridad de Poder Adquisitivo",
#        subtitle = "Estados Unidos  = 100")+
#   theme_minimal()+
#   theme(
#     axis.ticks.y = element_blank(), 
#     axis.text.y = element_blank(), 
#     axis.title.x = element_blank(), 
#     axis.title.y = element_blank(), 
#     plot.margin = margin(1,1,1,4, "cm"),
#     legend.position = "none")+
#   coord_flip(clip = "off", expand = TRUE) +
#   labs(title = 'Year: {round(frame_time)}') +
#   transition_time(ANO4)  +
#   ease_aes("cubic-in-out") +
#   view_follow()# permitir que los ejes cambien
# 
#   #transition_states(states = ANO4, transition_length = 4, state_length = 1) + 
#   #labs(title = 'Year: {round(closest_state)}')
#   
# 
#  b<- animate(anim2, nframes = 2, fps = 1,duration = 3,
#          width = 1000, renderer = gifski_renderer())
# anim_save("Resultados/Salario_Nacional.gif",animation = b)
