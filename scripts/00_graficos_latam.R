rm(list = ls())

#####Librerias#####
library(openxlsx)
library(tidyverse)
library(stringr)
library(ggrepel)
library(ggthemes)
###funciones####
reorder_within <- function(x, by, within, fun = mean, sep = " - ", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = " - ") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

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

paleta3 <- c(azul[3],
             naranja[3],
             verde[3])
paleta1 <- c(azul[1],
             naranja[1],
             verde[1])
###############################Data Encuestas#########################
perfiles <- readRDS(file = "Resultados/America.RDS")
agregado <- readRDS(file = "Resultados/America_agregado.RDS")


periodos <- unique(agregado[c("Pais","periodo")])

###############################Fuentes complementarias#########################
Paises <- read.xlsx("Fuentes Complementarias/Prod y Salarios.xlsx",
                    sheet = "Paises_Latam") 
Paises %>%
  filter(Color == "Si") %>% 
  select(COD.OCDE)

Paises <- Paises %>% 
  mutate(
    Cluster = case_when(
    COD.OCDE %in% c("BOL","ECU","SLV") ~ "Cluster 1",
    COD.OCDE %in% c("GTM","PRY","PER") ~ "Cluster 2",
    COD.OCDE %in% c("BRA","URY","MEX","CHL","ARG","CRI","COL") ~ "Cluster 3",
    COD.OCDE %in% c("USA") ~ "EE.UU.",
    ),
    Orden = case_when(
      COD.OCDE %in% c("BOL")~ 1,
      COD.OCDE %in% c("ECU")~ 2,
      COD.OCDE %in% c("SLV")~ 3,
      COD.OCDE %in% c("GTM")~ 4,
      COD.OCDE %in% c("PRY")~ 5,
      COD.OCDE %in% c("PER")~ 6,
      COD.OCDE %in% c("ARG")~ 7,
      COD.OCDE %in% c("BRA")~ 8,
      COD.OCDE %in% c("CHL")~ 9,
      COD.OCDE %in% c("COL")~ 10,
      COD.OCDE %in% c("CRI")~ 11,
      COD.OCDE %in% c("MEX")~ 12,
      COD.OCDE %in% c("URY")~ 13))

Paises$nombre.pais[Paises$nombre.pais== "México"]<- "Mexico"

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
 
IPC_2005 <- read.xlsx("Fuentes Complementarias/Prod y Salarios.xlsx",
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
names(PPA_WB)[1] <- "Country"

IPC_2005$nombre.pais[IPC_2005$nombre.pais == "Perú"] <- "Peru"
IPC_2005$nombre.pais[IPC_2005$nombre.pais == "México"] <- "Mexico"

PPA_WB_IPC <- IPC_2005 %>% 
  left_join(Paises) %>% 
  left_join(PPA_WB %>% select(COD.OCDE,ANO4,PPA.BENCHMARK.2017))  %>%
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
           )) %>% 
  ungroup()

  

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


####Base unificada general####
options(scipen = 999)
America <- perfiles %>%
  bind_rows(agregado) %>% 
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
  left_join(Paises %>% select(Pais = nombre.pais,Orden,Cluster)) %>% 
  filter(Pais != "Canada")
         

#############Perfiles######
###### Peso en empleo total#####
America %>% 
  filter(tamanio.calif!= "Total",Pais!= "Canada") %>% 
  ggplot(.,
         aes(x = reorder(Pais,Orden), y = particip.ocup,
             fill = tamanio.calif,group = tamanio.calif,
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
  facet_grid(cols = vars(Cluster),
             space = "free",
             scales = "free_x")+
  guides(fill=guide_legend(title="Tamaño - Calificación"))
  

particip_media_cluster<- America %>% 
  filter(tamanio.calif!= "Total",Pais!= "Canada") %>% 
  group_by(Cluster,tamanio.calif) %>% 
  summarise(particip.ocup = mean(particip.ocup)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Cluster,values_from = particip.ocup)

particip_calif_cluster<- America %>% 
  filter(tamanio.calif!= "Total",Pais!= "Canada") %>% 
  group_by(Pais,grupos.calif,Cluster) %>% 
  summarise(particip.ocup = sum(particip.ocup)) %>% 
  group_by(grupos.calif,Cluster) %>%
  summarise(particip.ocup = mean(particip.ocup)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Cluster,values_from = particip.ocup)


particip_tamanios_cluster<- America %>% 
  filter(tamanio.calif!= "Total",Pais!= "Canada") %>% 
  group_by(Pais,grupos.tamanio,Cluster) %>% 
  summarise(particip.ocup = sum(particip.ocup)) %>% 
  group_by(grupos.tamanio,Cluster) %>%
  summarise(particip.ocup = mean(particip.ocup)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = Cluster,values_from = particip.ocup)

lista_export <- list("Datos America" = America,
                     "Particip ocup cluster"  = particip_media_cluster,
                     "Particip calif cluster"  = particip_calif_cluster,
                     "Particip tamanio cluster"  = particip_tamanios_cluster)

  # openxlsx::write.xlsx(x = lista_export,
  #                    file = "Resultados/America/Cuadros/Resultados_America.xlsx",
  #                    overwrite = T)

ggsave("Resultados/America/America_calificacion_tamanio.jpg",width = 10,height = 8)

###### TCP-calificacion#####
TCPs<- America %>% 
  filter(tamanio.calif!= "Total",Pais!= "Canada") %>% 
  group_by(Pais) %>% 
  mutate(ocupados_spriv.pais = sum(ocupados)) %>% 
  filter(grupos.tamanio== "Pequeño") %>% 
  group_by(Pais,grupos.calif,Orden,Cluster) %>% 
  summarise(peso_tcp = tcp/ocupados_spriv.pais) %>% 
  group_by(grupos.calif,Cluster) %>% 
  mutate(peso_tcp_cluster = mean(peso_tcp))
  

America %>% 
  filter(grupos.tamanio== "Pequeño") %>% 
  ggplot(.,
         aes(x = reorder(Pais,Orden), y = particip.tcp,
             fill = tamanio.calif,group = tamanio.calif,
             label = scales::percent(particip.tcp,accuracy = 0.01))) +
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
  #scale_fill_manual(values = paleta)+
  scale_y_continuous(labels = scales::percent)+
  guides(fill=guide_legend(title="Tamaño - Calificación"))

ggsave("Resultados/America/America_calificacion_tamanio_TCP.jpg",width = 10,height = 8)

######TCP en empleo total####

TCPs%>% 
  filter(Pais != "Estados Unidos") %>% 
  ggplot(.,
         aes(x = reorder(Pais,Orden), y = peso_tcp,
             fill = grupos.calif,group = grupos.calif,
             label = scales::percent(peso_tcp,accuracy = 0.01))) +
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5),size=3)+
  #labs(title = "Participacion del cuentapropismo")+
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
  facet_grid(cols = vars(Cluster),
             space = "free",
             scales = "free_x")+
  scale_y_continuous(labels = scales::percent)+
  guides(fill=guide_legend(title="Calificación"))


###### Asalariz en P1 a P3#####

America %>% 
  filter(grupos.tamanio== "Pequeño") %>% 
  ggplot(.,
         aes(x = Pais, y = tasa.asalarizacion,
             fill = tamanio.calif,group = tamanio.calif,
             label = scales::percent(tasa.asalarizacion,accuracy = 0.1))) +
  geom_col(position = "dodge")+
  geom_text(position = position_dodge(width = 1),size=3)+
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
  #scale_fill_manual(values = paleta)+
  scale_y_continuous(labels = scales::percent)+
  guides(fill=guide_legend(title="Tamaño - Calificación"))

###### TCP/ASAL - P1 a P3#####

America %>%
  filter(grupos.tamanio== "Pequeño") %>% 
  filter(tamanio.calif!= "Total") %>% 
  ungroup() %>% 
  select(Pais,tamanio.calif,asalariados = tasa.asalarizacion,cuentapropistas = tasa.cp) %>% 
  pivot_longer(cols = 3:4,names_to = "indicador",values_to = "valor") %>% 
  ggplot(.,
         aes(x = Pais,
             y = valor,
             fill = indicador,
             group = indicador,
             label = scales::percent(valor,accuracy = 0.1))) +
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5),size=3)+
  theme_tufte()+
  scale_x_discrete(expand=c(0, 1))+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title  = element_blank(),
        #legend.text  = element_text(size = 8),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.ticks.x = element_blank(),
        text = element_text(size = 16),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_y_continuous(labels = scales::percent)+
  facet_wrap(~tamanio.calif)

####Precariedad asalariados#####
###### Por perfiles####
America %>%
filter(tamanio.calif!= "Total") %>% 
filter(Pais != "Estados Unidos",Pais != "Canada") %>% 
  ggplot(.,
         aes(x = reorder(Pais,Orden), y = tasa.seguridad.social.asal,
             fill = tamanio.calif, 
             label = round(tasa.seguridad.social.asal,2))) +
  geom_col(position = "dodge")+
  #geom_text(position = position_dodge(),size=2.5,angle = 90)+
  #labs(title = "Tasa de ausencia de cobertura en la seguridad social")+
  theme_tufte()+
  theme(legend.position = "none",
        #legend.direction = "horizontal",
        legend.key.size = unit(0.2,"cm"),
        #legend.spacing.x  = unit(0.4,"cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 17),
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
  scale_fill_manual(values = paleta)+
  facet_wrap(~tamanio.calif)+
  guides(fill=guide_legend(keywidth = 0.8))

ggsave("Resultados/America/tasas seguridad social.jpg",width = 15.59,height = 8)

###### Agregado####

seguridad_social_agregado <- America %>%
  filter(tamanio.calif== "Total") %>% 
  filter(Pais != "Estados Unidos",Pais != "Canada") %>% 
  group_by(Cluster) %>% 
  mutate(tasa.seguridad.social.asal_cluster = mean(tasa.seguridad.social.asal)) %>% 
  select(Pais,Orden,Cluster,tamanio.calif,tasa.seguridad.social.asal,tasa.seguridad.social.asal_cluster) %>% 
  arrange(Orden)

    ggplot(seguridad_social_agregado,
         aes(x = reorder(Pais,Orden), y = tasa.seguridad.social.asal,
             label = round(tasa.seguridad.social.asal,2))) +
  geom_col(position = "dodge")+
  #geom_text(position = position_dodge(),size=2.5,angle = 90)+
 # labs(title = "Tasa de ausencia de cobertura en la seguridad social")+
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
  facet_grid(cols = vars(Cluster),scales = "free_x",space = "free")
  guides(fill=guide_legend(keywidth = 0.8))
ggsave("Resultados/America/tasas seguridad social agregado.jpg",width = 15.59,height = 8)

lista_export[["seguridad_social_agregado"]] <- seguridad_social_agregado
#####INGRESOS##################
###### TCP vs ASAL##################
America %>%
 # mutate(grupos.calif = factor(grupos.calif,levels = c("Baja","Media","Alta"))) %>% 
 filter(grupos.tamanio== "Pequeño",Pais != "Canada",
        Pais != "Estados Unidos") %>% 
 filter(tamanio.calif!= "Total") %>% 
 ggplot(.,
         aes(x = reorder(Pais,Orden), y = ing.tcp.ing.asal,
             fill = grupos.calif)) +
  geom_col(position = "dodge")+
  geom_hline(mapping = aes(yintercept = 1),size = 1)+
  theme_tufte()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        #legend.title  = element_text(size = 8),
        #legend.text  = element_text(size = 8),
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.ticks.x = element_blank(),
        text = element_text(size = 16),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  facet_grid(cols = vars(Cluster),scales = "free_x",space = "free")+
  guides(fill=guide_legend(title="Calificación"))
         
  #scale_fill_manual(values = paleta)

ggsave("Resultados/America/Ingresos TCP ASAL.png",width = 15.59,height = 9)

###### Primas Salariales##################
desigualdad_salarios<- America %>% 
  filter(tamanio.calif!= "Total",Pais != "Canada") %>% 
  group_by(Pais,Cluster) %>% 
  summarise(salario_perfil9_perfil1 = 
              promedio.ing.oc.prin.asal[tamanio.calif == "Grande - Alta"]/
              promedio.ing.oc.prin.asal[tamanio.calif == "Pequeño - Baja"],
            distancias.promedio = 
              sum(abs(prima.salario.medio-1))
              ) %>% 
  group_by(Cluster) %>% 
  mutate(salario_perfil9_perfil1_cluster = 
              mean(salario_perfil9_perfil1),
         distancias.promedio_cluster = mean(distancias.promedio)) %>% 
  arrange(Cluster)

lista_export[["desigualdad_salarios"]] <-   desigualdad_salarios
  
America %>%
  filter(tamanio.calif!= "Total",Pais != "Canada") %>% 
  ggplot(.,
         aes(x = reorder(Pais,Cluster), y = prima.salario.medio,
             fill = tamanio.calif,group = tamanio.calif)) +
  geom_col(position = "dodge")+
  geom_hline(mapping = aes(yintercept = 1),size = 1)+
  theme_tufte()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        #legend.title  = element_text(size = 8),
        #legend.text  = element_text(size = 8),
        axis.title = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 16),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta)+
  facet_grid(cols = vars(Cluster),scales = "free_x",space = "free")
ggsave("Resultados/America/Ingreso Asalariados.png",width = 15.59,height = 9)

#####Salarios PPA ###########
data.graf.PPA <- America %>% 
  mutate(periodo = case_when(Pais != "El Salvador" ~ 2019,
                             TRUE ~ periodo)) %>% 
  #select(Pais,tamanio.calif,periodo,promedio.ing.oc.prin.asal) %>% 
  left_join(Paises %>% rename(Pais = nombre.pais)) %>% 
  left_join(PPA_WB_IPC %>% select(periodo = ANO4,COD.OCDE,PPA.BENCHMARK.2017.EXTRAPOLADO)) %>% 
  mutate(salario.PPA = promedio.ing.oc.prin.asal/PPA.BENCHMARK.2017.EXTRAPOLADO) %>% 
  group_by(tamanio.calif) %>% 
  mutate(salario.PPA.relativo.usa = salario.PPA/salario.PPA[Pais == "Estados Unidos"]) %>% 
  ungroup()
###### Agregados###############
 
ggplot(data.graf.PPA %>% 
          filter(tamanio.calif == "Total"),
        aes(x = reorder(Pais,Orden),
            y = salario.PPA)) +
   geom_col(position = "dodge")+
   scale_x_reordered()+
   # geom_text(position = position_dodge(),size=2.5)+
   #labs(title = "Salario PPA - Encuestas. Año 2019")+
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
         panel.grid.major.x = element_line(colour = "grey"),
         text = element_text(size = 15))+
   scale_fill_manual(values = paleta)+
   facet_grid(cols = vars(Cluster), scales = "free_x",space = "free")
 ggsave("Resultados/America/salarios ppa encuestas agregado.png",width = 15.59,height = 9)
 
###### Usa = 1 en cada perfil#####
 ggplot(data.graf.PPA %>% 
          filter(tamanio.calif != "Total") %>% 
          filter(Pais != "Estados Unidos",Pais != "Canadá"),
        aes(x = reorder(Pais,salario.PPA.relativo.usa),
            y = salario.PPA.relativo.usa,
            fill = tamanio.calif,group = tamanio.calif)) +
   geom_col(position = "dodge")+
   # geom_text(position = position_dodge(),size=2.5)+
   labs(title = "Salario PPA. Año 2019")+
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
   facet_wrap(~tamanio.calif, scales = "fixed")
 ggsave("Resultados/America/salarios ppa encuestas.png",width = 15.59,height = 9)

###### Distribucion de distancias####
ggplot(data.graf.PPA %>% 
          filter(tamanio.calif != "Total") %>% 
          filter(Pais != "Estados Unidos",Pais != "Canadá"),
        aes(x = reorder(Pais,Orden),
            y = salario.PPA.relativo.usa,
            color = tamanio.calif,group = tamanio.calif)) +
   geom_point(position = "dodge",size = 5)+
   # geom_text(position = position_dodge(),size=2.5)+
   #labs(title = "Salario relativo a un mismo perfil en EUA. Año 2019")+
   theme_tufte()+
   theme(legend.position = "left",
         legend.direction = "vertical",
         axis.title = element_blank(),
         axis.text.x = element_text(angle = 90),
         axis.ticks.x = element_blank(),
         panel.spacing = unit(1,"cm"),
         panel.grid.major.y = element_line(colour = "grey"),
         panel.grid.minor.y = element_line(colour = "grey30"),
         panel.grid.minor.x = element_line(colour = "grey"),
         panel.grid.major.x = element_line(colour = "grey"))+
   scale_color_manual(values = paleta)+
   facet_grid(cols = vars(Cluster), scales = "free_x",space = "free")
 
 
ggsave("Resultados/America/salarios relativos a mismo perfil.png",
       width = 15.59,height = 9)


ggplot(data.graf.PPA %>% 
         filter(tamanio.calif != "Total") %>% 
         filter(Pais != "Estados Unidos",Pais != "Canadá"),
       aes(x = reorder(Pais,salario.PPA.relativo.usa),
           y = salario.PPA.relativo.usa,
           color = grupos.tamanio,shape = grupos.calif)) +
  geom_point(position = "dodge",size = 5,alpha = 0.7)+
  # geom_text(position = position_dodge(),size=2.5)+
  labs(title = "Salario relativo a un mismo perfil en EUA. Año 2019")+
  theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        axis.title = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.ticks.x = element_blank(),
        panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_color_manual(values = rev(paleta3))+
  guides(color=guide_legend(title="Tamaño"),
         shape=guide_legend(title="Calificación"))

# con_numeros <-  data.graf.PPA %>%
#   filter(tamanio.calif != "Total") %>% 
#   arrange(Pais,tamanio.calif) %>% 
#   mutate(Perfil = rep_len(1:9,length.out = nrow(.))) 
#  
# ggplot(con_numeros %>% 
#          filter(Pais != "Estados Unidos",Pais != "Canadá"),
#        aes(x = reorder(Pais,salario.PPA.relativo.usa),
#            y = salario.PPA.relativo.usa,label = Perfil,
#            color = tamanio.calif,group = tamanio.calif)) +
#   #geom_point(position = "dodge",size = 5)+
#   geom_text(size=5)+
#   labs(title = "Salario relativo a un mismo perfil en EUA. Año 2019")+
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
#   scale_color_manual(values = paleta)

###### Escalera al cielo#####
 
 ggplot(data.graf.PPA %>% 
          filter(tamanio.calif != "Total"),
         aes(x = reorder_within(Pais,salario.PPA,tamanio.calif),
             y = salario.PPA,
             fill = tamanio.calif,group = tamanio.calif)) +
  geom_col(position = "dodge")+
  scale_x_reordered()+
 # geom_text(position = position_dodge(),size=2.5)+
  labs(title = "Salario PPA. Año 2019")+
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
  facet_grid(~tamanio.calif, scales = "free")
ggsave("Resultados/America/salarios ppa encuestas.png",width = 15.59,height = 9)
 
write.xlsx(data.graf.PPA,"Resultados/America/Cuadros/Series completas.xlsx")
            
openxlsx::write.xlsx(x = lista_export,
                   file = "Resultados/America/Cuadros/Resultados_America.xlsx",
                   overwrite = T)
