# Importacion ####
options(scipen=99)
library(tidyverse)

rutas <- list.files("Bases_homog/",full.names = T,pattern = ".rds")

Base <- data.frame()
for(i in rutas){
  base_temp<- readRDS(i) %>%
    mutate(PERIODO = as.character(PERIODO),
           EDAD = as.numeric(EDAD),
           ING = as.numeric(ING))
Base <-   bind_rows(Base,base_temp)
print(i)
}
table(Base$ANO,Base$PAIS)

rm(list = setdiff(ls(), "Base"))

# Chequeos ####
#Cosas a resolver - CATOCUP y SECTOR
table(Base$CATOCUP,Base$SECTOR,useNA = "always")
table(Base$CATOCUP,Base$PAIS,useNA = "always")

for (pais in unique(Base$PAIS)){
subset_data <- Base %>% filter(PAIS == pais)
cat("PAIS:", pais, "\n")
print(table(subset_data$CATOCUP, subset_data$SECTOR, useNA = "always"))}


Base$CATOCUP[Base$CATOCUP=="Asalariados"] <- "Asalariado"
Base$CATOCUP[Base$CATOCUP=="Cuenta Propia"] <- "Cuenta propia"

chequeo <- Base %>%
  group_by(PAIS,CATOCUP,SECTOR) %>% 
  count()
#En algunos países los CP no quedan dentro del sector privado

#Resultados por periodo (sin agregar anualmente) ####
Resultados <- Base                                 %>%
  filter(SECTOR == "Priv", !is.na(CALIF), !is.na(TAMA))   %>%
  group_by(PAIS, PERIODO, TAMA, CALIF)                             %>%
  summarise('total.ocupados'                       = sum(WEIGHT, na.rm=TRUE),
            'tasa.asalarizacion'                   = sum(WEIGHT[CATOCUP=="Asalariados"], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'total.asal'                           = sum(WEIGHT[CATOCUP=="Asalariados"], na.rm=TRUE),
            'tasa.partime.asal'                    = sum(WEIGHT[PRECAPT==1 & CATOCUP=="Asalariados"], na.rm=TRUE)/sum(WEIGHT[CATOCUP=="Asalariados"], na.rm=TRUE),         
            'tasa.temp.asal'                       = sum(WEIGHT[PRECATEMP==1 & CATOCUP=="Asalariados"], na.rm=TRUE)/sum(CATOCUP=="Asalariados", na.rm=TRUE), 
            'tasa.1.asalariados'                   = sum(WEIGHT[PRECAPT==1 | PRECATEMP==1], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'tasa.2.asalariados'                   = sum(WEIGHT[PRECAPT==1 & PRECATEMP==1], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'total.tcp'                            = sum(WEIGHT[CATOCUP=="Cuenta propia"], na.rm=TRUE),       
            'tasa.parttime.tcp'                    = sum(WEIGHT[PRECAPT==1 & CATOCUP=="Cuenta propia"], na.rm=TRUE)/sum(WEIGHT[CATOCUP=="Cuenta propia"], na.rm=TRUE))  %>%
  ungroup() %>%
  group_by(PAIS) %>%
  mutate('particip.ocup'          = total.ocupados/sum(total.ocupados),     
         'particip.asal'          = total.asal/sum(total.ocupados), 
         'particip.tcp'           = total.tcp/sum(total.ocupados))

# Alternativa Perfiles - CP ####
base_grupos <- Base %>% 
  filter(SECTOR == "Priv"|CATOCUP =="Cuenta propia", !is.na(CALIF), !is.na(TAMA)) %>%
  mutate(grupos = case_when(CATOCUP == "Cuenta propia" ~ paste0("CP - ",CALIF),
                            #SECTOR == "Pub" ~ "S. Publico",
                            TRUE ~ paste0(TAMA, " - ",CALIF)),
  tamanio.calif = factor(grupos,
                         levels = 
                           c("CP - Baja",
                             "CP - Media",
                             "CP - Alta",
                             "Pequeño - Baja",
                             "Pequeño - Media",
                             "Pequeño - Alta",
                             "Mediano - Baja",
                             "Mediano - Media", 
                             "Mediano - Alta",
                             "Grande - Baja",
                             "Grande - Media",
                             "Grande - Alta"
                             ))) 
# Calculos articulo mundial ####
pesos_perfiles<- base_grupos %>% 
  group_by(PAIS,tamanio.calif) %>% 
  summarise(casos_pond = sum(WEIGHT,na.rm = T)) %>% 
  group_by(PAIS) %>% 
  mutate(particip.ocup= casos_pond/sum(casos_pond))

buenos_empleos <- base_grupos %>% 
  filter(CATOCUP == "Asalariado") %>% 
  mutate(buen_empleo = ifelse(PRECAPT %in% c(0,NA) &
                              PRECAREG%in% c(0,NA) &
                              PRECASEG%in% c(0,NA) &
                              PRECATEMP%in% c(0,NA),"Si","No"))

peso_buenos_emp <- buenos_empleos %>% 
  group_by(PAIS,buen_empleo) %>% 
  summarise(casos_pond = sum(WEIGHT,na.rm = T)) %>% 
  group_by(PAIS) %>% 
  mutate(porcentaje= casos_pond/sum(casos_pond))

expresiones_pais <- buenos_empleos %>% 
  group_by(PAIS) %>% 
  summarise(part_time = sum(WEIGHT[PRECAPT==1],na.rm = T)/sum(WEIGHT[PRECAPT %in% 0:1],na.rm = T),
            no_registro = sum(WEIGHT[PRECAREG==1],na.rm = T)/sum(WEIGHT[PRECAREG %in% 0:1],na.rm = T),
            no_seg_social = sum(WEIGHT[PRECASEG==1],na.rm = T)/sum(WEIGHT[PRECASEG %in% 0:1],na.rm = T),
            temporario = sum(WEIGHT[PRECATEMP==1],na.rm = T)/sum(WEIGHT[PRECATEMP %in% 0:1],na.rm = T),
            ) %>% 
  pivot_longer(cols = 2:ncol(.),names_to = "expresion",values_to = "valor")

peso_buenos_emp_perfil <- buenos_empleos %>% 
  group_by(PAIS,grupos,buen_empleo) %>% 
  summarise(casos_pond = sum(WEIGHT,na.rm = T)) %>% 
  group_by(PAIS,grupos) %>% 
  mutate(porcentaje= casos_pond/sum(casos_pond))

# Graficos ####
## Paleta colores#####
azul <- colorspace::diverge_hcl(n = 12,h = c(255,330),
                                l = c(40,90))[c(4,2,1)]
verde <- colorspace::diverge_hcl(n = 12,h = c(130,43),
                                 c = 100,
                                 l = c(70,90))[c(4,2,1)]

naranja <- colorspace::diverge_hcl(n = 12,h = c(130,43),
                                   c = 100,
                                   l = c(70,90))[c(10,11,12)]

rojo <- colorspace::diverge_hcl(n = 12,h = 14,
                                c = 100,
                                l = c(70,90))[c(10,11,12)]
#colorspace::choose_color()
paleta <- c(rojo,
            azul,
            naranja,
            verde
            )

## Peso de perfiles#####

pesos_perfiles %>% 
  ggplot(.,
         aes(x = PAIS, y = particip.ocup,
             fill = tamanio.calif,group = tamanio.calif,
             label = scales::percent(particip.ocup,decimal.mark = ",",
                                     accuracy = 0.1))) +
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5),size=3)+
  #  labs(title = "Distribución del empleo según grupos")+
  ggthemes::theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 14),
        axis.title = element_blank(),
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 45),
        plot.margin = margin(0,1,0,0, "cm"),
        #panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta)+
  scale_y_continuous(labels = scales::percent)+
  # facet_grid(cols = vars(Cluster),
  #            space = "free",
  #            scales = "free_x")+
  guides(fill=guide_legend(title="Tamaño - Calificación"))


## % buenos empleos #####

peso_buenos_emp %>% 
  filter(buen_empleo == "Si") %>% 
  ggplot(.,
         aes(x = reorder(PAIS,porcentaje), y = porcentaje,
             label = scales::percent(porcentaje,decimal.mark = ",",
                                     accuracy = 0.1))) +
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5),size=3)+
    labs(title = '% de "buenos empleos" asalariados',
         subtitle = "Expreiones evaluadas:  | No registro | Falta de cobertura social | Part Time Involuntario| Empleo de duración determinada")+
    # labs(title = "% de asalariados con alguna expresión de precariedad",
    #      subtitle = "Expreiones evaluadas:  | No registro | Falta de cobertura social | Part Time Involuntario| Empleo de duración determinada")+
  ggthemes::theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 14),
        axis.title = element_blank(),
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 45),
        plot.margin = margin(0,1,0,0, "cm"),
        #panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta)+
  scale_y_continuous(labels = scales::percent)

## % precariedad s/ expresion #####

expresiones_pais %>% 
  ggplot(.,
         aes(x = reorder(PAIS,valor), y = valor,
             label = scales::percent(valor,decimal.mark = ",",
                                     accuracy = 0.1))) +
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5),size=3)+
   labs(title = "% de asalariados con expresión de precariedad",
        subtitle = "Expreiones evaluadas:  | No registro | Falta de cobertura social | Part Time Involuntario| Empleo de duración determinada")+
  ggthemes::theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 14),
        axis.title = element_blank(),
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 45),
        plot.margin = margin(0,1,0,0, "cm"),
        #panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  facet_wrap(facets = vars(expresion),ncol = 1)+
  scale_y_continuous(labels = scales::percent)
