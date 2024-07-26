#Librerias ####
library(ggflags)
library(tidyverse)
library(openxlsx)

#Base ####
Base <- readRDS("base_homogenea.RDS")

# Fuentes complementarias #####
iso_codes <- read.xlsx("Fuentes Complementarias/isocodes.xlsx",
                       sheet = "INGLES") 
Paises <- read.xlsx("Fuentes Complementarias/Prod y Salarios.xlsx",
                    sheet = "Paises") 
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
# Calculos####
pesos_perfiles<- base_grupos %>% 
  group_by(PAIS,tamanio.calif) %>% 
  summarise(casos_pond = sum(WEIGHT,na.rm = T)) %>% 
  group_by(PAIS) %>% 
  mutate(particip.ocup= casos_pond/sum(casos_pond))

asalariados <- base_grupos %>% 
  filter(CATOCUP == "Asalariado") %>% 
  mutate(buen_empleo = ifelse(PRECAPT %in% c(0,NA) &
                                #   PRECAREG%in% c(0,NA) &
                                PRECASEG%in% c(0,NA) &
                                PRECATEMP%in% c(0,NA),"Si","No"))

peso_buenos_emp <- asalariados %>% 
  group_by(PAIS,buen_empleo) %>% 
  summarise(casos_pond = sum(WEIGHT,na.rm = T)) %>% 
  group_by(PAIS) %>% 
  mutate(porcentaje= casos_pond/sum(casos_pond))

peso_buenos_emp_perfiles <- asalariados %>% 
  group_by(PAIS,buen_empleo,tamanio.calif) %>% 
  summarise(casos_pond = sum(WEIGHT,na.rm = T)) %>% 
  group_by(PAIS,tamanio.calif) %>% 
  mutate(porcentaje= casos_pond/sum(casos_pond))

expresiones_pais <- asalariados %>% 
  group_by(PAIS) %>% 
  summarise(part_time = sum(WEIGHT[PRECAPT==1],na.rm = T)/sum(WEIGHT[PRECAPT %in% 0:1],na.rm = T),
            no_registro = sum(WEIGHT[PRECAREG==1],na.rm = T)/sum(WEIGHT[PRECAREG %in% 0:1],na.rm = T),
            no_seg_social = sum(WEIGHT[PRECASEG==1],na.rm = T)/sum(WEIGHT[PRECASEG %in% 0:1],na.rm = T),
            temporario = sum(WEIGHT[PRECATEMP==1],na.rm = T)/sum(WEIGHT[PRECATEMP %in% 0:1],na.rm = T),
  ) %>% 
  pivot_longer(cols = 2:ncol(.),names_to = "expresion",values_to = "valor")

salario_preca <- asalariados %>% 
  group_by(ANO,PAIS) %>% 
  summarise(salrio_prom =weighted.mean(ING_PPA,na.rm = T),
            buenos_empleos = 
              sum(WEIGHT[buen_empleo=="Si"],na.rm = T)/
              sum(WEIGHT,na.rm = T)
  ) %>% 
  filter(buenos_empleos>0,salrio_prom>0)

salario_preca_grupos <- asalariados %>% 
  group_by(ANO,PAIS,tamanio.calif) %>% 
  summarise(salrio_prom =weighted.mean(ING_PPA,na.rm = T),
            buenos_empleos = 
              sum(WEIGHT[buen_empleo=="Si"],na.rm = T)/
              sum(WEIGHT,na.rm = T)
  ) %>% 
  filter(buenos_empleos>0,salrio_prom>0)

salario_buenos <- asalariados %>% 
  filter(buen_empleo=="Si") %>% 
  group_by(ANO,PAIS) %>% 
  summarise(salrio_prom = weighted.mean(ING_PPA,na.rm = T),
            salario_median = median(ING_PPA,na.rm = T)) %>% 
  pivot_longer(cols = 3:4,names_to = "indicador",values_to = "valor")

salario_buenos_perfiles <- asalariados %>% 
  filter(buen_empleo=="Si") %>% 
  group_by(ANO,PAIS,tamanio.calif) %>% 
  summarise(salrio_prom = weighted.mean(ING_PPA,na.rm = T),
            salario_median = median(ING_PPA,na.rm = T)) %>% 
  group_by(tamanio.calif) %>% 
  mutate(salario_prom_rel_usa = salrio_prom/salrio_prom[PAIS == "Estados Unidos"],
         salario_mediana_rel_usa = salario_median/salario_median[PAIS == "Estados Unidos"],
  )

salario_perfiles <- asalariados %>% 
  group_by(ANO,PAIS,tamanio.calif) %>% 
  summarise(salrio_prom = weighted.mean(ING_PPA,na.rm = T),
            salario_median = median(ING_PPA,na.rm = T)) %>% 
  group_by(tamanio.calif) %>% 
  mutate(salario_prom_rel_usa = salrio_prom/salrio_prom[PAIS == "Estados Unidos"],
         salario_mediana_rel_usa = salario_median/salario_median[PAIS == "Estados Unidos"],
  )

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

paleta9 <- c(azul,
            naranja,
            verde
)
## Peso de perfiles#####
paises_orden <- Paises %>% select(PAIS = nombre.pais,region,Orden,COD.OCDE)


pesos_perfiles %>% 
  left_join(paises_orden) %>% 
  filter(! COD.OCDE %in% c("BOL","ROU","BGR")) %>% 
  ggplot(.,
         aes(x = reorder(PAIS,Orden), y = particip.ocup,
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
  facet_grid(cols = vars(region),
             space = "free",
             scales = "free_x")+
  guides(fill=guide_legend(title="Tamaño - Calificación"))

ggsave("Resultados/Mundial/perfiles_v2.jpg",width = 15,height = 12,bg = "white")

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
ggsave("Resultados/Mundial/empleos_buenos.jpg",width = 15,height = 12,bg = "white")

### por perfil ####
peso_buenos_emp_perfiles %>% 
  filter(buen_empleo == "Si") %>% 
  left_join(paises_orden) %>% 
  ggplot(.,
         aes(x = tamanio.calif, y = porcentaje,
             group = tamanio.calif,fill = tamanio.calif,
             label = scales::percent(porcentaje,decimal.mark = ",",
                                     accuracy = 0.1))) +
  geom_col(position = "stack")+
  # geom_text(position = position_stack(vjust = .5),size=3)+
  labs(title = '% de "buenos empleos" asalariados',
       subtitle = "Expreiones evaluadas:  Falta de cobertura social | Part Time Involuntario| Empleo de duración determinada")+
  # labs(title = "% de asalariados con alguna expresión de precariedad",
  #      subtitle = "Expreiones evaluadas:  | No registro | Falta de cobertura social | Part Time Involuntario| Empleo de duración determinada")+
  ggthemes::theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 14),
        axis.title = element_blank(),
        text = element_text(size = 18),
        axis.text.x = element_blank(),
        plot.margin = margin(0,1,0,0, "cm"),
        #panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  facet_wrap(vars(reorder(PAIS,Orden)))+
  scale_fill_manual(values = paleta[4:12])+
  scale_y_continuous(labels = scales::percent)
ggsave("Resultados/Mundial/empleos_buenos_perfiles.jpg",width = 15,height = 12,bg = "white")

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

## Salarios #####
### Banderas ####    
con_bandera<- salario_preca %>% 
  left_join(paises_orden) %>% 
  left_join(iso_codes %>% select(alpha_2,
                                 COD.OCDE = alpha_3)) %>% 
  mutate(alpha_2= str_to_lower(alpha_2))

con_bandera_grupos<- salario_preca_grupos %>% 
  left_join(paises_orden) %>% 
  left_join(iso_codes %>% select(alpha_2,
                                 COD.OCDE = alpha_3)) %>% 
  mutate(alpha_2= str_to_lower(alpha_2))

con_bandera %>% 
  ggplot(aes(x = buenos_empleos,
             y = salrio_prom, 
             group = alpha_2,
             country = alpha_2)) +
  geom_flag(size = 10) +
  labs(x = "% de empleos no precarios",
       y = "Salario Promedio PPA")+
  theme(legend.title = element_blank(),
        legend.position = "none",
        text = element_text(size=15))
ggsave("Resultados/Mundial/banderas_paises.jpg",width = 15,height = 9,bg = "white")



con_bandera_grupos %>% 
  mutate(perfil = as.numeric(tamanio.calif)) %>% 
  mutate(perfil_text = paste0(perfil," - ",tamanio.calif)) %>% 
  ggplot(aes(x = buenos_empleos,
             y = salrio_prom, 
             country = alpha_2)) +
  geom_flag() +
  labs(x = "% de empleos no precarios",
       y = "Salario Promedio PPA")+
  theme(legend.title = element_blank(),
        legend.position = "left")
facet_wrap(vars(tamanio.calif),scales = "")
ggsave("Resultados/Mundial/banderas_grupos.jpg",width = 15,height = 12,bg = "white")

con_bandera_grupos %>% 
  mutate(perfil = as.numeric(tamanio.calif)) %>% 
  mutate(perfil_text = paste0(perfil," - ",tamanio.calif)) %>% 
  ggplot(aes(x = buenos_empleos,
             y = salrio_prom, 
             group = perfil_text,
             fill = perfil_text,
             label = perfil,
             country = alpha_2)) +
  geom_point(size = 0) +
  geom_flag() +
  geom_text() +
  labs(x = "% de empleos no precarios",
       y = "Salario Promedio PPA")+
  theme(legend.title = element_blank(),
        legend.position = "left")
ggsave("Resultados/Mundial/banderas_grupos_todo_junto.jpg",width = 15,height = 12,bg = "white")
### Salarios - sin filtrar ####

salario_perfiles %>% 
  left_join(paises_orden) %>% 
  filter(!is.na(salrio_prom),region != "USA") %>% 
  ggplot(aes(x = reorder(PAIS,Orden),
             y = salario_prom_rel_usa,
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
  scale_color_manual(values = paleta9)+
  scale_y_continuous(breaks = seq(0.1,1.2,0.1))+
  facet_grid(cols = vars(region), scales = "free_x",space = "free")


ggsave("Resultados/America/relativos a mismo perfil.png",
       width = 15.59,height = 9)

salario_perfiles %>% 
  left_join(paises_orden) %>% 
  filter(!is.na(salrio_prom),region != "USA") %>% 
  ggplot(aes(x = reorder(PAIS,Orden),
             y = salario_mediana_rel_usa,
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
  scale_color_manual(values = paleta9)+
  scale_y_continuous(breaks = seq(0.1,1.2,0.1))+
  facet_grid(cols = vars(region), scales = "free_x",space = "free")


ggsave("Resultados/America/medianas_relativos a mismo perfil.png",
       width = 15.59,height = 9)

### No precarios ####
salario_buenos %>% 
  filter(!is.na(valor)) %>% 
  left_join(paises_orden) %>% 
  ggplot(.,
         aes(x = reorder(PAIS,valor), y = valor,
             label = round(valor,digits = 0))) +
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5),size=3)+
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
  facet_wrap(vars(indicador))


salario_buenos_perfiles %>% 
  filter(!is.na(salrio_prom)) %>% 
  left_join(paises_orden) %>% 
  ggplot(.,
         aes(x = reorder(PAIS,Orden), y = salrio_prom,
             label = round(salrio_prom,digits = 0))) +
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5),size=3)+
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
  facet_wrap(vars(tamanio.calif))

salario_buenos_perfiles %>% 
  left_join(paises_orden) %>% 
  filter(!is.na(salrio_prom),region != "USA") %>% 
  ggplot(aes(x = reorder(PAIS,Orden),
           y = salario_prom_rel_usa,
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
  scale_color_manual(values = paleta9)+
  scale_y_continuous(breaks = seq(0.1,1.2,0.1))+
  facet_grid(cols = vars(region), scales = "free_x",space = "free")


ggsave("Resultados/America/relativos a mismo perfil_no_precarios.png",
       width = 15.59,height = 9)
