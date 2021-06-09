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

paleta3 <- c(azul[1],
             naranja[1],
             verde[1])
###############################Data Encuestas#########################
perfiles <- readRDS(file = "Resultados/America.RDS")
agregado <- readRDS(file = "Resultados/America_agregado.RDS")


periodos <- unique(agregado[c("Pais","periodo")])


tac <- agregado %>% 
  mutate(ratio = promedio.ing.oc.prin.noasal/promedio.ing.oc.prin.asal) %>% 
  select(Pais,ratio,promedio.ing.oc.prin.noasal,promedio.ing.oc.prin.asal)

tac2 <- perfiles %>% 
  mutate(ratio = promedio.ing.oc.prin.noasal/promedio.ing.oc.prin.asal) %>%
  filter(grupos.tamanio == "Pequeño") %>% 
  select(Pais,grupos.calif,grupos.tamanio,ratio,promedio.ing.oc.prin.noasal,promedio.ing.oc.prin.asal)

###############################Fuentes complementarias#########################
Paises <- read.xlsx("Fuentes Complementarias/Prod y Salarios.xlsx",
                    sheet = "Paises_Latam")


product.salarios.america <- read.xlsx("Fuentes Complementarias/Compilacion.xlsx",
                           sheet = "Para R") %>% 
  rename(nombre.pais = Pais,
         ANO4 = Anio)

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
IPC_2005$nombre.pais[IPC_2005$nombre.pais == "Perú"] <- "Peru"

PPA_WB <- read.csv("Fuentes Complementarias/All_PPA_WB_2017.csv") %>%
  rename(COD.OCDE = Country.Code) %>%
  filter(Classification.Code == "PPPGlob",Series.Code %in%  c("1000000","9020000")) %>%
  pivot_longer(cols = 7:ncol(.),
               names_to = "Año",
               values_to = "PPA.BENCHMARK.2017") %>%
  mutate(ANO4 = as.numeric(str_extract(Año,"[[:digit:]]{4}")),
         PPA.BENCHMARK.2017 = as.numeric(as.character(PPA.BENCHMARK.2017)))
names(PPA_WB)[1] <- "Country"

PPA_WB <- PPA_WB %>% 
  select(ANO4,Country,COD.OCDE,Series.Name,PPA.BENCHMARK.2017) %>% 
  pivot_wider(names_from = Series.Name,values_from = PPA.BENCHMARK.2017)

names(PPA_WB)[4] <- "PPA.GDP"
names(PPA_WB)[5] <- "PPA.AIC"


Data_CCNN <- IPC_2005 %>% 
  left_join(Paises) %>% 
  left_join(PPA_WB)  %>%
  group_by(ANO4) %>%
  mutate(IPC_2005_USA = IPC_2005[nombre.pais == "Estados Unidos"]) %>%
  group_by(nombre.pais) %>%
  mutate(PPA.GDP.EXTRAPOLADO =
           case_when(!is.na(PPA.GDP)~PPA.GDP,
                     is.na(PPA.GDP) ~ PPA.GDP[ANO4 == 2017]*(IPC_2005/IPC_2005[ANO4 == 2017])/(IPC_2005_USA/IPC_2005_USA[ANO4 == 2017])
        ),
        PPA.AIC.EXTRAPOLADO =
          case_when(!is.na(PPA.AIC)~PPA.AIC,
                    is.na(PPA.AIC) ~ PPA.AIC[ANO4 == 2017]*(IPC_2005/IPC_2005[ANO4 == 2017])/(IPC_2005_USA/IPC_2005_USA[ANO4 == 2017])
                    
         )) %>% 
  ungroup() %>% 
  left_join(Salarios_UMN) %>% 
  left_join(product.salarios.america) %>% 
  mutate(salario.PPA.CEPED = Salario.UMN/PPA.AIC.EXTRAPOLADO,
         salario.PPA = Salarios/PPA.AIC.EXTRAPOLADO,
         productiv.PPA = Productividad/PPA.GDP.EXTRAPOLADO)

  
####Salarios Y productividad####
salarios.prod.GW <- Data_CCNN %>%
  ungroup() %>%
  filter(ANO4  %in%  2016) %>%
  filter(!is.na(salario.PPA)) %>%
  group_by(ANO4) %>%
  mutate(salario.relativo.usa.CEPED = salario.PPA.CEPED/salario.PPA.CEPED[nombre.pais=="Estados Unidos"]*100,
         salario.relativo.usa = salario.PPA/salario.PPA[nombre.pais=="Estados Unidos"]*100,
         productiv.relativa.usa = productiv.PPA/productiv.PPA[nombre.pais=="Estados Unidos"]*100)


ggplot(salarios.prod.GW,
       aes(x=salario.relativo.usa.CEPED,
           y=productiv.relativa.usa,
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

ggplot(salarios.prod.GW,  
       aes(x = reorder(nombre.pais,salario.PPA),
           y = salario.PPA)) +
  geom_col(position = "dodge")+
  scale_x_reordered()+
  # geom_text(position = position_dodge(),size=2.5)+
  labs(title = "Salario PPA CCNN - Series Nuevas  - 2016")+
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
  scale_fill_manual(values = paleta)




####PERFILES####
options(scipen = 999)
America <- perfiles %>%
  bind_rows(agregado) %>% 
  mutate(
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
  arrange(Pais,tamanio.calif)



America %>% 
  filter(tamanio.calif!= "Total") %>% 
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
filter(tamanio.calif!= "Total") %>% 
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

America %>%
  filter(tamanio.calif== "Total") %>% 
  filter(Pais != "Estados Unidos",Pais != "Canada") %>% 
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
ggsave("Resultados/America/tasas seguridad social agregado.jpg",width = 15.59,height = 8)

###########Primas salariales##################
America %>%
 filter(tamanio.calif!= "Total") %>% 
 ggplot(.,
         aes(x = tamanio.calif, y = prima.salario.medio,
             fill = tamanio.calif,group = tamanio.calif)) +
  geom_col(position = "dodge")+
  geom_hline(mapping = aes(yintercept = 1),size = 1)+
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

########Salarios PPA encuestas###########
America$Pais[America$Pais =="Canada"] <- "Canadá"
America$Pais[America$Pais =="Mexico"] <- "México"

 data.graf.PPA <- America %>% 
  mutate(periodo = case_when(Pais != "El Salvador" ~ 2019,
                             TRUE ~ periodo)) %>% 
  select(Pais,tamanio.calif,periodo,promedio.ing.oc.prin.asal,salario.promedio.pais) %>% 
  left_join(Paises %>% rename(Pais = nombre.pais)) %>%
  left_join(PPA_WB_IPC %>% select(periodo = ANO4,COD.OCDE,PPA.BENCHMARK.2017.EXTRAPOLADO)) %>% 
  mutate(salario.PPA = promedio.ing.oc.prin.asal/PPA.BENCHMARK.2017.EXTRAPOLADO) 


  
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
 

ggplot(data.graf.PPA %>% 
         filter(tamanio.calif == "Total"),
       aes(x = reorder(Pais,salario.PPA),
           y = salario.PPA)) +
  geom_col(position = "dodge")+
  scale_x_reordered()+
  # geom_text(position = position_dodge(),size=2.5)+
  labs(title = "Salario PPA - Encuestas. Año 2019")+
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
  facet_grid(~tamanio.calif, scales = "free")
ggsave("Resultados/America/salarios ppa encuestas agregado.png",width = 15.59,height = 9)


# write.xlsx(data.graf.PPA,"Resultados/America/Cuadros/salarios_encuestas_ppa.xlsx")
            
