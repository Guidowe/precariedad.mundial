setwd("~/GitHub/precariedad.mundial")

library(tidyverse)
library(ggrepel)

set.seed(111)

perfiles <- readRDS(file = "Resultados/America.RDS")  

#Cluster segun participacion de tamano pequeno y calificacion baja

part.pequenio <- perfiles %>% ungroup() %>% 
  select(Pais, particip.ocup, grupos.tamanio) %>% 
  filter(grupos.tamanio=="Pequeño") %>% 
  group_by(Pais) %>% 
  summarise(part.pequenio=sum(particip.ocup))

part.baja <- perfiles %>% ungroup() %>% 
  select(Pais, particip.ocup, grupos.calif) %>% 
  filter(grupos.calif=="Baja") %>% 
  group_by(Pais) %>% 
  summarise(part.baja=sum(particip.ocup))


base <- left_join(part.pequenio, part.baja, by="Pais") %>% 
  filter(!Pais %in% c("Estados Unidos", "Canada"))


####Graf Exploracion####
base %>% 
  ggplot(.,
         aes(x = part.pequenio,y = part.baja,label = Pais))+
  geom_point()+
  geom_text_repel(max.overlaps = 20)+
  theme_minimal()+
  # theme(axis.title = element_blank(),
  #       legend.title = element_blank())+
  #scale_fill_hc()+
  scale_y_continuous(labels = scales::percent,limits = c(0,1))+
  scale_x_continuous(labels = scales::percent,limits = c(0,1))
###

CL  = kmeans(base[2:3], 2)
base$k2 = CL$cluster
k2_tot = CL$tot.withinss

CL  = kmeans(base[2:3], 3)
base$k3 = CL$cluster
k3_tot = CL$tot.withinss

CL  = kmeans(base[2:3], 4)
base$k4 = CL$cluster
k4_tot = CL$tot.withinss

library(factoextra)
fviz_nbclust(x = base[2:3], FUNcluster = kmeans, method = "wss", k.max = 10, 
             diss = get_dist(base[2:3], method = "euclidean"), nstart = 50)

#Cluster segun participacion de tamano grande y calificacion alta

part.grande <- perfiles %>% ungroup() %>% 
  select(Pais, particip.ocup, grupos.tamanio) %>% 
  filter(grupos.tamanio=="Grande") %>% 
  group_by(Pais) %>% 
  summarise(part.grande=sum(particip.ocup))

part.alta <- perfiles %>% ungroup() %>% 
  select(Pais, particip.ocup, grupos.calif) %>% 
  filter(grupos.calif=="Alta") %>% 
  group_by(Pais) %>% 
  summarise(part.alta=sum(particip.ocup))


base2 <- left_join(part.grande, part.alta, by="Pais") %>% 
  filter(!Pais %in% c("Estados Unidos", "Canada"))

CL  = kmeans(base[2:3], 2)
base2$k2 = CL$cluster

CL  = kmeans(base[2:3], 3)
base2$k3 = CL$cluster

CL  = kmeans(base[2:3], 4)
base2$k4 = CL$cluster


# Cluster segun la participacion de los 9 perfiles

part.perfiles <- perfiles %>% ungroup() %>% 
  select(Pais, particip.ocup, tamanio.calif) %>% 
  spread(tamanio.calif, particip.ocup) %>% 
  filter(!Pais %in% c("Estados Unidos", "Canada"))

CL  = kmeans(part.perfiles[2:10], 2)
part.perfiles$k2 = CL$cluster

CL  = kmeans(part.perfiles[2:10], 3)
part.perfiles$k3 = CL$cluster

CL  = kmeans(part.perfiles[2:10], 4)
part.perfiles$k4 = CL$cluster

CL  = kmeans(part.perfiles[2:10], 5)
part.perfiles$k5 = CL$cluster

set.seed(12)
fviz_nbclust(x = part.perfiles[2:10], FUNcluster = kmeans,method = "silhouette", k.max = 12, 
             diss = get_dist(part.perfiles[2:10], method = "euclidean"), nstart = 50)

###Pruebo iterar 10 veces el cluster con k = 3
resultado <- part.perfiles[1:10]
for (i in 1:10) {
    CL  = kmeans(part.perfiles[2:10], 3)
    resultado <-   bind_cols(resultado,
                           i = CL$cluster) 
names(resultado)[ncol(resultado)] <- paste0("itera",i)
    }

