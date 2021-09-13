setwd("~/GitHub/precariedad.mundial")

library(tidyverse)

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

CL  = kmeans(base[2:3], 2)
base$k2 = CL$cluster

CL  = kmeans(base[2:3], 3)
base$k3 = CL$cluster

CL  = kmeans(base[2:3], 4)
base$k4 = CL$cluster

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




