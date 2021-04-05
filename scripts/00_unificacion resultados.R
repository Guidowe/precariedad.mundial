library(tidyverse)
####Data####
load("Resultados/ResultadosAL.RDATA")

argentina<- readRDS("Resultados/Argentina.RDS")
canada <- readRDS("Resultados/Canada.RDS")
colombia <- readRDS("Resultados/Colombia.RDS")
costa.rica <- readRDS("Resultados/Costa Rica.RDS")
ecuador <- readRDS("Resultados/Ecuador.RDS")
el.salvador <- readRDS("Resultados/El Salvador.RDS")
guatemala <- readRDS("Resultados/Guatemala.RDS")
mexico <- readRDS("Resultados/Mexico.RDS")

resultados.todos <- 
 #bind_rows(argentina %>% filter(periodo==2019)) %>% 
  bind_rows(canada,
            colombia,
            costa.rica,
            ecuador,
            el.salvador,
            guatemala,
            mexico,
            Resultados  %>% rename(particip.no.asal = particip.noasal)) %>% 
  select(Pais,tamanio.calif,tamanio.calif2,everything())
  

tabla <-  resultados.todos %>% 
  mutate(
     across(
     .cols = 7:ncol(.),
     .fns = ~round(.x, digits = 2))) %>%
  ungroup()
  

resultados <- resultados.todos %>% 
  pivot_longer(cols = 7:ncol(.),
               names_to = "Serie",
               values_to = "Valor") %>% 
  mutate(tamanio.calif = paste0(grupos.tamanio," - ",grupos.calif),
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
                               "Grande - Alta")))%>% 
  ungroup()

saveRDS(resultados.todos,file = "Resultados/America.RDS")
save(list = c("tabla","resultados"),
     file = "Precariedad.app/datashiny.RDATA")

