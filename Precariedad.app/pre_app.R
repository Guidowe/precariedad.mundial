library(plotly)
library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(ggthemes)

####Paleta####
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


####Data####
load("Resultados/ResultadosGuido.RDATA")
load("Resultados/ResultadosAL.RDATA")
load("Resultados/ResultadoArg.RDATA")
resultados.todos <- resultados  %>% 
  bind_rows(arg.resultado %>% mutate(Pais = "Argentina") %>% filter(periodo==2019) ) %>% 
  bind_rows(Resultados %>% rename(particip.no.asal = particip.noasal)) %>% 
  select(1:(ncol(.)-2)) 
  

tabla <-  resultados.todos %>% 
  # mutate(
  #    across(
  #    .cols = 4:ncol(.),
  #    .fns = ~round(.x, digits = 2))) %>% 
  ungroup()
  

resultados <- resultados.todos %>% 
  pivot_longer(cols = 5:ncol(.),
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

save(list = c("tabla","resultados"),
     file = "Precariedad.app/datashiny.RDATA")

