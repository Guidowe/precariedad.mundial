library(tidyverse)
####Data Perfiles####
load("Resultados/ResultadosFacu.RDATA")
argentina<- readRDS("Resultados/Argentina.RDS")
canada <- readRDS("Resultados/Canada.RDS")
colombia <- readRDS("Resultados/Colombia.RDS")
costa.rica <- readRDS("Resultados/Costa Rica.RDS")
ecuador <- readRDS("Resultados/Ecuador.RDS")
el.salvador <- readRDS("Resultados/El Salvador.RDS")
estados.unidos <- readRDS("Resultados/Estados Unidos.RDS")
guatemala <- readRDS("Resultados/Guatemala.RDS")
mexico <- readRDS("Resultados/Mexico.RDS")

####Data Agregada####
load("Resultados/Resultados_agregados_Facu.RDATA")
argentina.agregado<- readRDS("Resultados/Argentina_agregado.RDS") %>% 
  filter(ANO4==2019) %>% 
  rename(periodo = ANO4)
canada.agregado <- readRDS("Resultados/Canada_agregado.RDS")
colombia.agregado <- readRDS("Resultados/Colombia_agregado.RDS")
costa.rica.agregado <- readRDS("Resultados/Costa Rica_agregado.RDS")
ecuador.agregado <- readRDS("Resultados/Ecuador_agregado.RDS")
el.salvador.agregado <- readRDS("Resultados/El Salvador_agregado.RDS")
estados.unidos.agregado <- readRDS("Resultados/Estados Unidos_agregado.RDS")
guatemala.agregado <- readRDS("Resultados/Guatemala_agregado.RDS")
mexico.agregado <- readRDS("Resultados/Mexico_agregado.RDS")

############Uno Perfiles#####
perfiles <- 
  bind_rows(canada,
            colombia,
            costa.rica,
            ecuador %>% mutate(periodo = as.numeric(periodo)),
            el.salvador,
            estados.unidos %>% filter(periodo==2018),
            guatemala,
            mexico,
            Resultados  %>% rename(particip.no.asal = particip.noasal),
            argentina %>% filter(periodo==2019)) %>% 
  select(Pais,tamanio.calif,tamanio.calif2,everything()) %>% 
  group_by(Pais,periodo) %>% 
  mutate(salario.promedio.pais = weighted.mean(x = promedio.ing.oc.prin.asal,
                                               w = asalariados)) %>% 
  ungroup() %>% 
  mutate(prima.salario.medio = promedio.ing.oc.prin.asal/salario.promedio.pais)   %>% 
  ungroup() 


perfiles.tidy <- perfiles %>% 
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

##########Uno datos agregados#####
agregado <- 
  bind_rows(mexico.agregado,
            argentina.agregado,
            canada.agregado,
            colombia.agregado,
            costa.rica.agregado,
            ecuador.agregado %>% mutate(periodo = as.numeric(periodo)),
            el.salvador.agregado,
                        estados.unidos.agregado %>% filter(periodo==2018),
            guatemala.agregado,
            Resultados2) %>%
  mutate(tamanio.calif = "Total") %>% 
  select(Pais,tamanio.calif,everything())


####Exporto bases unificadas#####
saveRDS(perfiles,file = "Resultados/America.RDS")
saveRDS(agregado,file = "Resultados/America_agregado.RDS")
save(list = c("perfiles","perfiles.tidy","agregado"),
     file = "Precariedad.app/datashiny.RDATA")

