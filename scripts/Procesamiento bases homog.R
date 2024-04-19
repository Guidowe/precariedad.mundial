options(scipen = 999) 
setwd("~/GitHub/precariedad.mundial")
paises = c('Bolivia', 'Brasil', 'Chile', 'Paraguay', 'Peru', 'Uruguay')

for (pais in paises) {
#### Join de todas las bases y modificaciones generales ####
load(paste0("Bases_homog/", pais, ".Rdata"))
#Paso todos los TCP a Tamaño Pequeño
Base <- Base %>% 
  mutate(
    TAMA=factor(case_when(
      CATOCUP== "Cuenta propia" ~ "Pequeño", 
      TRUE                      ~ as.character(TAMA)), 
      levels= c("Pequeño", "Mediano", "Grande", "Ns/Nc")))

Resultados <- Base                                          %>%      
  filter(COND=="Ocupado" & CALIF!="Ns/Nc" & TAMA!="Ns/Nc" & CATOCUP!="Resto" & CATOCUP!= "Ns/Nc")   %>%
  group_by(PAIS, PERIODO, TAMA, CALIF)                     %>%
  summarise('periodo'                              = mean(ANO),         ## Ojo, hay una variable 'periodo' y otra 'PERIODO' que luego se descarta
            'casos'                                = n(),
            'ocupados'                             = sum(WEIGHT, na.rm=TRUE),
            'asalariados'                          = sum(WEIGHT[CATOCUP=="Asalariados"], na.rm=TRUE),
            'tcp'                                  = sum(WEIGHT[CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'tasa.asalarizacion'                   = sum(WEIGHT[CATOCUP=="Asalariados"], na.rm=TRUE)/sum(WEIGHT[CATOCUP=="Asalariados" | CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'seguridad.social.si.asal'             = sum(WEIGHT[PRECASEG=="Con aportes" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'seguridad.social.no.asal'             = sum(WEIGHT[PRECASEG=="Sin aportes" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'registrados.asal'                     = sum(WEIGHT[PRECAREG=="Registrado" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'no.registrados.asal'                  = sum(WEIGHT[PRECAREG=="No registrado" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'empleo.temporal.asal'                 = sum(WEIGHT[PRECATEMP=="Temporal" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'empleo.no.temporal.asal'              = sum(WEIGHT[PRECATEMP=="No temporal" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'part.involun.asal'                    = sum(WEIGHT[PRECAPT=="Part-time involuntario" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'part.volunt.asal'                     = sum(WEIGHT[PRECAPT=="Part-time voluntario" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'full.time.asal'                       = sum(WEIGHT[PRECAPT=="Tiempo completo" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'seguridad.social.si.tcp'              = sum(WEIGHT[PRECASEG=="Con aportes" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'seguridad.social.no.tcp'              = sum(WEIGHT[PRECASEG=="Sin aportes" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'registrados.tcp'                      = sum(WEIGHT[PRECAREG=="Registrado" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'no.registrados.tcp'                   = sum(WEIGHT[PRECAREG=="No registrado" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'empleo.temporal.tcp'                  = sum(WEIGHT[PRECATEMP=="Temporal" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'empleo.no.temporal.tcp'               = sum(WEIGHT[PRECATEMP=="No temporal" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'part.involun.tcp'                     = sum(WEIGHT[PRECAPT=="Part-time involuntario" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'part.volunt.tcp'                      = sum(WEIGHT[PRECAPT=="Part-time voluntario" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'full.time.tcp'                        = sum(WEIGHT[PRECAPT=="Tiempo completo" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'tasa.1.asalariados'                   = sum(WEIGHT[PRECACOUNT==1 | PRECACOUNT==2 | PRECACOUNT==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'tasa.2.asalariados'                   = sum(WEIGHT[PRECACOUNT==2 | PRECACOUNT==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'tasa.3.asalariados'                   = sum(WEIGHT[PRECACOUNT==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'promedio.ing.oc.prin'                 = weighted.mean(ING[CATOCUP=="Asalariados" | CATOCUP=="Cuenta propia"], WEIGHT[CATOCUP=="Asalariados" | CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'promedio.ing.oc.prin.asal'            = weighted.mean(ING[CATOCUP=="Asalariados"], WEIGHT[CATOCUP=="Asalariados"], na.rm=TRUE),
            'promedio.ing.oc.prin.tcp'             = weighted.mean(ING[CATOCUP=="Cuenta propia"], WEIGHT[CATOCUP=="Cuenta propia"], na.rm=TRUE))  %>%
  ungroup()                                                                    %>%
  mutate(   'tasa.partime.asal'                    = part.involun.asal/(part.involun.asal + part.volunt.asal + full.time.asal), 
            'tasa.seguridad.social.asal'           = seguridad.social.no.asal/(seguridad.social.si.asal+seguridad.social.no.asal),
            'tasa.no.registro.asal'                = no.registrados.asal/(no.registrados.asal+registrados.asal),
            'tasa.temp.asal'                       = part.involun.asal/(part.involun.asal+part.volunt.asal+part.volunt.asal),
         
            'tasa.partime.tcp'                     = part.involun.tcp/(part.involun.tcp + part.volunt.tcp + full.time.tcp), 
            'tasa.seguridad.social.tcp'            = seguridad.social.no.tcp/(seguridad.social.si.tcp+seguridad.social.no.tcp),
            'tasa.no.registro.tcp'                 = no.registrados.tcp/(no.registrados.tcp+registrados.tcp),
            'tasa.temp.tcp'                        = part.involun.tcp/(part.involun.tcp+part.volunt.tcp+part.volunt.tcp)) %>%          
  group_by(PAIS, PERIODO)                                                      %>%                         
  mutate('particip.ocup'           = ocupados/sum(ocupados),
         'particip.asal'           = asalariados/sum(asalariados),
         'particip.tcp'            = tcp/sum(tcp))                             %>%  
  ungroup()                                                                    %>%
  group_by(PAIS, TAMA, CALIF)                                                  %>% 
  mutate('total.casos'             = sum(casos))                               %>% 
  rename(Pais=PAIS, 
    grupos.tamanio=TAMA, 
    grupos.calif=CALIF)

#Resultados <-  Resultados[,c(1:5, 16, 17, 6:15, 18)]
Resultados[is.na(Resultados)] <- 0
Resultados <- Resultados %>%                        
  group_by(Pais, grupos.tamanio, grupos.calif) %>%
  summarise_each(funs(mean)) %>%
  select(-PERIODO)
Resultados   <-  Resultados  %>% 
  mutate(tamanio.calif= paste(grupos.tamanio, " - ", grupos.calif, sep=""),
         tamanio.calif2 = case_when(
           tamanio.calif == "Pequeño - Baja"  ~ "1) Pequeño - Baja",
           tamanio.calif == "Pequeño - Media" ~ "2) Pequeño - Media",
           tamanio.calif == "Pequeño - Alta" ~ "3) Pequeño - Alta",
           tamanio.calif == "Mediano - Baja" ~ "4) Mediano - Baja",
           tamanio.calif == "Mediano - Media" ~ "5) Mediano - Media",
           tamanio.calif == "Mediano - Alta" ~ "6) Mediano - Alta",
           tamanio.calif == "Grande - Baja" ~ "7) Grande - Baja",
           tamanio.calif == "Grande - Media" ~ "8) Grande - Media",
           tamanio.calif == "Grande - Alta" ~ "9) Grande - Alta"))

save(Resultados, file = paste0("Resultados/", pais, ".Rdata"))


#### Resultados sin desagregar por perfiles ####


Resultados2 <- Base                                          %>%      
  filter(COND=="Ocupado" & CALIF!="Ns/Nc" & TAMA!="Ns/Nc")   %>%
  group_by(PAIS, PERIODO)                                %>%
  summarise('periodo'                              = mean(ANO),
            'ocupados'                             = sum(WEIGHT, na.rm=TRUE),
            'asalariados'                          = sum(WEIGHT[CATOCUP=="Asalariados"], na.rm=TRUE),
            'tcp'                                  = sum(WEIGHT[CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'tasa.asalarizacion'                   = sum(WEIGHT[CATOCUP=="Asalariados"], na.rm=TRUE)/sum(WEIGHT[CATOCUP=="Asalariados" | CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'seguridad.social.si.asal'             = sum(WEIGHT[PRECASEG=="Con aportes" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'seguridad.social.no.asal'             = sum(WEIGHT[PRECASEG=="Sin aportes" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'registrados.asal'                     = sum(WEIGHT[PRECAREG=="Registrado" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'no.registrados.asal'                  = sum(WEIGHT[PRECAREG=="No registrado" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'empleo.temporal.asal'                 = sum(WEIGHT[PRECATEMP=="Temporal" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'empleo.no.temporal.asal'              = sum(WEIGHT[PRECATEMP=="No temporal" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'part.involun.asal'                    = sum(WEIGHT[PRECAPT=="Part-time involuntario" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'part.volunt.asal'                     = sum(WEIGHT[PRECAPT=="Part-time voluntario" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'full.time.asal'                       = sum(WEIGHT[PRECAPT=="Tiempo completo" & CATOCUP=="Asalariados"], na.rm=TRUE),
            'seguridad.social.si.tcp'              = sum(WEIGHT[PRECASEG=="Con aportes" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'seguridad.social.no.tcp'              = sum(WEIGHT[PRECASEG=="Sin aportes" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'registrados.tcp'                      = sum(WEIGHT[PRECAREG=="Registrado" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'no.registrados.tcp'                   = sum(WEIGHT[PRECAREG=="No registrado" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'empleo.temporal.tcp'                  = sum(WEIGHT[PRECATEMP=="Temporal" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'empleo.no.temporal.tcp'               = sum(WEIGHT[PRECATEMP=="No temporal" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'part.involun.tcp'                     = sum(WEIGHT[PRECAPT=="Part-time involuntario" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'part.volunt.tcp'                      = sum(WEIGHT[PRECAPT=="Part-time voluntario" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'full.time.tcp'                        = sum(WEIGHT[PRECAPT=="Tiempo completo" & CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'tasa.1.asalariados'                   = sum(WEIGHT[PRECACOUNT==1 | PRECACOUNT==2 | PRECACOUNT==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'tasa.2.asalariados'                   = sum(WEIGHT[PRECACOUNT==2 | PRECACOUNT==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'tasa.3.asalariados'                   = sum(WEIGHT[PRECACOUNT==3], na.rm=TRUE)/sum(WEIGHT, na.rm=TRUE),
            'promedio.ing.oc.prin'                 = weighted.mean(ING[CATOCUP=="Asalariados" | CATOCUP=="Cuenta propia"], WEIGHT[CATOCUP=="Asalariados" | CATOCUP=="Cuenta propia"], na.rm=TRUE),
            'promedio.ing.oc.prin.asal'            = weighted.mean(ING[CATOCUP=="Asalariados"], WEIGHT[CATOCUP=="Asalariados"], na.rm=TRUE),
            'promedio.ing.oc.prin.tcp'             = weighted.mean(ING[CATOCUP=="Cuenta propia"], WEIGHT[CATOCUP=="Cuenta propia"], na.rm=TRUE))  %>%
  ungroup()                                                                    %>%
  mutate(   'tasa.partime.asal'                    = part.involun.asal/(part.involun.asal + part.volunt.asal + full.time.asal), 
            'tasa.seguridad.social.asal'           = seguridad.social.no.asal/(seguridad.social.si.asal+seguridad.social.no.asal),
            'tasa.no.registro.asal'                = no.registrados.asal/(no.registrados.asal+registrados.asal),
            'tasa.temp.asal'                       = part.involun.asal/(part.involun.asal+part.volunt.asal+part.volunt.asal),
            
            'tasa.partime.tcp'                     = part.involun.tcp/(part.involun.tcp + part.volunt.tcp + full.time.tcp), 
            'tasa.seguridad.social.tcp'            = seguridad.social.no.tcp/(seguridad.social.si.tcp+seguridad.social.no.tcp),
            'tasa.no.registro.tcp'                 = no.registrados.tcp/(no.registrados.tcp+registrados.tcp),
            'tasa.temp.tcp'                        = part.involun.tcp/(part.involun.tcp+part.volunt.tcp+part.volunt.tcp)) %>% 
  rename(Pais=PAIS)

Resultados2[is.na(Resultados2)] <- 0
Resultados2 <- Resultados2 %>%                        
  group_by(Pais) %>%
  summarise_each(funs(mean)) %>%
  select(-PERIODO)

save(Resultados, file = paste0("Resultados/", pais, "_agregado.Rdata"))

}

