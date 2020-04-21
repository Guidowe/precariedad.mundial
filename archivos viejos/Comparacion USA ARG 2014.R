  ## ----setup, include=FALSE--------------------------------------------------------------------------
  knitr::opts_chunk$set(echo = FALSE,warning = FALSE,message = FALSE)
  knitr::opts_chunk$set(fig.width = 10)
  
  
  
  ## ----message=FALSE,cache.comments=FALSE------------------------------------------------------------
  library(ipumsr)
  library(eph)
  library(ggthemes)
  library(ggalt)
  library(tidyverse)
  library(kableExtra)
  library(formattable)
  library(openxlsx)
  library(Weighted.Desc.Stat)
  # Funcion de redondeo para presentación (queda como character)
  formato_porc <- function(numero, dec = 1){
    format(round(numero, digits = dec), nsmall = dec, decimal.mark = ",")
  }
  
  formato_pesos <- function(numero, dec = 2){
    paste0("$", format(round(numero, digits = dec), nsmall = dec, big.mark = ".", decimal.mark = ","))
  }
  
  formato_cantidad <- function(numero, dec = 0){
    format(round(numero, digits = dec), nsmall = dec, big.mark = ".", decimal.mark = ",")
  }
  
  
  ## ----message=FALSE,cache.comments=FALSE------------------------------------------------------------
  ###Estados Unidos####
  ocup_usa <- read.xlsx("data/Codigos Ocup USA.xlsx")
  ramas_usa <- read.xlsx("data/Codigos Ocup USA.xlsx",sheet = 2)
  
  cps_ddi_file <- "../bases/cps_00004.xml"
  cps_ddi_file_mas_variables <- "../bases/cps_00004.xml"
  cps_data_file <- "../bases/cps_00004.dat"
  
  cps_ddi <- read_ipums_ddi(cps_ddi_file) 
  cps_ddi_mas_variables <- read_ipums_ddi(cps_ddi_file_mas_variables) #
  
  # Base_USA <- ipumsr::read_ipums_micro(ddi = cps_ddi_file,
  #                                  data_file =  cps_data_file) %>%
  #     filter(ASECFLAG ==1,YEAR == 2019)
  # saveRDS(Base_USA,"../bases/Base_USA2019.RDS")
  #Base_USA<- readRDS("../bases/Base_USA2019.RDS")
  
  listado.variables <- cps_ddi[["var_info"]]
  listado.variables.completa <- cps_ddi_mas_variables[["var_info"]]
  Variables_categorias_Usa<- unnest(listado.variables,val_labels)
  
  ####Lineas para ver descripción y categorias de las variables####
  estadousa <- ipums_val_labels(cps_ddi, var = "CLASSWLY")
  #listado.variables$var_desc[listado.variables$var_name=="FIRMSIZE"]
  
  ###EPH##
  #Bases_eph <- readRDS("../bases/eph2019.RDS")
  Bases_eph <- eph::get_microdata(year = 2014,trimester = 1:4)
  
  bases_bind <- Bases_eph %>%
    dplyr::select(microdata) %>%
    tidyr::unnest(cols = c(microdata))
  
  ## ----clasifico-------------------------------------------------------------------------------------
  ####USA####
  Base_USA.c <- Base_USA %>% 
    mutate(grupos.tamanio = factor(case_when(FIRMSIZE==1~"Pequeño",
                                     FIRMSIZE %in% 2:4~"Mediano",
                                     FIRMSIZE %in% 5:9~"Grande",
                                     FIRMSIZE %in% 0 ~ "Ns/Nr"),
                          levels = c("Pequeño","Mediano","Grande","Ns/Nr")),
           grupos.nivel.ed = factor(case_when(EDUC %in% 2:72~ "Menor a Secundaria",
                                 EDUC %in% 73:110~ "Secundaria Completa",
                                 EDUC %in% 111:125~ "Superior Completo",
                                 TRUE ~ "Ns/Nr"),
                              c("Menor a Secundaria","Secundaria Completa","Superior Completo")),
            Categoria =  case_when(CLASSWLY %in% 10:19 ~ "Patrones y CP",
                             CLASSWLY  %in% 20:28 ~ "Asalariados",
                             CLASSWLY == 29 ~ "TFSR",
                             CLASSWLY == 99 ~ "Ns/Nr"),
           precario.part = case_when(FULLPART == 2 & WHYPTLY %in% c(1,3,4)~"Precario",
                                     TRUE ~ "Resto"))
  table(Base_EPH.c$sector_actividad) 
  ####ARGENTINA####
  Base_EPH.c <- bases_bind %>%
    eph::organize_ocupations() %>% 
    mutate(PP04B_COD = PP04B_CAES) %>% 
    mutate(
      grupos.calif = factor(
        case_when(
          CALIFICACION %in% c("Profesionales","Técnicos") ~ "Alta",
          CALIFICACION ==   "Operativos" ~ "Media",
          CALIFICACION ==   "No calificados" ~ "Baja"),
        levels = c("Baja","Media","Alta")),
      grupos.nivel.ed = factor(
        case_when(NIVEL_ED %in% c(7,1,2,3) ~ "Menor a Secundaria",
                  NIVEL_ED %in% c(4,5) ~ "Secundaria Completa",
                  NIVEL_ED == 6 ~ "Superior Completo",
                  TRUE ~ "Ns/Nr"),
        levels = c("Menor a Secundaria","Secundaria Completa","Superior Completo")),
      grupos.tamanio = factor(
        case_when(PP04C %in% 1:6  |(PP04C %in% 99 & PP04C99 == 1)~ "Pequeño",
                  PP04C %in% 7:8  |(PP04C %in% 99 & PP04C99 == 2)~ "Mediano",
                  PP04C %in% 9:12 |(PP04C %in% 99 & PP04C99 == 3)~ "Grande",
                  PP04C %in% 99 & PP04C99 == 9 ~ "Ns/Nr"),
        levels = c("Pequeño","Mediano","Grande","Ns/Nr")),
      sector_actividad = 
        case_when(
          PP04B_COD %in% c(1,0101:0300) ~ "Agricultura",
          PP04B_COD %in% c(0500:0900) ~ "Minería",
          PP04B_COD %in% c(10:33,1001:3300) ~ "Ind. Manufacturera",
          PP04B_COD %in% c(35:39,3501:3900) ~ "Elec, Gas y Agua",
          PP04B_COD %in% c(40,4000)  ~ "Construcción", 
          PP04B_COD %in% c(45:48,4501:4811) ~ "Comercio y Reparaciones",
          PP04B_COD %in% c(49:53,4901:5300) ~ "Transporte",
          PP04B_COD %in% c(55:56,5500:5602) ~ "Hoteles y Restaurantes",
          PP04B_COD %in% c(58:63,5800:6300) ~ "Información y Comunicaciones",
          PP04B_COD %in% c(64:66,6400:6600) ~ "Serv. Financieros",
          PP04B_COD %in% c(68,6800)   ~ "Serv. Inmobiliarios",
          PP04B_COD %in% c(69:75,6900:7500) ~ "Act. Profes., Cient. y Técnicas",
          PP04B_COD %in% c(77:82,7701:8200) ~ "Serv. Administrativos",
          PP04B_COD %in% c(83:84,8300:8403) ~ "Adm. Pública",
          PP04B_COD %in% c(85,8501:8509) ~ "Educacion", 
          PP04B_COD %in% c(86,8600:8800) ~ "Salud", 
          PP04B_COD %in% c(90:93,9000:9302) ~ "Arte, Entreten. y Recreación", 
          PP04B_COD %in% c(94:96,9401:9609) ~ "Otros Servicios",
          PP04B_COD %in% c(97:98,9700:9800) ~ "Serv Domestico",
          PP04B_COD %in% c(99,9900) ~ "Org Extraterritorial",
          PP04B_COD %in% c(9999) ~ "Ns.Nc"),
      Categoria =  case_when(CAT_OCUP == 1 ~ "Patrones",
                             CAT_OCUP == 2 ~ "TCP",
                             CAT_OCUP == 3 ~ "Asalariados",
                             CAT_OCUP == 4 ~ "TFSR",
                             CAT_OCUP == 9 ~ "Ns/Nr"),
               precario.part = case_when(ESTADO == 1 & INTENSI == 1 ~"Precario",
                                     TRUE ~ "Resto"))
  
  
  ## --------------------------------------------------------------------------------------------------
  insercion.niveles.arg <- Base_EPH.c %>% 
    group_by(ANO4,TRIMESTRE,grupos.nivel.ed) %>% 
    summarise(PEA = sum(PONDERA[ESTADO %in% 1:2],na.rm = TRUE),
              ocupados = sum(PONDERA[ESTADO %in% 1],na.rm = TRUE),
              desocupados = sum(PONDERA[ESTADO %in% 2],na.rm = TRUE),
              tasa.desocup = desocupados/PEA) %>%
    group_by(ANO4,TRIMESTRE) %>% 
    mutate(PEA.porc = PEA/sum(PEA),
           ocup.porc = ocupados/sum(ocupados)) %>% 
    ungroup() %>% 
    select(-TRIMESTRE,-PEA,-ocupados,-desocupados)
  
  
  
  insercion.niveles.arg
  
  
  ## --------------------------------------------------------------------------------------------------
  insercion.niveles.usa <- Base_USA.c %>% 
    filter(grupos.nivel.ed != "ns/nr") %>% 
    group_by(YEAR,grupos.nivel.ed) %>% 
    summarise(PEA = sum(ASECWT[EMPSTAT %in% 1:22],na.rm = TRUE),
              ocupados = sum(ASECWT[EMPSTAT %in% 1:12],na.rm = TRUE),
              desocupados = sum(ASECWT[EMPSTAT %in% 20:22],na.rm = TRUE),
              tasa.desocup = desocupados/PEA)%>%
    group_by(YEAR) %>% 
    mutate(PEA.porc = PEA/sum(PEA),
           ocup.porc = ocupados/sum(ocupados))%>% 
    select(-PEA,-ocupados,-desocupados)
  
  insercion.niveles.usa  
  
  
  ## ----filtros---------------------------------------------------------------------------------------
  eph.ocup.privados <- Base_EPH.c %>% 
    filter(!(PP04B_COD %in% c(83:84,8300:8403)),
           sector_actividad!= "Serv Domestico",
           ESTADO == 1) %>% 
    mutate(Pais = "ARG",
           PONDERA_SALARIOS = PONDERA) %>% 
    rename(IOP = P21)
  
  
  usa.ocup.privados <- Base_USA.c %>% 
    filter(INDLY <9370,#sin Sector publico
           INDLY <9290,#sin Sector publico ni S. doméstico 
           EMPSTAT %in%  1:12)%>% 
    mutate(Pais = "USA",
           PONDERA_SALARIOS = ASECWT,
           IOP = INCWAGE/12,
           IOP2 = EARNWEEK*4,
           ANO4 = YEAR,
           PONDERA = ASECWT,
           TRIMESTRE = 1)
  
  variables_comunes <- 
    c("ANO4","TRIMESTRE","Pais","IOP","PONDERA","PONDERA_SALARIOS",
      "grupos.nivel.ed","grupos.tamanio","Categoria",
      "precario.part")
  
  
  base.unica <- bind_rows(eph.ocup.privados %>% select(variables_comunes),
              usa.ocup.privados %>% select(variables_comunes))
  
  
  ## --------------------------------------------------------------------------------------------------
  distrib.grupos<- base.unica %>% 
    filter(grupos.tamanio != "Ns/Nr") %>% 
    group_by(grupos.nivel.ed,grupos.tamanio,Pais,ANO4,TRIMESTRE) %>% 
    summarise(ocupados=n(),
              ocupados.pond = sum(PONDERA,na.rm = TRUE),
              asalariados.pond = sum(PONDERA[Categoria=="Asalariados"]),
              tasa.asalarizacion = asalariados.pond/ocupados.pond,
              subocup.horaria.ocup = sum(PONDERA[precario.part=="Precario"]),
              subocup.horaria.asal = sum(PONDERA[precario.part=="Precario" &
                                                   Categoria=="Asalariados"]),
              ingreso.mensual.prom = weighted.mean(IOP,PONDERA_SALARIOS),
              ingreso.mensual.mediana = median(IOP),
              ingreso.coef.variacion = w.cv(IOP,PONDERA_SALARIOS),
              salario.mensual.prom = weighted.mean(IOP[Categoria=="Asalariados"],
                                                   PONDERA_SALARIOS[Categoria=="Asalariados"]),
              salario.mensual.mediana = median(IOP[Categoria=="Asalariados"]),
              salario.coef.variacion = w.cv(IOP[Categoria=="Asalariados"],
                                            PONDERA_SALARIOS[Categoria=="Asalariados"])) %>% 
    group_by(Pais,ANO4,TRIMESTRE) %>% 
    mutate(Particip_emp = ocupados.pond/sum(ocupados.pond)*100,
           Particip_emp_asalariad = asalariados.pond/sum(asalariados.pond)*100,
           tasa.subocup.horaria.ocup = subocup.horaria.ocup/ocupados.pond*100,
           tasa.subocup.horaria.asal = subocup.horaria.asal/asalariados.pond*100) %>% 
    ungroup()
  
  
  indicadores.todos <- distrib.grupos %>% 
    group_by(grupos.nivel.ed,grupos.tamanio,Pais,ANO4) %>% 
    summarise_all(mean, na.rm = TRUE) %>% 
    select(-TRIMESTRE) %>% 
    group_by(Pais,grupos.nivel.ed) %>% 
    mutate(Penalidad.salario.tamanio = salario.mensual.prom/
             salario.mensual.prom[grupos.tamanio=="Grande"],
           Penalidad.ingreso.tamanio = ingreso.mensual.prom/
             ingreso.mensual.prom[grupos.tamanio=="Grande"])%>% 
    group_by(Pais,grupos.tamanio) %>% 
    mutate(Penalidad.salario.educacion = salario.mensual.prom/salario.mensual.prom[grupos.nivel.ed=="Superior Completo"],
           Penalidad.ingreso.educacion = ingreso.mensual.prom/ingreso.mensual.prom[grupos.nivel.ed=="Superior Completo"])

  
  
