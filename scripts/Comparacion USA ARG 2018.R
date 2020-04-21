  ## ----setup, include=FALSE--------------------------------------------------------------------------
  knitr::opts_chunk$set(echo = FALSE,warning = FALSE,message = FALSE)
  knitr::opts_chunk$set(fig.width = 10)
  
  
  
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
  
  
  ###Estados Unidos####
  ocup_usa <- read.xlsx("data/Codigos Ocup USA.xlsx")
  ramas_usa <- read.xlsx("data/Codigos Ocup USA.xlsx",sheet = 2)
  
  cps_ddi_file <- "../bases/cps_00004.xml"
  cps_data_file <- "../bases/cps_00004.dat"
  
  cps_ddi <- read_ipums_ddi(cps_ddi_file) 
  
  # Base_USA <- ipumsr::read_ipums_micro(ddi = cps_ddi_file,
  #                                  data_file =  cps_data_file) %>%
  #     filter(ASECFLAG ==1,YEAR == 2018)
  # saveRDS(Base_USA,"../bases/Base_USA2018.RDS")
  Base_USA<- readRDS("../bases/Base_USA2018.RDS")
  
  listado.variables.USA <- cps_ddi[["var_info"]]
  variables.categorias.USA<- unnest(listado.variables,val_labels)
  
  Variables.USA <- c("FIRMSIZE","EDUC","LABFORCE","EMPSTAT",
                     "CLASSWKR","CLASSWLY",
                     "WKSTAT","FULLPART",
                     "WHYPTLY","WHYPTLWK",
                     "IND","INDLY",
                     "OCC","OCCLY",
                     "EARNWEEK","PENSION",
                     "INCWAGE","INCBUS","INCLONGJ","SRCEARN",
                     "INCTOT","EARNWEEK",
                     "ASECWT","EARNWT") 
    
  var.cat.utilizadas.USA <- variables.categorias.USA %>% 
    filter(var_name %in% Variables.USA)
  
  var.utilizadas.USA <- listado.variables %>% 
    filter(var_name %in% Variables.USA)
  
  ####Lineas para ver descripción y categorias de las variables####
  estadousa <- ipums_val_labels(cps_ddi, var = "WHYPTLY")
  #listado.variables$var_desc[listado.variables$var_name=="FIRMSIZE"]
  
  ###EPH##
  #Bases_eph <- readRDS("../bases/eph2019.RDS")
  Bases_eph <- eph::get_microdata(year = 2018,trimester = 1:4)
  
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
           pension = case_when(PENSION %in% 1:2 ~ "No",
                               PENSION %in% 3 ~ "Si",
                               PENSION %in% 0 ~ "NIU"),
           precario.part = case_when(FULLPART == 2 & WHYPTLY %in% c(1,3,4)~"Precario",
                                     TRUE ~ "Resto"))
table(Base_USA.c$FIRMSIZE)
  
  Ingresos  <- Base_USA.c %>% 
    select(FIRMSIZE,EMPSTAT,WORKLY,OCC,OCCLY,INCWAGE,INCBUS,INCTOT,ASECWT,ELIGORG,EARNWEEK,EARNWT) %>% 
    filter(EARNWEEK>0,EARNWEEK!=9999.99,ELIGORG==1) %>% 
    filter(WORKLY == 2)
  
    ####ARGENTINA####
  Base_EPH.c <- bases_bind %>%
    eph::organize_ocupations() %>% 
    #mutate(PP04B_COD = PP04B_CAES) %>% 
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
      descuento_jubil = case_when(PP07H == 1 ~ "Si",
                                  PP07H == 2 ~ "No",
                                  PP07H == 0 ~ "0"),
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
  
  
  
  ## ----filtros---------------------------------------------------------------------------------------
  eph.ocup.privados <- Base_EPH.c %>% 
    filter(!(PP04B_COD %in% c(83:84,8300:8403)),
           sector_actividad!= "Serv Domestico",
           ESTADO == 1) %>% 
    mutate(Pais = "ARG",
           PONDERA_SALARIOS = PONDIIO) %>% 
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
  
  
  base.unica <- bind_rows(
    eph.ocup.privados %>% select(variables_comunes,descuento_jubil),
    usa.ocup.privados %>% select(variables_comunes,pension)) %>% 
    rownames_to_column(var = "Id") %>% 
    filter(grupos.tamanio != "Ns/Nr",!is.na(grupos.tamanio),
           grupos.nivel.ed != "Ns/Nr",!is.na(grupos.tamanio))
  

set.seed(68150)    
Decil <- base.unica %>%
  filter( IOP>0) %>% 
  mutate(IOP_d = IOP+runif(nrow(.),min = -.01,max =.01)) %>% 
  group_by(Pais,ANO4,TRIMESTRE) %>% 
  mutate(Decil = statar::xtile(IOP_d,n=10,w = PONDERA_SALARIOS)) %>%
  group_by(Pais,ANO4,TRIMESTRE,Categoria) %>% 
  mutate(Decil.cat = statar::xtile(IOP_d,n=10,w = PONDERA_SALARIOS)) %>%
  ungroup() %>% 
  select(Id,Decil,Decil.cat)
  
#########OCUPADOS#############################################
  perfiles.ocupados <- base.unica %>% 
    filter(grupos.tamanio != "Ns/Nr",grupos.nivel.ed != "Ns/Nr") %>% 
    left_join(Decil) %>% 
    group_by(grupos.nivel.ed,grupos.tamanio,Pais,ANO4,TRIMESTRE) %>% 
    summarise(casos.muestrales=n(),
              total = sum(PONDERA,na.rm = TRUE),
              asalariados = sum(PONDERA[Categoria=="Asalariados"]),
              tasa.asalarizacion = asalariados/total,
              subocup.horaria = sum(PONDERA[precario.part=="Precario"]),
              tasa.subocup.horaria = subocup.horaria/total*100,
              s_desc_jubilat =sum(PONDERA[descuento_jubil=="No"],na.rm = T),
              c_desc_jubilat =sum(PONDERA[descuento_jubil=="Si"],na.rm = T),
              s_pension =sum(PONDERA[pension=="No"],na.rm = T),
              c_pension =sum(PONDERA[pension=="Si"],na.rm = T),
              tasa.s.desc.jubil = s_desc_jubilat/(c_desc_jubilat+s_desc_jubilat),
              tasa.s.pension = s_pension/(c_pension+s_pension),
              ingreso.mensual.prom = weighted.mean(IOP[IOP>0],
                                                   PONDERA_SALARIOS[IOP>0]),
              ingreso.mensual.mediana = median(IOP[IOP>0]),
              ingreso.coef.variacion = w.cv(IOP[IOP>0],
                                            PONDERA_SALARIOS[IOP>0]),
              decil.promedio = weighted.mean(Decil,PONDERA_SALARIOS,na.rm = T)) %>% 
    group_by(Pais,ANO4,TRIMESTRE) %>% 
    mutate(Particip_emp = total/sum(total)*100) %>% 
    ungroup()

#Ver <- base.unica %>% filter(IOP %in% c(-9,0,9,99,999,9999,99999,999999,9999999))  

    indicadores.anuales.ocupados <- perfiles.ocupados %>% 
    group_by(grupos.nivel.ed,grupos.tamanio,Pais,ANO4) %>% 
    summarise_all(mean, na.rm = TRUE) %>% 
    select(-TRIMESTRE) %>% 
    group_by(Pais,grupos.nivel.ed) %>% 
    mutate(Penalidad.ingreso.tamanio = ingreso.mensual.prom/
             ingreso.mensual.prom[grupos.tamanio=="Grande"])%>% 
    group_by(Pais,grupos.tamanio) %>% 
    mutate(Penalidad.ingreso.educacion = ingreso.mensual.prom/ingreso.mensual.prom[grupos.nivel.ed=="Superior Completo"]) %>% 
      arrange(ANO4,Pais,grupos.tamanio)

#########Asalariados#############################################
    perfiles.asalariados <- base.unica %>% 
      filter(Categoria =="Asalariados") %>% 
      filter(grupos.tamanio != "Ns/Nr",grupos.nivel.ed != "Ns/Nr") %>% 
      left_join(Decil) %>% 
      group_by(grupos.nivel.ed,grupos.tamanio,Pais,ANO4,TRIMESTRE) %>% 
      summarise(casos.muestrales=n(),
                total = sum(PONDERA,na.rm = TRUE),
                asalariados = sum(PONDERA[Categoria=="Asalariados"]),
                tasa.asalarizacion = asalariados/total,
                subocup.horaria = sum(PONDERA[precario.part=="Precario"]),
                tasa.subocup.horaria = subocup.horaria/total*100,
                s_desc_jubilat =sum(PONDERA[descuento_jubil=="No"],na.rm = T),
                c_desc_jubilat =sum(PONDERA[descuento_jubil=="Si"],na.rm = T),
                s_pension =sum(PONDERA[pension=="No"],na.rm = T),
                c_pension =sum(PONDERA[pension=="Si"],na.rm = T),
                tasa.s.desc.jubil = s_desc_jubilat/(c_desc_jubilat+s_desc_jubilat),
                tasa.s.pension = s_pension/(c_pension+s_pension),
                ingreso.mensual.prom = weighted.mean(IOP[IOP>0],
                                                     PONDERA_SALARIOS[IOP>0]),
                ingreso.mensual.mediana = median(IOP[IOP>0]),
                ingreso.coef.variacion = w.cv(IOP[IOP>0],
                                              PONDERA_SALARIOS[IOP>0]),
                decil.promedio = weighted.mean(Decil.cat,PONDERA_SALARIOS,na.rm = T)) %>% 
      group_by(Pais,ANO4,TRIMESTRE) %>% 
      mutate(Particip_emp = total/sum(total)*100) %>% 
      ungroup()
    
    #Ver <- base.unica %>% filter(IOP %in% c(-9,0,9,99,999,9999,99999,999999,9999999))  
    
    indicadores.anuales.asalariados <- perfiles.ocupados %>% 
      group_by(grupos.nivel.ed,grupos.tamanio,Pais,ANO4) %>% 
      summarise_all(mean, na.rm = TRUE) %>% 
      select(-TRIMESTRE) %>% 
      group_by(Pais,grupos.nivel.ed) %>% 
      mutate(Penalidad.ingreso.tamanio = ingreso.mensual.prom/
               ingreso.mensual.prom[grupos.tamanio=="Grande"])%>% 
      group_by(Pais,grupos.tamanio) %>% 
      mutate(Penalidad.ingreso.educacion = ingreso.mensual.prom/ingreso.mensual.prom[grupos.nivel.ed=="Superior Completo"]) %>% 
      arrange(ANO4,Pais,grupos.tamanio)
    
    
        
    
    
write.xlsx(x = list("Ocupados" = indicadores.anuales.ocupados,
                    "Asalariados" = indicadores.anuales.asalariados),
           file = "Resultados/Arg_USA_2018.xlsx")  
write.xlsx(x = list("Definicion" = var.utilizadas.USA, 
                    "Definic y categorias"=var.cat.utilizadas.USA,
                    "Variables disponibles"=listado.variables.USA),
           file = "Resultados/listado.variables.xlsx")  
  
