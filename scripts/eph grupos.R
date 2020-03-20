library(eph)
library(tidyverse)
Variables_Continua <- c("CODUSU","NRO_HOGAR","COMPONENTE","ANO4","TRIMESTRE" ,"AGLOMERADO","H15",
                        "CH04", "CH06", "CH12","CH13","CH14","CH15","ESTADO","CAT_OCUP",
                        "PP04A", "PP04B_COD","PP07H","P21","PP04D_COD","PP04C","PP07C",
                        "PP07A","PP05B2_ANO","PP04B3_ANO","PP07E","NIVEL_ED","PONDIIO","PONDERA")

Bases_Continua <- readRDS("../../../Bases EPH/Adicionales/Base_Cont_29_aglom.RDS") 

###clasificacion de variables####
base_clasificada <- Bases_Continua %>%
  filter(ANO4 == 2017) %>% 
  eph::organize_ocupations() %>% 
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
      case_when(PP04C %in% 1:6 ~ "1 a 10",
                PP04C %in% 7:8 ~ "11 a 40",
                PP04C %in% 9:12 ~ "40 +",
                PP04C %in% 99 ~ "Ns/Nr"),
      levels = c("1 a 10","11 a 40","40 +","Ns/Nr")),
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
        PP04B_COD %in% c(9999) ~ "Ns.Nc"))

###Algunos conteos rapidos####
eph::calculate_tabulates(base_clasificada %>% filter(TRIMESTRE == 1),
                         "grupos.calif","grupos.nivel.ed",weights = "PONDERA")

###Caracteristicas Grupos####
desempleo.arg <- base_clasificada %>% 
  group_by(ANO4,TRIMESTRE,grupos.nivel.ed) %>% 
  summarise(PEA = sum(PONDERA[ESTADO %in% 1:2],na.rm = TRUE),
            ocupados = sum(PONDERA[ESTADO %in% 1],na.rm = TRUE),
            desocupados = sum(PONDERA[ESTADO %in% 2],na.rm = TRUE),
            tasa.desocup = desocupados/PEA) %>%
  group_by(ANO4,TRIMESTRE) %>% 
  mutate(PEA.porc = PEA/sum(PEA),
         ocup.porc = ocupados/sum(ocupados))

base.filtrada.ocupados <- base_clasificada %>% 
  filter(!(PP04B_COD %in% c(83:84,8300:8403)),ESTADO == 1)

salario.grupos.educac <- base.filtrada.ocupados %>% 
  filter(grupos.nivel.ed != "Ns/Nr",grupos.tamanio != "Ns/Nr") %>% 
  group_by(ANO4,TRIMESTRE,grupos.nivel.ed,grupos.tamanio) %>% 
  summarise(casos = n(),
            casos.pond = sum(PONDIIO),
            salario.mensual.prom = weighted.mean(P21,PONDIIO),
            salario.mensual.mediana = median(P21)) %>% 
  arrange(ANO4,TRIMESTRE,salario.mensual.prom)

salario.grupos.calif <- base.filtrada.ocupados %>% 
  filter(grupos.calif != "Ns/Nr",grupos.tamanio != "Ns/Nr") %>% 
  group_by(ANO4,TRIMESTRE,grupos.calif,grupos.tamanio) %>% 
  summarise(casos = n(),
            casos.pond = sum(PONDIIO),
            salario.mensual.prom = weighted.mean(P21,PONDIIO),
            salario.mensual.mediana = median(P21)) %>% 
  arrange(ANO4,TRIMESTRE,salario.mensual.prom)
