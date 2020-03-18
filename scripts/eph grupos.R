library(eph)
Variables_Continua <- c("CODUSU","NRO_HOGAR","COMPONENTE","ANO4","TRIMESTRE" ,"AGLOMERADO","H15",
                        "CH04", "CH06", "CH12","CH13","CH14","CH15","ESTADO","CAT_OCUP",
                        "PP04A", "PP04B_COD","PP07H","P21","PP04D_COD","PP04C",
                        "PP07A","PP05B2_ANO","PP04B3_ANO","PP07E","NIVEL_ED","PONDIIO","PONDERA")

Bases <- eph::get_microdata(year = 2017,trimester = 1:4,vars = Variables_Continua)

base_2017 <- Bases %>%
  dplyr::filter(year==2018) %>%
  dplyr::select(microdata) %>%
  tidyr::unnest(microdata)
