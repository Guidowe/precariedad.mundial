## Sacar datos de apropiacion del ingreso por decil (a nivel nacional) de EUROSTAT

library(tidyverse)
library(xlsx)

Tabla <- read.table(file = "C:/Users/facun/Documents/Investigación/1. LFS 2020 - CEPED Precariedad mundial/ilc_di01.tsv", sep = '\t', header = TRUE)

Tabla2 <- rename(Tabla, varEU= quantile.indic_il.currency.geo.time)

Tabla2 <- Tabla2 %>%
  mutate(
    quantile= substr(varEU, 1, 1), 
    id=       substr(varEU, 4, 12),
    #Para agarrar a los de decil 10
    id= case_when(id==",SHARE,NA" ~ "SHARE,NAC", 
              TRUE ~ id))

Tabla2 <- Tabla2 %>%
  filter(quantile=="D" & id=="SHARE,NAC") %>%
  mutate(id=       substr(varEU, 4, 12),
         decil=    substring(varEU, 2, 2),
         decil=    case_when(id==",SHARE,NA" ~ "10", 
                              TRUE          ~ decil))
 
Tabla2 <- Tabla2 %>%
  mutate(pais=case_when(decil %in% 1:9 ~ substr(varEU, 14, 15),
                        decil=="10"      ~ substr(varEU, 15, 16)))

Tabla2$id <- paste0(Tabla2$pais, Tabla2$decil)

Tabla2 <- Tabla2 %>%
  select(-varEU, -quantile, -pais, -decil) %>%
  select(id, everything())

Tabla2Tidy <- gather(Tabla2, ano, apropiacion, 2:25)

Tabla2Tidy <- Tabla2Tidy %>%
  mutate(pais=substr(id, 1, 2),
         decil=substr(id, 3, 4), 
         ano=substr(ano, 2, 5)) %>%
  select(ano, pais, decil, apropiacion, -id)

Tabla2Tidy <- Tabla2Tidy %>% 
  filter(pais %in% c("ES", "FR", "UK", "DE", "GR", "IT", "PT", "DK", "SE", "BG", "RO")) %>%
  filter(ano %in% 2008:2018)


## Sacar datos de apropiacion de limites de decil (a nivel nacional) de EUROSTAT

Tabla3 <- rename(Tabla, varEU= quantile.indic_il.currency.geo.time)

Tabla3 <- Tabla3 %>%
  mutate(
    quantile= substr(varEU, 1, 1), 
    id=       substr(varEU, 4, 10))

Tabla3 <- Tabla3 %>%
  filter(quantile=="D" & id=="TC,NAC,") %>%
  mutate(pais=       substr(varEU, 11, 12),
         decil=    substring(varEU, 2, 2))

Tabla3$id <- paste0(Tabla3$pais, Tabla3$decil)

Tabla3 <- Tabla3 %>%
  select(-varEU, -quantile, -pais, -decil) %>%
  select(id, everything())

Tabla3Tidy <- gather(Tabla3, ano, limite, 2:25)

Tabla3Tidy <- Tabla3Tidy %>%
  mutate(pais=substr(id, 1, 2),
         decil=substr(id, 3, 3), 
         ano=substr(ano, 2, 5)) %>%
  select(ano, pais, decil, limite, -id)

Tabla3Tidy$ano <- as.numeric(Tabla3Tidy$ano)

Tabla3Tidy <- Tabla3Tidy %>% 
  filter(pais %in% c("ES", "FR", "UK", "DE", "GR", "IT", "PT", "DK", "SE", "BG", "RO")) %>%
  filter(ano %in% 2008:2018)

GE <- Tabla3Tidy %>% filter(pais=="DE")


CarpetaRdos <- "C:/Users/facun/Documents/Investigación/1. LFS 2020 - CEPED Precariedad mundial/Resultados/"

write.xlsx(as.data.frame(Tabla2Tidy), paste0(CarpetaRdos, "DecilesTidy.xlsx"), sheetName = "Apropiacion", append=TRUE, row.names = FALSE )
write.xlsx(as.data.frame(Tabla3Tidy), paste0(CarpetaRdos, "DecilesTidy.xlsx"), sheetName = "Limites", append=TRUE, row.names = FALSE )




           