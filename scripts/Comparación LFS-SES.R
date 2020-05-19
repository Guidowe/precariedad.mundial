library(xlsx)
library(tidyverse)

## Comparación SES-LFS

LFS <- readRDS(paste0("F:/LFS/BasesUnificadas/SeleccionPaises2018.Rda"))

SES <- readRDS(paste0("F:/SES/BaseUnificada.Rda"))    

OcupadosTamaLFS <- LFS                                              %>%
  filter(COND=="Ocupado" &  TAMA!="Ns/Nc" & STAPRO==3)              %>%
  group_by(COUNTRY, TAMA)                                           %>%
  summarise('TotalLFS'                  = sum(WEIGHT, na.rm=TRUE))  %>%
  ungroup()                                                         %>%
  group_by(COUNTRY)                                                 %>%
  mutate('ParticipLFS'          = TotalLFS/sum(TotalLFS))           


OcupadosTamaSES <- SES                              %>% 
  group_by(PAIS, A12_CLASS)                         %>%
  summarise('TotalSES' =sum(WEIGHT, na.rm = TRUE))  %>%
  ungroup()                                         %>%
  group_by(PAIS)                                    %>%
  mutate('ParticipSES'          = TotalSES/sum(TotalSES))

OcupadosTamaCalifLFS <- LFS                                              %>%
  filter(COND=="Ocupado" &  TAMA!="Ns/Nc" &  CALIF!="Ns/Nc" & STAPRO==3)   %>%
  group_by(COUNTRY, TAMA, CALIF)                                           %>%
  summarise('TotalLFS'                  = sum(WEIGHT, na.rm=TRUE))  %>%
  ungroup()                                                         %>%
  group_by(COUNTRY)                                                 %>%
  mutate('ParticipLFS'          = TotalLFS/sum(TotalLFS))           


OcupadosTamaCalifSES <- SES                              %>% 
  group_by(PAIS, A12_CLASS, CALIF)                         %>%
  summarise('TotalSES' =sum(WEIGHT, na.rm = TRUE))  %>%
  ungroup()                                         %>%
  group_by(PAIS)                                    %>%
  mutate('ParticipSES'          = TotalSES/sum(TotalSES))


CarpetaRdos <- "C:/Users/facun/Documents/Investigación/1. LFS 2020 - CEPED Precariedad mundial/Resultados/"

write.xlsx(as.data.frame(OcupadosTamaLFS), paste0(CarpetaRdos, "ComparacionLFS-SES.xlsx"), sheetName = "OcupadosTamaLFS", append=TRUE, row.names = FALSE)
write.xlsx(as.data.frame(OcupadosTamaSES), paste0(CarpetaRdos, "ComparacionLFS-SES.xlsx"), sheetName = "OcupadosTamaSES", append=TRUE, row.names = FALSE)
write.xlsx(as.data.frame(OcupadosTamaCalifLFS), paste0(CarpetaRdos, "ComparacionLFS-SES.xlsx"), sheetName = "OcupadosTamaCalifLFS", append=TRUE, row.names = FALSE)
write.xlsx(as.data.frame(OcupadosTamaCalifSES), paste0(CarpetaRdos, "ComparacionLFS-SES.xlsx"), sheetName = "OcupadosTamaCalifSES", append=TRUE, row.names = FALSE)

