#Script para achicar el tamaño de la base de uruguay
#La base original proviene de https://www4.ine.gub.uy/Anda5/index.php/catalog/715/get-microdata
library(haven)
data <- read_sav("bases/uruguay_P_2019_Terceros.sav")
variables <- c("region_4", "pobpcoac", "pesoano", "e26", "e27", "e49", "e197_1", "e201_1", 
                        "e212_1", "e215_1", "e218_1", "e221_1", "e224_1", "f73", "f71_2", "f85", 
                        "f102", "f82", "f263", "e45_1", "e45_2", "e45_3", "e45_4", "e45_5", "e45_6", 
                        "e45_7", "f77", "PT2")
data <- data %>% select(variables)
saveRDS(data, "bases/uruguay_2019.RDS")
