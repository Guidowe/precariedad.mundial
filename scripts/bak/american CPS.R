library(tidyverse)
library(ipumsr)
cps_ddi_file <- "cps_00002.xml"
cps_data_file <- "cps_00002.dat"
cps_ddi <- read_ipums_ddi(cps_ddi_file) # Contains metadata, nice to have as separate object

Base <- ipumsr::read_ipums_micro(ddi = cps_ddi_file,data_file =  cps_data_file)

# examine all available AHS microdata files
a <- read.table(file = "asec2019_pubuse.dat",header = T)

weighted.mean(person$WAGP,person$WAGP)
cpsbasic_cat <-
  get_catalog( "cpsbasic" ,
               output_dir = file.path( path.expand( "~" ) , "CPSBASIC" ) )

cpsbasic_cat <- subset( cpsbasic_cat , year == 2017 & month == 3 )
# download the microdata to your local computer
cpsbasic_cat <- lodown( "cpsbasic" , cpsbasic_cat )

library(survey)

cpsbasic_df <- 
  readRDS( file.path( path.expand( "~" ) , "CPSBASIC" , "2017 03 cps basic.rds" ) )

cpsbasic_design <-cpsbasic_df %>%  
  mutate(class_of_worker =
      factor( peio1cow , levels = 1:8 ,
              labels = 
                c( "government - federal" , "government - state" ,
                   "government - local" , "private, for profit" ,
                   "private, nonprofit" , "self-employed, incorporated" ,
                   "self-employed, unincorporated" , "without pay" )))
table(cpsbasic_design$class_of_worker)

#https://thedataweb.rm.census.gov/ftp/cps_ftp.html?#
hist(cpsbasic_design$prernwa)  

# ELG			2		EARNINGS ELIGIBILITY FLAG							498 - 499
# 
# EDITED UNIVERSE:	PEMLR = 1-2 AND HRMIS = 4 OR 8
# 
# VALID ENTRIES
# 
# 0	NOT ELIGIBLE FOR EDIT
# 1	ELIGIBLE FOR EDIT

###solo se relevan ingresos "usuales" de la semana de asalariados
A <- cpsbasic_design %>% 
  filter(prernwa>0) %>%
  group_by(pesex) %>% 
  summarise(Ingreso_semanal_Prom = weighted.mean(prernwa,pwsswgt)) %>% 
  mutate(Sexo = case_when(pesex==1~"Varón",
                          pesex==2~"Mujer"))

hist(A$prernwa)  
weighted.mean(A$prernwa,A$pwsswgt)  
