library(foreign)
library(haven)
library(tidyverse)
library(openxlsx)
#library(sampleSelection)
#library(tidymodels)
#library(stargazer)

## Funciones ####
tidy.selection <- function(x,
                           conf.int = FALSE,
                           conf.level = 0.95,
                           ...) {
  s <- summary(x, ...)
  ret <- broom:::as_tidy_tibble(s$estimate,
                                new_names = c("estimate", "std.error",
                                              "statistic", "p.value"))
  
  # selection models include an outcome and a selection equation
  ret$group <- NA_character_
  ret$group[s$param$index$betaS] <- "Selection"
  ret$group[s$param$index$betaO] <- "Outcome"
  
  if (isTRUE(conf.int)) {
    ci <- broom:::broom_confint_terms(x, level = conf.level)
    ret <- dplyr::left_join(ret, ci, by = "term")
  }
  ret
}

glance.selection <- function(x, ...) {
  broom:::as_glance_tibble(
    nobs = stats::nobs(x),
    method = x$method[1],
    rho = x$rho,
    sigma = x$sigma,
    na_types = "icrr")
}


#Carga de Datos#####
## Expansores####
expansores_gw <- openxlsx::read.xlsx("Fuentes Complementarias/CHIPweights GW modificacion.xlsx",
                                  sheet = "4) 2018_GW",startRow = 3)

expansores_urbano <- expansores_gw %>% 
  dplyr::select(Province,Province.code,pondera_urbano_gw) %>% 
  mutate(Province.code = as.character(Province.code)) %>% 
  filter(!is.na(Province.code),!is.na(pondera_urbano_gw))

ramas <- openxlsx::read.xlsx("Fuentes Complementarias/CHIPweights GW modificacion.xlsx",
                                  sheet = "branches_2018") %>% 
  mutate(branch_name = factor(branch_name,levels = unique(branch_name)),
         branch_group =case_when(C03_3 %in% c(3,5,6,7,8,15,16,19) ~ branch_name,
                                 TRUE ~"other"),
         branch_group = factor(branch_group,levels = unique(branch_group)))

ocupaciones_18 <- openxlsx::read.xlsx(
  "Fuentes Complementarias/CHIPweights GW modificacion.xlsx",
  sheet = "Ocupations 2018")

##2018####

base_urbana_2018 <- haven::read_dta("../bases/China/chip2018_urban_person.dta") 
base_urbana_2018[] <- lapply(base_urbana_2018, function(x) { attributes(x) <- NULL; x })


base_urbana_2018 %>% 
#base_rural_2018 <- haven::read_dta("Data/CHIP/2018/chip2018_rural_person.dta")
variables <- c("hhcode","A10","C03_1","C05_1","C01_1","C01_2","C01_3",
               "A_03")
base_dif_2018 <-  base_urbana_2018  %>% 
  mutate(estrato = case_when(A10 != 1 ~"Urban",
                             A10 == 1 ~"Migrant")) %>% 
  mutate(Province.code = str_sub(hhcode,1,2),
         Province.code!= 15) %>% 
  left_join(expansores_urbano) %>% 
  left_join(ramas)  %>% 
  mutate(age = 2018 - A04_1,
         estado = case_when(A20 == 1~"Employed",
                            A20 %in% 2:3~"Retiree",
                            A20 %in% 4~"Student",
                            A20 %in% 5~"Unemployed",
                            A20 %in% 6~"Full-time homemaker",
                            A20 %in% 7~"Pregnant-maternity leave",
                            A20 %in% 8~"Long term sick leave",
                            A20 %in% 9~"Other"),
         cat_ocup = case_when(C03_1 == 1~"Employer",
                              C03_1 == 2~"Employee",
                              C03_1 == 3~"Self-Employed",
                              C03_1 == 4~"Family worker"),
         hukou =  case_when(A10 == 1~"Rural",
                            A10 == 2~"Urban",
                            A10 == 3~"Resident",
                            TRUE ~ "Others"))

casos_urbano<- base_dif_2018 %>% 
  group_by(Province,Province.code) %>% 
  summarise(casos = n())


dif.asalariados.2018 <- base_dif_2018 %>%
  filter(estado %in%  c("Employed")) %>%  ###Filtro Ocupados
  filter(C03_1 %in%  2:3) %>% ###Filtro Asalariados y TCP
#  filter(Province.code!= 15) %>% 
  filter(C05_1>0,C01_1>=1,C01_2>0,C01_3>0) %>% # Filtro Salario, Horas, Meses, Dias positivos
  mutate(asalariado = case_when(C03_1 == 2 ~1,
                                TRUE ~ 0),
         salario.mensual.prom = C05_1/C01_1,
         salario.diario.prom = salario.mensual.prom/C01_2,
         salario.horario.prom = salario.diario.prom/C01_3,
         ocupacion_zhang =case_when(
           C03_4 %in% 1:3~"white-collar",
           C03_4 %in% c(4,7)~"service",
           C03_4 %in% c(5,6)~"blue-collar",
           TRUE ~"Other",
         ),
         grupos.calif = factor(
           case_when(
             C03_4%in%  8 ~"Baja",
             C03_4%in%  3:6 ~"Media",
             C03_4%in%  1:2 ~"Alta"),
           levels = c("Baja","Media","Alta")),
         
         age = 2018 - A04_1,
         sex = factor(
           case_when(A03==1~"Varon",
                     A03==2 ~"Mujer"),
           levels = c("Varon","Mujer")),
         han = factor(
           case_when(A06==1~"Yes",
                     TRUE ~"No"),
           levels = c("Yes","No")),
         party_member = factor(
           case_when(A07_1==1~"Yes",
                     TRUE ~"No"),
           levels = c("Yes","No")),
         married_cohabitation = factor(
           case_when(
             A05 %in% 1:4~"Yes",
             A05 %in% 5:8 ~"No"),
           levels = c("Yes","No")),
         tamanio = factor(
           case_when(
             C08%in%  1~"8 or less",
             C08%in%  2:3~"9-100",
             C08%in%  4:5~"101-500",
             C08%in%  6:7~"501 or more"),
           levels = c("8 or less","9-100","101-500","501 or more")),
         grupos.tamanio = factor(
           case_when(
             C08%in%  1 ~"Pequeño",
             C08%in%  2 ~"Mediano",
             C08%in%  3:7 ~"Grande"),
           levels = c("Pequeño","Mediano","Grande")),
         grupos.tamanio = factor(
           case_when( 
             C03_1 %in%  3 ~"Pequeño", #TCP a pequeño
             TRUE ~grupos.tamanio),
           levels = c("Pequeño","Mediano","Grande")),
         status = case_when(
           C03_2 %in% 1:3~"state-owned, public inst and party agencies",
           C03_2 %in% c(6)~"foreign owned",
           C03_2 %in% c(4,7,5)~"private sector",
           TRUE ~"Others"),
         status2 = case_when(
           C03_2 %in% 1:3~"state-owned, public inst and party agencies",
           C03_2 %in% c(6)~"foreign owned",
           C03_2 %in% c(4,7,5)~"private, individual and colective enterprises",
           TRUE ~"Others"),
         sin_contrato_o_temporario = factor(case_when(
           C07_1 %in% 3:4~"Yes",
           TRUE ~"No"),
           levels = c("No","Yes")),
         sin_contrato = factor(case_when(
           C07_1 %in% 4~"Yes",
           TRUE ~"No"),
           levels = c("No","Yes")),
         no_social_benefits = factor(case_when(
           C07_7 %in% 6~"Yes",
           TRUE ~"No"),
           levels = c("No","Yes"))
         
  ) %>% 
  rename(years_educ = A13_3,
         rama = C03_3,
         huk_status = A09_1,
         contract_type =C07_1) %>% 
  mutate(year = 2018,
         years_educ = case_when(years_educ>0~years_educ),
         status = case_when(status>0~status))

variables <- c("sin_contrato_o_temporario","pondera_urbano_gw","estrato" ,"estado","cat_ocup","asalariado","ocupacion_zhang" ,"Province",
               "branch_group","branch_name","grupos.tamanio","grupos.calif", "status","status2", "age" ,"party_member",  "sex" , "tamanio" , "han" , "married_cohabitation",
               "contract_type","rama","years_educ","sin_contrato","sin_contrato_o_temporario","no_social_benefits","year","salario.horario.prom",
               "salario.diario.prom","salario.mensual.prom","A05","A07_1","C03_1","C03_4","C05_1","C01_1","C01_2","C01_3","C03_2")

base_final<- dif.asalariados.2018 %>% 
  dplyr::select(all_of(variables)) %>% 
  left_join(ocupaciones_18 %>% rename(C03_4 =OCC_code)) 

####Chequeos#####

salario_ocupaciones <- base_final %>% 
  group_by(year,C03_4,name) %>% 
  summarise(casos = n(),
            casos_construc = sum(branch_name == "Construction",na.rm = T),
            casos_salud = sum(branch_name == "Health and social work ",na.rm = T),
            salario.horario.prom = mean(salario.horario.prom,na.rm = T),
            anios_prom_educ = mean(years_educ,na.rm = T),
            anios_mediana_educ = median(years_educ,na.rm = T),
            ) %>% 
  group_by(year) %>% 
  mutate(porcentaje = casos/sum(casos)) 

salario_grupo_ocupaciones <- base_final %>% 
  filter(!is.na(grupos.calif)) %>% 
  group_by(year,grupos.calif) %>% 
  summarise(casos = n(),
            salario.horario.prom = mean(salario.horario.prom,na.rm = T),
            anios_prom_educ = mean(years_educ,na.rm = T),
            anios_mediana_educ = median(years_educ,na.rm = T),
  ) %>% 
  group_by(year) %>% 
  mutate(porcentaje = casos/sum(casos)) 


salario_tamanio <- base_final %>% 
  filter(!is.na(grupos.tamanio)) %>% 
  group_by(year,grupos.tamanio) %>% 
  summarise(casos = n(),
            salario.horario.prom = mean(salario.horario.prom,na.rm = T)) %>% 
  group_by(year) %>% 
  mutate(porcentaje = casos/sum(casos)) 


salario_tamanio_calif <- base_final %>% 
  filter(!is.na(grupos.tamanio),!is.na(grupos.calif)) %>% 
  group_by(year,grupos.tamanio,grupos.calif) %>% 
  summarise(casos = n(),
            salario.horario.prom = mean(salario.horario.prom,na.rm = T)) %>% 
  group_by(year) %>% 
  mutate(particip.ocup = casos/sum(casos)) 


#### Ocupados.distric ####
china.ocupados.distrib <-  base_final  %>% 
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio) %>% 
  summarise(
    total.casos = n(),
    total.asalariados = sum(C03_1 %in%  2),
    ocupados = sum(pondera_urbano_gw,na.rm = T),
    asalariados = sum(pondera_urbano_gw[C03_1 %in%  2],na.rm = T),
    tcp = sum(pondera_urbano_gw[!(C03_1 %in%  2)],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = salario.mensual.prom,
      w = pondera_urbano_gw,na.rm = T),
    promedio.ing.oc.prin.tcp=weighted.mean(
      x = salario.mensual.prom[!(C03_1 %in%  2)],
      w = pondera_urbano_gw[!(C03_1 %in%  2)],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = salario.mensual.prom[C03_1 %in%  2],
      w = pondera_urbano_gw[C03_1 %in%  2],na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.tcp = tcp/sum(tcp))


china.ocupados.distrib.agregado <-  base_final  %>% 
  summarise(
    total.casos = n(),
    total.asalariados = sum(C03_1 %in%  2),
    ocupados = sum(pondera_urbano_gw,na.rm = T),
    asalariados = sum(pondera_urbano_gw[C03_1 %in%  2],na.rm = T),
    tcp = sum(pondera_urbano_gw[!(C03_1 %in%  2)],na.rm = T),
    tasa.asalarizacion = asalariados/ocupados,
    promedio.ing.oc.prin=weighted.mean(
      x = salario.mensual.prom,
      w = pondera_urbano_gw,na.rm = T),
    promedio.ing.oc.prin.tcp=weighted.mean(
      x = salario.mensual.prom[!(C03_1 %in%  2)],
      w = pondera_urbano_gw[!(C03_1 %in%  2)],na.rm = T),
    promedio.ing.oc.prin.asal=weighted.mean(
      x = salario.mensual.prom[C03_1 %in%  2],
      w = pondera_urbano_gw[C03_1 %in%  2],na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(particip.ocup = ocupados/sum(ocupados),
         particip.asal = asalariados/sum(asalariados),
         particip.tcp = tcp/sum(tcp))


china.asalariados.tasas <- base_final %>% 
  filter(C03_1 %in%  2) %>% # Asalariado
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio) %>% 
  summarise(
    seguridad.social.si = sum(pondera_urbano_gw[no_social_benefits=="No"],na.rm = T),
    seguridad.social.no = sum(pondera_urbano_gw[no_social_benefits=="Yes"],na.rm = T),
    registrados =sum(pondera_urbano_gw[sin_contrato=="No"],na.rm = T),
    no.registrados =sum(pondera_urbano_gw[sin_contrato=="Yes"],na.rm = T),
    empleo.temporal =sum(pondera_urbano_gw[sin_contrato_o_temporario=="Si"],na.rm = T),
    empleo.no.temporal =sum(pondera_urbano_gw[sin_contrato_o_temporario=="No"],na.rm = T),
#    part.involun = sum(pondera_urbano_gw[part.time.inv=="Part Involunt"],na.rm = T),
#    part.volunt = sum(pondera_urbano_gw[part.time.inv=="Part Volunt"],na.rm = T),
#    full.time = sum(pondera_urbano_gw[part.time.inv=="Full Time"],na.rm = T),
    # tasa.partime = part.involun/(part.involun+
    #                                part.volunt+
    #                                full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp = empleo.temporal/(empleo.temporal+
                                   empleo.no.temporal))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".asal"), .cols = 3:ncol(.))


china.tcp.tasas <- base_final %>% 
  filter(C03_1 %in%  3) %>% # tcp
  filter(!is.na(grupos.calif),!is.na(grupos.tamanio)) %>% 
  group_by(grupos.calif,grupos.tamanio) %>% 
  summarise(
    seguridad.social.si = sum(pondera_urbano_gw[no_social_benefits=="No"],na.rm = T),
    seguridad.social.no = sum(pondera_urbano_gw[no_social_benefits=="Yes"],na.rm = T),
    registrados =sum(pondera_urbano_gw[sin_contrato=="No"],na.rm = T),
    no.registrados =sum(pondera_urbano_gw[sin_contrato=="Yes"],na.rm = T),
    empleo.temporal =sum(pondera_urbano_gw[sin_contrato_o_temporario=="Si"],na.rm = T),
    empleo.no.temporal =sum(pondera_urbano_gw[sin_contrato_o_temporario=="No"],na.rm = T),
    #    part.involun = sum(pondera_urbano_gw[part.time.inv=="Part Involunt"],na.rm = T),
    #    part.volunt = sum(pondera_urbano_gw[part.time.inv=="Part Volunt"],na.rm = T),
    #    full.time = sum(pondera_urbano_gw[part.time.inv=="Full Time"],na.rm = T),
    # tasa.partime = part.involun/(part.involun+
    #                                part.volunt+
    #                                full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp = empleo.temporal/(empleo.temporal+
                                   empleo.no.temporal))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".tcp"), .cols = 3:ncol(.))


china.asalariados.tasas.agregado <- base_final %>% 
  filter(C03_1 %in%  2) %>% # Asalariado
  summarise(
    seguridad.social.si = sum(pondera_urbano_gw[no_social_benefits=="No"],na.rm = T),
    seguridad.social.no = sum(pondera_urbano_gw[no_social_benefits=="Yes"],na.rm = T),
    registrados =sum(pondera_urbano_gw[sin_contrato=="No"],na.rm = T),
    no.registrados =sum(pondera_urbano_gw[sin_contrato=="Yes"],na.rm = T),
    empleo.temporal =sum(pondera_urbano_gw[sin_contrato_o_temporario=="Si"],na.rm = T),
    empleo.no.temporal =sum(pondera_urbano_gw[sin_contrato_o_temporario=="No"],na.rm = T),
    #    part.involun = sum(pondera_urbano_gw[part.time.inv=="Part Involunt"],na.rm = T),
    #    part.volunt = sum(pondera_urbano_gw[part.time.inv=="Part Volunt"],na.rm = T),
    #    full.time = sum(pondera_urbano_gw[part.time.inv=="Full Time"],na.rm = T),
    # tasa.partime = part.involun/(part.involun+
    #                                part.volunt+
    #                                full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp = empleo.temporal/(empleo.temporal+
                                   empleo.no.temporal))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".asal"), .cols = 1:ncol(.))

china.tcp.tasas.agregado <- base_final %>% 
  filter(C03_1 %in%  3) %>% # TCP
  summarise(
    seguridad.social.si = sum(pondera_urbano_gw[no_social_benefits=="No"],na.rm = T),
    seguridad.social.no = sum(pondera_urbano_gw[no_social_benefits=="Yes"],na.rm = T),
    registrados =sum(pondera_urbano_gw[sin_contrato=="No"],na.rm = T),
    no.registrados =sum(pondera_urbano_gw[sin_contrato=="Yes"],na.rm = T),
    empleo.temporal =sum(pondera_urbano_gw[sin_contrato_o_temporario=="Si"],na.rm = T),
    empleo.no.temporal =sum(pondera_urbano_gw[sin_contrato_o_temporario=="No"],na.rm = T),
    #    part.involun = sum(pondera_urbano_gw[part.time.inv=="Part Involunt"],na.rm = T),
    #    part.volunt = sum(pondera_urbano_gw[part.time.inv=="Part Volunt"],na.rm = T),
    #    full.time = sum(pondera_urbano_gw[part.time.inv=="Full Time"],na.rm = T),
    # tasa.partime = part.involun/(part.involun+
    #                                part.volunt+
    #                                full.time),
    tasa.seguridad.social = seguridad.social.no/(seguridad.social.si+
                                                   seguridad.social.no),
    tasa.no.registro = no.registrados/(registrados+
                                         no.registrados),
    tasa.temp = empleo.temporal/(empleo.temporal+
                                   empleo.no.temporal))%>% 
  ungroup() %>% 
  rename_with(~str_c(.,".tcp"), .cols = 1:ncol(.))

china.resultado <- china.ocupados.distrib %>%
  left_join(china.asalariados.tasas)%>% 
  left_join(china.tcp.tasas)%>% 
  mutate(Pais = "China",
         periodo = 2018,
         tamanio.calif = paste0(grupos.tamanio," - ",grupos.calif),
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
                                    "Grande - Alta"))) %>% 
  arrange(tamanio.calif)

china.resultado.agregado <- china.ocupados.distrib.agregado %>%
  mutate(periodo = 2018) %>% 
  left_join(china.asalariados.tasas.agregado %>% mutate(periodo = 2018))%>% 
  left_join(china.tcp.tasas.agregado%>% mutate(periodo = 2018))%>% 
  mutate(Pais = "China")

saveRDS(china.resultado,file = "Resultados/China.RDS")  
saveRDS(china.resultado.agregado,file = "Resultados/China_agregado.RDS")  

microbase <- base_final %>% 
  select(pondera = pondera_urbano_gw,
         rama = branch_name,sin_contrato_o_temporario,
         periodo = year,no_social_benefits,
         cat_ocup,age,sex,years_educ,sin_contrato,
         grupos.tamanio,grupos.calif) %>% 
  mutate(pais = "China")

#saveRDS(microbase,file = "Resultados/China_microdata.RDS")  

####Grafiquito ####
########PALETA#####
azul <- colorspace::diverge_hcl(n = 12,h = c(255,330),
                                l = c(40,90))[c(4,2,1)]
verde <- colorspace::diverge_hcl(n = 12,h = c(130,43),
                                 c = 100,
                                 l = c(70,90))[c(4,2,1)]

naranja <- colorspace::diverge_hcl(n = 12,h = c(130,43),
                                   c = 100,
                                   l = c(70,90))[c(10,11,12)]
paleta <- c(azul,
            naranja,
            verde)

paleta3 <- c(azul[1],
             naranja[1],
             verde[1])

salario_tamanio_calif %>%
  mutate(tamanio.calif = paste0(grupos.tamanio, " - ",grupos.calif),
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
                               "Grande - Alta",
                               "Total"))) %>% 
  ggplot(.,
         aes(x = "China", y = particip.ocup,
             fill = tamanio.calif,group = tamanio.calif,
             label = scales::percent(particip.ocup,accuracy = 0.01))) +
  geom_col(position = "stack")+
  geom_text(position = position_stack(vjust = .5),size=3)+
  #  labs(title = "Distribución del empleo según grupos")+
  theme_tufte()+
  theme(legend.position = "left",
        legend.direction = "vertical",
        legend.title = element_text(size = 14),
        axis.title = element_blank(),
        text = element_text(size = 15),
        axis.text.x = element_text(angle = 45),
        #panel.spacing = unit(1,"cm"),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.grid.minor.y = element_line(colour = "grey30"),
        panel.grid.minor.x = element_line(colour = "grey"),
        panel.grid.major.x = element_line(colour = "grey"))+
  scale_fill_manual(values = paleta)+
  scale_y_continuous(labels = scales::percent)+
  guides(fill=guide_legend(title="Tamaño - Calificación"))


write.xlsx(list("cuadro_ocupaciones_13" = cuadro_ocupaciones_13,
                "cuadro_ocupaciones_18"= cuadro_ocupaciones_18),
           "Resultados/Mercado de Trabajo/diferenciacion/chequeos.xlsx")
####Estadisticas####
options(scipen = 999)
descriptiva <- base_1318 %>% 
  filter(C03_1 == 2) %>% 
  group_by(estrato,year) %>% 
  summarise(casos = n(),
            meses_al_anio = mean(C01_1),
            dias_al_mes = mean(C01_2),
            horas_al_dia = mean(C01_3),
            age = mean(age,na.rm = T),
            varones = sum(sex=="Varon",na.rm = T),
            mujeres = sum(sex=="Mujer",na.rm = T),
            perc_han = sum(han=="Yes",na.rm = T)/casos,
            party_membership = sum(party_member=="Yes",na.rm = T)/casos,
            perc_varones = varones/casos,
            anios_educ = mean(years_educ[years_educ>0],na.rm = T),
            blue_collar = sum(ocupacion_zhang=="blue-collar",na.rm = T)/casos,
            casado_convive = sum(A05 %in% 1:4,na.rm = T),
            soltero = sum(A05 %in% 5:8,na.rm = T),
            perc_casado = casado_convive/(casado_convive+soltero),
            sincontrato_temporario = sum(contract_type %in% 3:4,na.rm = T),
            permanente_largop = sum(contract_type %in% 1:2,na.rm = T),
            perc_sincontrato_temporario = sincontrato_temporario/(sincontrato_temporario+permanente_largop),
            perc_econ_privada = sum(C03_2 ==7,na.rm = T)/casos,
            salario.mensual.prom = mean(salario.mensual.prom),
            salario.diario.prom = mean(salario.diario.prom),
            salario.horario.prom = mean(salario.horario.prom),
  ) %>% 
  ungroup() %>% 
  pivot_longer(3:ncol(.)) %>% 
  pivot_wider(names_from = "estrato",values_from = "value") 

tabla_rama <- base_1318 %>% 
  filter(C03_1 == 2) %>% 
  group_by(year,estrato,branch_name,branch_group) %>%
  summarise(casos = n()) %>% 
  group_by(year,estrato) %>% 
  mutate(porcentaje = casos/sum(casos))

tabla_status <- base_1318 %>% 
  filter(C03_1 == 2) %>% 
  group_by(year,estrato,status) %>%
  summarise(casos = n()) %>% 
  group_by(year,estrato) %>% 
  mutate(porcentaje = casos/sum(casos))

tabla_status2 <- base_1318 %>% 
  filter(C03_1 == 2) %>% 
  group_by(year,estrato,status2) %>%
  summarise(casos = n()) %>% 
  group_by(year,estrato) %>% 
  mutate(porcentaje = casos/sum(casos))

tabla_ocupacion <- base_1318 %>% 
  filter(C03_1 == 2) %>% 
  group_by(year,estrato,ocupacion_zhang) %>%
  summarise(casos = n()) %>% 
  group_by(year,estrato) %>% 
  mutate(porcentaje = casos/sum(casos))

tabla_sin_contract <- base_1318 %>% 
  filter(C03_1 == 2) %>% 
  group_by(year,estrato,sin_contrato) %>%
  summarise(casos = n()) %>% 
  group_by(year,estrato) %>% 
  mutate(porcentaje = casos/sum(casos))
