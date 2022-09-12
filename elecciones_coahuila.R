## Indicadores de Economía para estacionalizar cosas
## David A. Ortega


# Preparar Entorno ----
rm(list=ls())
dev.off()
pacman::p_load(tidyverse,scales,janitor)
# Cargar bdd ----

# Elecciones Muncipales Coahuila 2021

coah_21 <- read_csv("input/coahuila/elecciones_coah_2021.csv", 
                    locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
# Analisis ----

coah_21 %>% select(nom_mun,pan,pri,prd,pvem,pt,udc,mc,morena,pes,rsp,fpm,pri_prd,nulos,total_votos,percent_part) %>% 
  pivot_longer(c(pan,pri,prd,pvem,pt,udc,mc,morena,pes,rsp,fpm,pri_prd),names_to = "partido",values_to = "votos") %>% 
  group_by(nom_mun,partido) %>% filter(



































