# Indicadores Macroeconómicos
# David A. Ortega


# Preparar Entorno ----
rm(list=ls())
dev.off()
pacman::p_load(tidyverse,scales,janitor)
# Cargar BdD ----
tiee_banxico <- read_csv("input/macroeconomía/tiee_banxico.csv",
                         locale=locale(encoding="ISO-8859-2")) %>% clean_names()
fed_ir <- read_csv("input/macroeconomía/FEDFUNDS.csv") %>% clean_names()
# Analisis  Banxico ----
glimpse(tiee_banxico)

tiee_banxico %>% mutate(fecha=as.Date(fecha,tryFormats="%d/%m/%Y")) %>% mutate(tasa_objetivo=as.numeric(tasa_objetivo)) %>% 
  select(fecha,tasa_objetivo) %>% filter(!is.na(tasa_objetivo)) %>% 
  ggplot(aes(fecha,tasa_objetivo))+
  geom_line()+
  geom_smooth()+
  labs(x="",y="%",title="Tasa de Interés Objetivo de Banxico entre 2008 y 2022", subtitle = "En porcentaje",caption = "Fuente: SIE - BANXICO")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16))+
  geom_label(data=labs,aes(label=paste0(format(fecha,format="%Y-%m"),": ","%",tasa_objetivo)))

labs <-tiee_banxico %>% mutate(fecha=as.Date(fecha,tryFormats="%d/%m/%Y")) %>% mutate(tasa_objetivo=as.numeric(tasa_objetivo)) %>% 
  select(fecha,tasa_objetivo) %>% filter(!is.na(tasa_objetivo)) %>%
  filter(fecha %in% c(as.Date("2022-06-24"),as.Date("2010-01-01"),as.Date("2018-01-01"),as.Date("2018-11-01"),as.Date("2014-06-06")))
# Analisis FED ----

fed_ir %>% mutate(date=as.Date(date,tryFormats="%Y-%m-%d")) %>% mutate(fedfunds=as.numeric(fedfunds)) %>% ggplot(aes(date,fedfunds))+
  geom_path(size=1)+
  geom_label(data=labs,aes(label=paste0(format(date,"%Y-%m"),": %",fedfunds)))+
  labs(x="",y="%",title="Tasasa de Interés Objetivo de la Reserva Federal de EEUU",subtitle="Entre 2000 y 2022",
       caption="Fuente: FRED")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16))+
  geom_smooth()

labs <- fed_ir %>% mutate(date=as.Date(date,tryFormats="%Y-%m-%d")) %>% mutate(fedfunds=as.numeric(fedfunds)) %>% 
  filter(date %in%c(as.Date("2000-07-01"),as.Date("2007-12-01"),as.Date("2022-05-01"),as.Date("2022-06-01")))

fed_ir <- rbind(fed_ir,data.frame(date=as.Date("2022-06-01"),fedfunds="1.5"))






















































































