# Indicadores sobre energía 
# Base de produccion sobre tiempo
# David A. Ortega Alfaro

## Preparar Espacio ----
pacman::p_load(tidyverse,scales,janitor,sf,leaflet)
rm(list=ls())
dev.off()

## Cargar Bases ----
# Producción de energía por tipo de producción - SIE 01/07/22
produccion_energia <- read_csv("input/energia/produccion_energia_jun_22.csv",
                               locale=locale(encoding = "ISO-8859-2"))
# Ajustar estructura de base
produccion_energia <- data.frame(t(produccion_energia)) %>% row_to_names(row_number = 1) %>% rownames_to_column("fecha")
produccion_energia <- produccion_energia %>% separate(fecha, into = c("mes","ano")) %>% 
  mutate(mes=case_when(mes =="Ene"~"01",
         mes =="Feb"~"02",
         mes =="Mar"~"03",
         mes =="Abr"~"04",
         mes =="May"~"05",
         mes =="Jun"~"06",
         mes =="Jul"~"07",
         mes =="Ago"~"08",
         mes =="Sep"~"09",
         mes =="Oct"~"10",
         mes =="Nov"~"11",
         mes =="Dic"~"12")) %>% 
  mutate(fecha=as.Date(paste0(ano,"-",mes,"-01")))

## Gráfica Total de energía por Mes ----
labs <- produccion_energia %>% mutate(total = as.numeric(Total)) %>% filter(fecha == as.Date("2022-05-01"))

produccion_energia %>% mutate(total = as.numeric(Total)) %>% 
  select(fecha,total) %>% 
  ggplot(aes(fecha,total))+
  geom_line(size=1,color="#007a53")+
  geom_smooth()+
  labs(x="",y="MegaWatts-Hora",title = "Generación de Energía Total de la CFE", subtitle = "En Megawatts-Hora, al mes entre 2002 y Mayo 2022",
       caption = "Fuente: SIE - 2022")+
  geom_label(data=labs,aes(label = paste0(format(fecha,format="%Y-%m"),": ", comma(total)," MwH"),),nudge_x = -500)+
  theme(plot.title = element_text(size=20,face="bold",color="#4f9241"),
        plot.subtitle = element_text(size=16,face="bold"))

## Gráfica Total de energía por Mes desde amlo ----
labs <- produccion_energia %>% mutate(total = as.numeric(Total)) %>% filter(fecha %in% c(as.Date("2022-05-01"),as.Date("2021-05-01"),
                                                                                         as.Date("2020-05-01"),as.Date("2019-05-01")))

produccion_energia %>% mutate(total = as.numeric(Total)) %>% filter(fecha > as.Date("2018-01-01")) %>% 
  select(fecha,total) %>% 
  ggplot(aes(fecha,total))+
  geom_line(size=1,color="#007a53")+
  geom_smooth(se=F,size=2)+
  labs(x="",y="MegaWatts-Hora",title = "Generación de Energía Total de la CFE", subtitle = "En Megawatts-Hora, al mes entre 2002 y Mayo 2022",
       caption = "Fuente: SIE - 2022")+
  geom_label(data=labs,aes(label = paste0(format(fecha,format="%Y-%m"),": ", comma(total)," MwH"),),nudge_x = -100)+
  theme(plot.title = element_text(size=20,face="bold",color="#4f9241"),
        plot.subtitle = element_text(size=16,face="bold"))
  
## Gráfica total por ano ----


produccion_energia %>% mutate(total = as.numeric(Total)) %>% 
  select(fecha,total) %>% mutate(fecha = format(fecha,format="%Y")) %>% 
  group_by(fecha) %>% summarise(total = sum(total)) %>% 
  ggplot(aes(fecha,total))+
  geom_col(fill = "#007a53")+
  geom_text(y=50000000,aes(label = paste0(comma(total), " MwH")),angle=90,color="white",size=8)+
  labs(x="",y="MegaWatts x Hora", title="Megawatts hora generados por la CFE y Privadors",subtitle="Al año, entre 2002 y Mayo 2022")+
  theme(plot.title = element_text(size=20,face="bold",color="#4f9241"),
        plot.subtitle = element_text(size=16,face="bold"),axis.text.x = element_text(face="bold",color="black"))


# Energía por generacion ----
por_generacion <- produccion_energia %>% select(-c(mes,fecha)) %>% pivot_longer(!ano,names_to = "generacion",values_to = "megawatts") %>% 
  mutate(megawatts=as.numeric(megawatts))

por_generacion <- por_generacion[-grep("total",por_generacion$generacion,ignore.case = T),]
#Termoelectrica abarca varias, quitar
por_generacion <- por_generacion %>% filter(generacion!="Termoeléctrica")

por_generacion %>% mutate(pie=ifelse(generacion %in% c("Ciclo combinado PIE 1","Eólica PIE 1"),"Productores Independientes","CFE")) %>% 
  group_by(pie,ano) %>% summarise(megawatts = sum(megawatts,na.rm = T)) %>%  filter(ano != 2022) %>% 
  ggplot(aes(ano,megawatts,fill=pie,group=pie))+
  geom_col(position = "stack")+
  labs(x="",y="Megawatts",title="Generación de Energía de la CFE por tipo de generador",
       subtitle="Entre 2002 y Mayo 2022 en TeraWatts (1x10^12) Hora",caption="Fuente: SIE - 2022",fill="Generador")+
  theme(plot.title = element_text(size=20,face="bold",color="#4f9241"),
        plot.subtitle = element_text(size=16,face="bold"),
        axis.text.x = element_text(face="bold",color="black"),
        legend.position = "bottom")+
  geom_label(aes(label = paste(format(round(megawatts / 1e6, 1), trim = TRUE), "TwH")),angle=90)+
  geom_smooth()

#En Porcentaje

por_generacion %>% mutate(pie=ifelse(generacion %in% c("Ciclo combinado PIE 1","Eólica PIE 1"),"Productores Independientes","CFE")) %>% 
  group_by(pie,ano) %>% summarise(megawatts = sum(megawatts,na.rm = T)) %>% ungroup() %>% 
  group_by(ano) %>% summarise(porcentaje = round(megawatts/sum(megawatts)*100,2),pie) %>% 
  ggplot(aes(ano,porcentaje,fill=pie,group=pie))+
  geom_col(position = "stack")+
  labs(x="",y="Megawatts",title="Porcentaje de Generación de Energía de la CFE por tipo de generador",
       subtitle="Entre 2002 y Mayo 2022 en Megawatts Hora",caption="Fuente: SIE - 2022",fill="Generador")+
  theme(plot.title = element_text(size=20,face="bold",color="#4f9241"),
        plot.subtitle = element_text(size=16,face="bold"),
        axis.text.x = element_text(face="bold",color="black"),
        legend.position = "bottom")+
  geom_label(aes(label = paste0(comma(porcentaje,2),"%"),angle=90))+
  geom_smooth()


















