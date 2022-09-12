# Indicadores sobre energía 
# Base de capacidad de generación por estado
# David A. Ortega Alfaro

## Preparar Espacio ----
pacman::p_load(tidyverse,scales,janitor,sf,leaflet)
rm(list=ls())
dev.off()
## Cargar Bases ----
capacidad_estatal <- read_csv("input/energia/capacidad_generacion_estatal.csv",
                              locale = locale(encoding = "ISO-8859-2"))

capacidad_estatal <- data.frame(t(capacidad_estatal)) %>% row_to_names(row_number = 1) %>% rownames_to_column("ano")

# Capacidad Estatal ----
capacidad_estatal %>% pivot_longer(!ano,names_to = "estado",values_to = "megawatts") %>% mutate(megawatts=as.numeric(megawatts)) %>% 
  filter(estado=="Total") %>% 
  ggplot(aes(ano,megawatts))+
  geom_col(fill="#4e9221")+
  geom_label(aes(label = paste(comma(megawatts),"MWxH")))+
  labs(x="",y="MegaWatts por Hora", title="Capacidad Instalada para generar electricidad",
       subtitle="En todo el país, entre 2002 y 2020, en MegaWatts por Hora (Incluye PIEs)")+
  theme(plot.title = element_text(size=20,face="bold",color="#4f9241"),
        plot.subtitle = element_text(size=16,face="bold"),
        axis.text.x = element_text(face="bold",color="black"),
        legend.position = "bottom")

## Capacidad para demanda de horas críticas 2022 ----

horas_crit <- read_csv("input/energia/100_hrs_crits.csv") %>% clean_names()
horas_crit %>% glimpse()
bcs <- horas_crit %>% select(demanda_mw_bcs,capacidad_disponible_mw_bcs,mr_percent_bcs)
bcn <- horas_crit %>% select(demanda_mw_bcn,capacidad_disponible_mw_bcn,mr_percent_bcn)
sin <- horas_crit %>% select(demanda_mw_sin,capacidad_disponible_mw_sin,mr_percent_sin)



promedio_demandas_sistemas <- rbind(
sin %>% summarise(demanda = mean(demanda_mw_sin),capacidad = mean(capacidad_disponible_mw_sin)) %>% 
  mutate(porcentaje_adicional  = round(capacidad/demanda,2)-1) %>% mutate(sistema = "sin"),

bcs %>% summarise(demanda = mean(demanda_mw_bcs),capacidad = mean(capacidad_disponible_mw_bcs)) %>% 
  mutate(porcentaje_adicional  = round(capacidad/demanda,2)-1)%>% mutate(sistema = "bcs"),

bcn %>% summarise(demanda = mean(demanda_mw_bcn),capacidad = mean(capacidad_disponible_mw_bcn)) %>% 
  mutate(porcentaje_adicional  = round(capacidad/demanda,2)-1)%>% mutate(sistema = "bcn"))


promedio_demandas_sistemas %>% pivot_longer(!sistema,names_to = "concepto",values_to = "valor") %>% 
  filter(concepto!="porcentaje_adicional") %>% 
  mutate(concepto = ifelse(concepto=="capacidad","Capacidad de Generación Promedio","Demanda Promedio")) %>% 
  mutate(sistema = case_when(sistema =="bcs" ~ "Baja California Sur",
                             sistema=="bcn"~ "Baja California Norte",
                             sistema=="sin"~"Sistema Interconectado Nacional")) %>% 
  ggplot(aes(reorder(sistema,valor),valor,group = concepto,fill=concepto))+
    geom_col(position = "dodge")+
  scale_y_log10()+
  geom_label(aes(label = paste0(comma(valor)," MwH")),position = position_dodge(width = 1))+
  labs(x="",y="Megawatts Hora",title="Megawatts Hora promedio para horas críticas (de mayor demanda) en 2022",
  subtitle = "Capacidad de Generación y Demanda Promedio",
       caption = "Fuente: SIE - 2022",fill="")+
  theme(plot.title = element_text(size=20,face="bold",color="#4f9241"),
        plot.subtitle = element_text(size=16,face="bold"),
        axis.text.x = element_text(face="bold",color="black"),
        legend.position = "bottom",legend.text = element_text(size=10,face="bold"))



































































































