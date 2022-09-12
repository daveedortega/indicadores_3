## Indicadores Energía: Balanza Energética SIE
## David A. Ortega


# Preparar Entorno ----
rm(list=ls())
dev.off()
pacman::p_load(tidyverse,scales,janitor,plotly)

#  Cargar Bases ----

balanza_energetica <- read_csv("input/energia/balanza_energetica.csv")

balanza_energetica %>% glimpse()

balanza_energetica <- cbind( balanza_energetica %>% select(c(concepto,pais)),
       balanza_energetica %>% select_if(is.numeric),
       balanza_energetica %>% select_if(is.character) %>% select(!c(concepto,pais)) %>% mutate_if(is.character,funs(as.numeric)))
                             
balanza_energetica <- balanza_energetica %>% pivot_longer(!c(concepto,pais),names_to = "mes",values_to = "mwh") %>% 
  separate(mes, into = c("mes","ano")) %>% 
  mutate(fecha = case_when(mes == "Ene"~as.Date(paste0(ano,"-01-01")),
                           mes == "Feb"~as.Date(paste0(ano,"-02-01")),
                           mes == "Mar"~as.Date(paste0(ano,"-03-01")),
                           mes == "Abr"~as.Date(paste0(ano,"-04-01")),
                           mes == "May"~as.Date(paste0(ano,"-05-01")),
                           mes == "Jun"~as.Date(paste0(ano,"-06-01")),
                           mes == "Jul"~as.Date(paste0(ano,"-07-01")),
                           mes == "Ago"~as.Date(paste0(ano,"-08-01")),
                           mes == "Sep"~as.Date(paste0(ano,"-09-01")),
                           mes == "Oct"~as.Date(paste0(ano,"-10-01")),
                           mes == "Nov"~as.Date(paste0(ano,"-11-01")),
                           mes == "Dic"~as.Date(paste0(ano,"-12-01")))) 

## Exportaciones ----


labs <- balanza_energetica %>% filter(concepto == "exportaciones",pais != "total exportada") %>% 
  filter(fecha %in% c(as.Date("2021-04-01"),as.Date("2018-10-01"),as.Date("2017-12-01"),as.Date("2010-05-01")))


balanza_energetica %>% filter(concepto == "exportaciones",pais != "total exportada") %>% 
  ggplot(aes(fecha,mwh,group = pais,fill = pais))+
  geom_area()+
  geom_label(data=labs,aes(label = (comma(mwh,prefix = paste0(format(fecha,format = "%Y-%m"),": "),suffix = " MwH"))),size=4.5,hjust = 1,
             position="stack")+
  scale_y_sqrt()+
  labs(x="",y="Megawatts Hora",title = "Exportaciones de Energía en México",
       subtitle = "Desde enero 2004 a Diciembre 2021",caption = "Fuente: SENER - SIE",fill = "País")+
  theme(plot.title = element_text(size = 22,face="bold",color = "#9f2441"),plot.subtitle = element_text(size = 18,face="bold"),
        legend.position = "bottom",legend.text = element_text(size=12))



## Importaciones ----


labs <- balanza_energetica %>% filter(concepto == "importaciones",pais != "total importada") %>% 
  filter(fecha %in% c(as.Date("2021-08-01"),as.Date("2018-10-01"),as.Date("2017-12-01"),as.Date("2010-05-01")))

balanza_energetica %>% filter(concepto == "importaciones",pais != "total importada") %>% 
  ggplot(aes(fecha,mwh,group = pais,fill = pais))+
  geom_area()+
  geom_label(data=labs,aes(label = (comma(mwh,prefix = paste0(format(fecha,format = "%Y-%m"),": "),suffix = " MwH"))),size=4.5,hjust = 1,
             position = "stack")+
  scale_y_sqrt()+
  labs(x="",y="Megawatts Hora",title = "Importaciones de Energía en México",
       subtitle = "Desde enero 2004 a Diciembre 2021",caption = "Fuente: SENER - SIE",fill = "País")+
  theme(plot.title = element_text(size = 22,face="bold",color = "#9f2441"),plot.subtitle = element_text(size = 18,face="bold"),
        legend.position = "bottom",legend.text = element_text(size=12))

## Imp - Exp Anual ----
balanza_energetica %>% group_by(concepto,pais,ano) %>% summarise(mwh = sum(mwh)) %>% filter(!str_detect(pais,"total"),ano %in% c(2020,2021)) %>% 
  ggplot(aes(reorder(pais,mwh),mwh,group = concepto,fill = concepto))+
  geom_col(position = position_dodge(width = 1))+
  geom_label(aes(label = comma(mwh,suffix = "MwH")),position = position_dodge(width = 1),size = 5)+
  labs(x="",y="Megawatts Hora",title="Exportaciones - Importaciones de Energía en México", subtitle = "Total durante 2020, en Megawatts hora",
       caption = "Fuente: SENER - SIE", fill ="")+
  theme(plot.title = element_text(size = 22,face="bold",color = "#9f2441"),plot.subtitle = element_text(size = 18,face="bold"),
        legend.position = "bottom",legend.text = element_text(size=16),axis.text.x = element_text(size = 20,color = "black"),
        strip.text.x = element_text(size=12, face="bold"))+
  scale_y_sqrt()+
  facet_wrap(~ano)










































