## Indicadores de agricultura
## David A. Ortega


# Preparar Entorno ----
rm(list=ls())
dev.off()
pacman::p_load(tidyverse,scales,janitor,plotly)

#  Cargar Bases ----
prod_bienestar <- read_csv("./input/agricultura/padron_prodbien_2021cierre.csv") %>% clean_names()


## Prod p Bienestar -----

prod_bienestar %>% glimpse()
prod_bienestar %>% summarise(sum(monto_apoyado))
prod_bienestar %>% count(nombre_de_la_delegacion) %>% summarise(sum(n))

# Número de gente con apoyos ----
prod_bienestar %>% count(nombre_de_la_delegacion) %>% 
  ggplot(aes(reorder(nombre_de_la_delegacion,n),n,fill=nombre_de_la_delegacion))+
  geom_col()+
  labs(title = "Beneficiarios por entidad del programa Producción al Bienestar",subtitle = "Al cierre de 2021 por Entidad",
       caption = "Fuente: SADER 2021",x="",y="Número de Beneficiarios")+
  theme(plot.title = element_text(size=24,face="bold",color="#9f2441"),plot.subtitle = element_text(size=16,face="bold"),
        legend.position = "none",axis.text.y = element_text(size=10,color="black"))+
  geom_label(aes(label=comma(n)),size=5)+
  coord_flip()

## Monto por estado ----
prod_bienestar %>% group_by(nombre_de_la_delegacion) %>% 
  summarise(apoyo_total=sum(monto_apoyado)) %>% 
  ggplot(aes(reorder(nombre_de_la_delegacion,apoyo_total),apoyo_total,fill=nombre_de_la_delegacion))+
  geom_col()+
  coord_flip()+
  labs(title = "Monto total entregado por entidad del programa Producción al Bienestar",subtitle = "Al cierre de 2021 por Entidad",
       caption = "Fuente: SADER 2021",x="",y="Pesos")+
  theme(plot.title = element_text(size=22,face="bold",color = "#9f2441"),plot.subtitle = element_text(size=18,face="bold"),
        legend.position = "none")+
  geom_label(aes(label=comma(apoyo_total)),nudge_y = -1000)+
  scale_y_sqrt()

## Porcentaje por Cultivo
prod_bienestar %>% group_by(cultivo) %>% summarise(monto_apoyado=sum(monto_apoyado)) %>% ungroup() %>% 
  mutate(porcentaje =round(100*monto_apoyado/sum(monto_apoyado),2)) %>% 
  ggplot(aes(reorder(cultivo,porcentaje),porcentaje,fill=cultivo))+
  geom_col()+
  labs(title = "Porcentaje de apoyo entregado por Cultivo del programa Producción al Bienestar",subtitle = "Al cierre de 2021",
       caption = "Fuente: SADER 2021",x="",y="%")+
  theme(plot.title = element_text(size=22,face="bold",color = "#9f2441"),plot.subtitle = element_text(size=18,face="bold"),
        legend.position = "none")+
  geom_label(aes(label=comma(porcentaje,suffix = "%")),size=6)+
  coord_flip()

## Monto por cultivo -----

prod_bienestar %>% group_by(cultivo) %>% summarise(total=sum(monto_apoyado)) %>% 
  ggplot(aes(reorder(cultivo,total),total,fill=cultivo))+
  geom_col()+
  labs(title = "Monto total entregado por Cultivo del programa Producción al Bienestar",subtitle = "Al cierre de 2021 por Entidad",
       caption = "Fuente: SADER 2021",x="",y="Pesos")+
  theme(plot.title = element_text(size=22,face="bold",color = "#9f2441"),plot.subtitle = element_text(size=18,face="bold"),
        legend.position = "none")+
  geom_label(aes(label=comma(total)),nudge_y = -1000,size=6)+
  coord_flip()+
  scale_y_sqrt()

## Monto por Grupo Etarea ----

prod_bienestar %>% group_by(grupo_de_edad) %>% summarise(total=sum(monto_apoyado)) %>% 
  ggplot(aes(reorder(grupo_de_edad,total),total,fill=grupo_de_edad))+
  geom_col()+
  labs(x="",y="Pesos",fill="Grupo de Edad",title ="Monto total entregado por Grupo de Edad del programa Producción para el Bienestar",
       subtitle = "Al cierre de 2021 por Entidad", caption = "Fuente: SADER 2021",x="",y="Pesos")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),legend.position = "none")+
  scale_y_sqrt()+
  geom_label(aes(label=comma(total)))

## por hectarea promedio -----

prod_bienestar %>% glimpse()

prod_bienestar %>% group_by(nombre_de_la_delegacion) %>% 
  summarise(hectareas=sum(superficie_apoyada),total=sum(monto_apoyado)) %>% 
  mutate(apoyo_xh=total/hectareas) %>% 
  ggplot(aes(reorder(nombre_de_la_delegacion,apoyo_xh),apoyo_xh,fill=nombre_de_la_delegacion))+
  geom_col()+
  geom_label(aes(label=comma(round(apoyo_xh,2))))+
  labs(x="",y="Pesos",title="Apoyo promedio por Hectárea del programa Producción para el Bienestar",subtitle = "Al cierre de 2021 por Entidad",
       caption = "Fuente: SADER 2021")+
  coord_flip()+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),legend.position = "none")

# Por ciclo y regimen hidirico -----

prod_bienestar %>% group_by(ciclo,regimen_hidrico) %>% summarise(total=sum(monto_apoyado)) %>% 
  mutate(ciclo=ifelse(ciclo=="OI20","Otoño-Invierno 2020","Primavera-Verano 2021")) %>% 
  ggplot(aes(ciclo,total,group=regimen_hidrico,fill=regimen_hidrico))+
  geom_col()+
  scale_y_sqrt()+
  geom_label(aes(label=comma(total)),position="stack")+
  labs(x="",y="Pesos",title="Monto del programa Producción para el Bienestar por ciclo Productivo",
       subtitle = "Al cierre de 2021", caption = "Fuente: SADER 2021", fill="Regimen Hídrico")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16))


## Productores Privados ----

prod_bienestar %>% filter(ejido %in% c(0,NA)) %>% group_by(cultivo) %>% summarise(monto=sum(monto_apoyado)) %>% 
  mutate(porcentaje=round(monto/sum(monto)*100,2)) %>% 
  ggplot(aes(reorder(cultivo,porcentaje),porcentaje,fill=cultivo))+
  geom_col()+
  scale_y_sqrt()+
  labs(x="",y="Porcentaje",title="Porcentaje de apoyos por tipo de cultivo del programa Producción para el Bienestar",
       subtitle = "Para productores privados",caption = "Fuente: SADER 2021")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),
        legend.position = "none")+
  coord_flip()+
  geom_label(aes(label=paste0("%",porcentaje)))

## Apoyos promedios por Ejido ----

prod_bienestar %>% mutate(is_ejido=ifelse(nombre_del_ejido=="P,PROPIEDAD","Propiedad Privada","Ejido")) %>% 
  group_by(is_ejido,cultivo) %>% summarise(monto=sum(monto_apoyado)) %>% 
  ggplot(aes(factor(is_ejido),monto,group=cultivo,fill=cultivo))+
  geom_col()

prod_bienestar %>% filter(ejido %in% c(0,NA))  %>% summarise(sum(monto_apoyado))

prod_bienestar %>% filter(!ejido %in% c(0,NA))   %>% summarise(sum(monto_apoyado))

































































