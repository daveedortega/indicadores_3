## Indicadores de agricultura
## David A. Ortega


# Preparar Entorno ----
rm(list=ls())
dev.off()
pacman::p_load(tidyverse,scales,janitor,plotly)
# Cargar BdD ----
# Imports - Exports BANXICO
balanza_agricola <- read_csv("input/agricultura/balanza_agricola_sie.csv", 
                             locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
# Balanza Agrícola Desglosada SIE - BANXICO
balanza_desglosada <- read_csv("input/agricultura/balanza_agricola_sie_desglosada.csv") %>% clean_names()

# cierres Agrícolas SIAP
cierre_agricola <- read_csv("input/agricultura/Cierre_agricola_1980.csv", 
                            locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
for (i in 1981:2002){
  print(i)
  cierre_agricola <- rbind(cierre_agricola,read_csv(paste0("input/agricultura/Cierre_agricola_",i,".csv"), 
                                                    locale = locale(encoding = "ISO-8859-2")) %>% clean_names())
}
cierre_agricola_2 <- read_csv("input/agricultura/Cierre_agricola_mun_2003.csv", 
                              locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
for (i in 2003:2014){
  print(i)
  cierre_agricola_2 <- rbind(cierre_agricola_2,read_csv(paste0("input/agricultura/Cierre_agricola_mun_",i,".csv"), 
                                                        locale = locale(encoding = "ISO-8859-2")) %>% clean_names())
}
cierre_agricola_3 <- read_csv("input/agricultura/Cierre_agricola_mun_2015.csv", 
                              locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
for (i in 2016:2020){
  print(i)
  cierre_agricola_3 <- rbind(cierre_agricola_3,read_csv(paste0("input/agricultura/Cierre_agricola_mun_",i,".csv"), 
                                                        locale = locale(encoding = "ISO-8859-2")) %>% clean_names())
}
# Completo con columnas selectas

SIAP_completo <- rbind(cierre_agricola %>% select(anio,nomestado,nomcultivo,nomcicloproductivo,sembrada,cosechada,volumenproduccion,
                                                  nomunidad,rendimiento,precio,valorproduccion ),
                       cierre_agricola_2  %>% select(anio,nomestado,nomcultivo,nomcicloproductivo,sembrada,cosechada,volumenproduccion,
                                                     nomunidad,rendimiento,precio,valorproduccion ),
                       cierre_agricola_3 %>% select(anio,nomestado,nomcultivo=nomcultivo_sin_um,nomcicloproductivo,sembrada,cosechada,
                                                    volumenproduccion,nomunidad,rendimiento,precio,valorproduccion ))
# Composición PIB Sector Primario -  INEGI
composion_pib_primario <- read_csv("input/agricultura/composicion_pib_sectorprimario.csv") %>% clean_names()


## Balanza Agricola a Pesos ----
balanza_agricola <- balanza_agricola %>% mutate(fecha=as.Date(fecha,tryFormats="%d/%m/%Y")) %>% 
  pivot_longer(!fecha,names_to = "concepto",values_to = "mdd") %>% 
  mutate(dolares=mdd*1000) %>% mutate(ano=format(fecha,format="%Y")) %>% left_join(dolar_peso_final,by = "ano") %>% 
  left_join(deflactor_2013_anual,by="ano") %>% 
  mutate(normalizado=(dolares*tipo_de_cambio)/deflactor)
## Plots ----

balanza_agricola %>% count(concepto) %>% as.data.frame()
# Balanza Total
plotly_test <- balanza_agricola %>%filter(concepto %in% c("exportaciones_totales_no_petroleras_agropecuarias","importacion_de_productos_agropecuarios"))%>% 
  mutate(concepto=ifelse(concepto=="exportaciones_totales_no_petroleras_agropecuarias","Exportaciones","Importaciones"))%>% 
  ggplot(aes(fecha,normalizado,group=concepto,color=concepto))+
  geom_line(size=1)+
  geom_smooth()+
  labs(x="",y="Pesos", title = "Balanza Agrícola en Pesos desde 1993",subtitle = "Deflactada a Pesos de 2013 con Deflactor de INEGI",
       caption = "Fuentes: SIE - Banxico, INEGI",color="Concepto")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16),legend.position = "bottom")+
  geom_label(data=labs,aes(label=paste0(format(fecha,format="%Y-%m"),": $",comma(normalizado))),nudge_x = -1000)
  
labs <- balanza_agricola %>% filter(concepto %in% c("exportaciones_totales_no_petroleras_agropecuarias","importacion_de_productos_agropecuarios"))%>% 
  filter(fecha=="2022-04-01")%>% 
  mutate(concepto=ifelse(concepto=="exportaciones_totales_no_petroleras_agropecuarias","Exportaciones","Importaciones"))

ggplotly(plotly_test)

# Exports - Imports
exports <- balanza_agricola %>%filter(concepto == c("exportaciones_totales_no_petroleras_agropecuarias"))%>% 
  mutate(concepto=ifelse(concepto=="exportaciones_totales_no_petroleras_agropecuarias","Exportaciones","Importaciones")) %>%
  select(fecha,exports=normalizado)


imports <- balanza_agricola %>%filter(concepto == c("importacion_de_productos_agropecuarios"))%>% 
  mutate(concepto=ifelse(concepto=="exportaciones_totales_no_petroleras_agropecuarias","Exportaciones","Importaciones")) %>% 
  select(fecha,imports=normalizado)
# balanza
balanza_ag <- exports %>% left_join(imports,by="fecha") %>% mutate(balanza=exports-imports)

balanza_ag %>% 
  ggplot(aes(fecha,balanza))+
  geom_line(y=0,size=1)+
  geom_line(color="#9f2241",size=1)+
  geom_smooth(color="#235b4e")+
  labs(x="",y="Pesos", title = "Balanza Agrícola en Pesos desde 1993",subtitle = "Deflactada a Pesos de 2013 con Deflactor de INEGI",
       caption = "Fuentes: SIE - Banxico, INEGI",color="Concepto")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=16))+
  geom_label(data=labs,aes(label=paste0(ano,": $",comma(balanza))),nudge_x=-100)

labs <- balanza_ag %>% mutate(ano = format(fecha,format="%Y-%m")) %>% filter(ano %in% c("1999-03","2022-04","2014-01"))

##  Seleccion de granos
cierre_agricola %>% count(nomcultivo) %>% as.data.frame()

cierre_agricola %>% group_by(nomcultivo) %>% summarise(valor=sum(valorproduccion)) %>% arrange(desc(valor))

seleccion_fyv <- c("Maíz grano","Frijol","Tomate rojo (jitomate)","Trigo grano","Limón","Cana de azúcar","Aguacate")

colnames(cierre_agricola_2)

granos_selectos <- rbind(cierre_agricola %>% filter(nomcultivo %in% seleccion_fyv) %>% select(anio,nomestado,nomcultivo,sembrada,cosechada,volumenproduccion,
                                                                         rendimiento,precio),
cierre_agricola_2 %>% filter(nomcultivo %in% seleccion_fyv) %>% select(anio,nomestado,nomcultivo,sembrada,cosechada,volumenproduccion,
                                                                         rendimiento,precio),
cierre_agricola_3 %>% filter(nomcultivo_sin_um %in% seleccion_fyv) %>% select(anio,nomestado,nomcultivo=nomcultivo_sin_um,sembrada,
                                                                              cosechada,volumenproduccion,rendimiento,precio))
# Hectareas 

granos_selectos %>% group_by(anio,nomcultivo) %>% summarise(sembrada=sum(sembrada),
                                                            cosechada=sum(cosechada),volumenproduccion=sum(volumenproduccion)) %>% 
  pivot_longer(c(sembrada,cosechada,volumenproduccion),names_to = "categoria",values_to = "valor") %>% 
  filter(categoria!="volumenproduccion") %>% 
  ggplot(aes(anio,valor,group=nomcultivo,color=nomcultivo))+
  geom_line(size=1)+
  geom_smooth()+
  labs(x="",y="hectareas",title = "Número de Hectareas Cosechadas de granos selectos",
       subtitle = "Entre 1980 y 2020 en todo México", caption = "Fuente: SIAP",color="* Cultivo: ")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=15),
        strip.text = element_text(face="bold"),legend.position = "bottom",legend.text = element_text(size=12,face="bold"))+
  scale_y_sqrt()+
  geom_label(data=labs,aes(label=paste0(anio, ": ",comma(valor))),nudge_x = -2)+
  facet_wrap(~categoria,scales = "free_y")

labs <- granos_selectos %>% group_by(anio,nomcultivo) %>% summarise(sembrada=sum(sembrada),
                                                            cosechada=sum(cosechada),volumenproduccion=sum(volumenproduccion)) %>% 
  pivot_longer(c(sembrada,cosechada,volumenproduccion),names_to = "categoria",values_to = "valor") %>% 
  filter(anio %in% c(2003,2020),categoria!="volumenproduccion")

# Volumen de Produccion 

granos_selectos %>% group_by(anio,nomcultivo) %>% summarise(sembrada=sum(sembrada),
                                                            cosechada=sum(cosechada),volumenproduccion=sum(volumenproduccion)) %>% 
  pivot_longer(c(sembrada,cosechada,volumenproduccion),names_to = "categoria",values_to = "valor") %>% 
  filter(categoria=="volumenproduccion") %>% 
  ggplot(aes(anio,valor,group=nomcultivo,color=nomcultivo))+
  geom_line(size=1)+
  geom_smooth()+
  labs(x="",y="Toneladas",title = "Volumen Cosechadas de granos selectos en Toneladas",
       subtitle = "Entre 1980 y 2020 en todo México", caption = "Fuente: SIAP",color="* Cultivo: ")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=15),
        strip.text = element_text(face="bold"),legend.position = "bottom",legend.text = element_text(size=12,face="bold"))+
  scale_y_sqrt()+
  geom_label(data=labs,aes(label=paste0(anio, ": ",comma(valor))),nudge_x = -2)

labs <- granos_selectos %>% group_by(anio,nomcultivo) %>% summarise(sembrada=sum(sembrada),
                                                                    cosechada=sum(cosechada),volumenproduccion=sum(volumenproduccion)) %>% 
  pivot_longer(c(sembrada,cosechada,volumenproduccion),names_to = "categoria",values_to = "valor") %>% 
  filter(anio %in% c(2003,2020,2017,1990),categoria=="volumenproduccion")

# Maíz:


granos_selectos %>% group_by(anio,nomcultivo) %>% summarise(sembrada=sum(sembrada),
                                                            cosechada=sum(cosechada),volumenproduccion=sum(volumenproduccion)) %>% 
  pivot_longer(c(sembrada,cosechada,volumenproduccion),names_to = "categoria",values_to = "valor") %>% 
  filter(nomcultivo=="Maíz grano",categoria=="volumenproduccion") %>% 
  ggplot(aes(anio,valor))+
  geom_line(size=2,color="#9f2241")+
  geom_smooth()+
  labs(x="",y="Toneladas",title = "Volumen de Producción de maíz en grano en México",
       subtitle = "En toneladas, entre 1980 y 2020 en todo México", caption = "Fuente: SIAP",color="* Cultivo: ")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=15),
        strip.text = element_text(face="bold"),legend.position = "bottom",legend.text = element_text(size=12,face="bold"))+
  scale_y_sqrt()+
  geom_label(data=labs,aes(label=paste0(anio, ": ",comma(valor))),nudge_x = -2)
  # facet_wrap(~categoria)

labs <- granos_selectos %>% group_by(anio,nomcultivo) %>% summarise(sembrada=sum(sembrada),
                                                                    cosechada=sum(cosechada),volumenproduccion=sum(volumenproduccion)) %>% 
  pivot_longer(c(sembrada,cosechada,volumenproduccion),names_to = "categoria",values_to = "valor") %>% 
  filter(anio %in% c(2003,2020,2017,1990),nomcultivo=="Maíz grano",categoria=="volumenproduccion")


# Frijol:


granos_selectos %>% group_by(anio,nomcultivo) %>% summarise(sembrada=sum(sembrada),
                                                            cosechada=sum(cosechada),volumenproduccion=sum(volumenproduccion)) %>% 
  pivot_longer(c(sembrada,cosechada,volumenproduccion),names_to = "categoria",values_to = "valor") %>% 
  filter(nomcultivo=="Frijol",categoria=="volumenproduccion") %>% 
  ggplot(aes(anio,valor))+
  geom_line(size=1,color="#9f2241")+
  geom_smooth()+
  labs(x="",y="Toneladas",title = "Volumen Cosechadas de Frijol en Toneladas",
       subtitle = "Entre 1980 y 2020 en todo México", caption = "Fuente: SIAP",color="* Cultivo: ")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=15),
        strip.text = element_text(face="bold"),legend.position = "bottom",legend.text = element_text(size=12,face="bold"))+
  scale_y_sqrt()+
  geom_label(data=labs,aes(label=paste0(anio, ": ",comma(valor))),nudge_x = -2)+
  facet_wrap(~categoria)

labs <- granos_selectos %>% group_by(anio,nomcultivo) %>% summarise(sembrada=sum(sembrada),
                                                                    cosechada=sum(cosechada),volumenproduccion=sum(volumenproduccion)) %>% 
  pivot_longer(c(sembrada,cosechada,volumenproduccion),names_to = "categoria",values_to = "valor") %>% 
  filter(anio %in% c(2003,2020,2017,1990),nomcultivo=="Frijol",categoria=="volumenproduccion")

## Sembrado Cosechado Frijol

granos_selectos %>% group_by(anio,nomcultivo) %>% summarise(sembrada=sum(sembrada),
                                                            cosechada=sum(cosechada),volumenproduccion=sum(volumenproduccion)) %>% 
  pivot_longer(c(sembrada,cosechada,volumenproduccion),names_to = "categoria",values_to = "valor") %>% 
  filter(nomcultivo=="Frijol",categoria!="volumenproduccion") %>% 
  ggplot(aes(anio,valor,grouo=categoria,color=categoria))+
  geom_line(size=1,color="#9f2241")+
  geom_smooth()+
  labs(x="",y="Hectareas",title = "Hectareas Sembradas y Cosechadas de Frijol en Toneladas",
       subtitle = "Entre 1980 y 2020 en todo México", caption = "Fuente: SIAP",color="* Cultivo: ")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=15),
        strip.text = element_text(face="bold"),legend.position = "bottom",legend.text = element_text(size=12,face="bold"))+
  scale_y_sqrt()+
  geom_label(data=labs,aes(label=paste0(anio, ": ",comma(valor))),nudge_x = -2)

labs <- granos_selectos %>% group_by(anio,nomcultivo) %>% summarise(sembrada=sum(sembrada),
                                                                    cosechada=sum(cosechada),volumenproduccion=sum(volumenproduccion)) %>% 
  pivot_longer(c(sembrada,cosechada,volumenproduccion),names_to = "categoria",values_to = "valor") %>% 
  filter(anio %in% c(2003,2020,2017,1990),nomcultivo=="Frijol",categoria!="volumenproduccion")

## Sembrado Cosechado Maíz


granos_selectos %>% group_by(anio,nomcultivo) %>% summarise(sembrada=sum(sembrada),
                                                            cosechada=sum(cosechada),volumenproduccion=sum(volumenproduccion)) %>% 
  pivot_longer(c(sembrada,cosechada,volumenproduccion),names_to = "categoria",values_to = "valor") %>% 
  filter(nomcultivo=="Maíz grano",categoria!="volumenproduccion") %>% 
  ggplot(aes(anio,valor,grouo=categoria,color=categoria))+
  geom_line(size=1,color="#9f2241")+
  geom_smooth()+
  labs(x="",y="Hectareas",title = "Hectareas Sembradas y Cosechadas de Maíz en Toneladas",
       subtitle = "Entre 1980 y 2020 en todo México", caption = "Fuente: SIAP",color="* Cultivo: ")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=15),
        strip.text = element_text(face="bold"),legend.position = "bottom",legend.text = element_text(size=12,face="bold"))+
  scale_y_sqrt()+
  geom_label(data=labs,aes(label=paste0(anio, ": ",comma(valor))),nudge_x = -2)

labs <- granos_selectos %>% group_by(anio,nomcultivo) %>% summarise(sembrada=sum(sembrada),
                                                                    cosechada=sum(cosechada),volumenproduccion=sum(volumenproduccion)) %>% 
  pivot_longer(c(sembrada,cosechada,volumenproduccion),names_to = "categoria",values_to = "valor") %>% 
  filter(anio %in% c(2003,2020,2017,1990),nomcultivo=="Maíz grano",categoria!="volumenproduccion")


# Limpiamos Espacio

rm(list=c("i","sleccion_fyv","labs","deflactor2013","anos_faltantes","cierre_agricola_2","cierre_agricola_3"))

## precio de la tonelada de maíz 

precios_maiz <- granos_selectos %>% filter(nomcultivo =="Maíz grano",anio>1992) %>% select(ano=anio,nomestado,precio)

deflactor_2013_anual <- deflactor_2013_anual %>% mutate(ano=as.numeric(ano))

precios_maiz %>% group_by(ano) %>% summarise(precio=mean(precio)) %>% left_join(deflactor_2013_anual) %>% 
  mutate(deflactado=precio/deflactor) %>% 
  ggplot(aes(ano,deflactado))+
  geom_line(y=3406,size=1)+
  geom_smooth(size=1,se=F)+
  geom_line(color="#9f2241",size=2)+
  labs(x="",y="Pesos de 2013",title = "Precio promedio del maíz en grano por tonelada en México entre 1993 y 2020",
       subtitle = "Deflactado a pesos de 2013",caption = "Fuente: SIAP - INEGI")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=15))+
  geom_label(data=labs,aes(label=paste0(ano,": ",comma(deflactado))))+
  geom_label(data=data.frame(ano=2019,deflactado=3406),aes(label=paste0("Precio Promedio: ",comma(deflactado))))
  # geom_line(y=4112.435,color="#235b4e",size=2)+
  # geom_label(data=data.frame(ano=2017,deflactado=4112.435),aes(label=paste0("Precio De Garantía 2020 (5610 + 150) deflactado: ",
                                                                           # comma(deflactado))),nudge_y = 50)
  
labs <- precios_maiz %>% group_by(ano) %>% summarise(precio=mean(precio)) %>% left_join(deflactor_2013_anual) %>% 
  mutate(deflactado=precio/deflactor) %>% filter(ano %in% c(2000,1993,2010,2016,2020))
  
# Precio Garantía 2020
(5610+150)/(1.40063)
# Frijol

precios_frijol <- granos_selectos %>% filter(nomcultivo =="Frijol",anio>1992) %>% select(ano=anio,nomestado,precio)


precios_frijol %>% group_by(ano) %>% summarise(precio=mean(precio)) %>% left_join(deflactor_2013_anual) %>% 
  mutate(deflactado=precio/deflactor) %>% 
  ggplot(aes(ano,deflactado))+
  geom_line(y=12112,size=1)+
  geom_smooth(size=1,se=F)+
  geom_line(color="#9f2241",size=1)+
  labs(x="",y="Pesos de 2013",title = "Precio promedio del frijol por tonelada en México entre 1993 y 2020",
       subtitle = "Deflactado a pesos de 2013",caption = "Fuente: SIAP - INEGI")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=15))+
  geom_label(data=labs,aes(label=paste0(ano,": ",comma(deflactado))))+
  geom_label(data=data.frame(ano=2019,deflactado=12112),aes(label=paste0("Precio Promedio: ",comma(12112))),nudge_y = 200)+
  geom_line(y=10352.48,color="#235b4e",size=2)+
  geom_label(data=data.frame(ano=2017,deflactado= 10352.48),aes(label=paste0("Precio De Garantía 2020 (14500) deflactado: ",
                                                                            comma(deflactado))),nudge_y = 200)

labs <- precios_frijol %>% group_by(ano) %>% summarise(precio=mean(precio)) %>% left_join(deflactor_2013_anual) %>% 
  mutate(deflactado=precio/deflactor) %>% filter(ano %in% c(2000,1993,2010,2016,2020))
  
# Precio Garantía 2020
14500/(1.40063)

# Precio aguacate

precios_aguacate <- granos_selectos %>% filter(nomcultivo =="Aguacate",anio>1992) %>% select(ano=anio,nomestado,precio)
deflactor_2013_anual <- deflactor_2013_anual %>% mutate(ano=as.numeric(ano))

precios_aguacate %>% group_by(ano) %>% summarise(precio=mean(precio)) %>% left_join(deflactor_2013_anual) %>% 
  mutate(deflactado=precio/deflactor) %>% 
  ggplot(aes(ano,deflactado))+
  geom_line(y=8503,size=1)+
  geom_smooth(size=1,se=F)+
  geom_line(color="#9f2241",size=1)+
  labs(x="",y="Pesos de 2013",title = "Precio promedio del Aguacate por tonelada en México entre 1993 y 2020",
       subtitle = "Deflactado a pesos de 2013",caption = "Fuente: SIAP - INEGI")+
  theme(plot.title = element_text(size=20,face="bold"),plot.subtitle = element_text(size=15))+
  geom_label(data=labs,aes(label=paste0(ano,": ",comma(deflactado))))+
  geom_label(data=data.frame(ano=2019,deflactado=8503),aes(label=paste0("Precio Promedio: ",comma(8503))),nudge_y = 200)

labs <- precios_aguacate %>% group_by(ano) %>% summarise(precio=mean(precio)) %>% left_join(deflactor_2013_anual) %>% 
  mutate(deflactado=precio/deflactor) %>% filter(ano %in% c(2000,1993,2010,2016,2020))

## SIAP Completo ----


plotly_test <- SIAP_completo %>% filter(nomestado=="Guerrero",nomunidad=="Tonelada") %>% group_by(anio,nomcultivo) %>% 
  summarise(valor=sum(valorproduccion,na.rm=T),sembrada=sum(sembrada),cosechada = sum(cosechada),volumen=sum(volumenproduccion)) %>% 
  ggplot(aes(anio,valor,color = nomcultivo))+
  geom_line() +
  theme(legend.position = "none")+
  scale_y_log10()

ggplotly(plotly_test)


SIAP_completo %>% filter(nomestado=="Guerrero",nomunidad=="Tonelada") %>% group_by(anio,nomcultivo) %>% 
  summarise(volumen=sum(volumenproduccion),sembrada=sum(sembrada),cosechada = sum(cosechada), valor=sum(valorproduccion)) %>%
  ungroup() %>% arrange(desc(valor))
## SIAP 2020 ----
siap_2020 <- SIAP_completo %>% filter(anio==2020)
siap_2020 %>% summarise(sum(valorproduccion))
 
# Composicion PIB Primario ----

composion_pib_primario %>% pivot_longer(!c(ano,trimestre),names_to = "concepto",values_to = "mdp") %>% 
  filter(concepto!="sector_primario") %>% 
  mutate(fecha = case_when(trimestre == "T1" ~ as.Date(paste0(ano,"-01-01")),
                           trimestre == "T2" ~ as.Date(paste0(ano,"-04-01")),
                           trimestre == "T3" ~ as.Date(paste0(ano,"-07-01")),
                           trimestre == "T4" ~ as.Date(paste0(ano,"-10-01")),
                           )) %>% filter(trimestre=="T4") %>% 
  mutate(concepto = case_when(concepto=="cria_y_explotacion_de_animales"~"Cría y Explotación de Animales",
                              concepto=="aprovechamiento_forestal"~"Aprovechamiento Forestal",
                              concepto=="servicios_relacionados_con_las_actividades_agropecuarias_y_forestales"~"Servicios Relacionados",
                              concepto=="agricultura"~"Agricultura",
                              concepto=="pesca_caza_y_captura"~"Pesca, Caza y Captura")) %>% 
  ggplot(aes(fecha,mdp,group=concepto,color=concepto))+
    geom_line(size=1.5)+
  scale_y_sqrt()+
  labs(x="",y="Millones de Pesos",title="Producto Interno Bruto del Sector Primario Nacional",
       subtitle = "Desglosado por actividad, entre 1993 y 2022 por trimestre, en pesos de 2013", caption = "Fuente: INEGI",color="Concepto")+
  theme(plot.title = element_text(size=22,face="bold",color = "#9f2441"),plot.subtitle = element_text(size=14,face="bold"),
        legend.position = "bottom",legend.text = element_text(size=12,face="bold"))+
  geom_label(data=labs,aes(label =paste0(ano,": $",comma(mdp))))

labs <- composion_pib_primario %>% pivot_longer(!c(ano,trimestre),names_to = "concepto",values_to = "mdp") %>% 
  filter(concepto!="sector_primario") %>% 
  mutate(fecha = case_when(trimestre == "T1" ~ as.Date(paste0(ano,"-01-01")),
                           trimestre == "T2" ~ as.Date(paste0(ano,"-04-01")),
                           trimestre == "T3" ~ as.Date(paste0(ano,"-07-01")),
                           trimestre == "T4" ~ as.Date(paste0(ano,"-10-01")))) %>% filter(trimestre=="T4") %>% 
  mutate(concepto = case_when(concepto=="cria_y_explotacion_de_animales"~"Cría y Explotación de Animales",
                              concepto=="aprovechamiento_forestal"~"Aprovechamiento Forestal",
                              concepto=="servicios_relacionados_con_las_actividades_agropecuarias_y_forestales"~"Servicios Relacionados",
                              concepto=="agricultura"~"Agricultura",
                              concepto=="pesca_caza_y_captura"~"Pesca, Caza y Captura")) %>% 
  filter(ano %in% c(2021,2018,2000))



#
composion_pib_primario %>% pivot_longer(!c(ano,trimestre),names_to = "concepto",values_to = "mdp") %>% 
  filter(concepto!="sector_primario") %>% 
  mutate(fecha = case_when(trimestre == "T1" ~ as.Date(paste0(ano,"-01-01")),
                           trimestre == "T2" ~ as.Date(paste0(ano,"-04-01")),
                           trimestre == "T3" ~ as.Date(paste0(ano,"-07-01")),
                           trimestre == "T4" ~ as.Date(paste0(ano,"-10-01")),
  )) %>% filter(ano %in% c(2021,2022)) %>% 
  mutate(concepto = case_when(concepto=="cria_y_explotacion_de_animales"~"Cría y Explotación de Animales",
                              concepto=="aprovechamiento_forestal"~"Aprovechamiento Forestal",
                              concepto=="servicios_relacionados_con_las_actividades_agropecuarias_y_forestales"~"Servicios Relacionados",
                              concepto=="agricultura"~"Agricultura",
                              concepto=="pesca_caza_y_captura"~"Pesca, Caza y Captura")) %>% 
  group_by(ano,trimestre) %>% mutate(porcentaje  = round(mdp /sum(mdp)*100,2)) %>% 
  filter(trimestre =="T4") %>% 
  ggplot(aes(x="",y=porcentaje,group=concepto,fill=concepto))+
  geom_bar(stat="identity",width = 1)+
  coord_polar("y",start = 0)+
  labs(x="",y="",title="Aportación al Producto Interno Bruto del Sector Primario Nacional",
       subtitle = "Desglosado por actividad, en el 4to Trimestre de 2021", caption = "Fuente: INEGI",fill="Concepto")+
  theme(plot.title = element_text(size=22,face="bold",color = "#9f2441"),plot.subtitle = element_text(size=14,face="bold"),
        legend.position = "bottom",legend.text = element_text(size=12,face="bold"))+
  geom_label(y=c(80,20,30,10,0),aes(label = comma(porcentaje,suffix = "%")),size=8)



# Balanza Agrícola Desglosada -----

balanza_desglosada %>% glimpse()

balanza_desglosada <- balanza_desglosada %>% pivot_longer(!fecha,names_to = "concepto",values_to = "mdd") %>% 
  mutate(tipo = case_when(str_detect(concepto,"exportacion_")~"Exportaciones",
                                              str_detect(concepto,"importacion_")~"Importaciones")) %>% 
  filter(!is.na(tipo)) %>% 
  mutate(fecha = as.Date(fecha,tryFormats="%d/%m/%Y")) %>% 
  filter(concepto !="importacion_de_productos_agropecuarios") %>% 
  mutate(concepto = str_remove(concepto,"exportacion_de_productos_agropecuarios_")) %>% 
  mutate(concepto = str_remove(concepto,"importacion_de_productos_agropecuarios_")) %>% 
  mutate(concepto = str_replace_all(concepto,"_"," "))


balanza_desglosada %>% filter(concepto %in% c("trigo","aguacates","frijol","leche y sus derivados","maiz","fresas frescas","camaron congelado")) %>% 
  filter(fecha > "2021-12-01") %>% 
  ggplot(aes(reorder(concepto,mdd),mdd,group=tipo,fill=tipo))+
  geom_col(position = position_dodge(width=1))

# Principales Exportaciones ----
balanza_desglosada %>% filter(fecha > "2021-12-01") %>% filter(tipo=="Exportaciones") %>% group_by(concepto,tipo) %>% summarise(mdd=sum(mdd)) %>% 
  ggplot(aes(reorder(concepto,mdd),mdd,fill=concepto))+
  geom_col()+
  geom_label(aes(label=comma(mdd,prefix = "$")))+
  labs(x="",y="Miles de Dólares", title="Principales Exportaciones Agropecuarias de México",subtitle ="En miles de Dólares entre enero y mayo 2022",
       caption="Fuente: Banxico - SIE 2022")+
  theme(plot.title = element_text(size=22,face="bold",color = "#9f2441"),plot.subtitle = element_text(size=14,face="bold"),
        legend.position = "none",axis.text.y = element_text(size=14,color = "black"))+
  coord_flip()


balanza_desglosada %>% filter(fecha > "2021-12-01") %>% filter(tipo=="Importaciones") %>% group_by(concepto,tipo) %>% summarise(mdd=sum(mdd)) %>% 
  ggplot(aes(reorder(concepto,mdd),mdd,fill=concepto))+
  geom_col()+
  geom_label(aes(label=comma(mdd,prefix = "$")))+
  labs(x="",y="Miles de Dólares", title="Principales Importaciones Agropecuarias de México",subtitle ="En miles de Dólares entre enero y mayo 2022",
       caption="Fuente: Banxico - SIE 2022")+
  theme(plot.title = element_text(size=22,face="bold",color = "#9f2441"),plot.subtitle = element_text(size=14,face="bold"),
        legend.position = "none",axis.text.y = element_text(size=14,color = "black"))+
  coord_flip()





























