## Indicadores de agricultura
## David A. Ortega


# Preparar Entorno ----
rm(list=ls())
dev.off()
pacman::p_load(tidyverse,scales,janitor,plotly,sf)
# Cargar Bases ----
#2020
balanza_agricola <- read_csv("input/agricultura/Cierre_agricola_mun_2020.csv", 
                             locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
# Mapa
mapa_nacional <- read_sf("/Users/NSMJ01TM8X/Desktop/mapas_mx/entidades_mex/ENTIDAD.shp")
# Mapa de Riego y Temporal por Estado

mapa_cierre_2020 <- balanza_agricola %>% group_by(nomestado,nommodalidad) %>% summarise(valor=sum(valorproduccion,na.rm = T)) %>% 
  pivot_wider(names_from=nommodalidad,values_from = valor) %>% mutate(Temporal =ifelse(is.na(Temporal)==T,0,Temporal)) %>%
  mutate(total = Riego+Temporal) %>% 
  mutate(porcentaje_riego = round(Riego/total*100,2)) %>% mutate(porcentaje_temporal = round(100-porcentaje_riego,2)) %>% 
  ungroup() %>% mutate(porcentaje_nacional_riego = round(Riego/sum(Riego)*100,2)) %>% mutate(porcentaje_nacional_temp = round(Temporal/sum(Temporal)*100,2))

mapa_cierre_2020 <- left_join(balanza_agricola %>% filter(nomcultivo_sin_um=="Maíz grano") %>% group_by(nomestado) %>% 
  summarise(volumen_maiz = sum(volumenproduccion),valor_maiz = sum(valorproduccion)), mapa_cierre_2020,by="nomestado") %>% 
  mutate(maiz_contribucion = round(valor_maiz/total*100,2)) %>% mutate(maiz_contribucion_prod_nacional = round(volumen_maiz/sum(volumen_maiz),2)) %>% 
  mutate(NOMBRE = toupper(nomestado))

mapa_cierre_2020$NOMBRE <- gsub("É","E",mapa_cierre_2020$NOMBRE)
mapa_cierre_2020$NOMBRE <- gsub("Á","A",mapa_cierre_2020$NOMBRE)
mapa_cierre_2020$NOMBRE <- gsub("Í","I",mapa_cierre_2020$NOMBRE)
mapa_cierre_2020$NOMBRE <- gsub("Ó","O",mapa_cierre_2020$NOMBRE)

mapa_cierre_2020$NOMBRE[!mapa_cierre_2020$NOMBRE %in% mapa_nacional$NOMBRE]

mapa_nacional <- mapa_nacional %>% left_join(mapa_cierre_2020,by="NOMBRE")


mapa_nacional <- mapa_nacional %>% rename(pr=porcentaje_riego, pt=porcentaje_temporal,
                         pnr=porcentaje_nacional_riego,pnt=porcentaje_nacional_temp,
                         mce=maiz_contribucion,mcn=maiz_contribucion_prod_nacional)

st_write(mapa_nacional,"output/mapa_cierre_agricola_2020.shp")




rm(list=ls())












