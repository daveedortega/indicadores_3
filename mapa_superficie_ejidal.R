## Mapa SUperficie Ejidal Registrada
## David A. Ortega


# Preparar Entorno ----
rm(list=ls())
dev.off()
pacman::p_load(tidyverse,scales,janitor,plotly,sf)
## Cargar bases ----
superficie_ejidal <- read_csv("input/agricultura/superficie_ejidal.csv") %>% clean_names() %>% rename(NOMBRE = entidad_federativa)

mapa_nacional <- read_sf("/Users/NSMJ01TM8X/Desktop/mapas_mx/entidades_mex/ENTIDAD.shp")
## Pegar y escribir mapa
superficie_ejidal$NOMBRE <- gsub("VERACRUZ DE.*","VERACRUZ",superficie_ejidal$NOMBRE)
superficie_ejidal$NOMBRE <- gsub("COAHUILA DE.*","COAHUILA",superficie_ejidal$NOMBRE)
superficie_ejidal$NOMBRE <- gsub("MICHOACAN DE.*","MICHOACAN",superficie_ejidal$NOMBRE)
superficie_ejidal$NOMBRE <- gsub("CIUDAD .*","CIUDAD DE MEXICO",superficie_ejidal$NOMBRE)

superficie_ejidal$NOMBRE[!superficie_ejidal$NOMBRE %in% mapa_nacional$NOMBRE]

mapa_nacional <- left_join(mapa_nacional,superficie_ejidal,by="NOMBRE")

st_write(mapa_nacional,"output/mapa_superficie_ejidal.shp")










