## Indicadores Ejidatarios
## David A. Ortega


# Preparar Entorno ----
rm(list=ls())
dev.off()
pacman::p_load(tidyverse,scales,janitor,sf)
## Cargar base ----
registro_nacional_agrario <- read_csv("input/agricultura/censo_registroagricolanacional.csv") %>% clean_names() %>% 
  rename(NOMBRE =nombre)

mapa_nacional <- read_sf("/Users/NSMJ01TM8X/Desktop/mapas_mx/entidades_mex/ENTIDAD.shp")


registro_nacional_agrario$NOMBRE[!registro_nacional_agrario$NOMBRE %in% mapa_nacional$NOMBRE]
mapa_nacional$NOMBRE

registro_nacional_agrario$NOMBRE <- gsub("COAHUILA DE ZARAGOZA","COAHUILA",registro_nacional_agrario$NOMBRE)
registro_nacional_agrario$NOMBRE <- gsub("É","E",registro_nacional_agrario$NOMBRE)
registro_nacional_agrario$NOMBRE <- gsub("Á","A",registro_nacional_agrario$NOMBRE)
registro_nacional_agrario$NOMBRE <- gsub("Ó","O",registro_nacional_agrario$NOMBRE)
registro_nacional_agrario$NOMBRE <- gsub("Í","I",registro_nacional_agrario$NOMBRE)
registro_nacional_agrario$NOMBRE <- gsub("MICHOACAN DE OCAMPO","MICHOACAN",registro_nacional_agrario$NOMBRE)
registro_nacional_agrario$NOMBRE <- gsub("VERACRUZ DE LA LLAVE","VERACRUZ",registro_nacional_agrario$NOMBRE)


registro_nacional_agrario <- registro_nacional_agrario %>% rename(ehc = ejidatarios_hombres_certificados, 
                                     emc = ejidatarios_mujeres_certificados,
                                     etc = ejidatarios_total_certificados,
                                     chc = comuneros_hombres_certificados,
                                     cmc = comuneros_mujeres_certificados,
                                     ctc = comuneros_total_certificados,
                                     phc = posesionarios_hombres_certificados,
                                     pmc = posesionarios_mujeres_certificados,
                                     ptc = posesionarios_total_certificados,
                                     ahc =avecindados_hombres_certificados,
                                     amc = avecindados_mujeres_certificados,
                                     atc = avecindados_total_certificados,
                                     hc = hombres_certificados,
                                     mc = mujeres_certificados,
                                     tc = total_certificados,
                                     tnc =total_no_certificados,
                                     ehcn = ejidatarios_hombres_no_certificados,
                                     emnc = ejidatariosmujeres_no_certificados,
                                     etnc = ejidatarios_total_no_certificados,
                                     chnc = comuneros_hombres_no_certificados,
                                     cmnc = comuneros_mujeres_no_certificados,
                                     ctnc = comuneros_total_no_certificados,
                                     hnc = hombres_no_certificados, 
                                     mnc = mujeres_no_certificados,
                                     tnc = total_no_certificados)

registro_nacional_agrario %>% colnames()

mapa_ra <- left_join(mapa_nacional,registro_nacional_agrario,by="NOMBRE")

st_write(mapa_ra,"output/mapa_cna.shp")



































