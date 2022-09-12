## Mapa Entidades - Aporte al PIB Sector Primario
## David A.Ortega

## Preparar Entorno ----
pacman::p_load(tidyverse,leaflet,plotly,scales,janitor,sf)
rm(list=ls())
dev.off()
## cargar base ----
sector_primario <- read_csv("input/macroeconomía/pibe_sector_primario.csv", 
                            locale = locale(encoding = "ISO-8859-2"))
mapa_nacional <- read_sf("/Users/NSMJ01TM8X/Desktop/mapas_cdmx/ENTIDAD.shp")

## Datos Abiertos 
colnames(sector_primario)[19:20] <- c("2019","2020")
sector_primario <- sector_primario %>% pivot_longer(!c(Estado,Variable),names_to = "ano",values_to = "mdp") %>%
  pivot_wider(names_from = Variable, values_from = mdp) %>% clean_names()

# Pegar Datos 2020 en mapa

sector_primario <- sector_primario %>% mutate(participacion = round(pib_sector_primario/pib*100,2)) %>% 
  mutate(ano=as.numeric(ano)) %>% filter(ano==2020) %>% mutate(NOMBRE = toupper(estado)) 

sector_primario$NOMBRE <- gsub("Ó","O",sector_primario$NOMBRE)
sector_primario$NOMBRE <- gsub("Í","I",sector_primario$NOMBRE)
sector_primario$NOMBRE <- gsub("É","E",sector_primario$NOMBRE)
sector_primario$NOMBRE <- gsub("CDMX", "CIUDAD DE MEXICO",sector_primario$NOMBRE)
sector_primario$NOMBRE <- gsub("ESTADO DE MEXICO", "MEXICO",sector_primario$NOMBRE)

sector_primario$NOMBRE[!sector_primario$NOMBRE %in% mapa_nacional$NOMBRE]

# local/Nacional 
nacional <- sector_primario %>% filter(estado=="Nacional")
nacional <- nacional$pib_sector_primario

sector_primario <- sector_primario %>% mutate(aportacion_nacional = round(pib_sector_primario/nacional*100,2))

# Escribir
mapa_nacional <- mapa_nacional %>% left_join(sector_primario,by="NOMBRE")

st_write(mapa_nacional,"output/mapa_sector_primario.shp")

plot(mapa_nacional)












