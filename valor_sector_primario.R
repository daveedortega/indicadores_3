#### Indicadores de agricultura
## David A. Ortega


# Preparar Entorno ----
dev.off()
pacman::p_load(tidyverse,scales,janitor,plotly)

# Cargar datos ----

pecuario_2020 <- read_csv("input/pecuario/cierre_2020.csv",
                          locale=locale(encoding = "ISO-8859-2")) %>% clean_names()
siap_2020 <- read_csv("input/agricultura/Cierre_agricola_mun_2020.csv",
                      locale=locale(encoding = "ISO-8859-2")) %>% clean_names()
conapesca_2020 <- read_csv("input/pesca/Produccion_Pesquera_2020.csv") %>% clean_names()


#  Valor del Sector Primario 
valor_sect1_2020 <- rbind(siap_2020 %>% group_by(nomestado) %>% summarise(valor=sum(valorproduccion)) %>% mutate(sector="Agricutura") %>% 
          mutate(nomestado=toupper(nomestado)),
        pecuario_2020 %>% group_by(nomestado) %>% summarise(valor=sum(valor)) %>% mutate(valor=valor*1000) 
        %>% mutate(sector="Pecuario") %>% 
          mutate(nomestado=toupper(nomestado)),
        conapesca_2020 %>% group_by(entidad_federativa) %>% summarise(valor=sum(valor_pesos)) %>% 
          mutate(sector="Pesca") %>% select(nomestado=entidad_federativa,valor,sector))

valor_sect1_2020$nomestado <- gsub("Á","A",valor_sect1_2020$nomestado)
valor_sect1_2020$nomestado <- gsub("É","E",valor_sect1_2020$nomestado)
valor_sect1_2020$nomestado <- gsub("Í","I",valor_sect1_2020$nomestado)
valor_sect1_2020$nomestado <- gsub("Ó","O",valor_sect1_2020$nomestado)
valor_sect1_2020$nomestado <- gsub("Ú","U",valor_sect1_2020$nomestado)

#Total por sector
plotly_test <- valor_sect1_2020 %>% group_by(sector,nomestado) %>% summarise(valor=sum(valor)/deflactor*100) %>% ungroup() %>% group_by(nomestado) %>%  
  mutate(aportacion = round(valor/sum(valor),4)*100) %>% 
  ggplot(aes(x="",aportacion,fill=sector))+
  geom_bar(stat="identity",width = 1)+
  coord_polar("y",start=0)+
  facet_wrap(~nomestado)
ggplotly(plotly_test)

deflactor <- 140.063

valor_sect1_2020 %>% summarise(sum(valor))

#
pecuario_2020 %>% count(nomespecie)
siap_2020 %>% count(nomcultivo_sin_um)
conapesca_2020 %>% count(nombre_principal)










