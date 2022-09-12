## Indicadores de Economía para estacionalizar cosas
## David A. Ortega


# Preparar Entorno ----
# rm(list=ls())
dev.off()
pacman::p_load(tidyverse,scales,janitor)

# Cargar Bases----
dolar_peso <- read_csv("input/serie_dolar_peso.csv") %>% clean_names()
deflactor_2013 <- read_csv("input/deflactor_2013.csv")
#


# deflactor anual ----

deflactor_2013$Periodo <- gsub("/","-",deflactor_2013$Periodo)

deflactor_2013_anual <- deflactor_2013[grep("04",deflactor_2013$Periodo),]

deflactor_2013_anual <- deflactor_2013_anual %>% mutate(deflactor=Dato/100) %>% separate(Periodo,into = c("ano","trimestre")) %>% 
  select(ano,deflactor)

deflactor_2013_anual <- rbind(deflactor_2013[grep("2022",deflactor_2013$Periodo),] %>%  mutate(deflactor=Dato/100)%>% separate(Periodo,into = c("ano","trimestre")) %>% 
                                select(ano,deflactor),deflactor_2013_anual)

# Serie del Dolar peso anualizada
dolar_peso_31 <- dolar_peso %>% separate(fecha, into=c("dia","mes","ano")) %>% filter(mes=="12",dia=="31") %>% select(ano,tipo_de_cambio)
dolar_peso_30 <- dolar_peso %>% separate(fecha, into=c("dia","mes","ano")) %>% filter(mes=="12",dia=="30") %>% select(ano,tipo_de_cambio)

##
dolar_peso_casi <- rbind(dolar_peso_30[which(!dolar_peso_30$ano %in% dolar_peso_31$ano),],dolar_peso_31) %>% mutate(ano=as.numeric(ano)) %>% arrange(ano)

dolar_peso %>% as.data.frame()

anos_faltantes = c(1961,1967,1972,1978,1979,1989,1990,1995,2000,2006,2017,2022)

dolar_peso_casi <- rbind(dolar_peso %>% separate(fecha, into=c("dia","mes","ano")) %>% mutate(ano=as.numeric(ano)) %>% filter(ano %in% anos_faltantes,mes=="12") %>% 
                           arrange(desc(dia)) %>% filter(dia==28) %>% select(ano,tipo_de_cambio),dolar_peso_casi) %>% arrange(ano)

dolar_peso_final <- rbind(dolar_peso %>% separate(fecha, into=c("dia","mes","ano")) %>%  filter(ano=="2022",mes=="06",dia=="21") %>%
                            select(ano,tipo_de_cambio),dolar_peso_casi) %>% arrange(ano)
#Limpiar Espacio
rm(list=c("dolar_peso","dolar_peso_30","dolar_peso_31","dolar_peso_casi","anos_faltantes"))