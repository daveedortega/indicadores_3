## Estadísticos Educación 
## Datos de SEP - INEGI
## David A. Ortega 

## Preparar Espacio ----
pacman::p_load(tidyverse,janitor,readxl,scales)
rm(list=ls())
dev.off()
## Cargar Datos ------
puja_nacional <- read_csv("./input/educacion/educacion_nacional.csv") %>% clean_names()
matricula <- read_csv("./input/educacion/matricula.csv") %>% clean_names()

## Puja ------
puja_nacional %>% colnames()
puja_nacional %>% count(entidad)

puja_nacional %>% filter(entidad=="09 Ciudad de México",edad!="Total",
                         !edad%in%c("03-05 años","06-09 años")) %>% select(edad,secundaria_1_a_2_grados)

## Puja con Secundaria Terminada 
puja_nacional %>% filter(entidad=="09 Ciudad de México",edad!="Total",
                         !edad%in%c("03-05 años","06-09 años")) %>% 
  ggplot(aes(edad,secundaria_3_grados))+
  geom_col(fill="#235B4E")+
  coord_flip()+
  labs(x="Número de Personas",y="Rango de Edad",title="Número de Personas en la CDMX con Secundaria Terminada",
       subtitle = "Por rango de Edad en 2020")+
  geom_label(aes(label=comma(secundaria_3_grados)))

## Puja con 1 o 2 años de secundaria
puja_nacional %>% filter(entidad=="09 Ciudad de México",edad!="Total",!edad%in%c("03-05 años","06-09 años")) %>% 
  ggplot(aes(edad,secundaria_1_a_2_grados))+
  geom_col(fill="#235B4E")+
  coord_flip()+
  labs(x="Número de Personas",y="Rango de Edad",title="Número de Personas en la CDMX con 1 a 2 años de Secundaria Terminada",
       subtitle = "Por rango de Edad en 2020")+
  geom_label(aes(label=comma(secundaria_1_a_2_grados)))


# total de graduados de secundaria sin bachillerato -

bachillerato <- 455030
# 
estados_revision <-c("09 Ciudad de México","10 Durango","15 México","17 Morelos","20 Oaxaca")
matricula <- c(455030,75714,658668,74708,145548)
maestros <-  c(35191,6546,59464,7553,10790)
poblacion <- c(9209944, 1832650,  16992418, 1971520,4132148 )
comparativa_estados <- data.frame(entidad=estados_revision,matricula,maestros, poblacion)

# Porcentaje de Cobertura educativa calculada
puja_nacional %>% filter(entidad %in% estados_revision) %>% select(entidad,secundaria_3_grados) %>% 
  group_by(entidad)%>% summarise(demanda_potencial_secundaria = sum(secundaria_3_grados)) %>%
  left_join(comparativa_estados,by="entidad") %>% mutate(porcentaje_cobertura=matricula/demanda_potencial_secundaria*100) %>% 
  ggplot(aes(entidad,porcentaje_cobertura))+
  geom_col(fill="#BC955C")+
  labs(title = "Porcentaje de Cobertura Educativa Potencial",subtitle = "Calculado como:Demanda Potencial Total / Matricula Total Actual",
       x="",y="Porcentaje")+
  geom_label(aes(label=paste0(comma(porcentaje_cobertura),"%")))

# Porcentaje de Cobertura educativa en rango de edad, "tasa neta de mat4riculación"
puja_nacional %>% filter(entidad %in% estados_revision,edad=="15-19 años") %>% select(entidad,secundaria_3_grados) %>% 
  group_by(entidad)%>% summarise(demanda_potencial_secundaria = sum(secundaria_3_grados)) %>%
  left_join(comparativa_estados,by="entidad") %>% mutate(porcentaje_cobertura=(matricula/demanda_potencial_secundaria) *100) %>% 
  ggplot(aes(entidad,porcentaje_cobertura))+
  geom_col(fill="#9F2241")+
  labs(title = "Porcentaje de Cobertura Educativa Potencial para Rango de Edad Bachillerato"
       ,subtitle = "Calculado como:Demanda Potencial Total / Matricula Total Actual",
       x="",y="Porcentaje")+
  geom_label(aes(label=paste0(comma(porcentaje_cobertura),"%")))

# Alumnos potenciales para cada Profesor de Bachillerato

puja_nacional %>% filter(entidad %in% estados_revision,edad=="15-19 años") %>% select(entidad,secundaria_3_grados) %>% 
  group_by(entidad)%>% summarise(demanda_potencial_secundaria = sum(secundaria_3_grados)) %>%
  left_join(comparativa_estados,by="entidad") %>% mutate(prop_alumnos = demanda_potencial_secundaria/maestros) %>% 
  ggplot(aes(entidad,prop_alumnos))+
  geom_col(fill="#DDc9A3")+
  labs(title = "Número de Alumnos potenciales para cada Profesor de Bachillerato",x="",y="Alumnos")+
  geom_label(aes(label=paste0(round(prop_alumnos,2))))


## Estimador Cobertura 2020/2021


puja_nacional %>% filter(entidad=="09 Ciudad de México",edad!="Total",!edad%in%c("03-05 años","06-09 años")) %>% 
  summarise(sum(secundaria_3_grados))

coberturabachiller_tot<- 455030/1488720

## No. de Persoans Curzando la Preparatoria / Número de personas (entre 14 y 17 ) curzando la secundaria

puja_nacional %>% filter(entidad=="09 Ciudad de México",edad!="Total",edad=="10-14 años") %>% 
  summarise(sum(secundaria_total)) 

coberturabachiller_14_17 <- 455030/221789

## No. Maestros MS

maestrosms_tot<- 35191
maestrosms_priv<- 7960
maestros_pub <- 27231


## Total de gente curzando 2021 ----

puja_nacional %>% filter(entidad=="09 Ciudad de México") %>% 
  select(preparatoria_o_bachillerato_total) %>% summarise(sum(preparatoria_o_bachillerato_total))


puja_nacional %>% filter(entidad=="09 Ciudad de México") %>% 
  select(secundaria_3_grados) %>% summarise(sum(secundaria_3_grados))



puja_nacional %>% filter(entidad=="09 Ciudad de México",edad=="15-19 años") %>% 
  select(secundaria_3_grados) %>% summarise(sum(secundaria_3_grados))

maestroxal_bach <- 348905/35191
mxa_sec <- 2977440/35191



