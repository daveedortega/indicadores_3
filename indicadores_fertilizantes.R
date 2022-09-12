## Indicadores de fertilizantes
## David A. Ortega


# Preparar Entorno ----
rm(list=ls())
dev.off()
pacman::p_load(tidyverse,scales,janitor)


# Cargar Base -----
#2020 - FALTA 8ava lista, no se pudo descargar 
padron_fertilizantes_2020 <- rbind(read_csv("input/agricultura/fertilizantes/2020/1-listado-de-beneficiarios-del-programa-fertilizantes-del-ano-2020.csv",
                                            locale=locale(encoding = "ISO-8859-2")),
                                   read_csv("input/agricultura/fertilizantes/2020/2-parte-del-listado-de-beneficiarios-del-programa-de-fertilizantes-del-ano-2020.csv",
                                            locale=locale(encoding = "ISO-8859-2")),
                                   read_csv("input/agricultura/fertilizantes/2020/3-listado-de-beneficiarios-del-programa-de-fertilizantes-del-ano-2020.csv",
                                            locale=locale(encoding = "ISO-8859-2")),
                                   read_csv("input/agricultura/fertilizantes/2020/4-listado-de-beneficiarios-del-programa-fertilizantes-del-ano-2020.csv",
                                            locale=locale(encoding = "ISO-8859-2")),
                                   read_csv("input/agricultura/fertilizantes/2020/5-listado-de-beneficiarios-del-programa-fertilizantes-del-ano-2020.csv",
                                            locale=locale(encoding = "ISO-8859-2")),
                                   read_csv("input/agricultura/fertilizantes/2020/6-listado-de-beneficiarios-del-programa-fertilizantes-del-ano-2020.csv",
                                            locale=locale(encoding = "ISO-8859-2")),
                                   read_csv("input/agricultura/fertilizantes/2020/7-listado-de-beneficiarios-del-programa-fertilizantes-del-ano-2020.csv",
                                            locale=locale(encoding = "ISO-8859-2")),
                                   read_csv("input/agricultura/fertilizantes/2020/9-listado-de-beneficiarios-del-programa-fertilizantes-del-ano-2020.csv",
                                            locale=locale(encoding = "ISO-8859-2")),
                                   read_csv("input/agricultura/fertilizantes/2020/10-listado-de-beneficiarios-del-programa-fertilizantes-del-ano-2020.csv",
                                            locale=locale(encoding = "ISO-8859-2")),
                                   read_csv("input/agricultura/fertilizantes/2020/11-listado-de-beneficiarios-del-programa-fertilizantes-del-ano-2020.csv",
                                            locale=locale(encoding = "ISO-8859-2"))) %>% clean_names()

municipios <- padron_fertilizantes_2020 %>% count(municipio) %>% select(municipio)

padron_fertilizantes_2020_2 <- rbind(read_csv("input/agricultura/fertilizantes/2020/13-listado-de-fertilizante.csv",
                                            locale=locale(encoding = "ISO-8859-2")),
                                   read_csv("input/agricultura/fertilizantes/2020/14-listado-de-fertilizante.csv",
                                            locale=locale(encoding = "ISO-8859-2")),
                                   read_csv("input/agricultura/fertilizantes/2020/15-listado-de-fertilizante.csv",
                                            locale=locale(encoding = "ISO-8859-2")),
                                   read_csv("input/agricultura/fertilizantes/2020/16-listado-de-fertilizante.csv",
                                            locale=locale(encoding = "ISO-8859-2")),
                                   read_csv("input/agricultura/fertilizantes/2020/17-listado-de-fertilizante.csv",
                                            locale=locale(encoding = "ISO-8859-2")),
                                   read_csv("input/agricultura/fertilizantes/2020/18-listado-de-fertilizante-18-listado-31mar.csv",
                                            locale=locale(encoding = "ISO-8859-2"))) %>% clean_names() 
#12 tiene un error de columna, borramos y agregamos
fertilizantes_12_2020 <- read_csv("input/agricultura/fertilizantes/2020/12-listado-de-fertilizante.csv",
         locale=locale(encoding = "ISO-8859-2")) %>% select(!`...6`) %>% clean_names()
padron_fertilizantes_2020_2 <- rbind(padron_fertilizantes_2020_2,fertilizantes_12_2020)

#Vemos municipio, estado
municipio_estado <- padron_fertilizantes_2020_2 %>% count(estado,municipio) %>% select(!n)
# La mitad no tiene estado, lo intentamos sacar con municipio - estado de los otros
padron_fertilizantes_2020 <- padron_fertilizantes_2020 %>% left_join(municipio_estado,by="municipio") %>% 
  select(numero,estado,municipio,nombre=nombre_completo,numero_de_paquete_s)
#padron completo
padron_fertilizantes_2020_completo <- rbind(padron_fertilizantes_2020,padron_fertilizantes_2020_2)

# 2021
padron_fertilizantes_2021 <- rbind(read_csv("input/agricultura/fertilizantes/2021/1er-listado-fertilizantes-2021v2.csv",
                                 locale=locale(encoding = "ISO-8859-2")),
                              read_csv("input/agricultura/fertilizantes/2021/2do-listado-de-fertilizante-2021-corte-7-abrilv2.csv",
                                       locale=locale(encoding = "ISO-8859-2")),
                              read_csv("input/agricultura/fertilizantes/2021/3er-listado-de-fertilizante-2021-corte-17-juniov2.csv",
                                       locale=locale(encoding = "ISO-8859-2")),
                              read_csv("input/agricultura/fertilizantes/2021/4to-listado-fertilizantes-2021v2.csv",
                                       locale=locale(encoding = "ISO-8859-2")),
                              read_csv("input/agricultura/fertilizantes/2021/5to-listado-de-fertilizante-2021-corte-5-agostov2.csv",
                                       locale=locale(encoding = "ISO-8859-2")),
                              read_csv("input/agricultura/fertilizantes/2021/6to-listado-fertilizantes-2021v2.csv",
                                       locale=locale(encoding = "ISO-8859-2")),
                              read_csv("input/agricultura/fertilizantes/2021/7mo-listado-fertilizantes-2021v2.csv",
                                       locale=locale(encoding = "ISO-8859-2")),
                              read_csv("input/agricultura/fertilizantes/2021/8vo-listado-fertilizantes-2021v2.csv",
                                       locale=locale(encoding = "ISO-8859-2")),
                              read_csv("input/agricultura/fertilizantes/2021/9no-listado-fertilizantes-2021.csv",
                                       locale=locale(encoding = "ISO-8859-2"))) %>% clean_names()

#2022

padron_fertilizantes_2022 <- rbind(read_csv("input/agricultura/fertilizantes/2022/1er-listado_fertilizantes-22.csv",
                                            locale=locale(encoding = "ISO-8859-2")),
                                   read_csv("input/agricultura/fertilizantes/2022/2do-listado_fertilizantes-22.csv",
                                            locale=locale(encoding = "ISO-8859-2")),
                                   read_csv("input/agricultura/fertilizantes/2022/3er-listado_fertilizantes-22.csv",
                                            locale=locale(encoding = "ISO-8859-2"))) %>% clean_names()
# Beneficiados por estado 2020 ----
padron_fertilizantes_2020_completo %>% count(estado) %>%
  ggplot(aes(reorder(estado,n),n,fill=estado))+
  geom_col()+
  geom_label(aes(label = comma(n)),size=10)+
  labs(x="",y="Número de Beneficiados",title = "Número de Beneficiados en 2021 por el Programa Fertilizantes para el Bienestar",
       subtitle = "Última actualización: a Marzo 2020",caption = "Fuente: SADER")+
  theme(plot.title = element_text(size=20,face="bold",color="#9f4221"),plot.subtitle = element_text(size=16,face="bold"),
        axis.text.x = element_text(size=12,color="black"),legend.position = "none")+
  scale_y_sqrt()
  


# Número de Beneficiados por Estado 2021 ----

padron_fertilizantes_2021 %>% count(estado) %>% 
  ggplot(aes(reorder(estado,n),n,fill=estado))+
  geom_col()+
  geom_label(aes(label = comma(n)),size=10)+
  labs(x="",y="Número de Beneficiados",title = "Número de Beneficiados en 2021 por el Programa Fertilizantes para el Bienestar",
       subtitle = "Última actualización: a Marzo 2021",caption = "Fuente: SADER")+
  theme(plot.title = element_text(size=20,face="bold",color="#9f4221"),plot.subtitle = element_text(size=16,face="bold"),
        axis.text.x = element_text(size=12,color="black"),legend.position = "none")+
  scale_y_sqrt()

# vemos cómo ha cambiado 2020 y 2022 ----
padron_fertilizantes_2020_completo <- padron_fertilizantes_2020_completo %>% mutate(ano=2020)
padron_fertilizantes_2021 <- padron_fertilizantes_2021 %>% mutate(ano=2021)
padron_fertilizantes_2022 <- padron_fertilizantes_2022 %>% mutate(ano=2022)

padron_completo <- rbind(padron_fertilizantes_2020_completo,padron_fertilizantes_2021,padron_fertilizantes_2022)

padron_completo %>% count(estado,ano) %>% ggplot(aes(estado,n,group=factor(ano),fill=estado))+
  geom_col(position = position_dodge(width =1))+
  geom_label(aes(label = paste0(ano,": ",comma(n))),position=position_dodge(1),size=5)+
  scale_y_sqrt()+
  labs(x="",y="Número de Beneficiados",title="Número de beneficiados de Fertilizantes para el Bienestar",
       subtitle="Por Estado entre 2020 y 2022",caption = "Fuente: SADER")+
  theme(plot.title = element_text(size=20,face="bold",color="#9f4221"),plot.subtitle = element_text(size=16,face="bold"),
        axis.text.x = element_text(size=14,color="black"),legend.position = "none")

# Por monto de apoyos

padron_completo %>% group_by(estado,ano) %>% summarise(monto =sum(numero_de_paquete_s)) %>% 
  ggplot(aes(estado,monto,group=factor(ano),fill=estado))+
  geom_col(position = position_dodge(width =1))+
  geom_label(aes(label = paste0(ano,": ",comma(monto))),position=position_dodge(1),size=5)+
  scale_y_sqrt()+
  labs(x="",y="Número de Bultos",title="Número de Bultos de Fertilizantes entregados para el Bienestar",
       subtitle="Por Estado entre 2020 y 2022",caption = "Fuente: SADER")+
  theme(plot.title = element_text(size=20,face="bold",color="#9f4221"),plot.subtitle = element_text(size=16,face="bold"),
        axis.text.x = element_text(size=14,color="black"),legend.position = "none")

























































































