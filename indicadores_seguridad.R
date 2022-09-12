## Indicadores de XXX
## David A. Ortega


# Preparar Entorno ----
rm(list=ls())
dev.off()
pacman::p_load(tidyverse,scales,janitor)


# Cargar BdD ----

IDEFC_jun2021 <- read_csv("input/seguridad/IDEFC_jun2021.csv", 
                           locale = locale(encoding = "ISO-8859-2")) %>% clean_names()

IDEFC_NM_may22 <- read_csv("input/seguridad/IDEFC_NM_may22.csv", 
                           locale = locale(encoding = "ISO-8859-2")) %>% clean_names()
IDVFC_NM_may22 <- read_csv("input/IDVFC_NM_may22.csv", 
                           locale = locale(encoding = "ISO-8859-2")) %>% clean_names()

# Analisis ----

IDEFC_NM_may22 %>% select(!bien_juridico_afectado) %>% count(subtipo_de_delito) %>% as.data.frame()

catalogo_revision <- c("Violación equiparada","Violación simple","Acoso sexual",
  "Violencia de género en todas sus modalidades distinta a la violencia familia","Violencia familiar", "Abuso sexual")

# 

IDEFC_NM_may22 %>% select(!bien_juridico_afectado) %>% filter(subtipo_de_delito %in% catalogo_revision) %>% 
  select(ano,entidad,tipo_de_delito,enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre) %>% 
  pivot_longer(!c(ano,entidad,tipo_de_delito),names_to = "mes",values_to = "delitos") %>% 
  group_by(ano,tipo_de_delito) %>% summarise(delitos = sum(delitos)) %>% 
  ggplot(aes(ano,delitos,group=tipo_de_delito,color=tipo_de_delito))+
  geom_line(size=1)+
  labs(x="",y="Carpetas de Investitigación",
       title = "Carpetas de Investigación iniciadas en México por delitos que afectan mayoritariamente a mujeres"
       ,subtitle = "Entre 2005 y 2022", caption = "Fuente: SESNEP", color="Delito")+
  theme(plot.title = element_text(size=18,face="bold"),plot.subtitle = element_text(size=16), legend.position = "bottom")+
  geom_label(data = labs,aes(label = paste0 (ano,": ",comma(delitos))))+
  scale_y_sqrt()

labs = IDEFC_NM_may22 %>% select(!bien_juridico_afectado) %>% filter(subtipo_de_delito %in% catalogo_revision) %>% 
  select(ano,entidad,tipo_de_delito,enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre) %>% 
  pivot_longer(!c(ano,entidad,tipo_de_delito),names_to = "mes",values_to = "delitos") %>% 
  group_by(ano,tipo_de_delito) %>% summarise(delitos = sum(delitos)) %>% filter(ano %in% c(2021,2016,2018))
 
# Tasa de Crecimiento Mensual 

IDEFC_NM_may22 %>% select(!bien_juridico_afectado) %>% filter(subtipo_de_delito %in% catalogo_revision) %>% 
  select(ano,entidad,tipo_de_delito,enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre) %>% 
  pivot_longer(!c(ano,entidad,tipo_de_delito),names_to = "mes",values_to = "delitos") %>% 
  mutate(mes = case_when(mes=="enero"~1,
                         mes=="febrero"~2,
                         mes=="marzo"~3,
                         mes=="abril"~4,
                         mes=="mayo"~5,
                         mes=="junio"~6,
                         mes=="julio"~7,
                         mes=="agosto"~8,
                         mes=="septiembre"~9,
                         mes=="octubre"~10,
                         mes=="noviembre"~11,
                         mes=="diciembre"~12)) %>% 
  mutate(previos = ifelse(ano<2019,"EPN","AMLO")) %>% 
  group_by(entidad,previos) %>% summarise(delitos = sum(delitos,na.rm = T)) %>% ungroup() %>% group_by(previos) %>%  
  mutate(porcentaje = round(delitos / sum(delitos)*100,2)) %>% 
  ggplot(aes(reorder(entidad,delitos),delitos,group=factor(previos),fill=entidad))+
  geom_col()+
  facet_wrap(~previos)+
  labs(x="",y="Número de Carpetas de Investigación",
       title="Carpetas de Investigación Abiertas por Delitos que afectan principalmente a las mujeres", 
       subtitle="Entre 2015 y 2021, dividido por Sexenio", caption="Fuente: SESNEP")+
  theme(plot.title = element_text(size=18,face="bold"),plot.subtitle = element_text(size=16), legend.position = "none", 
        axis.text.x = element_text(angle = 90))+
  geom_text(aes(label = comma(delitos)),angle=90)


# Concentración por Estado

IDEFC_NM_may22 %>% select(!bien_juridico_afectado) %>% filter(subtipo_de_delito %in% catalogo_revision) %>% 
  select(ano,entidad,tipo_de_delito,enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre) 

# 

IDVFC_NM_may22 %>% count(subtipo_de_delito) %>% as.data.frame()



IDVFC_NM_may22 %>% filter(tipo_de_delito %in% c("Homicidio","Secuestro","Trata de personas","Feminicidio")) %>% 
  select(ano,entidad,tipo_de_delito,sexo,rango_de_edad,
         enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre) %>% 
  pivot_longer(c(enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre),
               names_to = "mes",values_to = "delitos") %>% 
  group_by(ano,tipo_de_delito,sexo) %>% summarise(delitos=sum(delitos,na.rm = T)) %>% filter(sexo!="No identificado") %>% 
  ggplot(aes(ano,delitos,group=tipo_de_delito,color=tipo_de_delito))+
  geom_line(size=1.5)+
  geom_label(data=labs,aes(label=paste0(ano,": ",comma(delitos))))+
  labs(x="",y="Carpetas Iniciadas",title="Número de Carpetas Abiertas por tipo de Delito a Nivel Federal",
       subtitle="En México entre 2015 y Mayo 2022", caption="Fuente: SESNEP",color="Tipo de Delito")+
  theme(plot.title = element_text(size=18,face="bold"),plot.subtitle = element_text(size=16), legend.position = "bottom")+
  facet_wrap(~sexo)+
  scale_y_log10()
  

labs <- IDVFC_NM_may22 %>% filter(tipo_de_delito %in% c("Homicidio","Secuestro","Trata de personas","Feminicidio")) %>% 
  select(ano,entidad,tipo_de_delito,sexo,rango_de_edad,
         enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre) %>% 
  pivot_longer(c(enero,febrero,marzo,abril,mayo,junio,julio,agosto,septiembre,octubre,noviembre,diciembre),
               names_to = "mes",values_to = "delitos") %>% 
  group_by(ano,tipo_de_delito,sexo) %>% summarise(delitos=sum(delitos,na.rm = T)) %>%
  filter(sexo!="No identificado",ano %in% c(2016,2020,2021,2022))





















