## Indicadores de migración 
## David A.Ortega

## Preparar Entorno ----
pacman::p_load(tidyverse,leaflet,plotly,scales,janitor)
rm(list=ls())
dev.off()
## cargar base ----
us_census_immigration <- read_csv("input/migracion/us_migration_census.csv") %>% clean_names()
us_population <- read_csv("input/migracion/us_statepopulation_2020.csv") %>% clean_names()
#Corregimos Tab Error
us_census_immigration$grupo <- gsub("<a0>","",us_census_immigration$grupo)
us_census_immigration$subgrupo <- gsub("<a0>","",us_census_immigration$subgrupo)
us_census_immigration$subsubgrupo <- gsub("<a0>","",us_census_immigration$subsubgrupo)
us_census_immigration$dato <- gsub("<a0>","",us_census_immigration$dato)
# Deseleccionamos errores estandar
us_census_immigration <- us_census_immigration %>% select(!ends_with("error"))
## Menu de Categorías ----
us_census_immigration %>% count(grupo) %>% as.data.frame()
us_census_immigration %>% View()

# Poblacin Migrante por Estado ----
poblacion_migrante_xestado <- us_census_immigration %>% filter(grupo=="Foreign-born population") %>% 
  select(!c(subgrupo,subsubgrupo,dato)) %>% pivot_longer(!grupo,names_to = "estado",values_to = "migrantes") %>% 
  mutate(migrantes = as.numeric(str_remove_all(migrantes,",")))
  
poblacion_migrante_xestado %>% View()
# Use str_remove and stringr functions in order to pipe gsub

poblacion_migrante_xestado <- poblacion_migrante_xestado %>% filter(str_detect(estado,"total")) %>% separate(estado,into = c("estado","a","b")) %>% 
  mutate(a = ifelse(a=="total","",a)) %>% mutate(b = ifelse(b%in%c("estimate","total"),"",b)) %>% 
  unite("estado",c(estado,a,b),sep=" ") %>% mutate(estado=toupper(str_squish(estado))) %>% 
  left_join(us_population %>% mutate(estado=toupper(estado))) %>% 
  mutate(porcentaje = round(migrantes/poblacion_2020*100,2)) 

poblacion_migrante_xestado %>% ggplot(aes(reorder(estado,migrantes),migrantes,fill=estado))+
  geom_col()+
  theme(legend.position = "none")+
  geom_label(aes(label = comma(migrantes,prefix = paste0(estado," "))))+
  coord_flip()+
  scale_y_sqrt()+
  labs(x="",y="Número de Migrantes",title="Número de Migrantes por Estado en 2020", subtitle = "De acuerdo al Censo de 2020",
       caption = "Fuente: US CENSUS 2020")+
  theme(plot.title = element_text(size=20,face="bold",color="#9f2241"),plot.subtitle = element_text(size=16,face="bold"))

poblacion_migrante_xestado %>% ggplot(aes(reorder(estado,porcentaje),porcentaje,fill=estado))+
  geom_col()+
  theme(legend.position = "none")+
  geom_label(aes(label=comma(porcentaje,suffix = " %",prefix = paste0(estado, ": "),accuracy = 0.01)))+
  coord_flip()+
  labs(x="",y="Porcentaje de Migrantes",title="Migrantes como Proporción de la población del Estado en 2020", 
       subtitle = "De acuerdo al Censo de 2020", caption = "Fuente: US CENSUS 2020")+
  theme(plot.title = element_text(size=20,face="bold",color="#9f2241"),plot.subtitle = element_text(size=16,face="bold"))



## Por si están o no Naturalizados

tipo_migrante <- us_census_immigration %>% filter(grupo=="CITIZENSHIP") %>% select(!c(grupo,subgrupo,subsubgrupo)) %>% 
  pivot_longer(!dato,names_to = "estado",values_to = "porcentaje") %>% separate(estado,into = c("estado","a","b","c","d","e","f")) %>% 
  mutate(estado=toupper(str_squish(estado))) %>% 
  filter(b=="estimate") %>% mutate(porcentaje_migrantes = 1/100*as.numeric(str_remove(porcentaje,"%"))) %>% 
  select(!c(porcentaje,c,d,e,f)) %>% 
  left_join(poblacion_migrante_xestado,by="estado") %>% 
  mutate(num_nat=porcentaje_migrantes*migrantes)
  
# Porcentaje de Migrantes Naturalizados por Estado
tipo_migrante %>% ggplot(aes(reorder(estado,poblacion_2020),porcentaje_migrantes,group=dato,fill=dato))+
    geom_col()+
  labs(x="",y="Porcentaje de Migrantes",title="Población Migrante Naturalizada y No Naturalizada en EEUU", 
       subtitle = "De acuerdo al Censo de 2020 por Estado", caption = "Fuente: US CENSUS 2020",fill="Calidad Jurídica")+
  theme(plot.title = element_text(size=20,face="bold",color="#9f2241"),plot.subtitle = element_text(size=16,face="bold"), 
        legend.position = "bottom")+
  coord_flip()+
  geom_text(aes(label=comma(porcentaje_migrantes*100,accuracy = 0.1,suffix = "%")),position = position_stack(vjust = 0.5))

# Porcentaje de Migrantes Naturalizados por Estado
tipo_migrante %>% group_by(dato) %>% summarise(nn = sum(num_nat)) %>% ungroup() %>% mutate(porcent = nn/sum(nn)) %>% 
  ggplot(aes(x="",y=nn,fill=dato))+
  geom_bar(stat="identity",width = 1)+
  coord_polar("y",start=0)+
  geom_label(y=c(850000000000,6210000000),aes(label = paste0(dato,": ",comma(nn))),size=9)+
  labs(x="",y="",title = "Población Migrante Naturalizada y No Naturalizada en EEUU",subtitle = "De acuerdo al Censo de 2020",
       caption = "Fuente: US CENSUS 2020", fill="")+
  theme(plot.title = element_text(size=20,face="bold",color="#9f2241"),
        plot.subtitle = element_text(size=16,face="bold"),legend.position = "bottom")

# Fuentes de Empleo
us_census_immigration %>% count() %>% as.data.frame()

migrantes_tipo_salario <- us_census_immigration %>% filter(grupo=="CLASS OF WORKER") %>% select(!c(grupo,subgrupo,subsubgrupo)) %>% 
  pivot_longer(!dato,names_to = "estado",values_to = "porcentaje") %>% 
  filter(str_detect(estado,"total"))  %>% mutate(porcentaje_migrantes = 1/100*as.numeric(str_remove(porcentaje,"%"))) %>% 
  separate(estado,into = c("estado","a","b")) %>% 
  mutate(a = ifelse(a=="total","",a)) %>% mutate(b = ifelse(b%in%c("estimate","total"),"",b)) %>% 
  unite("estado",c(estado,a,b),sep=" ") %>% mutate(estado=toupper(str_squish(estado))) %>% 
  left_join(poblacion_migrante_xestado,by="estado") %>% 
  mutate(migarntes_tipo_trabajo = porcentaje_migrantes*migrantes) %>% 
  group_by(dato) %>% summarise(migrantes = sum(migarntes_tipo_trabajo)) %>% 
  ungroup() %>% mutate(porcentaje = migrantes/sum(migrantes)*100)

migrantes_tipo_salario %>% 
  ggplot(aes(x="",y=porcentaje,fill=dato))+
  geom_bar(stat="identity",width = 1)+
  coord_polar("y",start=0)+
  geom_label(y=c(90,50,20,1),aes(label = paste0(dato,": ",comma(porcentaje))),size=4)+
  labs(x="",y="",title = "Población Migrante por Fuente de Empleo",subtitle = "De acuerdo al Censo de 2020",
       caption = "Fuente: US CENSUS 2020", fill="")+
  theme(plot.title = element_text(size=20,face="bold",color="#9f2241"),
        plot.subtitle = element_text(size=16,face="bold"),legend.position = "bottom")


# Raza
us_census_immigration %>% count(grupo) %>% as.data.frame()

# poblacion y raza
una_mas_razas <- us_census_immigration %>% filter(grupo =="RACE AND HISPANIC OR LATINO ORIGIN",dato!="One race") %>% 
  select(!c(grupo,subgrupo,subsubgrupo)) %>% 
  pivot_longer(!dato,names_to = "estado",values_to = "porcentaje") %>% 
  filter(str_detect(estado,"total"))  %>% mutate(porcentaje_migrantes = 1/100*as.numeric(str_remove(porcentaje,"%"))) %>% 
  separate(estado,into = c("estado","a","b")) %>% 
  mutate(a = ifelse(a=="total","",a)) %>% mutate(b = ifelse(b%in%c("estimate","total"),"",b)) %>% 
  unite("estado",c(estado,a,b),sep=" ") %>% mutate(estado=toupper(str_squish(estado))) %>% 
  left_join(poblacion_migrante_xestado,by="estado") %>% 
  mutate(migrantes_raza = porcentaje_migrantes*migrantes) %>% 
  select(!c(porcentaje.x,grupo,grupo,poblacion_2020,porcentaje.y))


# 
una_mas_razas %>% group_by(dato) %>% summarise(migrantes = sum(migrantes_raza)) %>% ungroup() %>%  mutate(porcentaje = migrantes/sum(migrantes)) %>% 
  ggplot(aes(reorder(dato,migrantes),migrantes,fill=dato))+
  geom_col()+
  geom_label(aes(label = paste0(comma(migrantes))),size=4)+
  labs(x="",y="",title = "Población Migrante por raza",subtitle = "Autoreportada, de acuerdo al Censo de 2020",
       caption = "Fuente: US CENSUS 2020", fill="")+
  theme(plot.title = element_text(size=20,face="bold",color="#9f2241"),
        plot.subtitle = element_text(size=16,face="bold"),legend.position = "none",axis.text.y = element_text(size=10,color="black"))+
  scale_y_sqrt()+
  coord_flip()

  # coord_polar("y",start=0)+

# # Porcentajes por Estado de una Raza
# una_raza <- us_census_immigration %>% filter(grupo =="RACE AND HISPANIC OR LATINO ORIGIN",subgrupo=="One race",dato!="One race") %>% 
#   select(!c(grupo,subgrupo,subsubgrupo)) %>% 
#   pivot_longer(!dato,names_to = "estado",values_to = "porcentaje") %>% 
#   filter(str_detect(estado,"total"))  %>% mutate(porcentaje_migrantes = 1/100*as.numeric(str_remove(porcentaje,"%"))) %>% 
#   separate(estado,into = c("estado","a","b")) %>% 
#   mutate(a = ifelse(a=="total","",a)) %>% mutate(b = ifelse(b%in%c("estimate","total"),"",b)) %>% 
#   unite("estado",c(estado,a,b),sep=" ") %>% mutate(estado=toupper(str_squish(estado))) %>% mutate(subgrupo="One race") %>% 
#   select(!porcentaje)
# 
# # Porcentajes más de una raza
# mas_de_una_raza <- us_census_immigration %>% filter(grupo =="RACE AND HISPANIC OR LATINO ORIGIN",subgrupo!="One race",dato!="Two or more races") %>% 
#   select(!c(grupo,subgrupo,subsubgrupo)) %>% 
#   pivot_longer(!dato,names_to = "estado",values_to = "porcentaje") %>% 
#   filter(str_detect(estado,"total"))  %>% mutate(porcentaje_migrantes = 1/100*as.numeric(str_remove(porcentaje,"%"))) %>% 
#   separate(estado,into = c("estado","a","b")) %>% 
#   mutate(a = ifelse(a=="total","",a)) %>% mutate(b = ifelse(b%in%c("estimate","total"),"",b)) %>% 
#   unite("estado",c(estado,a,b),sep=" ") %>% mutate(estado=toupper(str_squish(estado))) %>% mutate(subgrupo="Two or more races") %>% 
#   select(!porcentaje)
# 
# # join dummy
# 
# dummy <- rbind(una_raza,mas_de_una_raza)

one_race_us %>% 
  ggplot(aes(x="",y=porcentaje,fill=dato))+
  geom_bar(stat="identity",width = 1)+
  coord_polar("y",start=0)+
  geom_label(y=c(10,80,70,40,0,10),aes(label = paste0(dato,": ",comma(porcentaje))),size=4)+
  labs(x="",y="",title = "Población Migrante por Fuente de Empleo",subtitle = "De acuerdo al Censo de 2020",
       caption = "Fuente: US CENSUS 2020", fill="")+
  theme(plot.title = element_text(size=20,face="bold",color="#9f2241"),
        plot.subtitle = element_text(size=16,face="bold"),legend.position = "bottom")










