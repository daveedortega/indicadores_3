## Consumo eléctrico por Activodad 1960 - 2022
# David A. Ortega

# Preparar Entorno ----
rm(list=ls())
dev.off()
pacman::p_load(tidyverse,scales,janitor,plotly,sf)
## Cargar bases ----
consumo_energetico <- read_csv("input/energia/consumo_energetico_sectores.csv") %>% clean_names()
# Análisis ----
consumo_energetico %>% glimpse()
# as character
character_years <- consumo_energetico %>% select(!c(x2005,x2006,x2007,x2008,x2009,x2010,x2011,x2012,x2013,x2014,x2015,x2016,x2017,x2019,x2020)) %>%
  pivot_longer(!c(sector,fuente),names_to = "ano",values_to = "petajoules") %>% mutate(ano = as.numeric(str_remove(ano,"x"))) %>%
  mutate(petajoules = as.numeric(petajoules))

# as numeric
numeric_years <- consumo_energetico %>% select(c(sector,fuente,x2005,x2006,x2007,x2008,x2009,x2010,x2011,x2012,x2013,x2014,x2015,x2016,x2017,x2019,x2020)) %>%
  pivot_longer(!c(sector,fuente),names_to = "ano",values_to = "petajoules") %>% mutate(ano = as.numeric(str_remove(ano,"x")))
# Pegamos
# Serie de Tiempo total por sector -----
consumo_energetico <- rbind(character_years,numeric_years)
labs <- consumo_energetico[str_detect(consumo_energetico$sector,pattern = "Total"),]%>% filter(fuente == "Electricidad",ano %in% c(2018,2020,1997))

consumo_energetico[str_detect(consumo_energetico$sector,pattern = "Total"),]%>% filter(fuente == "Electricidad") %>% 
  ggplot(aes(ano, petajoules, group=sector,color = sector))+
  geom_line(size=1)+
  geom_label(data = labs,aes(label = comma(petajoules,prefix = paste0(ano, ": "))),size=8)+
  labs(x = "",y = "Petajoules", title = "Consumo Eléctrico por Sector entre 1965 y 2020 en México",subtitle = "Anual, en petajoules",color = "Sector: ",
       caption = "Fuente: SENER - SIE")+
  theme(legend.position = "bottom", plot.title = element_text(size=22,color = "#9f2441",face="bold"),plot.subtitle = element_text(size=16,face="bold"),
        legend.text = element_text(size=12,color = "black",face="bold"))+
  xlim(1968,2022)

# Consumo por Sector 2020 ----
consumo_energetico[str_detect(consumo_energetico$sector,pattern = "Total"),]%>% filter(fuente == "Electricidad",ano %in% c(2018,2019,2020)) %>% 
  group_by(ano) %>% 
  mutate(porcentaje = round(100*petajoules / sum(petajoules),2)) %>% 
  ggplot(aes(reorder(sector, porcentaje),porcentaje,group = ano, fill = factor(ano)))+
  geom_col(position = position_dodge(width = 0.95))+
  scale_y_sqrt()+
  geom_label(aes(label=comma(porcentaje,suffix = "%")),size=10,position = position_dodge(width = 0.95))+
  labs(x="",y="%", title="Consumo de energía eléctrica total por Sector",subtitle = "En 2020, porcentaje del consumo total",
       caption ="Fuente: SIE",fill="Año:")+
  theme(legend.position = "bottom", plot.title = element_text(size=22,color = "#9f2441",face="bold"),plot.subtitle = element_text(size=16,face="bold"),
        legend.text = element_text(size=12,color = "black",face="bold"),axis.text.x = element_text(size = 12,face="bold",color = "black"))

# Desglose Sector Industrial Sector serie dt 1965 - 2020 ----

consumo_energetico %>% count(sector) %>% as.data.frame()

labs <- consumo_energetico %>% filter(sector %in% c("Industria básica del hierro y el acero", #1
                                                    "Industria Química",#2
                                                    "Pemex Petroquímica",#3
                                                    "Construcción",#4
                                                    "Minería de minerales metálicos y no metálicos",#5
                                                    "Elaboración de azúcares",#6
                                                    "Elaboración de cerveza",#7
                                                    "Elaboración de refrescos, hielo y otras bebidas no alcohólicas, purificación y embotellado de agua", #8
                                                    "Elaboración de productos de tabaco",#9
                                                    "Fabricación de cemento y productos a base de cemento en plantas integradas",#10
                                                    "Fabricación de pulpa, papel y cartón",#11
                                                    "Fabricación de vidrio y productos de vidrio", #12
                                                    "Fabricación de automóviles y camiones",#13
                                                    "Fabricación de productos de hule",#14
                                                    "Fabricación de fertilizantes",#15
                                                    "Otras ramas"),fuente =="Electricidad",
                                      ano %in% c(1997,2010,2020,1980,1990))

consumo_energetico %>% filter(sector %in% c("Industria básica del hierro y el acero", #1
                                            "Industria Química",#2
                                            "Pemex Petroquímica",#3
                                            "Construcción",#4
                                            "Minería de minerales metálicos y no metálicos",#5
                                            "Elaboración de azúcares",#6
                                            "Elaboración de cerveza",#7
                                            "Elaboración de refrescos, hielo y otras bebidas no alcohólicas, purificación y embotellado de agua", #8
                                            "Elaboración de productos de tabaco",#9
                                            "Fabricación de cemento y productos a base de cemento en plantas integradas",#10
                                            "Fabricación de pulpa, papel y cartón",#11
                                            "Fabricación de vidrio y productos de vidrio", #12
                                            "Fabricación de automóviles y camiones",#13
                                            "Fabricación de productos de hule",#14
                                            "Fabricación de fertilizantes",#15
                                            "Otras ramas"),fuente =="Electricidad") %>% 
  ggplot(aes(ano,petajoules,group=sector,color = sector))+
  geom_line(size =1)+
  geom_label(data = labs,aes(label=comma(petajoules,prefix = paste0(ano,": "),accuracy = 0.1)))+
  geom_label(data = subset(labs,ano==1990),aes(label=comma(petajoules,prefix = paste0(sector,": "),accuracy = 0.1)))+
  labs(x = "",y = "Petajoules", title = "Consumo Eléctrico Industrial por Sector entre 1965 y 2020 en México",
       subtitle = "Anual, en petajoules",color = "Sector: ",caption = "Fuente: SENER - SIE")+
  theme(legend.position = "bottom", plot.title = element_text(size=22,color = "#9f2441",face="bold"),plot.subtitle = element_text(size=16,face="bold"),
        legend.text = element_text(size=12,color = "black",face="bold"))+
  xlim(1968,2022)+
  scale_y_sqrt()


consumo_energetico$sector %>% unique()

# Desglose Sector Industrial 2020 ----


consumo_energetico %>% count(sector)


consumo_energetico %>% filter(sector %in% c("Industria básica del hierro y el acero", #1
                                            "Industria Química",#2
                                            "Pemex Petroquímica",#3
                                            "Construcción",#4
                                            "Minería de minerales metálicos y no metálicos",#5
                                            "Elaboración de azúcares",#6
                                            "Elaboración de cerveza",#7
                                            "Elaboración de refrescos, hielo y otras bebidas no alcohólicas, purificación y embotellado de agua", #8
                                            "Elaboración de productos de tabaco",#9
                                            "Fabricación de cemento y productos a base de cemento en plantas integradas",#10
                                            "Fabricación de pulpa, papel y cartón",#11
                                            "Fabricación de vidrio y productos de vidrio", #12
                                            "Fabricación de automóviles y camiones",#13
                                            "Fabricación de productos de hule",#14
                                            "Fabricación de fertilizantes",#15
                                            "Otras ramas"),fuente =="Electricidad",ano==2020) %>% 
  ggplot(aes(reorder(sector,petajoules),petajoules,fill = sector))+
  geom_col()+
  geom_label(aes(label=comma(petajoules)))+
  scale_y_sqrt()+
  labs(x = "",y = "Petajoules", title = "Consumo Eléctrico Industrial por Sector en 2020",
       subtitle = "Anual, en petajoules",color = "Sector: ",caption = "Fuente: SENER - SIE")+
  theme(legend.position = "none", plot.title = element_text(size=22,color = "#9f2441",face="bold"),plot.subtitle = element_text(size=16,face="bold"),
        legend.text = element_text(size=12,color = "black",face="bold"), axis.text.y = element_text(size=12,color="black"))+ 
  coord_flip()+
  scale_x_discrete(labels = wrap_format(30))

































































