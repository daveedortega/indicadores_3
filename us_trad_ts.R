## Indicadores de Intercambio EEUU - Mundo
## David A. Ortega


# Preparar Entorno ----
rm(list=ls())
dev.off()
pacman::p_load(tidyverse,scales,janitor)
# Cargar bdd ----
imp_exp_eeuu <- read_csv("input/macroeconomía/imports_exports_us.csv")
usd_deflator <- read_csv("input/macroeconomía/us_pib_deflator.csv")
usd_deflator <- usd_deflator %>% rbind(c(year=2022,base_2015=113.06641))

## Mexico Imp - Exp
imports <- imp_exp_eeuu %>% filter(CTYNAME=="Mexico") %>% select(year,starts_with("I")) %>% pivot_longer(!year,names_to = "mes",values_to = "mdd") %>% 
  mutate(mes = case_when(mes =="IJAN" ~ "01",
                         mes =="IFEB" ~ "02",
                         mes =="IMAR" ~ "03",
                         mes =="IAPR" ~ "04",
                         mes =="IMAY" ~ "05",
                         mes =="IJUN" ~ "06",
                         mes =="IJUL" ~ "07",
                         mes =="IAUG" ~ "08",
                         mes =="ISEP" ~ "09",
                         mes =="IOCT" ~ "10",
                         mes =="INOV" ~ "11",
                         mes =="IDEC" ~ "12")) %>% mutate(fecha = as.Date(paste0(year,"-",mes,"-01"))) %>% mutate(clase = "Imports")
  

exports <- imp_exp_eeuu %>% filter(CTYNAME=="Mexico") %>% select(year,starts_with("E")) %>% pivot_longer(!year,names_to = "mes",values_to = "mdd") %>% 
  mutate(mes = case_when(mes =="EJAN" ~ "01",
                         mes =="EFEB" ~ "02",
                         mes =="EMAR" ~ "03",
                         mes =="EAPR" ~ "04",
                         mes =="EMAY" ~ "05",
                         mes =="EJUN" ~ "06",
                         mes =="EJUL" ~ "07",
                         mes =="EAUG" ~ "08",
                         mes =="ESEP" ~ "09",
                         mes =="EOCT" ~ "10",
                         mes =="ENOV" ~ "11",
                         mes =="EDEC" ~ "12")) %>% mutate(fecha = as.Date(paste0(year,"-",mes,"-01"))) %>% mutate(clase = "Exports")


trade_balance <- rbind(imports,exports) %>% left_join(usd_deflator,by="year") %>% mutate(deflated = mdd*(113.066/base_2015)) %>% filter(!is.na(mes)) %>% 
  filter(fecha<"2022-05-01")

trade_balance %>%   ggplot(aes(fecha,deflated,group=clase,color=clase))+
  geom_line(size=1)+
  ylim(c(0,3.5e+04))+
  labs(x="",y="Millones de Dólares",title="Intercambio comercial entre EEUU y México",subtitle = "Entre 1998 y Mayo 2022, en millones de dolares de 2021",
       caption = "Fuente: US - Census Bureau, World Bank",color="")+
  geom_smooth()+
  geom_label(data= labs,aes(label = comma(mdd,accuracy = 1,prefix = paste0(format(fecha,format="%Y-%m"),": "))),nudge_x = -0.8)+
  theme(plot.title = element_text(size=22,face="bold",color = "#9f2441"),plot.subtitle = element_text(size = 18,face="bold"),legend.position = "bottom")
  
labs <- trade_balance %>% filter(fecha %in% c(as.Date("1988-01-01"),as.Date("2000-01-01"),as.Date("1994-01-01"),
                                              as.Date("2006-01-01"),as.Date("2016-01-01"),as.Date("2018-01-01"),
                                              as.Date("2021-12-01"),as.Date("2022-03-01")))


trade_balance %>% View()

# 

imp_exp_eeuu %>% count(CTYNAME) %>% as.data.frame()


# Multiples países 
imp_exp_eeuu %>% filter(CTYNAME %in% c("Mexico","Turkey","European Union","Brazil","China","Russia","India","United Kingdom","Japan","Canada"),year==2021) %>% 
  select(!c(CTY_CODE,year)) %>% 
  pivot_longer(!CTYNAME,names_to = "mes",values_to = "mdd") %>% mutate(clase = ifelse(str_starts(mes,"I"),"Imports","Exports")) %>% 
  group_by(CTYNAME,clase) %>% summarise(mdd = sum(mdd)) %>% 
  ggplot(aes(reorder(CTYNAME,mdd),mdd,group=mdd,fill=clase))+
  geom_col(position = position_dodge(width = 1))+
  geom_label(aes(label=comma(mdd)), position = position_dodge(1))+
  labs(x="",y="MdD", title="Importaciones y Exportaciones a EEUU por País",subtitle = "En 2021,en millones de dólares",caption="Fuente: US Census",fill="")+
  theme(plot.title = element_text(size=22,face="bold",color = "#9f2441"),plot.subtitle = element_text(size = 18,face="bold"),legend.position = "bottom",
        axis.text.x = element_text(size=12,color="black"),legend.text = element_text(size=14,face="bold"))
  


#Lineas de Tiempo
country_selection_ts <- imp_exp_eeuu %>% filter(CTYNAME %in% c("Mexico","European Union","Brazil","China","Canada","India")) %>% 
  select(!c(CTY_CODE)) %>% 
  pivot_longer(!c(CTYNAME,year),names_to = "mes",values_to = "mdd") %>% mutate(clase = ifelse(str_starts(mes,"I"),"Imports","Exports")) %>% 
  mutate(mes = case_when(mes =="IJAN" ~ "01",
                         mes =="IFEB" ~ "02",
                         mes =="IMAR" ~ "03",
                         mes =="IAPR" ~ "04",
                         mes =="IMAY" ~ "05",
                         mes =="IJUN" ~ "06",
                         mes =="IJUL" ~ "07",
                         mes =="IAUG" ~ "08",
                         mes =="ISEP" ~ "09",
                         mes =="IOCT" ~ "10",
                         mes =="INOV" ~ "11",
                         mes =="IDEC" ~ "12",
                         mes =="EJAN" ~ "01",
                         mes =="EFEB" ~ "02",
                         mes =="EMAR" ~ "03",
                         mes =="EAPR" ~ "04",
                         mes =="EMAY" ~ "05",
                         mes =="EJUN" ~ "06",
                         mes =="EJUL" ~ "07",
                         mes =="EAUG" ~ "08",
                         mes =="ESEP" ~ "09",
                         mes =="EOCT" ~ "10",
                         mes =="ENOV" ~ "11",
                         mes =="EDEC" ~ "12")) %>% mutate(fecha = as.Date(paste0(year,"-",mes,"-01"))) %>% 
  left_join(usd_deflator,by="year") %>% mutate(deflated = mdd*(113.066/base_2015)) %>% filter(!is.na(mes)) %>% 
  filter(fecha<"2022-05-01")

# Find a better wat to communicate it
country_selection_ts %>%  ggplot(aes(fecha,deflated,group=interaction(CTYNAME,clase),color=clase))+
  labs(x="",y="MdD", title="Importaciones y Exportaciones a EEUU por País",subtitle = "Entre 1990 y Mayo 2022 2021,en millones de dólares de 2021",
       caption="Fuente: US Census",color="")+
  theme(plot.title = element_text(size=22,face="bold",color = "#9f2441"),plot.subtitle = element_text(size = 18,face="bold"),legend.position = "bottom",
        axis.text.x = element_text(size=12,color="black"),legend.text = element_text(size=14,face="bold"), strip.text.x = element_text(size = 12,face="bold"))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~CTYNAME)+
  geom_label(data=labs,aes(label = comma(mdd,accuracy = 1,prefix = paste0(format(fecha,format="%Y-%m"),": "))),nudge_x = -1000)

labs <- country_selection_ts %>% filter(fecha %in%c(as.Date("2022-04-01"),as.Date("2010-01-01"),as.Date("1994-12-01")))



































