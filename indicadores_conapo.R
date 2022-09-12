# Proyecciones de Poblacion 
# CONAPO - 1950 - 2050
# daoa 2022


# Preparar Entorno ----
rm(list=ls())
dev.off()
pacman::p_load(tidyverse,scales,janitor,plotly)

#  Cargar Bases ----
conapo_2050 <- read_csv("input/CONAPO/pob_mit_proyecciones.csv",
                        locale = locale(encoding = "ISO-8859-2")) %>% clean_names()

# Entre 18 y 30 para mediados de 2024 ----

conapo_2050 %>% filter(ano == 2024, edad>=18, edad<=30, entidad == "República Mexicana") %>% select(entidad,edad,sexo,poblacion) %>% 
  pivot_wider(names_from = sexo,values_from = poblacion) %>% mutate(total = Hombres + Mujeres) %>% 
  summarise(sum(total))


conapo_2050 %>% filter(ano == 2024, edad>=18, edad<=30, entidad == "República Mexicana") %>% 
  ggplot(aes(factor(edad), poblacion,fill = sexo))+
  geom_col(position = "stack")+
  geom_label(aes(label = comma(poblacion)), position = position_stack())














































































