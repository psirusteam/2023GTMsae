############################################################
# Proyecto Unfpa Métodos de planificación Familiar         #
# Mapas de Indicadores de Planificación familiar           #
# Lectura y preparación de las bases de datos              #
# Autor: Stalyn Guerrero,Andrés Gutiérrez & Gabriel Nieto  #
############################################################

### Cleaning R environment ###
rm(list = ls())
library(Rcpp)
library(RcppProgress)
library(redatam)
library(dplyr)
library(tidyverse)
library(haven)
library(DataExplorer)
library(openxlsx)
## Diccionario traido directamente de los repositorios en Celade
CONTEOS <- readRDS("Data/UNFPA/D6/CONTEOS.RDS")

# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>% filter_at(vars(matches("_label")),all_vars(. !=  "__tot__"))

##### Creando Censo_mrp

censo_mrp <- CONTEOS2 %>%
  transmute(mpio = str_pad(
    string = REDCODEN1_value,
    width = 4,
    pad = "0"
  ),
  area = case_when(VAREA2_value == 1 ~ "1", # 1 = Urbana
                   TRUE ~ "0"),    # 0 = Rural
  sexo = as.character(C5P024_value),
  
  edad = case_when(
    C5P0413_value %in% 0:14 ~  "1",       # 5 a 14
    C5P0413_value %in% 15:20 ~ "2",      # 15 a 20
    C5P0413_value %in% 21:30 ~ "3",      # 21 a 30
    C5P0413_value %in% 31:39 ~ "4",      # 31 a 39
    C5P0413_value %in% 40:49 ~ "5",      # 40 a 49
    TRUE ~ "6"                     
  ),     
  
  anoest = case_when(
    EDUCA5_value == 98  ~ "98", # No aplica
    EDUCA5_value == 99  ~ "99", #NS/NR
    EDUCA5_value == 1   ~ "1",  # Sin educacion
    EDUCA5_value == 2   ~ "2",  # 1-6
    EDUCA5_value == 3   ~ "3",  # 7-12
    EDUCA5_value == 4   ~ "4" ,  # 12 o mas
    TRUE ~ "Error"
  ),
  
  etnia = case_when(
    PBLOPER8_value == 1 ~ "1", # Indigena
    PBLOPER8_value == 2 ~ "2", # Afro
    TRUE ~ "3"), # Otro
  
  discapacidad = case_when(
    P09DISC7_value == 63 ~ "0", # No discapacitado
    TRUE ~ "1"
    ), # Discapacitado
  
  unida = case_when(
    UnidasR6_value == 1 ~ "1",# Unida
    TRUE ~ "2" # Otro
  ),
  
  value) %>%
  group_by(mpio, area, sexo, edad, etnia, discapacidad, anoest, unida) %>%
  summarise(n = sum(value), .groups = "drop")

# Suma del total nacional
sum(censo_mrp$n)
saveRDS(censo_mrp, "Data/UNFPA/D6/censo_mrp_temp.rds")

