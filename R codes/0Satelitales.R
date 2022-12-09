#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###

rm(list = ls())

#################
### Libraries ###
#################
memory.limit(500000)

library(tidyverse)
library(sampling)
library(reticulate) # Conexión con Python
library(rgee) # Conexión con Google Earth Engine
library(sf) # Paquete para manejar datos geográficos
library(concaveman)
library(geojsonio)
library(magrittr)

####################################################
### Loading datasets: EH and Population census ###
####################################################
guatemala <- redatam.open("V:/DAT/SAECEPAL/MrPMunicipal/GTM/1.Ingreso/Data/cpv2018gtm-cde.dicx")
redatam.entities(guatemala)

redatam.variables(guatemala, "DEPTO")

mpio <- redatam.query(
  guatemala, "freq DEPTO.REDCODEN by
                   MUPIO.REDCODEN") %>% 
  transmute(dam = str_pad(REDCODEN1_value, width = 2,pad = "0"),
            dam2 = str_pad(REDCODEN2_value, width = 4,pad = "0"),
            dam3 = paste0(dam,str_pad(str_sub(dam2,3,4), width = 3,pad = "0")),
            nombre = toupper(REDCODEN2_label)
            )


statelevel_predictors_df <-
  readRDS("R codes/statelevel_predictors_df.rds") %>% 
  mutate(dam = str_sub(dam2,2,3),
         dam2 = str_pad(str_sub(dam2,4,5), width = 3,pad = "0"),
         dam3 = paste0(dam,dam2)
         )


#######################################
### configuración inicial de Python ###
#######################################

#rgee_environment_dir = "C://Users//agutierrez1//Anaconda3//envs//rgee_py//python.exe"
rgee_environment_dir = "C://Users//sguerrero//Anaconda3//envs//rgee_py//python.exe"
# Configurar python (Algunas veces no es detectado y se debe reiniciar R)
reticulate::use_python(rgee_environment_dir, required = T)
rgee::ee_install_set_pyenv(py_path = rgee_environment_dir, py_env = "rgee_py")
Sys.setenv(RETICULATE_PYTHON = rgee_environment_dir)
Sys.setenv(EARTHENGINE_PYTHON = rgee_environment_dir)
rgee::ee_Initialize(drive = T)

GTM <- read_sf("Shape/Guatemala_adm2_uscb_2020.shp") %>% 
  mutate(dam2 = gsub(pattern = "\\D", replacement = "",x = GEO_MATCH),
         dam = substr(dam2, 1,2),
         dam3 = paste0(dam,str_pad(str_sub(dam2,3,4), width = 3,pad = "0"))
         )

names(GTM)
nombre_shape <- GTM %>% data.frame() %>% select(dam,dam2,dam3,ADM2_NAME)
full_join(nombre_shape,mpio) %>% mutate(nombre == ADM2_NAME) %>% 
  full_join(statelevel_predictors_df %>% select(dam3,area1)) %>%
  view()
###################
### Luminosidad ###
###################
luces = ee$ImageCollection("NOAA/DMSP-OLS/NIGHTTIME_LIGHTS") %>%
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("stable_lights")) %>%
  ee$ImageCollection$toBands()



GTM_luces <- map(unique(GTM$dam),
                 ~tryCatch(ee_extract(
                   x = luces,
                   y = GTM[c("dam", "dam2","dam3")] %>% filter(dam == .x),
                   ee$Reducer$sum(),
                   sf = FALSE
                 ) ,  error = function(e)data.frame(dam2 = .x)))

GTM_luces %<>% bind_rows()

#################
### Urbanismo ###
#################

tiposuelo = ee$ImageCollection("COPERNICUS/Landcover/100m/Proba-V-C3/Global") %>%
  ee$ImageCollection$filterDate("2016-01-01", "2016-12-31") %>%
  ee$ImageCollection$map(function(x) x$select("urban-coverfraction", "crops-coverfraction")) %>% 
  ee$ImageCollection$toBands()


GTM_urbano_cultivo <-map(unique(GTM$dam),
                 ~tryCatch(ee_extract(
                   x = tiposuelo,
                   y = GTM[c("dam","dam2","dam3")] %>% filter(dam == .x),
                   ee$Reducer$sum(),
                   sf = FALSE
                 ) ,  error = function(e)data.frame(dam2 = .x)))

GTM_urbano_cultivo %<>% bind_rows()

#################
### Distancia a hospitales ###
#################

dist_salud = ee$Image('Oxford/MAP/accessibility_to_healthcare_2019') 

GTM_dist_salud <- map(unique(GTM$dam),
                      ~tryCatch(ee_extract(
                        x = dist_salud,
                        y = GTM[c("dam","dam2","dam3")] %>% filter(dam == .x),
                        ee$Reducer$sum(),
                        sf = FALSE
                      ) ,
                      error = function(e)data.frame(dam2 = .x)))

GTM_dist_salud %<>% bind_rows()

#################
# CSP gHM: Global Human Modification
#################

CSP_gHM = ee$ImageCollection('CSP/HM/GlobalHumanModification') 


GTM_CSP_gHM <-map(unique(GTM$dam),
                  ~tryCatch(ee_extract(
                    x = CSP_gHM,
                    y = GTM[c("dam","dam2","dam3")] %>% filter(dam == .x),
                    ee$Reducer$sum(),
                    sf = FALSE
                  ) ,
                  error = function(e)data.frame(dam2 = .x)))
GTM_CSP_gHM %<>% bind_rows()



###############
### Guardar ###
###############
statelevel_predictors_df <-
  full_join(GTM_luces, GTM_urbano_cultivo) %>%
  full_join(GTM_CSP_gHM) %>% full_join(GTM_dist_salud) %>% 
  inner_join(statelevel_predictors_df %>% select(-dam2)) %>%
  select(-dam2)

statelevel_predictors_df <- statelevel_predictors_df %>% rename(dam2 = dam3)

statelevel_predictors_df <- statelevel_predictors_df %>%
  rename( luces_nocturnas = F182013_stable_lights, 
          suelo_cultivos = X2016_crops.coverfraction,
          suelo_urbanos = X2016_urban.coverfraction,
          modifica_humana = X2016_gHM,
          tiempo_hospital = accessibility,
          tiempo_hospital_no_motor = accessibility_walking_only)


saveRDS(statelevel_predictors_df, "Data/statelevel_predictors_df_dam2.rds")
readRDS("Data/statelevel_predictors_df.rds") %>% names()
