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
statelevel_predictors_df <-
  readRDS("R codes/statelevel_predictors_df.rds")


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

###################################################
### Arreglar la shape PER                       ###
### Aplicar el CONVEX HULL a los multipolígonos ###
###################################################

## revisando PERentina
PER <- read_sf("Shape/PROVINCIAS.shp")
PER %<>% mutate(provi = str_pad(IDPROV, pad = "0", width = 4))

###################
### Luminosidad ###
###################

luces = ee$ImageCollection("NOAA/DMSP-OLS/NIGHTTIME_LIGHTS") %>%
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("stable_lights")) %>%
  ee$ImageCollection$toBands()

PER_luces <- map(unique(PER$provi),
                 ~tryCatch(ee_extract(
                   x = luces,
                   y = PER["provi"] %>% filter(provi == .x),
                   ee$Reducer$sum(),
                   sf = FALSE
                 ) %>% mutate(provi = .x),
                 error = function(e)data.frame(provi = .x)))

PER_luces %<>% bind_rows()
#################
### Urbanismo ###
#################

tiposuelo = ee$ImageCollection("COPERNICUS/Landcover/100m/Proba-V-C3/Global") %>%
  ee$ImageCollection$filterDate("2016-01-01", "2016-12-31") %>%
  ee$ImageCollection$map(function(x) x$select("urban-coverfraction", "crops-coverfraction")) %>% 
  ee$ImageCollection$toBands()


PER_urbano_cultivo <- map(unique(PER$provi),
                 ~tryCatch(ee_extract(
                   x = tiposuelo,
                   y = PER["provi"] %>% filter(provi == .x),
                   ee$Reducer$sum(),
                   sf = FALSE
                 ) %>% mutate(provi = .x), 
                 error = function(e)data.frame(provi = .x)))

PER_urbano_cultivo %<>% bind_rows() 

#################
### Distancia a hospitales ###
#################

dist_salud = ee$Image('Oxford/MAP/accessibility_to_healthcare_2019') 

PER_dist_salud <- map(unique(PER$provi),
                      ~tryCatch(ee_extract(
                        x = dist_salud,
                        y = PER["provi"] %>% filter(provi == .x),
                        ee$Reducer$sum(),
                        sf = FALSE
                      ) %>% mutate(provi = .x), 
                      error = function(e)data.frame(provi = .x)))

PER_dist_salud %<>% bind_rows() 

#################
# CSP gHM: Global Human Modification
#################

CSP_gHM = ee$ImageCollection('CSP/HM/GlobalHumanModification') 

PER_CSP_gHM <- map(unique(PER$provi),
                   ~tryCatch(ee_extract(
                     x = CSP_gHM,
                     y = PER["provi"] %>% filter(provi == .x),
                     ee$Reducer$sum(),
                     sf = FALSE
                   ) %>% mutate(provi = .x), 
                   error = function(e)data.frame(provi = .x)))

PER_CSP_gHM %<>% bind_rows() 


###############
### Guardar ###
###############

statelevel_predictors_df <-
  full_join(PER_luces, PER_urbano_cultivo) %>%
  full_join(PER_CSP_gHM) %>% full_join(PER_dist_salud) %>% 
  full_join(statelevel_predictors_df)


statelevel_predictors_df %>% filter_all(any_vars(is.na(.)))


saveRDS(statelevel_predictors_df, 
        "Data/statelevel_predictors_df_provi.rds")
