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
#memory.limit(500000)

library(tidyverse)
library(sampling)
library(DataExplorer)

####################################################
### Loading datasets: CASEN and Population census ###
####################################################
encuesta_mrp <- readRDS("Data/UNFPA/D6/encuesta_mrp.rds")
censo_mrp <- readRDS("Data/UNFPA/D6/censo_mrp_temp.rds")
##### Adecuando censo_mrp (eliminando las categorías que
#####                       no se tienen en cuenta en el análisis)
###### Eliminando sexo y edad

censo_mrp <- censo_mrp %>%
  filter(sexo != 1)

table(censo_mrp$edad, useNA = "a")

censo_mrp <- censo_mrp %>%
  filter(!edad %in% c(1, 6))

table(censo_mrp$sexo, useNA = "a")
table(censo_mrp$edad, useNA = "a")

##### Elimando categoria 98 No aplica

censo_mrp <- censo_mrp %>%
  filter(anoest != 98)

table(censo_mrp$anoest, useNA = "a")
###### Eliminando las categorías no comtempladas de edad

table(encuesta_mrp$anoest, useNA = "a")
table(encuesta_mrp$etnia, useNA = "a")
table(encuesta_mrp$edad, useNA = "a")
table(encuesta_mrp$area, useNA = "a")
table(encuesta_mrp$discapacidad, useNA = "a")
# Actualización de tabla censal- IPFP -------------------------------------

names_cov <- c("area","edad", "etnia")


# Matriz Calibrada creada únicamente para los niveles completos

# IMPORTANTE: Excluir las covariables que tengan niveles incompletos


##### Años de estudio

encuesta_mrp %>% group_by(anoest) %>%
  summarise(n = sum(fexp),
            n1 = n(),
            .groups = "drop") %>%
  mutate(prop = n1 / sum(n1))

censo_mrp %>% group_by(anoest) %>%
  summarise(n = sum(n),
            .groups = "drop") %>%
  mutate(prop = n / sum(n))


### Edad

encuesta_mrp %>% group_by(edad) %>%
  summarise(n = sum(fexp),
            n1 = n(),
            .groups = "drop") %>%
  mutate(prop = n1 / sum(n1))

censo_mrp %>% group_by(edad) %>%
  summarise(n = sum(n),
            .groups = "drop") %>%
  mutate(prop = n / sum(n))


### Area

encuesta_mrp %>% group_by(area) %>%
  summarise(n = sum(fexp),
            n1 = n(),
            .groups = "drop") %>%
  mutate(prop = n1 / sum(n1))

censo_mrp %>% group_by(area) %>%
  summarise(n = sum(n),
            .groups = "drop") %>%
  mutate(prop = n / sum(n))


### Etnia

encuesta_mrp %>% group_by(etnia) %>%
  summarise(n = sum(fexp),
            n1 = n(),
            .groups = "drop") %>%
  mutate(prop = n1 / sum(n1))

censo_mrp %>% group_by(etnia) %>%
  summarise(n = sum(n),
            .groups = "drop") %>%
  mutate(prop = n / sum(n))

###################################

auxSuma <- function(dat, col, ni) {
  dat %>% ungroup() %>% select(all_of(col))  %>%
    fastDummies::dummy_cols(remove_selected_columns = TRUE) %>%
    mutate_all( ~ . * ni) %>% colSums()
}

###### Verificando el n de cada una de las categorías
### Para el censo y la encuesta expandida

## encuesta
N.g <- map(names_cov,
           ~ auxSuma(encuesta_mrp, col = .x, ni = encuesta_mrp$fexp)) %>%
  unlist()

N.g

## censo

N_censo.g <- map(names_cov,
                 ~ auxSuma(censo_mrp, col = .x, ni = censo_mrp$n)) %>%
  unlist()

N_censo.g

#### verificando el nombre y orden de las variables comunes en los datos

names_xk <- intersect(names(N.g), names(N_censo.g))
names_xk

N.g <- N.g[names_xk]
N.g
N_censo.g <- N_censo.g[names_xk]
N_censo.g

### Dataframe con las variables comunes en el orden
var_com <- data.frame(N.g, N_censo.g)
var_com

#### Creando indicadoras para cada una de las variables-categorias
Xk <- censo_mrp %>% ungroup() %>% select(all_of(names_cov)) %>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE) %>%
  select(all_of(names_xk))
Xk

#### verificando que los valores sean igual a los originales
colSums(Xk * censo_mrp$n)


# Iniciando el proceso de calibración -------------------------------------

## el peso de las ponderaciones para actualizar o acercar los valores
##                          censales hacia los valores de la encuesta
##      (El más desactualizado hacia el actualizado)

gk <- calib(
  Xs = Xk,
  d = censo_mrp$n,
  total = N.g,
  method = "logit"
) # linear primera opcion

### verificando que el proceso se haya realizado y este convergiendo

checkcalibration(Xs = Xk,
                 d = censo_mrp$n,
                 total = N.g,
                 g = gk)

##### Validación visual

hist(gk)
summary(gk)
length(table(gk))


##### n1 con pesos actualizados
n1 <- ceiling(censo_mrp$n * gk)
summary(n1)
summary(censo_mrp$n)

##### verificación visual

plot(censo_mrp$n, n1)

#### Verificando que los valores sean cercanos
sum(round(censo_mrp$n))
sum(n1)
sum(encuesta_mrp$fexp)
censo_mrp$n <- n1

# saveRDS(censo_mrp, "Data/UNFPA/D6/censo_mrp.rds")

