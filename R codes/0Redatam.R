#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

rm(list = ls())
library(Rcpp)
library(RcppProgress)
library(redatam)
library(dplyr)
library(tidyverse)
library(haven)
library(DataExplorer)

peru <- redatam.open("R codes/cpv2017per-cde.dicx")

redatam.entities(peru)
redatam.variables(peru, "PROVINCI")
redatam.variables(peru, "PERSONA")

CONTEOS <- redatam.query(peru, "freq PROVINCI.REDCODEN
                                  by VIVIENDA.VAREA
                                  by PERSONA.C5P041
                                  by PERSONA.C5P02
                                  by PERSONA.ANEST
                                  by PERSONA.P09DISC
                                  by PERSONA.PBLOPER",
                         tot.omit = FALSE)
saveRDS(CONTEOS, file = "R codes/CONTEOS.RDS")
rm("$table1")
# CONTEOS <- readRDS(file = "R codes/CONTEOS.RDS")

# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>% filter_at(vars(matches("_label")),all_vars(. !=  "__tot__"))

## sumas por variables de agregación, coincidir con el total nacional.
map(grep(pattern = "_value", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })
map(grep(pattern = "_label", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })


CONTEOS2 %>% group_by(ANEST5_label, ANEST5_value) %>%
  summarise(n = sum(value))  %>%
  mutate(N = sum(n)) %>%
  data.frame()

CONTEOS2 %>% group_by(PBLOPER7_label, PBLOPER7_value) %>%
  summarise(n = sum(value))  %>%
  mutate(N = sum(n)) %>%
  data.frame()


censo_mrp <- CONTEOS2 %>%
  transmute(provi = str_pad(
              string = REDCODEN1_value,
              width = 4,
              pad = "0"
            ),
            area = case_when(VAREA2_value == 1 ~ "1", # 1 = Urbana
                             TRUE ~ "0"),    # 0 = Rural
            sexo = as.character(C5P024_value),

          edad = case_when(
              C5P0413_value  %in% 0:14 ~ "1", # 5 a 14
              C5P0413_value  %in% 15:29 ~ "2", # 15 a 29
              C5P0413_value  %in% 30:44 ~ "3", # 30 a 44
              C5P0413_value  %in% 45:64 ~ "4", # 45 a 64
              TRUE ~ "5"), # 65 o mas

          anoest = case_when(
            C5P0413_value < 4| is.na(ANEST5_value) ~ "98",     # No aplica
            ANEST5_value == 99 ~ "99", #NS/NR
            ANEST5_value %in% 0 ~ "1",  # Sin educacion
            ANEST5_value %in% c(1:6) ~ "2",  # 1-6
            ANEST5_value %in% c(7:11) ~ "3",  # 7-12 (caso particular  de perú)
            ANEST5_value > 11 ~ "4" ,  # 12 o mas
            TRUE ~ "Error"
          ),
          etnia = case_when(
            PBLOPER7_value == 1 ~ "1", # Indigena
            PBLOPER7_value == 2 ~ "2", # Afro
            TRUE ~ "3"), # Otro

          discapacidad = case_when(
            P09DISC6_value == 63 ~ "0", # No discapacitado
            TRUE ~ "1"), # Discapacitado
            value) %>%
  group_by(provi, area, sexo, edad, etnia, discapacidad, anoest) %>%
  summarise(n = sum(value), .groups = "drop")

# Suma del total nacional
sum(censo_mrp$n)

# agregados por nuevas variables
map(c(
  "provi",
  "area",
  "discapacidad",
  "sexo",
  "edad",
  "etnia",
  "anoest"
),
function(x) {
  censo_mrp %>% group_by_at(x) %>%
    summarise(n = sum(n)) %>%
    mutate(Prop = n / sum(n), N = sum(n))
})

plot_intro(censo_mrp)
plot_missing(censo_mrp)
plot_bar(censo_mrp, with = "n")

saveRDS(object = censo_mrp , file = "R codes/censo_provi.rds")
## Variables agregadas.
#prop.table(table(censo_mrp$depto,censo_mrp$etnia),margin = 1)

tasa_censo <- model.matrix(provi ~ -1 +.,
                           data = censo_mrp %>% select(-n)) %>% 
  data.frame() %>%
  mutate(provi = censo_mrp$provi, 
         n = censo_mrp$n) %>% 
  group_by(provi) %>%
  summarise_all(~weighted.mean(x = .,w = n)) %>%
  mutate(etnia1 = 1-etnia3-etnia2) %>% 
  select(-area0, -anoest98,-etnia3,-n) 

# Alcantarillado  -----------------------------------------------------------
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODEN
                          by VIVIENDA.C2P10",
                         tot.omit = FALSE)

ALCANTARILLADO <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__") ))

tasa_alcantarillado <- ALCANTARILLADO %>%
  mutate(Pobx = ifelse(!C2P102_value %in% c(1,2), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            tiene_alcantarillado = Pobx/PobT) 
# carencia de sanitario   -----------------------------------------------------------
# Energía eléctrica ----------------------------------------------------
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODEN
                          by VIVIENDA.C2P11",
                         tot.omit = FALSE)

ELECTRICIDAD_RED <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__") ))

tasa_electricidad <- ELECTRICIDAD_RED %>%
  mutate(Pobx = ifelse(!C2P112_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            tiene_electricidad = Pobx/PobT) 

# Agua cocinar ---------------------------------------------------------------
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODEN
                          by VIVIENDA.C2P06",
                         tot.omit = FALSE)

ACUEDUCTO <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__") ))

tasa_agua <- ACUEDUCTO %>%
  mutate(Pobx = ifelse(!C2P062_value %in% c(1,2), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            tiene_acueducto = Pobx/PobT) 
# Gas natural ----------------------------------------------------
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODEN
                          by HOGAR.C3P12",
                         tot.omit = FALSE)

GAS_RED <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__") ))

tasa_gas <- GAS_RED %>%
  mutate(Pobx = ifelse(!C3P122_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            tiene_gas = Pobx/PobT) 
# Eliminación de basura ----------------------------------------------------
# Tasa de acceso a Internet  ----------------------------------------------
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODEN
                          by HOGAR.C3P213",
                         tot.omit = FALSE)

INTERNET <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__") ))

tasa_internet <- INTERNET %>%
  mutate(Pobx = ifelse(!C3P2132_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            tiene_internet = Pobx/PobT) 
# Piso de tierra ----------------------------------------------------------
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODEN
                          by VIVIENDA.C2P05",
                         tot.omit = FALSE)

PISO <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__") ))

tasa_piso <- PISO %>%
  mutate(Pobx = ifelse(C2P052_value %in% c(6), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            piso_tierra = Pobx/PobT) 
# Material de paredes ----------------------------------------------------
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODEN
                          by VIVIENDA.C2P03",
                         tot.omit = FALSE)

PAREDES <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__") ))

tasa_paredes <- PAREDES %>%
  mutate(Pobx = ifelse(!C2P032_value %in% c(1,2), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            material_paredes = Pobx/PobT) 
# Material de techo ----------------------------------------------------
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODEN
                          by VIVIENDA.C2P04",
                         tot.omit = FALSE)

TECHO <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__") ))

tasa_techo <- TECHO %>%
  mutate(Pobx = ifelse(!C2P042_value %in% c(1:4), value, 0),
         PobT = value) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            material_techo = Pobx/PobT) 
# Tasa de personas con más 12 años de educación  y > 20 años. -------------
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODEN
                      by PERSONA.C5P041
                      by PERSONA.ANEST",
                         tot.omit = FALSE)
EDUCACION <- CONTEOS %>%
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__" ) ))

tasa_edu_sup <- EDUCACION %>%
  mutate(Pobx = ifelse(C5P0412_value > 20 & ANEST3_value > 12,
                       value, 0),
         PobT = ifelse(C5P0412_value > 20, value, 0)) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            rezago_escolar = Pobx/PobT)

# Tasa de personas analfabeta. ------------------------------------------
# Población de 15 años y más que no sabe leer y escribir dividido por la 
# población de 15 años y más, multiplicado por 100.
CONTEOS <- redatam.query(peru,
                         "freq PROVINCI.REDCODEN
                      by PERSONA.C5P041
                      by PERSONA.C5P12",
                         tot.omit = FALSE)
ALFABETA <- CONTEOS %>%
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__" , "__na__") ))

tasa_alfabeta  <- ALFABETA %>%
  mutate(Pobx = ifelse(C5P0412_value > 15 & C5P123_value == 2,
                       value, 0),
         PobT = ifelse(C5P0412_value > 15, value, 0)) %>%
  group_by(
    provi = str_pad(
      string = REDCODEN1_value,
      width = 4,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(provi,
            alfabeta = Pobx/PobT)


####################################################
OCUPACION <- redatam.query(peru, "freq PROVINCI.REDCODEN
                           by PERSONA.PET
                           ", tot.omit = FALSE)

OCUPACION2 <- OCUPACION %>%
  filter_at(vars(matches("_label")),
            all_vars(!. %in%   c(
              "__mv__", "__tot__", "No especificado", "__na__"
            )))

group_by(OCUPACION2, PET2_label, PET2_value) %>% summarise(n = sum(value))

sum(OCUPACION2$value)


OCUPACION2 <- OCUPACION2 %>%
  transmute(
    provi = str_pad(
      string = REDCODEN1_value,
      width = 4,
      pad = "0"
    ),
    ocupados = ifelse(PET2_value  %in% c(1), 1, 0),
    desocupados = ifelse(PET2_value  %in% c(2), 1, 0),
    value
  ) %>% group_by(provi, ocupados, desocupados) %>%
  summarise(value = sum(value), .groups = "drop")


tabla <-
  pivot_wider(
    OCUPACION2,
    names_from = c("ocupados", "desocupados"),
    values_from = value,
    names_prefix = c("ocupados")
  )

tasa_desocupacion <- tabla %>%
  transmute(provi,
            tasa_desocupacion = ocupados0_1 / sum(ocupados0_1 + ocupados1_0))

statelevel_predictors_df <- list(
  tasa_censo,
  #NBI
  # tasa_NBI_subsistencia,
  # tasa_sanitarias,
  # tasa_NBI_escolar,
  # tasa_hacinamiento,
  # tasa_vivienda,
  #NO NBI
  tasa_alcantarillado,
  # tasa_sanitario,
  tasa_electricidad,
  tasa_agua,
  tasa_gas,
  # tasa_basuras,
  tasa_internet,
  tasa_piso,
  tasa_paredes,
  tasa_techo,
  tasa_edu_sup,
  tasa_alfabeta,
  tasa_desocupacion
) %>%
  reduce(.f = inner_join)

saveRDS(statelevel_predictors_df, "R codes/statelevel_predictors_df.rds")

