#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###

rm(list =ls())
cat("\f")

library(Rcpp)
library(RcppProgress)
library(redatam)
library(dplyr)
library(tidyverse)
library(haven)
library(DataExplorer)

## leer base desde el repositorio CEPAL
guatemala <- redatam.open("V:/DAT/SAECEPAL/MrPMunicipal/GTM/1.Ingreso/Data/cpv2018gtm-cde.dicx")
CONTEOS <- redatam.query(
  guatemala, "freq MUPIO.MPIO
             by PERSONA.PCP6", tot.omit = FALSE)


temp <- CONTEOS %>% filter(PCP62_mask == 1, MPIO1_mask != 1) %>% 
  transmute(paso = str_pad(MPIO1_value, width = 4, pad = "0"),
            dam = str_sub(paso,1,2),
            paso = str_pad(str_sub(paso,3,4),  width = 3, pad = "0"),
            dam2 = paste0(dam,paso),
            paso = NULL, 
            total_pp = value) 
saveRDS(object = temp, "Data/total_personas_dam.rds")

sum(temp$total_pp)

CONTEOS <- redatam.query(
  guatemala, "freq MUPIO.MPIO
             by VIVIENDA.PLG11
             by  PERSONA.PCP7
             by PERSONA.PCP6
             by PERSONA.ANEDUCA
             by PERSONA.PBLOPER
  ",  tot.omit = FALSE
)
saveRDS(CONTEOS, "R codes/CONTEOS.RDS")
rm("$table1")
# CONTEOS <- readRDS(file =  "GTM/2021/1.Ingreso/Data/CONTEOS.RDS")
#   revisando valores unicos.
# Eliminando totales de la tabla
CONTEOS2 <-
  CONTEOS %>% filter_at(vars(matches("_label")), all_vars(. !=  "__tot__"))


censo_mrp <- CONTEOS2 %>% transmute(
  dam2 = str_pad(
    string = MPIO1_value,
    width = 5,
    pad = "0"
  ),
  area = case_when(PLG112_value == 1 ~ "1", # 1 = Urbana
                   TRUE ~ "0"),
  # 0 = Rural
  sexo = as.character(PCP64_value),

  edad = case_when(
    PCP73_value %in% 0:14 ~ "1",       # 5 a 14
    PCP73_value %in% 15:29 ~ "2",      # 15 a 29
    PCP73_value %in% 30:44 ~ "3",      # 30 a 44
    PCP73_value %in% 45:64 ~ "4",      # 45 a 64
    TRUE ~ "5"
  ),     # 65 o mas

  anoest = case_when(

    is.na(ANEDUCA5_value) | PCP73_value < 7 ~ "98",     # No aplica
    ANEDUCA5_value == 99 ~ "99", #NS/NR
    ANEDUCA5_value %in% 0 ~ "1",  # Sin educacion
    ANEDUCA5_value %in% c(1:6) ~ "2",  # 1-6
    ANEDUCA5_value %in% c(7:12) ~ "3",  # 7-12
    ANEDUCA5_value > 12 ~ "4" ,  # 12 o mas
    TRUE ~ "Error"
  ),    
  etnia = case_when(
    PBLOPER6_value %in% 2:3 ~ "2",    # Afro
    PBLOPER6_value == 1  ~ "1", # Indigena,
    TRUE ~ "3" # Otro
  ),
  value
) %>% group_by(dam2, area, etnia, sexo, edad, anoest) %>%
  summarise(n = sum(value), .groups = "drop")

# Suma del total nacional
sum(censo_mrp$n)

saveRDS(censo_mrp, "Data/censo_dam2.rds")

## Variables agregadas.
#prop.table(table(censo_mrp$dam2,censo_mrp$etnia),margin = 1)

tasa_censo <- model.matrix(dam2 ~ -1 +.,
                           data = censo_mrp %>% select(-n)) %>% 
  data.frame() %>%
  mutate(dam2 = censo_mrp$dam2, 
         n = censo_mrp$n) %>% 
  group_by(dam2) %>%
  summarise_all(~weighted.mean(x = .,w = n)) %>%
  mutate(etnia1 = 1-etnia3-etnia2) %>% 
  select(-area0, -anoest98,-etnia3,-n) 

# Alcantarillado  -----------------------------------------------------------
CONTEOS <- redatam.query(guatemala,
                         "freq MUPIO.MPIO
                          by HOGAR.PCH5",
                         tot.omit = FALSE)

ALCANTARILLADO <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__") ))

tasa_alcantarillado <- ALCANTARILLADO %>%
  mutate(Pobx = ifelse(!PCH52_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    dam2 = str_pad(
      string = MPIO1_value,
      width = 5,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(dam2,
            tiene_alcantarillado = Pobx/PobT)
# carencia de sanitario   -----------------------------------------------------------
# Energía eléctrica ----------------------------------------------------
CONTEOS <- redatam.query(guatemala,
                         "freq MUPIO.MPIO
                          by HOGAR.PCH8",
                         tot.omit = FALSE)

ELECTRICIDAD_RED <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__") ))

tasa_electricidad <- ELECTRICIDAD_RED %>%
  mutate(Pobx = ifelse(!PCH82_value %in% c(1,2), value, 0),
         PobT = value) %>%
  group_by(
    dam2 = str_pad(
      string = MPIO1_value,
      width = 5,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(dam2,
            tiene_electricidad = Pobx/PobT)
# Agua cocinar ---------------------------------------------------------------
CONTEOS <- redatam.query(guatemala,
                         "freq MUPIO.MPIO
                          by HOGAR.PCH4",
                         tot.omit = FALSE)

ACUEDUCTO <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__") ))

tasa_agua <- ACUEDUCTO %>%
  mutate(Pobx = ifelse(!PCH42_value %in% c(1,2), value, 0),
         PobT = value) %>%
  group_by(
    dam2 = str_pad(
      string = MPIO1_value,
      width = 5,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(dam2,
            tiene_acueducto = Pobx/PobT)
# Gas natural ----------------------------------------------------
CONTEOS <- redatam.query(guatemala,
                         "freq MUPIO.MPIO
                          by HOGAR.PCH14",
                         tot.omit = FALSE)

GAS_RED <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__") ))

tasa_gas <- GAS_RED %>%
  mutate(Pobx = ifelse(!PCH142_value %in% c(1,5), value, 0),
         PobT = value) %>%
  group_by(
    dam2 = str_pad(
      string = MPIO1_value,
      width = 5,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(dam2,
            tiene_gas = Pobx/PobT)
# Eliminación de basura ----------------------------------------------------
CONTEOS <- redatam.query(guatemala,
                         "freq MUPIO.MPIO
                          by HOGAR.PCH10",
                         tot.omit = FALSE)

RECOLECTOR_BASURAS <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__") ))

tasa_basuras <- RECOLECTOR_BASURAS %>%
  mutate(Pobx = ifelse(!PCH102_value %in% c(1,2), value, 0),
         PobT = value) %>%
  group_by(
    dam2 = str_pad(
      string = MPIO1_value,
      width = 5,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(dam2,
            eliminar_basura = Pobx/PobT)
# Tasa de acceso a Internet  ----------------------------------------------
CONTEOS <- redatam.query(guatemala,
                         "freq MUPIO.MPIO
                          by HOGAR.PCH9_I",
                         tot.omit = FALSE)

INTERNET <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__") ))

tasa_internet <- INTERNET %>%
  mutate(Pobx = ifelse(!PCH9_I2_value %in% c(1), value, 0),
         PobT = value) %>%
  group_by(
    dam2 = str_pad(
      string = MPIO1_value,
      width = 5,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(dam2,
            tiene_internet = Pobx/PobT)
# Piso de tierra ----------------------------------------------------------
CONTEOS <- redatam.query(guatemala,
                         "freq MUPIO.MPIO
                          by VIVIENDA.PCV5",
                         tot.omit = FALSE)

PISO <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__") ))

tasa_piso <- PISO %>%
  mutate(Pobx = ifelse(PCV52_value %in% c(7), value, 0),
         PobT = value) %>%
  group_by(
    dam2 = str_pad(
      string = MPIO1_value,
      width = 5,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(dam2,
            piso_tierra = Pobx/PobT)
# Material de paredes ----------------------------------------------------
CONTEOS <- redatam.query(guatemala,
                         "freq MUPIO.MPIO
                          by VIVIENDA.PCV2",
                         tot.omit = FALSE)

PAREDES <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__") ))

tasa_paredes <- PAREDES %>%
  mutate(Pobx = ifelse(!PCV22_value %in% c(1:5), value, 0),
         PobT = value) %>%
  group_by(
    dam2 = str_pad(
      string = MPIO1_value,
      width = 5,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(dam2,
            material_paredes = Pobx/PobT)
# Material de techo ----------------------------------------------------
CONTEOS <- redatam.query(guatemala,
                         "freq MUPIO.MPIO
                          by VIVIENDA.PCV3",
                         tot.omit = FALSE)

TECHO <- CONTEOS %>% 
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__") ))

tasa_techo <- TECHO %>%
  mutate(Pobx = ifelse(!PCV32_value %in% c(1:4), value, 0),
         PobT = value) %>%
  group_by(
    dam2 = str_pad(
      string = MPIO1_value,
      width = 5,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(dam2,
            material_techo = Pobx/PobT)
# Tasa de personas con más 12 años de educación  y > 20 años. -------------

CONTEOS <- redatam.query(guatemala,
                         "freq MUPIO.MPIO
                      by PERSONA.PCP7
                      by PERSONA.ANEDUCA",
                         tot.omit = FALSE)
EDUCACION <- CONTEOS %>%
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__" ) ))

tasa_edu_sup <- EDUCACION %>%
  mutate(Pobx = ifelse(PCP72_value > 20 & ANEDUCA3_value > 12,
                       value, 0),
         PobT = ifelse(PCP72_value > 20, value, 0)) %>%
  group_by(
    dam2 = str_pad(
      string = MPIO1_value,
      width = 5,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(dam2,
            rezago_escolar = Pobx/PobT)
# Tasa de personas analfabeta. ------------------------------------------
# Población de 15 años y más que no sabe leer y escribir dividido por la 
# población de 15 años y más, multiplicado por 100.
CONTEOS <- redatam.query(guatemala,
                         "freq MUPIO.MPIO
                      by PERSONA.PCP7
                      by PERSONA.PCP22",
                         tot.omit = FALSE)
ALFABETA <- CONTEOS %>%
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__" ) ))

tasa_alfabeta <- ALFABETA %>%
  mutate(Pobx = ifelse(PCP72_value > 15 & PCP223_value == 2,
                       value, 0),
         PobT = ifelse(PCP72_value > 15, value, 0)) %>%
  group_by(
    dam2 = str_pad(
      string = MPIO1_value,
      width = 5,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(dam2,
            alfabeta = Pobx/PobT)
# Hacinamiento ----------------------------------------------------
CONTEOS <- redatam.query(guatemala,
                         "freq MUPIO.MPIO
                      by HOGAR.PCH12
                      by HOGAR.TOTAL_PERS",
                         tot.omit = FALSE)
HACINAMIENTO <- CONTEOS %>%
  filter_at(vars(matches("_label")),
            all_vars(!. %in%  c("__tot__","__mv__","__na__" ) ))

tasa_hacinamiento <- HACINAMIENTO %>%
  mutate(Pobx = ifelse(TOTAL_PERS3_value/PCH122_value > 2,
                       value, 0),
         PobT = value) %>%
  group_by(
    dam2 = str_pad(
      string = MPIO1_value,
      width = 5,
      pad = "0"
    )
  ) %>%
  summarise(PobT = sum(PobT),
            Pobx = sum(Pobx)) %>% 
  transmute(dam2,
            hacinamiento = Pobx/PobT)
## tasa de desocupación
OCUPACION <-
  redatam.query(guatemala, "freq  MUPIO.MPIO by  PERSONA.PET",
                tot.omit = FALSE)
OCUPACION2 <- OCUPACION %>%
  filter(!PET2_label %in% c("__tot__", "No especificado", "__na__"))

group_by(OCUPACION2, PET2_value, PET2_label) %>% summarise(n = sum(value))

sum(OCUPACION2$value)


OCUPACION2 <- OCUPACION2 %>%
  transmute(
    dam2 = str_pad(
      string = MPIO1_value,
      width = 5,
      pad = "0"
    ),
    ocupados = ifelse(PET2_value  %in% c(1), 1, 0),
    desocupados = ifelse(PET2_value  %in% c(2), 1, 0),
    value
  ) %>% group_by(dam2, ocupados, desocupados) %>%
  summarise(value = sum(value))


tabla <-
  pivot_wider(
    OCUPACION2,
    names_from = c("ocupados", "desocupados"),
    values_from = value,
    names_prefix = c("ocupados")
  )


tasa_desocupacion <- tabla %>%
  transmute(dam2,
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
  tasa_basuras,
  tasa_internet,
  tasa_piso,
  tasa_paredes,
  tasa_techo,
  tasa_edu_sup,
  tasa_alfabeta,
  tasa_hacinamiento,
  tasa_desocupacion
) %>%
  reduce(.f = inner_join)

saveRDS(statelevel_predictors_df, "R codes/statelevel_predictors_df.rds")


