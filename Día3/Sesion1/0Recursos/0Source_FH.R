################################################################################
## Title:        Función para el cálculo de estimadores directos empleando    ##
##               criterios de calidad                                         ##
## Returns:      Estimadores directos poor supresión de cifras, funciones     ## 
##               necesarias para ejecutar el modelo FH posteriormente         ## 
## Author:       Felipe Molina - Andrés Gutierrez - Diego Lemus               ##        
## Institution:  CEPAL                                                        ##
## División:     División de Estadísticas                                     ##
## Date:         2020 - 2021                                                  ##
## Disclaimer:   Estos códigos computacionales han sido programados con el    ##
##               fin de ejemplificar las metodologías propuestas por CEPAL.   ##
##               La responsabilidad del uso de los programas recae            ##
##               completamente sobre  los funcionarios a quienes se hace      ##
##               entrega. Se exime a la CEPAL de los errores que puedan ser   ##
##               ocasionados por el uso incorrecto de estos códigos.          ##
################################################################################

###--------------------------------------------------------------------------###
###    Función estimación directa con criterios de supresión se considera    ###
###                                fpc de UPMS                               ###
###--------------------------------------------------------------------------###

direct.supr = function(design.base,variable, group, upm, estrato) {
  
  library(rlang)
  library(tidyverse)
  library(dplyr)
  library(survey)
  library(srvyr)
  
  group = enquo(group)
  x = enquo(variable)
  upm = enquo(upm)
  estrato = enquo(estrato)
  estimacion = design.base %>% group_by(!!group) %>%
    summarise(n = unweighted(n()), 
              p = survey_mean(!!x,
              vartype = c("ci", "cv", "se"), 
              proportion = TRUE, na.rm = TRUE), 
              defff = survey_mean(!!x, na.rm = TRUE, deff = TRUE), 
              y = unweighted(sum(!!x, na.rm = T))) %>%
    mutate(LI = p_low, LS = p_upp, CV = 100 * p_cv, 
           deff = defff_deff,      # Deff
           n.eff = n/deff,         # Tamaño de muestra efectivo
           CVl = ifelse(p <= 0.5, 
                        100 * p_se/(-p * log(p)),
                        100 * p_se/(-(1 - p) * log(1 - p)))) %>%
    select(p, LI, LS, ee = p_se, CV, deff, n.eff, y, CVl) %>%
    as.data.frame()
  
  N <- sum(design.base$variables$wkx)
  n_s <- dim(design.base)[1]
  
  deff_tabla = design.base$variables %>% as.data.frame() %>% group_by(!!group) %>%
    summarise(Nd = sum(wkx, na.rm = T),
              vari = (1 - n_s/N) * (N^2/n_s) * (1/(N-1)) * sum(wkx*((!!x - weighted.mean(!!x,wkx, na.rm = T))/Nd)^2, na.rm = T)) %>% 
    select(vari) 
  
  Tabla = encuesta %>% group_by(!!group) %>%
    dplyr::summarise(n = n(), 
                     estratos = length(unique(!!estrato)),
                     UPMs = length(unique(!!upm)),
                     #upms_MM = unique(upms_MM),
                     grados = UPMs - estratos) %>%
    select(!!group, n, grados, UPMs)#, upms_MM)

###-------------------------- Criterios de calidad --------------------------###  
  
  Estimaciones <- cbind(Tabla, estimacion, deff_tabla) %>%
    transmute(!!group, p, LI, LS, ee, CV, deff =  ee^2/vari,
               n, n.eff = n/deff, grados, y, CVl, UPMs) %>%   #, upms_MM) %>%
    mutate(#fpc_upms = UPMs/upms_MM, 
           #fpc_upms = ifelse(is.na(fpc_upms), 0, fpc_upms), 
           Inclusion_GL = ifelse(grados >= 5, 1, 0), 
           
           # Inclusion_fpc = ifelse(grados >= 3 & fpc_upms >= 0.3 & fpc_upms != Inf, 1, 0),  
           
           Exclusion_n = ifelse(n < 50, 1, 0), 
           Exclusion_DEFF = ifelse(deff < 1, 1, 0), 
           Exclusion_GL = ifelse(grados <= 2, 1, 0), 
           Exclusion = ifelse((Exclusion_n == 1 | Exclusion_DEFF == 1 | 
                               Exclusion_GL == 1), 1, 0), # & Inclusion == 0, 1, 0), 
           
           # Inclusion = ifelse((Inclusion_GL == 1 | Inclusion_fpc == 1) & 
           
           # Inclusion = ifelse(Inclusion_GL == 1 & Exclusion == 0, 1, 0),
           
           # Flag = ifelse(Inclusion == 1 | grados >= 14, "Incluir"
           
           Flag = ifelse(Inclusion_GL == 1, "Incluir", # Cambiar grados por el mínimo de Las Condes, Vitacura, Ñuñoa, La Reina y Lo Barnechea
                         ifelse(Exclusion == 1, "Excluir", "Incluir")))
  
  # Revisar = Estimaciones %>% filter(Flag == "Revisar" | is.na(Flag)) %>%
  #           mutate(Flag = ifelse(ee > 0.1, "Incluir", "Incluir"))
  
  # Flag_final <- Estimaciones %>% filter(Flag != "Revisar") %>% 
  #               bind_rows(Revisar)
  
  return(Estimaciones)
}

###--------------------------------------------------------------------------###
###                 Función Intervalos de confianza logit                    ###
###--------------------------------------------------------------------------###

ICL <- function(p, mse, alpha = 0.05, student = FALSE, nu = NULL) {
  if (student == TRUE) {
    q <- qt(1 - alpha/2, nu)
  } else {
    q <- qnorm(1 - alpha/2)
  }
  CL <- log(p/(1 - p)) - (q * sqrt(mse))/(p * (1 - p))
  CU <- log(p/(1 - p)) + (q * sqrt(mse))/(p * (1 - p))
  IC_1 <- exp(CL)/(1 + exp(CL))
  IC_2 <- exp(CU)/(1 + exp(CU))
  
  return(data.frame(L.I = IC_1, L.S = IC_2))
}

###--------------------------------------------------------------------------###
###                   Función de recorte genérico de Potter                  ###
###--------------------------------------------------------------------------###

potter <- function(weights, ValorC) {
  
  n <- length(weights)
  K <- sqrt(ValorC * (sum(weights^2) / n))
  
  # Each weight in excess of Kn is given this value and the other weights are  #
  #               adjusted to reproduce the original weight sum                #
  
  weights_trunc <- ifelse(weights >= K, K, weights)
  weights_trunc <- weights_trunc * (sum(weights)  / sum(weights_trunc))
  
  return(weights_trunc)
}

###--------------------------------------------------------------------------###
###        Función de recorte de Potter con optimización no lineal           ###
###--------------------------------------------------------------------------###


optimizador <- function(ValorC, encuesta){
  encuesta$segmento <- encuesta$upm
  encuesta$poor <- encuesta$ISA
  # Factores de expansión truncados #
  
  encuesta$weights_trunc <- potter(encuesta$expc, ValorC)
  
  # Diseño de muestreo: Factores de expansión originales #
  
  diseno_weights <- encuesta %>% as_survey_design(ids = segmento, 
                    strat = estrato, weights = expc, nest = TRUE)
  
  # Diseño de muestreo: Factores de expansión recortados #
  
  diseno_weightsTrunc <- encuesta %>% as_survey_design(ids = segmento, 
                         strat = estrato, weights = weights_trunc, nest = TRUE)
  
  # Estimación: Factores de expansión originales #
  estima_weights <- diseno_weights %>%
    summarise(theta_weight = survey_mean(poor, vartype = "var", na.rm = TRUE))
  
  # Estimación: Factores de expansión recortados #
  
  estima_weightsTrunc <- diseno_weightsTrunc %>%
    summarise(theta_weightTrunc = survey_mean(poor, vartype = "var", 
                                              na.rm = TRUE))
  
  # Estimación ECM #
  
  MSE <- estima_weightsTrunc$theta_weightTrunc_var + 
    (estima_weights$theta_weight - estima_weightsTrunc$theta_weightTrunc)^2
  
  return(MSE)
}

