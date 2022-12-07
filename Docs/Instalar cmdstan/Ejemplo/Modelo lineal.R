## Ejemplo para validar instalción de cmdstanr
#Libreria 
library(cmdstanr)
# Base de ejemplo
datalm <- readRDS(file = "datalm.rds")
# Precompilar código stan
fitLm1 <- cmdstan_model(stan_file = "7ModeloLm.stan") 
# Preparar insumos 
sample_data <- list(n = nrow(datalm),
                    x = datalm$material_paredes ,
                    y = datalm$Promedio)
# Ejecutar el modelo 
model_fitLm1 <- fitLm1$sample(data = sample_data, 
                              chains = 4,
                              parallel_chains = 4,
                              num_warmup = 1000,
                              num_samples = 1000,
                              seed = 1234,
                              refresh = 1000)
# Resultados de stan
model_fitLm1$summary(variables = c("b0","b1","sigma2","sigma")) 

