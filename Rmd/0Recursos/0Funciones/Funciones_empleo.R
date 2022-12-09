Indicadores_encuesta <- function(setdata, by = NULL) {
  ### Calculando indicadores
  # Población economicamente activa (PEA)
  # Población total Mayor igual a 15  (PT)
  empleo <- c("1" = "Ocupado",
              "3" = "Inactivo",
              "2" = "Desocupado")
  if (is.null(by)) {
    setdata <- setdata %>% mutate(Nacional = "Nacional")
    by = "Nacional"
  }
  
  # Tasa de desocupación (TD) = Desocupados / PEA ×100
  Ind_TD <- setdata %>% group_by_at(by) %>%
    summarise(n = n(),
      Desocupados = sum((empleo == 2) * fep),
              PEA =  sum((empleo != 3) * fep), .groups = "drop") %>%
    mutate(n_sample = n, TD = (Desocupados / PEA) * 100, 
           n = NULL, PEA = NULL)
  
  # Tasa de ocupación (TO) = Ocupados / Todos ×100
  Ind_TO <- setdata %>% group_by_at(by)  %>%
    summarise(Ocupados = sum((empleo == 1) * fep),
                             TODOS = sum(fep),
              .groups = "drop") %>%
    mutate(TO = (Ocupados / TODOS) * 100, TODOS = NULL)
  
  # Tasa de participación  (TP) = PEA / Todos ×100
  Ind_TP <- setdata %>% group_by_at(by)  %>%
    summarise(PEA =  sum((empleo != 3) * fep),
                             TODOS = sum(fep),
              .groups = "drop") %>%
    mutate(TP = (PEA / TODOS) * 100, TODOS = NULL, PEA = NULL)

  Ind <- full_join(Ind_TD,Ind_TO, by = by) %>% 
    select(-Desocupados,-Ocupados) %>%
    full_join(Ind_TP, by = by)
  
  # # Tasa de participación (TP)  (PEA / PT)*100.
  # PT = sum(censo_mrp$n)
  # encuesta_mrp %>% summarise(TP = sum(fep) / PT * 100)
  return(Ind)
}
Indicadores_censo <- function(setdata, by = NULL) {
  ### Calculando indicadores
  # Población economicamente activa (PEA)
  # Población total Mayor igual a 15  (PT)
  empleo <- c("theta_1" = "Ocupado",
              "theta_3" = "Inactivo",
              "theta_2" = "Desocupado")
  if (is.null(by)) {
    setdata <- setdata %>% mutate(Nacional = "Nacional")
    by = "Nacional"
  }
  
  # Tasa de desocupación (TD) = Desocupados / PEA ×100
  Ind_TD <- setdata %>% group_by_at(by) %>%
    summarise(Desocupados = sum(n*theta_2),
              PEA =  sum(n*(theta_2 + theta_1)), .groups = "drop") %>%
    mutate(TD = (Desocupados / PEA) * 100, PEA = NULL)
  
  # Tasa de ocupación (TO) = Ocupados / PEA ×100
  Ind_TO <- setdata %>% group_by_at(by)  %>%
    summarise(Ocupados = sum(n*theta_1),
              TODOS = sum(n), .groups = "drop") %>%
    mutate(TO = (Ocupados / TODOS) * 100, TODOS = NULL)
  
# Tasa de participación  (TP) = PEA / Todos ×100
  Ind_TP <- setdata %>% group_by_at(by)  %>%
    summarise(PEA =  sum(n*(theta_2 + theta_1)),
                             TODOS = sum(n),
              .groups = "drop") %>%
    mutate(TP = (PEA / TODOS) * 100, TODOS = NULL, PEA = NULL)


  Ind <- full_join(Ind_TD,Ind_TO, by = by) %>% 
    select(-Desocupados,-Ocupados) %>%
    full_join(Ind_TP, by = by)
  
  # # Tasa de participación (TP)  (PEA / PT)*100.
  # PT = sum(censo_mrp$n)
  # encuesta_mrp %>% summarise(TP = sum(fep) / PT * 100)
  return(Ind)
}

## Plot de comparación empleando los pesos de muestreo

plot_compare_Ind <- function(sample, poststrat, by1, Ind = "TD"){
  
  if (all(by1 %in% names(sample))) {
    dat_encuesta <- Indicadores_encuesta(setdata = sample, by1)
  } else{
    dat_encuesta <- poststrat %>% distinct_at(vars(by1))
    dat_encuesta %<>% mutate(
       n_sample = 1,
       
      )
    
  }

  dat_censo <- bind_rows(list(
    directo = dat_encuesta %>% select(-n_sample),
    lmer = Indicadores_censo(setdata = poststrat, by1) ,
    bench = Indicadores_censo(setdata = poststrat %>%
                                mutate(n = n * gk), by1)
  ),
  .id = "variable")
  
 
  dat_plot <- full_join(dat_encuesta %>% select(by1,n_sample),
                        dat_censo, by = by1)
  dat_plot[["value"]] <- dat_plot[[Ind]] 
  if(length(by1)== 2) {
    x <-
      apply(poststrat[, by1], MARGIN = 2, function(x)
        length(unique(x))) %>% sort(decreasing = TRUE) %>% names()
    
    xmax <- x[1]
    dat_plot[["x"]] <- dat_plot[[xmax]]
    xmin <- x[2]
    dat_plot[["facet"]] <- dat_plot[[xmin]]
  } else if (length(by1) == 1) {
    xmax = by1
    dat_plot[["x"]] <- dat_plot[[by1]]
  }else if(length(by1)== 3) {
    x <-
      apply(poststrat[, by1], MARGIN = 2, function(x)
        length(unique(x))) %>% sort(decreasing = TRUE) %>% names()
    xmax <- x[1] 
    dat_plot[["x"]] <- dat_plot[[xmax]]
    xcol <- x[2]
    dat_plot[["xcol"]] <- dat_plot[[xcol]]
    xfila <- x[3]
    dat_plot[["xfila"]] <- dat_plot[[xfila]]
  } 
  
  plot1 <- ggplot(data = dat_plot) +
    geom_jitter(aes(
      x = fct_reorder2(x, x, n_sample),
      y = value,
      color = variable
    ), size = 2.5,width = 0.3) +
    scale_color_manual(
      breaks = c("directo", "lmer", "bench"),
      values = c("red", "blue", "green")
    ) +
    theme_bw(20) + labs(x = xmax, y = "", color = "") +
    theme(
      legend.position = "bottom",
      axis.title = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(
        angle = 90,
        size = 8,
        vjust = 0.3
      ),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 15)
    )
  
  if(length(by1) == 2){
    plot1 <- plot1 + facet_wrap(vars(facet), ncol = 2)
  } else if(length(by1) == 3){
    plot1 <- plot1 + facet_grid(xcol~xfila)  
  }
  return(list(tabla = dat_plot,  Plot = plot1))
}
