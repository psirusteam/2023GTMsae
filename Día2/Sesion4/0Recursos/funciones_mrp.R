library(tidyr)
library(purrr)
library(patchwork)
library(magrittr)
## plot para validar interacciones

plot_interaction <- function(dat_encuesta, by, by2 ){

  list_par <- list(
    escolar = c(
      "1" = "Sin educacion",
      "2" = "Primaria",
      "3" = "Secundaria",
      "4" = "Superior"
    ),
    anoest = c(
      "1" = "Sin educacion",
      "2" = "1 - 6 años",
      "3" = "7 - 12 años",
      "4" = "Más de 12",
      "98" = "No aplica",
      "99" = "NS/NR"
    ),
    sexo = c("1" = "Hombre", "2" = "Mujer"),
    etnia = c(
      "1" = "Indigena",
      "2" = "Afro",
      "3" = "Otro"
      ),
    edad =
      c(
        "1" = "0 -14",
        "2" = "15 - 29",
        "3" = "30 - 44",
        "4" = "45 - 64",
        "5" = "65 - más"
      ),
    discapacidad = c("0" = "No discapacitado", "1" = "Discapacitado"),
    area = c("0" = "Rural", "1" = "Urbana")
  )

  dat_inter <- dat_encuesta %>% group_by_at(vars(by,by2)) %>%
    summarise(Pobreza = mean(pobreza), .groups = "drop")


  by_labels <- list_par[[by]]
  by_labels2 <- list_par[[by2]]

  if (!"depto" %in% c(by, by2)) {
    if (is.null(by_labels) | is.null(by_labels2)) {
      stop(paste0(
        "La(s) variable(s) _",
        by,
        "_ , _",
        by2,
        "_ no esta(n) en la lista de parametros"
      ))
    }

    dat_inter[["by_Color"]] <-
      dplyr::recode(as.character(dat_inter[[by]]) ,!!!by_labels)


    dat_inter[["by_x"]] <-
      dplyr::recode(as.character(dat_inter[[by2]]) ,!!!by_labels2)
  } else{
    if (by2 == "depto") {
      dat_inter[["by_x"]] <- dat_inter[["depto"]]
      dat_inter[["by_Color"]] <-
        dplyr::recode(as.character(dat_inter[[by]]) ,!!!by_labels)
    } else{
      dat_inter[["by_Color"]] <- dat_inter[["depto"]]
      dat_inter[["by_x"]] <-
        dplyr::recode(as.character(dat_inter[[by2]]) ,!!!by_labels2)
    }
  }

  p1 <- dat_inter %>% ggplot(aes(x = by_x, y = Pobreza, col = by_Color)) +
    geom_point() + geom_line(aes(group = by_Color)) +
    ggtitle(paste0(by, " x ", by2)) + labs(x = by2) +
    theme(legend.position = "none")

  return(p1)

}

## plot para comparar las proporciones del censo con la encuesta

Plot_Compare <- function(dat_censo, dat_encuesta, by){

  list_par <- list(
    escolar2 = c(
      "1" = "Sin educacion",
      "2" = "Primaria",
      "3" = "Secundaria",
      "4" = "Superior"
    ),
    anoest = c(
      "1" = "Sin educacion",
      "2" = "1 - 6 años",
      "3" = "7 - 12 años",
      "4" = "Más de 12",
      "98" = "No aplica",
      "99" = "NS/NR"
    ),
    sexo = c("1" = "Hombre", "2" = "Mujer"),
    etnia = c(
      "1" = "Indigena",
      "2" = "Afro",
      "3" = "Otro"
    ),
    edad =
      c(
        "1" = "0 -14",
        "2" = "15 - 29",
        "3" = "30 - 44",
        "4" = "45 - 64",
        "5" = "65 - más"
      ),
    discapacidad = c("0" = "No discapacitado", "1" = "Discapacitado"),
    area = c("0" = "Rural", "1" = "Urbana")
  )

    dat_sample <- dat_encuesta %>% group_by_at(by) %>% summarise(n = n()) %>%
      mutate(Sample = n/sum(n), n = NULL)

  dat_post <- dat_censo %>% group_by_at(by) %>% summarise(n_post = sum(n)) %>%
    mutate(Population = n_post/sum(n_post), n_post = NULL)

  dat_plot <- full_join(dat_sample, dat_post, by = by)

  if(by != "depto") {
    by_labels <- list_par[[by]]

    if (is.null(by_labels)) {
      warning(paste0("La variable _",
                  by, "_ no esta en la lista de parametros"))

      dat_plot[["x"]] <- dat_plot[[by]]
      dat_plot[[by]] <- NULL

      }else{

    dat_plot[["x"]] <-
      dplyr::recode(as.character(dat_plot[[by]]) , !!!by_labels)
    dat_plot[[by]] <- NULL
    }

  } else{
    dat_plot[["x"]] <- dat_plot[[by]]
    dat_plot[[by]] <- NULL
  }
  #--- Plot ---#

  gg_plot <- ggplot() +  ylab("") + xlab("Proporción en muestra") +
    coord_flip() + geom_dumbbell(data = dat_plot, aes(y =x, x = Sample,
                                                      xend = Population)) +
    geom_point(data = melt(dat_plot, id = "x"),
               aes(y = x, x = value, color = variable), size = 3) +
   # scale_x_continuous(limits = c(0.08, 0.28), breaks = c(0, .1, .15, .2, .25)) +
    theme(legend.position = "none") + ggtitle(by)

  dat_plot[[by]] <-  dat_plot[["x"]]
  dat_plot[["x"]] <- NULL

  print(dat_plot %>% select(by, Sample, Population))
  return(gg_plot)
}


Aux_Agregado <-
  function(poststrat,
    epredmat,
    byMap = c("depto", "etnia", "sexo"),
    ponde_Benchmarking = NULL) {

    # poststrat: Información obtenida del censo.
    # epredmat: resultados obtenidos de la distribución posterior.
    # byMap: Variables por las cuales desea obtener los agregados

  ## Creación de variable para el calculo nacional
  if(is.null(byMap)){
    poststrat  %<>% mutate(Nacional = "Nacional")
    byMap <- "Nacional"
  }
  ## Creación de indicadora poscición
    poststrat2 <- poststrat %>% ungroup() %>%
      mutate(Posi = 1:n()) %>%
      group_by_at(byMap) %>% group_nest()

     ## Creación de alertas por eliminar las categosrías de anoest

if(any(byMap == "anoest")){
  poststrat2 %<>% filter(!anoest %in% c("99", "98"))
  cat("
     ############################# NOTA #################################
     # En las tabla de escolaridad (anoest) se eliminan los conteos de  #
     # NA y NS/NR                                                       #
     ############################# NOTA #################################
      ")
  if(!is.null(ponde_Benchmarking)){
  cat("
     ####################### IMPORTANTE #################################
     # Para todas las tablas con escolaridad la estimación no es exacta #
     # dado que se omiten los conteos con NA y NS/NR, en caso de ser    #
     # incluidos la estimación es exacta.                               #
     ###################### IMPORTANTE ##################################
     \n")}
}

    ## Estimado los mrp

    Estimado_mrp <- poststrat2 %>%
      mutate(Estimado_mrp =
               map(data,
                   function(subgrupo) {
                     filtering_condition <- subgrupo$Posi
                     n_filtered <- subgrupo$n
                     epred_mat_filtered <-
                       epredmat[, filtering_condition]

                     if (length(n_filtered) > 1) {
                       mrp_estimates <-
                         epred_mat_filtered %*% n_filtered / sum(n_filtered)

                     } else{
                       mrp_estimates <- as.numeric(epred_mat_filtered)
                     }

                     data.frame(
                       mrp_estimate = mean(mrp_estimates),
                       mrp_estimate_se = sd(mrp_estimates)
                     )
                   }), data = NULL) %>% unnest("Estimado_mrp")

    ## calculo de Benchmarking, se repite el proceso anterior
    # incluyendo un ponderador (ponde_Benchmarking) obtenido previamente

    if(!is.null(ponde_Benchmarking)) {
      ponde <- ponde_Benchmarking
      Estimado_Benchmarking <- poststrat2 %>%
        mutate(Estimado_Benchmarking =
                 map(data,
                     function(subgrupo) {
                       filtering_condition <- subgrupo$Posi
                       n_filtered <- subgrupo$n
                       epred_mat_filtered <-
                         epredmat[, filtering_condition]
                       if (length(n_filtered) > 1) {
                         mrp_estimates <-
                           epred_mat_filtered %*% (n_filtered * ponde) / sum(n_filtered)

                       } else{
                         mrp_estimates <- as.numeric(epred_mat_filtered * ponde)
                       }

                       data.frame(Benchmarking_estimate = mean(mrp_estimates),
                                  N = sum(n_filtered))
                     }),
               data = NULL) %>% unnest("Estimado_Benchmarking")
      Estimado_mrp <-
        full_join(Estimado_mrp, Estimado_Benchmarking, by = byMap)

      ## Creación del gráfico comparativo entre MRP y Benchmarking
      # Obtener limites para el gráfico.
      ls <-
        max(c(
          Estimado_mrp$mrp_estimate,
          Estimado_mrp$Benchmarking_estimate
        )) + 0.5

      li <-
        min(c(
          0,
          Estimado_mrp$mrp_estimate,
          Estimado_mrp$Benchmarking_estimate
        ))
      # Validación de valores negativos en las estimaciones
      if(li < 0){stop("Estimaciones negativas en mrp o Benchmarking")}

    ## Creación del gráfico.
     g <- ggplot(data = Estimado_mrp,
                 aes(x = mrp_estimate, y =  Benchmarking_estimate)) +
        geom_point() +
        labs(y = "Benchmarking", x = "MRP",
             subtitle = paste0(byMap[-1], collapse = " x ") ) +
        geom_abline(slope = 1, intercept = 0) +
        ylim(c(0, ls)) + xlim(c(0, ls))
      print(g)
    }

    return(Estimado_mrp)
  }

###############################################################################
Aux_Maps <- function(Shape,
  dat_df,
  fnames,
  cnames,
  brks =  c(0, .1, .2, .4, .6, 1),
  outPaht = NULL,
  color_p = "YlOrRd") {
  select <- dplyr::select
  byMap <- c(fnames, cnames)
  ## incluir validaciones de estimaciones ECM
  list_par <- list(
    escolar2 = c(
      "1" = "Sin educacion",
      "2" = "Primaria",
      "3" = "Secundaria",
      "4" = "Superior"
    ),
    anoest = c(
      "1" = "Sin educacion",
      "2" = "1 - 6 años",
      "3" = "7 - 12 años",
      "4" = "Más de 12",
      "98" = "No aplica",
      "99" = "NS/NR"
    ),
    sexo = c("1" = "Hombre", "2" = "Mujer"),
    etnia = c(
      "1" = "Indigena",
      "2" = "Afro",
      "3" = "Otro"
    ),
    edad =
      c(
        "1" = "0 -14",
        "2" = "15 - 29",
        "3" = "30 - 44",
        "4" = "45 - 64",
        "5" = "65 - más"
      ),
    discapacidad = c("0" = "No discapacitado", "1" = "Discapacitado"),
    area = c("0" = "Rural", "1" = "Urbana")
  )


if(all(any(byMap != "depto"))) {
  fvalue = list_par[[fnames]]
  cvalue = list_par[[cnames]]

  if (is.null(fvalue) | is.null(cvalue) ) {
    stop(paste0("La variable ",
                   paste(fnames, " o ", cnames), " no esta en la lista de parametros"))

  }

}

  ## Completando registros
    th <- paste0(
    "dat_df %<>%tidyr::expand(depto ,", fnames,",", cnames,
    ") %>% full_join(dat_df, by = c('depto','", fnames,"' , '", cnames ,"' ))"
  )
  eval(parse(text = th))


  paso <- Shape %>% left_join(dat_df,  by = c("depto")) %>%
    group_by_at(vars(byMap)) %>%
    group_nest()

  paso[["Fila"]] <-
    dplyr::recode(as.character(paso[[fnames]]) ,!!!fvalue)
  paso[["Col"]] <-
    dplyr::recode(as.character(paso[[cnames]]) ,!!!cvalue)

  if("edad" %in% byMap){
    if(fnames == "edad"){fnames <- "Edad"}else{cnames <- "Edad"}
  }
  if("anoest" %in% byMap){
    if(fnames == "anoest"){fnames <- "Escolar"}else{cnames <- "Escolar"}
  }
  if("etnia" %in% byMap){
    if(fnames == "etnia"){fnames <- "Etnia"}else{cnames <- "Etnia"}
  }
  if("sexo" %in% byMap){
    if(fnames == "sexo"){fnames <- "Sexo"}else{cnames <- "Sexo"}
  }
  if (any(c("discapacidad", "discapacitado") %in% byMap)) {
    if (any((fnames %in% c("discapacidad", "discapacitado")))) {
      fnames <- "Discapacitado"
    } else{
      cnames <- "Discapacitado"
    }
  }
  if("area" %in% byMap){
    if(fnames == "area"){fnames <- "Área"}else{cnames <- "Área"}
  }

  paso %<>% mutate(Maps = pmap(
    list(data, Fila, Col),
    .f = function(dat, fila, col) {
      tm_shape(dat) +
        tm_polygons(
          "Benchmarking_estimate",
          breaks = brks,
          title = paste0(fnames, " = ", fila, "\n", cnames, " = ", col),
          palette = color_p
        ) +
        tm_layout(asp = 0)
    }
  ))

  gmaps <- tmap_arrange(paso$Maps,
    ncol = length(unique(paso$Col)),
    norw = length(unique(paso$Fila)))

  if(!is.null(outPaht)){
  #  cat("Una copia del archivo esta en: ", outPaht, "\n")
    tmap_save(gmaps,
      outPaht,
      width = 6920,
      height = 4080,
      asp = 0
    )
  }
  return(gmaps)
}

## Plot de comparación empleando los pesos de muestreo
plot_compare2 <- function(sample_diseno, poststrat, by1){
  
  
if (all(by1 %in% names(sample_diseno$variables))) {
    dat_encuesta <- sample_diseno %>%
      group_by_at(vars(by1)) %>%
      summarise(directo = survey_mean(yk_dir),
                n_sample = unweighted(n())) %>%
      ungroup()
    
  } else{
    dat_encuesta <- unique(poststrat[,by1])
    n <- nrow(dat_encuesta)
    dat_encuesta %<>%
      mutate(
        directo = NA,
        n_sample = 1,
        directo_se = NA
      )

  }
  
  dat_censo <- poststrat %>% group_by_at(vars(by1)) %>%
    summarise(lmer = sum(n * yk_lmer) / sum(n),
              bench = sum(n * yk_bench) / sum(n), .groups = "drop") %>% 
    ungroup()
  
  dat_plot <- full_join(dat_encuesta,dat_censo, by = by1)
  
  dat_plot1 <- dat_plot %>% 
    reshape2::melt(id = c(by1,"n_sample","directo_se"))
  

  if(length(by1)== 2) {
    x <-
      apply(poststrat[, by1], MARGIN = 2, function(x)
        length(unique(x))) %>% sort(decreasing = TRUE) %>% names()
    
    xmax <- x[1]
    dat_plot1[["x"]] <- dat_plot1[[xmax]]
    xmin <- x[2]
    dat_plot1[["facet"]] <- dat_plot1[[xmin]]
  } else if (length(by1) == 1) {
    xmax = by1
    dat_plot1[["x"]] <- dat_plot1[[by1]]
  }else if(length(by1)== 3) {
    x <-
      apply(poststrat[, by1], MARGIN = 2, function(x)
        length(unique(x))) %>% sort(decreasing = TRUE) %>% names()
    xmax <- x[1] 
    dat_plot1[["x"]] <- dat_plot1[[xmax]]
    xcol <- x[2]
    dat_plot1[["xcol"]] <- dat_plot1[[xcol]]
    xfila <- x[3]
    dat_plot1[["xfila"]] <- dat_plot1[[xfila]]
  } 
  
  plot1 <- ggplot(data = dat_plot1) +
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
  
  if(all(by1 %in% names(sample_diseno$variables))){
    dat_plot2 <-
      dat_plot %>% reshape2::melt(id = c(by1, "n_sample", "directo_se", "directo"))
    
    lsy = max(dat_plot2$value)
    lsx = max(dat_plot2$directo)
    
    plot2 <- ggplot(data = dat_plot2) +
      geom_point(aes(
        x = directo,
        y = value,
      ), size = 3) +
      geom_abline(slope = 1,intercept = 0, color = "red", size = 2) +
      facet_wrap(vars(variable), ncol = 2) +
      theme_bw(20) + labs(y = "") +
      xlim(c(0, lsx+0.5)) + ylim(c(0,lsy+0.5))
  }else{
    lsy = max(dat_plot$bench)
    lsx = max(dat_plot$lmer)
    
    plot2 <- ggplot(data = dat_plot) +
      geom_point(aes(
        x = lmer,
        y = bench,
      ), size = 3) +
      geom_abline(slope = 1,intercept = 0, color = "red", size = 2) +
      theme_bw(20) + labs(y = "") +
      xlim(c(0, lsx+0.5)) + ylim(c(0,lsy+0.5)) 
  }
  #print(plot2  + plot1)
  return(list(tabla = dat_plot,  Plot = list(plot1 = plot1, plot2= plot2)))
}


## Plot de comparación empleando los pesos de muestreo
plot_compare2_depto <- function(sample_diseno, poststrat, by1){
  
  
  if (all(by1 %in% names(sample_diseno$variables))) {
    dat_encuesta <- sample_diseno %>%
      group_by_at(vars(by1)) %>%
      summarise(directo = survey_mean(yk_dir),
                n_sample = unweighted(n())) %>%
      ungroup()
    
  } else{
    dat_encuesta <- unique(poststrat[,by1])
    n <- nrow(dat_encuesta)
    dat_encuesta %<>%
      mutate(
        directo = NA,
        n_sample = 1,
        directo_se = NA
      )
    
  }
  
  dat_censo <- poststrat %>% group_by_at(vars(by1)) %>%
    summarise(lmer = sum(n * yk_lmer) / sum(n/gk),
              bench = sum(n * yk_bench) / sum(n), .groups = "drop") %>% 
    ungroup()
  
  dat_plot <- full_join(dat_encuesta,dat_censo, by = by1)
  
  dat_plot1 <- dat_plot %>% 
    reshape2::melt(id = c(by1,"n_sample","directo_se"))
  
  
  if(length(by1)== 2) {
    x <-
      apply(poststrat[, by1], MARGIN = 2, function(x)
        length(unique(x))) %>% sort(decreasing = TRUE) %>% names()
    
    xmax <- x[1]
    dat_plot1[["x"]] <- dat_plot1[[xmax]]
    xmin <- x[2]
    dat_plot1[["facet"]] <- dat_plot1[[xmin]]
  } else if (length(by1) == 1) {
    xmax = by1
    dat_plot1[["x"]] <- dat_plot1[[by1]]
  }else if(length(by1)== 3) {
    x <-
      apply(poststrat[, by1], MARGIN = 2, function(x)
        length(unique(x))) %>% sort(decreasing = TRUE) %>% names()
    xmax <- x[1] 
    dat_plot1[["x"]] <- dat_plot1[[xmax]]
    xcol <- x[2]
    dat_plot1[["xcol"]] <- dat_plot1[[xcol]]
    xfila <- x[3]
    dat_plot1[["xfila"]] <- dat_plot1[[xfila]]
  } 
  
  plot1 <- ggplot(data = dat_plot1) +
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
  
  if(all(by1 %in% names(sample_diseno$variables))){
    dat_plot2 <-
      dat_plot %>% reshape2::melt(id = c(by1, "n_sample", "directo_se", "directo"))
    
    lsy = max(dat_plot2$value)
    lsx = max(dat_plot2$directo)
    
    plot2 <- ggplot(data = dat_plot2) +
      geom_point(aes(
        x = directo,
        y = value,
      ), size = 3) +
      geom_abline(slope = 1,intercept = 0, color = "red", size = 2) +
      facet_wrap(vars(variable), ncol = 2) +
      theme_bw(20) + labs(y = "") +
      xlim(c(0, lsx+0.5)) + ylim(c(0,lsy+0.5))
  }else{
    lsy = max(dat_plot$bench)
    lsx = max(dat_plot$lmer)
    
    plot2 <- ggplot(data = dat_plot) +
      geom_point(aes(
        x = lmer,
        y = bench,
      ), size = 3) +
      geom_abline(slope = 1,intercept = 0, color = "red", size = 2) +
      theme_bw(20) + labs(y = "") +
      xlim(c(0, lsx+0.5)) + ylim(c(0,lsy+0.5)) 
  }
  print(plot2  + plot1)
  return(list(tabla = dat_plot,  Plot = list(plot1 = plot1, plot2= plot2)))
}
