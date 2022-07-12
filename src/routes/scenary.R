
library(elastic)
library(tidyverse)
library(lubridate)
library(randomForest)

#* Return single calculation from input
#* @post /scenary
function(req, res) {
body <- req$body
bd <- request_dataframe(body)
bd <- generate_dart_twig(bd)
predict_cuaja(bd) -> bd$cuaja_estimada  
predict_calibre(bd) -> bd$calibre_estimado
predict_rendimiento(bd) -> bd$rend_estimado_prepoda
bd

}

request_dataframe <- function(body) {
  season <- c(body$season)
  season_year <- c(body$season_year)
  cliente <- c(body$cliente)
  agricola <- c(body$agricola)
  huerto <- c(body$huerto)
  centro_costo <- c(body$centro_costo)
  cuartel <- c(body$cuartel)
  quarter <- c(body$quarter)
  comuna <- c(body$comuna)
  variety <- c(body$variety)
  pattern <- c(body$pattern)
  surface <- c(body$surface)
  plants_ha <- c(body$plants_ha)
  edad <- c(body$edad)
  conduction <- c(body$conduction)
  laterales <- c(body$laterales)
  dardos_prepoda <- c(body$dardos_prepoda)
  dardos <- c(body$dardos)
  flores_dardo <- c(body$flores_dardo)
  yemas_dardo <- c(body$yemas_dardo)
  flores_yemas_dardo <- c(body$flores_yemas_dardo)
  desyeme <- c(body$desyeme)
  flores_dardo_desyeme <- c(body$flores_dardo_desyeme)
  desyeme_post <- c(body$desyeme_post)
  ramillas_prepoda <- c(body$ramillas_prepoda)
  ramillas <- c(body$ramillas)
  flores_ramillas <- c(body$flores_ramillas)
  flores_ha_helada <- c(body$flores_ha_helada)
  cuaja_estimada <- c(body$cuaja_estimada)
  cuaja_real <- c(body$cuaja_real)
  calibre_estimado <- c(body$calibre_estimado)
  calibre_real <- c(body$calibre_real)
  rend_estimado_prepoda <- c(body$rend_estimado_prepoda)
  rend_estimado_pospoda <- c(body$rend_estimado_pospoda)
  rendimiento_real <- c(body$rendimiento_real)
  pf_acum_mayo <- c(body$pf_acum_mayo)
  pf_acum_jun <- c(body$pf_acum_jun)
  pf_acum_jul <- c(body$pf_acum_jul)
  dg_agosto <- c(body$dg_agosto)
  dg_septiembre <- c(body$dg_septiembre)
  dg_octubre <- c(body$dg_octubre)
  dg_noviembre <- c(body$dg_noviembre)
  t_entre_17_22_sep <- c(body$t_entre_17_22_sep)
  t_entre_15_20_sep <- c(body$t_entre_15_20_sep)
  t_entre_20_25_sep <- c(body$t_entre_20_25_sep)
  hr_agosto <- c(body$hr_agosto)
  hr_septiembre <- c(body$hr_septiembre)
  pf_acum_mayo_hist <- c(body$pf_acum_mayo_hist)
  pf_acum_junio_hist <- c(body$pf_acum_junio_hist)
  pf_acum_julio_hist <- c(body$pf_acum_julio_hist)
  dg_agosto_hist <- c(body$dg_agosto_hist)
  dg_septiembre_hist <- c(body$dg_septiembre_hist)
  dg_octubre_hist <- c(body$dg_octubre_hist)
  dg_noviembre_hist <- c(body$dg_noviembre_hist)
  t_entre_17_22_sep_hist <- c(body$t_entre_17_22_sep_hist)
  t_entre_15_20_sep_hist <- c(body$t_entre_15_20_sep_hist)
  t_entre_20_25_sep_hist <- c(body$t_entre_20_25_sep_hist)
  hr_agosto_hist <- c(body$hr_agosto_hist)
  hr_septiembre_hist <- c(body$hr_septiembre_hist)
  
  bd <- data.frame(
    season,
    season_year,
    cliente,
    huerto,
    cuartel,
    centro_costo,
    agricola,
    comuna,
    quarter,
    variety,
    pattern,
    surface,
    plants_ha,
    edad,
    conduction,
    laterales,
    dardos_prepoda,
    dardos,
    flores_dardo,
    yemas_dardo,
    flores_yemas_dardo,
    desyeme,
    flores_dardo_desyeme,
    desyeme_post,
    ramillas_prepoda,
    ramillas,
    flores_ramillas,
    flores_ha_helada,
    cuaja_estimada,
    cuaja_real,
    calibre_estimado,
    calibre_real,
    rend_estimado_prepoda,
    rend_estimado_pospoda,
    rendimiento_real,
    pf_acum_mayo,
    pf_acum_jun,
    pf_acum_jul,
    t_entre_17_22_sep,
    t_entre_15_20_sep,
    t_entre_20_25_sep,
    hr_agosto,
    hr_septiembre,
    pf_acum_mayo_hist,
    pf_acum_junio_hist,
    pf_acum_julio_hist,
    dg_agosto_hist,
    dg_septiembre_hist,
    dg_octubre_hist,
    dg_noviembre_hist,
    dg_agosto,
    dg_septiembre,
    dg_octubre,
    dg_noviembre,
    t_entre_17_22_sep_hist,
    t_entre_15_20_sep_hist,
    t_entre_20_25_sep_hist,
    hr_agosto_hist,
    hr_septiembre_hist
  )
  
  bd <- bd %>%
    mutate(across(
      any_of(
        c(
          "conduction",
          "pattern",
          "variety"
        )
      ),
      as.factor
    )) %>%
    mutate(across(any_of(c(
      "season_year",
      "surface",
      "plants_ha",
      "edad",
      "laterales",
      "dardos_prepoda",
      "dardos",
      "flores_dardo",
      "yemas_dardo",
      "flores_yemas_dardo",
      "desyeme",
      "flores_dardo_desyeme",
      "desyeme_post",
      "ramillas_prepoda",
      "ramillas",
      "flores_ramillas",
      "flores_ha_helada",
      "cuaja_estimada",
      "cuaja_real",
      "calibre_estimado",
      "calibre_real",
      "rend_estimado_prepoda",
      "rend_estimado_pospoda",
      "rendimiento_real",
      "pf_acum_mayo",
      "pf_acum_jun",
      "pf_acum_jul",
      "dg_agosto",
      "dg_septiembre",
      "dg_octubre",
      "dg_noviembre",
      "t_entre_17_22_sep",
      "t_entre_15_20_sep",
      "t_entre_20_25_sep",
      "hr_agosto",
      "hr_septiembre",
      "pf_acum_mayo_hist",
      "pf_acum_junio_hist",
      "pf_acum_julio_hist",
      "dg_agosto_hist",
      "dg_septiembre_hist",
      "dg_octubre_hist",
      "dg_noviembre_hist",
      "t_entre_17_22_sep_hist",
      "t_entre_15_20_sep_hist",
      "t_entre_20_25_sep_hist",
      "hr_agosto_hist",
      "hr_septiembre_hist"
    )), as.numeric))
}

generate_dart_twig <- function(bd){
  # Generar valores de Dardos y ramillas ================================================================================
  # Establecer combinaciones para DyR según criterios conversados
  
  ## Dardos ---------
  # Secuencia (partiendo del 35% del valor inicial)
  dardos_prop <- seq(from= round(as.numeric((bd[ , "dardos"]) * 0.35), 0),
                     to= as.numeric(bd[ , "dardos"]), 
                     by=10)
  ## Ramillas -------
  # Secuencia (partiendo del 35% del valor inicial)
  ramillas_prop <- seq(from= round(as.numeric((bd[ , "ramillas"]) * 0.35), 0),
                       to= as.numeric(bd[ , "ramillas"]), 
                       by=5)
  
  ### Tabla con combinaciones de D y R -----------------------
  dardos_ramillas <- expand.grid(dardos_prop= dardos_prop,
                                 ramillas_prop= ramillas_prop) 
  
  
  do.call("rbind", 
          replicate(length(dardos_prop)*length(ramillas_prop) , 
                    bd, 
                    simplify=FALSE) )-> escenarios
  
  
  # Crear columna escenarios
  escenarios |> 
    mutate(escenarios = paste( paste(paste("D", rep(seq(1,length(dardos_prop)),length(ramillas_prop))  , sep="") ,  
                                     paste( "R", rep(c(1:length(ramillas_prop)), each = length(dardos_prop)), sep="" ),
                                     sep = "-"),
                               sep=" " )) -> escenarios
  
  # Agregar casos probables de D y R  (combinatoria proveniente de tabla de combinaciones)
  escenarios$dardos <- dardos_ramillas$dardos_prop
  escenarios$ramillas <- dardos_ramillas$ramillas_prop
  
  
  # Corregir el valor de flores_ha (que fue repetido a partir de datos prepoda)
  # esto corregirá a nuevos valores de flores según los cambios en dardos y ramillas 
  escenarios %>% 
    mutate(flores_ha_helada = (dardos*flores_dardo_desyeme*plants_ha)+
             (ramillas*flores_ramillas*plants_ha)) -> escenarios
  
}


predict_cuaja <- function(bd) {
  bd %>%
    dplyr::select(
      season,
      season_year,
      cliente,
      huerto,
      comuna,
      quarter,
      variety,
      pattern,
      plants_ha,
      edad,
      conduction,
      dardos,
      ramillas,
      flores_ha_helada,
      flores_dardo_desyeme,
      flores_ramillas,
      pf_acum_mayo,
      pf_acum_jun,
      pf_acum_jul,
      dg_agosto,
      dg_septiembre,
      t_entre_17_22_sep, # Contar horas en estos rangos
      t_entre_15_20_sep,
      t_entre_20_25_sep,
      hr_agosto, # 1 Mes
      hr_septiembre,
      pf_acum_mayo_hist,
      pf_acum_junio_hist,
      pf_acum_julio_hist,
      dg_agosto_hist,
      dg_septiembre_hist,
      t_entre_17_22_sep_hist,
      t_entre_15_20_sep_hist,
      t_entre_20_25_sep_hist,
      hr_agosto_hist, # Promedio Mes todos los años
      hr_septiembre_hist,
      rendimiento_real,
      cuaja_real,
      cuaja_estimada
    ) -> test_data
  
  fit_cuaja_model <- readRDS("../models/cuaja.rds")
  for (col_name in names(test_data)) {
    levels(test_data[[col_name]]) <- fit_cuaja_model$forest$xlevels[[col_name]]
  }
  
  for (i in 1:nrow(test_data)){
    predict(fit_cuaja_model,
            data.frame(variety= test_data$variety,
                       pattern= test_data$pattern,
                       plants_ha= test_data$plants_ha,
                       edad= test_data$edad,
                       conduction= test_data$conduction,
                       dardos= test_data$dardos,
                       ramillas= test_data$ramillas,
                       flores_ha_helada= test_data$flores_ha_helada,
                       flores_dardo_desyeme= test_data$flores_dardo_desyeme,
                       flores_ramillas= test_data$flores_ramillas,
                       pf_acum_mayo= test_data$pf_acum_mayo,
                       pf_acum_jun= test_data$pf_acum_junio_hist,
                       pf_acum_jul= test_data$pf_acum_julio_hist,
                       dg_agosto= test_data$dg_agosto_hist, 
                       dg_septiembre= test_data$dg_septiembre_hist,
                       t_entre_17_22_sep= test_data$t_entre_17_22_sep_hist,
                       t_entre_15_20_sep= test_data$t_entre_15_20_sep_hist,
                       t_entre_20_25_sep= test_data$t_entre_20_25_sep_hist,
                       hr_agosto= test_data$hr_agosto_hist,
                       hr_septiembre=test_data$hr_septiembre_hist)[i,]) -> test_data$cuaja_estimada[i]
    
  }
  
  test_data$cuaja_estimada -> bd$cuaja_estimada
}


predict_calibre <- function(bd) {
  bd %>%
    select(c(
      calibre_real,
      season,
      huerto,
      quarter,
      variety,
      pattern,
      edad,
      dardos,
      ramillas,
      flores_dardo_desyeme,
      flores_ha_helada,
      flores_ramillas,
      cuaja_estimada,
      cuaja_real,
      dg_septiembre,
      dg_octubre,
      dg_septiembre_hist,
      dg_octubre_hist
    )) -> test_data
  fit_calibre_model <- readRDS("../models/calibre.rds")
  for (col_name in names(test_data)) {
    levels(test_data[[col_name]]) <- fit_calibre_model$forest$xlevels[[col_name]]
  }
  for (i in 1:nrow(test_data)){
    predict(fit_calibre_model,
            data.frame(variety= test_data$variety,
                       pattern= test_data$pattern,
                       edad= test_data$edad,
                       dardos= test_data$dardos,
                       ramillas= test_data$ramillas,
                       flores_ha_helada= test_data$flores_ha_helada,
                       flores_dardo_desyeme= test_data$flores_dardo_desyeme,
                       flores_ramillas= test_data$flores_ramillas,
                       cuaja_real = as.numeric(bd$cuaja_estimada), 
                       dg_septiembre= test_data$dg_septiembre_hist,
                       dg_octubre= test_data$dg_octubre_hist)[i,]) -> test_data$calibre_estimado[i]
    
  }
  test_data$calibre_estimado -> bd$calibre_estimado
}







predict_rendimiento <- function(bd) {
  bd %>%
    select(c(
      season,
      huerto,
      variety,
      pattern,
      edad,
      dardos,
      ramillas,
      flores_ha_helada,
      cuaja_estimada,
      cuaja_real,
      calibre_estimado,
      calibre_real,
      rend_estimado_prepoda,
      rendimiento_real,
      dg_octubre,
      dg_noviembre,
      dg_octubre_hist,
      dg_noviembre_hist
    )) -> test_data
  fit_rendimiento_model <- readRDS("../models/rendimiento.rds")
  for (col_name in names(test_data)) {
    levels(test_data[[col_name]]) <- fit_rendimiento_model$forest$xlevels[[col_name]]
  }
  for (i in 1:nrow(test_data)){
    predict(fit_rendimiento_model,
            data.frame(variety= test_data$variety,
                       pattern= test_data$pattern,
                       edad= test_data$edad,
                       dardos= test_data$dardos,
                       ramillas= test_data$ramillas,
                       flores_ha_helada= test_data$flores_ha_helada,
                       cuaja_real = as.numeric(bd$cuaja_estimada),
                       calibre_real = as.numeric(bd$calibre_estimado),
                       dg_octubre = test_data$dg_octubre_hist,
                       dg_noviembre = test_data$dg_noviembre_hist)[i,]) -> test_data$rend_estimado_prepoda[i]
    
  }
  
  test_data$rend_estimado_prepoda -> bd$rend_estimado_prepoda
}



x <- c(
  "calibre_estimado",
  "calibre_real",
  "cliente",
  "comuna",
  "cuartel",
  "agricola",
  "centro_costo",
  "conduction",
  "cuaja_estimada",
  "cuaja_real",
  "dardos_prepoda",
  "dardos",
  "desyeme_post",
  "desyeme",
  "dg_agosto",
  "dg_agosto_hist",
  "dg_septiembre_hist",
  "dg_septiembre",
  "edad",
  "flores_dardo_desyeme",
  "flores_dardo",
  "flores_ha_helada",
  "flores_ramillas",
  "flores_yemas_dardo",
  "hr_agosto",
  "hr_agosto_hist",
  "hr_septiembre_hist",
  "hr_septiembre",
  "huerto",
  "laterales",
  "pattern",
  "pf_acum_jul",
  "pf_acum_jun",
  "pf_acum_mayo",
  "pf_acum_julio_hist",
  "pf_acum_junio_hist",
  "pf_acum_mayo_hist",
  "plants_ha",
  "quarter",
  "ramillas_prepoda",
  "ramillas",
  "rend_estimado_pospoda",
  "rend_estimado_prepoda",
  "rendimiento_real",
  "season_year",
  "season",
  "surface",
  "t_entre_15_20_sep_hist",
  "t_entre_15_20_sep",
  "t_entre_17_22_sep_hist",
  "t_entre_17_22_sep",
  "t_entre_20_25_sep_hist",
  "t_entre_20_25_sep",
  "variety",
  "yemas_dardo"
)


extract_source <- function(results) {
  source_raw <- unlist(results$`_source`)
  
  # source <- list(x)
  source <- NULL
  for (name in x) {
    if (is.na(source_raw[name]) || source_raw[name] == "") {
      source[name] <- 0
    } else {
      source[name] <- source_raw[name]
    }
  }
  source
}
