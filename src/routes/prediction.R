library(elastic)
library(tidyverse)
library(lubridate)
library(randomForest)

#* Return single calculation from input
#* @post /quarter
function(req, res) {
  body <- req$body
  # fit_cuaja_model <- readRDS("../models/cuaja.rds")
  bd <- request_dataframe(body)
  predict_cuaja(bd, body$season_year) -> bd[(bd$season_year == body$season_year), "cuaja_estimada"]
  predict_cuaja(bd, body$season_year) -> bd[(bd$season_year == body$season_year), "cuaja_real"]
  predict_calibre(bd, body$season_year) -> bd[(bd$season_year == body$season_year), "calibre_estimado"]
  predict_calibre(bd, body$season_year) -> bd[(bd$season_year == body$season_year), "calibre_real"]
  predict_rendimiento(bd, body$season_year) -> bd[(bd$season_year == body$season_year), "rend_estimado_prepoda"]
  bd
  # rend_potencial <- 10000

  # bd %>%
  #   arrange(season) -> cuartel_tipo

  # bd %>%  
  #   dplyr::filter(huerto == unique(cuartel_tipo$huerto) &
  #     variety == unique(cuartel_tipo$variety)) %>%
  #   summarise(mean = mean(calibre_real, na.rm = T)) %>%
  #   as.numeric() %>%
  #   round(2) -> cuartel_tipo[(cuartel_tipo$season_year == "2021"), "calibre_estimado"]



  # # # # Dardos
  # # # # Secuencia (partiendo de 5)
  # dardos_prop <- seq(
  #   from = 50,
  #   to = unique(as.numeric(cuartel_tipo[(cuartel_tipo$season_year == "2021"), "dardos_prepoda"] - 1)),
  #   by = 10
  # )

  # # # # Ramillas
  # # # # Secuencia (partiendo de 3)
  # ramillas_prop <- seq(
  #   from = 10,
  #   to = unique(as.numeric(cuartel_tipo[(cuartel_tipo$season_year == "2021"), "ramillas_prepoda"] - 1)),
  #   by = 5
  # )

  # # # # Tabla con combinaciones de Dardos y Ramillas
  # dardos_ramillas <- expand.grid(
  #   dardos_prop = dardos_prop,
  #   ramillas_prop = ramillas_prop
  # )

  # cuartel_tipo %>%
  #   dplyr::filter(season_year == "2021") -> caso_2021

  # do.call(
  #   "rbind",
  #   replicate(length(dardos_prop) * length(ramillas_prop),
  #     caso_2021,
  #     simplify = FALSE
  #   )
  # ) -> escenarios_2021

  # # Cambiar season data por escenarios DR
  # escenarios_2021$season_year %>%
  #   paste(paste(paste("D", rep(seq(1, length(dardos_prop)), length(ramillas_prop)), sep = ""),
  #     paste("R", rep(c(1:length(ramillas_prop)), each = length(dardos_prop)), sep = ""),
  #     sep = "-"
  #   ),
  #   sep = " "
  #   ) -> escenarios_2021$season_year

  # # # Agregar casos probables de D y R
  # escenarios_2021$dardos <- dardos_ramillas$dardos_prop
  # escenarios_2021$ramillas <- dardos_ramillas$ramillas_prop

  # # Corregir el valor de flores_ha (que fue repetido a partir de datos prepoda)
  # escenarios_2021 %>%
  #   mutate(flores_ha_helada = (dardos * flores_dardo_desyeme * plants_ha) +
  #     (ramillas * flores_ramillas * plants_ha)) -> escenarios_2021

  # test_data <- data.frame(
  #   conduction = caso_2021$conduction,
  #   dardos = caso_2021$dardos_prepoda,
  #   dg_agosto = caso_2021$dg_agosto,
  #   dg_septiembre = caso_2021$dg_septiembre,
  #   edad = caso_2021$edad,
  #   flores_dardo_desyeme = caso_2021$flores_dardo_desyeme,
  #   flores_ha_helada = caso_2021$flores_ha_helada,
  #   flores_ramillas = caso_2021$flores_ramillas,
  #   hr_agosto = caso_2021$hr_agosto_hist,
  #   hr_septiembre = caso_2021$hr_septiembre_hist,
  #   pattern = caso_2021$pattern,
  #   pf_acum_jul = caso_2021$pf_acum_jul,
  #   pf_acum_jun = caso_2021$pf_acum_jun,
  #   pf_acum_mayo = caso_2021$pf_acum_mayo,
  #   plants_ha = caso_2021$plants_ha,
  #   ramillas = caso_2021$ramillas_prepoda,
  #   t_entre_15_20_sep = caso_2021$t_entre_15_20_sep,
  #   t_entre_17_22_sep = caso_2021$t_entre_17_22_sep,
  #   t_entre_20_25_sep = caso_2021$t_entre_20_25_sep,
  #   variety = caso_2021$variety
  # )

  # for (col_name in names(test_data)) {
  #   levels(test_data[[col_name]]) <- fit_cuaja_model$forest$xlevels[[col_name]]
  # }
  # for (col_name in names(escenarios_2021)) {
  #   levels(escenarios_2021[[col_name]]) <- fit_cuaja_model$forest$xlevels[[col_name]]
  # }


  # predict(
  #   fit_cuaja_model,
  #   test_data
  # ) -> cuartel_tipo[cuartel_tipo$season_year == "2021", "cuaja_estimada"]

  # (cuartel_tipo[cuartel_tipo$season_year == "2021", "flores_ha_helada"] *
  #   cuartel_tipo[cuartel_tipo$season_year == "2021", "cuaja_estimada"] / 100 *
  #   cuartel_tipo[cuartel_tipo$season_year == "2021", "calibre_estimado"]) / 1000 -> cuartel_tipo[cuartel_tipo$season_year == "2021", "rend_estimado_prepoda"]

  # # # Pospoda (con datos de escenarios)

  # # # 1 - Cuaja para escenarios completos

  # for (i in 1:nrow(escenarios_2021)) {
  #   escenarios_2021$cuaja_estimada[i] <- predict(
  #     fit_cuaja_model,
  #     data.frame(
  #       variety = escenarios_2021$variety,
  #       pattern = escenarios_2021$pattern,
  #       plants_ha = escenarios_2021$plants_ha,
  #       edad = escenarios_2021$edad,
  #       conduction = escenarios_2021$conduction,
  #       dardos = escenarios_2021$dardos,
  #       ramillas = escenarios_2021$ramillas,
  #       flores_ha_helada = escenarios_2021$flores_ha_helada,
  #       flores_dardo_desyeme = escenarios_2021$flores_dardo_desyeme,
  #       flores_ramillas = escenarios_2021$flores_ramillas,
  #       pf_acum_mayo = escenarios_2021$pf_acum_mayo,
  #       pf_acum_jun = escenarios_2021$pf_acum_junio_hist,
  #       pf_acum_jul = escenarios_2021$pf_acum_julio_hist,
  #       dg_agosto = escenarios_2021$dg_agosto_hist,
  #       dg_septiembre = escenarios_2021$dg_septiembre_hist,
  #       t_entre_17_22_sep = escenarios_2021$t_entre_17_22_sep_hist,
  #       t_entre_15_20_sep = escenarios_2021$t_entre_15_20_sep_hist,
  #       t_entre_20_25_sep = escenarios_2021$t_entre_20_25_sep_hist,
  #       hr_agosto = escenarios_2021$hr_agosto_hist,
  #       hr_septiembre = escenarios_2021$hr_septiembre_hist
  #     )[i, ]
  #   ) # predice cada fila
  # }

  # # # Rendimiento estimado
  # escenarios_2021 %>%
  #   mutate(rend_estimado_pospoda = (flores_ha_helada * (cuaja_estimada / 100) * calibre_estimado) / 1000) -> escenarios_2021

  # # Filtro para rendimiento esperado (+/- 5%)
  # escenarios_2021 %>%
  #   dplyr::filter(rend_estimado_pospoda >= (rend_potencial - (rend_potencial * 0.05)) &
  #     rend_estimado_pospoda <= (rend_potencial + (rend_potencial * 0.05))) %>%
  #   arrange(desc(rend_estimado_pospoda)) -> escenarios_2021_rend_esperado

  # # Juntar tablas cuartel tipo + estimaciones filtradas
  # rbind(cuartel_tipo, escenarios_2021_rend_esperado) -> propuesta_poda
}

request_dataframe <- function(body) {
  season <- c(body$season)
  season_year <- c(body$season_year)
  cliente <- c(body$cliente)
  huerto <- c(body$huerto)
  cuartel <- c(body$cuartel)
  agricola <- c(body$agricola)
  centro_costo <- c(body$centro_costo)
  comuna <- c(body$comuna)
  quarter <- c(body$quarter)
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
  dg_agosto <- c(body$dg_agosto)
  dg_septiembre <- c(body$dg_septiembre)
  dg_octubre <- c(body$dg_octubre)
  dg_noviembre <- c(body$dg_noviembre)
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

predict_calibre <- function(bd, season_year) {
  bd %>%
    dplyr::filter(season_year == season_year) %>%
    dplyr::select(c(
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

  predict(
    fit_calibre_model,
    test_data
  ) -> bd[(bd$season_year == season_year), "calibre_estimado"]
}

predict_cuaja <- function(bd, season_year) {
  bd %>%
    dplyr::filter(season_year == season_year) %>%
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
  predict(
    fit_cuaja_model,
    test_data
  ) -> bd[bd$season_year ==  season_year, "cuaja_estimada"]
}

predict_rendimiento <- function(bd, season_year) {
  bd %>%
    dplyr::filter(season_year == season_year) %>%
    dplyr::select(c(
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
      rend_estimado_pospoda,
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
  predict(
    fit_rendimiento_model,
    test_data
  ) -> bd[bd$season_year == season_year, "rend_estimado_prepoda"]
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
