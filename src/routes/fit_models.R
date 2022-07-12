library(elastic)
library(tidyverse)
library(lubridate)
library(randomForest)

#* Return calculate and save model cuaja
#* @get /models/cuaja
function() {
  bd <- as.data.frame(do.call(rbind, get_from_elasticsearch(size = 5000)))
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

  fit_cuaja(bd)
  fit_calibre(bd)
  fit_rendimiento(bd)
  print("OK")
}

fit_cuaja <- function(bd) {
  bd_cuaja <- bd %>%
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
    )


  # Casos completos
  bd_cuaja <- bd_cuaja[complete.cases(bd_cuaja), ]

  # # # # Ajuste Modelo Random Forest
  set.seed(7)
  fit_cuaja_model <- randomForest(cuaja_real ~
  variety +
    pattern +
    plants_ha +
    edad +
    conduction +
    dardos +
    ramillas +
    flores_ha_helada +
    flores_dardo_desyeme +
    flores_ramillas +
    pf_acum_mayo +
    pf_acum_jun +
    pf_acum_jul +
    dg_agosto +
    dg_septiembre +
    t_entre_17_22_sep +
    t_entre_15_20_sep +
    t_entre_20_25_sep +
    hr_agosto +
    hr_septiembre,
  data = bd_cuaja,
  mtry = 4,
  ntree = 1000
  )

  saveRDS(fit_cuaja_model, file = "../models/cuaja.rds")
}

fit_calibre <- function(bd) {
  bd_calibre <- bd %>%
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
    ))

  # Casos completos
  bd_calibre <- bd_calibre[complete.cases(bd_calibre), ]

  set.seed(6)
  fit_calibre_model <- randomForest(calibre_real ~
  variety +
    pattern +
    edad +
    dardos +
    ramillas +
    flores_dardo_desyeme +
    flores_ha_helada +
    cuaja_real +
    dg_septiembre +
    dg_octubre,
  data = bd_calibre,
  mtry = 4,
  ntree = 1000
  )
  saveRDS(fit_calibre_model, file = "../models/calibre.rds")
}

fit_rendimiento <- function(bd) {
  bd_rendimiento <- bd %>%
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
    ))

  # Casos completos
  bd_rendimiento <- bd_rendimiento[complete.cases(bd_rendimiento), ]

  # Ajuste Random Forest
  set.seed(6)
  fit_rendimiento_model <- randomForest(rendimiento_real ~
  variety +
    pattern +
    edad +
    dardos +
    ramillas +
    flores_ha_helada +
    cuaja_real +
    calibre_real +
    dg_octubre +
    dg_noviembre,
  data = bd_rendimiento,
  mtry = 4,
  ntree = 1000
  )
  saveRDS(fit_rendimiento_model, file = "../models/rendimiento.rds")
}

get_from_elasticsearch <- function(size = 5000) {
  ELASTICSEARCH_R_HOST <- Sys.getenv("ELASTICSEARCH_R_HOST")
  es <- connect(
    host = ELASTICSEARCH_R_HOST, path = "",
    port = 9200, transport_schema = "http"
  )
  body <- '{
    "query": {
        "bool": {
            "filter": [
                {
                  "match_phrase": {
                    "kind_insert.keyword": "upload_file"
                  }
                },
                {
                    "exists": {
                        "field": "season"
                    }
                },
                {
                    "exists": {
                        "field": "season_year"
                    }
                },
                {
                    "exists": {
                        "field": "cuartel"
                    }
                },
                {
                    "exists": {
                        "field": "agricola"
                    }
                },
                {
                    "exists": {
                        "field": "centro_costo"
                    }
                },
                {
                    "exists": {
                        "field": "cliente"
                    }
                },
                {
                    "exists": {
                        "field": "huerto"
                    }
                },
                {
                    "exists": {
                        "field": "comuna"
                    }
                },
                {
                    "exists": {
                        "field": "quarter"
                    }
                },
                {
                    "exists": {
                        "field": "variety"
                    }
                },
                {
                    "exists": {
                        "field": "pattern"
                    }
                },
                {
                    "exists": {
                        "field": "plants_ha"
                    }
                },
                {
                    "exists": {
                        "field": "edad"
                    }
                },
                {
                    "exists": {
                        "field": "conduction"
                    }
                },
                {
                    "exists": {
                        "field": "dardos"
                    }
                },
                {
                    "exists": {
                        "field": "cuaja_real"
                    }
                },
                {
                    "exists": {
                        "field": "calibre_real"
                    }
                }
            ],
            "must_not": []
        }
    },
    "_source": [
      "season",
      "season_year",
      "cliente",
      "huerto",
      "agricola",
      "centro_costo",
      "cuartel",
      "comuna",
      "quarter",
      "variety",
      "pattern",
      "surface",
      "plants_ha",
      "edad",
      "conduction",
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
    ]
  }'
  body
  result <- Search(
    es,
    index = "ceranalytics_index", size = size, body = body
  )$hits$hits

  map(result, parse_source)
}

x <- c(
  "calibre_estimado",
  "calibre_real",
  "cliente",
  "cuartel",
  "centro_costo",
  "agricola",
  "comuna",
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
  "dg_octubre",
  "dg_octubre_hist",
  "dg_noviembre",
  "dg_noviembre_hist",
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

parse_source <- function(results) {
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
