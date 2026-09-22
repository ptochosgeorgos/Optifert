# ==============================================================================
# SCRIPT: Lysimeter_prep_data.R
# ZWECK:  Vollständige modulare Datenaufbereitung für das Teilprojekt Lysimeter.
#         Erzeugt harmonisierte Ausgabedateien im Ordner prep_data/.
# ==============================================================================

library(tidyverse)
library(lubridate)
library(glue)

# Robuste Pfad-Ermittlung
get_script_dir <- function() {
  if (requireNamespace("this.path", quietly = TRUE)) {
    return(this.path::here())
  }
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  }
  if (dir.exists("data/Lysimeter")) {
    return(normalizePath("data/Lysimeter"))
  }
  return(getwd())
}
script_dir <- get_script_dir()
here_lysi <- function(...) file.path(script_dir, ...)

# ==============================================================================
# 0. STANDORT- & VERSUCHS-KONFIGURATION
# ==============================================================================
site_config <- list(
  lte           = "Lysimeter",
  site_name     = "Reckenholz / Tänikon Lysimeter",
  plz           = 8356,
  latitude      = 47.254,
  longitude     = 8.310,
  crop          = "WW",
  base_temp     = 0,
  swissflux_uid = "feslj41e3vt34c"
)

prep_dir <- here_lysi("prep_data")
if (!dir.exists(prep_dir)) {
  dir.create(prep_dir, recursive = TRUE)
}

# ==============================================================================
# 1. NMIN & ENZYMAKTIVITÄTEN (2025 & dynamisch 2026+)
# ==============================================================================
eea_file <- here_lysi("raw_data/Lysimeter_NMin_EEA.csv")
df_eea <- tibble()

if (file.exists(eea_file)) {
  cat("  Lese Lysimeter NMin_EEA Daten...\n")
  df_eea <- read_delim(eea_file, delim = ";", show_col_types = FALSE) |>
    mutate(
      date = dmy(date),
      year = year(date),
      plot_nr = as.numeric(id_lysi),
      id_lysi = plot_nr,
      treatment = as.character(treat),
      treat = treatment,
      soil_type = as.character(carbon),
      carbon = soil_type,
      crop = site_config$crop,
      NH4 = as.numeric(NH4_N),
      NO3 = as.numeric(NO3_N),
      Ntot = NH4 + NO3,
      across(c(LAP, NAG, GLS, MUP, MUX), as.numeric),
      LTE = site_config$lte
    )
}

# Ergänzung mit 2026 Daten falls vorhanden
nmin_2026_file <- here_lysi("raw_data/Lysimeter_NMin_2026.csv")
if (file.exists(nmin_2026_file)) {
  cat("  Lese Lysimeter NMin 2026 Daten...\n")
  df_2026 <- read_csv(nmin_2026_file, show_col_types = FALSE) |>
    mutate(
      date = dmy(date),
      year = year(date),
      plot_nr = as.numeric(id_lysi),
      id_lysi = plot_nr,
      treatment = as.character(id_treat),
      treat = treatment,
      soil_type = as.character(soil_type),
      carbon = soil_type,
      crop = site_config$crop,
      NH4 = as.numeric(`NH4-N`),
      NO3 = as.numeric(`NO3-N`),
      Ntot = NH4 + NO3,
      LAP = NA_real_,
      NAG = NA_real_,
      GLS = NA_real_,
      MUP = NA_real_,
      MUX = NA_real_,
      LTE = site_config$lte
    )
  
  # Zusammenführen, Duplikate vermeiden
  df_eea <- bind_rows(df_eea, df_2026) |>
    distinct(date, plot_nr, .keep_all = TRUE)
}

# Speichere bereinigten NMin/EEA Datensatz
write_csv(df_eea, file.path(prep_dir, "Lysimeter_cleansed.csv"))
saveRDS(df_eea, file.path(prep_dir, "Lysimeter_cleansed.rds"))
cat("  Lysimeter_cleansed gespeichert (", nrow(df_eea), " Zeilen)\n")

# ==============================================================================
# 2. SEEPAGE WATER / SICKERWASSER & AUSWASCHUNG (N-Leaching)
# ==============================================================================
fia_file <- here_lysi("raw_data/Lysimeter_Seepage_water_FIA.csv")
if (file.exists(fia_file)) {
  cat("  Berechne N-Leaching...\n")
  seepage_raw <- read_csv(fia_file, show_col_types = FALSE)
  
  # Berechnung N-Auswaschung pro Lysimeter
  seepage_prep <- seepage_raw |>
    mutate(
      date = as.Date(date),
      plot_nr = as.numeric(id_lysi),
      Nleach = (NO3_N + NH4_N) * volume * 10000 / (.3^2 * pi) / 10^6
    ) |>
    group_by(id_lysi) |>
    arrange(date) |>
    mutate(Nleach_cum = cumsum(coalesce(Nleach, 0))) |>
    ungroup()
  
  saveRDS(seepage_prep, file.path(prep_dir, "leachate_postsummer25.rds"))
}

# ==============================================================================
# 3. ERTRAGSDATEN (Crop Yield WW25 & zukünftige Ernten)
# ==============================================================================
crop_file <- here_lysi("raw_data/Lysimeter_crop_WW25.csv")
if (file.exists(crop_file)) {
  cat("  Verarbeite Ertragsdaten...\n")
  crop_raw <- read_csv(crop_file, show_col_types = FALSE)
  
  crop_prep <- crop_raw |>
    mutate(
      plot_nr = as.numeric(id_lysi),
      yield = as.numeric(korn_TS),
      yield_type = "grain",
      yield_unit = "g_ts_per_lysi",
      crop = site_config$crop,
      year = 2025
    )
  
  saveRDS(crop_prep, file.path(prep_dir, "Lysimeter_crop_yield.rds"))
}

# ==============================================================================
# 4. WETTER & BODENFEUCHTE (SWC via SwissFlux API & Station)
# ==============================================================================
weather_file <- here_lysi("raw_data/Lysimeter_weather.csv")
if (file.exists(weather_file)) {
  cat("  Aggregiere tägliche Wetterdaten...\n")
  weather_raw <- read_csv(weather_file, show_col_types = FALSE)
  
  weather_daily <- weather_raw |>
    mutate(date = as.Date(datetime)) |>
    group_by(date) |>
    summarise(
      precip_sum = sum(precipitation, na.rm = TRUE),
      temp_median = median(air_temp_mean, na.rm = TRUE),
      soil_temp_10cm_median = median(soil_temp_10cm, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(date) |>
    mutate(
      GDD = pmax(0, temp_median - site_config$base_temp),
      GDD_cum = cumsum(replace_na(GDD, 0))
    )
  
  # SWC via SwissFluxNet API abrufen
  swc_daily_file <- file.path(prep_dir, "Lysimeter_swc_daily.rds")
  swc_daily <- tibble(date = as.Date(character()), SWC = numeric())
  
  swissflux_script <- normalizePath(file.path(script_dir, "../../scripts/fetch_swissflux.R"), mustWork = FALSE)
  if (!file.exists(swissflux_script)) {
    swissflux_script <- "scripts/fetch_swissflux.R"
  }
  
  if (file.exists(swissflux_script)) {
    tryCatch({
      cat("  Rufe SWC-Bodenfeuchte via SwissFluxNet API ab...\n")
      source(swissflux_script)
      swc_raw <- fetch_swissflux_data(
        measurement = "SWC", 
        interval = "1d", 
        from = "2024-01-01T00:00:00Z", 
        to = "2026-12-31T23:59:59Z"
      )
      if (nrow(swc_raw) > 0) {
        swc_daily <- swc_raw |>
          mutate(date = as.Date(Time), SWC = as.numeric(Value)) |>
          select(date, SWC) |>
          distinct(date, .keep_all = TRUE)
        saveRDS(swc_daily, swc_daily_file)
      }
    }, error = function(e) {
      cat("  Hinweis (SwissFlux):", e$message, "\n")
    })
  }
  
  # Falls API-Abruf offline fehlschlägt, gecachte Version laden
  if (nrow(swc_daily) == 0 && file.exists(swc_daily_file)) {
    swc_daily <- readRDS(swc_daily_file)
  }
  
  env_daily <- weather_daily |>
    left_join(swc_daily, by = "date")
  
  saveRDS(env_daily, file.path(prep_dir, "Lysimeter_env_daily.rds"))
  cat("  Lysimeter_env_daily gespeichert.\n")
}

cat("Lysimeter Vorbereitung erfolgreich abgeschlossen!\n")

