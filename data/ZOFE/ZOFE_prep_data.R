# ==============================================================================
# SCRIPT: ZOFE_prep_data.R
# ZWECK:  Datenimport, Bereinigung, Merging (Agroscope, Digit Soil, Nmin)
# ==============================================================================


# 1. Pakete & Pfad-Setup
library(tidyverse)
library(lubridate)
library(readxl)

# Robuste Pfad-Ermittlung (funktioniert via Rscript, RStudio und Source)
get_script_dir <- function() {
  if (requireNamespace("this.path", quietly = TRUE)) {
    return(this.path::here())
  }
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  }
  if (dir.exists("data/ZOFE")) {
    return(normalizePath("data/ZOFE"))
  }
  return(getwd())
}

script_dir <- get_script_dir()
here_zofe <- function(...) file.path(script_dir, ...)

script_dir <- get_script_dir()
here_zofe <- function(...) file.path(script_dir, ...)

# Projekt-Root (zwei Ebenen über data/ZOFE)
proj_root <- normalizePath(file.path(script_dir, "../.."))
utils_script <- file.path(proj_root, "scripts/meteo_nmin_utils.R")

if (!file.exists(utils_script)) {
  stop("Konnte scripts/meteo_nmin_utils.R nicht finden unter: ", utils_script)
}
source(utils_script)

# ==============================================================================
# 0. STANDORT- & VERSUCHS-KONFIGURATION
# ==============================================================================
site_config <- list(
  lte          = "ZOFE",
  site_name    = "Reckenholz (Zürich)",
  plz          = 8046,
  latitude     = 47.426805,
  longitude    = 8.518893,
  crop         = "SG",
  base_temp    = get_crop_base_temp("SG")
)

# ==============================================================================
# 2. AGROSCOPE ZOFE-DATEN (Erträge & Nährstoffentzug)
# ==============================================================================




# ==============================================================================
# 2. EEA
# ==============================================================================

## EEA Daten für die Beprobung 2024
eea_file <- here_zofe("raw_data/ZOFE_EEA_2024.xlsx")

df_eea_raw <- read_excel(
  eea_file, sheet = "Results_EEA_24102024", 
  na = "<DL") %>%
    mutate(across(where(is.numeric), ~ round(., 4)))

df_eea_clean <- df_eea_raw |> 
  rename(
    LAP = `LAP (pmol * min-1)`,
    NAG = `NAG (pmol * min-1)`,
    GLS = `GLS (pmol * min-1)`,
    MUP = `MUP (pmol * min-1)`,
    MUX = `MUX (pmol * min-1)`
  ) |> 
  # Aufdröselung der ID
  separate(
    project_sample_id, 
    sep = "-", 
    into = c("BOB","ID")
  ) |>
  separate(
    ID,
    sep = "_",
    into = c("id_plot","id_block","id_treat")
  ) |>
  mutate(
    Versuchsjahr = 2024,
    UID = paste(Versuchsjahr, id_plot, sep = "_"),
    across(c(LAP, NAG, GLS, MUP, MUX, id_plot, id_block, id_treat), as.numeric)
  ) |>
  select(UID, Versuchsjahr, id_plot, id_block, id_treat, BOB, LAP, NAG, GLS, MUP, MUX)

## EEA Daten für die Beprobung 2026

# Daten noch bei DigitSoil anfragen
  
# ==============================================================================
# 3. Bodeneigenschaften
# ============================================================================== 

soil_file <- here_zofe("raw_data/ZOFE_soil_2019-2020.xlsx")
  
df_soil_clean <- read_excel(soil_file) |>
  rename(
    Verfahren = treatment_ID,
    id_plot = plot_nr,
    id_block = replicate,
    id_treat = treatment
  )
  
# ==============================================================================
# 4. Nmin
# ==============================================================================

# ==============================================================================
# 5. MERGE (EEA + Bodeneigenschaften + Nmin)
# ==============================================================================

# 1.
df_final <- full_join(
  df_eea_clean, 
  # df_nmin_clean,
  df_soil_clean,
  by = c("id_plot", "id_block", "id_treat")
)

# ==============================================================================
# 5. DATEN EXPORT
# ==============================================================================

prep_dir <- here_zofe("prep_data")
if (!dir.exists(prep_dir)) {
  dir.create(prep_dir, recursive = TRUE)
}

output_file <- file.path(prep_dir, "ZOFE_2024_cleansed.csv")
message("Exportiere finalen Datensatz nach: ", output_file)
write_csv(df_final, output_file)
write_csv(df_final, file.path(prep_dir, "ZOFE_cleansed.csv"))
saveRDS(df_final, file.path(prep_dir, "ZOFE_2025_cleansed.rds"))
saveRDS(df_final, file.path(prep_dir, "ZOFE_cleansed.rds"))
message("Erfolgreich abgeschlossen! Zeilen: ", nrow(df_final), ", Spalten: ", ncol(df_final))
