# ==============================================================================
# SCRIPT: ZOFE_prep_data.R
# ZWECK:  Datenimport, Bereinigung, Merging (Agroscope, Digit Soil, Ertrag 2024,
#         Bodeneigenschaften 2019/2020 & 2025 (0-20 / 20-30 cm), Open-Meteo)
# ==============================================================================

# 1. Pakete & Pfad-Setup
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(readxl)
})

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
  base_temp    = get_crop_base_temp("SG") # 0.0 °C (Porter & Gawith, 1999; Bonhomme, 2000)
)

# ==============================================================================
# 2. EEA DATEN (Digit Soil Beprobung Herbst 2024: 24.10.2024, 60 Parzellen)
# ==============================================================================
eea_file <- here_zofe("raw_data/ZOFE_EEA_2024.xlsx")

df_eea_raw <- read_excel(
  eea_file, sheet = "Results_EEA_24102024",
  na = "<DL"
) |>
  mutate(across(where(is.numeric), ~ round(., 4)))

df_eea_clean <- df_eea_raw |>
  rename(
    LAP = `LAP (pmol * min-1)`,
    NAG = `NAG (pmol * min-1)`,
    GLS = `GLS (pmol * min-1)`,
    MUP = `MUP (pmol * min-1)`,
    MUX = `MUX (pmol * min-1)`
  ) |>
  separate(project_sample_id, sep = "-", into = c("BOB", "ID")) |>
  separate(ID, sep = "_", into = c("id_plot", "id_block", "id_treat")) |>
  mutate(
    Versuchsjahr = 2024,
    Datum = as.Date("2024-10-24"),
    UID = paste(Versuchsjahr, id_plot, sep = "_"),
    across(c(LAP, NAG, GLS, MUP, MUX, id_plot, id_block, id_treat), as.numeric)
  ) |>
  select(UID, Versuchsjahr, Datum, id_plot, id_block, id_treat, BOB, LAP, NAG, GLS, MUP, MUX)

# ==============================================================================
# 3. BODENEIGENSCHAFTEN 2019-2020 (Deduplizierung über 2019 & 2020 -> 60 Parzellen)
# ==============================================================================
soil_file_1920 <- here_zofe("raw_data/ZOFE_soil_2019-2020.xlsx")

df_soil_1920 <- read_excel(soil_file_1920) |>
  rename(
    Verfahren = treatment_ID,
    id_plot   = plot_nr,
    id_block  = replicate,
    id_treat  = treatment
  ) |>
  group_by(id_plot, id_block, id_treat, Verfahren, treatment_en) |>
  summarise(
    across(starts_with("soil_0_20_"), ~ mean(as.numeric(.x), na.rm = TRUE)),
    .groups = "drop"
  )

# ==============================================================================
# 4. BODENEIGENSCHAFTEN 2025 (0-20 cm & 20-30 cm aus Agroscope LIMS)
# ==============================================================================
read_zofe_lims_soil <- function(csv_path, prefix) {
  lines <- read_lines(csv_path, locale = locale(encoding = "latin1"))
  df_raw <- read_delim(
    I(lines[c(11, 15:length(lines))]),
    delim = ";",
    show_col_types = FALSE,
    name_repair = "unique"
  )
  df_raw |>
    transmute(
      id_plot = as.numeric(`Probe/Parzelle`),
      !!paste0(prefix, "_Ctot")    := as.numeric(Kohlenstoff),
      !!paste0(prefix, "_Corg")    := as.numeric(Corg),
      !!paste0(prefix, "_Humus")   := as.numeric(Humus),
      !!paste0(prefix, "_Ntot")    := as.numeric(Stickstoff),
      !!paste0(prefix, "_CN")      := as.numeric(Corg) / pmax(as.numeric(Stickstoff), 1e-4),
      !!paste0(prefix, "_pH_H2O")  := as.numeric(`pH-Wert`),
      !!paste0(prefix, "_Sand")    := as.numeric(Sand),
      !!paste0(prefix, "_Schluff") := as.numeric(Schluff),
      !!paste0(prefix, "_Ton")     := as.numeric(Ton),
      !!paste0(prefix, "_KAK")     := as.numeric(`potentielle KAK`),
      !!paste0(prefix, "_BS")      := as.numeric(Basensättigung),
      !!paste0(prefix, "_P_AAE10") := as.numeric(`Phosphor...24`),
      !!paste0(prefix, "_P_CO2")   := as.numeric(`Phosphor...25`),
      !!paste0(prefix, "_K_CO2")   := as.numeric(`Kalium...15`),
      !!paste0(prefix, "_K_AAE10") := as.numeric(`Kalium...35`),
      !!paste0(prefix, "_Mg_AAE10"):= as.numeric(`Magnesium...34`),
      !!paste0(prefix, "_Ca_AAE10"):= as.numeric(`Calcium...33`)
    ) |>
    filter(!is.na(id_plot))
}

df_soil_25_0_20  <- read_zofe_lims_soil(here_zofe("raw_data/ZOFE_soil_2025_0-20cm.csv"), "soil25_0_20")
df_soil_25_20_30 <- read_zofe_lims_soil(here_zofe("raw_data/ZOFE_soil_2025_20-30cm.csv"), "soil25_20_30")

df_soil_25 <- df_soil_25_0_20 |>
  left_join(df_soil_25_20_30, by = "id_plot") |>
  mutate(
    # Tiefengewichtetes 0-30 cm Mittel (2/3 * 0-20 cm + 1/3 * 20-30 cm)
    soil_Corg    = (2/3) * soil25_0_20_Corg    + (1/3) * coalesce(soil25_20_30_Corg, soil25_0_20_Corg),
    soil_Ntot    = (2/3) * soil25_0_20_Ntot    + (1/3) * coalesce(soil25_20_30_Ntot, soil25_0_20_Ntot),
    soil_CN      = soil_Corg / pmax(soil_Ntot, 1e-4),
    soil_pH_H2O  = (2/3) * soil25_0_20_pH_H2O  + (1/3) * coalesce(soil25_20_30_pH_H2O, soil25_0_20_pH_H2O),
    soil_Ton     = (2/3) * soil25_0_20_Ton     + (1/3) * coalesce(soil25_20_30_Ton, soil25_0_20_Ton),
    soil_Schluff = (2/3) * soil25_0_20_Schluff + (1/3) * coalesce(soil25_20_30_Schluff, soil25_0_20_Schluff),
    soil_Sand    = (2/3) * soil25_0_20_Sand    + (1/3) * coalesce(soil25_20_30_Sand, soil25_0_20_Sand),
    soil_KAK     = (2/3) * soil25_0_20_KAK     + (1/3) * coalesce(soil25_20_30_KAK, soil25_0_20_KAK),
    soil_P_AAE10 = (2/3) * soil25_0_20_P_AAE10 + (1/3) * coalesce(soil25_20_30_P_AAE10, soil25_0_20_P_AAE10),
    soil_K_AAE10 = (2/3) * soil25_0_20_K_AAE10 + (1/3) * coalesce(soil25_20_30_K_AAE10, soil25_0_20_K_AAE10)
  )

# ==============================================================================
# 5. SOMMERGERSTE ERTRAGS- & N-ENTZUGSDATEN 2024 (ZOFE_yield-SG_2024.xlsx)
# ==============================================================================
yield_file_24 <- here_zofe("raw_data/ZOFE_yield-SG_2024.xlsx")

df_yield_24 <- read_excel(yield_file_24, sheet = "Sheet") |>
  transmute(
    id_plot       = as.numeric(Plot),
    Kultur        = as.character(crop_abr),
    Sorte         = as.character(variety),
    SaatTag_Date  = as.Date(sow_date),
    ErnteTag_Date = as.Date(harv1_date),
    # Mähdrusch-Parzellenertrag (harv1_mp_FM in kg auf harv1_mp_area = 7.71 m², harv1_mp_DM in %)
    # Umrechnung auf dt TS/ha (1 dt/ha = 100 kg/ha = 0.01 kg/m² -> Faktor 100 / area)
    Ertrag_Korn_TS_dt_ha = (harv1_mp_FM * (harv1_mp_DM / 100) / harv1_mp_area) * 100,
    # Handernte Korn/Stroh-Verhältnis (Ernteindex HI) zur Ableitung des Strohertrags
    HI_hand              = (hlm1_grain_FM * (hlm1_grain_DM / 100)) /
                           pmax((hlm1_grain_FM * (hlm1_grain_DM / 100)) + (hlm1_straw_FM * (hlm1_straw_DM / 100)), 1e-4),
    Ertrag_Stroh_TS_dt_ha = Ertrag_Korn_TS_dt_ha * ((1 - HI_hand) / pmax(HI_hand, 0.1)),
    TKG_g                = as.numeric(harv1_TKW_FM),
    HLG_kg_hl            = as.numeric(harv1_HLW_FM),
    Aehren_m2            = as.numeric(hlm1_ear_nr) / as.numeric(hlm_area),
    Protein_pct          = as.numeric(protein_gehalt),
    # Korn-N-Gehalt (%) = Rohprotein (%) / 6.25 (Gerste); Stroh-N-Gehalt ~ 0.50 % TS (GRUD 2017)
    N_Entzug_Korn_kg_ha  = Ertrag_Korn_TS_dt_ha * 100 * ((Protein_pct / 6.25) / 100),
    N_Entzug_Stroh_kg_ha = Ertrag_Stroh_TS_dt_ha * 100 * 0.0050,
    Total_N_Entzug_kg_ha = N_Entzug_Korn_kg_ha + N_Entzug_Stroh_kg_ha,
    # Langjährige Düngungsregime-Klassifikation im ZOFE (GRUD / Oberholzer et al. 2014)
    Duengungstyp = case_when(
      ENTRY == 1 ~ "Ungedüngt (K0)",
      ENTRY == 10 ~ "Mineralisch ohne N (PK)",
      ENTRY %in% c(2, 3, 4) ~ "Rein Organisch (Mist/Kompost/Agrosan)",
      ENTRY %in% c(6, 7, 8, 9) ~ "Organisch-Mineralisch (+PKN)",
      ENTRY %in% c(5, 11, 12) ~ "Rein Mineralisch (NPK)"
    )
  )

# ==============================================================================
# 6. PEDOKLIMATISCHE DATEN (Open-Meteo API für 2024)
# ==============================================================================
df_meteo_24 <- fetch_meteo_site(site_config, "2024-01-01", "2024-12-31")

# ==============================================================================
# 7. MERGE & EXPORT (Exakt 60 Parzellen, keine Duplikate!)
# ==============================================================================
df_final <- df_eea_clean |>
  left_join(df_soil_1920, by = c("id_plot", "id_block", "id_treat")) |>
  left_join(df_soil_25, by = "id_plot") |>
  left_join(df_yield_24, by = "id_plot") |>
  left_join(df_meteo_24, by = "Datum") |>
  mutate(
    LTE       = "ZOFE",
    date      = as.character(Datum),
    year      = Versuchsjahr,
    plot_nr   = id_plot,
    treatment = Verfahren,
    crop      = coalesce(Kultur, "SG"),
    rep       = as.character(id_block),
    yield     = Ertrag_Korn_TS_dt_ha,
    yield_type = "grain_dry_matter",
    yield_unit = "dt/ha"
  ) |>
  arrange(id_plot)

prep_dir <- here_zofe("prep_data")
if (!dir.exists(prep_dir)) {
  dir.create(prep_dir, recursive = TRUE)
}

output_file <- file.path(prep_dir, "ZOFE_2024_cleansed.csv")
write_csv(df_final, output_file)
write_csv(df_final, file.path(prep_dir, "ZOFE_cleansed.csv"))
saveRDS(df_final, file.path(prep_dir, "ZOFE_2025_cleansed.rds"))
saveRDS(df_final, file.path(prep_dir, "ZOFE_cleansed.rds"))
message(sprintf("[ZOFE_prep_data] Erfolgreich abgeschlossen! Zeilen: %d (exakt 60 Parzellen), Spalten: %d", nrow(df_final), ncol(df_final)))
