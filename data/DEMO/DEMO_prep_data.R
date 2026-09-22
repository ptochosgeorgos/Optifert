# ==============================================================================
# SCRIPT: DEMO_prep_data.R
# ZWECK:  Datenimport, Bereinigung, Merging (Agroscope, Digit Soil, Nmin)
#         sowie Berechnung von Intervallen, Delta Nmin, Meteo & N-Entzug
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
  if (dir.exists("data/DEMO")) {
    return(normalizePath("data/DEMO"))
  }
  return(getwd())
}

script_dir <- get_script_dir()
here_demo <- function(...) file.path(script_dir, ...)

# Projekt-Root (zwei Ebenen über data/DEMO)
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
  lte          = "DEMO",
  site_name    = "Reckenholz (Zürich)",
  plz          = 8046,
  latitude     = 47.428,
  longitude    = 8.520,
  crop         = "MA",
  base_temp    = 6
)

# ==============================================================================
# 2. AGROSCOPE DEMO-DATEN (Erträge & Nährstoffentzug)
# ==============================================================================

yield_file <- here_demo("raw_data/demo89_20_plant_220411.xlsx")
df_demo_raw <- readxl::read_xlsx(yield_file, sheet = "Ertrag_89_19")

df_demo_clean <- df_demo_raw |> 
  select(-contains("Res")) |> 
  mutate(
    Versuchsjahr = as.numeric(Versuchsjahr),
    UID = paste(Versuchsjahr, ParzNrFeld, sep = "_"),
    Kultur = as.factor(Kultur),
    Verfahren = as.factor(VerfBezeichnung),
    WiederholungNr = as.factor(WiederholungNr),
    Plot_Label = paste(Verfahren, Kultur, WiederholungNr, Versuchsjahr, sep = "-"),
    
    # Saattag als DOY und Datum
    SaatTag_DOY = as.numeric(SaatTag),
    SaatTag_Date = as.Date(SaatTag_DOY, origin = paste0(Versuchsjahr - 1, "-12-31")),
    
    # N-Düngung
    N_fert_1 = as.numeric(`1MinNGabe_kgN_ha`),
    N_fert_2 = as.numeric(`2MinNGabe_kgN_ha`),
    Total_N_fert_kg_ha = rowSums(cbind(N_fert_1, N_fert_2), na.rm = TRUE),
    
    # Kumulative Summenbildung über alle 6 potenziellen Ernten
    Total_Ertrag_HP_TS_kg_a = rowSums(across(matches("Ernte[1-6]_Ertrag_HP_TS_kg_a")), na.rm = TRUE),
    Total_N_Entzug_kg_ha    = rowSums(across(matches("Ernte[1-6]_NEntz_HP_kg_ha")), na.rm = TRUE),
    Total_P_Entzug_kg_ha    = rowSums(across(matches("Ernte[1-6]_PEntz_HP_kg_ha")), na.rm = TRUE),
    Total_OS_Prod_kg_ha     = rowSums(across(matches("Ernte[1-6]_OSprod_HP_kg_ha")), na.rm = TRUE)
  ) |> 
  select(
    UID, Plot_Label, Versuchsjahr, ParzNrFeld, WiederholungNr,
    Kultur, Verfahren, SaatTag_DOY, SaatTag_Date, Total_N_fert_kg_ha,
    Total_Ertrag_HP_TS_kg_a, 
    Total_N_Entzug_kg_ha, 
    Total_P_Entzug_kg_ha, 
    Total_OS_Prod_kg_ha
  )

# Berechne historische Mittelwerte für Mais (MA) pro Parzelle (für Jahre ohne vorliegende Erntemessung)
df_yield_avg <- df_demo_clean |>
  filter(Kultur == "MA") |>
  group_by(ParzNrFeld) |>
  summarise(
    Avg_N_Entzug = mean(Total_N_Entzug_kg_ha, na.rm = TRUE),
    Avg_SaatTag  = mean(SaatTag_DOY, na.rm = TRUE),
    .groups = "drop"
  )

# ==============================================================================
# 3. DIGIT SOIL EEA-DATEN (Enzymaktivitäten)
# ==============================================================================

eea_file <- here_demo("raw_data/eea_report_LTE_2025_basic_analysis.CSV")
df_eea_raw <- read_csv2(eea_file, show_col_types = FALSE)

df_eea_clean <- df_eea_raw |> 
  # Fehlertexte in NA wandeln, als numerisch deklarieren, negative Werte nullen
  mutate(across(LAP:MUX, ~na_if(as.character(.), "inv Samp"))) |> 
  mutate(across(LAP:MUX, as.numeric)) |> 
  mutate(across(LAP:MUX, ~ifelse(!is.na(.) & . < 0, 0, .))) |> 
  
  # Regex-Extraktion für ID und Datum
  mutate(
    Parzelle_Roh = str_extract(project_sample_id, "\\d{1,3}[AB]"),
    ParzNrFeld = parse_number(Parzelle_Roh),
    Probe = str_extract(Parzelle_Roh, "[AB]"), # Replikat A oder B
    
    Datum_Text = str_extract(project_sample_id, "\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}"),
    Datum = dmy(Datum_Text), 
    
    # Fehlende Daten mit Standarddatum auffüllen falls nötig
    Datum = replace_na(Datum, as.Date("2025-09-11")),
    Versuchsjahr = year(Datum),
    UID = paste(Versuchsjahr, ParzNrFeld, sep = "_")
  ) |> 
  select(UID, Versuchsjahr, ParzNrFeld, Probe, Datum, LAP, NAG, GLS, MUP, MUX, Comments)

# ==============================================================================
# 4. LABOR-DATEN (Nmin: Ammonium, Nitrat, Schwefel)
# ==============================================================================

nmin_file <- here_demo("raw_data/DEMO_Nmin.xlsx")
spaltennamen <- names(read_xlsx(nmin_file, sheet = "List", n_max = 0))
df_nmin_raw <- read_xlsx(nmin_file, sheet = "List", skip = 4, col_names = spaltennamen)

df_nmin_clean <- df_nmin_raw |> 
  # Kommas durch Punkte ersetzen
  mutate(across(c(Ammoniumstickstoff, Nitratstickstoff, Schwefel), 
                ~ as.numeric(str_replace(.x, ",", ".")))) |> 
  mutate(
    Parzelle_Roh = str_extract(`Verfahren-Bez,`, "\\d{1,3}[AB]"),
    ParzNrFeld = parse_number(Parzelle_Roh),
    Probe = str_extract(Parzelle_Roh, "[AB]"), 
    Verfahren_Nmin = `Verfahren Nr,`, # Fallback für die Behandlungs-Labels
    Datum = as.Date(Datum),
    Versuchsjahr = year(Datum),
    UID = paste(Versuchsjahr, ParzNrFeld, sep = "_")
  ) |> 
  rename(NH4 = Ammoniumstickstoff, NO3 = Nitratstickstoff, S = Schwefel) |> 
  group_by(UID, ParzNrFeld, Versuchsjahr, Datum, Probe, Verfahren_Nmin) |> 
  summarise(
    across(c(NH4, NO3, S), ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# Datum-Harmonisierung: Nmin-Datum an das nächste EEA-Datum anpassen (Toleranz: max. 3 Tage)
eea_unique_dates <- unique(df_eea_clean$Datum)
df_nmin_clean <- df_nmin_clean |>
  mutate(Datum = map_vec(Datum, function(d) {
    if (length(eea_unique_dates) == 0) return(d)
    closest <- eea_unique_dates[which.min(abs(eea_unique_dates - d))]
    if (abs(as.numeric(closest - d)) <= 3) return(closest) else return(d)
  })) |>
  mutate(Datum = as.Date(Datum))

# ==============================================================================
# 5. DREI-WEGE-MERGE (Ertrag + EEA + Nmin)
# ==============================================================================

# 1. Temporaler Merge INKLUSIVE 'Probe' (A/B)
df_temporal <- full_join(
  df_eea_clean, 
  df_nmin_clean, 
  by = c("UID", "ParzNrFeld", "Versuchsjahr", "Datum", "Probe")
)

# 2. Finaler Merge mit Ertrags- und historischen Durchschnittswerten
df_merged <- df_temporal |> 
  left_join(df_demo_clean, by = c("UID", "ParzNrFeld", "Versuchsjahr")) |> 
  left_join(df_yield_avg, by = "ParzNrFeld") |> 
  mutate(
    Verfahren = coalesce(Verfahren, as.factor(Verfahren_Nmin)),
    Total_N_Entzug_kg_ha = coalesce(Total_N_Entzug_kg_ha, Avg_N_Entzug),
    SaatTag_DOY = coalesce(SaatTag_DOY, Avg_SaatTag),
    SaatTag_Date = as.Date(SaatTag_DOY, origin = paste0(Versuchsjahr - 1, "-12-31"))
  ) |> 
  select(-any_of(c("Verfahren_Nmin", "Avg_N_Entzug", "Avg_SaatTag")))

# ==============================================================================
# 6. METEO & BERECHNUNGEN (Intervalle, Delta Nmin, GDD, N-Entzug)
# ==============================================================================

# Dynamische Jahreserkennung (z.B. für 2025 und künftig 2026)
all_years <- unique(na.omit(df_merged$Versuchsjahr))
if (length(all_years) == 0) all_years <- 2025

# Wetterdaten für Standort via site_config über alle relevanten Jahre abrufen
meteo_df <- map_dfr(all_years, function(yr) {
  fetch_meteo_site(
    site_config = site_config,
    start_date  = paste0(yr, "-01-01"), 
    end_date    = paste0(yr, "-12-31")
  )
})

# Anwenden der modularen Feature-Engineering Funktionen
df_final <- df_merged |> 
  add_intervals_and_delta_nmin(
    group_vars = c("UID", "Probe"), 
    date_col = "Datum", 
    saattag_col = "SaatTag_Date",
    nh4_col = "NH4", 
    no3_col = "NO3"
  ) |> 
  add_meteo_intervals(
    meteo_df = meteo_df, 
    t1_col = "t1", 
    t2_col = "t2", 
    saattag_col = "SaatTag_Date"
  ) |> 
  add_maize_n_uptake(
    total_n_entzug_col = "Total_N_Entzug_kg_ha",
    gdd_t1_col = "Cum_GDD_t1",
    gdd_t2_col = "Cum_GDD_t2"
  ) |> 
  mutate(
    LTE = site_config$lte,
    # Standardisiertes Spaltenschema
    date = Datum,
    year = Versuchsjahr,
    plot_nr = ParzNrFeld,
    treatment = Verfahren,
    crop = Kultur,
    rep = Probe,
    Ntot = Nmin_Total,
    delta_nmin = Delta_Nmin,
    yield = Total_Ertrag_HP_TS_kg_a,
    yield_type = "total_hp_ts",
    yield_unit = "kg/a"
  )

# ==============================================================================
# 7. DATEN EXPORT
# ==============================================================================

prep_dir <- here_demo("prep_data")
if (!dir.exists(prep_dir)) {
  dir.create(prep_dir, recursive = TRUE)
}

output_file <- file.path(prep_dir, "DEMO_2025_cleansed.csv")
message("Exportiere finalen Datensatz nach: ", output_file)
write_csv(df_final, output_file)
write_csv(df_final, file.path(prep_dir, "DEMO_cleansed.csv"))
saveRDS(df_final, file.path(prep_dir, "DEMO_cleansed.rds"))
message("Erfolgreich abgeschlossen! Zeilen: ", nrow(df_final), ", Spalten: ", ncol(df_final))
