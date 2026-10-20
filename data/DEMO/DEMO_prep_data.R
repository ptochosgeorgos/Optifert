# ==============================================================================
# SCRIPT: DEMO_prep_data.R
# ZWECK:  Datenimport, Bereinigung, Merging (Agroscope, Digit Soil, Nmin, Boden)
#         sowie Berechnung von Intervallen, Delta Nmin, Meteo, GDD_soil-basierter
#         Zwei-Pool-Düngerdiskontierung & kulturspezifischem N-Entzug
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
  latitude     = 47.4257,
  longitude    = 8.5165,
  crop         = "MA",
  base_temp    = get_crop_base_temp("MA")
)

# ==============================================================================
# 2. AGROSCOPE DEMO-HISTORIE (Erträge, Nährstoffentzug, Düngergaben & Bilanzen)
# ==============================================================================

yield_file <- here_demo("raw_data/demo89_20_plant_220411.xlsx")
df_demo_raw <- suppressWarnings(readxl::read_xlsx(yield_file, sheet = "Ertrag_89_19"))

df_demo_clean <- df_demo_raw |> 
  select(-contains("Res")) |> 
  mutate(
    Versuchsjahr = as.numeric(Versuchsjahr),
    UID = paste(Versuchsjahr, ParzNrFeld, sep = "_"),
    Kultur = as.character(Kultur),
    Verfahren = as.character(VerfBezeichnung),
    WiederholungNr = as.factor(WiederholungNr),
    Plot_Label = paste(Verfahren, Kultur, WiederholungNr, Versuchsjahr, sep = "-"),
    
    # Saattag als DOY und Datum
    SaatTag_DOY = as.numeric(SaatTag),
    SaatTag_Date = as.Date(SaatTag_DOY, origin = paste0(Versuchsjahr - 1, "-12-31")),
    
    # Detaillierte Mineraldünger-Splits (kg N/ha)
    N_min_1 = as.numeric(`1MinNGabe_kgN_ha`),
    N_min_2 = as.numeric(`2MinNGabe_kgN_ha`),
    N_min_3 = as.numeric(`3MinNGabe_kgN_ha`),
    Total_N_fert_kg_ha = rowSums(cbind(N_min_1, N_min_2, N_min_3), na.rm = TRUE),
    
    # Hofdünger: Mist & Gülle aufgeschlüsselt in NH4-N (fast) und Org-N (slow)
    Mist_NH4_kg_ha    = as.numeric(Mistgabe_t_ha) * as.numeric(NH4NGehaltMist_g_N_kg),
    Mist_OrgN_kg_ha   = as.numeric(Mistgabe_t_ha) * as.numeric(OrgNGehaltMist_g_N_kg),
    Guelle1_NH4_kg_ha = as.numeric(`1GuelleGabe_m3_ha`) * as.numeric(NH4NGehalt1Guelle_kgN_m3),
    Guelle1_OrgN_kg_ha= as.numeric(`1GuelleGabe_m3_ha`) * as.numeric(OrgNGehalt1Guelle_kgN_m3),
    Guelle2_NH4_kg_ha = as.numeric(`2GuelleGabe_m3_ha`) * as.numeric(NH4NGehalt2Guelle_kgN_m3),
    Guelle2_OrgN_kg_ha= as.numeric(`2GuelleGabe_m3_ha`) * as.numeric(OrgNGehalt2Guelle_kgN_m3),
    Guelle3_NH4_kg_ha = as.numeric(`3GuelleGabe_m3_ha`) * as.numeric(NH4NGehalt3Guelle_kgN_m3),
    Guelle3_OrgN_kg_ha= as.numeric(`3GuelleGabe_m3_ha`) * as.numeric(OrgNGehalt3Guelle_kgN_m3),
    
    NGabe_minNO3NH4_total = as.numeric(NGabe_minNO3NH4total_kgN_ha),
    NGabe_Org_total       = as.numeric(NGabe_Orgtotal_kgN_ha),
    NGabe_total           = as.numeric(NGabe_total_kgN_ha),
    OSGabe_total_kg_ha    = as.numeric(OSGabe_total_kgOS_ha),
    
    NBilanz_cum_kg_ha     = as.numeric(NBilanzseitVersBeg_kgN_ha),
    PBilanz_cum_kg_ha     = as.numeric(PBilanzseitVersBeg_kgP_ha),
    KBilanz_cum_kg_ha     = as.numeric(KBilanzseitVersBeg_kgK_ha),
    
    # Kumulative Summenbildung über alle 6 potenziellen Ernten
    Total_Ertrag_HP_TS_kg_a = rowSums(across(matches("Ernte[1-6]_Ertrag_HP_TS_kg_a")), na.rm = TRUE),
    Total_N_Entzug_kg_ha    = rowSums(across(matches("Ernte[1-6]_NEntz_HP_kg_ha")), na.rm = TRUE),
    Total_P_Entzug_kg_ha    = rowSums(across(matches("Ernte[1-6]_PEntz_HP_kg_ha")), na.rm = TRUE),
    Total_OS_Prod_kg_ha     = rowSums(across(matches("Ernte[1-6]_OSprod_HP_kg_ha")), na.rm = TRUE)
  )

# Historische Referenzwerte pro Parzelle für Silomais (MA) und Wintergerste (WG)
df_hist_ma <- df_demo_clean |>
  filter(Kultur == "MA") |>
  group_by(ParzNrFeld) |>
  summarise(
    Avg_N_Entzug_MA   = mean(Total_N_Entzug_kg_ha, na.rm = TRUE),
    Avg_Ertrag_TS_MA  = mean(Total_Ertrag_HP_TS_kg_a, na.rm = TRUE),
    Avg_SaatTag_MA    = mean(SaatTag_DOY, na.rm = TRUE),
    N_min_1_MA        = mean(N_min_1, na.rm = TRUE),
    N_min_2_MA        = mean(N_min_2, na.rm = TRUE),
    Mist_NH4_MA       = mean(Mist_NH4_kg_ha, na.rm = TRUE),
    Mist_OrgN_MA      = mean(Mist_OrgN_kg_ha, na.rm = TRUE),
    Guelle1_NH4_MA    = mean(Guelle1_NH4_kg_ha, na.rm = TRUE),
    Guelle1_OrgN_MA   = mean(Guelle1_OrgN_kg_ha, na.rm = TRUE),
    Guelle2_NH4_MA    = mean(Guelle2_NH4_kg_ha, na.rm = TRUE),
    Guelle2_OrgN_MA   = mean(Guelle2_OrgN_kg_ha, na.rm = TRUE),
    NGabe_min_tot_MA  = mean(NGabe_minNO3NH4_total, na.rm = TRUE),
    NGabe_org_tot_MA  = mean(NGabe_Org_total, na.rm = TRUE),
    OSGabe_tot_MA     = mean(OSGabe_total_kg_ha, na.rm = TRUE),
    .groups = "drop"
  )

df_hist_wg <- df_demo_clean |>
  filter(Kultur == "WG") |>
  group_by(ParzNrFeld) |>
  summarise(
    Avg_N_Entzug_WG   = mean(Total_N_Entzug_kg_ha, na.rm = TRUE),
    Avg_Ertrag_TS_WG  = mean(Total_Ertrag_HP_TS_kg_a, na.rm = TRUE),
    N_min_1_WG        = mean(N_min_1, na.rm = TRUE),
    N_min_2_WG        = mean(N_min_2, na.rm = TRUE),
    N_min_3_WG        = mean(N_min_3, na.rm = TRUE),
    Mist_NH4_WG       = mean(Mist_NH4_kg_ha, na.rm = TRUE),
    Mist_OrgN_WG      = mean(Mist_OrgN_kg_ha, na.rm = TRUE),
    Guelle1_NH4_WG    = mean(Guelle1_NH4_kg_ha, na.rm = TRUE),
    Guelle1_OrgN_WG   = mean(Guelle1_OrgN_kg_ha, na.rm = TRUE),
    Guelle2_NH4_WG    = mean(Guelle2_NH4_kg_ha, na.rm = TRUE),
    Guelle2_OrgN_WG   = mean(Guelle2_OrgN_kg_ha, na.rm = TRUE),
    Guelle3_NH4_WG    = mean(Guelle3_NH4_kg_ha, na.rm = TRUE),
    Guelle3_OrgN_WG   = mean(Guelle3_OrgN_kg_ha, na.rm = TRUE),
    NGabe_min_tot_WG  = mean(NGabe_minNO3NH4_total, na.rm = TRUE),
    NGabe_org_tot_WG  = mean(NGabe_Org_total, na.rm = TRUE),
    OSGabe_tot_WG     = mean(OSGabe_total_kg_ha, na.rm = TRUE),
    .groups = "drop"
  )

df_hist_bal <- df_demo_clean |>
  group_by(ParzNrFeld) |>
  arrange(Versuchsjahr) |>
  summarise(
    NBilanz_cum_kg_ha = last(na.omit(NBilanz_cum_kg_ha)),
    PBilanz_cum_kg_ha = last(na.omit(PBilanz_cum_kg_ha)),
    KBilanz_cum_kg_ha = last(na.omit(KBilanz_cum_kg_ha)),
    .groups = "drop"
  )

# ==============================================================================
# 3. BODENANALYSEN 2025 (DEMO87_soil_2025.csv)
# ==============================================================================
soil_file <- here_demo("raw_data/DEMO87_soil_2025.csv")
soil_lines <- readLines(soil_file, warn = FALSE, encoding = "UTF-8")
soil_data_lines <- soil_lines[15:length(soil_lines)]
soil_data_lines <- soil_data_lines[nzchar(trimws(soil_data_lines))]

soil_col_names <- c(
  "Probenummer", "ParzNrFeld", "Versuch", "Kultur_Produkt", "Material_Form",
  "Block_Kultur_2025", "Verf_Bezeichnung_Soil", "Wiederholung_Soil",
  "soil_Ca_H2O", "soil_Corg", "soil_Humus", "soil_K_CO2", "soil_K_H2O",
  "soil_Mg_CaCl2", "soil_Mg_H2O", "soil_P_AAE10", "soil_P_CO2", "soil_P_H2O",
  "soil_pH_H2O", "soil_Ca_AAE10", "soil_Mg_AAE10", "soil_K_AAE10"
)

df_soil_2025 <- read_delim(
  I(paste(soil_data_lines, collapse = "\n")),
  delim = ";",
  col_names = soil_col_names,
  show_col_types = FALSE
) |>
  mutate(
    ParzNrFeld = as.numeric(ParzNrFeld),
    Kultur_2025 = str_extract(Block_Kultur_2025, "[A-Z]+$"),
    Verf_Soil_Clean = str_remove(Verf_Bezeichnung_Soil, "^'"),
    across(starts_with("soil_"), as.numeric),
    # Labor-Artefakt (0.01 % Corg/Humus in Parzelle 15) als NA setzen und mit Verfahrensmittel imputieren
    soil_Corg  = ifelse(soil_Corg <= 0.1, NA_real_, soil_Corg),
    soil_Humus = ifelse(soil_Humus <= 0.1, NA_real_, soil_Humus)
  ) |>
  group_by(Verf_Soil_Clean) |>
  mutate(
    soil_Corg  = coalesce(soil_Corg, mean(soil_Corg, na.rm = TRUE)),
    soil_Humus = coalesce(soil_Humus, mean(soil_Humus, na.rm = TRUE))
  ) |>
  ungroup() |>
  select(ParzNrFeld, Kultur_2025, starts_with("soil_"))

# ==============================================================================
# 4. DIGIT SOIL EEA-DATEN (Enzymaktivitäten - Jasmins Neuberechnung)
# ==============================================================================

eea_file <- here_demo("raw_data/DEMO87_EEA_2025_Jasmins_recalc.xlsx")
df_eea_raw <- readxl::read_xlsx(eea_file)

df_eea_clean <- df_eea_raw |> 
  rename(
    LAP = LEU,
    NAG = GLA,
    GLS = GLS,
    MUP = PHO,
    MUX = XYL
  ) |> 
  mutate(
    across(c(LAP, NAG, GLS, MUP, MUX), as.numeric),
    ParzNrFeld = parse_number(Plot),
    Probe = str_extract(Plot, "[AB]"),
    Datum = as.Date(Datum),
    Versuchsjahr = year(Datum),
    UID = paste(Versuchsjahr, ParzNrFeld, sep = "_")
  ) |> 
  select(UID, Versuchsjahr, ParzNrFeld, Probe, Datum, BOB, LAP, NAG, GLS, MUP, MUX)

# ==============================================================================
# 5. LABOR-DATEN (Nmin: Ammonium, Nitrat, Schwefel)
# ==============================================================================

nmin_file <- here_demo("raw_data/DEMO87_Nmin_2025.xlsx")
spaltennamen <- names(read_xlsx(nmin_file, sheet = "List", n_max = 0))
df_nmin_raw <- read_xlsx(nmin_file, sheet = "List", skip = 4, col_names = spaltennamen)

df_nmin_clean <- df_nmin_raw |> 
  mutate(across(c(Ammoniumstickstoff, Nitratstickstoff, Schwefel), 
                ~ as.numeric(str_replace(.x, ",", ".")))) |> 
  mutate(
    Parzelle_Roh = str_extract(`Verfahren-Bez,`, "\\d{1,3}[AB]"),
    ParzNrFeld = parse_number(Parzelle_Roh),
    Probe = str_extract(Parzelle_Roh, "[AB]"), 
    Verfahren_Nmin = `Verfahren Nr,`,
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
# 6. MERGE (EEA + Nmin + Boden 2025 + Historische Ertrags-/Düngungsdaten)
# ==============================================================================

df_temporal <- full_join(
  df_eea_clean, 
  df_nmin_clean, 
  by = c("UID", "ParzNrFeld", "Versuchsjahr", "Datum", "Probe")
)

df_merged <- df_temporal |> 
  left_join(df_soil_2025, by = "ParzNrFeld") |>
  left_join(df_hist_ma, by = "ParzNrFeld") |> 
  left_join(df_hist_wg, by = "ParzNrFeld") |>
  left_join(df_hist_bal, by = "ParzNrFeld") |>
  mutate(
    Verfahren = as.factor(Verfahren_Nmin),
    # Kulturspezifische Parameter (SM auf Parzellen 1-8, WG auf Parzellen 9-16; Mais-Baseline als Referenz)
    Kultur = coalesce(Kultur_2025, "SM"),
    Total_N_Entzug_kg_ha = Avg_N_Entzug_MA,
    Total_N_Entzug_actual_crop_kg_ha = ifelse(Kultur == "WG", Avg_N_Entzug_WG, Avg_N_Entzug_MA),
    Total_Ertrag_HP_TS_kg_a = ifelse(Kultur == "WG", Avg_Ertrag_TS_WG, Avg_Ertrag_TS_MA),
    SaatTag_Date = as.Date("2025-04-23"), # Aussaat SM LID 4040C laut DEMO87_management_2025.xlsx
    SaatTag_DOY  = yday(SaatTag_Date),
    Total_N_fert_min_kg_ha = ifelse(Kultur == "WG", NGabe_min_tot_WG, NGabe_min_tot_MA),
    Total_N_fert_org_kg_ha = ifelse(Kultur == "WG", NGabe_org_tot_WG, NGabe_org_tot_MA),
    Total_N_fert_kg_ha     = Total_N_fert_min_kg_ha + Total_N_fert_org_kg_ha,
    Total_OS_fert_kg_ha    = ifelse(Kultur == "WG", OSGabe_tot_WG, OSGabe_tot_MA)
  ) |> 
  select(-any_of(c("Verfahren_Nmin")))

# ==============================================================================
# 7. METEO, ZWEI-POOL-DÜNGERDISKONTIERUNG & INTERVALL-BERECHNUNGEN
# ==============================================================================

all_years <- unique(na.omit(df_merged$Versuchsjahr))
if (length(all_years) == 0) all_years <- 2025

meteo_df <- fetch_meteo_site(
  site_config = site_config,
  start_date  = paste0(min(all_years) - 1, "-07-01"), 
  end_date    = paste0(max(all_years), "-12-31")
)

# Rollierende Wetterfenster berechnen (1d, 3d, 7d, 14d, 30d, 90d=3m, 180d=6m für Niederschlag, GDD & GDD_soil)
meteo_roll <- meteo_df |>
  arrange(Datum) |>
  mutate(
    Precip_1d   = Precip_mm,
    Precip_3d   = zoo::rollsumr(Precip_mm, k = 3, fill = NA),
    Precip_7d   = zoo::rollsumr(Precip_mm, k = 7, fill = NA),
    Precip_14d  = zoo::rollsumr(Precip_mm, k = 14, fill = NA),
    Precip_30d  = zoo::rollsumr(Precip_mm, k = 30, fill = NA),
    Precip_90d  = zoo::rollsumr(Precip_mm, k = 90, fill = NA),
    Precip_180d = zoo::rollsumr(Precip_mm, k = 180, fill = NA),
    GDD_1d      = GDD,
    GDD_3d      = zoo::rollsumr(GDD, k = 3, fill = NA),
    GDD_7d      = zoo::rollsumr(GDD, k = 7, fill = NA),
    GDD_14d     = zoo::rollsumr(GDD, k = 14, fill = NA),
    GDD_30d     = zoo::rollsumr(GDD, k = 30, fill = NA),
    GDD_90d     = zoo::rollsumr(GDD, k = 90, fill = NA),
    GDD_180d    = zoo::rollsumr(GDD, k = 180, fill = NA),
    GDD_soil_1d   = GDD_soil,
    GDD_soil_3d   = zoo::rollsumr(GDD_soil, k = 3, fill = NA),
    GDD_soil_7d   = zoo::rollsumr(GDD_soil, k = 7, fill = NA),
    GDD_soil_14d  = zoo::rollsumr(GDD_soil, k = 14, fill = NA),
    GDD_soil_30d  = zoo::rollsumr(GDD_soil, k = 30, fill = NA),
    GDD_soil_90d  = zoo::rollsumr(GDD_soil, k = 90, fill = NA),
    GDD_soil_180d = zoo::rollsumr(GDD_soil, k = 180, fill = NA),
    CWB_7d  = zoo::rollsumr(CWB_mm, k = 7, fill = NA),
    CWB_14d = zoo::rollsumr(CWB_mm, k = 14, fill = NA),
    CWB_30d = zoo::rollsumr(CWB_mm, k = 30, fill = NA),
    Aridity_Index_14d = Precip_14d / (GDD_14d + 1)
  )

# Exakte Dünge-Events 2025 pro Parzelle aus DEMO87_management_2025.xlsx & Historie aufbauen:
# NH3-Ausbringungsverlust für Hofdünger-NH4 nach GRUD: 15 % (Faktor 0.85)
nh3_retention <- 0.85

fert_events_sm <- df_hist_ma |>
  filter(ParzNrFeld %in% c(1, 4, 5, 7, 8)) |>
  reframe(
    fert_date = as.Date(c("2025-02-20", "2025-05-20", "2025-06-16")),
    N_fast_kgha = c(
      coalesce(Mist_NH4_MA, 0) * nh3_retention,
      coalesce(N_min_1_MA, 0) + coalesce(Guelle1_NH4_MA, 0) * nh3_retention,
      coalesce(N_min_2_MA, 0) + coalesce(Guelle2_NH4_MA, 0) * nh3_retention
    ),
    N_slow_kgha = c(
      coalesce(Mist_OrgN_MA, 0),
      coalesce(Guelle1_OrgN_MA, 0),
      coalesce(Guelle2_OrgN_MA, 0)
    ),
    .by = ParzNrFeld
  )

fert_events_wg <- df_hist_wg |>
  filter(ParzNrFeld %in% c(9, 12, 13, 15, 16)) |>
  reframe(
    # WG Düngetermine laut DEMO87_management_2025.xlsx: 20.02. (Mist), 26.02. (1. Gabe), 27.03. (2. Gabe), 17.04. (3. Gabe), plus 04.09. (KW Neu)
    fert_date = as.Date(c("2025-02-20", "2025-02-26", "2025-03-27", "2025-04-17", "2025-09-04")),
    N_fast_kgha = c(
      coalesce(Mist_NH4_WG, 0) * nh3_retention,
      coalesce(N_min_1_WG, 0) + coalesce(Guelle1_NH4_WG, 0) * nh3_retention,
      coalesce(N_min_2_WG, 0) + coalesce(Guelle2_NH4_WG, 0) * nh3_retention,
      coalesce(N_min_3_WG, 0) + coalesce(Guelle3_NH4_WG, 0) * nh3_retention,
      ifelse(ParzNrFeld %in% c(15, 16), 30, 0) # Ansaatdüngung KW Neu am 04.09.2025
    ),
    N_slow_kgha = c(
      coalesce(Mist_OrgN_WG, 0),
      coalesce(Guelle1_OrgN_WG, 0),
      coalesce(Guelle2_OrgN_WG, 0),
      coalesce(Guelle3_OrgN_WG, 0),
      ifelse(ParzNrFeld == 16, 25, 0)
    ),
    .by = ParzNrFeld
  )

fert_events_2025 <- bind_rows(fert_events_sm, fert_events_wg) |>
  filter(N_fast_kgha > 0 | N_slow_kgha > 0)

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
  add_fertilizer_discounts(
    fert_events = fert_events_2025,
    meteo_df = meteo_df,
    join_col = "ParzNrFeld",
    t1_col = "t1",
    t2_col = "t2"
  ) |>
  left_join(meteo_roll, by = "Datum") |>
  mutate(
    # Kulturspezifischer Pflanzenentzug (SM: Mais-S-Kurve; WG: bis Ernte am 24.06.2025 aktiv, danach Stoppel/KW-Aufgang)
    Interval_N_Entzug_actual_kg_ha = case_when(
      Kultur == "SM" ~ Interval_N_Entzug_kg_ha,
      Kultur == "WG" & t2 <= as.Date("2025-06-25") ~ Total_N_Entzug_actual_crop_kg_ha * 0.22, # Kornfüllungsphase Mai-Juni
      Kultur == "WG" & t1 >= as.Date("2025-08-01") ~ 12.0, # Jungpflanzenaufnahme KW Neu (Saat 13.08.)
      Kultur == "WG" ~ 0.0, # Stoppelphase nach Ernte (24.06. - 07.08.)
      TRUE ~ Interval_N_Entzug_kg_ha
    ),
    # Physikalische Massenbilanz dN (in kg N/ha für 0-30 cm Bodenschicht, Faktor 4.5):
    # 1. dN_raw: Bisherige Formulierung (ohne Dünger-Abzug, reine Mais-Referenzkurve)
    dN_raw  = Delta_Nmin * 4.5 + Interval_N_Entzug_kg_ha,
    # 2. dN_fert_adj: Mit Abzug des GDD_soil-diskontierten mineralischen Düngerpulses (Delta F_fast)
    dN_fert_adj = Delta_Nmin * 4.5 + Interval_N_Entzug_kg_ha - Fert_N_fast_Interval,
    # 3. dN_full_mech: Kulturspezifischer Entzug (SM vs. WG) UND GDD_soil-diskontierter Düngerabzug (Delta F_fast)
    dN_full_mech = Delta_Nmin * 4.5 + Interval_N_Entzug_actual_kg_ha - Fert_N_fast_Interval,
    
    LTE = site_config$lte,
    # Standardisiertes Spaltenschema für versuchsübergreifendes Pooling
    date = Datum,
    year = Versuchsjahr,
    plot_nr = ParzNrFeld,
    treatment = Verfahren,
    crop = Kultur,
    rep = Probe,
    Ntot = Nmin_Total,
    delta_nmin = Delta_Nmin,
    yield = Total_Ertrag_HP_TS_kg_a,
    yield_type = "total_dm",
    yield_unit = "kg_ts_per_a"
  )

# ==============================================================================
# 8. DATEN EXPORT
# ==============================================================================

prep_dir <- here_demo("prep_data")
if (!dir.exists(prep_dir)) {
  dir.create(prep_dir, recursive = TRUE)
}

output_file <- file.path(prep_dir, "DEMO_2025_cleansed.rds")
message("Exportiere finalen Datensatz nach: ", output_file)
saveRDS(df_final, output_file)
saveRDS(df_final, file.path(prep_dir, "DEMO_cleansed.rds"))
write_csv(df_final, file.path(prep_dir, "DEMO_cleansed.csv"))
message("Erfolgreich abgeschlossen! Zeilen: ", nrow(df_final), ", Spalten: ", ncol(df_final))
