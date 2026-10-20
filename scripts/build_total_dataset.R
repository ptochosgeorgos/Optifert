#!/usr/bin/env Rscript
# ==============================================================================
# SCRIPT: build_total_dataset.R
# ZWECK:  Erstellt den versuchsübergreifenden, harmonisierten Intervall-Datensatz
#         (DEMO + Lysimeter + Fields25) mit einheitlicher GDD_soil-basierter
#         Zwei-Pool-Düngerdiskontierung (T_base,soil = 0 °C, 0-28 cm),
#         kulturspezifischem Pflanzenentzug und harmonisierter Massenbilanz dN.
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(rrcov)
})

source("scripts/meteo_nmin_utils.R")

# ------------------------------------------------------------------------------
# 1. DEMO 2025 laden (bereits durch DEMO_prep_data.R mit Boden & Düngung angereichert)
# ------------------------------------------------------------------------------
df_demo <- readRDS("data/DEMO/prep_data/DEMO_2025_cleansed.rds") |>
  filter(!is.na(t1), !is.na(t2), t2 > t1) |>
  mutate(
    LTE           = "DEMO",
    plot_id       = paste0("DEMO_", ParzNrFeld),
    sub_id        = paste0("DEMO_", ParzNrFeld, "_", Probe),
    plot_nr       = as.numeric(ParzNrFeld),
    rep           = as.character(Probe),
    treatment     = as.character(Verfahren),
    crop          = as.character(Kultur),
    interval_days = as.numeric(t2 - t1),
    Delta_Nmin_kgha = Delta_Nmin * 4.5,
    Interval_N_Entzug_kgha = Interval_N_Entzug_kg_ha,
    dN_raw        = dN_raw,
    dN_mech       = dN_fert_adj
  )

# ------------------------------------------------------------------------------
# 2. LYSIMETER 2025 (Winterweizen WW, 32 Lysimeter) harmonisieren
# ------------------------------------------------------------------------------
lysi_site <- list(
  lte       = "Lysimeter",
  latitude  = 47.427,
  longitude = 8.519,
  crop      = "WW",
  base_temp = get_crop_base_temp("WW") # 0.0 °C (Porter & Gawith, 1999)
)

meteo_lysi <- fetch_meteo_site(lysi_site, "2024-10-01", "2025-12-31")

lysi_raw <- readRDS("data/Lysimeter/prep_data/Lysimeter_cleansed.rds") |>
  filter(year == 2025, treatment %in% c("null", "conventional", "optimized", "inhibitor")) |>
  mutate(
    Datum = as.Date(date),
    SaatTag_Date = as.Date("2024-10-15") # Aussaat Winterweizen Herbst 2024
  )

lysi_yield <- readRDS("data/Lysimeter/prep_data/Lysimeter_crop_yield.rds") |>
  mutate(
    # Lysimeter-Fläche: r = 0.3 m (oder 1 m² Standard); Umrechnung Korn + Stroh N-Entzug auf kg N/ha
    # Typischer Weizen-N-Entzug im Lysimeter: proportional zum Gesamtertrag (Normierung auf ~140 kg N/ha Mittelwert)
    total_dm_g = coalesce(korn_TS, 0) + coalesce(stroh_TS, 0),
    Total_N_Entzug_kg_ha = (total_dm_g / mean(total_dm_g, na.rm = TRUE)) * 135.0
  ) |>
  select(id_lysi, Total_N_Entzug_kg_ha)

# WW-Düngungstermine 2025 im Lysimeter (3 mineralische/stabilisierte Gaben im Frühjahr)
lysi_fert_events <- lysi_raw |>
  distinct(id_lysi, treatment) |>
  reframe(
    fert_date = as.Date(c("2025-03-14", "2025-04-15", "2025-05-20")),
    N_fast_kgha = if (treatment == "conventional") {
      c(60, 50, 40)
    } else if (treatment == "optimized") {
      c(50, 45, 35)
    } else if (treatment == "inhibitor") {
      c(55, 55, 0)
    } else {
      c(0, 0, 0)
    },
    N_slow_kgha = c(0, 0, 0),
    .by = c(id_lysi, treatment)
  ) |>
  filter(N_fast_kgha > 0)

df_lysi <- lysi_raw |>
  left_join(lysi_yield, by = "id_lysi") |>
  add_intervals_and_delta_nmin(
    group_vars = "id_lysi",
    date_col = "Datum",
    saattag_col = "SaatTag_Date",
    nh4_col = "NH4",
    no3_col = "NO3"
  ) |>
  filter(!is.na(t1), !is.na(t2), t2 > t1, !is.na(Delta_Nmin)) |>
  add_meteo_intervals(
    meteo_df = meteo_lysi,
    t1_col = "t1",
    t2_col = "t2",
    saattag_col = "SaatTag_Date"
  ) |>
  # Winterweizen logistische N-Aufnahme (T_base = 0 °C, Wendepunkt ~ 950 GDD_0 ab Saat bzw. Schossen/Ährenschieben)
  add_maize_n_uptake(
    total_n_entzug_col = "Total_N_Entzug_kg_ha",
    gdd_t1_col = "Cum_GDD_t1",
    gdd_t2_col = "Cum_GDD_t2",
    k = 0.006,
    x0 = 1150
  ) |>
  add_fertilizer_discounts(
    fert_events = lysi_fert_events,
    meteo_df = meteo_lysi,
    join_col = "id_lysi",
    t1_col = "t1",
    t2_col = "t2"
  ) |>
  mutate(
    LTE             = "Lysimeter",
    plot_id         = paste0("LYSI_", id_lysi),
    sub_id          = paste0("LYSI_", id_lysi),
    plot_nr         = as.numeric(id_lysi),
    rep             = "A",
    treatment       = as.character(treatment),
    crop            = "WW",
    interval_days   = as.numeric(t2 - t1),
    # Corg Proxy für Lysimeter Bodenart (low ~ 1.1 %, high ~ 2.1 %)
    soil_Corg       = ifelse(soil_type == "high", 2.10, 1.15),
    soil_pH_H2O     = ifelse(soil_type == "high", 6.80, 6.50),
    Delta_Nmin_kgha = Delta_Nmin * 4.5,
    Interval_N_Entzug_kgha = coalesce(Interval_N_Entzug_kg_ha, 0),
    dN_raw          = Delta_Nmin_kgha + Interval_N_Entzug_kgha,
    dN_mech         = dN_raw - Fert_N_fast_Interval
  )

# ------------------------------------------------------------------------------
# 3. FIELDS25 (On-Farm Praxisversuche, 0-30 cm Schicht) harmonisieren
# ------------------------------------------------------------------------------
fc_raw <- readRDS("data/Fields25/prep_data/Fields25_cleansed.rds") |>
  filter(depth == "30", treatment %in% c("null", "ueblich", "empfohlen")) |>
  mutate(
    PLZ   = as.numeric(PLZ),
    Datum = as.Date(calendar_date),
    SaatTag_Date = as.Date("2025-05-01")
  ) |>
  # Mittelwert über techn. Replikate pro Betrieb, Behandlung und Termin
  group_by(plot_nr, PLZ, crop, treatment, Datum, SaatTag_Date) |>
  summarise(
    across(c(LAP, NAG, GLS, MUP, MUX, NH4, NO3, Ntot, yield), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  filter(!is.na(Datum))

fc_soil <- readRDS("data/Fields25/prep_data/Fields25_Soil.rds") |>
  filter(Depth == "30") |>
  group_by(PLZ, Treatment) |>
  summarise(
    soil_Corg = mean(TOC.Var.B, na.rm = TRUE),
    bdensity  = mean(bdensity, na.rm = TRUE),
    .groups = "drop"
  )

fc_fert <- readRDS("data/Fields25/prep_data/Fields25_Fert_Yield.rds") |>
  mutate(
    fert_date = as.Date(date),
    n_qty     = as.numeric(nitrogen_quantity),
    is_org    = str_detect(tolower(coalesce(fertilizer_form, "")), "hofdünger|gülle|mist"),
    N_fast_kgha = ifelse(is_org, n_qty * 0.45 * 0.85, n_qty),
    N_slow_kgha = ifelse(is_org, n_qty * 0.55, 0),
    plot_treat_key = paste(plot_nr, treatment, sep = "_")
  ) |>
  filter(!is.na(fert_date), !is.na(n_qty), n_qty > 0) |>
  select(plot_treat_key, fert_date, N_fast_kgha, N_slow_kgha)

# Verwende Reckenholz-Meteo als schnellen regionalen Mittelland-Anker für die Intervall-Summen
meteo_fc <- fetch_meteo(47.428, 8.520, "2025-01-01", "2025-12-31", base_temp = 6.0)

df_fc <- fc_raw |>
  left_join(fc_soil, by = c("PLZ" = "PLZ", "treatment" = "Treatment")) |>
  mutate(
    plot_treat_key = paste(plot_nr, treatment, sep = "_"),
    Total_N_Entzug_kg_ha = case_when(
      !is.na(yield) & yield > 50 ~ yield * 1.15,
      treatment == "null" ~ 150,
      TRUE ~ 210
    )
  ) |>
  add_intervals_and_delta_nmin(
    group_vars = c("plot_nr", "treatment"),
    date_col = "Datum",
    saattag_col = "SaatTag_Date",
    nh4_col = "NH4",
    no3_col = "NO3"
  ) |>
  filter(!is.na(t1), !is.na(t2), t2 > t1, !is.na(Delta_Nmin)) |>
  add_meteo_intervals(
    meteo_df = meteo_fc,
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
    fert_events = fc_fert,
    meteo_df = meteo_fc,
    join_col = "plot_treat_key",
    t1_col = "t1",
    t2_col = "t2"
  ) |>
  mutate(
    LTE             = "Fields25",
    plot_id         = paste0("FC_", plot_nr),
    sub_id          = paste0("FC_", plot_nr, "_", treatment),
    rep             = "A",
    interval_days   = as.numeric(t2 - t1),
    soil_Corg       = coalesce(soil_Corg, mean(soil_Corg, na.rm = TRUE)),
    soil_pH_H2O     = 6.8,
    Delta_Nmin_kgha = Delta_Nmin * 4.5,
    Interval_N_Entzug_kgha = coalesce(Interval_N_Entzug_kg_ha, 0),
    dN_raw          = Delta_Nmin_kgha + Interval_N_Entzug_kgha,
    dN_mech         = dN_raw - Fert_N_fast_Interval
  )

# ------------------------------------------------------------------------------
# 4. ZUSAMMENFÜHRUNG & ROBUSTE PCA (Global & Trial-spezifisch)
# ------------------------------------------------------------------------------
common_cols <- c(
  "LTE", "plot_id", "sub_id", "plot_nr", "rep", "treatment", "crop",
  "t1", "t2", "interval_days",
  "NAG", "GLS", "MUP", "MUX",
  "NH4", "NO3", "Nmin_Total", "Delta_Nmin", "Delta_Nmin_kgha",
  "Interval_N_Entzug_kgha", "Fert_N_fast_Interval", "Fert_N_slow_Interval", "Fert_N_total_Interval",
  "dN_raw", "dN_mech",
  "soil_Corg", "soil_pH_H2O",
  "Cum_Precip_mm", "Cum_ET0_mm", "Cum_CWB_mm",
  "Cum_GDD_Interval", "Cum_GDD_Soil_Interval",
  "Mean_Soil_Temp_0_7_Interval", "Mean_Soil_Temp_7_28_Interval", "Mean_Soil_Temp_0_28_Interval",
  "Mean_Soil_Moist_0_7_Interval", "Mean_Soil_Moist_7_28_Interval", "Mean_Soil_Moist_0_28_Interval",
  "Mean_VPD_Interval", "Mean_Solar_Rad_Interval"
)

df_total <- bind_rows(
  df_demo |> select(any_of(common_cols)),
  df_lysi |> select(any_of(common_cols)),
  df_fc   |> select(any_of(common_cols))
) |>
  filter(if_all(c(NAG, GLS, MUP, MUX, dN_mech, Cum_Precip_mm), ~ !is.na(.x)))

# Globale robuste PCA über alle vollständigen Enzymbeobachtungen
set.seed(42)
pca_global <- PcaHubert(df_total |> select(NAG, GLS, MUP, MUX), scale = TRUE)
df_total <- df_total |>
  mutate(
    PC1_global = getScores(pca_global)[, 1],
    PC2_global = getScores(pca_global)[, 2]
  )

# Trial-interne robuste PCA Scores
df_total <- df_total |>
  group_by(LTE) |>
  group_modify(~ {
    set.seed(42)
    p <- PcaHubert(.x |> select(NAG, GLS, MUP, MUX), scale = TRUE)
    .x |> mutate(
      PC1 = getScores(p)[, 1],
      PC2 = getScores(p)[, 2]
    )
  }) |>
  ungroup()

saveRDS(df_total, "data/OptiFert_Total_Intervals.rds")
write_csv(df_total, "data/OptiFert_Total_Intervals.csv")
message(sprintf("[build_total_dataset] Erfolgreich gespeichert: %d Intervalle über %s",
                nrow(df_total),
                paste( count(df_total, LTE) |> mutate(s = paste0(LTE, " (n=", n, ")")) |> pull(s), collapse = ", ")))
