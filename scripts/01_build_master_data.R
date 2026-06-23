# ==============================================================================
# SCRIPT: 01_build_master_data.R
# ZWECK:  Generic Datenimport, Bereinigung und Merging für mehrere LTEs
# ==============================================================================

library(tidyverse)
library(lubridate)
library(readxl)
library(jsonlite)

# ==============================================================================
# 1. Konfiguration
# ==============================================================================
config <- tibble::tribble(
  ~LTE,   ~Year, ~Meteo_Lat, ~Meteo_Lon, ~EEA_File,                                       ~Nmin_File,             ~Yield_File,                        ~Yield_Sheet,
  "DEMO", 2025,  47.428,     8.520,      "data/eea_report_LTE_2025_basic_analysis.CSV",  "data/DEMO_Nmin.xlsx",  "data/demo89_20_plant_220411.xlsx", "Ertrag_89_19"
)

# Hilfsfunktion: Open-Meteo Daten laden
fetch_meteo <- function(lat, lon, start_date, end_date) {
  url <- sprintf("https://archive-api.open-meteo.com/v1/archive?latitude=%.4f&longitude=%.4f&start_date=%s&end_date=%s&daily=temperature_2m_mean,precipitation_sum&timezone=Europe%%2FBerlin", lat, lon, start_date, end_date)
  
  cat("  Lade Meteo-Daten via Open-Meteo API...\n")
  res <- fromJSON(url)
  meteo_df <- tibble(
    Datum = as.Date(res$daily$time),
    Temp_mean = res$daily$temperature_2m_mean,
    Precip_mm = res$daily$precipitation_sum
  ) |>
    # Calculate daily GDD (base temperature = 10 for Maize)
    mutate(GDD = ifelse(Temp_mean > 10, Temp_mean - 10, 0))
  return(meteo_df)
}

# ==============================================================================
# 2. Verarbeitungs-Funktion pro LTE & Jahr
# ==============================================================================
process_lte <- function(row) {
  lte_name <- row$LTE
  year_val <- row$Year
  
  cat("Verarbeite LTE:", lte_name, "für Jahr:", year_val, "\n")
  
  # --- 2.1 Ertragsdaten ---
  df_yield_clean <- tibble()
  if (!is.na(row$Yield_File) && file.exists(row$Yield_File)) {
    cat("  Lese Ertragsdaten...\n")
    df_yield_raw <- read_xlsx(row$Yield_File, sheet = row$Yield_Sheet)
    df_yield_clean <- df_yield_raw |> 
      select(-contains("Res")) |> 
      mutate(
        Versuchsjahr = if("Versuchsjahr" %in% names(df_yield_raw)) Versuchsjahr else year_val,
        UID = paste(Versuchsjahr, ParzNrFeld, sep = "_"),
        Kultur = as.factor(Kultur),
        Verfahren = as.factor(VerfBezeichnung),
        WiederholungNr = as.factor(WiederholungNr),
        Plot_Label = paste(Verfahren, Kultur, WiederholungNr, Versuchsjahr, sep = "-"),
        
        # SaatTag is Day of Year (DOY) in the dataset
        SaatTag_DOY = as.numeric(SaatTag),
        SaatTag_Date = as.Date(SaatTag_DOY, origin = paste0(Versuchsjahr - 1, "-12-31")),
        
        # N_fert extraction (average for the moment)
        N_fert_1 = as.numeric(`1MinNGabe_kgN_ha`),
        N_fert_2 = as.numeric(`2MinNGabe_kgN_ha`),
        Total_N_fert_kg_ha = rowSums(cbind(N_fert_1, N_fert_2), na.rm = TRUE),
        
        Total_Ertrag_HP_TS_kg_a = rowSums(across(matches("Ernte[1-6]_Ertrag_HP_TS_kg_a")), na.rm = TRUE),
        Total_N_Entzug_kg_ha    = rowSums(across(matches("Ernte[1-6]_NEntz_HP_kg_ha")), na.rm = TRUE)
      ) |> 
      select(
        UID, Plot_Label, Versuchsjahr, ParzNrFeld, WiederholungNr,
        Kultur, Verfahren, SaatTag_DOY, SaatTag_Date, Total_N_fert_kg_ha,
        Total_Ertrag_HP_TS_kg_a, Total_N_Entzug_kg_ha
      )
  }
  
  # --- 2.2 EEA-Daten (Digit Soil) ---
  df_eea_clean <- tibble()
  if (!is.na(row$EEA_File) && file.exists(row$EEA_File)) {
    cat("  Lese EEA-Daten...\n")
    df_eea_raw <- read_csv2(row$EEA_File, show_col_types = FALSE)
    df_eea_clean <- df_eea_raw |> 
      mutate(across(LAP:MUX, ~na_if(as.character(.), "inv Samp"))) |> 
      mutate(across(LAP:MUX, as.numeric)) |> 
      mutate(across(LAP:MUX, ~ifelse(!is.na(.) & . < 0, 0, .))) |> 
      mutate(
        Parzelle_Roh = str_extract(project_sample_id, "\\d{1,3}[AB]"),
        ParzNrFeld = parse_number(Parzelle_Roh),
        Probe = str_extract(Parzelle_Roh, "[AB]"), 
        Datum_Text = str_extract(project_sample_id, "\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}"),
        Datum = dmy(Datum_Text), 
        Versuchsjahr = year(Datum)
      ) |> 
      mutate(Versuchsjahr = if_else(is.na(Versuchsjahr), as.numeric(year_val), as.numeric(Versuchsjahr))) |> 
      mutate(UID = paste(Versuchsjahr, ParzNrFeld, sep = "_")) |> 
      select(UID, Versuchsjahr, ParzNrFeld, Probe, Datum, LAP, NAG, GLS, MUP, MUX)
  }
  
  # --- 2.3 Labor-Daten (Nmin) ---
  df_nmin_clean <- tibble()
  if (!is.na(row$Nmin_File) && file.exists(row$Nmin_File)) {
    cat("  Lese Nmin-Daten...\n")
    spaltennamen <- names(read_xlsx(row$Nmin_File, sheet = "List", n_max = 0))
    df_nmin_raw <- read_xlsx(row$Nmin_File, sheet = "List", skip = 4, col_names = spaltennamen)
    df_nmin_clean <- df_nmin_raw |> 
      mutate(across(c(Ammoniumstickstoff, Nitratstickstoff), ~ as.numeric(str_replace(.x, ",", ".")))) |> 
      mutate(
        Parzelle_Roh = str_extract(`Verfahren-Bez,`, "\\d{1,3}[AB]"),
        ParzNrFeld = parse_number(Parzelle_Roh),
        Probe = str_extract(Parzelle_Roh, "[AB]"), 
        Verfahren_Nmin = `Verfahren Nr,`,
        Datum = as.Date(Datum),
        Versuchsjahr = year(Datum)
      ) |> 
      mutate(Versuchsjahr = if_else(is.na(Versuchsjahr), as.numeric(year_val), as.numeric(Versuchsjahr))) |> 
      mutate(UID = paste(Versuchsjahr, ParzNrFeld, sep = "_")) |> 
      rename(NH4 = Ammoniumstickstoff, NO3 = Nitratstickstoff) |> 
      group_by(UID, ParzNrFeld, Versuchsjahr, Datum, Probe, Verfahren_Nmin) |> 
      summarise(across(c(NH4, NO3), ~mean(.x, na.rm = TRUE)), .groups = "drop")
  }
  
    # --- 2.4 Temporaler Merge & Meteo Integration ---
    cat("  Erstelle interval-basierte Datenstruktur...\n")
    
    # Snap Nmin dates to nearest EEA date if within 3 days to fix sampling mismatches
    df_nmin_clean <- df_nmin_clean |>
      mutate(Datum = map_vec(Datum, function(d) {
        eea_dates <- unique(df_eea_clean$Datum)
        if(length(eea_dates) == 0) return(d)
        closest <- eea_dates[which.min(abs(eea_dates - d))]
        if(abs(closest - d) <= 3) return(closest) else return(d)
      })) |>
      mutate(Datum = as.Date(Datum))
    
    df_temporal <- full_join(df_eea_clean, df_nmin_clean, by = c("UID", "ParzNrFeld", "Versuchsjahr", "Datum", "Probe"))  
  # Berechne historische Mittelwerte pro Parzelle für fehlende aktuelle Ertragsdaten (gefiltert für Mais "MA")
  df_yield_avg <- df_yield_clean |>
    filter(Kultur == "MA") |>
    group_by(ParzNrFeld) |>
    summarise(
      Avg_N_Entzug = mean(Total_N_Entzug_kg_ha, na.rm = TRUE),
      Avg_SaatTag = mean(SaatTag_DOY, na.rm = TRUE),
      .groups = "drop"
    )
    
  df_final <- df_temporal |> 
    left_join(df_yield_clean, by = c("UID", "ParzNrFeld", "Versuchsjahr")) |> 
    left_join(df_yield_avg, by = "ParzNrFeld") |>
    mutate(
      Verfahren = coalesce(Verfahren, as.factor(Verfahren_Nmin)),
      Total_N_Entzug_kg_ha = coalesce(Total_N_Entzug_kg_ha, Avg_N_Entzug),
      SaatTag_DOY = coalesce(SaatTag_DOY, Avg_SaatTag),
      SaatTag_Date = as.Date(SaatTag_DOY, origin = paste0(Versuchsjahr - 1, "-12-31"))
    ) |> 
    select(-any_of(c("Verfahren_Nmin", "Avg_N_Entzug", "Avg_SaatTag")))
    
  # Hole Meteo Daten
  meteo_df <- fetch_meteo(row$Meteo_Lat, row$Meteo_Lon, paste0(year_val, "-01-01"), paste0(year_val, "-12-31"))
  
  # Berechne Intervalle (t1 zu t2)
  df_final <- df_final |> 
    group_by(UID, Probe) |> 
    arrange(Datum) |> 
    mutate(
      t2 = Datum,
      t1 = lag(Datum, default = first(SaatTag_Date)),
      # Berechne Delta Nmin (Aktuell - Vorherige Messung)
      Nmin_Total = NH4 + NO3,
      Delta_Nmin = Nmin_Total - lag(Nmin_Total)
    ) |> 
    ungroup()
    
  # Integriere Meteo Summen
  df_final <- df_final |>
    rowwise() |>
    mutate(
      Cum_Precip_mm = sum(meteo_df$Precip_mm[meteo_df$Datum > t1 & meteo_df$Datum <= t2], na.rm = TRUE),
      Cum_GDD_Interval = sum(meteo_df$GDD[meteo_df$Datum > t1 & meteo_df$Datum <= t2], na.rm = TRUE),
      Cum_GDD_t1 = sum(meteo_df$GDD[meteo_df$Datum >= SaatTag_Date & meteo_df$Datum <= t1], na.rm = TRUE),
      Cum_GDD_t2 = sum(meteo_df$GDD[meteo_df$Datum >= SaatTag_Date & meteo_df$Datum <= t2], na.rm = TRUE)
    ) |>
    ungroup()
    
  # Logistic N-Uptake (S-Curve) for Maize
  df_final <- df_final |>
    mutate(
      N_up_t1 = Total_N_Entzug_kg_ha / (1 + exp(-0.012 * (Cum_GDD_t1 - 750))),
      N_up_t2 = Total_N_Entzug_kg_ha / (1 + exp(-0.012 * (Cum_GDD_t2 - 750))),
      Interval_N_Entzug_kg_ha = N_up_t2 - N_up_t1
    ) |>
    mutate(LTE = lte_name)
    
  return(df_final)
}

# ==============================================================================
# 3. Alle LTEs verarbeiten und zusammenführen
# ==============================================================================
all_data <- config |> 
  rowwise() |> 
  group_split() |> 
  map_dfr(process_lte)

# ==============================================================================
# 4. Daten Export
# ==============================================================================
export_path <- "digitsoil_master.csv"
cat("Schreibe Master-Datensatz nach:", export_path, "\n")
write_csv(all_data, export_path)
cat("Fertig!\n")
