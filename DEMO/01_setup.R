# ==============================================================================
# SCRIPT: 01_Setup.R
# ZWECK:  Datenimport, Bereinigung und Merging (Agroscope, Digit Soil, Nmin)
# ==============================================================================

# 1. Pakete laden
library(tidyverse)
library(lubridate)
library(readxl)

# ==============================================================================
# 2. AGROSCOPE DEMO-DATEN (Erträge & Nährstoffentzug)
# ==============================================================================


df_demo_raw <- readxl::read_xlsx("demo89_20_plant_220411.xlsx", sheet = "Ertrag_89_19")

df_demo_clean <- df_demo_raw |> 
  select(-contains("Res")) |> 
  mutate(
    UID = paste(Versuchsjahr, ParzNrFeld, sep = "_"),
    Kultur = as.factor(Kultur),
    Verfahren = as.factor(VerfBezeichnung),
    WiederholungNr = as.factor(WiederholungNr),
    Plot_Label = paste(Verfahren, Kultur, WiederholungNr, Versuchsjahr, sep = "-"),
    
    # Kumulative Summenbildung über alle 6 potenziellen Ernten
    Total_Ertrag_HP_TS_kg_a = rowSums(across(matches("Ernte[1-6]_Ertrag_HP_TS_kg_a")), na.rm = TRUE),
    Total_N_Entzug_kg_ha    = rowSums(across(matches("Ernte[1-6]_NEntz_HP_kg_ha")), na.rm = TRUE),
    Total_P_Entzug_kg_ha    = rowSums(across(matches("Ernte[1-6]_PEntz_HP_kg_ha")), na.rm = TRUE),
    Total_OS_Prod_kg_ha     = rowSums(across(matches("Ernte[1-6]_OSprod_HP_kg_ha")), na.rm = TRUE)
  ) |> 
  select(
    UID, Plot_Label, Versuchsjahr, ParzNrFeld, WiederholungNr,
    Kultur, Verfahren, 
    Total_Ertrag_HP_TS_kg_a, 
    Total_N_Entzug_kg_ha, 
    Total_P_Entzug_kg_ha, 
    Total_OS_Prod_kg_ha
  )

# ==============================================================================
# 3. DIGIT SOIL EEA-DATEN (Enzymaktivitäten)
# ==============================================================================

# ACHTUNG: Dateipfad anpassen
df_eea_raw <- read_csv2("eea_report_LTE_2025_basic_analysis.CSV")

df_eea_clean <- df_eea_raw |> 
  # Fehlertexte in NA wandeln, als numerisch deklarieren, negative Werte nullen
  mutate(across(LAP:MUX, ~na_if(as.character(.), "inv Samp"))) |> 
  mutate(across(LAP:MUX, as.numeric)) |> 
  mutate(across(LAP:MUX, ~ifelse(. < 0, 0, .))) |> 
  
  # Regex-Extraktion für ID und Datum
  mutate(
    Parzelle_Roh = str_extract(project_sample_id, "\\d{1,3}[AB]"),
    ParzNrFeld = parse_number(Parzelle_Roh),
    Probe = str_extract(Parzelle_Roh, "[AB]"), # Replikat A oder B
    
    Datum_Text = str_extract(project_sample_id, "\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}"),
    Datum = dmy(Datum_Text), 
    
    # NEU: Fehlende Daten mit dem 11.09.2025 auffüllen
    Datum = replace_na(Datum, as.Date("2025-09-11")),
    
    # Versuchsjahr fest auf 2025 setzen (verhindert NA beim Join)
    Versuchsjahr = 2025,
    UID = paste(Versuchsjahr, ParzNrFeld, sep = "_")
  ) |> 
  select(UID, Versuchsjahr, ParzNrFeld, Probe, Datum, LAP, NAG, GLS, MUP, MUX, Comments)

# ==============================================================================
# 4. LABOR-DATEN (Nmin: Ammonium, Nitrat, Schwefel)
# ==============================================================================

# ACHTUNG: Dateipfad anpassen
spaltennamen <- names(read_xlsx("DEMO_Nmin.xlsx", sheet = "List", n_max = 0))
df_nmin_raw <- read_xlsx("DEMO_Nmin.xlsx", sheet = "List", skip = 4, col_names = spaltennamen)

df_nmin_clean <- df_nmin_raw |> 
  # Kommas durch Punkte ersetzen
  mutate(across(c(Ammoniumstickstoff, Nitratstickstoff, Schwefel), 
                ~ as.numeric(str_replace(.x, ",", ".")))) |> 
  mutate(
    # Extraktion von Probe A/B analog zu den Sensordaten
    Parzelle_Roh = str_extract(`Verfahren-Bez,`, "\\d{1,3}[AB]"),
    ParzNrFeld = parse_number(Parzelle_Roh),
    Probe = str_extract(Parzelle_Roh, "[AB]"), 
    
    Verfahren_Nmin = `Verfahren Nr,`, # Fallback für die Behandlungs-Labels
    Versuchsjahr = year(Datum),
    Datum = as.Date(Datum),
    UID = paste(Versuchsjahr, ParzNrFeld, sep = "_")
  ) |> 
  rename(NH4 = Ammoniumstickstoff, NO3 = Nitratstickstoff, S = Schwefel) |> 
  
  # Aggregation nur falls im Labor versehentlich Doppelmessungen derselben Probe passierten
  group_by(UID, ParzNrFeld, Versuchsjahr, Datum, Probe, Verfahren_Nmin) |> 
  summarise(
    across(c(NH4, NO3, S), ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# ==============================================================================
# 5. DREI-WEGE-MERGE (Ertrag + EEA + Nmin)
# ==============================================================================

# 1. Temporaler Merge INKLUSIVE 'Probe' (A/B)
df_temporal <- df_eea_clean |> 
  full_join(df_nmin_clean, by = c("UID", "ParzNrFeld", "Versuchsjahr", "Datum", "Probe"))

# 2. Finaler Merge mit den Ertragsdaten
# Wir nutzen left_join, um die Sensordaten von 2025 nicht zu verlieren, 
# falls die Erträge noch fehlen.
df_final <- df_temporal |> 
  left_join(df_demo_clean, by = c("UID", "ParzNrFeld", "Versuchsjahr")) |> 
  
  # Sicherheitsnetz: Falls 'Verfahren' aus Agroscope (2025) noch NA ist, 
  # füllen wir es mit den Labels aus dem Nmin-Datensatz auf!
  mutate(Verfahren = coalesce(Verfahren, as.factor(Verfahren_Nmin))) |> 
  select(-Verfahren_Nmin)

# Aufräumen des Arbeitsspeichers
rm(df_demo_raw, df_demo_clean, df_eea_raw, df_eea_clean, df_nmin_raw, df_nmin_clean, df_temporal, spaltennamen)


