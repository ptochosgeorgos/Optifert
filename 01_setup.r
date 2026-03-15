# ==============================================================================
# SCRIPT: 01_Setup.R
# ZWECK:  Datenimport, Bereinigung und Merging (Agroscope DEMO & Digit Soil)
# ==============================================================================

# 1. Pakete laden
library(tidyverse)
library(dplyr)
library(lubridate)   # Für die Datumsumwandlung
library(robustbase)
library(rrcov)

# ==============================================================================
# 2. AGROSCOPE DEMO-DATEN (Erträge & Behandlungen)
# ==============================================================================
setwd("~/Optifert")
df_demo_raw <- readxl::read_xlsx("demo89_20_plant_220411.xlsx")


df_demo_clean <- df_demo_raw |> 
  select(-contains("Res")) |> 
  mutate(
    UID = paste(Versuchsjahr, ParzNrFeld, sep = "_"),
    Kultur = as.factor(Kultur),
    Verfahren = as.factor(VerfBezeichnung),
    WiederholungNr = as.factor(WiederholungNr),
    Plot_Label = paste(Verfahren, Kultur, WiederholungNr, Versuchsjahr, sep = "-")
  ) |> 
  select(
    UID, Plot_Label, Versuchsjahr, ParzNrFeld, WiederholungNr,
    Kultur, Verfahren, Ertrag_HP_TS_kg_a = Ernte1_Ertrag_HP_TS_kg_a
  )

# ==============================================================================
# 3. DIGIT SOIL EEA-DATEN (Enzymaktivitäten)
# ==============================================================================


df_eea_raw <- read_csv2("eea_report_LTE_2025_basic_analysis.CSV")


df_eea_clean <- df_eea_raw |> 
  # 1. Fehlertexte in NA wandeln und numerisch machen
  mutate(across(LAP:MUX, ~na_if(as.character(.), "inv Samp"))) |> 
  mutate(across(LAP:MUX, as.numeric)) |> 
  
  # 2. Negative Werte auf 0 setzen
  mutate(across(LAP:MUX, ~ifelse(. < 0, 0, .))) |> 
  
  # 3. Metadaten per Regex extrahieren
  mutate(
    Parzelle_Roh = str_extract(project_sample_id, "\\d{1,3}[AB]"),
    ParzNrFeld = parse_number(Parzelle_Roh),
    
    Datum_Text = str_extract(project_sample_id, "\\d{1,2}\\.\\d{1,2}\\.\\d{2,4}"),
    Datum = dmy(Datum_Text), 
    
    Versuchsjahr = 2025,
    UID = paste(Versuchsjahr, ParzNrFeld, sep = "_")
  ) |> 
  
  # 4. NEU: MITTELWERT BILDEN (Aggregieren auf Parzellen-Ebene)
  group_by(UID, ParzNrFeld, Versuchsjahr, Datum) |> 
  summarise(
    # Mittelwert der Enzyme berechnen (NA-Werte ignorieren)
    across(LAP:MUX, ~mean(.x, na.rm = TRUE)),
    
    # Eventuelle Kommentare von Probe A und B zusammenführen
    Comments = paste(na.omit(Comments), collapse = " | "),
    .groups = "drop"
  ) |> 
  
  # Wenn es keine Kommentare gab, leere Strings wieder in echtes NA umwandeln
  mutate(Comments = na_if(Comments, ""))
# ==============================================================================
# 4. JOIN DER DATENSÄTZE
# ==============================================================================

# Wir mergen die Digit Soil Daten (many) an die Agroscope Daten (one)
df_final <- df_demo_clean |> 
  inner_join(df_eea_clean, by = c("UID", "ParzNrFeld"))

# Aufräumen des Arbeitsspeichers (wir behalten nur df_final)
rm(df_demo_raw, df_demo_clean, df_eea_raw, df_eea_clean)

print("Setup abgeschlossen. Datensatz 'df_final' ist bereit.")
