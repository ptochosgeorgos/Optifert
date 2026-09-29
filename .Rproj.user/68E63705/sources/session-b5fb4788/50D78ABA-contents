library(tidyverse)
library(readr)
library(jsonlite)
library(this.path)
library(glue)

rm(list = ls())

# Nmin ----

fClean <- function (df) {
  
  names(df) <- gsub("\\.\\d+$", "", names(df))
  names(df)[c(8,14,15,19,20)] <-  c("Skelett","Nitrat_mgL","Ammonium_mgL","Nitrat_mgKgTS","Ammonium_mgKgTS")
  df[,!duplicated(names(df))]
  
}

Nmin_head <- read.csv(
  here("raw_data/Fields25_NMin.csv"), 
  header = TRUE, sep = ";", nrows = 1)

Nmin_0 <- tibble(
  read.csv(
    here("raw_data/Fields25_NMin.csv"), 
    sep = ";", col.names = colnames(Nmin_head), skip = 2, header = FALSE
    )
  )

Nmin_1 <- Nmin_0 |>
  mutate(Charge = str_remove(Charge, ",.*$"),
         Charge = str_replace(Charge, "T2.5", "T5")) |>
  
  separate(col = "Charge", sep = c(3,5,7,11,13), into = c("Site","Crop","Year","PLZ","Date", "Treatment"), remove = FALSE) |>
  mutate(Date = case_when(Date == "T5" ~ "T2.5",
                          .default = Date)) |>
  
  mutate_at(c("Date","Treatment","Site", "Crop", "Probentiefe", "Auftrag", "Analysedatum" ), as.factor) %>%
  mutate(Treatment = case_when(Treatment == "all" ~ "ueblich",
                               .default = Treatment)) |>
  
  mutate(Treatment = fct_relevel(Treatment, c("null","ueblich", "empfohlen", "hofduenger",
                                              "null_ZR", "ueblich_ZR", "empfohlen_ZR", "hofduenger_ZR"))) |>
  
  nest("0_30" = 15:36, "30_60" = 37:58, "60_90" = 59:80) %>%
  
  mutate(across(.cols = c("0_30","30_60","60_90"), .fns = ~ map(., fClean)))

Nmin_2 <- Nmin_1 %>% 
  unnest(c("0_30","30_60","60_90"), names_sep = "_") %>%
  
  pivot_longer(cols = -c(1:15), names_to = c("Schicht", ".value"), names_pattern = "^(\\d+_\\d+)_(.*)$") %>%
  
  mutate(Depth = substr(Schicht, nchar(Schicht)-1, nchar(Schicht)))


# Calculate Nmin according to formula

Nmin_3 <- Nmin_2 %>%
  
  mutate(
    
    trg = case_when(
      
      Schicht == "0_30" ~ case_when(
        Humus < 10 ~ 1.25,
        Humus >= 10 & Humus < 20 ~ 1,
        Humus >= 20 & Humus < 40 ~ .85,
        Humus >= 40 & Humus < 60 ~ .65,
        Humus >= 60 ~ .5
      ),
      
      Schicht == "30_60" ~ case_when(
        Humus <= 10 ~ 1.3,
        Humus > 10 & Humus < 20 ~ 1.25,
        Humus >= 20 & Humus < 40 ~ .85,
        Humus >= 40 & Humus < 60 ~ .65,
        Humus >= 60 ~ .5
      ),
      
      Schicht == "60_90" ~ case_when(
        Humus < 20 ~ 1.35,
        Humus >= 20 & Humus < 40 ~ .85,
        Humus >= 40 & Humus < 60 ~ .65,
        Humus >= 60 ~ .5
      )
    ),
    
    skg = case_when( 
      Skelettgehalt == 1 ~ 1, 
      Skelettgehalt == 2 ~ .8,
      Skelettgehalt == 3 ~ .6),
    
    NMin.Berechnung = (Nitrat_mgKgTS + Ammonium_mgKgTS) * trg * skg * 3,
    NMin.delta = NMin.Berechnung - NMin.pro.Schicht,
    Schichtfaktor.Berechnung = trg * skg * 3,
    Schichtfaktor.delta = Schichtfaktor.Berechnung - Schichtfaktor
    
  ) |>
  
  group_by(Site,Date,Treatment) |>
  mutate(NMin.Gesamt = case_when(is.na(NMin.Gesamtgehalt) ~ round(sum(NMin.Berechnung, na.rm = TRUE), 1),
                                 .default = NMin.Gesamtgehalt)) |>
  ungroup() 




Nmin_4 <- Nmin_3 |>
  
  mutate(
    
    Treatment = case_when( # Korrektur für Markus Rütter
      Site == 708 & Treatment == "empfohlen" & Date %in% c("T1","T3") ~ "null",
      Site == 708 & Treatment == "null" & Date %in% c("T1","T3") ~ "empfohlen",
      .default = Treatment),
    
    Date = case_when( # Korrektur für Posieux
      Site == 702 & Probenahme == "11.06.2025" ~ "T2",
      .default = Date),
    
    Treatment = case_when( # Korrektur für Remund
      Site == 698 & Date == "T2" & Treatment == "null" ~ "ueblich",
      Site == 698 & Date == "T2" & Treatment == "ueblich" ~ "null",
      .default = Treatment),
    
    Depth = case_when( # Korrektur für Landis
      Site == 700 & Date == "T1" & Treatment == "empfohlen" & Depth == "30" ~ "60",
      Site == 700 & Date == "T1" & Treatment == "empfohlen" & Depth == "60" ~ "30",
      .default = Depth)
  )

saveRDS(
  Nmin_4,
  here("prep_data/Fields25_NMin.rds")
)


# EEA ----

# MUX = XYL, NAG = GLA , MUP = PHO, GLS = GLS, LAP = LEU
# DigitSoil Formula for EEA: 0.5 + 0.042*GLS + 0.056*GLA

EEA <- tibble(
  read_delim(
    here("raw_data/Fields25_EEA.csv"),
    delim = ";",
    locale = locale(
      decimal_mark = c("."),
      encoding = "UTF-8"
    )
  )
)

EEA2 <- EEA %>%
  
  select(-ncol(.)) |>
  slice(-1) |>
  filter(
    !str_detect(project_sample_id, "WW|Berger|101|102"),
    !str_detect(project_sample_id, "Senn.*T1.*90")
  )

EEA3 <- EEA2 |>
  
  separate(
    col = "project_sample_id", 
    sep = "-",
    into = c("BOB","ID"), 
    extra ="merge"
  ) |>
  
  mutate(
    ID = str_replace_all(ID, " ", ""),
    
    ID = str_replace_all(
      ID, "^([0-9]+)(?=[A-Za-z])",
      sapply(
        ID, function(x) {
          n <- nchar(x)
          if (n <= 3) {x} else {substr(x, 1, 3)}
        }
        )
    ),
    
    ID = str_replace_all(ID, "84T3", "8472T3"),
    ID = str_replace_all(ID, "703SM258356T2703SM258356T2", "703SM258356T2"),
    ID = str_replace_all(ID, "T2.5", "T5"),
    ID = str_replace_all(ID, "cm", "")
  )|>
  
  separate(
    col = "ID", 
    sep = c(3,5,7,11,13), 
    into = c("Site","Crop","Year","PLZ","Date", "Rest"), 
    remove = FALSE
  ) |>
  
  separate(
    col = "Rest",
    sep = c("-|\\_"), 
    into = c("A", "Rep")
  ) |> 
  
  separate(
    col = "A",
    sep = -2, 
    into = c("Treatment", "Depth")
  ) |> 
  
  mutate(
    Crop = ifelse(Crop == "km", "KM", Crop),
    Date = if_else(Date == "T5", true = "T2.5", false = Date)
  ) |>
  
  mutate(   #Korrektur für Markus Rütter (708)
    Treatment = case_when(
      Site == 708 & Treatment == "empfohlen" & Date %in% c("T1","T3") ~ "null",
      Site == 708 & Treatment == "null" & Date %in% c("T1","T3") ~ "empfohlen",
      .default = Treatment)
  )


saveRDS(
  EEA3,
  here("prep_data/Fields25_EEA.rds")
)


# Soil Characteristics: TOC & Bulk Density ----

TOC_head <- read.csv(here("raw_data/Fields25_TOC.csv"), header = FALSE, sep = ";", nrows = 1)

TOC <- tibble(
  
  read.csv(here("raw_data/Fields25_TOC.csv"), sep = ";", col.names = TOC_head, skip = 3, header = FALSE)) %>%
  
  separate(col = Probe.Parzelle, sep = "_", into = c("ID", "Rest")) %>%
  
  separate(col = Rest, sep = c(4,-2), into = c("PLZ","Treatment","Depth")) %>%
  
  mutate(PLZ = as.numeric(PLZ),
         Check = TIC.VarB - Wiederholung, 
         Ctot = TIC.VarB + TOC.Var.B)


TS <- tibble(
  read.csv(here("raw_data/Fields25_TS.csv"), sep = ";")) %>%
  
  separate(col = "ID", sep = "_", into = c("ID", "Rest")) %>%
  separate(col = "Rest", sep = c(4, -2), into = c("PLZ", "Treatment", "Depth")) %>%
  mutate(PLZ = as.numeric(PLZ),
         bdensity = TS/(2.5^2 * pi * Length))


Soil <- left_join(
  TS, TOC, by = c("ID", "PLZ", "Depth", "Treatment")
  ) %>% select(-ID)

saveRDS(
  Soil,
  here("prep_data/Fields25_Soil.rds"),
)



# Melior Feed Quality ----

Qua <- read_delim(
  here("raw_data/Fields25_Quality.csv"),
  delim = ";",
  locale = locale(decimal_mark = ".",
                  encoding = "UTF-8"))

Qua <- Qua |> select(-1)

#Korrektur für Markus Rütter
Qua2 <- Qua |>
  
  mutate(
    Treatment = case_when(
      Betrieb == "Rütter" & Treatment == "empfohlen" ~ "null",
      Betrieb == "Rütter" & Treatment == "null" ~ "empfohlen",
      .default = Treatment
    )
  )

saveRDS(
  Qua2,
  here("prep_data/Fields25_Feed_Quality.rds")
)

# MaisNet Data ----

maisnet <- read_csv(
  here("raw_data/Fields25_MaisNet.csv"), 
  locale = locale(encoding = "UTF-8"))

Fert <- maisnet |>
  
  separate_wider_delim(procedure_nitrogen_inputs,
                       delim = "|",
                       names = c("first", "second", "third", "fourth"),
                       too_few = "align_start") %>%
  mutate(across(c(first, second, third, fourth), ~ map(replace_na(.x, "null"), fromJSON)))

Fert2 <- Fert %>%
  mutate(ID = experiment_id, 
         Treatment = procedure_procedure_type,
         first_fert = first,
         second_fert = second,
         third_fert = third,
         fourth_fert = fourth,
         .keep = "none") %>%
  mutate(Treatment = case_when(Treatment == "Variante: empfohlene Düngung" ~ "empfohlen",
                               Treatment == "Variante: Null-Düngung" ~ "null",
                               Treatment == "Variante: betriebsübliche Düngung" ~ "ueblich")) %>%
  pivot_longer(cols = where(is.list), names_to = "Fertilization", values_to = "data" ) %>%
  unnest_wider(data) %>%
  drop_na(date)


Yield <- maisnet |>
  
  mutate(ID = experiment_id,
         Treatment = procedure_procedure_type,
         Date = procedure_harvest_date,
         GrainMoisture = procedure_grain_moisture,
         GrainYield = procedure_grain_yield,
         SiloTS = procedure_ms_teneure,
         SiloYield = procedure_ms_yield,
         .keep = "none") |>
  
  mutate(Treatment = case_when(Treatment == "Variante: empfohlene Düngung" ~ "empfohlen",
                               Treatment == "Variante: Null-Düngung" ~ "null",
                               Treatment == "Variante: betriebsübliche Düngung" ~ "ueblich")) 

fert_harv <- left_join(Fert2,Yield)

saveRDS(
  fert_harv,
  here("prep_data/Fields25_Fert_Yield.rds")
)
