library(tidyverse)
library(rrcov)
library(lme4)
library(lmerTest)

# Lade Daten
df_final <- read_csv("/home/marc/Documents/Agroscope/Optifert/digitsoil_master.csv", show_col_types = FALSE) |> filter(LTE == "DEMO")

df_pca <- df_final |> drop_na(LAP, NAG, GLS, MUP, MUX, Verfahren)
pca_rob <- PcaHubert(df_pca |> select(LAP, NAG, GLS, MUP, MUX), scale = TRUE)
df_pca <- df_pca |> mutate(PC1 = getScores(pca_rob)[, 1], PC2 = getScores(pca_rob)[, 2])

df_season <- df_pca |>
  group_by(UID, ParzNrFeld, Probe) |>
  arrange(Datum) |>
  filter(n() >= 2) |>
  summarise(
    Nmin_first = first(NH4 + NO3),
    Nmin_last = last(NH4 + NO3),
    PC1_first = first(PC1),
    PC1_last = last(PC1),
    PC2_first = first(PC2),
    PC2_last = last(PC2),
    PC1_AUC = sum((PC1 + lag(PC1)) / 2 * as.numeric(Datum - lag(Datum)), na.rm = TRUE),
    PC2_AUC = sum((PC2 + lag(PC2)) / 2 * as.numeric(Datum - lag(Datum)), na.rm = TRUE),
    Season_length_days = as.numeric(last(Datum) - first(Datum)),
    Season_Precip_mm = sum(Cum_Precip_mm[-1], na.rm = TRUE),
    Season_N_Entzug = sum(Interval_N_Entzug_kg_ha[-1], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Delta_Nmin = Nmin_last - Nmin_first,
    Delta_PC1 = PC1_last - PC1_first,
    Delta_PC2 = PC2_last - PC2_first,
    PC1_TWA = PC1_AUC / Season_length_days,
    PC2_TWA = PC2_AUC / Season_length_days,
    Delta_Nmin_kgha = Delta_Nmin * 4.5,
    dN_season = Delta_Nmin_kgha - Season_N_Entzug
  ) |>
  drop_na(dN_season, PC1_TWA, PC2_TWA, Season_Precip_mm, ParzNrFeld)

cat("Anzahl Reihen in df_season: ", nrow(df_season), "\n")

# Modell fitten
model_base <- lmer(dN_season ~ Season_Precip_mm + I(Season_Precip_mm^2) + (1 | ParzNrFeld), data = df_season)
df_season$residual <- resid(model_base)

model_resid <- lm(residual ~ PC1_TWA + PC2_TWA, data = df_season)
summary(model_resid)

# Anderes Modell fitten
model_full <- lmer(dN_season ~ PC1_TWA + PC2_TWA + Season_Precip_mm + I(Season_Precip_mm^2) + (1 | ParzNrFeld), data = df_season)
summary(model_full)
