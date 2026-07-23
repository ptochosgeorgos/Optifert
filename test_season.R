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
    Season_Precip_mm = sum(Cum_Precip_mm[-1], na.rm = TRUE),
    Season_N_Entzug = sum(Interval_N_Entzug_kg_ha[-1], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Delta_Nmin = Nmin_last - Nmin_first,
    Delta_PC1 = PC1_last - PC1_first,
    Delta_PC2 = PC2_last - PC2_first,
    Delta_Nmin_kgha = Delta_Nmin * 4.5,
    dN_season = Delta_Nmin_kgha - Season_N_Entzug
  ) |>
  drop_na(dN_season, Delta_PC1, Delta_PC2, Season_Precip_mm, ParzNrFeld)

cat("Anzahl Reihen in df_season: ", nrow(df_season), "\n")

# Modell fitten
model_base <- lmer(dN_season ~ Season_Precip_mm + I(Season_Precip_mm^2) + (1 | ParzNrFeld), data = df_season)
df_season$residual <- resid(model_base)

model_resid <- lm(residual ~ Delta_PC1 + Delta_PC2, data = df_season)
summary(model_resid)

# Anderes Modell fitten
model_full <- lmer(dN_season ~ Delta_PC1 + Delta_PC2 + Season_Precip_mm + I(Season_Precip_mm^2) + (1 | ParzNrFeld), data = df_season)
summary(model_full)
