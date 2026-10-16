# ==============================================================================
# SCRIPT: import_maisnet.R
# ZWECK:  Wrapper für das Einlesen von exportiereten Daten aus Maisnet im JSON-Format
# ==============================================================================

# UNDER CONSTRUCTION!

data_maisnet <- read_csv(glue("{rwpath}MaisNet_20260602.csv"), locale = locale(encoding = "UTF-8"))

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

saveRDS(
  Fert2,
  paste0(path,"prep_data/Fertilization.rds"),
  compress = "xz"   # hohe Kompression
)

Harvest <- maisnet |>
  
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
                               Treatment == "Variante: betriebsübliche Düngung" ~ "ueblich")) |>
  
  saveRDS(
    glue({"{path}prep_data/Harvest.rds"}),
    compress = "xz"
  )
