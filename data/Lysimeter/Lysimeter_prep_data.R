library(tidyverse)
library(lubridate)
library(this.path)
library(glue)


rm(list = ls())

raw_list <- list.files(here("raw_data/"), pattern="*.csv", full.names = TRUE)

walk(raw_list, function(f) {
  obj_name <- str_remove_all(basename(f), c("Lysimeter_|\\.csv"))
  assign(obj_name, read_delim(f), envir = .GlobalEnv)
})


# Calculate N Leaching ----
Seepage_water_FIA2 <- Seepage_water_FIA |>
  mutate(Nleach = (NO3_N + NH4_N)* volume * 10000/(.3^2*pi) / 10^6) |>
  
  group_by(id_lysi) |>
  mutate(Nleach_cum = cumsum(Nleach)) |>
  ungroup() 

saveRDS(
  Seepage_water_FIA2,
  here("prep_data/leachate_postsummer25.rds")
)
