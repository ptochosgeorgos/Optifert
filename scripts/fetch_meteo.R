# ==============================================================================
# SCRIPT: fetch_meteo.R
# ZWECK:  Kompatibilitäts-Wrapper für Wetterdaten-Abruf via Open-Meteo
#         (Implementierung siehe scripts/meteo_nmin_utils.R)
# ==============================================================================

# Lade die modularisierten Utility-Funktionen
utils_path <- file.path(dirname(sys.frame(1)$ofile %||% "."), "meteo_nmin_utils.R")
if (!file.exists(utils_path)) {
  utils_path <- "scripts/meteo_nmin_utils.R"
}
if (file.exists(utils_path)) {
  source(utils_path)
} else {
  # Fallback falls direkt gesourced
  fetch_meteo <- function(lat, lon, start_date, end_date, base_temp = 6) {
    url <- sprintf(
      "https://archive-api.open-meteo.com/v1/archive?latitude=%.4f&longitude=%.4f&start_date=%s&end_date=%s&daily=temperature_2m_mean,precipitation_sum&timezone=Europe%%2FBerlin",
      lat, lon, as.character(start_date), as.character(end_date)
    )
    res <- jsonlite::fromJSON(url)
    tibble::tibble(
      Datum = as.Date(res$daily$time),
      Temp_mean = as.numeric(res$daily$temperature_2m_mean),
      Precip_mm = as.numeric(res$daily$precipitation_sum)
    ) |>
      dplyr::mutate(GDD = ifelse(!is.na(Temp_mean) & Temp_mean > base_temp, Temp_mean - base_temp, 0))
  }
}
