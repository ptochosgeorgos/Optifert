# Hilfsfunktion: Open-Meteo Daten laden
fetch_meteo <- function(lat, lon, start_date, end_date) {
  url <- sprintf("https://archive-api.open-meteo.com/v1/archive?latitude=%.4f&longitude=%.4f&start_date=%s&end_date=%s&daily=temperature_2m_mean,precipitation_sum&timezone=Europe%%2FBerlin", lat, lon, start_date, end_date)
  
  cat("  Lade Meteo-Daten via Open-Meteo API...\n")
  res <- jsonlite::fromJSON(url)
  meteo_df <- tibble::tibble(
    Datum = as.Date(res$daily$time),
    Temp_mean = res$daily$temperature_2m_mean,
    Precip_mm = res$daily$precipitation_sum
  ) |>
    # Calculate daily GDD (base temperature = 10 for Maize)
    dplyr::mutate(GDD = ifelse(Temp_mean > 10, Temp_mean - 10, 0))
  return(meteo_df)
}
