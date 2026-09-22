library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)

# Funktion zum Abrufen und Aufbereiten der Daten definieren
fetch_swissflux_data <- function(measurement = "SWC", interval = "1h", from = "now-90d", to = "now") {
  
  # Die statische Datenbank-ID
  uid <- "feslj41e3vt34c"
  
  # InfluxQL Query formulieren ($timeFilter wird serverseitig ersetzt)
  query_string <- sprintf(
    "SELECT mean(*) FROM \"%s\" WHERE $timeFilter GROUP BY time(%s) fill(null)",
    measurement, interval
  )
  
  # JSON-Payload strukturieren
  payload <- list(
    queries = list(
      list(
        datasource = list(uid = uid, type = "influxdb"),
        query = query_string,
        rawQuery = TRUE,
        refId = "A"
      )
    ),
    from = from,
    to = to
  )
  
  # Request bauen und ausführen
  req <- request("https://dataviews.swissfluxnet.ethz.ch/api/ds/query") |>
    req_method("POST") |>
    req_headers(
      `Content-Type` = "application/json",
      `Accept` = "application/json"
    ) |>
    req_body_json(payload)
  
  resp <- req_perform(req)
  data_raw <- resp_body_json(resp, simplifyVector = FALSE)
  
  # Verschachtelte Daten extrahieren
  frames <- data_raw$results$A$frames
  if (length(frames) == 0) {
    stop("Die API hat keine Daten zurückgegeben. Überprüfe die Parameter.")
  }
  
  values_list <- frames[[1]]$data$values
  col_names <- map_chr(frames[[1]]$schema$fields, "name")
  
  # In Tibble konvertieren, dabei NULL-Werte zu NA machen
  df <- map(values_list, function(col) {
    map_dbl(col, ~ if (is.null(.x)) NA_real_ else as.numeric(.x))
  }) |> 
    set_names(col_names) |> 
    as_tibble()
  
  if ("Time" %in% names(df)) {
    df <- df |>
      mutate(Time = as.POSIXct(Time / 1000, origin = "1970-01-01", tz = "UTC"))
  }
  
  return(df)
}
