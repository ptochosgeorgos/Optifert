# ==============================================================================
# SCRIPT: meteo_nmin_utils.R
# ZWECK:  Modulare Funktionen für Wetterdaten-Import (Open-Meteo),
#         Intervallberechnungen, Delta Nmin und Stickstoffentzug (N-Uptake)
# ==============================================================================

library(dplyr)
library(purrr)
library(tibble)
library(jsonlite)
library(lubridate)

#' Lade tägliche Wetterdaten über die Open-Meteo Archiv-API
#'
#' @param lat Breitengrad (numeric)
#' @param lon Längengrad (numeric)
#' @param start_date Startdatum als String ("YYYY-MM-DD") oder Date
#' @param end_date Enddatum als String ("YYYY-MM-DD") oder Date
#' @param base_temp Basistemperatur für GDD (z.B. 6 oder 10 für Mais, 0 für Weizen)
#' @return Tibble mit Spalten: Datum, Temp_mean, Precip_mm, GDD
#' @export
fetch_meteo <- function(lat, lon, start_date, end_date, base_temp = 6) {
  start_str <- as.character(start_date)
  end_str <- as.character(end_date)
  
  url <- sprintf(
    "https://archive-api.open-meteo.com/v1/archive?latitude=%.4f&longitude=%.4f&start_date=%s&end_date=%s&daily=temperature_2m_mean,precipitation_sum&timezone=Europe%%2FBerlin",
    lat, lon, start_str, end_str
  )
  
  message("  Lade Meteo-Daten via Open-Meteo API (", start_str, " bis ", end_str, ")...")
  res <- jsonlite::fromJSON(url)
  
  meteo_df <- tibble::tibble(
    Datum = as.Date(res$daily$time),
    Temp_mean = as.numeric(res$daily$temperature_2m_mean),
    Precip_mm = as.numeric(res$daily$precipitation_sum)
  ) |>
    dplyr::mutate(
      GDD = ifelse(!is.na(Temp_mean) & Temp_mean > base_temp, Temp_mean - base_temp, 0)
    )
  
  return(meteo_df)
}

#' Lade tägliche Wetterdaten direkt über eine Standort-Konfigurationsliste (site_config)
#'
#' @param site_config Liste mit Feldern: latitude (oder lat), longitude (oder lon), optional base_temp
#' @param start_date Startdatum
#' @param end_date Enddatum
#' @return Tibble mit Spalten: Datum, Temp_mean, Precip_mm, GDD
#' @export
fetch_meteo_site <- function(site_config, start_date, end_date) {
  lat <- site_config$latitude %||% site_config$lat
  lon <- site_config$longitude %||% site_config$lon
  base_temp <- site_config$base_temp %||% 6
  
  if (is.null(lat) || is.null(lon)) {
    stop("site_config muss 'latitude' (oder 'lat') und 'longitude' (oder 'lon') enthalten.")
  }
  
  fetch_meteo(lat = lat, lon = lon, start_date = start_date, end_date = end_date, base_temp = base_temp)
}

#' Fügt Messintervalle (t1, t2), Gesamten Nmin und Delta Nmin zu einem Tibble hinzu
#'
#' @param df Data Frame / Tibble mit Messdaten
#' @param group_vars Spaltennamen zur Gruppierung (z.B. c("UID", "Probe") oder "ParzNrFeld")
#' @param date_col Name der Datumsspalte (Default: "Datum")
#' @param saattag_col Name der Saattag-Datumsspalte als Fallback für erstes t1 (Default: "SaatTag_Date")
#' @param nh4_col Name der Ammoniumspalte (Default: "NH4")
#' @param no3_col Name der Nitratspalte (Default: "NO3")
#' @return Tibble mit Spalten: t1, t2, Nmin_Total, Delta_Nmin
#' @export
add_intervals_and_delta_nmin <- function(df, 
                                        group_vars = c("UID", "Probe"), 
                                        date_col = "Datum", 
                                        saattag_col = "SaatTag_Date",
                                        nh4_col = "NH4", 
                                        no3_col = "NO3") {
  
  req_cols <- c(group_vars, date_col)
  missing_cols <- setdiff(req_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Fehlende Pflichtspalten in add_intervals_and_delta_nmin: ", paste(missing_cols, collapse = ", "))
  }
  
  has_nh4 <- nh4_col %in% names(df)
  has_no3 <- no3_col %in% names(df)
  has_saattag <- saattag_col %in% names(df)
  
  df |>
    dplyr::group_by(across(all_of(group_vars))) |>
    dplyr::arrange(.data[[date_col]], .by_group = TRUE) |>
    dplyr::mutate(
      t2 = .data[[date_col]],
      t1 = dplyr::lag(
        .data[[date_col]], 
        default = if (has_saattag) dplyr::first(na.omit(.data[[saattag_col]])) else as.Date(NA)
      ),
      Nmin_Total = if (has_nh4 && has_no3) {
        .data[[nh4_col]] + .data[[no3_col]]
      } else if (has_no3) {
        .data[[no3_col]]
      } else {
        NA_real_
      },
      Delta_Nmin = Nmin_Total - dplyr::lag(Nmin_Total)
    ) |>
    dplyr::ungroup()
}

#' Berechnet kumulierten Niederschlag und GDD für die definierten Zeitintervalle
#'
#' @param df Tibble mit t1, t2 und optional Saattag
#' @param meteo_df Tibble der täglichen Wetterdaten (Spalten: Datum, Precip_mm, GDD)
#' @param t1_col Name der t1-Spalte (Default: "t1")
#' @param t2_col Name der t2-Spalte (Default: "t2")
#' @param saattag_col Name der SaatTag_Date-Spalte (Default: "SaatTag_Date")
#' @return Tibble mit Spalten: Cum_Precip_mm, Cum_GDD_Interval, Cum_GDD_t1, Cum_GDD_t2
#' @export
add_meteo_intervals <- function(df, 
                                meteo_df, 
                                t1_col = "t1", 
                                t2_col = "t2", 
                                saattag_col = "SaatTag_Date") {
  
  if (!all(c(t1_col, t2_col) %in% names(df))) {
    stop("df benötigt Spalten: ", t1_col, " und ", t2_col)
  }
  if (!all(c("Datum", "Precip_mm", "GDD") %in% names(meteo_df))) {
    stop("meteo_df benötigt Spalten: Datum, Precip_mm, GDD")
  }
  
  has_saattag <- saattag_col %in% names(df)
  
  df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      Cum_Precip_mm = if (!is.na(.data[[t1_col]]) && !is.na(.data[[t2_col]])) {
        sum(meteo_df$Precip_mm[meteo_df$Datum > .data[[t1_col]] & meteo_df$Datum <= .data[[t2_col]]], na.rm = TRUE)
      } else {
        NA_real_
      },
      Cum_GDD_Interval = if (!is.na(.data[[t1_col]]) && !is.na(.data[[t2_col]])) {
        sum(meteo_df$GDD[meteo_df$Datum > .data[[t1_col]] & meteo_df$Datum <= .data[[t2_col]]], na.rm = TRUE)
      } else {
        NA_real_
      },
      Cum_GDD_t1 = if (has_saattag && !is.na(.data[[saattag_col]]) && !is.na(.data[[t1_col]])) {
        sum(meteo_df$GDD[meteo_df$Datum >= .data[[saattag_col]] & meteo_df$Datum <= .data[[t1_col]]], na.rm = TRUE)
      } else {
        NA_real_
      },
      Cum_GDD_t2 = if (has_saattag && !is.na(.data[[saattag_col]]) && !is.na(.data[[t2_col]])) {
        sum(meteo_df$GDD[meteo_df$Datum >= .data[[saattag_col]] & meteo_df$Datum <= .data[[t2_col]]], na.rm = TRUE)
      } else {
        NA_real_
      }
    ) |>
    dplyr::ungroup()
}

#' Berechnet logistischen Stickstoff-Entzug (S-Kurve) für Mais
#'
#' @param df Tibble mit Cum_GDD_t1, Cum_GDD_t2 und Ertrags-/Entzugs-Spalten
#' @param total_n_entzug_col Name der Spalte mit dem Gesamt-N-Entzug (Default: "Total_N_Entzug_kg_ha")
#' @param gdd_t1_col Name der Spalte für GDD bis t1 (Default: "Cum_GDD_t1")
#' @param gdd_t2_col Name der Spalte für GDD bis t2 (Default: "Cum_GDD_t2")
#' @param k Steilheit der logistischen Kurve (Default: 0.012)
#' @param x0 Wendepunkt in GDD (Default: 750)
#' @return Tibble mit Spalten: N_up_t1, N_up_t2, Interval_N_Entzug_kg_ha
#' @export
add_maize_n_uptake <- function(df,
                               total_n_entzug_col = "Total_N_Entzug_kg_ha",
                               gdd_t1_col = "Cum_GDD_t1",
                               gdd_t2_col = "Cum_GDD_t2",
                               k = 0.012,
                               x0 = 750) {
  
  if (!all(c(total_n_entzug_col, gdd_t1_col, gdd_t2_col) %in% names(df))) {
    stop("Fehlende Spalten für add_maize_n_uptake: ", 
         paste(setdiff(c(total_n_entzug_col, gdd_t1_col, gdd_t2_col), names(df)), collapse = ", "))
  }
  
  df |>
    dplyr::mutate(
      N_up_t1 = .data[[total_n_entzug_col]] / (1 + exp(-k * (.data[[gdd_t1_col]] - x0))),
      N_up_t2 = .data[[total_n_entzug_col]] / (1 + exp(-k * (.data[[gdd_t2_col]] - x0))),
      Interval_N_Entzug_kg_ha = ifelse(!is.na(N_up_t2) & !is.na(N_up_t1), N_up_t2 - N_up_t1, NA_real_)
    )
}
