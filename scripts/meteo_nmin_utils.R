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

#' Ermittelt die agronomische Basistemperatur (T_base) für Growing Degree Days (GDD)
#'
#' Basiert auf physiologischen Schwellenwerten der europäischen Agrarmeteorologie:
#' - Wintergetreide (WW, WG, WR, TR): 0.0 °C (Porter & Gawith, 1999, doi:10.1016/S1161-0301(98)00047-1)
#' - Sommergetreide (SW, SG, HA): 0.0 °C (Porter & Gawith, 1999)
#' - Raps (RA): 4.5 °C (Bonhomme, 2000, doi:10.1016/S1161-0301(00)00058-7)
#' - Zuckerrüben (ZR): 5.0 °C (Bonhomme, 2000)
#' - Mais (MA, KM, SM): 6.0 °C (mitteleuropäischer Standard für kühl-temperierte C4-Hybride;
#'   Bonhomme, 2000; Sánchez et al., 2014, doi:10.1111/gcb.12389; vgl. Frei et al., 2024)
#' - Leguminosen / Ölsaaten (SO, SB): 8.0 °C (Bonhomme, 2000)
#' - Kunstwiesen / Gras (KW, GRAS): 5.0 °C (Bonhomme, 2000)
#'
#' @references
#' Bonhomme, R. (2000). Bases and limits to using 'degree.day' units. European Journal of Agronomy, 13(1), 1-10.
#' McMaster, G. S., & Wilhelm, W. W. (1997). Growing degree-days: one equation, two interpretations. Agricultural and Forest Meteorology, 87(4), 291-300.
#' Porter, J. R., & Gawith, M. (1999). Temperatures and the growth and development of wheat: a review. European Journal of Agronomy, 10(1), 23-36.
#' Sánchez, B., Rasmussen, A., & Porter, J. R. (2014). Temperatures and the growth and development of maize and rice: a review. Global Change Biology, 20(2), 408-417.
#'
#' @param crop Kulturkürzel (z.B. "MA", "WW", "KM", "SM", "RA") oder Klartext
#' @param default Fallback-Temperatur falls Kultur unbekannt (Default: 6.0)
#' @return Basistemperatur in °C (numeric)
#' @export
get_crop_base_temp <- function(crop, default = 6.0) {
  if (is.null(crop) || length(crop) == 0) {
    return(default)
  }
  
  crop_clean <- toupper(trimws(as.character(crop)))
  
  dplyr::case_when(
    crop_clean %in% c("WW", "WG", "WR", "TR", "WEIZEN", "WINTERWEIZEN", "GERSTE", "WINTERGERSTE", "ROGGEN", "TRITICALE") ~ 0.0,
    crop_clean %in% c("SW", "SG", "HA", "SOMMERWEIZEN", "SOMMERGERSTE", "HAFER") ~ 0.0,
    crop_clean %in% c("RA", "RAPS", "WINTERRAPS") ~ 4.5,
    crop_clean %in% c("ZR", "ZUCKERRUEBE", "RUEBEN") ~ 5.0,
    crop_clean %in% c("KW", "KUNSTWIESE", "GRAS", "WIESE") ~ 5.0,
    crop_clean %in% c("MA", "KM", "SM", "MAIS", "SILOMAIS", "KOERNERMAIS") ~ 6.0,
    crop_clean %in% c("SO", "SB", "SOJA", "SONNENBLUME") ~ 8.0,
    TRUE ~ default
  )
}

#' Berechnet GDD (Growing Degree Days) tages- oder zeilenweise
#'
#' @param temp_mean Mittlere Tagestemperatur (°C)
#' @param crop Kulturkürzel (optional)
#' @param base_temp Basistemperatur in °C (optional, überschreibt crop)
#' @return Vektor mit GDD-Werten
#' @export
calc_gdd <- function(temp_mean, crop = NULL, base_temp = NULL) {
  bt <- if (!is.null(base_temp)) {
    base_temp
  } else if (!is.null(crop)) {
    get_crop_base_temp(crop)
  } else {
    6.0
  }
  pmax(0, temp_mean - bt)
}

#' Lade tägliche Wetterdaten über die Open-Meteo Archiv-API
#'
#' @param lat Breitengrad (numeric)
#' @param lon Längengrad (numeric)
#' @param start_date Startdatum als String ("YYYY-MM-DD") oder Date
#' @param end_date Enddatum als String ("YYYY-MM-DD") oder Date
#' @param base_temp Basistemperatur für GDD (z.B. 6 oder 10 für Mais, 0 für Weizen)
#' @return Tibble mit Spalten: Datum, Temp_mean, Precip_mm, GDD und weitere Umweltvariablen
#' @export
fetch_meteo <- function(lat, lon, start_date, end_date, base_temp = 6) {
  start_str <- as.character(start_date)
  end_str <- as.character(end_date)
  
  # Erweiterte Liste von Variablen anfordern
  vars <- paste(
    "temperature_2m_mean",
    "temperature_2m_max",
    "temperature_2m_min",
    "precipitation_sum",
    "soil_moisture_0_to_7cm_mean",
    "soil_moisture_7_to_28cm_mean",
    "soil_moisture_28_to_100cm_mean",
    "soil_temperature_0_to_7cm_mean",
    "soil_temperature_7_to_28cm_mean",
    "et0_fao_evapotranspiration",
    "vapor_pressure_deficit_max",
    "shortwave_radiation_sum",
    sep = ","
  )
  
  url <- sprintf(
    "https://archive-api.open-meteo.com/v1/archive?latitude=%.4f&longitude=%.4f&start_date=%s&end_date=%s&daily=%s&timezone=Europe%%2FBerlin",
    lat, lon, start_str, end_str, vars
  )
  
  message("  Lade Meteo-Daten via Open-Meteo API (", start_str, " bis ", end_str, ")...")
  res <- jsonlite::fromJSON(url)
  
  meteo_df <- tibble::tibble(
    Datum = as.Date(res$daily$time),
    Temp_mean = as.numeric(res$daily$temperature_2m_mean),
    Temp_max = as.numeric(res$daily$temperature_2m_max),
    Temp_min = as.numeric(res$daily$temperature_2m_min),
    Precip_mm = as.numeric(res$daily$precipitation_sum),
    Soil_Moist_0_7 = as.numeric(res$daily$soil_moisture_0_to_7cm_mean),
    Soil_Moist_7_28 = as.numeric(res$daily$soil_moisture_7_to_28cm_mean),
    Soil_Moist_28_100 = as.numeric(res$daily$soil_moisture_28_to_100cm_mean),
    Soil_Temp_0_7 = as.numeric(res$daily$soil_temperature_0_to_7cm_mean),
    Soil_Temp_7_28 = as.numeric(res$daily$soil_temperature_7_to_28cm_mean),
    ET0 = as.numeric(res$daily$et0_fao_evapotranspiration),
    VPD_max = as.numeric(res$daily$vapor_pressure_deficit_max),
    Solar_Rad = as.numeric(res$daily$shortwave_radiation_sum)
  ) |>
    dplyr::mutate(
      # Kulturspezifisches GDD (Lufttemperatur über T_base,crop)
      GDD = ifelse(!is.na(Temp_mean) & Temp_mean > base_temp, Temp_mean - base_temp, 0),
      # Tiefengewichtete Oberboden-Temperatur und -Feuchte (0-28 cm: 25% 0-7cm + 75% 7-28cm)
      Soil_Temp_0_28 = ifelse(
        !is.na(Soil_Temp_0_7) & !is.na(Soil_Temp_7_28),
        0.25 * Soil_Temp_0_7 + 0.75 * Soil_Temp_7_28,
         dplyr::coalesce(Soil_Temp_7_28, Soil_Temp_0_7, Temp_mean)
      ),
      Soil_Moist_0_28 = ifelse(
        !is.na(Soil_Moist_0_7) & !is.na(Soil_Moist_7_28),
        0.25 * Soil_Moist_0_7 + 0.75 * Soil_Moist_7_28,
        dplyr::coalesce(Soil_Moist_7_28, Soil_Moist_0_7)
      ),
      # Boden-/Mikroben-GDD (Soil Degree Days, T_base,soil = 0 °C nach Honeycutt et al. 1988; Griffin & Honeycutt 2000)
      # Schichtweise trunkiert (pmax(0, T)), damit Bodenfrost in 0-7 cm nicht die Aktivität in 7-28 cm negiert
      GDD_soil = ifelse(
        !is.na(Soil_Temp_0_7) & !is.na(Soil_Temp_7_28),
        0.25 * pmax(0, Soil_Temp_0_7) + 0.75 * pmax(0, Soil_Temp_7_28),
        pmax(0, dplyr::coalesce(Soil_Temp_7_28, Soil_Temp_0_7, Temp_mean, 0))
      ),
      # Tägliche Klimatische Wasserbilanz (mm)
      CWB_mm = Precip_mm - dplyr::coalesce(ET0, 0)
    )
  
  return(meteo_df)
}

#' Lade tägliche Wetterdaten direkt über eine Standort-Konfigurationsliste (site_config)
#'
#' @param site_config Liste mit Feldern: latitude (oder lat), longitude (oder lon), optional crop und/oder base_temp
#' @param start_date Startdatum
#' @param end_date Enddatum
#' @return Tibble mit Spalten: Datum, Temp_mean, Precip_mm, GDD
#' @export
fetch_meteo_site <- function(site_config, start_date, end_date) {
  lat <- site_config$latitude %||% site_config$lat
  lon <- site_config$longitude %||% site_config$lon
  
  # Basistemperatur agronomisch/kulturspezifisch ermitteln:
  base_temp <- if (!is.null(site_config$base_temp)) {
    site_config$base_temp
  } else if (!is.null(site_config$crop)) {
    get_crop_base_temp(site_config$crop)
  } else {
    6.0
  }
  
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

#' Berechnet kumulierte und mittlere Meteo-/Boden-Kovariaten für die definierten Zeitintervalle
#'
#' @param df Tibble mit t1, t2 und optional Saattag
#' @param meteo_df Tibble der täglichen Wetterdaten (Spalten: Datum, Precip_mm, GDD, Soil_Temp_0_7, Soil_Temp_7_28, etc.)
#' @param t1_col Name der t1-Spalte (Default: "t1")
#' @param t2_col Name der t2-Spalte (Default: "t2")
#' @param saattag_col Name der SaatTag_Date-Spalte (Default: "SaatTag_Date")
#' @return Tibble mit Intervall-Summen (Precip, ET0, CWB, GDD, GDD_soil) und Intervall-Mittelwerten (Soil_Temp, Soil_Moist, VPD, Solar_Rad)
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
  
  # Falls meteo_df aus älterem Cache stammt, abgeleitete Boden-Spalten ergänzen
  if (!"Soil_Temp_0_28" %in% names(meteo_df)) {
    meteo_df <- meteo_df |>
      dplyr::mutate(
        Soil_Temp_0_28 = ifelse(
          !is.na(Soil_Temp_0_7) & !is.na(Soil_Temp_7_28),
          0.25 * Soil_Temp_0_7 + 0.75 * Soil_Temp_7_28,
          dplyr::coalesce(Soil_Temp_7_28, Soil_Temp_0_7, Temp_mean)
        ),
        Soil_Moist_0_28 = ifelse(
          !is.na(Soil_Moist_0_7) & !is.na(Soil_Moist_7_28),
          0.25 * Soil_Moist_0_7 + 0.75 * Soil_Moist_7_28,
          dplyr::coalesce(Soil_Moist_7_28, Soil_Moist_0_7)
        ),
        GDD_soil = ifelse(
          !is.na(Soil_Temp_0_7) & !is.na(Soil_Temp_7_28),
          0.25 * pmax(0, Soil_Temp_0_7) + 0.75 * pmax(0, Soil_Temp_7_28),
          pmax(0, dplyr::coalesce(Soil_Temp_7_28, Soil_Temp_0_7, Temp_mean, 0))
        ),
        CWB_mm = Precip_mm - dplyr::coalesce(ET0, 0)
      )
  }
  
  has_saattag <- saattag_col %in% names(df)
  
  df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      Cum_Precip_mm = if (!is.na(.data[[t1_col]]) && !is.na(.data[[t2_col]]) && .data[[t2_col]] > .data[[t1_col]]) {
        sum(meteo_df$Precip_mm[meteo_df$Datum > .data[[t1_col]] & meteo_df$Datum <= .data[[t2_col]]], na.rm = TRUE)
      } else {
        NA_real_
      },
      Cum_ET0_mm = if (!is.na(.data[[t1_col]]) && !is.na(.data[[t2_col]]) && .data[[t2_col]] > .data[[t1_col]]) {
        sum(meteo_df$ET0[meteo_df$Datum > .data[[t1_col]] & meteo_df$Datum <= .data[[t2_col]]], na.rm = TRUE)
      } else {
        NA_real_
      },
      Cum_CWB_mm = if (!is.na(.data[[t1_col]]) && !is.na(.data[[t2_col]]) && .data[[t2_col]] > .data[[t1_col]]) {
        sum(meteo_df$CWB_mm[meteo_df$Datum > .data[[t1_col]] & meteo_df$Datum <= .data[[t2_col]]], na.rm = TRUE)
      } else {
        NA_real_
      },
      Cum_GDD_Interval = if (!is.na(.data[[t1_col]]) && !is.na(.data[[t2_col]]) && .data[[t2_col]] > .data[[t1_col]]) {
        sum(meteo_df$GDD[meteo_df$Datum > .data[[t1_col]] & meteo_df$Datum <= .data[[t2_col]]], na.rm = TRUE)
      } else {
        NA_real_
      },
      Cum_GDD_Soil_Interval = if (!is.na(.data[[t1_col]]) && !is.na(.data[[t2_col]]) && .data[[t2_col]] > .data[[t1_col]]) {
        sum(meteo_df$GDD_soil[meteo_df$Datum > .data[[t1_col]] & meteo_df$Datum <= .data[[t2_col]]], na.rm = TRUE)
      } else {
        NA_real_
      },
      Mean_Soil_Temp_0_7_Interval = if (!is.na(.data[[t1_col]]) && !is.na(.data[[t2_col]]) && .data[[t2_col]] > .data[[t1_col]]) {
        mean(meteo_df$Soil_Temp_0_7[meteo_df$Datum > .data[[t1_col]] & meteo_df$Datum <= .data[[t2_col]]], na.rm = TRUE)
      } else {
        NA_real_
      },
      Mean_Soil_Temp_7_28_Interval = if (!is.na(.data[[t1_col]]) && !is.na(.data[[t2_col]]) && .data[[t2_col]] > .data[[t1_col]]) {
        mean(meteo_df$Soil_Temp_7_28[meteo_df$Datum > .data[[t1_col]] & meteo_df$Datum <= .data[[t2_col]]], na.rm = TRUE)
      } else {
        NA_real_
      },
      Mean_Soil_Temp_0_28_Interval = if (!is.na(.data[[t1_col]]) && !is.na(.data[[t2_col]]) && .data[[t2_col]] > .data[[t1_col]]) {
        mean(meteo_df$Soil_Temp_0_28[meteo_df$Datum > .data[[t1_col]] & meteo_df$Datum <= .data[[t2_col]]], na.rm = TRUE)
      } else {
        NA_real_
      },
      Mean_Soil_Moist_0_7_Interval = if (!is.na(.data[[t1_col]]) && !is.na(.data[[t2_col]]) && .data[[t2_col]] > .data[[t1_col]]) {
        mean(meteo_df$Soil_Moist_0_7[meteo_df$Datum > .data[[t1_col]] & meteo_df$Datum <= .data[[t2_col]]], na.rm = TRUE)
      } else {
        NA_real_
      },
      Mean_Soil_Moist_7_28_Interval = if (!is.na(.data[[t1_col]]) && !is.na(.data[[t2_col]]) && .data[[t2_col]] > .data[[t1_col]]) {
        mean(meteo_df$Soil_Moist_7_28[meteo_df$Datum > .data[[t1_col]] & meteo_df$Datum <= .data[[t2_col]]], na.rm = TRUE)
      } else {
        NA_real_
      },
      Mean_Soil_Moist_0_28_Interval = if (!is.na(.data[[t1_col]]) && !is.na(.data[[t2_col]]) && .data[[t2_col]] > .data[[t1_col]]) {
        mean(meteo_df$Soil_Moist_0_28[meteo_df$Datum > .data[[t1_col]] & meteo_df$Datum <= .data[[t2_col]]], na.rm = TRUE)
      } else {
        NA_real_
      },
      Mean_VPD_Interval = if (!is.na(.data[[t1_col]]) && !is.na(.data[[t2_col]]) && .data[[t2_col]] > .data[[t1_col]]) {
        mean(meteo_df$VPD_max[meteo_df$Datum > .data[[t1_col]] & meteo_df$Datum <= .data[[t2_col]]], na.rm = TRUE)
      } else {
        NA_real_
      },
      Mean_Solar_Rad_Interval = if (!is.na(.data[[t1_col]]) && !is.na(.data[[t2_col]]) && .data[[t2_col]] > .data[[t1_col]]) {
        mean(meteo_df$Solar_Rad[meteo_df$Datum > .data[[t1_col]] & meteo_df$Datum <= .data[[t2_col]]], na.rm = TRUE)
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

#' Berechnet die Zwei-Pool-Düngerdiskontierung (First-Order Decay über GDD_soil, 0-28 cm)
#'
#' Diskontiert jede Düngergabe j (zum Datum fert_date) über die tiefengewichteten
#' Boden-Wachstumsgradtage (GDD_soil, T_base = 0 °C; Honeycutt et al. 1988, Griffin & Honeycutt 2000)
#' für jedes Messintervall (t1, t2]:
#' - Pool 1 (N_fast_kgha): Mineralischer N + NH4-N (nach Abzug von NH3-Volatilisation), Rate k_fast
#' - Pool 2 (N_slow_kgha): Organisch gebundener N (Mist, Gülle), Rate k_slow
#'
#' @param df Tibble mit Messintervallen (Spalten t1_col, t2_col sowie join_col)
#' @param fert_events Tibble der Dünge-Ereignisse mit Spalten: join_col, fert_date, N_fast_kgha, N_slow_kgha
#' @param meteo_df Tibble der täglichen Wetterdaten mit Datum und GDD_soil
#' @param join_col Name der Spalte zur Verknüpfung (z.B. "Verfahren" oder "plot_nr")
#' @param t1_col Name der t1-Spalte (Default: "t1")
#' @param t2_col Name der t2-Spalte (Default: "t2")
#' @param k_fast Abbaurate für schnell verfügbaren Mineral-/NH4-Pool pro GDD_soil (Default: 0.008, T_1/2 ~ 87 GDD_soil)
#' @param k_slow Mineralisationsrate für organischen Pool pro GDD_soil (Default: 0.00012, ~28% pro 2800 GDD_soil)
#' @return df ergänzt um Fert_N_fast_Interval, Fert_N_slow_Interval, Fert_N_total_Interval
#' @export
add_fertilizer_discounts <- function(df,
                                     fert_events,
                                     meteo_df,
                                     join_col = "Verfahren",
                                     t1_col = "t1",
                                     t2_col = "t2",
                                     k_fast = 0.008,
                                     k_slow = 0.00012) {
  if (!"GDD_soil" %in% names(meteo_df)) {
    meteo_df <- meteo_df |>
      dplyr::mutate(
        GDD_soil = ifelse(
          !is.na(Soil_Temp_0_7) & !is.na(Soil_Temp_7_28),
          0.25 * pmax(0, Soil_Temp_0_7) + 0.75 * pmax(0, Soil_Temp_7_28),
          pmax(0, dplyr::coalesce(Soil_Temp_7_28, Soil_Temp_0_7, Temp_mean, 0))
        )
      )
  }
  
  # Kumulative GDD_soil-Zeitreihe für schnellen Lookup (O(1) Differenzbildung)
  meteo_lut <- meteo_df |>
    dplyr::arrange(Datum) |>
    dplyr::distinct(Datum, .keep_all = TRUE) |>
    dplyr::mutate(cum_gdd_soil = cumsum(dplyr::coalesce(GDD_soil, 0)))
  
  get_cum_gdd <- function(d) {
    if (is.na(d)) return(NA_real_)
    idx <- which(meteo_lut$Datum <= as.Date(d))
    if (length(idx) == 0) return(0)
    meteo_lut$cum_gdd_soil[max(idx)]
  }
  
  fert_events <- fert_events |>
    dplyr::mutate(
      fert_date = as.Date(fert_date),
      gdd_fert = purrr::map_dbl(fert_date, get_cum_gdd)
    )
  
  df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      Fert_N_fast_Interval = {
        t1_val <- .data[[t1_col]]
        t2_val <- .data[[t2_col]]
        key_val <- as.character(.data[[join_col]])
        if (is.na(t1_val) || is.na(t2_val) || t2_val <= t1_val || is.na(key_val)) {
          0
        } else {
          ev <- fert_events[as.character(fert_events[[join_col]]) == key_val & fert_events$fert_date <= t2_val, , drop = FALSE]
          if (nrow(ev) == 0) {
            0
          } else {
            gdd_t1 <- get_cum_gdd(t1_val)
            gdd_t2 <- get_cum_gdd(t2_val)
            dgdd_1 <- pmax(0, gdd_t1 - ev$gdd_fert)
            dgdd_2 <- pmax(0, gdd_t2 - ev$gdd_fert)
            sum(ev$N_fast_kgha * (exp(-k_fast * dgdd_1) - exp(-k_fast * dgdd_2)), na.rm = TRUE)
          }
        }
      },
      Fert_N_slow_Interval = {
        t1_val <- .data[[t1_col]]
        t2_val <- .data[[t2_col]]
        key_val <- as.character(.data[[join_col]])
        if (is.na(t1_val) || is.na(t2_val) || t2_val <= t1_val || is.na(key_val)) {
          0
        } else {
          ev <- fert_events[as.character(fert_events[[join_col]]) == key_val & fert_events$fert_date <= t2_val, , drop = FALSE]
          if (nrow(ev) == 0) {
            0
          } else {
            gdd_t1 <- get_cum_gdd(t1_val)
            gdd_t2 <- get_cum_gdd(t2_val)
            dgdd_1 <- pmax(0, gdd_t1 - ev$gdd_fert)
            dgdd_2 <- pmax(0, gdd_t2 - ev$gdd_fert)
            sum(ev$N_slow_kgha * (exp(-k_slow * dgdd_1) - exp(-k_slow * dgdd_2)), na.rm = TRUE)
          }
        }
      },
      Fert_N_total_Interval = Fert_N_fast_Interval + Fert_N_slow_Interval
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

#' Geokodiert eine Postleitzahl (PLZ) über die Nominatim API
#'
#' @param plz Die Postleitzahl als String oder Numeric
#' @param country Das Land (Default: "Switzerland")
#' @return Eine Liste mit lat und lon, oder NULL bei Fehler
#' @export
fetch_coords <- function(plz, country = "Switzerland") {
  url <- sprintf("https://nominatim.openstreetmap.org/search?postalcode=%s&country=%s&format=json", 
                 utils::URLencode(as.character(plz)), utils::URLencode(country))
  
  message("  Sammle Koordinaten für PLZ: ", plz, " via Nominatim...")
  # Nominatim verlangt eigentlich einen User-Agent, aber fromJSON reicht oft für wenige Requests.
  # Pause einfügen, um Nominatim Rate Limits (1 req/sec) zu respektieren:
  Sys.sleep(1.1)
  
  tryCatch({
    res <- jsonlite::fromJSON(url)
    if (is.data.frame(res) && nrow(res) > 0) {
      return(list(
        lat = as.numeric(res$lat[1]),
        lon = as.numeric(res$lon[1])
      ))
    } else {
      warning("Keine Koordinaten gefunden für PLZ: ", plz)
    }
  }, error = function(e) {
    message("Fehler beim Geokodieren der PLZ ", plz, ": ", e$message)
  })
  
  return(NULL)
}
