#' Erstellung einer Kennzahlenuebersicht
#'
#' Erzeuge eine Ueberischt der Fitnessdaten (Summary) mit den wichtigsten (aggregierten) Kennzahlen
#'
#' @author Ulf Schepsmeier
#'
#' @param data data.frame mit Messwerten (im richtigen Format)
#' @param sportart character-String; aktuell implementiert: "Indoor Cycling" (default),
#' "Cycling", "Walking", "Running"
#'
#' @note Die berechneten Kennzahlen sind:
#' * Trainingszeit
#' * Distanz
#' * Durchschnittsgeschwindigkeit
#' * Maximale Geschwindigkeit
#' * Durchschnittsherzrate
#' * Maximale Herzrate
#' * Standardabweichung bei der Herzrate (für interne Nutzung)
#' * Standardabweichung bei der Geschwindigkeit(für interne Nutzung)
#' * Durchschnittstrittanzahl (bei Rad, wenn vorhanden)
#' * Maximale Trittanzahl (bei Rad, wenn vorhanden)
#' * Standardabweichung bei der Trittanzahl (bei Rad, wenn vorhanden)
#'
#' @return uebersicht data.frame mit der Uebersicht
#' @export
#' @import dplyr
#' @import scales
#' @importFrom stats sd
#'

createUebersicht <- function(data, sportart = "Indoor Cycling") {
  uebersicht <- NULL

  if (!is.null(data)) {
    uebersicht <- data %>%
      filter(aktiv == TRUE) %>%
      group_by(Datum, trainingNR) %>%
      summarise(
        Trainingszeit = n() / 60,
        Distanz = ifelse(all(is.na(Distanz)), NA, round(max(Distanz, na.rm = TRUE), digits = 2)),
        "&empty; km/h" = round(mean(Geschwindigkeit, na.rm = TRUE), digits = 2),
        "Max km/h" = ifelse(all(is.na(Geschwindigkeit)), NA, round(max(Geschwindigkeit, na.rm = TRUE), digits = 2)),
        "&empty; bpm" = round(mean(Herzrate, na.rm = TRUE), digits = 2),
        "Max bpm" = ifelse(all(is.na(Herzrate)), NA, round(max(Herzrate, na.rm = TRUE), digits = 2)),
        sdBpm = sd(Herzrate, na.rm = TRUE),
        sdKmh = sd(Geschwindigkeit, na.rm = TRUE),
        .groups = "keep"
      ) %>%
      mutate(Sportart = sportart)

    if (sportart %in% c("Indoor Cycling", "Cycling")) {
      uebersicht2 <- data %>%
        filter(aktiv == TRUE) %>%
        group_by(Datum, trainingNR) %>%
        summarise(
          "&empty; rpm" = round(mean(Trittanzahl, na.rm = TRUE),
            digits = 2
          ),
          "Max rpm" = ifelse(all(is.na(Trittanzahl)), NA, max(Trittanzahl, na.rm = TRUE)),
          sdRpm = sd(Trittanzahl, na.rm = TRUE),
          .groups = "keep"
        )
      uebersicht <- uebersicht %>% inner_join(uebersicht2, by = join_by(Datum, trainingNR))
      uebersicht$`&empty; rpm` <- as.numeric(uebersicht$`&empty; rpm`)
      uebersicht$`Max rpm` <- as.numeric(uebersicht$`Max rpm`)
    }
    uebersicht$Distanz <- as.numeric(uebersicht$Distanz)
    uebersicht$`&empty; km/h` <- as.numeric(uebersicht$`&empty; km/h`)
    uebersicht$`Max km/h` <- as.numeric(uebersicht$`Max km/h`)
    uebersicht$`&empty; bpm` <- as.numeric(uebersicht$`&empty; bpm`)
    uebersicht$`Max bpm` <- as.numeric(uebersicht$`Max bpm`)

    if ("Altitude" %in% names(data) && !is.null(data$Altitude) && !all(is.na(data$Altitude))) {
      data$diffAltitude <- c(0, diff(data$Altitude))
      uebersicht3 <- data %>%
        filter(aktiv == TRUE) %>%
        group_by(Datum, trainingNR) %>%
        summarise(
          Aufstieg = sum(diffAltitude[diffAltitude > 0], na.rm = TRUE),
          Abstieg = -sum(diffAltitude[diffAltitude < 0], na.rm = TRUE),
          .groups = "keep"
        )
      uebersicht <- uebersicht %>% inner_join(uebersicht3, by = join_by(Datum, trainingNR))
    }
  }

  return(uebersicht)
}
