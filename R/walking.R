#' Auslesen und Aufbereitung der Daten fuer Walking und Running
#'
#' @author Ulf Schepsmeier
#'
#' @param path character-String mit dem Pfad zu der Datei
#' @param file character-String mit dem Namen der csv-Datei
#'
#' @return rawData data.frame mit den eingelesenen und aufbereiteten Daten
#' @export
#' @import readr
#' @import dplyr
#'
#' @examples
getWalkingData <- function(path, file) {

  rawData <- read_csv(file = paste0(path, file), col_types = cols())

  rawData$Zeit <- as.POSIXct(rawData$Time,
                             origin = rawData$Date[1], format = "%H:%M:%S")

  ind <- which(rawData$Latitude != 0)[1]

  rawData$Latitude[1:(ind - 1)] <- rawData$Latitude[ind]
  rawData$Longitude[1:(ind - 1)] <- rawData$Longitude[ind]
  rawData$Altitude[1:(ind - 1)] <- rawData$Altitude[ind]

  ind <- which(rawData$Latitude == 0)
  for (i in ind) {
    rawData$Latitude[i] <- rawData$Latitude[i - 1]
    rawData$Longitude[i] <- rawData$Longitude[i - 1]
    rawData$Altitude[i] <- rawData$Altitude[i - 1]
  }

  # Distanz von Punkt zu Punkt in km
  # https://www.kompf.de/gps/distcalc.html
  rawData$DiffLatitude <- c(0, diff(rawData$Latitude))
  rawData$DiffLongitude <- c(0, diff(rawData$Longitude))
  rawData$tmp_Distanz <- sqrt((111.3 * rawData$DiffLatitude)^2 +
                                (71.5 * rawData$DiffLongitude)^2)
  rawData$Distanz <- cumsum(rawData$tmp_Distanz)

  # Aus Distanz und Zeit kann man denn die Geschwindigkeit berechnen
  rawData$Geschwindigkeit <- rawData$tmp_Distanz / (1 / 60 / 60)

  # Pausen als inaktiv markieren; Indiz: min 10 Sek Geschwindigkeit = 0
  rawData$aktiv <- TRUE
  ind <- which(rawData$Geschwindigkeit == 0)
  n <- length(rawData$Geschwindigkeit)
  for (i in ind) {
    if (all(rawData$Geschwindigkeit[i:min(n, (i + 10))] == 0)) {
      rawData$aktiv[i] <- FALSE
    }
  }

  rawData <- rawData %>% rename("Herzrate" = "Heartrate", "Datum" = "Date")

  return(rawData)
}


#' Plot fuer Walking oder Running Daten
#'
#' @author Ulf Schepsmeier
#'
#' @param rawData data.frame mit den selektierten Daten
#' @param type charater-String; "Walking" (default) oder "Running"
#'
#' @return p ggplot2-Objekt
#' @export
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#'
#' @examples
plotWalking <- function(rawData, type = "Walking") {

  tag <- rawData$Datum[1]
  Trainingszeit <- nrow(rawData[rawData$aktiv == TRUE, ]) / 60
  Trainingszeit <- as.difftime(round(as.numeric(Trainingszeit, units = "mins"), 2),
                               units = "mins")

  tidyData <- rawData %>%
    filter(aktiv == TRUE) %>%
    select(Zeit, Geschwindigkeit, Herzrate) %>%
    gather("Feature", "Wert", 2:3)

  p <- ggplot(data = tidyData, aes(x = Zeit, y = Wert)) +
    geom_smooth(method = "auto") +
    facet_wrap(~Feature, nrow = 1, scales = "free_y", strip.position = "left",
               labeller = as_labeller(c(Geschwindigkeit = "km/h", Herzrate = "bpm"))) +
    ylab(NULL) +
    scale_x_datetime(breaks = waiver(), labels = date_format("%H:%M")) +
    ggtitle(paste0(as.character.Date(tag, format = "%d.%m.%Y"), " (", type, " ",
                   round(sum(rawData$tmp_Distanz), 2), "km, ",
                   Trainingszeit, units(Trainingszeit), ")"))

  zeitspanne <- range(tidyData$Zeit, na.rm = TRUE)
  mitte <- zeitspanne[1] + diff(zeitspanne) / 2
  rangeHerzrate <- range(tidyData$Wert[tidyData$Feature == "Herzrate"], na.rm = TRUE)

  if (rangeHerzrate[1] < 120) {
    p <- p +
      geom_rect(data = data.frame(Feature = rep("Herzrate", 1), stringsAsFactors = FALSE),
                aes(ymin = rangeHerzrate[1],
                    ymax = min(120, rangeHerzrate[2]),
                    xmin = min(tidyData$Zeit),
                    xmax = max(tidyData$Zeit)
                ),
                alpha = 0.1,
                fill = c("blue"),
                inherit.aes = FALSE) +
      geom_text(data = data.frame(Feature = "Herzrate", stringsAsFactors = FALSE),
                aes(x = mitte, y = min(120, rangeHerzrate[2]), label = c("Leicht")),
                alpha = 0.5,
                color = c("blue"),
                nudge_y = -1,
                inherit.aes = FALSE)
  }
  if (rangeHerzrate[1] < 140 &
     rangeHerzrate[2] >= 120) {
    p <- p +
      geom_rect(data = data.frame(Feature = rep("Herzrate", 1), stringsAsFactors = FALSE),
                aes(ymin = max(120, rangeHerzrate[1]),
                    ymax = min(140, rangeHerzrate[2]),
                    xmin = min(tidyData$Zeit),
                    xmax = max(tidyData$Zeit)
                ),
                alpha = 0.1,
                fill = c("green"),
                inherit.aes = FALSE) +
      geom_text(data = data.frame(Feature = "Herzrate", stringsAsFactors = FALSE),
                aes(x = mitte, y = min(140, rangeHerzrate[2]), label = c("Fettverbrennung")),
                alpha = 0.5,
                color = c("darkgreen"),
                nudge_y = -1,
                inherit.aes = FALSE)
  }
  if (rangeHerzrate[1] < 158 &
     rangeHerzrate[2] >= 140) {
    p <- p +
      geom_rect(data = data.frame(Feature = rep("Herzrate", 1), stringsAsFactors = FALSE),
                aes(ymin = max(140, rangeHerzrate[1]),
                    ymax = min(158, rangeHerzrate[2]),
                    xmin = min(tidyData$Zeit),
                    xmax = max(tidyData$Zeit)
                ),
                alpha = 0.1,
                fill = c("yellow"),
                inherit.aes = FALSE) +
      geom_text(data = data.frame(Feature = "Herzrate", stringsAsFactors = FALSE),
                aes(x = mitte, y = min(158, rangeHerzrate[2]), label = c("Kardio")),
                alpha = 0.5,
                color = c("orange"),
                nudge_y = -1,
                inherit.aes = FALSE)
  }
  if (rangeHerzrate[1] < 177 &
     rangeHerzrate[2] >= 158) {
    p <- p +
      geom_rect(data = data.frame(Feature = rep("Herzrate", 1), stringsAsFactors = FALSE),
                aes(ymin = max(158, rangeHerzrate[1]),
                    ymax = min(177, rangeHerzrate[2]),
                    xmin = min(tidyData$Zeit),
                    xmax = max(tidyData$Zeit)
                ),
                alpha = 0.1,
                fill = c("red"),
                inherit.aes = FALSE) +
      geom_text(data = data.frame(Feature = "Herzrate", stringsAsFactors = FALSE),
                aes(x = mitte, y = min(177, rangeHerzrate[2]), label = c("Schwierig")),
                alpha = 0.5,
                color = c("red"),
                nudge_y = -1,
                inherit.aes = FALSE)
  }
  if (rangeHerzrate[2] >= 177) {
    p <- p +
      geom_rect(data = data.frame(Feature = rep("Herzrate", 1), stringsAsFactors = FALSE),
                aes(ymin = max(177, rangeHerzrate[1]),
                    ymax = rangeHerzrate[2],
                    xmin = min(tidyData$Zeit),
                    xmax = max(tidyData$Zeit)
                ),
                alpha = 0.1,
                fill = c("pink"),
                inherit.aes = FALSE) +
      geom_text(data = data.frame(Feature = "Herzrate", stringsAsFactors = FALSE),
                aes(x = mitte, y = rangeHerzrate[2], label = c("Spitze")),
                alpha = 0.5,
                color = c("pink"),
                nudge_y = -1,
                inherit.aes = FALSE)
  }

  return(p)
}


#' Erzeuge eine Zusammenfassung der Daten fuer Walking oder Running Daten
#'
#' @author Ulf Schepsmeier
#'
#' @param walkingData data.frame mit den selektierten Daten
#'
#' @return summaryWalking data.frame mit den aggregierten Daten
#' @export
#' @import dplyr
#'
#' @examples
getSummaryWalking <- function(walkingData) {
  summaryWalking1 <- walkingData %>%
    group_by(trainingNR) %>%
    summarise(Mean = mean(Geschwindigkeit, na.rm = TRUE),
              Sd = sd(Geschwindigkeit, na.rm = TRUE),
              Min = min(Geschwindigkeit, na.rm = TRUE),
              Max = max(Geschwindigkeit, na.rm = TRUE)) %>%
    mutate(Messwert = "Geschwindigkeit")

  summaryWalking2 <- walkingData %>%
    group_by(trainingNR) %>%
    summarise(Mean = mean(Herzrate, na.rm = TRUE),
              Sd = sd(Herzrate, na.rm = TRUE),
              Min = min(Herzrate, na.rm = TRUE),
              Max = max(Herzrate, na.rm = TRUE)) %>%
    mutate(Messwert = "Herzrate")


  summaryWalking <- bind_rows(summaryWalking1, summaryWalking2)

  summaryWalking <- summaryWalking %>%
    left_join(walkingData %>% distinct(trainingNR, Datum))

  return(summaryWalking)
}
