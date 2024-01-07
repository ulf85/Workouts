#' Auslesen und Aufbereiten der Daten fuer Indoor Cycling und Cycling
#'
#' @param path character-String mit dem Pfad zu dem CSV-File
#' @param file character-String mit dem Namen des CSV-Files
#'
#' @return rawData data.frame mit den aufbereiteten Daten
#' @importFrom readr read_csv
#' @importFrom readr cols
#' @export
#'

getIndoorCyclingData <- function(path, file) {

  rawData <- readr::read_csv(file = paste0(path, file), col_types = readr::cols())

  rawData$Zeit <- as.POSIXct(rawData$Time,
                             origin = rawData$Date[1], format = "%H:%M:%S")
  if (is.null(rawData$Cadence)) rawData$Cadence <- NA

  rawData <- rawData %>% rename("Herzrate" = "Heartrate",
                                "Datum" = "Date",
                                "Geschwindigkeit" = "Speed",
                                "Trittanzahl" = "Cadence")
  rawData$Geschwindigkeit[is.na(rawData$Geschwindigkeit)] <- 0
  rawData$Geschwindigkeit <- rawData$Geschwindigkeit * 3.6
  rawData$tmp_Distanz <- rawData$Geschwindigkeit * 1 / (60 * 60)
  rawData$Distanz <- cumsum(rawData$tmp_Distanz)

  # Pausen als inaktiv markieren; Indiz: min 10 Sek Geschwindigkeit = 0
  rawData$aktiv <- TRUE
  ind <- which(rawData$Geschwindigkeit == 0)
  n <- length(rawData$Geschwindigkeit)
  for (i in ind) {
    if (all(rawData$Geschwindigkeit[i:min(n, (i + 10))] == 0)) {
      rawData$aktiv[i] <- FALSE
    }
  }

  return(rawData)
}



#' Plot fuer Indoor Cycling oder Cycling Daten
#'
#' @param rawData data.frame mit den Fitness-Daten
#' @param type character-String; "IndoorCycling" (default) oder "Cycling
#'
#' @note Der Plot für die Herzrate hat jetzt als neues Feature noch die Herzratzonen farblich hinterlegt.
#' Die Zonen basieren auf meinen Daten (männlich, 35 Jahre, 80km, 1.86m)
#'
#' @return ggplot2-Objekt
#' @export
#' @import ggplot2
#'

plotIndoorCycling <- function(rawData, type = "IndoorCycling") {

  tag <- rawData$Datum[1]
  Trainingszeit <- nrow(rawData[rawData$aktiv == TRUE, ]) / 60
  Trainingszeit <- as.difftime(round(as.numeric(Trainingszeit, units = "mins"), 2), units = "mins")

  rawData2 <- rawData
  # Es kann vorkommen, dass die Trittanzahl nicht gemessen wird oder nicht korrekt gemessen wird
  # wenn die Anzahl der NAs zu groß ist, macht es keinen Sinn diese auszuwerten
  # als Seiteneffekt werden hier auch noch die NAs gefiltert
  if (type == "Cycling") {
    if (all(is.na(rawData$Trittanzahl)) || sum(is.na(rawData$Trittanzahl)) / nrow(rawData) > 0.5) {
      rawData <- rawData[rawData$Geschwindigkeit > 10, ]
    } else {
      rawData <- rawData[rawData$Geschwindigkeit > 10 & rawData$Trittanzahl > 10, ]
    }
  }

  if (all(is.na(rawData$Trittanzahl)) || sum(is.na(rawData$Trittanzahl)) / nrow(rawData) > 0.5) {
    tidyData <- rawData %>%
      filter(aktiv == TRUE) %>%
      select(Zeit, Geschwindigkeit, Herzrate) %>%
      gather("Feature", "Wert", 2:3)

  } else {
    tidyData <- rawData %>%
      filter(aktiv == TRUE) %>%
      select(Zeit, Geschwindigkeit, Herzrate, Trittanzahl) %>%
      gather("Feature", "Wert", 2:4)
  }

  p <- ggplot(data = tidyData, aes(x = Zeit, y = Wert)) +
    geom_line() +
    ylab(NULL) +
    scale_x_datetime(breaks = waiver(), labels = date_format("%H:%M")) +
    ggtitle(paste0(as.character(format(tag, format = "%d.%m.%Y")), " (", type, " ",
                   round(sum(rawData2$tmp_Distanz), 2), "km, ",
                   Trainingszeit, units(Trainingszeit), ")"))

  if ("Trittanzahl" %in% tidyData$Feature) {
    p <- p + facet_wrap(~Feature, nrow = 1, scales = "free_y", strip.position = "left",
                        labeller = as_labeller(c(Geschwindigkeit = "km/h",
                                                 Herzrate = "bpm",
                                                 Trittanzahl = "rpm")))
  } else {
    p <- p + facet_wrap(~Feature, nrow = 1, scales = "free_y", strip.position = "left",
                        labeller = as_labeller(c(Geschwindigkeit = "km/h",
                                                 Herzrate = "bpm")))
  }

  if (type == "Cycling") {
    p <- p + geom_smooth(method = "auto")
  }


  zeitspanne <- range(tidyData$Zeit, na.rm = TRUE)
  mitte <- zeitspanne[1] + diff(zeitspanne) / 2
  rangeHerzrate <- range(tidyData$Wert[tidyData$Feature == "Herzrate"], na.rm = TRUE)
  cutHerzrate <- cut(x = tidyData$Wert[tidyData$Feature == "Herzrate"],
                     breaks = c(1, 120, 140, 158, 177, 200))
  tableHerzrate <- table(cutHerzrate)
  tableHerzrate <- tableHerzrate[tableHerzrate > 0] / sum(tableHerzrate[tableHerzrate > 0]) * 100
  tableHerzrate <- round(tableHerzrate, 0)
  tableHerzrate <- as.data.frame(tableHerzrate)

  if (nrow(tableHerzrate) == 1) {
    tmp <- data.frame(cutHerzrate = rownames(tableHerzrate),
                      Freq = tableHerzrate[1, 1],
                      stringsAsFactors = FALSE)
    tableHerzrate <- tmp
  }

  if (rangeHerzrate[1] < 120) {
    p <- p +
      geom_rect(data = data.frame(Feature = rep("Herzrate", 1), stringsAsFactors = FALSE),
                aes(ymin = rangeHerzrate[1],
                    ymax = 120,
                    xmin = min(tidyData$Zeit),
                    xmax = max(tidyData$Zeit)
                ),
                alpha = 0.1,
                fill = c("blue"),
                inherit.aes = FALSE) +
      geom_text(data = data.frame(Feature = "Herzrate", stringsAsFactors = FALSE),
                aes(x = mitte, y = 120, label = c("Leicht")),
                alpha = 0.5,
                color = c("blue"),
                nudge_y = -1,
                inherit.aes = FALSE) +
      geom_text(data = data.frame(Feature = "Herzrate", stringsAsFactors = FALSE),
                aes(x = mitte, y = 110,
                    label = paste0(tableHerzrate$Freq[tableHerzrate$cutHerzrate == "(1,120]"], "%")),
                alpha = 0.5,
                color = c("blue"),
                nudge_y = -1,
                size = 6,
                inherit.aes = FALSE)
  }
  if (rangeHerzrate[1] < 140 &&
      rangeHerzrate[2] >= 120) {
    p <- p +
      geom_rect(data = data.frame(Feature = rep("Herzrate", 1), stringsAsFactors = FALSE),
                aes(ymin = max(120, rangeHerzrate[1]),
                    ymax = 140,
                    xmin = min(tidyData$Zeit),
                    xmax = max(tidyData$Zeit)
                ),
                alpha = 0.1,
                fill = c("green"),
                inherit.aes = FALSE) +
      geom_text(data = data.frame(Feature = "Herzrate", stringsAsFactors = FALSE),
                aes(x = mitte, y = 140, label = c("Fettverbrennung")),
                alpha = 0.5,
                color = c("darkgreen"),
                nudge_y = -1,
                inherit.aes = FALSE) +
      geom_text(data = data.frame(Feature = "Herzrate", stringsAsFactors = FALSE),
                aes(x = mitte, y = 130,
                    label = paste0(tableHerzrate$Freq[tableHerzrate$cutHerzrate == "(120,140]"], "%")),
                alpha = 0.5,
                color = c("darkgreen"),
                nudge_y = -1,
                size = 6,
                inherit.aes = FALSE)
  }
  if (rangeHerzrate[1] < 158 &&
      rangeHerzrate[2] >= 140) {
    p <- p +
      geom_rect(data = data.frame(Feature = rep("Herzrate", 1), stringsAsFactors = FALSE),
                aes(ymin = max(140, rangeHerzrate[1]),
                    ymax = 158,
                    xmin = min(tidyData$Zeit),
                    xmax = max(tidyData$Zeit)
                ),
                alpha = 0.1,
                fill = c("yellow"),
                inherit.aes = FALSE) +
      geom_text(data = data.frame(Feature = "Herzrate", stringsAsFactors = FALSE),
                aes(x = mitte, y = 158, label = c("Kardio")),
                alpha = 0.5,
                color = c("orange"),
                nudge_y = -1,
                inherit.aes = FALSE) +
      geom_text(data = data.frame(Feature = "Herzrate", stringsAsFactors = FALSE),
                aes(x = mitte, y = 150,
                    label = paste0(tableHerzrate$Freq[tableHerzrate$cutHerzrate == "(140,158]"], "%")),
                alpha = 0.5,
                color = c("orange"),
                nudge_y = -1,
                size = 6,
                inherit.aes = FALSE)
  }
  if (rangeHerzrate[1] < 177 &&
      rangeHerzrate[2] >= 158) {
    p <- p +
      geom_rect(data = data.frame(Feature = rep("Herzrate", 1), stringsAsFactors = FALSE),
                aes(ymin = max(158, rangeHerzrate[1]),
                    ymax = 177,
                    xmin = min(tidyData$Zeit),
                    xmax = max(tidyData$Zeit)
                ),
                alpha = 0.1,
                fill = c("red"),
                inherit.aes = FALSE) +
      geom_text(data = data.frame(Feature = "Herzrate", stringsAsFactors = FALSE),
                aes(x = mitte, y = 177, label = c("Schwierig")),
                alpha = 0.5,
                color = c("red"),
                nudge_y = -1,
                inherit.aes = FALSE) +
      geom_text(data = data.frame(Feature = "Herzrate", stringsAsFactors = FALSE),
                aes(x = mitte, y = 170,
                    label = paste0(tableHerzrate$Freq[tableHerzrate$cutHerzrate == "(158,177]"], "%")),
                alpha = 0.5,
                color = c("red"),
                nudge_y = -1,
                size = 6,
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
                inherit.aes = FALSE) +
      geom_text(data = data.frame(Feature = "Herzrate", stringsAsFactors = FALSE),
                aes(x = mitte, y = 180,
                    label = paste0(tableHerzrate$Freq[tableHerzrate$cutHerzrate == "(177,200]"], "%")),
                alpha = 0.5,
                color = c("pink"),
                nudge_y = -1,
                size = 6,
                inherit.aes = FALSE)
  }

  return(p)
}

###################

#' Erzeuge eine Zusammenfassung der Daten fuer Indoor Cycling oder Cycling
#'
#' @param indoorCyclingData data.frame mit den Fitness-Daten
#'
#' @return summaryIndoorCycling data.frame
#' @export
#' @import dplyr
#' @importFrom stats sd
#'

getSummaryIndoorCycling <- function(indoorCyclingData) {
  summaryIndoorCycling1 <- indoorCyclingData %>%
    group_by(trainingNR, Datum) %>%
    summarise(Mean = mean(Geschwindigkeit, na.rm = TRUE),
              Sd = sd(Geschwindigkeit, na.rm = TRUE),
              Min = min(Geschwindigkeit, na.rm = TRUE),
              Max = max(Geschwindigkeit, na.rm = TRUE)) %>%
    mutate(Messwert = "Geschwindigkeit")

  summaryIndoorCycling2 <- indoorCyclingData %>%
    group_by(trainingNR, Datum) %>%
    summarise(Mean = mean(Herzrate, na.rm = TRUE),
              Sd = sd(Herzrate, na.rm = TRUE),
              Min = min(Herzrate, na.rm = TRUE),
              Max = max(Herzrate, na.rm = TRUE)) %>%
    mutate(Messwert = "Herzrate")

  summaryIndoorCycling3 <- indoorCyclingData %>%
    group_by(trainingNR, Datum) %>%
    summarise(Mean = mean(Trittanzahl, na.rm = TRUE),
              Sd = sd(Trittanzahl, na.rm = TRUE),
              Min = min(Trittanzahl, na.rm = TRUE),
              Max = max(Trittanzahl, na.rm = TRUE)) %>%
    mutate(Messwert = "Trittanzahl")

  summaryIndoorCycling <- bind_rows(summaryIndoorCycling1,
                                    summaryIndoorCycling2,
                                    summaryIndoorCycling3)
  summaryIndoorCycling <- summaryIndoorCycling %>%
    left_join(indoorCyclingData %>% distinct(trainingNR, Datum))
  summaryIndoorCycling <- summaryIndoorCycling %>% arrange(Datum)

  return(summaryIndoorCycling)
}
