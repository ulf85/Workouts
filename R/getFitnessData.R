#' Einlesen der Fitness-Daten aus den CSV Dateien
#'
#' @author Ulf Schepsmeier
#'
#' @param ordner character-String mit dem Ordner-Namen
#' "walking", "indoorCycling", "running" oder "cycling"
#' @param path character-string mit dem Pfad-Namen bis zu dem Ordner
#' @param skip vector mit Indices mit zu überspringenden Dateien in dem Ordner;
#' default: skip = NULL
#' @param trainingNR eine Art Index; default: trainingNR = 1; kann übergeben werden wenn mit
#' delta-Verfahren die Daten geladen werden. Dann nächste noch nicht vergebene Nummer
#'
#' @return Data eingelesene Daten
#' @export
#' @import dplyr
#'

getFitnessData <- function(ordner, path, skip = NULL, trainingNR = 1) {
  if (is.null(ordner) || length(ordner) == 0) return(NULL)
  if (length(ordner) > 1) return(NULL)
  if (!(ordner %in% c("walking", "indoorCycling", "running", "cycling"))) return(NULL)

  path <- paste0(path, "/", ordner, "/fixed/")
  files <- dir(path = path)

  if (!is.null(skip) && length(skip) > 0 && is.numeric(skip)) files <- files[-skip]
  if (length(files) <= 0) return(NULL)

  Data <- NULL
  for (file in files) {
    if (ordner %in% c("walking", "running")) {
      rawData <- getWalkingData(path, file)
    } else {
      rawData <- getIndoorCyclingData(path, file)
    }

    rawData$trainingNR <- trainingNR
    trainingNR <- trainingNR + 1

    Data <- bind_rows(Data, rawData)
  }
  return(Data)
}
