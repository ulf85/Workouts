#' .fit Dateien erkennen und einer Sportart zuordnen
#'
#' Nach einer Umstellung von Wahoo ist es schwieriger die .fit Dateien in Dropbox an die richtige Stelle zu tun.
#' Aktuell passiert das manuell. Um das zu automatisieren muss ich automatisch erkennen, ob es sich um
#' indoor Cycling, Cycling, walking oder running Daten handelt, um die .fit Datei in den richtigen Ordner zu kopieren.
#'
#' @author Ulf Schepsmeier
#'
#' @param path character-String mit dem Pfad zu dem fit-File
#' @param file character-String mit dem Namen der fit-Datei
#'
#' @return sportart
#'
#' @export
#' @importFrom readr read_csv

detectTypeOfSport <- function(path, file) {

  # .fit Datei temporär in csv umwandeln
  if (!grepl(pattern = "[/]+$", x = path)) {
    path <- paste0(path, "/")
  }

  outputPath <- paste0(path, "tmp/")

  fit2csv(path, file, outputPath)

  # .csv einlesen
  outputFile <- gsub(pattern = ".fit", replacement = ".csv", x = file)
  rawData <- readr::read_csv(file = paste0(outputPath, outputFile), col_types = readr::cols())


  ## Logik um die Sportarten zu unterscheiden

  # 1) wenn Latitute/Longitude existieren, dann war GPS an und damit waren wir draußen -> walking, running, cycling
  # ansonsten drinnen -> indoorCycling
  if (all(is.null(rawData$Latitude)) || all(rawData$Latitude == 0)) {
    sportart <- "indoorCycling"

    # tmp file löschen und raus
    file.remove(paste0(outputPath, outputFile))
    return(sportart)
  }

  # 2) wenn Cadence da ist, haben wir auch ein Indiz für cycling
  # da der Sensor aber nicht an allen Rädern ist, kann er auch leer sein
  if ("Cadence" %in% colnames(rawData) && !is.null(rawData$Cadence) && any(rawData$Cadence > 0)) {
    sportart <- "cycling"

    # tmp file löschen und raus
    file.remove(paste0(outputPath, outputFile))
    return(sportart)
  }

  # 3) bleibt uns noch die Geschwindigkeit, um zu unterscheiden, ob walking, running oder cycling
  # walking und running können recht nah bei einander sein. Bei running ist dann aber die 'Heartrate' höher.
  # Ausnahme: walking in den Bergen. Dazu benötige ich die Elevation (die es leider nach dem Update nicht mehr gibt)
  meanSpeed <- mean(rawData$Speed * 3.6, na.rm = TRUE)

  if (meanSpeed > 15) {
    sportart <- "cycling"

    # tmp file löschen und raus
    file.remove(paste0(outputPath, outputFile))
    return(sportart)
  }

  meanHeartrate <- mean(rawData$Heartrate, na.rm = TRUE)
  if (meanSpeed <= 15 && meanHeartrate < 125) {
    sportart <- "walking"

    # tmp file löschen und raus
    file.remove(paste0(outputPath, outputFile))
    return(sportart)
  }

  if (meanSpeed <= 15 && meanSpeed > 5 && meanHeartrate > 125) {
    sportart <- "running"
  } else {
    sportart <- "walking"
  }

  # tmp file löschen und raus
  file.remove(paste0(outputPath, outputFile))
  return(sportart)
}
