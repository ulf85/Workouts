#' Einlesen der Fitness-Daten aus den CSV Dateien
#'
#' @author Ulf Schepsmeier
#'
#' @param ordner character-String mit dem Ordner-Namen
#' "walking", "indoorCycling", "running" oder "cycling"
#' @param path character-string mit dem Pfad-Namen bis zu dem Ordner
#' @param skip vector mit Indices mit zu überspringenden Dateien in dem Ordner;
#' default: skip = NULL
#'
#' @return Data eingelesene Daten
#' @export
#' @import dplyr
#'
#' @examples
getFitnessData <- function(ordner, path, skip = NULL){
  if(is.null(ordner) || length(ordner) == 0) return(NULL)
  if(length(ordner) > 1) return(NULL)
  if(!(ordner %in% c("walking", "indoorCycling", "running", "cycling"))) return(NULL)

  path <- paste0(path, "/", ordner, "/fixed/")
  files <- dir(path = path)

  if(!is.null(skip) && length(skip) > 0 && is.numeric(skip)) files <- files[-skip]

  Data <- NULL
  if(length(files) > 0){
    trainingNR <- 1
    for(file in files){
      if(ordner %in% c("walking", "running")){
        rawData <- getWalkingData(path, file)
      } else {
        rawData <- getIndoorCyclingData(path, file)
      }

      rawData$trainingNR <- trainingNR
      trainingNR <- trainingNR + 1

      Data <- bind_rows(Data, rawData)
    }
  }
  return(Data)
}
