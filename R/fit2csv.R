#' .fit Dateien umwandeln/auslesen
#'
#' @author Ulf Schepsmeier
#'
#' @param path character-String mit dem Pfad zu dem fit-File
#' @param file character-String mit dem Namen der fit-Datei
#' @param outpath character-String mit dem Pfad für den Output; default: outpath = NULL, i.e. the fit-path from 'path'
#' is replaced with 'fixed'
#'
#' @return boolean ob die Umwandlung funktioniert hat oder nicht
#' @export
#' @note Das Programm GPSBabel muss installiert sein
#'

fit2csv <- function(path, file, outpath = NULL) {

  outfile <- gsub(pattern = ".fit", replacement = ".csv", x = file)
  if (is.null(outpath)) {
    outpath <- gsub(pattern = "fit", replacement = "fixed", x = path)
  }

  if (!dir.exists(outpath)) {
    dir.create(outpath)
  }

  befehl <- paste0("C:\\\"Program Files\"\\GPSBabel\\gpsbabel -t -i garmin_fit,allpoints=1 -f ",
                   path, file, " -o unicsv -F ", outpath, outfile)

  out <- system(befehl, intern = TRUE)

  if (length(out) == 0) return(TRUE)
  else return(FALSE)
}


#' Transformiere einen ganzen Ordner vom .fit-Format in CSV
#'
#' @author Ulf Schepsmeier
#'
#' @param ordner character-String der Ordners
#' @param path charater-String mit absolutem Pfad bis zum Ordner
#' (Leider ist der absolute Pfad notwendig für die EXE im Hintergrund)
#'
#' @export
#'

fit2csvOrdner <- function(ordner, path) {
  path <- paste0(path, "/", ordner, "/fit/")
  files <- dir(path)
  outpath <- gsub(pattern = "fit", replacement = "fixed", x = path)
  outfiles <- dir(outpath)

  # nur die umwandeln, die noch nicht existieren
  for (file in files) {
    outfile <- gsub(pattern = ".fit", replacement = ".csv", x = file)
    if (!(outfile %in% outfiles)) {
      fit2csv(path, file)
    }
  }
}
