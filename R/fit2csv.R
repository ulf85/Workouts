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

  infile_path <- file.path(path, file)
  outfile_path <- file.path(outpath, outfile)
  gpsbabel_path <- "C:/Program Files/GPSBabel/gpsbabel"
  befehl <- paste0('"', gpsbabel_path, '" -t -i garmin_fit,allpoints=1 -f "', 
  infile_path, '" -o unicsv -F "', outfile_path, '"')

  exit_status <- system(befehl, intern = FALSE, ignore.stdout = TRUE, ignore.stderr = TRUE)

  if (exit_status == 0) {
    return(TRUE)
  } else {
    warning(paste("Conversion failed for", infile_path, "to", outfile_path))
    return(FALSE)
  }
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
  fit_path <- file.path(path, ordner, "fit")
  files <- dir(fit_path)
  outpath <- gsub(pattern = "fit", replacement = "fixed", x = fit_path)
  outfiles <- dir(outpath)

  # nur die umwandeln, die noch nicht existieren
  for (file in files) {
    outfile <- gsub(pattern = ".fit", replacement = ".csv", x = file)
    if (!(outfile %in% outfiles)) {
      fit2csv(fit_path, file)
    }
  }
}
