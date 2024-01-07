#' gt-Tabelle für Jahresvergleich
#'
#' Im Jahresvergleich möchte ich die Anzahl bzw. die km von mehreren Jahren vergleichen.
#' Es werden Differenzen zum Vorjahr berechnet und negative rot hervorgehoben, sowie Gesamt-Anzahl bzw. Gesamt-km.
#'
#' @author Ulf Schepsmeier
#'
#' @param data data.frame mit aggregierten Daten aus den verschiedenen Jahren, s. Note
#' @param type string; Für welche Variable soll die Auswertung gemacht werden? "Anzahl" oder "km";
#' default: type = "Anzahl"
#' @param short string; Spaltenkürzel in der finalen Tabelle; default: short = "#"
#'
#' @note Der data.frame muss einen bestimmten Aufbau und damit bestimmte Spalten haben:
#' * Jahr <chr> mit Zeilen für die Jahre + "Gesamt" (pro Sportart)
#' * Sportart <chr> bei mir "Cycling" und "Indoor Cycling"
#' * Anzahl <int>
#' * km <dbl>
#' * DiffAnzahl <dbl> Änderung zum Vorjahr; Bei "Gesamt" 0
#' * DiffKm <dbl> Änderung zum Vorjahr; Bei "Gesamt" 0
#' * JahrInt <dbl> Das "Jahr" als numerischer Wert; "Gesamt" = 0
#'
#' @return gt-Objekt
#' @export
#' @import gt
#' @import dplyr
#' @importFrom purrr set_names
#'

gtJahresvergleichFct <- function(data, type = "Anzahl", short = "#") {

  if (type == "Anzahl") {
    jahresvergleichTidy <- data %>%
      select(-JahrInt, -km, -DiffKm) %>%
      pivot_wider(names_from = 1, values_from = c(3, 4), values_fill = 0)
  } else {
    jahresvergleichTidy <- data %>%
      select(-JahrInt, -Anzahl, -DiffAnzahl) %>%
      pivot_wider(names_from = 1, values_from = c(3, 4), values_fill = 0)
  }

  # DiffAnzahl_Gesamt oder DiffKm_Gesamt
  jahresvergleichTidy <- jahresvergleichTidy %>% select(-matches("^(Diff).*(_Gesamt)"))

  minJahr <- min(data$JahrInt[-which(data$JahrInt == 0)])
  indMinJahr <- grep(pattern = as.character(minJahr), x = colnames(jahresvergleichTidy))
  maxJahr <- max(data$JahrInt[-which(data$JahrInt == 0)])
  indMaxJahr <- grep(pattern = as.character(maxJahr), x = colnames(jahresvergleichTidy))
  indGesamt <- grep(pattern = "Gesamt", x = colnames(jahresvergleichTidy))

  cols_list <- as.list(c(short, "+/-", short, "+/-", short)) %>%
    purrr::set_names(c(paste0(ifelse(type == "Anzahl", "Anzahl_", "km_"), minJahr),
                       paste0(ifelse(type == "Anzahl", "DiffAnzahl_", "DiffKm_"), minJahr),
                       paste0(ifelse(type == "Anzahl", "Anzahl_", "km_"), maxJahr),
                       paste0(ifelse(type == "Anzahl", "DiffAnzahl_", "DiffKm_"), maxJahr),
                       paste0(ifelse(type == "Anzahl", "Anzahl_", "km_"), "Gesamt")))

  jahresvergleichgt <- gt(jahresvergleichTidy) %>%
    tab_spanner(label = minJahr, columns = indMinJahr) %>%
    tab_spanner(label = maxJahr, columns = indMaxJahr) %>%
    tab_spanner(label = "Gesamt", columns = indGesamt) %>%
    cols_label(.list = cols_list) %>%
    tab_style(style = cell_text(color = "red"),
              locations = cells_body(
                columns = c(indMaxJahr[2]),
                rows = jahresvergleichTidy[, indMaxJahr[2]] < 0
              ))

  # nur wenn wir mehr als zwei Jahre haben
  if (maxJahr - minJahr > 2) {
    for (i in (maxJahr - 1):(minJahr + 1)) {
      ind <- grep(pattern = as.character(i), x = colnames(jahresvergleichTidy))
      cols_list <- as.list(c(short, "+/-")) %>%
        purrr::set_names(c(paste0(ifelse(type == "Anzahl", "Anzahl_", "km_"), i),
                           paste0(ifelse(type == "Anzahl", "DiffAnzahl_", "DiffKm_"), i)))
      jahresvergleichgt <- jahresvergleichgt %>%
        tab_spanner(label = i, columns = ind) %>%
        cols_label(.list = cols_list) %>%
        tab_style(style = cell_text(color = "red"),
                  locations = cells_body(
                    columns = c(ind[2]),
                    rows = jahresvergleichTidy[, ind[2]] < 0
                  ))
    }
  }

  # Spaltentrenner
  ind <- grep(pattern = ifelse(type == "Anzahl", "^Anzahl_", "^km_"), x = colnames(jahresvergleichTidy))
  jahresvergleichgt <- jahresvergleichgt %>%
    tab_style(
      style = cell_borders(sides = c("left"), weight = px(1)),
      locations = cells_body(
        columns = ind[-1] # beim ersten nicht
      )
    )

  return(jahresvergleichgt)
}
