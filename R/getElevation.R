#' getElevation
#'
#' Nach einer Umstellung von Wahoo sind in der .fit-Datei keine Angaben mehr zur Elevation (Höhe) von Koordinaten.
#' Deswegen hole ich mir diese Informationen über eine open-source API.
#'
#' @author Ulf Schepsmeier
#'
#' @note Ich benutze die open-source API von \url{https://open-elevation.com/}
#' Details zu der Benutzung kann der README Seite von
#' \url{https://github.com/Jorl17/open-elevation/blob/master/docs/api.md} entnommen werden.
#'
#' @param df data.frame mit latitude und longitude
#'
#' @return data.frame mit latitude, longitude und elevation
#'
#' @export
#' @import httr
#' @importFrom jsonlite toJSON
#'

getElevation <- function(df) {

  # check if col names are correct
  if (!all(c("latitude", "longitude") %in% colnames(df))) {
    stop("Data frame has not the right format!")
  }

  # POST request
  url <- "https://api.open-elevation.com/api/v1/lookup"
  encoding <- content_type_json()
  body <- jsonlite::toJSON(df, pretty = TRUE)
  body <- paste("{ \"locations\": ", body, "}")
  res <- POST(url, encoding, body = body)

  # check
  if (http_error(res)) {
    stop("Error in API call")
  }

  # modify response
  jsonRespParsed <- content(res, as = "parsed")
  df_res <- jsonRespParsed$results %>% bind_rows %>% select(latitude, longitude, elevation)

  return(df_res)
}
