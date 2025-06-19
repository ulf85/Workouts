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
  body <- jsonlite::toJSON(df, pretty = TRUE, digits = 6)
  body <- paste("{ \"locations\": ", body, "}")
  res <- POST(url, encoding, body = body)

  # check
  if (http_error(res)) {
    status <- http_status(res)
    if (status$reason == "Request Entity Too Large") {
      # split data.frame in two chunks and call this function recursively
      n <- nrow(df)
      df_res <- NULL
      for (i in seq_len(2)) {
        tmp_df_res <- getElevation(df[(round(n / 2) * (i - 1) + 1) : min(n, (round(n / 2) * i)), ])
        df_res <- df_res %>% bind_rows(tmp_df_res)
      }
      return(df_res)
    } else {
      stop("Error in API call")
    }
  }

  # modify response
  jsonRespParsed <- content(res, as = "parsed")
  df_res <- jsonRespParsed$results %>% bind_rows %>% select(latitude, longitude, elevation)
}
