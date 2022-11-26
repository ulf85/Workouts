#' Fitness and Fatigure plot
#'
#' @author Ulf Schepsmeier
#' @note Date Daten werden von einem Jahr berechnet, aber nucr die letzten 180 Tage dargestellt
#'
#' @param df data.frame mit den Aufbereiteten Daten aus der Funktion calculateTL()
#' @param toStr character; Enddatum in der Form "\%Y-\%m-\%d"
#'
#' @return ggplot-Objekt
#' @export
#' @import ggplot2
#'

FitnessAndFatigurePlot <- function(df, toStr) {
  # data frame for Form zones
  rects <- data.frame(ystart = c(20, 5, -10, -30, -50),
                      yend = c(30, 20, 5, -10, -30),
                      xstart = rep(as.Date(toString) - 180, 5), # nehme von den 360 Tagen nur die letzten 180
                      xend = rep(as.Date(toString), 5),
                      col = factor(c("Transition", "Fresh", "Grey zone", "Optimal", "High risk"),
                                   levels = c("Transition", "Fresh", "Grey zone", "Optimal", "High risk")))

  p1 <- ggplot(df %>% filter(Date >= as.Date(toString) - 180), aes(x = Date)) +
    geom_area(aes(y = CTL), fill = "#58abdf", alpha = 0.2) +
    geom_line(aes(y = CTL), colour = "#58abdf") +
    geom_line(aes(y = ATL), colour = "#5e3cc4") +
    geom_text(aes(x = as.Date(toString), y = 0, vjust = "inward", hjust = "inward", label = "Fitness"),
              color = "#58abdf") +
    geom_text(aes(x = as.Date(toString) - 180, y = max(ATL), vjust = "inward", hjust = "inward", label = "Fatigue"),
              color = "#5e3cc4") +
    labs(x = "", y = "Training load per day") +
    theme_bw() +
    theme(legend.position = "none")

  return(p1)
}
