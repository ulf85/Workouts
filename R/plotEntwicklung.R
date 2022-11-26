#' Plot um die Entwicklung ueber die Zeit darzustellen
#'
#' @author Ulf Schepsmeier
#'
#' @param data data.frame mit Daten der Sportart, die dargestellt werden soll
#' @param date_breaks Einteilung der x-Achse; default: date_breaks = "3 months"
#' @param date_minor_breaks Untereinteilung der x-Achse; default: minor_breaks = "1 month"
#' @param labels Label der x-Achse; default: labels = date_format("\%Y-\%m")
#' @param CI Konfidenzband (Confidence interval) anzeigen? default CI = TRUE
#' @param trend Soll ein Trend ermittelt und angezeigt werden? default: trend = FALSE
#'
#' @return ggplot-Objekt
#' @export
#' @import ggplot2
#'

plotEntwicklung <- function(data, date_breaks = "3 months", date_minor_breaks = "1 month",
                            labels = date_format("%Y-%m"),
                            CI = TRUE, trend = FALSE) {
  data <- data[!is.nan(data$Mean) & data$Mean > 1, ]
  data$ymin <- round(pmax(data$Min, data$Mean - 2 * data$Sd), 0)
  data$ymax <- round(pmin(data$Max, data$Mean + 2 * data$Sd), 0)
  data$Mean <- round(data$Mean, 2)

  p <- ggplot(data = data, aes(x = Datum, y = Mean)) +
    geom_line() +
    ylab("") +
    xlab("") +
    facet_wrap(~Messwert, scales = "free_y") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_x_date(date_breaks = date_breaks, date_minor_breaks = date_minor_breaks, labels = labels)

  if (CI) {
    p <- p +
      geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "lightblue", alpha = 0.7)
  }
  if (trend) {
    p <- p +
      geom_smooth(color = "red", se = FALSE)
  }
  p
}
