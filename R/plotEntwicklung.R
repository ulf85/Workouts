#' Plot um die Entwicklung ueber die Zeit darzustellen
#'
#' @author Ulf Schepsmeier
#'
#' @param data data.frame mit Daten der Sportart, die dargestellt werden soll
#'
#' @return ggplot-Objekt
#' @export
#' @import ggplot2
#' @import plotly
#'
#' @examples
plotEntwicklung <- function(data) {
  data <- data[!is.nan(data$Mean) & data$Mean > 1, ]
  data$ymin <- round(pmax(data$Min, data$Mean - 2 * data$Sd), 0)
  data$ymax <- round(pmin(data$Max, data$Mean + 2 * data$Sd), 0)
  data$Mean <- round(data$Mean, 2)

  p <- ggplot(data = data, aes(x = Datum, y = Mean)) +
    geom_line() +
    ylab("") +
    xlab("") +
    geom_ribbon(aes(ymin = ymin, ymax = ymax), fill = "lightblue", alpha = 0.7) +
    facet_wrap(~Messwert, scales = "free_y") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_x_date(date_breaks = "3 month", minor_breaks = "1 month", labels = date_format("%Y-%m"))
  #ggplotly(p)
  p
}
