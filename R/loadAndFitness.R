#' Berechne den Load
#'
#' Funktion um die Last (Load) zu berechnen
#'
#' @author Ulf Schepsmeier
#'
#' @param df data.frame der Uebersichtstabelle (aggregierte Daten pro Training)
#'
#' @return df
#' @export
#'
#' @note add a column that contains the load of each activity
#' one way to calculate load is to multiply time in hours by avg HR and add 2.5 times avg HR
#' this relates to load by y = ax + b of a = 0.418, b = -150
#'
#' @references \url{https://www.r-bloggers.com/2022/11/form-and-file-estimating-running-form-in-r/}


calculateLoad <- function(df) {
  df$load <- 0.418 * ((df$Trainingszeit / 60 * df$`&empty; bpm`) + (2.5 * df$`&empty; bpm`)) - 150
  return(df)
}


#' Erzeugung eines leeren Fitness-DF
#'
#' Make a data frame that has every day in our time window represented
#'
#' @author Ulf Schepsmeier
#'
#' @param fromStr character; Startdatum in der Form "%Y-%m-%d"
#' @param toStr character; Enddatum in der Form "%Y-%m-%d"
#'
#' @references \url{https://www.r-bloggers.com/2022/11/form-and-file-estimating-running-form-in-r/}
#'
#' @return df
#' @export
#'

makeDateDF <- function(fromStr, toStr) {
  temp <- seq(as.Date(fromStr), as.Date(toStr), by = "days")
  df <- data.frame(Date = temp,
                   ATL = rep(0, length(temp)),
                   CTL = rep(0, length(temp)))

  return(df)
}


#' Aufsummieren der Loads
#'
#' Sum the load for each day
#'
#' @author Ulf Schepsmeier
#'
#' @param df data.frame with the load (calculated with calculateLoad())
#' @param daydf data.frame from makeDateDF()
#'
#' @references \url{https://www.r-bloggers.com/2022/11/form-and-file-estimating-running-form-in-r/}
#'
#' @return newdf merged data.frame
#' @importFrom stats aggregate
#' @export
#'

sumDays <- function(df, daydf) {
  tempdf <- aggregate(load ~ Datum, data = df, sum)
  newdf <- merge(daydf, tempdf, all.x = TRUE, by.x = "Date", by.y = "Datum")
  newdf[is.na(newdf)] = 0

  return(newdf)
}


#' Berechnung der Fitness Scores
#'
#' calculate training loads (Fitness (CTL), Fatigue (ATL) and Form (TSS)) out of the single loads
#'
#' @author Ulf Schepsmeier
#'
#' @param df data.frame with the loads (after sumDays())
#'
#' @return df data.frame with calculated values for Fitness (CTL), Fatigue (ATL) and Form (TSS)
#' @export
#'
#' @note Although the stress score acronyms are copyrighted, what they do is not too mysterious.
#' Fatigue is how tired you are feeling that week and Fitness is how much training you’ve done over six weeks.
#' Put another way, Fatigue is an exponentially weighted average of load over 7 days while Fitness
#' is an exponentially weight average of load over 42 days. Form is the difference between Fatigue and Fitness.
#'
#' @references \url{https://www.r-bloggers.com/2022/11/form-and-file-estimating-running-form-in-r/}
#'

calculateTL <- function(df) {
  for (i in 1:nrow(df)) {
    # add today's load to training load(s)
    df$ATL[i] <- df$ATL[i] + df$load[i]
    df$CTL[i] <- df$CTL[i] + df$load[i]
    for (j in (i + 1) : (i + 42)) {
      if(j > nrow(df)) {
        break
      }
      df$ATL[j] <- df$ATL[i] * exp(-(j-i)/7)
      df$CTL[j] <- df$CTL[i] * exp(-(j-i)/42)
    }
  }
  df <- df[,1:3]
  df[2] <- df[2] / 7
  df[3] <- df[3] / 42
  df$TSS <- df$CTL - df$ATL

  return(df)
}
