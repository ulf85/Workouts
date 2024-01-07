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
#' @details The idea behind training load is to have a metric how a training has an effect to your body.
#' Second, training load is (in theory) independent of the sport type or the intensity.
#' Thereby, we are able to compare a short but intensive training with a long but
#' low intense training.
#'
#' Since I do not have a power meter I have to estimate the load and the training stress score (TSS)
#' based on heart rate. Then it is called hrTSS.
#' see \url{https://help.trainingpeaks.com/hc/en-us/articles/204071944-Training-Stress-Scores-TSS-Explained}
#' hrTSS has some shortcomings \cr
#' Quote from \url{https://de-eu.wahoofitness.com/blog/ate-what-is-tss/} \cr
#' "Heart rate TSS or hrTSS is a less accurate model for determining your TSS of a workout.
#' While hrTSS is accurate when performing steady-state efforts, or endurance rides, it is far less accurate when
#' intervals come into play. \cr
#' hrTSS is based on time spent in each heart rate zone based on your Threshold Heart Rate. \cr
#' When there are large or frequent fluctuations in your heart rate data, such as data post-interval session,
#' it is hard for an accurate TSS to be quantified from this as it takes time for your HR to stabilize pre,
#' during, and post effort."
#'
#' @note add a column that contains the load of each activity
#' one way to calculate load is to multiply time in hours by avg HR and add 2.5 times avg HR
#' this relates to load by y = ax + b of a = 0.418, b = -150 \cr
#' These numbers are taken from \url{https://www.r-bloggers.com/2022/11/form-and-file-estimating-running-form-in-r/}
#' and have to be adjusted for me -> adjusted: a = 0.3398, b = -110
#'
#' @references \url{https://www.r-bloggers.com/2022/11/form-and-file-estimating-running-form-in-r/}


calculateLoad <- function(df) {
  df$load <- 0.3398 * ((df$Trainingszeit / 60 * df$`&empty; bpm`) + (2.5 * df$`&empty; bpm`)) - 110
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
  newdf[is.na(newdf)] <- 0

  return(newdf)
}


#' Berechnung der Fitness Scores
#'
#' calculate training loads (Fitness (CTL), Fatigue (ATL) and Form (TSB)) out of the single loads
#'
#' @author Ulf Schepsmeier
#'
#' @param df data.frame with the loads (after sumDays())
#'
#' @return df data.frame with calculated values for Fitness (CTL), Fatigue (ATL) and Form (TSB)
#' @export
#'
#' @details The basis for the calculation of the metrics is the training load.
#' How to calculate this \code{\link{calculateLoad}}. \cr
#' The training stress score (TSS) will be estimated by the heart rate TSS (hrTSS) since I do not have a power meter.
#' This has some shortcomings, in particular for HIIT. Nevertheless, it gives us a KPI for our training.
#' The acute training load (ACL) and the chronic training load (CTL) give us the basis for this KPI. \cr
#' I found some theoretical background here
#' \url{https://www.trainingpeaks.com/learn/articles/the-science-of-the-performance-manager/} \cr
#' The training score balance (TSB) can be used to have an idea of the "form". \cr
#' All calculations (as I understand the article) are simplifications of Banister’s impulse-response model.
#'
#' @note Although the stress score acronyms are copyrighted, what they do is not too mysterious.
#' Fatigue is how tired you are feeling that week and Fitness is how much training you’ve done over six weeks.
#' Put another way, Fatigue is an exponentially weighted average of load over 7 days while Fitness
#' is an exponentially weight average of load over 42 days. Form (TSB) is the difference between Fatigue and Fitness.
#'
#' @references \url{https://www.r-bloggers.com/2022/11/form-and-file-estimating-running-form-in-r/},
#' \url{https://help.trainingpeaks.com/hc/en-us/articles/204071944-Training-Stress-Scores-TSS-Explained}
#'

calculateTL <- function(df) {
  for (i in seq_len(nrow(df))) {
    # add today's load to training load(s)
    df$ATL[i] <- df$ATL[i] + df$load[i]
    df$CTL[i] <- df$CTL[i] + df$load[i]
    for (j in (i + 1) : (i + 42)) {
      if (j > nrow(df)) {
        break
      }
      df$ATL[j] <- df$ATL[i] * exp(-(j - i) / 7)
      df$CTL[j] <- df$CTL[i] * exp(-(j - i) / 42)
    }
  }
  df <- df[, 1:3]
  df[2] <- df[2] / 7
  df[3] <- df[3] / 42
  df$TSB <- df$CTL - df$ATL

  return(df)
}
