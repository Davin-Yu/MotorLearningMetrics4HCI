#' Z-score Peak Detection Algorithm
#'
#' @param y A list of timeseries data
#' @param lag The lag of the moving window that calculates the mean and standard deviation of historical data. Default = 3.
#' @param threshold The "z-score" at which the algorithm signals. Default = 3.
#' @param influence The influence (between 0 and 1) of new signals on the calculation of the moving mean and moving standard deviation. Default = 0.1.
#'
#' @return A list of values containing the signal results, the average filter, and the std filter. See https://stackoverflow.com/questions/22583391/peak-signal-detection-in-realtime-timeseries-data
#' @export
#'
#' @examples library(MotorLearningMetrics4HCI)
#' df <- read.csv("exampledata.csv")
#' compute_rtp(df$Timestamp, df$X, df$Y, df$Z, peak_detection_algorithm = z_score_peak_detection, visualization = TRUE)
z_score_peak_detection <- function(y,lag=3,threshold=3,influence=0.1) {
  signals <- rep(0,length(y))
  filteredY <- y[0:lag]
  avgFilter <- NULL
  stdFilter <- NULL
  avgFilter[lag] <- mean(y[0:lag], na.rm=TRUE)
  stdFilter[lag] <- sd(y[0:lag], na.rm=TRUE)
  for (i in (lag+1):length(y)){
    if (abs(y[i]-avgFilter[i-1]) > threshold*stdFilter[i-1]) {
      if (y[i] > avgFilter[i-1]) {
        signals[i] <- 1;
      } else {
        signals[i] <- -1;
      }
      filteredY[i] <- influence*y[i]+(1-influence)*filteredY[i-1]
    } else {
      signals[i] <- 0
      filteredY[i] <- y[i]
    }
    avgFilter[i] <- mean(filteredY[(i-lag):i], na.rm=TRUE)
    stdFilter[i] <- sd(filteredY[(i-lag):i], na.rm=TRUE)
  }
  return(list("signals"=signals,"avgFilter"=avgFilter,"stdFilter"=stdFilter))
}
