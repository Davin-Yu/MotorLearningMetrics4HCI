#' Custom Z-score Peak Detection Algorithm
#'
#' @param lag The lag of the moving window that calculates the mean and standard deviation of historical data. Default = 3.
#' @param threshold The "z-score" at which the algorithm signals. Default = 3.
#' @param influence The influence (between 0 and 1) of new signals on the calculation of the moving mean and moving standard deviation. Default = 0.1.
#'
#' @return A list of values containing the signal results, the average filter, and the std filter. See https://stackoverflow.com/questions/22583391/peak-signal-detection-in-realtime-timeseries-data
#' @export
#'
#' @examples library(MotorLearningMetrics4HCI)
#' df <- read.csv("exampledata.csv")
#' compute_rtp(df$Timestamp, df$X, df$Y, df$Z, peak_detection_algorithm = custom_z_score_peak_detection(5, 3, 0.1), visualization = TRUE)
custom_z_score_peak_detection <- function(lag, threshold, influence) {
  return(function(y){
    return(z_score_peak_detection(y, lag, threshold, influence))
  })
}
