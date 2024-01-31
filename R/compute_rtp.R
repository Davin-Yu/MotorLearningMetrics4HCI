#' Refinement Time Proportion (RTP)
#'
#' @param t A list of timestamps
#' @param x A list of x-axis positions/rotations during a movement
#' @param y A list of y-axis positions/rotations during a movement
#' @param z A list of z-axis positions/rotations during a movement
#' @param enable_ksmooth Whether to enable the built-in kernel smoother
#' @param peak_detection_algorithm customized peak detection algorithm, if any
#' @param included A list of included timestamps, if you want to analyze a subset of the movement
#' @param visualization Whether to enable the built-in visualization
#'
#' @return The calculated RTP
#' @export
#'
#' @examples library(MotorLearningMetrics4HCI)
#' df <- read.csv("exampledata.csv")
#' compute_rtp(df$Timestamp, df$X, df$Y, df$Z, visualization = TRUE)
compute_rtp <- function(t, x, y, z, enable_ksmooth = TRUE, peak_detection_algorithm = z_score_peak_detection, included = rep(TRUE, length(t)), visualization = FALSE) {
  # init
  df <- data.frame(t, x, y, z)
  df$distance <- 0
  df$speed <- 0

  # speed calculation
  for (j in 2:nrow(df)) {
    conf1 <- c(df[j-1,]$x, df[j-1,]$y, df[j-1,]$z)
    conf2 <- c(df[j,]$x, df[j,]$y, df[j,]$z)
    df[j,]$distance <- dist(rbind(conf1, conf2), method="euclidean")
    df[j,]$speed <- df[j,]$distance / (df[j,]$t - df[j-1,]$t)
  }

  # refinement time proportion calculation
  selected <- subset(df, included == TRUE)
  n.points = nrow(selected)
  if (enable_ksmooth) {
    kresult <- ksmooth(time(selected$speed),selected$speed,'normal',bandwidth=4,n.points=n.points) #kernel smoother
  } else {
    kresult <- data.frame(x=time(selected$speed), y=selected$speed)
  }

  y_reverse <- rev(kresult$y) #the reverse allows us to detect the ending of the first wave accurately (later movement is assumed to be more stable)
  peakresults <- peak_detection_algorithm(y_reverse)
  peakresults$signals <- rev(peakresults$signals)

  if (visualization) {
    plot(time(selected$speed), selected$speed, "l")
    lines(kresult$x, kresult$y, col="pink", lwd=3)
    lines(kresult$x, peakresults$signals * max(kresult$y), col="green", lwd=2)
  }

  tryCatch({
    first1 <- which(sapply(peakresults$signals, function(x) 1 %in% x))[1]
    subsignals <- peakresults$signals[first1:length(peakresults$signals)]
    first0after1 <- which(sapply(subsignals, function(x) 0 %in% x))[1]
    firstboundends <- first1 + first0after1 - 1
  },
  error = function(e){
    firstboundends <<- n.points #global assignment operator
    #message("The algorithm is unable to find the ending of the peak---we set it equal to the trial length")
  }
  )

  return(1 - firstboundends / n.points)
}
