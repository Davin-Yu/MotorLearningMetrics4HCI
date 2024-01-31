#' Normalized jerk error
#'
#' @param t A list of timestamps
#' @param x A list of x-axis positions/rotations during a movement
#' @param y A list of y-axis positions/rotations during a movement
#' @param z A list of z-axis positions/rotations during a movement
#' @param logging_frequency The logging frequency of your data
#' @param included A list of included timestamps, if you want to analyze a subset of the movement
#' @param visualization Whether to enable the built-in visualization
#'
#' @return The calculated NJE
#' @export
#'
#' @examples library(MotorLearningMetrics4HCI)
#' df <- read.csv("exampledata.csv")
#' compute_nje(df$Timestamp, df$X, df$Y, df$Z, 50, visualization=TRUE)
compute_nje <- function(t, x, y, z, logging_frequency, enable_ksmooth = TRUE, included = rep(TRUE, length(t)), visualization = FALSE) {
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

  # smoothing the tracking noise
  selected <- subset(df, included == TRUE)
  n.points = nrow(selected)
  if (enable_ksmooth) {
    kresult <- ksmooth(time(selected$speed),selected$speed,'normal',bandwidth=4,n.points=n.points) #kernel smoother
  } else {
    kresult <- data.frame(x=time(selected$speed), y=selected$speed)
  }

  # normalized jerk
  time_gap <- 1 / logging_frequency
  selected$acceleration <- 0
  selected$jerk <- 0
  for (j in 2:nrow(selected)) {
    selected$acceleration[j] <- (kresult$y[j] - kresult$y[j-1]) / time_gap
    selected$jerk[j] <- (selected$acceleration[j] - selected$acceleration[j-1]) / time_gap
  }
  selected$squareJerk <- selected$jerk * selected$jerk
  squareJerkIntegral <- sum(selected$squareJerk*time_gap)

  # ideal jerk
  f <- function(t) {t* t * (t-nrow(selected)) * (t-nrow(selected))};
  SumDistanceInLength <- sum(kresult$y * 1)
  A <- SumDistanceInLength / integrate(f, lower=0, upper=nrow(selected))$value;
  selected$idealSpeed <- 0
  selected$idealAcceleration <- 0
  selected$idealJerk <- 0

  for (j in 2:nrow(selected)) {
    selected$idealSpeed[j] <-  A * j^2 * (j-nrow(selected))^2
    selected$idealAcceleration[j] <- (selected$idealSpeed[j] - selected$idealSpeed[j-1])/time_gap
    selected$idealJerk[j] <- (selected$idealAcceleration[j] - selected$idealAcceleration[j-1])/time_gap
  }
  selected$idealSquareJerk <- selected$idealJerk * selected$idealJerk
  idealSquareJerkIntegral <- sum(selected$idealSquareJerk*time_gap)

  # distance and time
  sumDistance <- sum(selected$distance)
  trialLength <- selected$t[nrow(selected)] - selected$t[1]

  # NJE calculation
  normalizedJerk <- sqrt(squareJerkIntegral * 0.5 * trialLength^5 / sumDistance^2)
  idealNormalizedJerk <- sqrt(idealSquareJerkIntegral * 0.5 * trialLength^5 / sumDistance^2)

  if (visualization) {
    plot(kresult$x, kresult$y, "l")
    lines(kresult$x, selected$idealSpeed, col="blue", lwd=3)
  }

  return((normalizedJerk - idealNormalizedJerk) / normalizedJerk)
}
