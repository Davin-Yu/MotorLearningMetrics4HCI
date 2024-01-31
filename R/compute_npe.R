#' Normalized Path Error (NPE)
#'
#' @param t A list of timestamps
#' @param x A list of x-axis positions/rotations during a movement
#' @param y A list of y-axis positions/rotations during a movement
#' @param z A list of z-axis positions/rotations during a movement
#' @param shortest_path_length The pre-defined shortest path for the task condition
#' @param included A list of included timestamps, if you want to analyze a subset of the movement
#'
#' @return The calculated NPE
#' @export
#'
#' @examples library(MotorLearningMetrics4HCI)
#' df <- read.csv("exampledata.csv")
#' compute_npe(df$Timestamp, df$X, df$Y, df$Z, 0.24) #Note that there is no smoothing of positions in this implementation.
compute_npe <- function(t, x, y, z, shortest_path_length, included = rep(TRUE, length(t))) {
  # init
  df <- data.frame(t, x, y, z, included)
  df$distance <- 0

  # distance calculation
  for (j in 2:nrow(df)) {
    conf1 <- c(df[j-1,]$x, df[j-1,]$y, df[j-1,]$z)
    conf2 <- c(df[j,]$x, df[j,]$y, df[j,]$z)
    df[j,]$distance <- dist(rbind(conf1, conf2), method="euclidean")
  }

  # NPE calculation
  selected <- subset(df, included == TRUE)
  sumDistance <- sum(selected$distance)

  return((sumDistance - shortest_path_length) / sumDistance)
}
