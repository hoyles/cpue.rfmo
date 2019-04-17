#' Plot the mean fishing location by season.
#'
#' Function to make fishing location plot.
#' @param dat Dataset to cluster.
#'
plot_mean_fishing_location <- function(dat) {
  par(mfrow = c(1,2))
  ax <- tapply(dat$yrqtr,dat$yrqtr,mean); ay = tapply(dat$lat5,dat$yrqtr,mean)
  plot(ax,ay,xlab = "yr",ylab = "Mean latitude",type = "n")
  a <- 4*(.125+dat$yrqtr-floor(dat$yrqtr))
  a <- tapply(a,dat$yrqtr,mean)
  text(ax,ay,a,cex = 0.7)
  ax = tapply(dat$lon5,dat$yrqtr,mean);ay = tapply(dat$yrqtr,dat$yrqtr,mean)
  plot(ax,ay,ylab = "yr",xlab = "Mean longitude",type = "n")
  text(ax,ay,a,cex = 0.7)
}
