#' Make kmeans deviance plot.
#'
#' Function to make kmeans deviance plot, for estimating the appropriate number of clusters.
#' @param dat Dataset to cluster.
#' @param allsp species code variabl names to include in the kmeans clustering process.
#' @param r Region number to use as a plotting label.
#' @param ti Use in the plot filename.
#' @param regtype Not used.
#'
plot_km_deviance <- function(dat, allsp, r, ti, regtype = "regY") {
    a <- scale(dat[, allsp])
    wss <- (nrow(a) - 1) * sum(apply(a[, allsp], 2, var))
    for (i in 2:15) wss[i] <- sum(kmeans(a[, allsp], centers = i, iter.max = 40)$withinss)
    plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares", main = paste("Region", r))
    savePlot(paste0(ti, "_plot_km_deviance", ".png"), type = "png")
}

#' Make kmeans deviance plot at the trip level.
#'
#' Function to make kmeans deviance plot, for estimating the appropriate number of clusters.
#' @param ddd Dataset to cluster.
#' @param allsp species code variabl names to include in the kmeans clustering process.
#' @param r Region number to use as a plotting label.
#' @param ti Use in the plot filename.
#' @param regtype Not used.
#' @param tripid Defines the variable to use as trip identifier
#'
plot_km_deviance_trip <- function(ddd, allsp, r, ti, regtype = "regY", tripid="tripidmon") {
    ddd$TRIP_NUM <- ddd[,tripid]
    indat <- aggregate_by_trip(ddd, allsp)
    a <- indat[, allsp]
    # a <- scale(indat[, allsp])
    wss <- (nrow(a) - 1) * sum(apply(a[, allsp], 2, var))
    for (i in 2:15) wss[i] <- sum(kmeans(a[, allsp], centers = i, iter.max = 40)$withinss)
    plot(1:15, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares", main = paste("Region", r))
    savePlot(paste0(ti, "_plot_km_deviance_trip", ".png"), type = "png")
}

#' Map of the Indian Ocean.
#'
#' Function to make a map of the Indian Ocean, with regional boundaries.
#' @param plot_title Plot title.
#' @param uselims Latitudes and Longitudes for the edges of the map.
#' @param sp Region type code for the region boundaries.
#' @param newm If TRUE, create a new plot, otherwise add boundaries etc to existing plot.
#' @param lwdm Line width for boundaries.
#' @param axes If TRUE, create x and y axes.
#' @param tcol Text colour.
#' @param mapfill If TRUE, fill the land area of the map.
#' @param bgc Background colour
#'
plot_IO <- function(plot_title = "", uselims = c(20, 130, -50, 25), sp = "YFT", newm = T, lwdm = 3, axes = T, tcol = "red", mapfill = TRUE, bgc = "lightblue") {
  lims <- uselims
  if (newm) {
    plot(1, 1, yaxt = "n", xaxt = "n", type = "n", xlim = c(lims[1], lims[2]), ylim = c(lims[3], lims[4]), ylab = "", xlab = "", bg = bgc)
    polygon(c(lims[1] - 5, lims[2] + 5, lims[2] + 5, lims[1] - 5), c(lims[3] - 5, lims[3] - 5, lims[4] + 5, lims[4] + 5), col = bgc)
  }
  if (sp == "ALB") {
    lines(c(34.5, 44.2), c(-20, -20), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(49, 120), c(-20, -20), lwd = lwdm, col = "slate grey", lty = 1)
    xoffset <- 5
    yoffset <- 2.5
    text(115 + xoffset, -11 - yoffset, "N", col = tcol, cex = 1.5)
    text(115 + xoffset, -40 - yoffset, "S", col = tcol, cex = 1.5)
  }
  if (sp %in% c("YFT")) {
    lines(c(20, 120), c(-40, -40), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(20, 20), c(-40, -35), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(40, 40), c(-40, -30), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(40, 40), c(-10, 10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(60, 60), c(-30, -10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(75, 75), c(-15, 10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(100, 100), c(-5, 10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(110, 110), c(-10, -5), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(120, 120), c(-40, -30), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(125, 125), c(-20, -15), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(130, 130), c(-15, -10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(40, 60), c(-30, -30), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(60, 130), c(-15, -15), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(40, 60), c(-10, -10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(110, 130), c(-10, -10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(100, 110), c(-5, -5), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(40, 100), c(10, 10), lwd = lwdm, col = "slate grey", lty = 1)
    text(67.5, 15, "R1", col = tcol, cex = 1.5)
    text(57.5, -2.5, "R2", col = tcol, cex = 1.5)
    text(52.5, -27.5, "R3", col = tcol, cex = 1.5)
    text(82.5, -27.5, "R4", col = tcol, cex = 1.5)
    text(85, -2.5, "R5", col = tcol, cex = 1.5)
    text(90, 15, "R6", col = tcol, cex = 1.5)
  }
  if (sp %in% c("YFT2")) {
    lines(c(20, 120), c(-40, -40), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(20, 20), c(-40, -35), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(40, 40), c(-40, -30), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(40, 40), c(-10, 10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(60, 60), c(-30, -10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(75, 75), c(-15, 10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(100, 100), c(-5, 10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(110, 110), c(-10, -5), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(120, 120), c(-40, -30), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(125, 125), c(-20, -15), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(130, 130), c(-15, -10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(40, 60), c(-30, -30), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(60, 130), c(-15, -15), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(40, 60), c(-10, -10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(110, 130), c(-10, -10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(100, 110), c(-5, -5), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(40, 75), c(0, 0), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(40, 100), c(10, 10), lwd = lwdm, col = "slate grey", lty = 1)
    text(62.5, 17.5, "R1", col = tcol, cex = 1.5)
    text(62.5, 6, "R2N", col = tcol, cex = 1.5)
    text(62.5, -5, "R2S", col = tcol, cex = 1.5)
    text(52.5, -25, "R3", col = tcol, cex = 1.5)
    text(82.5, -27.5, "R4", col = tcol, cex = 1.5)
    text(85, -2.5, "R5", col = tcol, cex = 1.5)
    text(90, 17.5, "R6", col = tcol, cex = 1.5)
  }
  if (sp %in% c("BET")) {
    lines(c(20, 120), c(-35, -35), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(80, 80), c(-15, 10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(100, 100), c(-5, 10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(110, 110), c(-10, -5), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(120, 120), c(-35, -30), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(125, 125), c(-20, -15), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(130, 130), c(-15, -10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(47, 130), c(-15, -15), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(35, 45), c(-20, -20), lwd = lwdm, col = "slate grey", lty = 1)
    # lines(c(45, 45), c( -20, -15), lwd = lwdm, col = 'slate grey', lty = 1)
    lines(c(110, 130), c(-10, -10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(100, 110), c(-5, -5), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(40, 100), c(10, 10), lwd = lwdm, col = "slate grey", lty = 1)
    text(65, 0, "R1", col = tcol, cex = 1.5)
    text(90, 0, "R2", col = tcol, cex = 1.5)
    text(70, -30, "R3", col = tcol, cex = 1.5)
  }
  if (sp %in% c("BET3")) {
    lines(c(20, 120), c(-35, -35), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(80, 80), c(-15, 10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(100, 100), c(-5, 10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(110, 110), c(-10, -5), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(120, 120), c(-35, -30), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(125, 125), c(-20, -15), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(130, 130), c(-15, -10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(47, 130), c(-15, -15), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(35, 45), c(-20, -20), lwd = lwdm, col = "slate grey", lty = 1)
    # lines(c(45, 45), c( -20, -15), lwd = lwdm, col = 'slate grey', lty = 1)
    lines(c(110, 130), c(-10, -10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(100, 110), c(-5, -5), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(40, 80), c(0, 0), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(40, 100), c(10, 10), lwd = lwdm, col = "slate grey", lty = 1)
    text(62.5, 6, "R1N", col = tcol, cex = 1.5)
    text(62.5, -5, "R1S", col = tcol, cex = 1.5)
    text(87.5, -2.5, "R2", col = tcol, cex = 1.5)
    text(72.5, -27.5, "R3", col = tcol, cex = 1.5)
  }
  if (sp %in% c("BETcore", "YFTcore")) {
    lines(c(20, 140), c(-35, -35), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(20, 125.5), c(-15, -15), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(20, 100), c(10, 10), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(80, 80), c(10, -15), lwd = lwdm, col = "slate grey", lty = 1)
    xoffset <- 5
    yoffset <- 2.5
    text(115 + xoffset, -11 - yoffset, "N", col = tcol, cex = 1.5)
    text(115 + xoffset, -40 - yoffset, "S", col = tcol, cex = 1.5)
  }
  maps::map("world", yaxt = "n", xaxt = "n", add = T, resolution = 1, interior = F, fill = mapfill)
  if (axes) {
    box(lwd = 3)
    axis(1, at = seq(lims[1], lims[2], by = 10), labels = F)
    axis(2, at = seq(lims[3], lims[4], by = 5), labels = F)
    latseq <- seq(lims[3] + 10, lims[4] - 10, by = 10)
    latseq2 <- as.character(latseq)
    lonseq <- seq(lims[1] + 20, lims[2] - 10, by = 20)
    lonseq2 <- as.character(lonseq)
    latseq2[latseq < 0] <- paste(abs(latseq[latseq < 0]), "S", sep = "")
    latseq2[latseq > 0] <- paste(latseq[latseq > 0], "N", sep = "")
    lonseq2[lonseq < 180] <- paste(lonseq2[lonseq < 180], "E", sep = "")
    lonseq2[lonseq > 180] <- paste(360 - lonseq[lonseq > 180], "W", sep = "")
    axis(2, at = latseq, labels = latseq2, cex.axis = 0.75)
    axis(1, at = lonseq, labels = lonseq2, cex.axis = 0.75)
  }
  mtext(side = 3, line = 0.5, plot_title, font = 2, cex = 1.1)
}

#' Map of the Atlantic Ocean.
#'
#' Function to make a map of the Atlantic Ocean, with regional boundaries.
#' @param plot_title Plot title.
#' @param uselims Latitudes and Longitudes for the edges of the map.
#' @param sp Region type code for the region boundaries.
#' @param newm If TRUE, create a new plot, otherwise add boundaries etc to existing plot.
#' @param lwdm Line width for boundaries.
#' @param axes If TRUE, create x and y axes.
#' @param tcol Text colour.
#'
plot_AO <- function(plot_title = "", uselims = c(-100, 30, -50, 60), sp = "BET", newm = T, lwdm = 3, axes = T, tcol = "red") {
  lims <- uselims
  if (newm) {
    plot(1, 1, yaxt = "n", xaxt = "n", type = "n", xlim = c(lims[1], lims[2]), ylim = c(lims[3], lims[4]), ylab = "", xlab = "", bg = "lightblue")
    polygon(c(lims[1] - 5, lims[2] + 5, lims[2] + 5, lims[1] - 5), c(lims[3] - 5, lims[3] - 5, lims[4] + 5, lims[4] + 5), col = "lightblue")
  }
  if (sp %in% c("BET")) {
    lines(c(-80, -10), c(45, 45), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(-100, -10), c(25, 25), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(-50, 20), c(-15, -15), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(-60, 20), c(-35, -35), lwd = lwdm, col = "slate grey", lty = 1)
    text(-30, 35, "R1", col = tcol, cex = 1.5)
    text(-20, 0, "R2", col = tcol, cex = 1.5)
    text(-10, -25, "R3", col = tcol, cex = 1.5)
  }
  maps::map("world", yaxt = "n", xaxt = "n", add = T, resolution = 1, interior = F, fill = T)
  if (axes) {
    box(lwd = 3)
    axis(1, at = seq(lims[1], lims[2], by = 10), labels = F)
    axis(2, at = seq(lims[3], lims[4], by = 5), labels = F)
    latseq <- seq(lims[3] + 10, lims[4] - 10, by = 10)
    latseq2 <- as.character(latseq)
    lonseq <- seq(lims[1] + 20, lims[2] - 10, by = 20)
    lonseq2 <- as.character(lonseq)
    latseq2[latseq < 0] <- paste(abs(latseq[latseq < 0]), "S", sep = "")
    latseq2[latseq > 0] <- paste(latseq[latseq > 0], "N", sep = "")
    lonseq2[lonseq < 180] <- paste(lonseq2[lonseq < 180], "E", sep = "")
    lonseq2[lonseq > 180] <- paste(360 - lonseq[lonseq > 180], "W", sep = "")
    axis(2, at = latseq, labels = latseq2, cex.axis = 0.75)
    axis(1, at = lonseq, labels = lonseq2, cex.axis = 0.75)
  }
  mtext(side = 3, line = 0.5, plot_title, font = 2, cex = 1.1)
}

#' Map of the Pacific Ocean.
#'
#' Function to make a map of the Atlantic Ocean, with regional boundaries.
#' @param plot_title Plot title.
#' @param uselims Latitudes and Longitudes for the edges of the map.
#' @param sp Region type code for the region boundaries.
#' @param newm If TRUE, create a new plot, otherwise add boundaries etc to existing plot.
#' @param lwdm Line width for boundaries.
#' @param axes If TRUE, create x and y axes.
#' @param tcol Text colour.
#'
plot_pacific <- function(plot_title = "", uselims = c(-100, 30, -50, 60), sp = "BET", newm = T, lwdm = 3, axes = T, tcol = "red") {
  lims <- uselims
  if (newm) {
    plot(1, 1, yaxt = "n", xaxt = "n", type = "n", xlim = c(lims[1], lims[2]), ylim = c(lims[3], lims[4]), ylab = "", xlab = "", bg = "lightblue")
    polygon(c(lims[1] - 5, lims[2] + 5, lims[2] + 5, lims[1] - 5), c(lims[3] - 5, lims[3] - 5, lims[4] + 5, lims[4] + 5), col = "lightblue")
  }
  if (sp %in% c("BET")) {
    lines(c(-50, 50), c(210, 210), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(-100, -10), c(25, 25), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(-50, 20), c(-15, -15), lwd = lwdm, col = "slate grey", lty = 1)
    lines(c(-60, 20), c(-35, -35), lwd = lwdm, col = "slate grey", lty = 1)
    text(-30, 35, "R1", col = tcol, cex = 1.5)
    text(-20, 0, "R2", col = tcol, cex = 1.5)
    text(-10, -25, "R3", col = tcol, cex = 1.5)
  }
  maps::map("world", yaxt = "n", xaxt = "n", add = T, resolution = 1, interior = F, fill = T)
  if (axes) {
    box(lwd = 3)
    axis(1, at = seq(lims[1], lims[2], by = 10), labels = F)
    axis(2, at = seq(lims[3], lims[4], by = 5), labels = F)
    latseq <- seq(lims[3] + 10, lims[4] - 10, by = 10)
    latseq2 <- as.character(latseq)
    lonseq <- seq(lims[1] + 20, lims[2] - 10, by = 20)
    lonseq2 <- as.character(lonseq)
    latseq2[latseq < 0] <- paste(abs(latseq[latseq < 0]), "S", sep = "")
    latseq2[latseq > 0] <- paste(latseq[latseq > 0], "N", sep = "")
    lonseq2[lonseq < 180] <- paste(lonseq2[lonseq < 180], "E", sep = "")
    lonseq2[lonseq > 180] <- paste(360 - lonseq[lonseq > 180], "W", sep = "")
    axis(2, at = latseq, labels = latseq2, cex.axis = 0.75)
    axis(1, at = lonseq, labels = lonseq2, cex.axis = 0.75)
  }
  mtext(side = 3, line = 0.5, plot_title, font = 2, cex = 1.1)
}

#' Map of the Pacific Ocean with EPO boundaries.
#'
#' Function to make a map of the Pacific Ocean, with regional boundaries.
#' @param new Make a new plot
#' @param latlim Latitudes for the edges of the map.
#' @param lonlim Longitudes for the edges of the map.
#'
map_EPO <- function(new=FALSE, latlim = c(-40, 40), lonlim = c(140, 290)) {
  if(new) {
    doxax <- "n"
    plot(1:5, 1:5, ylim = latlim, xlim = lonlim, type = "n", xlab = "Longitude", ylab = "Latitude",xaxt=doxax)
  }
  axis(1, at = c(160, 210, 260), labels = c(-200, -150, -100))
  maps::map("world2", add = T, interior=F, fill = TRUE)
  abline(v=210, col = "slate grey", lwd = 2, lty=1)
  lines(c(250, 250), c(-70, 25), lwd = 2, col = "slate grey", lty = 1)
  lines(c(110, 280), c(-10, -10), lwd = 2, col = "slate grey", lty = 1)
  lines(c(210, 250), c(10, 10), lwd = 2, col = "slate grey", lty = 1)
  lines(c(140, 210), c(-40, -40), lwd = 2, col = "slate grey", lty = 1)
  lines(c(140, 150), c(-20, -20), lwd = 2, col = "slate grey", lty = 1)
  lines(c(140, 150), c(-15, -15), lwd = 2, col = "slate grey", lty = 1)
  lines(c(155, 160), c( -5,  -5), lwd = 2, col = "slate grey", lty = 1)
  lines(c(140, 155), c(  0,   0), lwd = 2, col = "slate grey", lty = 1)
  lines(c(140, 210), c( 10,  10), lwd = 2, col = "slate grey", lty = 1)
  lines(c(110, 140), c( 20,  20), lwd = 2, col = "slate grey", lty = 1)
  lines(c(140, 210), c( 50,  50), lwd = 2, col = "slate grey", lty = 1)
  lines(c(110, 110), c(-10,  20), lwd = 2, col = "slate grey", lty = 1)
  lines(c(120, 120), c( 20,  26), lwd = 2, col = "slate grey", lty = 1)
  lines(c(140, 140), c(-40,  20), lwd = 2, col = "slate grey", lty = 1)
  lines(c(150, 150), c(-20, -15), lwd = 2, col = "slate grey", lty = 1)
  lines(c(155, 155), c(-5,    0), lwd = 2, col = "slate grey", lty = 1)
  lines(c(160, 160), c(-10,  -5), lwd = 2, col = "slate grey", lty = 1)
  lines(c(170, 170), c(-40,  50), lwd = 2, col = "slate grey", lty = 1)
  lines(c(210, 210), c(-40,  50), lwd = 2, col = "slate grey", lty = 1)
  text(c(140,190,150,185,155,190,130,143,143), c(25,25,5,0,-30,-30,10,-4.5,-15.6), labels = 1:9, cex=1.6, font = 2, col = 4)
  text(c(230,260,230,260,230), c(0,0,-30,-30,25), labels = c(1:4,0), cex=1.6, font = 2, col = 1)
}

#' Make effects plot for a glm object.
#'
#' Function to make effects plot for a glm object. Originally developed for Japanese data in the WCPO.
#' @param model glm object to plot.
#' @param indat Input dataset used to create the glm.
#' @param addmain If TRUE, plot effects for mainline.
#' @param addbranch If TRUE, plot effects for branchline.
#' @param addalb If TRUE, plot effects for albacore.
#' @param addother If TRUE, plot effects for other.
#' @param ti Not used.
#'
plot_effects <- function(model, indat, addmain = F, addbranch = F, addalb = F, addother = F, ti = "") {
    cf <- model$coefficients
    pred <- predict(model, data = indat, type = "terms", se.fit = T)
    # fishlab <- switch(runsp, yft = "Yellowfin", bet = "Bigeye")
    # methlab <- switch(mt, deltabin = "Delta - binomial", deltapos = "Delta - positive", logl = "Lognormal(+0.01)", propn = "Proportion Bigeye")
    nfigs <- 6 + addmain + addbranch + addalb + addother
    mf <- switch(nfigs - 5, c(2, 3), c(3, 3), c(3, 3), c(3, 3), c(3, 4))
    hw <- c(14, 19)
    dev.new(height = hw[1], width = hw[2], noRStudioGD = TRUE)
    par(mfrow = mf, mar = c(5, 4, 2, 1), oma = c(0, 0, 3, 0))
    pr <- pred$fit
    prse <- pred$se.fit
    termlist <- dimnames(pred$fit)[[2]]
    llpos <- grep("latlong", termlist)
    hbfpos <- grep("hbf", termlist)
    mainpos <- grep("mainline", termlist)
    vesspos <- grep("vessid", termlist)
    branchpos <- grep("branchline", termlist)
    if (length(grep("hooks", termlist)) > 0)
        db <- T else db <- F

    index <- sort(unique(indat$yrqtr))
    b <- match(index, indat$yrqtr)
    out <- exp(pr[b, 1])
    se1 <- exp(pr[b, 1] - 1.96 * prse[b, 1])
    se2 <- exp(pr[b, 1] + 1.96 * prse[b, 1])
    plot(as.numeric(as.character(index)), out, ylim = c(0, 3), xlab = "Year", ylab = "Effect size", main = "Year effects")
    segments(as.numeric(as.character(index)), se1, as.numeric(as.character(index)), se2, lty = 1, col = "slate grey")
    points(as.numeric(as.character(index)), out, pch = 16)

    index <- sort(unique(indat$latlong))
    b <- match(index, indat$latlong)
    out <- exp(pr[b, llpos])
    se1 <- exp(pr[b, llpos] - 1.96 * prse[b, llpos])
    se2 <- exp(pr[b, llpos] + 1.96 * prse[b, llpos])
    plot(as.numeric(as.character(index)), out, ylim = c(0, 3), xlab = "Latlong", ylab = "Effect size", main = "Spatial effects")
    segments(as.numeric(as.character(index)), se1, as.numeric(as.character(index)), se2, lty = 1, col = "slate grey")
    points(as.numeric(as.character(index)), out, pch = 16)
    # plot(indat$latlong, pr[, 2], ylim = c( - .75, .75), xlab = 'Latitude', ylab = 'Effect size')

    ## plot coefficients
    requireNamespace("maps")
    ll <- as.numeric(indat$latlong)
    index <- sort(unique(ll))
    lats <- trunc(ll)
    lons <- 1000 * abs((ll - trunc(ll)))
    alat <- round(lats[match(index, ll)] * 2)/2 + 2.5
    alon <- round(lons[match(index, ll)] * 2)/2 + 2.5
    # lat <- trunc((dat$newlat[match(index, dat$latlong)] + 50)/5) * 5 + 2.5 -50 long <- trunc((dat$newlong[match(index, dat$latlong)])/5) * 5 + 2.5
    coefs <- exp(pr[match(index, ll), llpos])
    coefs2 <- tapply(coefs, list(alon, alat), mean)
    image(sort(as.numeric(unique(alon))), sort(unique(alat)), coefs2, zlim = c(0.5, 2.5), ylab = "Lat", xlab = "Long")
    contour(sort(unique(alon)), sort(unique(alat)), coefs2, levels = c(0, 1, 2, 2.5), add = TRUE, col = 4)
    if(max(alon, na.rm = TRUE) > 180) odb <- "world2" else odb <- "world"
    maps::map(database = odb, add = TRUE, fill = TRUE)

    index <- sort(unique(indat$vessid))
    b <- match(index, indat$vessid)
    out <- exp(pr[b, vesspos])
    se1 <- exp(pr[b, vesspos] - 1.96 * prse[b, vesspos])
    se2 <- exp(pr[b, vesspos] + 1.96 * prse[b, vesspos])
    plot(as.factor(index), out, type = "n", ylim = c(0, 3), xlab = "Vessel ID", ylab = "Effect size", main = "")
    segments(match(index, index), se1, match(index, index), se2, lty = 1, col = "slate grey")
    points(as.factor(index), out, pch = 16)

    vts <- tapply(pr[, vesspos], list(indat$yrqtr, indat$vessid), mean)
    y <- exp(as.vector(vts))
    x <- rep(as.numeric(dimnames(vts)[[1]]), dim(vts)[[2]])
    plot(x, y, type = "p", ylim = c(0, 3), xlab = "Vessel ID", ylab = "Effect size", main = "Vessel effects", pch = 16, cex = 0.9)
    mn <- tapply(exp(pr[, vesspos]), indat$yrqtr, mean)
    lines(as.numeric(dimnames(vts)[[1]]), mn, col = 2, lwd = 2)
    # plot(as.factor(indat$vessid), pr[, vesspos], ylim = c( - .75, .75), xlab = 'Vessel', ylab = 'Effect size')

    rsp <- pr[, hbfpos[1]]
    rsp.se <- prse[, hbfpos[1]]
    if (addmain) {
        a <- indat$mainline == 1
        rsp2 <- pr[, hbfpos[1]] + pr[, hbfpos[2]] + pr[, mainpos[1]]
        rsp2.se <- exp(prse[, hbfpos[1]] + prse[, hbfpos[2]] + prse[, mainpos[1]])
        b <- match(sort(unique(indat[a, ]$hbf)), indat[a, ]$hbf)
        plot(indat[a, ]$hbf[b], exp(rsp[a][b]), ylim = c(0, 10), xlab = "Nylon mainline HBF", ylab = "Effect size", main = paste("Nylon HBF"))
        lines(indat[a, ]$hbf[b], exp(rsp[a][b] + 1.96 * rsp.se[a][b]), lty = 2)
        lines(indat[a, ]$hbf[b], exp(rsp[a][b] - 1.96 * rsp.se[a][b]), lty = 2)
        b <- match(sort(unique(indat[a == F, ]$hbf)), indat[a == F, ]$hbf)
        plot(indat[a == F, ]$hbf[b], exp(rsp2[a == F][b]), ylim = c(0, 3), xlab = "Other mainline HBF", ylab = "Effect size", main = "Other HBF")
        lines(indat[a == F, ]$hbf[b], exp(rsp2[a == F][b] + 1.96 * rsp2.se[a == F][b]), lty = 2)
        lines(indat[a == F, ]$hbf[b], exp(rsp2[a == F][b] - 1.96 * rsp2.se[a == F][b]), lty = 2)
    } else {
        b <- match(sort(unique(indat$hbf)), indat$hbf)
        plot(indat$hbf[b], exp(pr[, hbfpos][b]), ylim = c(0, 3), xlab = "HBF", ylab = "Effect size", main = "HBF effects")
        lines(indat$hbf[b], exp(rsp[b] + 2 * rsp.se[b]), lty = 2)
        lines(indat$hbf[b], exp(rsp[b] - 2 * rsp.se[b]), lty = 2)
    }
}

#' Make slope ratio plot for two sets of coefficients.
#'
#' Function to make slope ratio plot two sets of coefficients.
#' @param coefs1 Coefficients for model 1.
#' @param coefs2 Coefficients for model 2.
#' @param aggyr Year sequence in the first glm.
#' @param opyr Year sequence in the 2nd glm.
#' @param titl Title of plot.
#' @param lab1 Label for the first glm.
#' @param lab2 Label for the second glm.
#' @param fname Filename for saving the plot.
#' @param addrate If TRUE, write the rate of change on the plot.
#'
plot_agg_slope_ratio <- function(coefs1, coefs2, aggyr, opyr, titl, lab1 = "coefs1", lab2 = "coefs2", fname = NULL, addrate = T) {
    # agg goes first, op goes second
    dev.new(height = 16, width = 14, noRStudioGD = TRUE)
    par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
    myr <- aggyr[match(opyr, aggyr)]
    coefs1 <- coefs1[match(opyr, aggyr)]
    plot(myr, coefs1/coefs2, ylim = c(0, 2), xlab = "Year", ylab = "Ratio of coefficients", cex = 0.8)
    title(main = titl, cex.main = 1.2)
    legend("bottomright", legend = paste(lab1, "/", lab2), col = 1, lty = 3)
    lin <- lm(coefs1/coefs2 ~ myr)
    logl <- lm(log(coefs1/coefs2) ~ myr)
    lines(myr, exp(logl$coefficients[1] + logl$coefficients[2] * myr), lty = 3)
    if (addrate) {
        tt <- paste(prettyNum(100 * logl$coefficients[2], digits = 2, format = "f"), "% ? ", prettyNum(100 * summary(logl)$coefficients[2, 2], digits = 2, format = "f"), ", p = ", prettyNum(summary(logl)$coefficients[2, 4], digits = 2, format = "f"), sep = "")
        text(min(as.numeric(as.character(myr))) + 15, 1.9, tt, font = 2, col = "red", cex = 1.1)
    }
    par(mar = c(5, 4, 1, 1))
    plot(myr, coefs1, type = "p", ylab = "Relative abundance estimate", xlab = "Year", ylim = c(0, 4), cex = 0.7)
    lines(myr, coefs2, col = "red")
    legend("topright", legend = c(lab1, lab2), col = 1:2, lty = c(0, 1), pch = c(1, -1))
    if (is.null(fname) == F)
        savePlot(paste(fname, lab1, lab2), type = "png")
}

#' Make slope ratio plot for two sets of coefficients.
#'
#' Function to make slope ratio plot for two sets of coefficients, with break in the year 2000.
#' @param coefs1 Coefficients for model 1.
#' @param coefs2 Coefficients for model 2.
#' @param aggyr Year sequence in the first glm.
#' @param opyr Year sequence in the 2nd glm.
#' @param titl Title of plot.
#' @param lab1 Label for the first glm.
#' @param lab2 Label for the second glm.
#'
plot_agg_slope_ratio_2000 <- function(coefs1, coefs2, aggyr, opyr, titl, lab1 = "coefs1", lab2 = "coefs2") {
    # agg goes first, op goes second
    dev.new(height = 14, width = 12, noRStudioGD = TRUE)
    par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
    myr <- aggyr[match(opyr, aggyr)]
    coefs1 <- coefs1[match(opyr, aggyr)]
    plot(myr, coefs1/coefs2, xlim = c(1976, 2010), ylim = c(0, 2), xlab = "Year", ylab = "Ratio of coefficients")
    title(main = titl, cex.main = 1.2)
    legend("bottomright", legend = paste(lab1, "/", lab2), col = 1, lty = 3)

    lin <- lm(coefs1/coefs2 ~ myr)
    logl <- lm(log(coefs1/coefs2) ~ myr)
    logl2000 <- lm(log(coefs1[myr <= 2000]/coefs2[myr <= 2000]) ~ myr[myr <= 2000])
    lines(myr[myr <= 2000], exp(logl2000$coefficients[1] + logl2000$coefficients[2] * myr[myr <= 2000]), lty = 3)
    tt <- paste(prettyNum(100 * logl2000$coefficients[2], digits = 2, format = "f"), "% ? ", prettyNum(100 * summary(logl2000)$coefficients[2, 2],
        digits = 2, format = "f"), ", p = ", prettyNum(summary(logl2000)$coefficients[2, 4], digits = 2, format = "f"), sep = "")
    text(min(myr) + 5, 1.9, tt, font = 2, col = "red", cex = 1.1)
    par(mar = c(5, 4, 1, 1))
    plot(myr, coefs1, type = "l", ylab = "Relative abundance estimate", xlab = "Year", ylim = c(0, 2.5))
    lines(myr, coefs2, col = "red")
    legend("topleft", legend = c(lab1, lab2), col = 1:2, lty = c(1, 1))
}

#' Make slope ratio plot for two glm summaries.
#'
#' Function to make slope ratio plot for 2 glm summaries, with break in the year 2000.
#' @param summ1 Summary object for model 1.
#' @param summ2 Summary object for model 2.
#' @param name1 Label for the first glm.
#' @param name2 Label for the second glm.
#'
plot_res <- function(summ1, summ2, name1, name2) {
    nyrs <- length(grep("yrqtr", rownames(summ1$coefficients))) + 1
    coefs1 <- get.summ.coefs(summ1, nyrs)
    coefs2 <- get.summ.coefs(summ2, nyrs)
    yrs <- rownames(summ1$coefficients)[grep("yrqtr", rownames(summ1$coefficients))]
    yrs <- c(1980.25, as.numeric(substring(yrs, 17)))
    dev.new(height = 13, width = 11, noRStudioGD = TRUE)
    par(mfrow = c(2, 1), mar = c(5, 4, 1, 2))
    plot_slope_ratio(coefs1, coefs2, yrs, "")
    plot(yrs, coefs1, type = "l", ylab = "Relative abundance estimate", xlab = "Year", ylim = c(0, 2.5))
    lines(yrs, coefs2, col = "red")
    legend("topleft", legend = c(name1, name2), lty = c(1, 1), col = c("black", "red"))
}

#' Make slope ratio plot for two sets of coefficients.
#'
#' Function to make slope ratio plot for two sets of coefficients. Assumes there are indices for all years. Probably not used any more.
#' @param coefs1 Coefficients for model 1.
#' @param coefs2 Coefficients for model 2.
#' @param yr Year sequence.
#' @param titl Title of plot.
#'
plot_slope_ratio <- function(coefs1, coefs2, yr, titl) {
    # base goes first, boat goes second
    dev.new(height = 14, width = 12, noRStudioGD = TRUE)
    par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
    plot(yr, coefs1/coefs2, xlim = c(1976, 2010), ylim = c(0, 2), xlab = "Year", ylab = "Ratio of coefficients")
    title(main = titl, cex.main = 1.2)
    lin <- lm(coefs1/coefs2 ~ yr)
    logl <- lm(log(coefs1/coefs2) ~ yr)
    lines(yr, exp(logl$coefficients[1] + logl$coefficients[2] * yr), lty = 3)
    tt <- paste(prettyNum(100 * logl$coefficients[2], digits = 2, format = "f"), "% ? ", prettyNum(100 * summary(logl)$coefficients[2, 2], digits = 2,
        format = "f"), ", p = ", prettyNum(summary(logl)$coefficients[2, 4], digits = 2, format = "f"), sep = "")
    text(min(yr) + 5, 1.9, tt, font = 2, col = "red", cex = 1.1)
    par(mar = c(5, 4, 1, 1))
    plot(yr, coefs1, type = "l", ylab = "Relative abundance estimate", xlab = "Year", ylim = c(0, 2.5))
    lines(yr, coefs2, col = "red")
}

#' Make slope ratio plot for two sets of coefficients.
#'
#' Function to make slope ratio plot for two sets of coefficients. Assumes there are indices for all years. Probably not used any more.
#' @param coefs1 Coefficients for model 1.
#' @param coefs2 Coefficients for model 2.
#' @param yr1 Year-qtr sequence in the 1st glm.
#' @param yr2 Year-qtr sequence in the 2nd glm.
#' @param titl Title of plot.
#'
plot_slope_ratio2 <- function(coefs1, coefs2, yr1, yr2, titl) {
    # base goes first, boat goes second
    dev.new(height = 14, width = 12, noRStudioGD = TRUE)
    par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
    yy <- c(yr1, yr2)
    yr <- seq(min(yy), max(yy), 0.25)
    coefs1 <- coefs1[match(yr, yr1)]
    coefs2 <- coefs2[match(yr, yr2)]
    plot(yr, coefs1/coefs2, ylim = c(0, 2), xlab = "Year", ylab = "Ratio of coefficients")
    title(main = titl, cex.main = 1.2)
    lin <- lm(coefs1/coefs2 ~ yr)
    logl <- lm(log(coefs1/coefs2) ~ yr)
    lines(yr, exp(logl$coefficients[1] + logl$coefficients[2] * yr), lty = 3)
    tt <- paste(prettyNum(100 * logl$coefficients[2], digits = 2, format = "f"), '% ? ', prettyNum(100 * summary(logl)$coefficients[2, 2], digits = 2,
        format = "f"), ", p = ", prettyNum(summary(logl)$coefficients[2, 4], digits = 2, format = "f"), sep = "")
    text(min(yr) + 10, 1.9, tt, font = 2, col = "red", cex = 1.1)
    par(mar = c(5, 4, 1, 1))
    plot(yr, coefs1, type = "l", ylab = "Relative abundance estimate", xlab = "Year", ylim = c(0, 3.5))
    lines(yr, coefs2, col = "red")
}

#' Plot map of catches or other spatial variable.
#'
#' Function to plot map of catches or other spatial variable.
#' @param indat Input dataset to plot.
#' @param vbl The variable to plot spatially.
#' @param dcd Time identifier for selecting data.
#' @param latlim Latitude limits for plot.
#' @param lonlim Longitude limits for plot.
#' @param brk Transition levels for the image colours.
#' @param brk2 Transition levels for the contour lines.
#' @param ti Title of the plot.
#' @param bgc Background color of the plot, defaults to grey.
#' @param maptype Set to world, world2, or EPO.
#'
plot_catchmap <- function(indat, vbl, dcd, latlim = c(-40, 20), lonlim = c(20, 130), brk = seq(0, 1, 0.05), brk2 = seq(0, 1, 0.1), ti = "", bgc = "grey", maptype = "world2") {
  if(maptype=="EPO") doxax <- "n" else doxax <- "s"
  plot(1:5, 1:5, ylim = latlim, xlim = lonlim, type = "n", xlab = "Longitude", ylab = "Latitude",xaxt=doxax)
    indat <- cbind(indat, vbl)
#    indat <- indat[indat$lon <= 140, ]
    a1 <- with(indat[indat$decade == dcd, ], tapply(vbl, list(lon, lat), sum))
    usr <- par('usr')
    rect(usr[1], usr[3], usr[2], usr[4], col = bgc)
    image(as.numeric(rownames(a1)), as.numeric(colnames(a1)), a1, add = T, col = heat.colors(length(brk) - 1), breaks = brk)
    contour(as.numeric(rownames(a1)), as.numeric(colnames(a1)), a1, add = T, levels = brk2)
    if(maptype %in% c("world2", "world")) {
      maps::map(database = maptype, add = T, interior = F, fill = T)
    }
    if(maptype=="EPO") map_EPO()
    title(paste(dcd, ti))
}

#' Plot map of catches or other spatial variable.
#'
#' Function to plot map of catches or other spatial variable. This version has no defined breaks, but ensures the spatial effects are constrained within the correct 1x1 square.
#' @param indat Input dataset to plot.
#' @param vbl The variable to plot spatially.
#' @param dcd Time identifier for selecting data.
#' @param latlim Latitude limits for plot.
#' @param lonlim Longitude limits for plot.
#' @param ti Title of the plot.
#' @param delta Lat and long resolution
#' @param bgc Background color
#' @param maptype Set to world, world2 or EPO.
#'
plot_catchmap2 <- function(indat, vbl, dcd, latlim = c(-40, 20), lonlim = c(20, 130), ti = "", delta=1, bgc = "grey", maptype = "world2") {
  if(maptype=="EPO") doxax <- "n" else doxax <- "s"
  plot(1:5, 1:5, ylim = latlim, xlim = lonlim, type = "n", xlab = "Longitude", ylab = "Latitude",xaxt=doxax)
    indat <- cbind(indat, vbl)
 #   indat <- indat[indat$lon <= 140, ]
    indat$lo <- factor(indat$lon, levels = seq(min(indat$lon, na.rm = T), max(indat$lon, na.rm = T), delta))
    indat$la <- factor(indat$lat, levels = seq(min(indat$lat, na.rm = T), max(indat$lat, na.rm = T), delta))
    a1 <- with(indat[indat$decade == dcd, ], tapply(vbl, list(lo, la), sum))
    usr <- par('usr')
    rect(usr[1], usr[3], usr[2], usr[4], col = bgc)
    image(as.numeric(rownames(a1)), as.numeric(colnames(a1)), a1, add = T)
    contour(as.numeric(rownames(a1)), as.numeric(colnames(a1)), a1, add = T)
    if(maptype %in% c("world2", "world")) {
      maps::map(database = maptype, add = T, interior = F, fill = T)
    }
    if(maptype=="EPO") map_EPO()
    title(paste(dcd, ti))
}

#' Plot map of cpue or the ratio of two other spatial variables.
#'
#' Function to plot map of cpue or other spatial variable. This version has defined breaks, but doesn't ensure the spatial effects are constrained within the correct 1x1 square.
#' @param indat Input dataset to plot.
#' @param vb1 The variable to plot spatially.
#' @param vb2 The variable to plot spatially.
#' @param dcd Time identifier for selecting data.
#' @param latlim Latitude limits for plot.
#' @param lonlim Longitude limits for plot.
#' @param brk Transition levels for the image colours.
#' @param brk2 Transition levels for the contour lines.
#' @param ti Title of the plot.
#' @param bgc Background color
#' @param maptype Set to world, world2 or EPO.
#'
plot_cpuemap <- function(indat, vb1, vb2, dcd, latlim = c(-40, 40), lonlim = c(120, 210), brk = seq(0, 1, 0.05), brk2 = seq(0, 1, 0.1), ti = "", bgc = "grey", maptype = "world2") {
  if(maptype=="EPO") doxax <- "n" else doxax <- "s"
  plot(1:5, 1:5, ylim = latlim, xlim = lonlim, type = "n", xlab = "Longitude", ylab = "Latitude",xaxt=doxax)
    indat <- cbind(indat, vb1, vb2)
    indat <- indat[indat$lon <= 210, ]
    a1 <- with(indat[indat$decade == dcd, ], tapply(vb1, list(lon, lat), sum)/tapply(vb2, list(lon, lat), sum))
    usr <- par('usr')
    rect(usr[1], usr[3], usr[2], usr[4], col = bgc)
    image(as.numeric(rownames(a1)), as.numeric(colnames(a1)), a1, add = T, col = heat.colors(length(brk) - 1), breaks = brk)
    contour(as.numeric(rownames(a1)), as.numeric(colnames(a1)), a1, add = T, levels = brk2)
    if(maptype %in% c("world2", "world")) {
       maps::map(database = maptype, add = T, interior = F, fill = T)
    }
    if(maptype=="EPO") map_EPO()
    title(paste(dcd, ti))
}

#' Plot map of cpue or the ratio of two other spatial variables.
#'
#' Function to plot map of cpue or other spatial variable. This version has no defined breaks, but ensures that the spatial effects are constrained within the correct 1x1 square.
#' @param indat Input dataset to plot.
#' @param vb1 The variable to plot spatially.
#' @param vb2 The variable to plot spatially.
#' @param dcd Time identifier for selecting data.
#' @param latlim Latitude limits for plot.
#' @param lonlim Longitude limits for plot.
#' @param ti Title of the plot.
#' @param delta The spatial resolution of the data being used.
#' @param bgc Background color
#' @param maptype Set to world, world2 or EPO.
#'
plot_cpuemap2 <- function(indat, vb1, vb2, dcd, latlim = c(-40, 40), lonlim = c(120, 210), ti = "", delta=1, bgc = "grey", maptype = "world2") {
  if(maptype=="EPO") doxax <- "n" else doxax <- "s"
  plot(1:5, 1:5, ylim = latlim, xlim = lonlim, type = "n", xlab = "Longitude", ylab = "Latitude",xaxt=doxax)
    indat <- cbind(indat, vb1, vb2)
#    indat <- indat[indat$lon <= 210, ]
    indat$lo <- factor(indat$lon, levels = seq(min(indat$lon, na.rm = T), max(indat$lon, na.rm = T), delta))
    indat$la <- factor(indat$lat, levels = seq(min(indat$lat, na.rm = T), max(indat$lat, na.rm = T), delta))
    a1 <- with(indat[indat$decade == dcd, ], tapply(vb1, list(lo, la), sum)/tapply(vb2, list(lo, la), sum))
    usr <- par('usr')
    rect(usr[1], usr[3], usr[2], usr[4], col = bgc)
    image(as.numeric(rownames(a1)), as.numeric(colnames(a1)), a1, add = T)
    contour(as.numeric(rownames(a1)), as.numeric(colnames(a1)), a1, add = T)
    if(maptype %in% c("world2", "world")) {
      maps::map(database = maptype, add = T, interior = F, fill = T)
    }
    if(maptype=="EPO") map_EPO()
    title(paste(dcd, ti))
}

#' Make effects plot for a glm object.
#'
#' Function to make effects plot for a glm object. Originally developed for analyses in the Indian Ocean.
#' @param model glm object to plot.
#' @param indat Input dataset used to create the glm.
#' @param dovess  If TRUE, plot effects for vessels.
#' @param addcl If TRUE, plot effects for clust.
#' @param addpca If TRUE, plot effects for PCA.
#' @param ti Title for plot.
#' @param dohbf If TRUE, plot effects for hbf.
#'
plot_effects_IO <- function(model, indat, dovess = T, addcl = F, addpca = F, ti = "", dohbf = T) {
    cf <- model$coefficients
    fam <- model$family$family
    pred <- predict(model, data = indat, type = "terms", se.fit = T)
#    fishlab <- switch(runsp, yft = "Yellowfin", bet = "Bigeye")
#    methlab <- switch(mt, deltabin = "Delta - binomial", deltapos = "Delta - positive", logl = "Lognormal(+mn)", propn = "Proportion Species")
    pr <- pred$fit
    prse <- pred$se.fit
    termlist <- dimnames(pred$fit)[[2]]
    docl = "clust" %in% termlist
    llpos <- grep("latlong", termlist)
    hbfpos <- grep("hbf", termlist)
    vesspos <- grep("vessid", termlist)
    branchpos <- grep("branchline", termlist)
    if (length(grep("hooks", termlist)) > 0)
        db <- T else db <- F

    nfigs = 3 + 2 * dovess + docl + addpca + dohbf
    mf <- switch(nfigs - 2, c(2, 2), c(2, 2), c(2, 3), c(2, 3), c(3, 3), c(3, 3), c(3, 3), c(3, 4))
    hw <- c(14, 19)
    dev.new(height = hw[1], width = hw[2], noRStudioGD = TRUE)
    par(mfrow = mf, mar = c(5, 4, 2, 1), oma = c(0, 0, 3, 0))

    coefs <- glm.coefs(model, indat)
    if (fam == "gaussian")
        lims <- c(0, 3) else lims <- c(0, 1)
    with(coefs, plot(as.numeric(as.character(yrqtr)), fit, ylim = lims, xlab = "Year", ylab = "Effect size", main = "Year effects"))
    with(coefs, segments(as.numeric(as.character(yrqtr)), lo, as.numeric(as.character(yrqtr)), hi, lty = 1, col = "slate grey"))
    with(coefs, points(as.numeric(as.character(yrqtr)), fit, pch = 16))

    coefs <- glm.coefs2(model, indat, parm = "latlong")
    with(coefs, plot(as.numeric(latlong), fit, ylim = lims, xlab = "Latlong", ylab = "Effect size", main = "Spatial effects"))
    with(coefs, segments(as.numeric(latlong), lo, as.numeric(latlong), hi, lty = 1, col = "slate grey"))
    with(coefs, points(as.numeric(latlong), fit, pch = 16))

    if (fam == "gaussian")
        trans <- function(x) exp(x)
    if (fam != "gaussian")
        trans <- function(x) inv.logit(x)

    ## plot coefficients
    requireNamespace("maps")
    ll <- as.character(indat$latlong)
    # browser()
    index <- sort(unique(ll))
    index2 <- unlist(strsplit(index, "\\_"))
    pos1 <- 2 * (1:length(index)) - 1
    pos2 <- pos1 + 1
    lats <- as.numeric(index2[pos1])
    lons <- as.numeric(index2[pos2])
    alat <- round(lats[match(ll, index)] * 2)/2
    alon <- round(lons[match(ll, index)] * 2)/2
    coefs <- exp(pr[, llpos])
    coefs2 <- tapply(coefs, list(alon, alat), mean, na.rm = TRUE)
    coefs2 <- coefs2/mean(coefs2, na.rm = TRUE)
    image(sort(as.numeric(unique(alon))), sort(unique(alat)), coefs2, zlim = c(0, 4), ylab = "Lat", xlab = "Long")
    if (length(coefs2) > 5)
        contour(sort(unique(alon)), sort(unique(alat)), coefs2, levels = c(0, 1, 2, 2.5), add = TRUE, col = 4)
    if(max(alon, na.rm = TRUE) > 180) odb <- "world2" else odb <- "world"
    maps::map(database = odb, add = TRUE, fill =TRUE)


    if (dovess) {
        coefs <- glm.coefs2(model, indat, parm = "vessid")
        if (fam == "gaussian")
            lims <- c(0, 3) else lims <- c(0, 1)
        with(coefs, plot(as.numeric(vessid), fit, ylim = lims, xlab = "Vessel ID", ylab = "Effect size", main = ""))
        with(coefs, segments(as.numeric(vessid), lo, as.numeric(vessid), hi, lty = 1, col = "slate grey"))
        with(coefs, points(as.numeric(vessid), fit, pch = 16))

        vts <- tapply(pr[, vesspos], list(indat$yrqtr, indat$vessid), mean)
        y <- trans(as.vector(vts))
        x <- rep(as.numeric(dimnames(vts)[[1]]), dim(vts)[[2]])
        plot(x, y, type = "p", ylim = lims, xlab = "Vessel ID", ylab = "Effect size", main = "Vessel effects", pch = 16, cex = 0.9)
        mn <- tapply(trans(pr[, vesspos]), indat$yrqtr, mean)
        lines(as.numeric(dimnames(vts)[[1]]), mn, col = 2, lwd = 2)
        # plot(as.factor(indat$vessid), pr[, vesspos], ylim = c( - .75, .75), xlab = 'Vessel', ylab = 'Effect size')
    }

    # if (dohbf) { rsp <- pr[, hbfpos[1]] rsp.se <- prse[, hbfpos[1]] b <- match(sort(unique(indat$hbf)), indat$hbf) plot(indat$hbf[b], trans(pr[,
    # hbfpos][b]), ylim = lims, xlab = 'HBF', ylab = 'Effect size', main = 'HBF effects') lines(indat$hbf[b], trans(rsp[b]+2*rsp.se[b]), lty = 2)
    # lines(indat$hbf[b], trans(rsp[b] - 2*rsp.se[b]), lty = 2) }

    if (dohbf) {
        coefs <- glm.coefs2(model, indat, parm = "hbf")
        if (fam == "gaussian")
            lims <- c(0, 3) else lims <- c(0, 1)
        with(coefs, plot(hbf, fit, ylim = lims, type = "l", xlab = "HBF", ylab = "Effect size", main = "HBF effects"))
        with(coefs, lines(hbf, hi, lty = 2))
        with(coefs, lines(hbf, lo, lty = 2))
    }

    if (docl) {
        coefs <- glm.coefs2(model, indat, parm = "clust")
        if (fam == "gaussian")
            lims <- c(0, 3) else lims <- c(0, 1)
        with(coefs, plot(clust, fit, ylim = lims, xlab = "Cluster", ylab = "Effect size", main = "Cluster effects"))
        with(coefs, segments(as.numeric(clust), lo, as.numeric(clust), hi, lty = 1, col = "slate grey"))
    }

    if (addpca) {
        ind <- indat[order(indat$pca), ]
        b <- match(sort(unique(indat$pca)), indat$pca)
        out <- trans(pr[b, 1])
        se1 <- trans(pr[b, 1] - 1.96 * prse[b, 1])
        se2 <- trans(pr[b, 1] + 1.96 * prse[b, 1])
        mout <- mean(out)
        out <- out/mout
        se1 <- se1/mout
        se2 <- se2/mout
        plot(as.numeric(as.character(index)), out, ylim = lims, xlab = "Cluster category", ylab = "Effect size", main = "Cluster effects")
        segments(as.numeric(as.character(index)), se1, as.numeric(as.character(index)), se2, lty = 1, col = "slate grey")
        points(as.numeric(as.character(index)), out, pch = 16)
    }
    title(main = ti, cex.main = 1.5, outer = T)
}

#' Make effects plot for a glm object.
#'
#' Function to make effects plot for a glm object. Originally developed for analyses in the Indian Ocean. No longer used.
#' @param model glm object to plot.
#' @param indat Input dataset used to create the glm.
#' @param dovess  If TRUE, plot effects for vessels.
#' @param addcl If TRUE, plot effects for clust.
#' @param addpca If TRUE, plot effects for PCA.
#' @param ti Title for plot.
#' @param dohbf If TRUE, plot effects for hbf.
#'
plot_effects_IO_old <- function(model, indat, dovess = T, addcl = F, addpca = F, ti = "", dohbf = T) {
    cf <- model$coefficients
    pred <- predict(model, data = indat, type = "terms", se.fit = T)
#    fishlab <- switch(runsp, yft = "Yellowfin", bet = "Bigeye")
#    methlab <- switch(mt, deltabin = "Delta - binomial", deltapos = "Delta - positive", logl = "Lognormal(+0.01)", propn = "Proportion Bigeye")
    if (addcl != F)
        nfigs <- 6 + addpca
    if (addcl == F)
        nfigs <- 5 + addpca
    mf <- switch(nfigs - 4, c(2, 3), c(2, 3), c(3, 3), c(3, 3), c(3, 3), c(3, 4))
    hw <- c(14, 19)
    dev.new(height = hw[1], width = hw[2], noRStudioGD = TRUE)
    par(mfrow = mf, mar = c(5, 4, 2, 1), oma = c(0, 0, 3, 0))
    pr <- pred$fit
    prse <- pred$se.fit
    termlist <- dimnames(pred$fit)[[2]]
    llpos <- grep("latlong", termlist)
    hbfpos <- grep("hbf", termlist)
    vesspos <- grep("vessid", termlist)
    branchpos <- grep("branchline", termlist)
    if (length(grep("hooks", termlist)) > 0)
        db <- T else db <- F

    # browser()
    index <- sort(unique(indat$yrqtr))
    b <- match(index, indat$yrqtr)
    out <- exp(pr[b, 1])
    se1 <- exp(pr[b, 1] - 1.96 * prse[b, 1])
    se2 <- exp(pr[b, 1] + 1.96 * prse[b, 1])
    mout <- median(out)
    out <- out/mout
    se1 <- se1/mout
    se2 <- se2/mout
    plot(as.numeric(as.character(index)), out, ylim = c(0, 4), xlab = "Year", ylab = "Effect size", main = "Year effects")
    segments(as.numeric(as.character(index)), se1, as.numeric(as.character(index)), se2, lty = 1, col = "slate grey")
    points(as.numeric(as.character(index)), out, pch = 16)

    index <- sort(unique(indat$latlong))
    b <- match(index, indat$latlong)
    out <- exp(pr[b, llpos])
    se1 <- exp(pr[b, llpos] - 1.96 * prse[b, llpos])
    se2 <- exp(pr[b, llpos] + 1.96 * prse[b, llpos])
    mout <- median(out)
    out <- out/mout
    se1 <- se1/mout
    se2 <- se2/mout
    plot(as.numeric(index), out, ylim = c(0, 4), xlab = "Latlong", ylab = "Effect size", main = "Spatial effects")
    segments(as.numeric(index), se1, as.numeric(index), se2, lty = 1, col = "slate grey")
    points(as.numeric(index), out, pch = 16)
    # plot(indat$latlong, pr[, 2], ylim = c( - .75, .75), xlab = 'Latitude', ylab = 'Effect size')

    ## plot coefficients
    requireNamespace("maps")
    ll <- as.character(indat$latlong)
    # browser()
    index <- sort(unique(ll))
    index2 <- unlist(strsplit(index, "\\_"))
    pos1 <- 2 * (1:length(index)) - 1
    pos2 <- pos1 + 1
    lats <- as.numeric(index2[pos1])
    lons <- as.numeric(index2[pos2])
    alat <- round(lats[match(ll, index)] * 2)/2
    alon <- round(lons[match(ll, index)] * 2)/2
    coefs <- exp(pr[, llpos])
    coefs2 <- tapply(coefs, list(alon, alat), mean, na.rm = TRUE)
    coefs2 <- coefs2/mean(coefs2, na.rm = TRUE)
    image(sort(as.numeric(unique(alon))), sort(unique(alat)), coefs2, zlim = c(0, 4), ylab = "Lat", xlab = "Long")
    if (length(coefs2) > 5)
        contour(sort(unique(alon)), sort(unique(alat)), coefs2, levels = c(0, 1, 2, 2.5), add = TRUE, col = 4)
    if(max(alon, na.rm = TRUE) > 180) odb <- "world2" else odb <- "world"
    maps::map(database = odb, add = TRUE, fill =TRUE)

    if (dovess) {
        index <- sort(unique(indat$vessid))
        b <- match(index, indat$vessid)
        out <- exp(pr[b, vesspos])
        se1 <- exp(pr[b, vesspos] - 1.96 * prse[b, vesspos])
        se2 <- exp(pr[b, vesspos] + 1.96 * prse[b, vesspos])
        mout <- median(out)
        out <- out/mout
        se1 <- se1/mout
        se2 <- se2/mout
        ind <- as.factor(index)
        plot(match(ind, ind), out, type = "n", ylim = c(0, 4), xlab = "Vessel ID", ylab = "Effect size", main = "")
        segments(match(ind, ind), se1, match(ind, ind), se2, lty = 1, col = "slate grey")
        points(match(ind, ind), out, pch = 16)

        vts <- tapply(pr[, vesspos], list(indat$yrqtr, indat$vessid), mean)
        y <- exp(as.vector(vts))
        x <- rep(as.numeric(dimnames(vts)[[1]]), dim(vts)[[2]])
        plot(x, y, type = "p", ylim = c(0, 3), xlab = "Vessel ID", ylab = "Effect size", main = "Vessel effects", pch = 16, cex = 0.9)
        mn <- tapply(exp(pr[, vesspos]), indat$yrqtr, mean)
        lines(as.numeric(dimnames(vts)[[1]]), mn, col = 2, lwd = 2)
        # plot(as.factor(indat$vessid), pr[, vesspos], ylim = c( - .75, .75), xlab = 'Vessel', ylab = 'Effect size')
    }

    if (dohbf) {
        rsp <- pr[, hbfpos[1]]
        rsp.se <- prse[, hbfpos[1]]
        b <- match(sort(unique(indat$hbf)), indat$hbf)
        plot(indat$hbf[b], exp(pr[, hbfpos][b]), ylim = c(0, 3), xlab = "HBF", ylab = "Effect size", main = "HBF effects")
        lines(indat$hbf[b], exp(rsp[b] + 2 * rsp.se[b]), lty = 2)
        lines(indat$hbf[b], exp(rsp[b] - 2 * rsp.se[b]), lty = 2)
    }

    if (addcl != F) {
        index <- sort(unique(indat$cl))
        b <- match(index, indat$cl)
        out <- exp(pr[b, 1])
        se1 <- exp(pr[b, 1] - 1.96 * prse[b, 1])
        se2 <- exp(pr[b, 1] + 1.96 * prse[b, 1])
        mout <- mean(out)
        out <- out/mout
        se1 <- se1/mout
        se2 <- se2/mout
        plot(index, out, ylim = c(0, 3), xlab = "Cluster category", ylab = "Effect size", main = "Cluster effects")
        segments(as.numeric(index), se1, as.numeric(index), se2, lty = 1, col = "slate grey")
        points(index, out, pch = 16)
    }

    if (addpca) {
        ind <- indat[order(indat$pca), ]
        out <- exp(pr[b, 1])
        se1 <- exp(pr[b, 1] - 1.96 * prse[b, 1])
        se2 <- exp(pr[b, 1] + 1.96 * prse[b, 1])
        mout <- mean(out)
        out <- out/mout
        se1 <- se1/mout
        se2 <- se2/mout
        plot(as.numeric(as.character(index)), out, ylim = c(0, 3), xlab = "Cluster category", ylab = "Effect size", main = "Cluster effects")
        segments(as.numeric(as.character(index)), se1, as.numeric(as.character(index)), se2, lty = 1, col = "slate grey")
        points(as.numeric(as.character(index)), out, pch = 16)
    }
    title(main = ti, cex.main = 1.5, outer = T)
}

#' Make effects plot for a glm object with interactions.
#'
#' Function to make effects plot for a glm object with time-area interactions. Originally developed for analyses in the Indian Ocean.
#' @param model glm object to plot.
#' @param indat Input dataset used to create the glm.
#' @param dovess  If TRUE, plot effects for vessels.
#' @param addcl If TRUE, plot effects for clust.
#' @param addpca If TRUE, plot effects for PCA.
#' @param ti Title for plot.
#' @param dohbf If TRUE, plot effects for hbf.
#'
plot_effects_IOx <- function(model, indat, dovess = T, addcl = F, addpca = F, ti = "", dohbf = T) {
    cf <- model$coefficients
    pred <- predict(model, data = indat, type = "terms", se.fit = T)
#    fishlab <- switch(runsp, yft = "Yellowfin", bet = "Bigeye")
#    methlab <- switch(mt, deltabin = "Delta-binomial", deltapos = "Delta-positive", logl = "Lognormal(+0.01)", propn = "Proportion Bigeye")
    if (addcl != F)
        nfigs <- 6 + addpca
    if (addcl == F)
        nfigs <- 5 + addpca
    mf <- switch(nfigs - 4, c(2, 3), c(2, 3), c(3, 3), c(3, 3), c(3, 3), c(3, 4))
    hw <- c(14, 19)
    dev.new(height = hw[1], width = hw[2], noRStudioGD = TRUE)
    par(mfrow = mf, mar = c(5, 4, 2, 1), oma = c(0, 0, 3, 0))
    pr <- pred$fit
    prse <- pred$se.fit
    termlist <- dimnames(pred$fit)[[2]]
    llpos <- grep("latlong", termlist)
    hbfpos <- grep("hbf", termlist)
    vesspos <- grep("vessid", termlist)
    branchpos <- grep("branchline", termlist)
    if (length(grep("hooks", termlist)) > 0)
        db <- T else db <- F

    # browser()
    index <- sort(unique(indat$yrqtr))
    b <- match(index, indat$yrqtr)
    out <- exp(pr[b, 1])
    se1 <- exp(pr[b, 1] - 1.96 * prse[b, 1])
    se2 <- exp(pr[b, 1] + 1.96 * prse[b, 1])
    mout <- mean(out)
    out <- out/mout
    se1 <- se1/mout
    se2 <- se2/mout
    plot(as.numeric(as.character(index)), out, ylim = c(0, 3), xlab = "Year", ylab = "Effect size", main = "Year effects")
    segments(as.numeric(as.character(index)), se1, as.numeric(as.character(index)), se2, lty = 1, col = "slate grey")
    points(as.numeric(as.character(index)), out, pch = 16)

    index <- sort(unique(indat$latlong))
    b <- match(index, indat$latlong)
    out <- exp(pr[b, llpos])
    se1 <- exp(pr[b, llpos] - 1.96 * prse[b, llpos])
    se2 <- exp(pr[b, llpos] + 1.96 * prse[b, llpos])
    mout <- mean(out)
    out <- out/mout
    se1 <- se1/mout
    se2 <- se2/mout
    plot(as.numeric(index), out, ylim = c(0, 3), xlab = "Latlong", ylab = "Effect size", main = "Spatial effects")
    segments(as.numeric(index), se1, as.numeric(index), se2, lty = 1, col = "slate grey")
    points(as.numeric(index), out, pch = 16)
    # plot(indat$latlong,pr[,2],ylim=c(-.75,.75),xlab='Latitude',ylab='Effect size')

    ## plot coefficients
    requireNamespace("maps")
    ll <- as.character(indat$latlong)
    index <- sort(unique(ll))
    index2 <- unlist(strsplit(index, "\\_"))
    pos1 <- 2 * (1:length(index)) - 1
    pos2 <- pos1 + 1
    lats <- as.numeric(index2[pos1])
    lons <- as.numeric(index2[pos2])
    alat <- round(lats[match(ll, index)] * 2)/2 + 2.5
    alon <- round(lons[match(ll, index)] * 2)/2 + 2.5
    # lat <- trunc((dat$newlat[match(index, dat$latlong)] + 50)/5) * 5 + 2.5 - 50 long <- trunc((dat$newlong[match(index, dat$latlong)])/5) * 5 + 2.5
    coefs <- exp(pr[match(ll, index), llpos])
    coefs2 <- tapply(coefs, list(alon, alat), mean)
    coefs2 <- coefs2/mean(coefs2)
    image(sort(as.numeric(unique(alon))), sort(unique(alat)), coefs2, zlim = c(0.5, 2.5), ylab = "Lat", xlab = "Long")
    if (length(coefs2) > 5)
        contour(sort(unique(alon)), sort(unique(alat)), coefs2, levels = c(0, 1, 2, 2.5), add = TRUE, col = 4)
    if(max(alon, na.rm = TRUE) > 180) odb <- "world2" else odb <- "world"
    maps::map(database = odb, add = TRUE, fill =TRUE)

    if (dovess) {
        index <- sort(unique(indat$vessid))
        b <- match(index, indat$vessid)
        out <- exp(pr[b, vesspos])
        se1 <- exp(pr[b, vesspos] - 1.96 * prse[b, vesspos])
        se2 <- exp(pr[b, vesspos] + 1.96 * prse[b, vesspos])
        mout <- mean(out)
        out <- out/mout
        se1 <- se1/mout
        se2 <- se2/mout
        ind <- as.factor(index)
        plot(match(ind, ind), out, type = "n", ylim = c(0, 3), xlab = "Vessel ID", ylab = "Effect size", main = "")
        segments(match(ind, ind), se1, match(ind, ind), se2, lty = 1, col = "slate grey")
        points(match(ind, ind), out, pch = 16)

        vts <- tapply(pr[, vesspos], list(indat$yrqtr, indat$vessid), mean)
        y <- exp(as.vector(vts))
        x <- rep(as.numeric(dimnames(vts)[[1]]), dim(vts)[[2]])
        plot(x, y, type = "p", ylim = c(0, 3), xlab = "Vessel ID", ylab = "Effect size", main = "Vessel effects", pch = 16, cex = 0.9)
        mn <- tapply(exp(pr[, vesspos]), indat$yrqtr, mean)
        lines(as.numeric(dimnames(vts)[[1]]), mn, col = 2, lwd = 2)
        # plot(as.factor(indat$vessid),pr[,vesspos],ylim=c(-.75,.75),xlab='Vessel',ylab='Effect size')
    }

    if (dohbf) {
        rsp <- pr[, hbfpos[1]]
        rsp.se <- prse[, hbfpos[1]]
        b <- match(sort(unique(indat$hbf)), indat$hbf)
        plot(indat$hbf[b], exp(pr[, hbfpos][b]), ylim = c(0, 3), xlab = "HBF", ylab = "Effect size", main = "HBF effects")
        lines(indat$hbf[b], exp(rsp[b] + 2 * rsp.se[b]), lty = 2)
        lines(indat$hbf[b], exp(rsp[b] - 2 * rsp.se[b]), lty = 2)
    }

    if (addcl != F) {
        index <- sort(unique(indat$cl))
        b <- match(index, indat$cl)
        out <- exp(pr[b, 1])
        se1 <- exp(pr[b, 1] - 1.96 * prse[b, 1])
        se2 <- exp(pr[b, 1] + 1.96 * prse[b, 1])
        mout <- mean(out)
        out <- out/mout
        se1 <- se1/mout
        se2 <- se2/mout
        plot(index, out, ylim = c(0, 3), xlab = "Cluster category", ylab = "Effect size", main = "Cluster effects")
        segments(as.numeric(index), se1, as.numeric(index), se2, lty = 1, col = "slate grey")
        points(index, out, pch = 16)
    }

    if (addpca) {
        ind <- indat[order(indat$pca), ]
        out <- exp(pr[b, 1])
        se1 <- exp(pr[b, 1] - 1.96 * prse[b, 1])
        se2 <- exp(pr[b, 1] + 1.96 * prse[b, 1])
        mout <- mean(out)
        out <- out/mout
        se1 <- se1/mout
        se2 <- se2/mout
        plot(as.numeric(as.character(index)), out, ylim = c(0, 3), xlab = "Cluster category", ylab = "Effect size", main = "Cluster effects")
        segments(as.numeric(as.character(index)), se1, as.numeric(as.character(index)), se2, lty = 1, col = "slate grey")
        points(as.numeric(as.character(index)), out, pch = 16)
    }
    title(main = ti, cex.main = 1.5, outer = T)
}

