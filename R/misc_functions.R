#' Select vessels based on quarters fished.
#'
#' The function reduces the size of the dataset according to criteria about number of quarters fished by a vessel.
#' @param gdat Input dataset
#' @param minqtrs Minimum number of quartrs fished by a vessel.
#' @param maxqtrs Maximum number of quartrs fished by a vessel, defaults to very high number.
#' @return Modified dataset.
#'
limit_vessels <- function(gdat, minqtrs, maxqtrs = 10000) {
  a <- table(gdat$vessid, gdat$yrqtr)
  a <- apply(a > 0, 1, sum)
  table(a)
  a <- a[a >= minqtrs & a <= maxqtrs]
  gdat <- gdat[gdat$vessid %in% names(a), ]
  a <- table(gdat$yrqtr)
  a
  a <- a[a >= 100]
  gdat <- gdat[gdat$yrqtr %in% names(a), ]
  a <- table(gdat$vessid)
  a
  a <- a[a >= 100]
  gdat <- gdat[gdat$vessid %in% names(a), ]
  return(gdat)
}

#' Select vessels based on quarters fished.
#'
#' The function splits all vessels' time series after a specified number of years.
#' @param indat Input dataset
#' @param period The number of years before the vessel id is changed.
#' @return Modified dataset.
#'
splitvessels <- function(indat, period) {
  vess <- unique(indat$vessid)
  indat$oldvess <- indat$vessid
  indat$vessid <- ""
  minyr <- maxyr <- vess
  for (i in 1:length(vess)) {
    a <- grep(vess[i], indat$oldvess)
    minyr <- min(indat[a, ]$yrqtr)
    indat[a, ]$vessid <- paste(indat[a, ]$oldvess, floor((indat[a, ]$yrqtr - minyr)/period))
  }
  return(indat)
}

