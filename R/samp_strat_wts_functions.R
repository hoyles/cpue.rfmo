#' Randomly sample dataset stratified by grid and yearqtr.
#'
#' The function reduces the size of the dataset per stratum to approximately nsamp.
#' @param dd Input dataset.
#' @param nsamp The number of samples needed.
#' @return Modified dataset.
#'
samp_strat_data <- function(dd, nsamp) {
    a <- tapply(dd$latlong, list(dd$latlong, dd$yrqtr), length)
    i <- match(dd$latlong, rownames(a))
    j <- match(dd$yrqtr, colnames(a))
    n <- mapply("[", list(a), i, j)
    p <- nsamp/n
    r <- runif(length(n))
    d2 <- dd[r < p, ]
    return(d2)
}

#' Randomly sample dataset.
#'
#' The function reduces the size of the dataset per stratum to approximately nsamp.
#' @param dat Input dataset
#' @param n A vector indicating the number of samples in the stratum of which the row is a member.
#' @param nsamp An integer indicating the maximum preferred number of samples per stratum.
#' @return Modified dataset.
#'
samp_data <- function(dat, n, nsamp) {
  p <- nsamp/n
  r <- runif(length(n))
  d2 <- dat[r < p, ]
  return(d2)
}

#' Randomly sample dataset.
#'
#' The function reduces the size of the dataset to proportion p of the original size.
#' @param dat Input dataset
#' @param p Proportion.
#' @return Modified dataset.
#'
samp_data2 <- function(dat, p) {
  nn <- dim(dat)[1]
  nsamp <- floor(nn * p)
  r <- runif(nn)
  d1 <- dat[order(r), ]
  d2 <- d1[1:nsamp, ]
  return(d2)
}

#' Stratify dataset prior to sampling.
#'
#' The function stratifies the dataset by grid cell and yrqtr, and returns a vector indicating the number of samples in the stratum of which the row is a member.
#' @param dat Input dataset
#' @return A vector indicating the number of samples in the stratum of which the row is a member.
#'
make_strat <- function(dat) {
  a <- tapply(dat$latlong, list(dat$latlong, dat$yrqtr), length)
  i <- match(dat$latlong, rownames(a))
  j <- match(dat$yrqtr, colnames(a))
  n <- mapply("[", list(a), i, j)
  return(n)  # n
}

#' Allocate statistical weights to each row.
#'
#' The function returns a vector indicating a statistical weight to apply to the row, based on the stratum of which the row is a member.
#' @param dat Input dataset
#' @param wttype Type of statistical weighting method; 'equal' gives equal weight to all rows, 'area' weights rows in invrse proportion to the number of rows per stratum; and 'catch' weights rows in proportion to the catch in the stratum divided by the number of rows in the stratum.
#' @param catch An optional vector of catch per stratum.
#' @return A vector indicating the statistical weight to apply to the stratum of which the row is a member.
#'
mk_wts_integer <- function(dat, wttype, catch = NULL) {
  if (wttype == "equal")
    wts <- NULL
  if (wttype == "area") {
    a <- tapply(dat$latlong, list(dat$latlong, dat$yrqtr), length)
    i <- match(dat$latlong, rownames(a))
    j <- match(dat$yrqtr, colnames(a))
    n <- mapply("[", list(a), i, j)
    wts <- 1/n
    wts <- floor(10 * max(n) * wts)
  }
  if (wttype == "catch") {
    if (is.null(catch))
      catch <- tapply(dat$bet, list(dat$latlong), sum)
    a <- tapply(dat$latlong, list(dat$latlong, dat$yrqtr), length)
    i <- match(dat$latlong, rownames(a))
    j <- match(dat$yrqtr, colnames(a))
    n <- mapply("[", list(a), i, j)
    cwts <- mapply("[", list(catch), i)/sum(catch)
    wts <- cwts/n
  }
  return(wts)
}

#' Allocate statistical weights to each row.
#'
#' The function returns a vector indicating a statistical weight to apply to the row, based on the stratum of which the row is a member.
#' @param dat Input dataset
#' @param wttype Type of statistical weighting method; 'equal' gives equal weight to all rows, 'area' weights rows in invrse proportion to the number of rows per stratum; and 'catch' weights rows in proportion to the catch in the stratum divided by the number of rows in the stratum.
#' @param catch An optional vector of catch per stratum.
#' @param sp The species to be used for catch weighting.
#' @return A vector indicating the statistical weight to apply to the stratum of which the row is a member.
#'
mk_wts <- function(dat, wttype, catch = NULL, sp = NULL) {
  if (wttype == "equal")
    wts <- NULL
  if (wttype == "propn")
    wts <- catch
  if (wttype == "area") {
    a <- tapply(dat$latlong, list(dat$latlong, dat$yrqtr), length)
    i <- match(dat$latlong, rownames(a))
    j <- match(dat$yrqtr, colnames(a))
    n <- mapply("[", list(a), i, j)
    wts <- 1/n
  }
  if (wttype == "catch") {
    if (is.null(catch))
      catch <- tapply(dat[, sp], list(dat$latlong), sum)
    a <- tapply(dat$latlong, list(dat$latlong, dat$yrqtr), length)
    i <- match(dat$latlong, rownames(a))
    j <- match(dat$yrqtr, colnames(a))
    n <- mapply("[", list(a), i, j)
    cwts <- mapply("[", list(catch), i)/sum(catch)
    wts <- cwts/n
  }
  return(wts)
}

