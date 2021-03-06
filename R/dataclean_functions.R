#' Clean data.
#'
#' The function sets up variables and removes bad data. Originally developed for Japanese data in the IO.
#' @param dat Input dataset
#' @param checktg If TRUE, remove effort identified as not targetng tuna. Not generally used.
#' @param allHBF Deprecated.
#' @param splist List of species codes
#' @return Modified dataset.
#'
dataclean_CNIO <- function(dat, checktg = F, allHBF = F, splist = c("bft","sbt","alb","bet","yft","swo","mls","bum","blm","sas","sha")) {
  dat$op_yr <- as.numeric(dat$op_yr)
  dat$op_mon <- as.numeric(dat$op_mon)
  dmy <- as.Date(dat$date)
  dat <- dat[!is.na(dmy), ]
  dat <- dat[!is.na(dat$lat), ]
  dat <- dat[!is.na(dat$lon), ]
#  dat <- dat[!is.na(dat$hooks), ]
  hist(dat$hooks, useNA = "always")
  dat$hbf <- as.numeric(dat$hbf)
  for (sp in splist) {
    dat[,sp] <- as.numeric(dat[,sp])
    if (sum(is.na(dat[,sp])) > 0) dat[is.na(dat[,sp]), sp] <- 0
  }
  dat <- dat[!is.na(dat$hooks), ]
  dat <- dat[dat$hooks < 5000, ]  # clean up outliers
  dat <- dat[dat$hooks > 200, ]
  dat <- dat[(!is.na(dat$hbf) & dat$hbf < 26) | (dat$op_yr < 1976 & is.na(dat$hbf)), ]
  return(dat)
}


#' Clean data.
#'
#' The function sets up variables and removes bad data. Originally developed for Japanese data in the IO.
#' @param dat Input dataset
#' @param checktg If TRUE, remove effort identified as not targetng tuna. Not generally used.
#' @param allHBF Deprecated.
#' @param splist List of species codes
#' @return Modified dataset.
#'
dataclean_JPIO <- function(dat, checktg = F, allHBF = F, splist = c("bft","sbt","alb","bet","yft","swo","mls","bum","blm","sas","sha")) {
  dat$op_yr <- as.numeric(dat$op_yr)
  dat$op_mon <- as.numeric(dat$op_mon)
  dat$op_day <- as.numeric(dat$op_day)
  dat <- dat[dat$op_day < 32, ]
  dat <- dat[!is.na(dat$op_day), ]
  dmy <- as.Date(paste(dat$op_yr, dat$op_mon, dat$op_day, sep = "-"))
  dat <- dat[!is.na(dmy), ]
  dat$lat <- as.numeric(dat$lat)
  dat <- dat[!is.na(dat$lat), ]
  dat <- dat[!is.na(dat$lon), ]
  dat$latcode <- as.numeric(dat$latcode)
  dat$lon <- as.numeric(dat$lon)
  dat$loncode <- as.numeric(dat$loncode)
  dat <- dat[dat$loncode %in% c(1, 2), ]
  dat$hbf <- as.numeric(dat$hbf)
  # dat$tonnage <- as.numeric(dat$tonnage)
  dat$hooks <- as.numeric(dat$hooks)
  for (sp in splist) {
    dat[,sp] <- as.numeric(dat[,sp])
    if (sum(is.na(dat[,sp])) > 0) dat[is.na(dat[,sp]), sp] <- 0
  }
  dat <- dat[!is.na(dat$hooks), ]
  dat <- dat[dat$hooks < 5000, ]  # clean up outliers
  dat <- dat[dat$hooks > 200, ]
  # dat <- dat[dat$yft < 250, ] dat <- dat[dat$bet < 250, ] dat <- dat[dat$alb < 250, ] dat <- dat[dat$tonnage < 30000 | is.na(dat$tonnage), ]
  # dat[dat$fishingcat == '0', ] dat <- dat[dat$fishingcat != '.', ] dat <- dat[dat$fishingcat != '0', ] dat <- dat[dat$hbf != ' .', ] dat <-
  # dat[is.na(dat$hbf) == FALSE | dat$op_yr < 1976, ] dat <- dat[dat$hbf < 26 | is.na(dat$hbf) == TRUE, ]
  dat <- dat[(!is.na(dat$hbf) & dat$hbf < 26) | (dat$op_yr < 1976 & is.na(dat$hbf)), ]
  # if (allHBF == F) { dat[dat$hbf > 22, ]$hbf <- 22 # pool hbf > 22 into 22 dat <- dat[dat$hbf > 4, ] # remove swordfish targeting in R1 and R2 }
  # dat$ncrew <- as.numeric(dat$ncrew)
  if (checktg)
    dat <- dat[dat$target == 3 | is.na(dat$target), ]  # tuna target  (remove to avoid a change in 1994 - but recent trend is more important)
  return(dat)
}

#' Clean data.
#'
#' The function sets up variables and removes bad data. Originally developed for Japanese data in the IO.
#' @param dat Input dataset
#' @param checktg If TRUE, remove effort identified as not targetng tuna. Not generally used.
#' @param splist List of species codes
#' @return Modified dataset.
#'
dataclean_JP_EPO <- function(dat, checktg = F, splist = c("alb","bet","yft","swo","mls","bum","blm","sas","sha","sfa","ssp")) {
  dat$hooks <- as.numeric(dat$hooks)
  dat <- dat[!is.na(dat$hooks) & !is.na(dat$dmy) & !is.na(dat$lat) & !is.na(dat$lon), ]
  for (sp in splist) {
    dat[,sp] <- as.numeric(dat[,sp])
    if (sum(is.na(dat[,sp])) > 0) dat[is.na(dat[,sp]), sp] <- 0
  }
  dat <- dat[dat$hooks < 10000, ]  # clean up outliers
  dat <- dat[dat$hooks > 200, ]
  dat <- dat[(!is.na(dat$hbf) & dat$hbf < 26) | (dat$op_yr < 1976 & is.na(dat$hbf)), ]

  if (checktg)
    dat <- dat[dat$target == 3 | is.na(dat$target), ]  # tuna target  (remove to avoid a change in 1994 - but recent trend is more important)

  dat <- dat[dat$vessel_type==1 | dat$vessel_type==4 |(is.na(dat$vessel_type)) | (dat$vessel_type==2 & dat$op_yr < 1971) ,]   # remove data fotr training vessels
  return(dat)
}

#' Clean data.
#'
#' The function sets up variables and removes bad data. Originally developed for Korean data in the IO.
#' @param dat Input dataset
#' @param yearlim Upper boundary for time period.
#' @param splist List of species codes
#' @return Modified dataset.
#'
dataclean_KR <- function(dat, yearlim = 2016, splist) {
    for (sp in splist) {
      dat[,sp] <- as.numeric(dat[,sp])
      if (sum(is.na(dat[,sp])) > 0) dat[is.na(dat[,sp]), sp] <- 0
    }
    dat <- dat[!is.na(dat$hooks), ]  # Clean up 294 NAs
    dat <- dat[dat$hooks < 5000, ]  # clean up outliers
    # dat <- dat[dat$hooks > 200, ]
    dat <- dat[dat$hooks >= 1000, ]
    dat <- dat[is.na(dat$hbf) == F, ]
    dat <- dat[dat$op_yr > 1976, ]
    dat <- dat[dat$yrqtr < yearlim, ]
    #dat <- dat[dat$EW == 1, ]
    dat <- dat[dat$hbf >= 5, ]
    return(dat)
}

#' Clean data.
#'
#' The function sets up variables and removes bad data. Originally developed for Taiwanese data in the IO.
#' @param dat1 Input dataset
#' @param doHBF If TRUE, remove HBF > 25.
#' @param splist List of species codes
#' @return Modified dataset.
#'
dataclean_TW_std <- function(dat1, doHBF = F, splist = c("alb", "bet", "yft", "ott", "swo", "mls", "bum", "blm", "otb", "skj", "sha", "oth", "sbt")) {
    dat1 <- dat1[!is.na(dat1$hooks), ]
    dat1 <- dat1[dat1$hooks < 10000, ]
    dat1 <- dat1[dat1$hooks > 1000, ]
    dat1 <- dat1[dat1$yft + dat1$bet + dat1$alb > 0, ]
    if (doHBF)
        dat1 <- dat1[dat1$hbf <= 25, ]
    lenzero <- function(x) sum(x > 0, na.rm = TRUE)
    ssp <- apply(dat1[, splist], 1, lenzero)
    dat1 <- dat1[ssp > 1, ]
    remvec <- c("G")
    if (length(grep(remvec, dat1$rem)) > 0) dat1 <- dat1[-grep(remvec, dat1$rem), ]
    return(dat1)
}

#' Clean TW data.
#'
#' The function sets up variables and removes bad data. Originally developed for Taiwanese data in the IO.
#' @param dat1 Input dataset
#' @param rmssp If TRUE, remove sets that report catch of only one species.
#' @param splist List of species codes.
#' @return Modified dataset.
#'
dataclean_TW <- function(dat1, rmssp = F, splist = c("alb", "bet", "yft", "ott", "swo", "mls", "bum", "blm", "otb", "skj", "sha", "oth", "sbt")) {
  dat1 <- dat1[!is.na(dat1$dmy),]
  dat1 <- dat1[!is.na(dat1$hooks), ]  #
  dat1 <- dat1[dat1$hooks < 5000, ]  # clean up outliers
  dat1 <- dat1[dat1$hooks > 200, ]
  dat1[dat1$hbf %in% c(35, 155, 20000), "hbf"] <- 15
  dat1[dat1$hbf %in% c(26, 30), "hbf"] <- 20
  dat1[dat1$hbf %in% c(25), "hbf"] <- 24
  dat1 <- dat1[dat1$hbf < 25,]
  lenzero <- function(x) sum(x > 0)
  dat1$nsp <- apply(dat1[, splist], 1, lenzero)
  if (rmssp) {
    ssp <- apply(dat1[, splist], 1, lenzero)
    dat1 <- dat1[ssp > 1, ]
  }
  dat1 <- dat1[!is.na(dat1$yrqtr),]
  # remove sets where 1 degree location is not in the op_area
  a <- dat1$lon - dat1$lon5
  loc <- !a %in% c(seq(-402.5, -3.5, 1), seq(2.5, 40.5, 1))
  dat1 <- dat1[loc,]
  a <- dat1$lat - dat1$lat5
  loc <- !a %in% c(seq(-402.5, -3.5, 1), seq(2.5, 40.5, 1))
  dat1 <- dat1[loc,]
  return(dat1)
}

#' Clean TW data.
#'
#' The function sets up variables and removes bad data. Originally developed for Taiwanese data in the IO.
#' @param dat1 Input dataset
#' @param rmssp If TRUE, remove sets that report catch of only one species.
#' @param splist List of species codes.
#' @return Modified dataset.
#'
dataclean_TW_EPO <- function(dat1, rmssp = F, splist = c("alb", "bet","yft", "ott", "swo", "mls", "bum", "blm", "otb", "skj", "sha", "oth")) {
  dat1 <- dat1[!is.na(dat1$dmy),]
  dat1 <- dat1[!is.na(dat1$hooks), ]  #
#  dat1 <- dat1[dat1$hooks < 5000, ]  # clean up outliers
#  dat1 <- dat1[dat1$hooks > 200, ]
  dat1[dat1$hbf %in% c(127), "hbf"] <- 17
  lenzero <- function(x) sum(x > 0)
  if (rmssp) {
    ssp <- apply(dat1[, splist], 1, lenzero)
    dat1 <- dat1[ssp > 1, ]
  }
  dat1 <- dat1[!is.na(dat1$yrqtr),]
  # remove sets where 1 degree location is not in the op_area
  a <- dat1$lon - dat1$lon5
  loc <- !a %in% c(seq(-402.5, -3.5, 1), seq(2.5, 40.5, 1))
  dat1 <- dat1[loc,]
  a <- dat1$lat - dat1$lat5
  loc <- !a %in% c(seq(-402.5, -3.5, 1), seq(2.5, 40.5, 1))
  dat1 <- dat1[loc,]
  return(dat1)
}

#' Clean SY data.
#'
#' The function sets up variables and removes bad data. Originally developed for Taiwanese data in the IO.
#' @param dat Input dataset
#' @param rmssp If TRUE, remove sets that report catch of only one species.
#' @param splist List of species codes.
#' @return Modified dataset.
#'
dataclean_SY <- function(dat, rmssp, splist) {

  # Dates
  dat$dmy <- as.Date(paste(dat$op_yr, dat$op_mon, dat$op_day, sep = "-")) # Generate dmy field
  dat <- dat[!is.na(dat$dmy),]    # Remove operations without fishing date
  # species
  lenzero <- function(x) sum(x > 0)
  if (rmssp) {
    ssp <- apply(dat[, splist], 1, lenzero)
    dat <- dat[ssp > 1, ]
  }

  for (sp in splist) {
    dat[,sp] <- as.numeric(dat[,sp])
    if (sum(is.na(dat[,sp])) > 0) dat[is.na(dat[,sp]), sp] <- 0
  }
  ### Replace NA by 0 values
  indic.column.species <- which(names(dat) %in% splist)
  dat[,indic.column.species][is.na(dat[,indic.column.species])] <- 0

  # hooks
  dat <- dat[!is.na(dat$hooks), ] # Remove operations without information on effort
  dat <- dat[dat$hooks < 5000, ]  # Remove operations with number of hooks >5000
  dat <- dat[dat$hooks > 200, ]   # Remove operations with nunmber of hooks <200
  # geographic positions
  dat$lat <- as.numeric(dat$lat)   # Convert lat to numeric values
  dat <- dat[!is.na(dat$lat),]    # Remove operations without latitude
  dat$lon <- as.numeric(dat$lon)   # Convert long to numeric values
  dat <- dat[!is.na(dat$lon),]    # Remove operations without longitude

  # hbf (not available in current data set)
  #dat <- dat[is.na(dat$hbf) == F, ]
  #dat <- dat[dat$hbf >= 5, ]
  return(dat)
}

#' Clean US data.
#'
#' The function sets up variables and removes bad data.
#' @param dat Input dataset
#' @param splist List of species codes
#' @return Modified dataset.
#'
dataclean_USAO <- function(dat, splist = c("bft","alb","bet","yft","swo","mls","bum", "bsh", "sma", "por"))
  {
    dat$op_yr <- as.numeric(dat$op_yr)
    dat$op_mon <- as.numeric(dat$op_mon)
    dat$op_day <- as.numeric(dat$op_day)
    dat$lat <- as.numeric(dat$lat)
    dat <- dat[!is.na(dat$lat), ]
    dat <- dat[!is.na(dat$lon), ]
    dat$lon <- as.numeric(dat$lon)
    dat$hbf <- as.numeric(dat$hbf)
    dat$hooks <- as.numeric(dat$hooks)
    for (sp in splist) {
      dat[,sp] <- as.numeric(dat[,sp])
      if (sum(is.na(dat[,sp])) > 0) dat[is.na(dat[,sp]), sp] <- 0
    }
    dim(dat)
    # dat <- dat[dat$hooks < 5000, ]
    #  dat <- dat[dat$hooks > 200, ]
    #  dat <- dat[(!is.na(dat$hbf) & dat$hbf < 26) | (dat$op_yr <
    #     1976 & is.na(dat$hbf)), ]
    return(dat)
  }

#' Clean Brazilian data.
#'
#' The function sets up variables and removes bad data.
#' @param dat Input dataset
#' @param yearlim Data is included only from before this year.
#' @param splist List of species codes
#' @return Modified dataset.
#'
dataclean_BR <- function (dat, yearlim = 2018, splist) {
  for (sp in splist) {
    dat[, sp] <- as.numeric(dat[, sp])
    if (sum(is.na(dat[, sp])) > 0)
      dat[is.na(dat[, sp]), sp] <- 0
  }
  data <- dat[dat$type == 1, ]
  dat <- dat[!is.na(dat$hooks), ]
  dat <- dat[dat$hooks < 5000, ]
  dat <- dat[dat$hooks >= 500, ]
  dat <- dat[is.na(dat$hbf) == FALSE, ]
  dat <- dat[dat$op_yr > 1976, ]
  dat <- dat[dat$yrqtr < yearlim, ]
  dat <- dat[dat$hbf >= 5, ]
  return(dat)
}
