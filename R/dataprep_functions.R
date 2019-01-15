
#' Prepare Korean SBT data.
#'
#' The function prepares the Korean SBT data.
#' @param dat Input dataset
#' @param splist List of species codes
#' @return Modified dataset.
#'
dataprep_KRSBT <- function(dat, splist) {
 dat$dmy <- as.Date(dat$DATE)
 dat$hbf <- round(dat$hooks/dat$floats, 0)
 dat$lat <- dat$Lat01
 dat$lon <- dat$Long01
 dat$lat[dat$NS == 2] <- (dat$Lat01[dat$NS == 2] + 1) * -1
 dat$lon[dat$EW == 2] <- 360 - (dat$Long01[dat$EW == 2] + 1)
 dat <- dat[dat$lon >= 0, ]
 dat <- dat[dat$lat < 29, ]

 dat$lat5 <- 5 * floor(dat$lat/5) + 2.5
 dat$lon5 <- 5 * floor(dat$lon/5) + 2.5

 # regSBT preliminary boudnaries - update later
 dat$regSBT <- 0
 dat[dat$lat < -30 & dat$lon < 60, ]$regSBT <- 1
 dat[dat$lat < -30 & dat$lon >= 60, ]$regSBT <- 2

 dat$yrqtr <- dat$op_yr + floor((dat$op_mon - 1)/3)/4 + 0.125
 dat$latlong <- paste(dat$lat5, dat$lon5, sep = "_")
 dat$vessid <- as.factor(as.numeric(dat$VESSEL_NAME_rev))
 dat$tripidmon <- paste(dat$vessid, dat$op_yr, dat$op_mon)
 return(dat)
}

#' Prepare Korean longline data.
#'
#' The function prepares Korean longline data for IO analyses.
#' @param dat Input dataset
#' @param splist List of species codes
#' @return Modified dataset.
#'
dataprep_KR <- function(dat, splist) {
 dat$dmy <- as.Date(dat$DATE)
 dat$hbf <- round(dat$hooks/dat$floats, 0)
 dat$moon <- lunar.illumination(dat$dmy)

 dat$lat <- dat$Lat01
 dat$lon <- dat$Long01
 dat$lat[dat$NS == 2] <- (dat$Lat01[dat$NS == 2] + 1) * -1
 dat$lon[dat$EW == 2] <- 360 - (dat$Long01[dat$EW == 2] + 1)
 dat <- dat[dat$lon >= 0, ]
 dat$lon[dat$lon >= 180] <- dat$lon[dat$lon >= 180] - 360
 dat <- dat[dat$lat < 29, ]

 dat$lat5 <- 5 * floor(dat$lat/5) + 2.5
 dat$lon5 <- 5 * floor(dat$lon/5) + 2.5

 dat$yrqtr <- dat$op_yr + floor((dat$op_mon - 1)/3)/4 + 0.125
 dat$latlong <- paste(dat$lat5, dat$lon5, sep = "_")
# dat$vessid <- as.factor(as.numeric(dat$VESSEL_NAME))
 dat$vessid <- as.factor(as.numeric(as.factor(dat$VESSEL_NAME)))
 # dat$vessid <- as.factor(as.numeric(dat$VESSEL_CD))
 dat$tripidmon <- paste(dat$vessid, dat$op_yr, dat$op_mon)
 dat$Totalx <- apply(dat[,splist], 1, sum, na.rm = TRUE)
 dat$Total2 <- apply(dat[, c("bet", "yft", "alb")], 1, sum, na.rm = TRUE)
 return(dat)
}

#' Prepare Japanese longline data for the Indian Ocean.
#'
#' The function prepares Japanese longline data for IO analyses.
#' @param dat Input dataset
#' @param alldat If FALSE, removes vessels without vessel ID.
#' @return Modified dataset.
#'
dataprep_JPIO <- function(dat, alldat = T) {
  dat <- dat[order(dat$op_yr, dat$op_mon, dat$op_day), ]
  dat$dmy <- ymd(paste(dat$op_yr, dat$op_mon, dat$op_day, sep = " - "))
  dat$moon <- lunar.illumination(dat$dmy)

  dat$lat_raw <- dat$lat
  dat$lon_raw <- dat$lon
  dat$lat[dat$latcode == 2] <- (dat$lat_raw[dat$latcode == 2] + 1) * -1
  dat$lon[dat$loncode == 2] <- 360 - (dat$lon_raw[dat$loncode == 2] + 1)
  dat <- dat[dat$lon >= 0, ]

  dat$lat5 <- 5 * floor(dat$lat/5) + 2.5
  dat$lon5 <- 5 * floor(dat$lon/5) + 2.5


  dat$callsign[is.na(dat$callsign)] <- ". "
  dat$callsign[dat$callsign == " "] <- ". "
  dat$vessid <- as.numeric(as.factor(paste(dat$callsign)))
  if (alldat == F) { dat <- dat[dat$vessid != 1, ] }
  dat$vessid <- as.numeric(as.factor(dat$vessid))
  dat$tripidmon <- as.factor(paste(dat$vessid, dat$op_yr, dat$op_mon))

  dat$yrqtr <- dat$op_yr + floor((dat$op_mon - 1)/3)/4 + 0.125
  dat$latlong <- paste(dat$lat5, dat$lon5, sep = "_")
  dat$Total <- with(dat, alb + bet + yft + sbt + swo + mls + bum + blm)
  dat$Total2 <- apply(dat[, c("bet", "yft", "alb")], 1, sum)

  dat$trip_yr <- as.numeric(substr(as.character(dat$trip_st), 1, 4))
  # dat <- dat[dat$trip_yr > 1945 | is.na(dat$trip_yr), ]

  dat$tripid <- paste(dat$vessid, dat$trip_st, sep = "_")
  dat$tripid[dat$vessid == 1] <- NA
  dat$tripid[dat$trip_st == 0] <- NA

  dat <- make_lbidmon(dat) # This sets up the clustering variable.

  dat$hbf[dat$op_yr < 1976 & is.na(dat$hbf)] <- 5
  # a <- table(dat$tripid) dat$sets_per_trip <- NA dat$sets_per_trip <- a[match(dat$tripid, names(a))] noms <- c('vessid', 'yrqtr', 'latlong',
  # 'op_yr', 'op_mon', 'hbf', 'hooks', 'tripid', 'tripidmon', 'trip_yr', 'moon', 'alb', 'bet', 'yft', 'swo', 'mls', 'bum', 'blm', 'sbt', 'Total',
  # 'Total2', 'dmy', 'lat', 'lon', 'lat5', 'lon5', 'regY', 'regY1', 'regB', 'regB1', 'regA', 'regA1', 'regA2', 'regA3') dat <- dat[, noms]

  return(dat)
}

#' Prepare Japanese longline data.
#'
#' The function prepares Japanese longline data for IO and AO analyses.
#' @param dat Input dataset
#' @param alldat If FALSE, removes vessels without vessel ID.
#' @param region Set the region to AO or IO.
#' @param splist List of species codes
#' @return Modified dataset.
#'
dataprep_JP <- function(dat, alldat = T, region = "IO", splist = c("bft","sbt","alb","bet","yft","swo","mls","bum","blm","sas","shk")) {
  dat <- dat[order(dat$op_yr, dat$op_mon, dat$op_day), ]
  dat$dmy <- ymd(paste(dat$op_yr, dat$op_mon, dat$op_day, sep = " - "))
  dat$moon <- lunar.illumination(dat$dmy)

  dat$lat_raw <- dat$lat
  dat$lon_raw <- dat$lon

  if (region == "IO") {
    dat$lat[dat$latcode == 2] <- (dat$lat_raw[dat$latcode == 2] + 1) * -1
    dat$lon[dat$loncode == 2] <- 360 - (dat$lon_raw[dat$loncode == 2] + 1)
    dat <- dat[dat$lon >= 0, ]
  }
  if (region == "AO") {
    dat$lat[dat$latcode == 2] <- (dat$lat_raw[dat$latcode == 2] + 1) * -1
    dat$lon[dat$loncode == 2] <- (dat$lon_raw[dat$loncode == 2] + 1) * -1
  }

  dat$lat5 <- 5 * floor(dat$lat/5) + 2.5
  dat$lon5 <- 5 * floor(dat$lon/5) + 2.5


  dat$callsign[is.na(dat$callsign)] <- ". "
  dat$callsign[dat$callsign == " "] <- ". "
  dat$vessid <- as.numeric(as.factor(paste(dat$callsign)))
  if (alldat == F) { dat <- dat[dat$vessid != 1, ] }
  dat$vessid <- as.numeric(as.factor(dat$vessid))
  dat$tripidmon <- as.factor(paste(dat$vessid, dat$op_yr, dat$op_mon))

  dat$yrqtr <- dat$op_yr + floor((dat$op_mon - 1)/3)/4 + 0.125
  dat$latlong <- paste(dat$lat5, dat$lon5, sep = "_")
  dat$Total <- apply(dat[, splist], 1, sum)
  dat$Total2 <- apply(dat[, c("bet", "yft", "alb")], 1, sum)

  dat$trip_yr <- as.numeric(substr(as.character(dat$trip_st), 1, 4))
  # dat <- dat[dat$trip_yr > 1945 | is.na(dat$trip_yr), ]

  dat$tripid <- paste(dat$vessid, dat$trip_st, sep = "_")
  dat$tripid[dat$vessid == 1] <- NA
  dat$tripid[dat$trip_st == 0] <- NA

  dat <- make_lbidmon(dat) # This sets up the clustering variable.

    dat$hbf[dat$op_yr < 1976 & is.na(dat$hbf)] <- 5
  return(dat)
}

#' Prepare Japanese longline data.
#'
#' The function prepares Japanese longline data for WCPO analyses.
#' @param dat Input dataset
#' @param alldat If FALSE, removes vessels without vessel ID.
#' @return Modified dataset.
#'
dataprep <- function(dat, alldat = F) {
 dat$lat_raw <- dat$lat
 dat$lon_raw <- dat$lon
 dat$lat[dat$latcode == 2] <- (dat$lat_raw[dat$latcode == 2] + 1) * -1
 dat$lon[dat$loncode == 2] <- 360 - (dat$lon_raw[dat$loncode == 2] + 1)
 dat <- dat[dat$lon >= 0, ]

 dat$lat5 <- 5 * floor(dat$lat/5)
 dat$lon5 <- 5 * floor(dat$lon/5)

 dat$reg <- 0
 dat[dat$lat < 40 & dat$lat >= 20 & dat$lon >= 110 & dat$lon < 170, ]$reg <- 1
 dat[dat$lat < 40 & dat$lat >= 20 & dat$lon >= 170 & dat$lon < 210, ]$reg <- 2
 dat[dat$lat < 20 & dat$lat >= -10 & dat$lon >= 110 & dat$lon < 170, ]$reg <- 3
 dat[dat$lat < 20 & dat$lat >= -10 & dat$lon >= 170 & dat$lon < 210, ]$reg <- 4
 dat[dat$lat < -10 & dat$lat >= -40 & dat$lon >= 140 & dat$lon < 170, ]$reg <- 5
 dat[dat$lat < -10 & dat$lat >= -40 & dat$lon >= 170 & dat$lon < 210, ]$reg <- 6
 dat[dat$lat < 40 & dat$lat >= 20 & dat$lon >= 210, ]$reg <- 7
 dat[dat$lat < 20 & dat$lat >= -40 & dat$lon >= 210, ]$reg <- 8

 dat$subreg <- 0
 dat[dat$lat < 40 & dat$lat >= 20 & dat$lon >= 110 & dat$lon < 170, ]$subreg <- 1
 dat[dat$lat < 40 & dat$lat >= 20 & dat$lon >= 170 & dat$lon < 210, ]$subreg <- 2
 dat[dat$lat < 20 & dat$lat >= 0 & dat$lon >= 110 & dat$lon < 150, ]$subreg <- 3.1
 dat[dat$lat < 20 & dat$lat >= 0 & dat$lon >= 150 & dat$lon < 170, ]$subreg <- 3.2
 dat[dat$lat < 0 & dat$lat >= -10 & dat$lon >= 110 & dat$lon < 150, ]$subreg <- 3.3
 dat[dat$lat < 0 & dat$lat >= -10 & dat$lon >= 150 & dat$lon < 170, ]$subreg <- 3.4
 dat[dat$lat < 20 & dat$lat >= -10 & dat$lon >= 170 & dat$lon < 180, ]$subreg <- 4.1
 dat[dat$lat < 20 & dat$lat >= -10 & dat$lon >= 180 & dat$lon < 210, ]$subreg <- 4.2
 dat[dat$lat < -10 & dat$lat >= -40 & dat$lon >= 140 & dat$lon < 170, ]$subreg <- 5
 dat[dat$lat < -10 & dat$lat >= -40 & dat$lon >= 170 & dat$lon < 210, ]$subreg <- 6
 dat[dat$lat < 40 & dat$lat >= 20 & dat$lon >= 210, ]$subreg <- 7
 dat[dat$lat < 20 & dat$lat >= -40 & dat$lon >= 210, ]$subreg <- 8


 dat$callsign[dat$callsign == " "] <- ". "
 dat$vessid <- as.numeric(as.factor(paste(dat$callsign)))
 if (alldat == F) { dat <- dat[dat$vessid != 1, ] }
 dat$vessid <- as.numeric(as.factor(dat$vessid))

 dat$yrqtr <- dat$op_yr + floor((dat$op_mon)/3)/4 + 0.125
 dat$latlong <- paste(dat$lat5, dat$lon5, sep = ".")
 dat <- dat[dat$yrqtr < 2012, ]
 # dat <- dat[dat$reg %in% 1:6, ]

 dat$newfishingcat <- NA
 dat <- dat[dat$fishingcat <= 3, ]
 dat <- dat[dat$op_yr < 1967 | dat$op_yr > 1970 | dat$fishingcat < 3, ]
 dat <- dat[dat$op_yr <= 1957 | dat$fishingcat != ".", ]
 dat[dat$op_yr <= 1957 & dat$reg %in% c(1), ]$newfishingcat <- 1
 dat[dat$op_yr <= 1957 & dat$reg %in% c(0, 2:8), ]$newfishingcat <- 2
 dat[dat$op_yr > 1957 & dat$op_yr <= 1993 & dat$fishingcat == 1, ]$newfishingcat <- 1
 dat[dat$op_yr > 1993 & dat$fishingcat == 3, ]$newfishingcat <- 1
 dat[dat$op_yr > 1957 & dat$op_yr <= 1966 & dat$fishingcat %in% c(2, 3), ]$newfishingcat <- 2
 dat[dat$op_yr > 1966 & dat$op_yr <= 1970 & dat$fishingcat %in% c(2), ]$newfishingcat <- 2
 dat[dat$op_yr >= 1971 & dat$op_yr <= 1993 & dat$fishingcat %in% c(2, 3), ]$newfishingcat <- 2
 dat[dat$op_yr > 1993 & dat$fishingcat %in% c(1, 2), ]$newfishingcat <- 2

 dat <- dat[dat$yrqtr > 1945, ]

 dat$trip_yr <- as.numeric(substr(as.character(dat$trip_st), 1, 4))
 dat <- dat[dat$trip_yr > 1945 | is.na(dat$trip_yr), ]

 dat$tripid <- paste(dat$vessid, dat$trip_st, sep = "_")
 dat$tripid[dat$vessid == 1] <- NA
 dat$tripid[dat$trip_st == " 0"] <- NA

 a <- table(dat$tripid)
 dat$sets_per_trip <- NA
 dat$sets_per_trip <- a[match(dat$tripid, names(a))]
 return(dat)
}

#' Prepare Taiwanese longline data.
#'
#' The function prepares Taiwanese longline data for IO analyses.
#' @param dat Input dataset
#' @param alldat Not used.
#' @param region IO or AO.
#' @param splist Define the species in the dataset
#' @return Modified dataset.
#'
dataprep_TW <- function(dat, alldat = F, region = "IO", splist = c("alb", "bet","yft", "ott", "swo", "mls", "bum", "blm", "otb", "skj", "sha", "oth", "pbf", "sbt")) {
  splist_w <- paste0(splist, "_w")
  dat$dmy <- ymd(paste(dat$op_yr, dat$op_mon, dat$op_day, sep = " - "))
  # dat <- dat[!is.na(dat$dmy),]
  # makedmy <- function(yy, mm, dd) {
  #   if(sum(!is.na(yy)) > 0) {
  #     a = paste(yy, mm, dd, sep = " - ")
  #     a1 <- gsub(" ", "", a)
  #     a2 <- ymd(a1)
  #   } else a2 <- yy
  #   return(a2)
  # }
  makedmy <- function(yy, mm, dd) {
    tmp <- data.frame(yy=yy,mm=mm,dd=dd)
    loc <- !(is.na(tmp$yy) | is.na(tmp$mm) | is.na(tmp$dd) | (tmp$yy==0) | (tmp$dd==0) )
    tm2 <- tmp[loc,]
    tm2$a <- paste(tm2$yy, tm2$mm, tm2$dd, sep = " - ")
    tm2$a1 <- gsub(" ", "", tm2$a)
    tm2$a2 <- ymd(tm2$a1)
    tmp$a2 <- NA
    tmp[loc,]$a2 <- tm2$a2
    return(tmp$a2)
  }
  dat$embark_dmy <- makedmy(dat$embark_yr, dat$embark_mn, dat$embark_dd)
  dat$debark_dmy <- makedmy(dat$debark_yr, dat$debark_mn, dat$debark_dd)
  dat$op_start_dmy <- makedmy(dat$op_start_yr, dat$op_start_mn, dat$op_start_dd)
  dat$op_end_dmy <- makedmy(dat$op_end_yr, dat$op_end_mn, dat$op_end_dd)

  # hist(dat$op_start_dmy, breaks = 'months')
  dat$lat <- dat$op_lat
  dat$lon <- dat$op_lon

  dat$tonnage <- as.factor(substring(dat$callsign, 1, 1))
  a <- levels(dat$tonnage)
  levs <- cbind(c("0", "1", "2", "3", "4", "5", "6", "7", "8"), c(" < 5", "5 -10", "10 -20", "20 -50", "50 -100", "100 -200", "200 -500", "500 -1000",
                                                                  " >= 1000"))
  levels(dat$tonnage) <- levs[match(a, levs[, 1]), 2]
  # table(dat$tonnage, substring(dat$callsign, 1, 1)) table(dat$NS, useNA = 'always') table(dat$EW, useNA = 'always')

  if(region == "IO") {
    dat$lat[is.na(dat$NS) == F & dat$NS %in% c(3, 7)] <- (dat$lat[is.na(dat$NS) == F & dat$NS %in% c(3, 7)] + 1) * -1
    dat$lon[is.na(dat$EW) == F & dat$EW == 2] <- 360 - (dat$lon[is.na(dat$EW) == F & dat$EW == 2] + 1)
    dat <- dat[is.na(dat$lon) | dat$lon >= 10, ]
    dat <- dat[is.na(dat$lon) | dat$lon < 130, ]
    dat <- dat[is.na(dat$lat) | dat$lat < 29, ]
  }
  if(region == "AO") {
    dat$lat[is.na(dat$NS) == F & dat$NS %in% c(3, 7)] <- (dat$lat[is.na(dat$NS) == F & dat$NS %in% c(3, 7)] + 1) * -1
    dat$lon[is.na(dat$EW) == F & dat$EW == 2] <- 360 - (dat$lon[is.na(dat$EW) == F & dat$EW == 2] + 1)
    dat$lon[!is.na(dat$lon) & dat$lon >= 180] <- dat$lon[!is.na(dat$lon) & dat$lon >= 180] - 360
  }
  la <- as.integer(substring(dat$op_area, 1, 2))
  lo <- as.integer(substring(dat$op_area, 3, 4))
  dat$lat5 <- 5 * (la - 73)/2 + 2.5
  dat$lat5[la%%2 == 0] <- -(5 * (la[la%%2 == 0] - 74))/2 - 2.5 #

  dat$lon5 <- -(5 * (lo - 1)/2 + 2.5)
  dat$lon5[lo%%2 == 0] <- 5 * (lo[lo%%2 == 0] - 2)/2 + 2.5
  # Indian ocean regions for YFT and BET?

  dat$bt1 <- !(is.na(dat$bait1) | dat$bait1 == 0)
  dat$bt2 <- !(is.na(dat$bait2) | dat$bait2 == 0)
  dat$bt3 <- !(is.na(dat$bait3) | dat$bait3 == 0)
  dat$bt4 <- !(is.na(dat$bait4) | dat$bait4 == 0)
  dat$bt5 <- !(is.na(dat$bait5) | dat$bait5 == 0)

  dat$vessid <- as.factor(as.numeric(as.factor(dat$callsign)))
  dat$tripid <- as.factor(paste(dat$vessid, dat$op_start_dmy))
  dat$tripidmon <- as.factor(paste(dat$vessid, dat$op_yr, dat$op_mon))
  dat$moon <- lunar.illumination(dat$dmy)

  dat$yrqtr <- dat$op_yr + floor((dat$op_mon - 1)/3)/4 + 0.125
  dat$latlong <- paste(dat$lat5, dat$lon5, sep = "_")
  if(region == "IO") {
    dat$sbt <- dat$sbf + dat$pbf
    dat$sbt_w <- dat$sbf_w + dat$pbf_w
    splist <- splist[splist != "pbf"]
    splist_w <- splist_w[splist_w != "pbf_w"]
  }
  dat$Total <- apply(dat[,splist], 1, sum)
  dat$Total2 <- apply(dat[, c("bet", "yft", "alb")], 1, sum)
  noms <- c("vessid", "callsign", "yrqtr", "latlong", "op_yr", "op_mon", "hbf", "hooks", "tonnage", "tripid", "tripidmon", "moon", splist, "Total", "Total2", splist_w, "sst", "bt1", "bt2", "bt3", "bt4", "bt5", "hookdp", "target", "rem", "dmy", "embark_dmy", "debark_dmy", "op_start_dmy", "op_end_dmy", "lat", "lon", "lat5", "lon5", "oilv", "foc")
  dat <- dat[, noms]
  return(dat)
}

#' Prepare Seychelles longline data.
#'
#' The function prepares Seychelles longline data for IO analyses.
#' @param dat Input dataset
#' @param region IO or AO.
#' @param splist Define the species in the dataset
#' @return Modified dataset.
#'

dataprep_SY <- function(dat, region, splist) {

  # species
  dat$Total <- apply(dat[,splist], 1, sum)
  dat$Total2 <- apply(dat[, c("bet", "yft", "alb")], 1, sum)

  # New fields
  dat$yrqtr <- dat$op_yr + floor((dat$op_mon - 1)/3)/4 + 0.125
  dat$tripidmon <- as.factor(paste(dat$vessid, dat$op_yr, dat$op_mon))  # id trip-month
  dat$moon <- lunar.illumination(dat$dmy)                               #

  # geographic positions
  dat$lat5 <- 5 * floor(dat$lat/5) + 2.5
  dat$lon5 <- 5 * floor(dat$lon/5) + 2.5
  dat$latlong <- paste(dat$lat5, dat$lon5, sep = "_")
  return(dat)
}

#' Prepare the variable lbid_mon.
#'
#' The function creates the variable lbid_mon which is assigned to Japanese data for clustering before vessel ids are available.
#' @param dat Input dataset
#' @return Modified dataset.
#'
make_lbidmon <- function(dat) {
 dat$lbid_mon <- paste(dat$logbookid, dat$op_mon)
 return(dat)
}

#' Set up Indian Ocean regions
#'
#' The function sets up the Indian Ocean regions for datasets with lat5 and lon5 variables.
#' @param dat Input dataset
#' @param regY If TRUE, set up regY
#' @param regY1 If TRUE, set up regY1
#' @param regY2 If TRUE, set up regY2
#' @param regB If TRUE, set up regB
#' @param regB1 If TRUE, set up regB1
#' @param regB2 If TRUE, set up regB2
#' @param regB3 If TRUE, set up regB3
#' @param regA If TRUE, set up regA
#' @param regA1 If TRUE, set up regA1
#' @param regA2 If TRUE, set up regA2
#' @param regA3 If TRUE, set up regA3
#' @param regA4 If TRUE, set up regA4
#' @param regA5 If TRUE, set up regA5
#' @return Modified dataset.
#'
setup_IO_regions <- function(dat, regY = F, regY1 = F, regY2 = F, regB = F, regB1 = F, regB2 = F, regB3 = F, regA = F, regA1 = F, regA2 = F, regA3 = F,
                             regA4 = F, regA5 = F) {
  lat5 <- lon5 <- NULL
  if (regY) {
    dat$regY <- 0
    dat <-
      mutate(dat,
             regY = replace(regY,which(lat5 >=  10 & lon5 < 80 & !is.na(lat5)),1)) %>%
      mutate(regY = replace(regY,which(lat5 <  10 & lat5 >=    0 & lon5 >= 40 & lon5 < 75 & !is.na(lat5)),2))  %>%
      mutate(regY = replace(regY,which(lat5 <   0 & lat5 >=  -10 & lon5 >= 35 & lon5 < 75 & !is.na(lat5)),2))  %>%
      mutate(regY = replace(regY,which(lat5 <  -10 & lat5 >=  -15 & lon5 >= 60 & lon5 < 75 & !is.na(lat5)),2)) %>%
      mutate(regY = replace(regY,which(lat5 <  -10 & lat5 >= -15 & lon5 >= 40 & lon5 < 60 & !is.na(lat5)),3))  %>%
      mutate(regY = replace(regY,which(lat5 <  -15 & lat5 >= -25 & lon5 >= 35 & lon5 < 60 & !is.na(lat5)),3))  %>%
      mutate(regY = replace(regY,which(lat5 <  -25 & lat5 >= -30 & lon5 >= 30 & lon5 < 60 & !is.na(lat5)),3))  %>%
      mutate(regY = replace(regY,which(lat5 <  -30 & lat5 >= -40 & lon5 >= 20 & lon5 < 40 & !is.na(lat5)),3))  %>%
      mutate(regY = replace(regY,which(lat5 <  -15 & lat5 >= -20 & lon5 >= 60 & lon5 < 125 & !is.na(lat5)),4)) %>%
      mutate(regY = replace(regY,which(lat5 <  -20 & lat5 >= -40 & lon5 >= 60 & lon5 < 120 & !is.na(lat5)),4)) %>%
      mutate(regY = replace(regY,which(lat5 <  -30 & lat5 >= -40 & lon5 >= 40 & lon5 < 60 & !is.na(lat5)),4))  %>%
      mutate(regY = replace(regY,which(lat5 < 10 & lat5 >= -15 & lon5 >= 75 & lon5 < 100 & !is.na(lat5)),5))   %>%
      mutate(regY = replace(regY,which(lat5 < -5 & lat5 >= -15 & lon5 >= 100 & lon5 < 110 & !is.na(lat5)),5))  %>%
      mutate(regY = replace(regY,which(lat5 < -10 & lat5 >= -15 & lon5 >= 110 & lon5 < 130 & !is.na(lat5)),5)) %>%
      mutate(regY = replace(regY,which(lat5 < 30 & lat5 >= 10 & lon5 >= 80 & lon5 < 100 & !is.na(lat5)),6))
  }

  if (regY1) {
    dat$regY1 <- 0
    dat <-
      mutate(dat, regY1 = replace(regY1, which(regY %in% c(1, 2)), 1)) %>%
      mutate(regY1 = replace(regY1, which(regY %in% c(3)), 2)) %>%
      mutate(regY1 = replace(regY1, which(regY %in% c(4)), 3)) %>%
      mutate(regY1 = replace(regY1, which(regY %in% c(5, 6)), 4))
  }

  if(regY2) {
    dat$regY2 <- 0
    dat <- mutate(dat,
             regY2 = replace(regY2,which(lat5 >=  10 & lon5 < 80 & !is.na(lat5)),1)) %>%
      mutate(regY2 = replace(regY2,which(lat5 <  10 & lat5 >=    0 & lon5 >= 40 & lon5 < 75 & !is.na(lat5)),7))  %>%
      mutate(regY2 = replace(regY2,which(lat5 <   0 & lat5 >=  -10 & lon5 >= 35 & lon5 < 75 & !is.na(lat5)),2))  %>%
      mutate(regY2 = replace(regY2,which(lat5 <  -10 & lat5 >=  -15 & lon5 >= 60 & lon5 < 75 & !is.na(lat5)),2)) %>%
      mutate(regY2 = replace(regY2,which(lat5 <  -10 & lat5 >= -15 & lon5 >= 40 & lon5 < 60 & !is.na(lat5)),3))  %>%
      mutate(regY2 = replace(regY2,which(lat5 <  -15 & lat5 >= -25 & lon5 >= 35 & lon5 < 60 & !is.na(lat5)),3))  %>%
      mutate(regY2 = replace(regY2,which(lat5 <  -25 & lat5 >= -30 & lon5 >= 30 & lon5 < 60 & !is.na(lat5)),3))  %>%
      mutate(regY2 = replace(regY2,which(lat5 <  -30 & lat5 >= -40 & lon5 >= 20 & lon5 < 40 & !is.na(lat5)),3))  %>%
      mutate(regY2 = replace(regY2,which(lat5 <  -15 & lat5 >= -20 & lon5 >= 60 & lon5 < 125 & !is.na(lat5)),4)) %>%
      mutate(regY2 = replace(regY2,which(lat5 <  -20 & lat5 >= -40 & lon5 >= 60 & lon5 < 120 & !is.na(lat5)),4)) %>%
      mutate(regY2 = replace(regY2,which(lat5 <  -30 & lat5 >= -40 & lon5 >= 40 & lon5 < 60 & !is.na(lat5)),4))  %>%
      mutate(regY2 = replace(regY2,which(lat5 < 10 & lat5 >= -15 & lon5 >= 75 & lon5 < 100 & !is.na(lat5)),5))   %>%
      mutate(regY2 = replace(regY2,which(lat5 < -5 & lat5 >= -15 & lon5 >= 100 & lon5 < 110 & !is.na(lat5)),5))  %>%
      mutate(regY2 = replace(regY2,which(lat5 < -10 & lat5 >= -15 & lon5 >= 110 & lon5 < 130 & !is.na(lat5)),5)) %>%
      mutate(regY2 = replace(regY2,which(lat5 < 30 & lat5 >= 10 & lon5 >= 80 & lon5 < 100 & !is.na(lat5)),6))
  }

  # regB North of 15S and west of 80 is R1, or north of 20 and west of 45; north of 15S and east of 80 is R2; north of 35S is R3
  if (regB) {
    dat$regB <- 0
    dat <- mutate(dat,
                  regB = replace(regB, which(lat5 < 10 & lat5 >= 0 & lon5 >= 40 & lon5 < 80 & !is.na(lat5)), 1)) %>%
      mutate(regB = replace(regB, which(lat5 < 0   & lat5 >= -10 & lon5 >= 35 & lon5 < 80 & !is.na(lat5)), 1)) %>%
      mutate(regB = replace(regB, which(lat5 < -10 & lat5 >= -15 & lon5 >= 40 & lon5 < 80 & !is.na(lat5)), 1)) %>%
      mutate(regB = replace(regB, which(lat5 < -15 & lat5 >= -20 & lon5 >= 35 & lon5 < 46 & !is.na(lat5)), 1)) %>%
      mutate(regB = replace(regB, which(lat5 < 10 & lat5 >= -15 & lon5 >= 80 & lon5 < 100 & !is.na(lat5)), 2)) %>%
      mutate(regB = replace(regB, which(lat5 < -3 & lat5 >= -15 & lon5 >= 100 & lon5 < 110 & !is.na(lat5)), 2)) %>%
      mutate(regB = replace(regB, which(lat5 < -8 & lat5 >= -15 & lon5 >= 110 & lon5 < 130 & !is.na(lat5)), 2)) %>%
      mutate(regB = replace(regB, which(lat5 < -20 & lat5 >= -25 & lon5 >= 35 & lon5 < 75 & !is.na(lat5)), 3)) %>%
      mutate(regB = replace(regB, which(lat5 < -25 & lat5 >= -30 & lon5 >= 30 & lon5 < 75 & !is.na(lat5)), 3)) %>%
      mutate(regB = replace(regB, which(lat5 < -30 & lat5 >= -35 & lon5 >= 20 & lon5 < 75 & !is.na(lat5)), 3)) %>%
      mutate(regB = replace(regB, which(lat5 < -15 & lat5 >= -35 & lon5 >= 46 & lon5 < 75 & !is.na(lat5)), 3)) %>%
      mutate(regB = replace(regB, which(lat5 < -15 & lat5 >= -20 & lon5 >= 75 & lon5 < 125 & !is.na(lat5)), 3)) %>%
      mutate(regB = replace(regB, which(lat5 < -20 & lat5 >= -35 & lon5 >= 75 & lon5 < 120 & !is.na(lat5)), 3))
  }

  if (regB1) {
    dat$regB1 <- 0
    dat <- mutate(dat, regB1 = replace(regB1, which(regB1 %in% 1:3), 1)) # Doesn't look right
  }

  if (regB2) {
    dat$regB2 <- 0
    dat <- mutate(dat,
             regB2 = replace(regB2, which(lat5 < 10 & lat5 >= 0 & lon5 >= 40 & lon5 < 80 & !is.na(lat5)), 1)) %>%
      mutate(regB2 = replace(regB2, which(lat5 < 0   & lat5 >= -10 & lon5 >= 35 & lon5 < 80 & !is.na(lat5)), 1)) %>%
      mutate(regB2 = replace(regB2, which(lat5 < -10 & lat5 >= -15 & lon5 >= 40 & lon5 < 80 & !is.na(lat5)), 1)) %>%
      mutate(regB2 = replace(regB2, which(lat5 < -15 & lat5 >= -20 & lon5 >= 35 & lon5 < 46 & !is.na(lat5)), 1)) %>%
      mutate(regB2 = replace(regB2, which(lat5 < 10 & lat5 >= -15 & lon5 >= 80 & lon5 < 100 & !is.na(lat5)), 2)) %>%
      mutate(regB2 = replace(regB2, which(lat5 < -3 & lat5 >= -15 & lon5 >= 100 & lon5 < 110 & !is.na(lat5)), 2)) %>%
      mutate(regB2 = replace(regB2, which(lat5 < -8 & lat5 >= -15 & lon5 >= 110 & lon5 < 130 & !is.na(lat5)), 2)) %>%
      mutate(regB2 = replace(regB2, which(lat5 < -20 & lat5 >= -25 & lon5 >= 35 & lon5 < 75 & !is.na(lat5)), 3)) %>%
      mutate(regB2 = replace(regB2, which(lat5 < -25 & lat5 >= -30 & lon5 >= 30 & lon5 < 75 & !is.na(lat5)), 3)) %>%
      mutate(regB2 = replace(regB2, which(lat5 < -30 & lat5 >= -35 & lon5 >= 20 & lon5 < 75 & !is.na(lat5)), 3)) %>%
      mutate(regB2 = replace(regB2, which(lat5 < -15 & lat5 >= -35 & lon5 >= 46 & lon5 < 75 & !is.na(lat5)), 3)) %>%
      mutate(regB2 = replace(regB2, which(lat5 < -15 & lat5 >= -20 & lon5 >= 75 & lon5 < 125 & !is.na(lat5)), 4)) %>%
      mutate(regB2 = replace(regB2, which(lat5 < -20 & lat5 >= -35 & lon5 >= 75 & lon5 < 120 & !is.na(lat5)), 4))
  }

  if (regB3) {
    dat$regB3 <- 0
    # Editr
    dat <- mutate(dat, regB3 = replace(regB3, which(lat5 < 10 & lat5 >= 0 & lon5 >= 40 & lon5 < 80 & !is.na(lat5)), 5)) %>%
      mutate(regB3 = replace(regB3, which(lat5 < 0   & lat5 >= -10 & lon5 >= 35 & lon5 < 80 & !is.na(lat5)), 1)) %>%
      mutate(regB3 = replace(regB3, which(lat5 < -10 & lat5 >= -15 & lon5 >= 40 & lon5 < 80 & !is.na(lat5)), 1)) %>%
      mutate(regB3 = replace(regB3, which(lat5 < -15 & lat5 >= -20 & lon5 >= 35 & lon5 < 46 & !is.na(lat5)), 1)) %>%
      mutate(regB3 = replace(regB3, which(lat5 < 10 & lat5 >= -15 & lon5 >= 80 & lon5 < 100 & !is.na(lat5)), 2)) %>%
      mutate(regB3 = replace(regB3, which(lat5 < -3 & lat5 >= -15 & lon5 >= 100 & lon5 < 110 & !is.na(lat5)), 2)) %>%
      mutate(regB3 = replace(regB3, which(lat5 < -8 & lat5 >= -15 & lon5 >= 110 & lon5 < 130 & !is.na(lat5)), 2)) %>%
      mutate(regB3 = replace(regB3, which(lat5 < -20 & lat5 >= -25 & lon5 >= 35 & lon5 < 75 & !is.na(lat5)), 3)) %>%
      mutate(regB3 = replace(regB3, which(lat5 < -25 & lat5 >= -30 & lon5 >= 30 & lon5 < 75 & !is.na(lat5)), 3)) %>%
      mutate(regB3 = replace(regB3, which(lat5 < -30 & lat5 >= -35 & lon5 >= 20 & lon5 < 75 & !is.na(lat5)), 3)) %>%
      mutate(regB3 = replace(regB3, which(lat5 < -15 & lat5 >= -35 & lon5 >= 46 & lon5 < 75 & !is.na(lat5)), 3)) %>%
      mutate(regB3 = replace(regB3, which(lat5 < -15 & lat5 >= -20 & lon5 >= 75 & lon5 < 125 & !is.na(lat5)), 4)) %>%
      mutate(regB3 = replace(regB3, which(lat5 < -20 & lat5 >= -35 & lon5 >= 75 & lon5 < 120 & !is.na(lat5)), 4))
  }

  # regA
  if (regA) {
    dat$regA <- 0
    dat <- mutate(dat, regA = replace(regA, which(dat$lat5 < -10 & dat$lat5 >= -20 & !is.na(dat$lat5)), 1)) %>%
      mutate(regA = replace(regA, which(dat$lat5 < -20 & dat$lat5 > -40 & !is.na(dat$lat5)), 2))
  }

  if (regA1) {
    dat$regA1 <- 0
    dat <- mutate(dat, regA1 = replace(regA1, which(dat$lat5 < -10 & dat$lat5 >= -25 & !is.na(dat$lat5)), 1)) %>%
      mutate(regA1 = replace(regA1, which(dat$lat5 < -25 & dat$lat5 > -40 & !is.na(dat$lat5)), 2))
  }

  if (regA2) {
    dat$regA2 <- 0
    dat <- mutate(dat, regA2 = replace(regA2, which(dat$lat5 < -10 & dat$lat5 >= -20 & dat$lon5 < 75 & !is.na(dat$lat5)), 1)) %>%
      mutate(regA2 = replace(regA2, which(dat$lat5 < -10 & dat$lat5 >= -20 & dat$lon5 >= 75 & !is.na(dat$lat5)), 2)) %>%
      mutate(regA2 = replace(regA2, which(dat$lat5 < -20 & dat$lat5 > -40 & dat$lon5 < 75 & !is.na(dat$lat5)), 3)) %>%
      mutate(regA2 = replace(regA2, which(dat$lat5 < -20 & dat$lat5 > -40 & dat$lon5 >= 75 & !is.na(dat$lat5)), 4))
  }

  if (regA3) {
    dat$regA3 <- 0
    dat <- mutate(dat, regA3 = replace(regA3, which(dat$lat5 < -10 & dat$lat5 >= -25 & dat$lon5 < 75 & !is.na(dat$lat5)), 1)) %>%
      mutate(regA3 = replace(regA3, which(dat$lat5 < -10 & dat$lat5 >= -25 & dat$lon5 >= 75 & !is.na(dat$lat5)), 2)) %>%
      mutate(regA3 = replace(regA3, which(dat$lat5 < -25 & dat$lon5 < 75 & !is.na(dat$lat5)), 3)) %>%
      mutate(regA3 = replace(regA3, which(dat$lat5 < -25 & dat$lon5 >= 75 & !is.na(dat$lat5)), 4))
  }

  if (regA4) {
    dat$regA4 <- 0
    dat <- mutate(dat, regA4 = replace(regA4, which(dat$lat5 < -10 & dat$lat5 >= -25 & dat$lon5 < 75 & !is.na(dat$lat5)), 1)) %>%
      mutate(regA4 = replace(regA4, which(dat$lat5 < -10 & dat$lat5 >= -25 & dat$lon5 >= 75 & !is.na(dat$lat5)), 2)) %>%
      mutate(regA4 = replace(regA4, which(dat$lat5 < -25 & dat$lat5 > -40 & dat$lon5 < 75 & !is.na(dat$lat5)), 3)) %>%
      mutate(regA4 = replace(regA4, which(dat$lat5 < -25 & dat$lat5 > -40 & dat$lon5 >= 75 & !is.na(dat$lat5)), 4))
  }

  if (regA5) {
    dat$regA5 <- 0
    dat <- mutate(dat, regA5 = replace(regA5, which(dat$lat5 < -15 & dat$lat5 >= -45 & dat$lon5 > 55 & dat$lon5 < 100 & !is.na(dat$lat5)), 1))
  }
  return(dat)
}


#' Set up Atlantic Ocean regions
#'
#' The function sets up the Atlantic Ocean regions for datasets with lat5 and lon5 variables.
#' @param dat Input dataset
#' @param regB If TRUE, set up regB
#' @param regB1 If TRUE, set up regB1
#' @return Modified dataset.
#'
setup_AO_regions <- function(dat, regB = F, regB1 = F) {
  # north of 25N, between 25N and 15S, and south of 15S
  lat5 <- lon5 <- NULL
  if (regB) {
    dat$regB <- 0
    dat <- mutate(dat, regB = replace(regB, which(lat5 < 45 & lat5 >= 25 & lon5 < -5 & !is.na(lat5)), 1)) %>%
      mutate(regB = replace(regB, which(lat5 < 25 & lat5 >= -15 & !is.na(lat5)), 2)) %>%
      mutate(regB = replace(regB, which(lat5 < -15 & lat5 >= -35& !is.na(lat5)), 3))
  }
  if (regB1) {
    dat$regB1 <- 0
    dat <- mutate(dat, regB1 = replace(regB1, which(lat5 < 40 & lat5 >= 20 & lon5 < -5 & !is.na(lat5)), 1)) %>%
      mutate(regB1 = replace(regB1, which(lat5 < 20 & lat5 >= -20 & !is.na(lat5)), 2)) %>%
      mutate(regB1 = replace(regB1, which(lat5 < -20 & lat5 >= -35& !is.na(lat5)), 3))
  }

  return(dat)
}

#' Prepare US longline data.
#'
#' The function prepares US longline data. Developed for AO analyses.
#' @param dat Input dataset
#' @param splist List of species codes
#' @return Modified dataset.
#'
dataprep_US <- function(dat, splist) {
  dat$qtr <- case_when(
    dat$qtr == "Jan-Mar" ~ 0.125,
    dat$qtr == "Apr-Jun" ~ 0.375,
    dat$qtr == "Jul-Sep" ~ 0.625,
    dat$qtr == "Oct-Dec" ~ 0.875)
  dat$yrqtr <- dat$op_yr + dat$qtr
  dat$latlong <- paste(dat$lat5, dat$lon5, sep = "_")

  dat$vessid <- as.factor(as.numeric(as.factor(dat$callsign)))
  dat$tripid <- dat$tripidmon <- paste(dat$vessid, dat$trip_st)
  dat$Total <- apply(dat[,splist], 1, sum, na.rm = TRUE)
  dat$Total2 <- apply(dat[, c("bet", "yft", "alb")], 1, sum, na.rm = TRUE)
  return(dat)
}

