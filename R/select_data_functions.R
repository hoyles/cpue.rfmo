#' Select data for the GLM.
#'
#' The function chooses fields and rows of data for the GLM analysis according to various criteria. This version developed for Japanese data in the Pacific.
#' @param indat Input dataset
#' @param runreg The region label to select data for.
#' @param runsp Species of interest.
#' @param mt Model type, used to select only nonzero sets when mt is 'deltapos'.
#' @param minqtrs Vessels must fish in at least this many qtrs.
#' @param maxqtrs Vessels must fish no more than this many qtrs.
#' @param addmain Include mainline in the dataset if TRUE.
#' @param addother Include bet or yft in the dataset if TRUE, depending on the species of interest.
#' @param addalb Include 'alb' in the dataset if TRUE.
#' @param fc Select only fishing category 1 or 2. Applies only to Japanese data with these variables reported.
#' @param bait For Japanese data - select certain bait types.
#' @param llstrat Define latlong based on this stratification level defined here.
#' @param doboth Remove whichever of bet and yft is not the species of interest, if FALSE.
#' @param addcl Deprecated, not used.
#' @return Modified dataset.
#'
select_data <- function(indat, runreg, runsp, mt, minqtrs = 2, maxqtrs = 500, addmain = F, addother = F, addalb = F, fc = "both", bait = "no23", llstrat = 5,
                        doboth = F, addcl = F) {
  gdat <- indat[indat$reg == runreg, ]
  if (runreg == 9)
    gdat <- indat[indat$reg == 3 & indat$lat >= -5 & indat$lat < 10, ]
  if (llstrat != 5)
    gdat$latlong <- paste(llstrat * floor(gdat$lat/llstrat), llstrat * floor(gdat$lon/llstrat), sep = "_")
  if (mt == "deltapos")
    gdat <- gdat[gdat[, runsp] > 0, ]
  if (bait == "no23")
    gdat <- gdat[(gdat$bait != 2 & gdat$bait != 3) | is.na(gdat$bait), ]
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
  if (sum(is.na(gdat$hbf)) > 0)
    gdat[is.na(gdat$hbf), ]$hbf <- 5
  gdat <- gdat[gdat$hbf >= 5, ]
  fcnum <- switch(fc, both = 0, OS = 1, DW = 2)
  if (fc != "both")
    gdat <- gdat[gdat$newfishingcat == fcnum, ]
  if (addmain) {
    gdat <- gdat[gdat$target == 3, c("vessid", "hooks", "yft", "bet", "alb", "hbf", "yrqtr", "latlong", "mainline", "branchline")]
    gdat <- gdat[gdat$mainline %in% c(1, 2), ]
    gdat <- gdat[gdat$branchline %in% c(1, 2), ]
  } else {
    gdat <- gdat[, c("vessid", "hooks", "yft", "bet", "alb", "hbf", "yrqtr", "latlong")]
  }
  if (addother) {
    a <- (gdat[, switch(runsp, bet = "yft", yft = "bet")] + 1)/gdat$hooks
    divs <- c(0, 0.1, 0.5, 0.9, 1)
    b <- quantile(a, divs)
    gdat$other <- findInterval(a, b)
  }
  if (addalb) {
    a <- (gdat[, "alb"] + 1)/gdat$hooks
    divs <- c(0, 0.1, 0.5, 0.9, 1)
    b <- quantile(a, divs)
    gdat$alb_cat <- findInterval(a, b)
  }
  a <- grep(switch(runsp, bet = "yft", yft = "bet"), names(gdat))
  if (!doboth)
    gdat <- gdat[, -a]
  a <- grep("alb", names(gdat), fixed = T)[1]
  gdat <- gdat[, -a]
  return(gdat)
}

#' Select data for the GLM.
#'
#' The function chooses fields and rows of data for the GLM analysis according to various criteria. This version developed for Japanese data in the Indian Ocean
#' @param indat Input dataset
#' @param runreg The region label to select data for.
#' @param clk Include all cluster numbers listed in the cluster key 'clk'.
#' @param runsp Species of interest.
#' @param mt Model type, used to select only nonzero sets when mt is 'deltapos'.
#' @param vars Default variables to include in the dataset.
#' @param minqtrs Vessels must fish in at least this many qtrs.
#' @param maxqtrs Vessels must fish no more than this many qtrs.
#' @param minvess Include only vessels with at least this many sets.
#' @param minll Include only grid cells with at least this many sets.
#' @param minyrqtr Include only year-qtrs with at least this many sets.
#' @param llstrat Define latlong based on this stratification level defined here.
#' @param addcl If TRUE, 'cltype' defines the name of the cluster variable to include in the dataset.
#' @param cltype Defines the name of the cluster variable to include in the dataset, if addcl is TRUE.
#' @param addpca Include PCA variables in the dataset if TRUE.
#' @param samp Unless NA, apply random subsampling with n = samp.
#' @param strsmp Unless NA, apply stratified sampling with n = strsmp.
#' @return Modified dataset.
#'
select_data_IO <- function(indat, runreg, clk = NA, runsp, mt, vars, minqtrs = 2, maxqtrs = 500, minvess = 100, minll = 100, minyrqtr = 100, llstrat = 5,
                           addcl = F, cltype = NA, addpca = NA, samp = NA, strsmp = 30) {
  gdat <- indat[indat$reg == runreg, ]
  if (addcl)
    names(gdat)[names(gdat) == cltype] <- "clust"
  if (sum(is.na(gdat$hbf)) > 0)
    gdat[is.na(gdat$hbf), ]$hbf <- 5
  gdat <- gdat[gdat$hbf >= 5, ]
  if (!is.na(clk[1]))
    gdat <- gdat[gdat$clust %in% clk, ]
  if (llstrat != 5)
    gdat$latlong <- paste(llstrat * floor(gdat$lat/llstrat), llstrat * floor(gdat$lon/llstrat), sep = "_")
  if (mt == "deltapos")
    gdat <- gdat[gdat[, runsp] > 0, ]
  a <- table(gdat$vessid, gdat$yrqtr)
  a <- apply(a > 0, 1, sum)
  a <- a[a >= minqtrs & a <= maxqtrs]
  gdat <- gdat[gdat$vessid %in% names(a), ]
  a <- table(gdat$yrqtr)
  a
  a <- a[a >= minyrqtr]
  gdat <- gdat[gdat$yrqtr %in% names(a), ]
  a <- table(gdat$latlong)
  a
  a <- a[a >= minll]
  gdat <- gdat[gdat$latlong %in% names(a), ]
  a <- table(gdat$vessid)
  a
  a <- a[a >= minvess]
  gdat <- gdat[gdat$vessid %in% names(a), ]
  vars <- c(vars, "clust")
  if (!is.na(addpca))
    vars <- c(vars, addpca)
  vars <- c(vars, runsp)
  gdat <- gdat[, vars]
  if (!is.na(samp))
    gdat <- samp_data2(gdat, samp)
  if (!is.na(strsmp))
    gdat <- samp_strat_data(gdat, strsmp)
  gdat$vessid <- as.factor(gdat$vessid)
  gdat$latlong <- as.factor(gdat$latlong)
  gdat$yrqtr <- as.factor(gdat$yrqtr)
  gdat$clust <- as.factor(gdat$clust)
  return(gdat)
}

#' Select joint data for the GLM.
#'
#' The function chooses fields and rows of data for the GLM analysis according to various criteria. This version developed for selecting from combined (joint) datasets in the Indian Ocean.
#' @param indat Input dataset
#' @param runreg The region label to select data for.
#' @param clk Include all cluster numbers listed in the cluster key 'clk'.
#' @param runsp Species of interest.
#' @param mt Model type, used to select only nonzero sets when mt is 'deltapos'.
#' @param vars Default variables to include in the dataset.
#' @param minqtrs Vessels must fish in at least this many qtrs.
#' @param maxqtrs Vessels must fish no more than this many qtrs.
#' @param minvess Include only vessels with at least this many sets.
#' @param minll Include only grid cells with at least this many sets.
#' @param minyrqtr Include only year-qtrs with at least this many sets.
#' @param llstrat Define latlong based on this stratification level defined here.
#' @param addcl If TRUE, 'cltype' defines the name of the cluster variable to include in the dataset.
#' @param cltype Defines the name of the cluster variable to include in the dataset, if addcl is TRUE.
#' @param addpca Include PCA variables in the dataset if TRUE.
#' @param samp Unless NA, apply random subsampling with n = samp.
#' @param strsmp Unless NA, apply stratified sampling with n = strsmp.
#' @param yrlims Bounding years for the analysis. Use integer values, because yrqtrs use 0.125 to 0.875.
#' @param oneflag If not NA, the flag to use in the analysis. Otherwise flags JP, KR, SY, TW are used.
#' @param minhbf All sets with hbf less than this value are removed.
#' @param minyqll Sets that are in yqll strata with fewer than minyqll sets are removed.
#' @return Modified dataset.
#'
select_data_JointIO <- function(indat, runreg, clk = NA, runsp, mt, vars, minqtrs = 2, maxqtrs = 500, minvess = 100, minll = 100, minyrqtr = 100, llstrat = 5, addcl = F, cltype = NA, addpca = NA, samp = NA, strsmp = 30, yrlims = NA, oneflag = NA, minhbf = 5, minyqll = 1) {
  gdat <- indat[indat$reg == runreg, ]
  if (!is.na(yrlims[1]))
    gdat <- gdat[gdat$yrqtr > yrlims[1] & gdat$yrqtr < yrlims[2], ]
  if (addcl) {
    names(gdat)[names(gdat) == cltype] <- "clust"
    a <- data.frame()
    if (!is.na(clk[1])) {
      if (!is.na(oneflag)) {
        gdat <- rbind(a, gdat[gdat$clust %in% clk[[oneflag]][[runsp]][[runreg]], ])
      } else {
        a <- rbind(a, gdat[gdat$clust %in% clk$JP[[runsp]][[runreg]] & gdat$flag == "JP", ])
        a <- rbind(a, gdat[gdat$clust %in% clk$KR[[runsp]][[runreg]] & gdat$flag == "KR", ])
        a <- rbind(a, gdat[gdat$clust %in% clk$SY[[runsp]][[runreg]] & gdat$flag == "SY", ])
        a <- rbind(a, gdat[gdat$clust %in% clk$CN[[runsp]][[runreg]] & gdat$flag == "CN", ])
        a <- rbind(a, gdat[gdat$clust %in% clk$US[[runsp]][[runreg]] & gdat$flag == "US", ])
        gdat <- rbind(a, gdat[gdat$clust %in% clk$TW[[runsp]][[runreg]] & gdat$flag == "TW", ])
      }
    }
    gdat$clust <- as.factor(paste0(gdat$flag, gdat$clust))
    vars <- c(vars, "clust")
  }
  if (dim(gdat)[1] > 0) {
    if (sum(is.na(gdat$hbf)) > 0)
      gdat[is.na(gdat$hbf), ]$hbf <- 5
    if (sum(gdat$hbf == 0) > 0)
      gdat[gdat$hbf == 0, ]$hbf <- 5
    gdat <- gdat[gdat$hbf >= minhbf, ]
    if (llstrat != 5)
      gdat$latlong <- paste(llstrat * floor(gdat$lat/llstrat), llstrat * floor(gdat$lon/llstrat), sep = "_")
    if (mt == "deltapos")
      gdat <- gdat[gdat[, runsp] > 0, ]

    yqll <- paste(gdat$yrqtr, gdat$latlong)
    a <- table(yqll)
    a <- apply(a > minyqll, 1, sum)
    gdat <- gdat[yqll %in% names(a), ]
    a <- table(gdat$vessid, gdat$yrqtr)
    a <- apply(a > 0, 1, sum)
    a <- a[a >= minqtrs & a <= maxqtrs]  # Vessel fishes in at least 'minqtrs' quarters
    gdat <- gdat[gdat$vessid %in% names(a), ]
    a <- table(gdat$yrqtr)
    a <- a[a >= minyrqtr]  # At least 'minyrqtr' sets in the yrqtr
    gdat <- gdat[gdat$yrqtr %in% names(a), ]
    a <- table(gdat$latlong)
    a <- a[a >= minll]  # At least 'minll' sets in the cell
    gdat <- gdat[gdat$latlong %in% names(a), ]
    a <- table(gdat$vessid)
    a <- a[a >= minvess]  # At least 'minvess' sets by the vessel
    gdat <- gdat[gdat$vessid %in% names(a), ]

    if (!is.na(addpca))
      vars <- c(vars, addpca)
    vars <- c(vars, runsp)
    gdat <- gdat[, vars]
    if (!is.na(samp))
      gdat <- samp_data2(gdat, samp)
    if (!is.na(strsmp))
      gdat <- samp_strat_data(gdat, strsmp)
    gdat$vessid <- as.factor(gdat$vessid)
    gdat$latlong <- as.factor(gdat$latlong)
    gdat$yrqtr <- as.factor(as.character(gdat$yrqtr))
    if (addcl != F)
      gdat$clust <- as.factor(gdat$clust)
  }
  return(gdat)
}

#' Select joint data for the GLM, updated 2018.
#'
#' The function chooses fields and rows of data for the GLM analysis according to various criteria. This version developed for selecting from combined (joint) datasets in the Indian Ocean.
#' @param indat Input dataset
#' @param runreg The region label to select data for.
#' @param runpars Settings for this run, passed as a list.
#' @param mt Model type, used to select only nonzero sets when mt is 'deltapos'.
#' @param vars Default variables to include in the dataset.
#' @param yrlims Bounding years for the analysis. Use integer values, because yrqtrs use 0.125 to 0.875.
#' @param oneflag If not NA, the flag to use in the analysis. Otherwise flags JP, KR, SY, TW are used.
#' @return Modified dataset.
#'
select_data_IO2 <- function(indat, runreg, runpars, mt, vars, yrlims = NA, oneflag = NA) {
  clk <- runpars$clk
  minss <- runpars$minss
  minqtrs <- minss$minq_byreg[runreg]
  minvess <- minss$minvess[runreg]
  minll <- minss$minll[runreg]
  minyrqtr <- minss$minyrqtr[runreg]
  minyqll <- minss$minyqll[runreg]
  runsp <- runpars$runsp
  addcl <- runpars$addcl
  cltype = runpars$cltype
  if("minhbf" %in% names(runpars))  minhbf <- runpars$minhbf else minhbf <- 5
  if("addpca" %in% names(runpars))  addpca <- runpars$addpca else addpca <- NA
  if("samp" %in% names(runpars))    samp <- runpars$samp else samp <- NA
  if("llstrat" %in% names(runpars)) llstrat <- runpars$llstrat else llstrat <- 5

  gdat <- indat[indat$reg == runreg, ]
  if (!is.na(yrlims[1]))
    gdat <- gdat[gdat$yrqtr > yrlims[1] & gdat$yrqtr < yrlims[2], ]
  if (addcl) {
    names(gdat)[names(gdat) == cltype] <- "clust"
    a <- data.frame()
    if (!is.na(clk[1])) {
      if (!is.na(oneflag)) {
        gdat <- rbind(a, gdat[gdat$clust %in% clk[[oneflag]][[runsp]][[runreg]], ])
      } else {
        a <- rbind(a, gdat[gdat$clust %in% clk$JP[[runsp]][[runreg]] & gdat$flag == "JP", ])
        a <- rbind(a, gdat[gdat$clust %in% clk$KR[[runsp]][[runreg]] & gdat$flag == "KR", ])
        a <- rbind(a, gdat[gdat$clust %in% clk$SY[[runsp]][[runreg]] & gdat$flag == "SY", ])
        a <- rbind(a, gdat[gdat$clust %in% clk$CN[[runsp]][[runreg]] & gdat$flag == "CN", ])
        a <- rbind(a, gdat[gdat$clust %in% clk$US[[runsp]][[runreg]] & gdat$flag == "US", ])
        gdat <- rbind(a, gdat[gdat$clust %in% clk$TW[[runsp]][[runreg]] & gdat$flag == "TW", ])
      }
    }
    gdat$clust <- as.factor(paste0(gdat$flag, gdat$clust))
    vars <- c(vars, "clust")
  }
  if (dim(gdat)[1] > 0) {
    if (sum(is.na(gdat$hbf)) > 0)
      gdat[is.na(gdat$hbf), ]$hbf <- 5
    if (sum(gdat$hbf == 0) > 0)
      gdat[gdat$hbf == 0, ]$hbf <- 5
    gdat <- gdat[gdat$hbf >= minhbf, ]
    if (llstrat != 5)
      gdat$latlong <- paste(llstrat * floor(gdat$lat/llstrat), llstrat * floor(gdat$lon/llstrat), sep = "_")
    if (mt == "deltapos")
      gdat <- gdat[gdat[, runsp] > 0, ]

    # Data cleaning
    yqll <- paste(gdat$yrqtr, gdat$latlong)
    a <- table(yqll)
    a <- a[a >= minyqll]
    gdat <- gdat[yqll %in% names(a), ] # Each stratum has at least minyqll sets

    a <- table(gdat$vessid, gdat$yrqtr)
    a <- apply(a > 0, 1, sum, na.rm = TRUE)
    a <- a[a >= minqtrs]  # Each vessel fishes in at least 'minqtrs' quarters

    gdat <- gdat[gdat$vessid %in% names(a), ]
    a <- table(gdat$yrqtr)
    a <- a[a >= minyrqtr]  # Each yrqtr has at least 'minyrqtr' sets

    gdat <- gdat[gdat$yrqtr %in% names(a), ]
    a <- table(gdat$latlong)
    a <- a[a >= minll]  # Each latlong cell at least 'minll' sets

    gdat <- gdat[gdat$latlong %in% names(a), ]
    a <- table(gdat$vessid)
    a <- a[a >= minvess]  # Each vessel has at least 'minvess' sets
    gdat <- gdat[gdat$vessid %in% names(a), ]

    yqll <- paste(gdat$yrqtr, gdat$latlong)
    a <- table(yqll)
    a <- a[a >= minyqll]
    gdat <- gdat[yqll %in% names(a), ] # Each stratum has at least minyqll sets

    if (!is.na(addpca))
      vars <- c(vars, addpca)
    vars <- c(vars, runsp)
    gdat <- gdat[, vars]
    if (!is.na(samp))
      gdat <- samp_data2(gdat, samp)
    gdat$vessid <- as.factor(gdat$vessid)
    gdat$latlong <- as.factor(gdat$latlong)
    gdat$yrqtr <- as.factor(as.character(gdat$yrqtr))
    if (addcl != F)
      gdat$clust <- as.factor(gdat$clust)
  }
  return(gdat)
}

#' Select JP data for the GLM.
#'
#' The function chooses fields and rows of data for the GLM analysis according to various criteria. This version developed for selecting from Japanese datasets in the Indian Ocean. Probably no longer used, replaced by select_data_JointIO.
#' @param indat Input dataset
#' @param runreg The region label to select data for.
#' @param runsp Species of interest.
#' @param mt Model type, used to select only nonzero sets when mt is 'deltapos'.
#' @param minqtrs Vessels must fish in at least this many qtrs.
#' @param maxqtrs Vessels must fish no more than this many qtrs.
#' @param minvess Include only vessels with at least this many sets.
#' @param minll Include only grid cells with at least this many sets.
#' @param minyrqtr Include only year-qtrs with at least this many sets.
#' @param llstrat Define latlong based on this stratification level defined here.
#' @param addcl If not NA, defines the name of the cluster variable to include in the dataset.
#' @param addpca Include PCA variables in the dataset if TRUE.
#' @param samp Unless NA, apply random subsampling with n = samp.
#' @param strsmp Unless NA, apply stratified sampling with n = strsmp.
#' @return Modified dataset.
#'
select_data_JPIO <- function(indat, runreg, runsp, mt, minqtrs = 2, maxqtrs = 500, minvess = 100, minll = 100, minyrqtr = 100, llstrat = 5, addcl = NA,
                             addpca = NA, samp = NA, strsmp = 30) {
  gdat <- indat[indat$reg == runreg, ]
  if (sum(is.na(gdat$hbf)) > 0)
    gdat[is.na(gdat$hbf), ]$hbf <- 5
  gdat <- gdat[gdat$hbf >= 5, ]
  if (llstrat != 5)
    gdat$latlong <- paste(llstrat * floor(gdat$lat/llstrat), llstrat * floor(gdat$lon/llstrat), sep = "_")
  if (mt == "deltapos")
    gdat <- gdat[gdat[, runsp] > 0, ]
  a <- table(gdat$vessid, gdat$yrqtr)
  a <- apply(a > 0, 1, sum)
  table(a)
  a <- a[a >= minqtrs & a <= maxqtrs]
  gdat <- gdat[gdat$vessid %in% names(a), ]
  a <- table(gdat$yrqtr)
  a
  a <- a[a >= minyrqtr]
  gdat <- gdat[gdat$yrqtr %in% names(a), ]
  a <- table(gdat$latlong)
  a
  a <- a[a >= minll]
  gdat <- gdat[gdat$latlong %in% names(a), ]
  a <- table(gdat$vessid)
  a
  a <- a[a >= minvess]
  gdat <- gdat[gdat$vessid %in% names(a), ]
  vars <- c("vessid", "hooks", "hbf", "yrqtr", "latlong", "moon")
  if (!is.na(addcl))
    vars <- c(vars, addcl)
  if (!is.na(addpca))
    vars <- c(vars, addpca)
  vars <- c(vars, runsp)
  gdat <- gdat[, vars]
  names(gdat)[names(gdat) == addcl] <- "clust"
  if (!is.na(samp))
    gdat <- samp_data2(gdat, samp)
  if (!is.na(strsmp))
    gdat <- samp_strat_data(gdat, strsmp)
  gdat$vessid <- as.factor(gdat$vessid)
  gdat$latlong <- as.factor(gdat$latlong)
  gdat$yrqtr <- as.factor(gdat$yrqtr)
  gdat$clust <- as.factor(gdat$clust)
  return(gdat)
}

#' Select TW data for the GLM.
#'
#' The function chooses fields and rows of data for the GLM analysis according to various criteria. This version developed for selecting from Taiwanese datasets in the Indian Ocean. Probably no longer used, replaced by select_data_JointIO.
#' @param indat Input dataset
#' @param runreg The region label to select data for.
#' @param runsp Species of interest.
#' @param mt Model type, used to select only nonzero sets when mt is 'deltapos'.
#' @param minqtrs Vessels must fish in at least this many qtrs.
#' @param maxqtrs Vessels must fish no more than this many qtrs.
#' @param addmain Include mainline in the dataset if TRUE.
#' @param addother Include bet or yft in the dataset if TRUE, depending on the species of interest.
#' @param addalb Include 'alb' in the dataset if TRUE.
#' @param fc Select only fishing category 1 or 2. Applies only to Japanese data with these variables reported.
#' @param bait Select certain bait types.
#' @param llstrat Define latlong based on this stratification level defined here.
#' @param doboth Remove whichever of bet and yft is not the species of interest, if FALSE.
#' @return Modified dataset.
#'
select_data_TW <- function(indat, runreg, runsp, mt, minqtrs = 2, maxqtrs = 500, addmain = F, addother = F, addalb = F, fc = "both", bait = "no23",
                           llstrat = 5, doboth = F) {
  gdat <- indat[indat$reg == runreg, ]
  if (llstrat != 5)
    gdat$latlong <- paste(llstrat * floor(gdat$lat/llstrat), llstrat * floor(gdat$lon/llstrat), sep = "_")
  if (mt == "deltapos")
    gdat <- gdat[gdat[, runsp] > 0, ]
  if (bait == "no23")
    gdat <- gdat[(gdat$bait != 2 & gdat$bait != 3) | is.na(gdat$bait), ]
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
  if (sum(is.na(gdat$hbf)) > 0)
    gdat[is.na(gdat$hbf), ]$hbf <- 5
  gdat <- gdat[gdat$hbf >= 5, ]
  fcnum <- switch(fc, both = 0, OS = 1, DW = 2)
  if (fc != "both")
    gdat <- gdat[gdat$newfishingcat == fcnum, ]
  if (addmain) {
    gdat <- gdat[gdat$target == 3, c("vessid", "hooks", "yft", "bet", "alb", "hbf", "yrqtr", "latlong", "mainline", "branchline")]
    gdat <- gdat[gdat$mainline %in% c(1, 2), ]
    gdat <- gdat[gdat$branchline %in% c(1, 2), ]
  } else {
    gdat <- gdat[, c("vessid", "hooks", "yft", "bet", "alb", "hbf", "yrqtr", "latlong")]
  }
  if (addother) {
    a <- (gdat[, switch(runsp, bet = "yft", yft = "bet")] + 1)/gdat$hooks
    divs <- c(0, 0.1, 0.5, 0.9, 1)
    b <- quantile(a, divs)
    gdat$other <- findInterval(a, b)
  }
  if (addalb) {
    a <- (gdat[, "alb"] + 1)/gdat$hooks
    divs <- c(0, 0.1, 0.5, 0.9, 1)
    b <- quantile(a, divs)
    gdat$alb_cat <- findInterval(a, b)
  }
  a <- grep(switch(runsp, bet = "yft", yft = "bet"), names(gdat))
  if (!doboth)
    gdat <- gdat[, -a]
  a <- grep("alb", names(gdat), fixed = T)[1]
  gdat <- gdat[, -a]
  return(gdat)
}


