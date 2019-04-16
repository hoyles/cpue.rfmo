#' Extract yrqtr coefficients from a binomial glm.
#'
#' The function extracts yrqtr coefficients from a binomial glm.
#' @param model The model result object from the binomial glm.
#' @param nyrs Deprecated, not used.
#' @param dat Deprecated, not used.
#' @return Yrqtr coefficients as proportions, with the same mean as the observed proportions.
#'
get.bin.coefs <- function(model, nyrs=NA, dat=NA) {
    nyrs <- length(unlist(model$xlevels[1]))
    mn <- logit(mean(model$y))
    a <- c(0, summary(model)$coefficients[2:nyrs, 1])
    a <- a - mean(a) + mn
    coefs <- inv.logit(a)
    return(coefs)
}

#' Extract yrqtr coefficients from a binomial glm summary object.
#'
#' The function extracts yrqtr coefficients from a binomial glm summary object.
#' @param modelsumm The model summary object from a binomial glm.
#' @param nyrs The number of yrqtr time variables to extract.
#' @param dat Deprecated, not used.
#' @return Yrqtr coefficients as proportions, with the same mean as the observed proportions.
#'
get.bin.coefs.summ <- function(modelsumm, nyrs, dat=NA) {
    mn <- logit(mean(modelsumm$deviance.resid > -0.001))
    a <- c(0, modelsumm$coefficients[2:nyrs, 1])
    a <- a - mean(a) + mn
    coefs <- inv.logit(a)
    return(coefs)
}

#' Old version of get.bin.coefs.
#'
#' The function extracts yrqtr coefficients from a binomial glm summary.
#' @param model The model summary object from a binomial glm.
#' @param nyrs The number of yrqtr time variables to extract.
#' @param dat Data file used to produce glm.
#' @return Yrqtr coefficients as proportions, with the same mean as the observed proportions.
#'
get.bin.coefs2 <- function(model, nyrs=NA, dat) {
    nyrs <- length(unique(dat$yrqtr))
    mn <- logit(mean(dat[, 3] != 0))
    a <- c(0, summary(model)$coefficients[2:nyrs, 1])
    a <- a - mean(a) + mn
    coefs <- inv.logit(a)
    # coefs <- coefs/mean(coefs)
    return(coefs)
}

#' Extract yrqtr coefficients from a lognormal positive glm summary object.
#'
#' The function extracts yrqtr coefficients from a lognormal positive glm summary object.
#' @param modelsumm The summary object from the lognormal glm.
#' @param nyrs The number of yrqtr time variables to extract.
#' @return Normalized yrqtr coefficients.
#'
get.coefs.summ <- function(modelsumm, nyrs) {
    coefs <- exp(c(0, modelsumm$coefficients[2:nyrs, 1]))
    coefs <- coefs/mean(coefs)
    return(coefs)
}

#' Extract yrqtr coefficients from a lognormal positive glm summary object.
#'
#' The function extracts yrqtr coefficients from a lognormal positive glm summary object. Same as get.coefs.summ().
#' @param model The summary object from the lognormal glm.
#' @param nyrs The number of yrqtr time variables to extract.
#' @return Normalized yrqtr coefficients.
#'
get.summ.coefs <- function(model, nyrs) {
  coefs <- exp(c(0, model$coefficients[2:nyrs, 1]))
  coefs <- coefs/mean(coefs)
}

#' Extract yrqtr coefficients from a lognormal positive glm object.
#'
#' The function extracts yrqtr coefficients from a lognormal positive glm object.
#' @param model The summary object from the lognormal glm.
#' @return Normalized yrqtr coefficients.
#'
get.coefs <- function(model) {
  nyrs <- length(unlist(model$xlevels[1]))
  coefs <- exp(c(0, summary(model)$coefficients[2:nyrs, 1]))
  coefs <- coefs/mean(coefs)
  return(coefs)
}

#' Generate area-averaged yrqtr coefs from predicted lognormal constant CPUE, with interactions.
#'
#' The function generates area-averaged yrqtr coefficients from the predicted dataset for a lognormal constant glm with interactions.
#' @param nd The predicted dataset by space and time.
#' @param mn The mean value used as the constant.
#' @param bcorr Use lognormal bias correction if TRUE.
#' @param cell_areas Object with cell areas to use in summing densities across areas.
#' @return Normalized yrqtr coefficients.
#'
get.coefsx <- function(nd, mn, bcorr = F, cell_areas) {
  nd$areas <- cell_areas$garea[match(nd$latlong, cell_areas$latlong)]
  nd$density <- exp(nd$fit + bcorr * (nd$sd^2/2)) - mn
  nd$fish <- nd$density * nd$areas
  index <- tapply(nd$fish, nd$yrqtr, sum)
  coefs <- index/mean(index)
  return(coefs)
}

#' Generate area-averaged yrqtr coefs from predicted lognormal positive CPUE.
#'
#' The function extracts yrqtr coefficients from the predicted dataset for a lognormal positive glm with interactions.
#' @param nd The predicted dataset by space and time.
#' @param bcorr Use lognormal bias correction if TRUE.
#' @param cell_areas Object with cell areas to use in summing densities across areas.
#' @return Normalized yrqtr coefficients.
#'
get.poscoefsx <- function(nd, bcorr = F, cell_areas) {
  nd$areas <- cell_areas$garea[match(nd$latlong, cell_areas$latlong)]
  nd$density <- exp(nd$fit + bcorr * (nd$sd^2/2))
  nd$fish <- nd$density * nd$areas
  index <- tapply(nd$fish, nd$yrqtr, sum)
  coefs <- index/mean(index)
  return(coefs)
}

#' Generate area-averaged yrqtr coefs from predicted binomial CPUE.
#'
#' The function generates area-averaged yrqtr coefficients from the predicted dataset for a binomial glm with interactions.
#' @param nd The predicted dataset by space and time.
#' @param cell_areas Object with cell areas to use in summing densities across areas.
#' @return Yrqtr coefficients.
#'
get.bincoefsx <- function(nd, cell_areas) {
  nd$areas <- cell_areas$garea[match(nd$latlong, cell_areas$latlong)]
  nd$prob <- inv.logit(nd$fit)
  nd$fish <- nd$prob * nd$areas
  coefs <- tapply(nd$fish, nd$yrqtr, sum)/tapply(nd$areas, nd$yrqtr, sum)
  return(coefs)
}

#' Predict CPUE from a lognormal positive glm object.
#'
#' The function predicts CPUE lognormal positive glm object.
#' @param model The fitted model object.
#' @param dat Dataset used in the glm.
#' @return Predicted catch rates by yrqtr with CIs.
#'
glm.coefs <- function(model, dat) {
  nms <- names(dat)
  bval <- data.frame(t(rep(0, length(nms))))
  names(bval) <- nms
  facs <- sapply(dat[1, ], is.factor)
  for (i in 1:length(names(dat))) {
    if (facs[i])
      bval[, i] <- Mode(dat[, i])
    if (!facs[i])
      bval[, i] <- median(dat[, i])
  }
  bval <- bval[-grep("yrqtr", nms)]
  yrqtr <- data.frame(yrqtr = sort(unique(dat$yrqtr)))
  newdat <- expand.grid.df(as.data.frame(bval), yrqtr)
  res <- predict(model, newdata = newdat, type = "response", se.fit = TRUE)
  res.te <- predict(model, newdata = newdat, type = "terms", se.fit = TRUE)
  newdat$fitx <- res$fit
  newdat$fitx.se <- res.te$se.fit[, "yrqtr"]
  if (model$family$family == "gaussian") {
    newdat$fit <- exp(newdat$fitx)
    mout <- mean(newdat$fit)
    newdat$hi <- exp(newdat$fitx + 1.96 * newdat$fitx.se)/mout
    newdat$lo <- exp(newdat$fitx - 1.96 * newdat$fitx.se)/mout
    newdat$fit <- newdat$fit/mout
  } else {
    newdat$fit <- newdat$fitx
    newdat$hi <- inv.logit(logit(newdat$fit) + 1.96 * newdat$fitx.se)
    newdat$lo <- inv.logit(logit(newdat$fit) - 1.96 * newdat$fitx.se)
  }
  return(newdat)
}

#' Predict CPUE from a lognormal positive glm object.
#'
#' The function predicts CPUE lognormal positive glm object.
#' @param model The fitted model object.
#' @param dat Dataset used in the glm.
#' @param parm Variable to predict for, defaults to 'yrqtr'.
#' @return Predicted catch rates by yrqtr with CIs.
#'
glm.coefs2 <- function(model, dat, parm = "yrqtr") {
  nms <- names(dat)
  bval <- data.frame(t(rep(0, length(nms))))
  names(bval) <- nms
  facs <- sapply(dat[1, ], is.factor)
  for (i in 1:length(names(dat))) {
    if (facs[i])
      bval[, i] <- Mode(dat[, i])
    if (!facs[i])
      bval[, i] <- median(dat[, i])
  }
  bval <- bval[-grep(parm, nms)]
  pm <- data.frame(pm = sort(unique(dat[, parm])))
  newdat <- expand.grid.df(as.data.frame(bval), pm)
  names(newdat)[grep("pm", names(newdat))] <- parm
  res <- predict(model, newdata = newdat, type = "response", se.fit = TRUE)
  res.te <- predict(model, newdata = newdat, type = "terms", se.fit = TRUE)
  newdat$fitx <- res$fit
  pos <- grep(parm, dimnames(res.te$fit)[[2]])
  newdat$fitx.se <- res.te$se.fit[, pos]
  if (model$family$family == "gaussian") {
    newdat$fit <- exp(newdat$fitx)
    mout <- mean(newdat$fit)
    newdat$hi <- exp(newdat$fitx + 1.96 * newdat$fitx.se)/mout
    newdat$lo <- exp(newdat$fitx - 1.96 * newdat$fitx.se)/mout
    newdat$fit <- newdat$fit/mout
  } else {
    newdat$fit <- newdat$fitx
    newdat$hi <- inv.logit(logit(newdat$fit) + 1.96 * newdat$fitx.se)
    newdat$lo <- inv.logit(logit(newdat$fit) - 1.96 * newdat$fitx.se)
  }
  return(newdat)
}

#' Combine binomial and lognormal indices.
#'
#' The function combines binomial and lognormal positive indices to generate an index of relative abundance.
#' @param fnamedelta Name of the delta object to load.
#' @param fnamepos Name of the lognormal positive object to load.
#' @param fname File name used for saving outputs.
#' @param runsp Species of interest.
#' @param runreg The model region, used in output file names.
#'
combine_delta <- function(fnamedelta, fnamepos, fname, runsp, runreg) {
  model.base <- model.boat <- NULL
  load(paste("model.", fnamedelta, ".base.RData", sep = ""))
  load(paste("model.", fnamedelta, ".boat.RData", sep = ""))
  yrbin <- as.numeric(model.base$xlevels[[1]])
  coefsdeltabin.base <- get.coefs(model.base)
  coefsdeltabin.boat <- get.coefs(model.boat)
  rm(model.base, model.boat)
  gc()
  load(paste("model.", fnamepos, ".base.RData", sep = ""))
  load(paste("model.", fnamepos, ".boat.RData", sep = ""))
  yrpos <- as.numeric(model.base$xlevels[[1]])
  coefsdeltapos.base <- get.coefs(model.base)
  coefsdeltapos.boat <- get.coefs(model.boat)
  rm(model.base, model.boat)
  gc()
  a <- match(names(coefsdeltapos.base), names(coefsdeltabin.base))
  coefs.base <- coefsdeltabin.base[a] * coefsdeltapos.base
  coefs.boat <- coefsdeltabin.boat[a] * coefsdeltapos.boat
  fishlab <- switch(runsp, yft = "Yellowfin", bet = "Bigeye", alb = "Albacore")
  plot_slope_ratio(coefs.base, coefs.boat, yrpos, titl = paste("Region", runreg, fishlab, "Delta lognormal combined"))
  par(mar = c(5, 4, 1, 1))
  plot(yrpos, coefs.base, type = "l", ylab = "Relative abundance estimate", xlab = "Year", ylim = c(0, 2.5))
  lines(yrpos, coefs.boat, col = "red")
  fname2 <- paste(fname, " deltacomb", sep = "")
  savePlot(paste(fname2, ".png", sep = ""), type = "png")
  write.csv(cbind(yrbin, coefs.base, coefs.boat), file = paste(fname, ".csv", sep = ""))
  graphics.off()
}

#' Combine binomial and lognormal indices, importing from excel.
#'
#' The function combines binomial and lognormal positive indices to generate an index of relative abundance.
#' @param fnamedelta Name of the delta file to load.
#' @param fnamepos Name of the lognormal positive file to load.
#' @param runsp Species of interest.
#' @param runreg The model region, used in output file names.
#'
combine_delta_xl <- function(fnamedelta, fnamepos, runsp, runreg) {
  xl_delta <- read.csv(paste(fnamedelta, ".csv", sep = ""))
  xl_pos <- read.csv(paste(fnamepos, ".csv", sep = ""))
  pos <- match(xl_pos[, 2], xl_delta[, 2])
  coefs.boat <- xl_delta[pos, 5] * xl_pos[, 5]
  coefs.base <- xl_delta[pos, 3] * xl_pos[, 3]
  yrpos <- xl_delta[pos, 2]
  fishlab <- switch(runsp, yft = "Yellowfin", bet = "Bigeye", alb = "Albacore")
  plot_slope_ratio(coefs.base, coefs.boat, yrpos, titl = paste("Region", runreg, fishlab, "Delta lognormal combined"))
  # par(mar = c(5, 4, 1, 1)) plot(yrpos, coefs.base, type = 'l', ylab = 'Relative abundance estimate', xlab = 'Year', ylim = c(0, 2.5)) lines(yrpos,
  # coefs.boat, col = 'red')
  fname2 <- gsub("deltabin", "deltacomb", fnamedelta)
  savePlot(paste(fname2, ".png", sep = ""), type = "png")
  write.csv(cbind(yrpos, coefs.base, coefs.boat), file = paste(fname2, ".csv", sep = ""))
  graphics.off()
}

#' Combine binomial and lognormal indices, importing from excel.
#'
#' The function combines binomial and lognormal positive indices to generate an index of relative abundance.
#' @param fnamedelta Name of the delta file to load.
#' @param fnamepos Name of the lognormal positive file to load.
#' @param runsp Species of interest.
#'
combine_delta_xl_indices <- function(fnamedelta, fnamepos, runsp) {
  xl_delta <- read.csv(paste(fnamedelta, ".csv", sep = ""))
  xl_pos <- read.csv(paste(fnamedelta, ".csv", sep = ""))
  coefs.boat <- xl_delta[, 3] * xl_pos[, 3]
  fishlab <- switch(runsp, yft = "Yellowfin", bet = "Bigeye", alb = "Albacore")
  yrpos <- xl_delta[, 2]
  dev.new(noRStudioGD = TRUE)
  par(mar = c(5, 4, 1, 1))
  plot(yrpos, coefs.boat, type = "l", ylab = "Relative abundance estimate", xlab = "Year", ylim = c(0, 2.5))
  fname2 <- gsub("deltabin", "deltacomb", fnamedelta)
  savePlot(paste(fname2, ".png", sep = ""), type = "png")
  write.csv(cbind(yrpos, coefs.boat), file = paste(fname2, ".csv", sep = ""))
  graphics.off()
}

