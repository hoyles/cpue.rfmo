#' Summarize and store GLM results.
#'
#' The function summarizes and stores the reuslts of a glm.
#' @param mod The model result file from the glm.
#' @param dat The dataset that was input to the glm.
#' @param fname File name used for saving outputs.
#' @param modlab Model label used in plots and for output filenames.
#' @param dohbf Include hbf if TRUE.
#' @param addcl Use clusters if TRUE.
#' @param docl Use clusters if TRUE. Deprecated.
#' @param keepd Remove data from model object unless TRUE.
#'
summarize_and_store <- function(mod, dat, fname, modlab, dohbf = dohbf, addcl=addcl, docl=docl, keepd = TRUE) {
  coefs <- get.coefs(mod)
  dev.new()
  par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))  # plot diagnostics
  plotdiags(mod$residuals, ti = paste(fname, modlab))
  savePlot(paste(fname, modlab, "diags", sep = "_"), type = "png")
  if (is.null(mod$xlevels$vessid))
    dovess <- FALSE else dovess <- TRUE
  plot_effects_IO(mod, indat = dat, dovess = dovess, addcl = addcl, dohbf = dohbf)
  savePlot(paste0(fname, "_", modlab, "_effects.png"), type = "png")
  nd <- make_newdat3(model = mod, datx = dat)
  save(nd, file = paste0(fname, "_", modlab, "_predictions.RData"))
  summ <- summary(mod)
  save(summ, file = paste0(fname, "_", modlab, "_summary.RData"))
  if (!keepd)
    mod$data <- NULL
  save(mod, file = paste0(fname, "_", modlab, "_model.RData"))
  gc()
  graphics.off()
}

#' Summarize and store GLM results for delta lognormal.
#'
#' The function summarizes and stores the reuslts of both components of a delta lognormal standardization
#' @param modb The model result file from the binomial glm.
#' @param modp The model result file from the lognormal positive glm.
#' @param dat The dataset that was input to the binomial glm.
#' @param datpos The dataset that was input to the lognormal positive glm.
#' @param fname File name used for saving outputs.
#' @param modlab Model label used in plots and for output filenames.
#' @param dohbf Include hbf if TRUE.
#' @param addcl Use clusters if TRUE.
#' @param keepd Remove data from model object unless TRUE.
#'
summarize_and_store_dl <- function(modb, modp, dat, datpos, fname, modlab, dohbf = dohbf, addcl = addcl, keepd = TRUE) {
  abin <- length(unique(dat$yrqtr))
  apos <- length(unique(datpos$yrqtr))
  summodb <- summary(modb)
  summodp <- summary(modp)
  save(summodb, summodp, file = paste(fname, "_", modlab, " summary_delta.RData", sep = ""))

  vessidx <- Mode(dat$vessid)
  latlongx <- Mode(dat$latlong)
  if ("clust" %in% names(dat))
    newdat <- expand.grid(yrqtr = sort(unique(dat$yrqtr)), latlong = latlongx, hooks = median(dat$hooks), vessid = vessidx, clust = Mode(dat$clust))
  if (!"clust" %in% names(dat))
    newdat <- expand.grid(yrqtr = sort(unique(dat$yrqtr)), latlong = latlongx, hooks = median(dat$hooks), vessid = vessidx)
  if (dohbf & "clust" %in% names(dat))
    newdat <- expand.grid(yrqtr = sort(unique(dat$yrqtr)), latlong = latlongx, hooks = median(dat$hooks), vessid = vessidx, clust = Mode(dat$clust),
                          hbf = median(dat$hbf))
  if (dohbf & !"clust" %in% names(dat))
    newdat <- expand.grid(yrqtr = sort(unique(dat$yrqtr)), latlong = latlongx, hooks = median(dat$hooks), vessid = vessidx, hbf = median(dat$hbf))
  # assign('fmla.bin', as.formula(fmla.bin), envir = .GlobalEnv)

  predresp <- predict.glm(modb, newdata = newdat, type = "response", se.fit = T)
  predterms <- predict.glm(modb, newdata = newdat, type = "terms", se.fit = T)
  mn <- logit(mean(modb$y))
  a <- predresp$fit - mean(predresp$fit) + mn
  predresp$fit2 <- inv.logit(a)
  ndbin = list(newdat = newdat, predresp = predresp, predterms = predterms)
  save(ndbin, file = paste0(fname, "_bin_", modlab, "_predictions.RData"))

  vessidx <- Mode(datpos$vessid)
  latlongx <- Mode(datpos$latlong)
  if (!dohbf & !"clust" %in% names(dat))
    newdat <- expand.grid(yrqtr = sort(unique(datpos$yrqtr)), latlong = latlongx, hooks = median(datpos$hooks), vessid = vessidx)
  if (!dohbf & "clust" %in% names(dat))
    newdat <- expand.grid(yrqtr = sort(unique(datpos$yrqtr)), latlong = latlongx, hooks = median(datpos$hooks), vessid = vessidx, clust = Mode(datpos$clust))
  if (dohbf & "clust" %in% names(dat))
    newdat <- expand.grid(yrqtr = sort(unique(datpos$yrqtr)), latlong = latlongx, hooks = median(datpos$hooks), vessid = vessidx, clust = Mode(datpos$clust),
                          hbf = median(datpos$hbf))
  if (dohbf & !"clust" %in% names(dat))
    newdat <- expand.grid(yrqtr = sort(unique(datpos$yrqtr)), latlong = latlongx, hooks = median(datpos$hooks), vessid = vessidx, hbf = median(datpos$hbf))
  predresp <- predict(modp, newdata = newdat, type = "response", se.fit = T)
  predterms <- predict(modp, newdata = newdat, type = "terms", se.fit = T)
  ndpos = list(newdat = newdat, predresp = predresp, predterms = predterms)
  save(ndpos, file = paste0(fname, "_pos_", modlab, "_predictions.RData"))

  pbin <- ndbin$predresp$fit2
  ppos <- ndpos$predresp$fit
  a <- na.omit(match(ndbin$newdat$yrqtr, ndpos$newdat$yrqtr))  # make indices
  pcoefs <- pbin[a] * exp(ppos)
  pcoefs <- pcoefs/mean(pcoefs)

  coefs.bin <- get.bin.coefs(modb, abin, dat)
  coefs.pos <- get.coefs(modp)
  dev.new()
  par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))  # plot diagnostics
  plotdiags(modp$residuals, ti = paste(fname, modlab))
  savePlot(paste(fname, modlab, "diags.png", sep = "_"), type = "png")
  if (is.null(modb$xlevels$vessid))
    dovess <- FALSE else dovess <- TRUE
  plot_effects_IO(modb, indat = dat, dovess = dovess, addcl = addcl, dohbf = dohbf)
  savePlot(paste0(fname, "_", modlab, "_bin_effects.png"), type = "png")
  plot_effects_IO(modp, indat = datpos, dovess = dovess, addcl = addcl, dohbf = dohbf)
  savePlot(paste0(fname, "_", modlab, "_pos_effects.png"), type = "png")

  mn <- logit(mean(modb$y))
  opyr <- sort(unique(datpos$yrqtr))
  a <- match(names(coefs.pos), names(coefs.bin))  # make indices
  coefs <- coefs.bin[a] * coefs.pos
  save(coefs.bin, coefs.pos, coefs, pcoefs, file = paste(fname, "_", modlab, "_indices.RData", sep = ""))
  if (!keepd) {
    modb$data <- NULL
    modp$data <- NULL
  }
  save(modb, modp, file = paste(fname, "_", modlab, " moddelta.RData", sep = ""))
  gc()
  graphics.off()
}

#' Summarize and store GLM results for delta lognormal, with time area interactions.
#'
#' The function summarizes and stores the results of all the components of a delta lognormal standardization, with time area interactions.
#' @param modb1 The model result file from the binomial glm with interactions.
#' @param modb2 The model result file from the binomial glm without interactions.
#' @param modp1 The model result file from the lognormal positive glm with interactions.
#' @param modp2 The model result file from the lognormal positive glm without interactions.
#' @param dat The dataset that was input to the binomial glm.
#' @param datpos The dataset that was input to the lognormal positive glm.
#' @param fname File name used for saving outputs.
#' @param modlab Model label used in plots and for output filenames.
#' @param dohbf Include hbf if TRUE.
#' @param addcl Use clusters if TRUE.
#' @param keepd Remove data from model object unless TRUE.
#' @param bcorr Use lognormal bias-correction if TRUE.
#' @param cell_areas Cell areas to use when aggregating densities across space.
#'
summarize_and_store_dlx <- function(modb1, modb2, modp1, modp2, dat, datpos, fname, modlab, dohbf = dohbf, addcl = addcl, keepd = TRUE, bcorr, cell_areas) {
  abin <- length(unique(dat$yrqtr))
  apos <- length(unique(datpos$yrqtr))
  summodb1 <- summary(modb1)
  summodp1 <- summary(modp1)
  summodb2 <- summary(modb2)
  summodp2 <- summary(modp2)
  save(summodb1, summodp1, summodb2, summodp2, file = paste(fname, "_", modlab, " summary_deltax.RData", sep = ""))

  # make data bin
  newdat <- make_newdat_x(modb1, dat)

  # predict responses bin
  predrespb1 <- predict.glm(modb1, newdata = newdat, type = "response", se.fit = T)
  predrespb2 <- predict.glm(modb2, newdata = newdat, type = "response", se.fit = T)
  predtermsb1 <- predict.glm(modb1, newdata = newdat, type = "terms", se.fit = T)
  predtermsb2 <- predict.glm(modb2, newdata = newdat, type = "terms", se.fit = T)
  ndb1 <- cbind(newdat, fit = predrespb1$fit, sd = predrespb1$se.fit, sdt = predtermsb1$se.fit)
  ndb2 <- cbind(newdat, fit = predrespb2$fit, sd = predrespb2$se.fit, sdt = predtermsb2$se.fit)

  # Substitute when missing values in interaction run
  loc <- is.na(ndb1$fit)
  ndb1[loc, ]$fit <- ndb2[loc, ]$fit
  ndb1[loc, ]$sd <- ndb2[loc, ]$sd
  ndb1[loc, ]$sdt <- ndb2[loc, ]$sdt
  # Make indices
  mnb <- logit(mean(modb1$y))
  a1 <- ndb1$fit - mean(ndb1$fit) + mnb
  a2 <- ndb2$fit - mean(ndb2$fit) + mnb
  ndb1$fit2 <- inv.logit(a1)
  ndb2$fit2 <- inv.logit(a2)
  ndbin1 = list(newdat = ndb1, predresp = predrespb1, predterms = predtermsb1)
  ndbin2 = list(newdat = ndb2, predresp = predrespb2, predterms = predtermsb2)
  save(ndbin1, ndbin2, file = paste0(fname, "_bin_", modlab, "_predictions.RData"))

  # make data pos
  newdat <- make_newdat_x(modp1, datpos)

  # predict responses pos
  predrespp1 <- predict.glm(modp1, newdata = newdat, type = "response", se.fit = T)
  predrespp2 <- predict.glm(modp2, newdata = newdat, type = "response", se.fit = T)
  predtermsp1 <- predict.glm(modp1, newdata = newdat, type = "terms", se.fit = T)
  predtermsp2 <- predict.glm(modp2, newdata = newdat, type = "terms", se.fit = T)
  ndp1 <- cbind(newdat, fit = predrespp1$fit, sd = predrespp1$se.fit, sdt = predtermsp1$se.fit)
  ndp2 <- cbind(newdat, fit = predrespp2$fit, sd = predrespp2$se.fit, sdt = predtermsp2$se.fit)

  # Substitute pos when missing values in interaction run
  loc <- is.na(ndp1$fit)
  ndp1[loc, ]$fit <- ndp2[loc, ]$fit
  ndp1[loc, ]$sd <- ndp2[loc, ]$sd
  ndp1[loc, ]$sdt <- ndp2[loc, ]$sdt
  # Make indices
  ndp1$fit2 <- exp(ndp1$fit)
  ndp2$fit2 <- exp(ndp2$fit)
  ndpos1 = list(newdat = ndp1, predresp = predrespp1, predterms = predtermsp1)
  ndpos2 = list(newdat = ndp2, predresp = predrespp2, predterms = predtermsp2)
  save(ndpos1, ndpos2, file = paste0(fname, "_pos_", modlab, "_predictions.RData"))

  coefs.bin1 <- get.bincoefsx(ndb1, cell_areas)
  coefs.bin2 <- get.bincoefsx(ndb2, cell_areas)
  mn <- logit(mean(modb1$y))
  coefs.pos1 <- get.poscoefsx(ndp1, bcorr = bcorr, cell_areas)
  coefs.pos2 <- get.poscoefsx(ndp2, bcorr = bcorr, cell_areas)

  a <- na.omit(match(su(ndb1$yrqtr), su(ndp1$yrqtr)))  # make indices
  coefs1 <- coefs.bin1[a] * coefs.pos1
  coefs2 <- coefs.bin2[a] * coefs.pos2

  dev.new(width = 12, height = 14)
  par(mfrow = c(2, 1), mar = c(4.2, 5, 2, 2))
  yq1 <- as.numeric(names(coefs1))
  yq2 <- as.numeric(names(coefs2))
  yqall <- seq(min(yq1), max(yq1), 0.25)
  plot(yqall, coefs1[match(yqall, yq1)], type = "l", ylim = c(0, 4), xlab = "Year - quarter", ylab = "Index")
  lines(yqall, coefs2[match(yqall, yq2)], col = 2)
  legend("topright", legend = c("Area x time", "Area + time"), lty = 1, col = 1:2)
  plot(yqall, coefs1[match(yqall, yq1)]/coefs2[match(yqall, yq1)], type = "p", ylim = c(0, 3), xlab = "Year - quarter", ylab = "Ratio of models, area x time / area + time")
  savePlot(paste(fname, modlab, "compare_types", sep = "_"), type = "png")

  dev.new()
  par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))  # plot diagnostics
  plotdiags(modp1$residuals, ti = paste(fname, modlab, "area intx"))
  savePlot(paste(fname, modlab, "diags_intx.png", sep = "_"), type = "png")
  plotdiags(modp2$residuals, ti = paste(fname, modlab))
  savePlot(paste(fname, modlab, "diags.png", sep = "_"), type = "png")

  if (is.null(modb1$xlevels$vessid))
    dovess <- FALSE else dovess <- TRUE
  plot_effects_IO(modb1, indat = dat, dovess = dovess, addcl = addcl, dohbf = dohbf)
  savePlot(paste0(fname, "_", modlab, "_binx_effects.png"), type = "png")
  plot_effects_IO(modp1, indat = datpos, dovess = dovess, addcl = addcl, dohbf = dohbf)
  savePlot(paste0(fname, "_", modlab, "_posx_effects.png"), type = "png")
  plot_effects_IO(modb2, indat = dat, dovess = dovess, addcl = addcl, dohbf = dohbf)
  savePlot(paste0(fname, "_", modlab, "_bin_effects.png"), type = "png")
  plot_effects_IO(modp2, indat = datpos, dovess = dovess, addcl = addcl, dohbf = dohbf)
  savePlot(paste0(fname, "_", modlab, "_pos_effects.png"), type = "png")

  if (!keepd) {
    modb1$data <- NULL
    modb2$data <- NULL
    modp1$data <- NULL
    modp2$data <- NULL
  }
  save(modb1, modp1, modb2, modp2, file = paste(fname, "_", modlab, " moddelta.RData", sep = ""))
  gc()
  graphics.off()
}

#' Summarize and store GLM results for lognormal constant glm, with time area interactions.
#'
#' The function summarizes and stores the results of all the components of a lognormal constant standardization, with time area interactions.
#' @param mod1 The model result file from the lognormal constant glm with interactions.
#' @param mod2 The model result file from the lognormal constant glm without interactions.
#' @param dat The dataset that was input to the glm.
#' @param fname File name used for saving outputs.
#' @param modlab Model label used in plots and for output filenames.
#' @param dohbf Include hbf if TRUE.
#' @param addcl Use clusters if TRUE.
#' @param keepd Remove data from model object unless TRUE.
#' @param mn Mean value used as the constant.
#' @param bcorr Use lognormal bias-correction if TRUE.
#' @param cell_areas Cell areas to use when aggregating densities across space.
#'
summarize_and_store_logCx <- function(mod1, mod2, dat, fname, modlab, addcl = addcl, dohbf = dohbf, keepd = TRUE, mn, bcorr = FALSE, cell_areas) {
  # mod1=model1, mod2=model2, dat, fname, modlab, addcl, dohbf, keepd, mn, bcorr
  # mod1=model1;mod2=model2;fname;modlab=modlab;dohbf=dohbf;addcl=addcl; keepd = keepd;bcorr=F
  adat <- length(unique(dat$yrqtr))
  summ1 <- summary(mod1)
  summ2 <- summary(mod2)
  save(summ1, summ2, file = paste0(fname, "_", modlab, "_summary.RData"))

  vessidx <- Mode(dat$vessid)
  latlongx <- Mode(dat$latlong)
  ndtxt <- "expand.grid("
  if (isTRUE(grep("clust", mod1$formula) >= 1))
    ndtxt <- paste0(ndtxt, "clust=Mode(dat$clust),")
  if (dohbf)
    ndtxt <- paste0(ndtxt, "hbf=median(dat$hbf),")
  if (isTRUE(grep("lat5", mod1$formula) >= 1))
    ndtxt <- paste0(ndtxt, "latlong=sort(unique(dat$latlong)),")
  ndtxt <- paste0(ndtxt, "yrqtr=sort(unique(dat$yrqtr)),hooks=median(dat$hooks),vessid=vessidx)")
  newdat <- eval(parse(text = ndtxt))
  yqloc <- match(newdat$yrqtr, dat$yrqtr)
  newdat$op_yr <- dat[yqloc, ]$op_yr
  newdat$qtr <- dat[yqloc, ]$qtr
  newdat$lat5 <- dat[match(newdat$latlong, dat$latlong), ]$lat5

  pred1resp <- predict(mod1, newdata = newdat, type = "response", se.fit = T)
  pred1terms <- predict(mod1, newdata = newdat, type = "terms", se.fit = T)
  nd1 <- cbind(newdat, fit = pred1resp$fit, sd = pred1resp$se.fit)

  pred2resp <- predict(mod2, newdata = newdat, type = "response", se.fit = T)
  nd2 <- cbind(newdat, fit = pred2resp$fit, sd = pred2resp$se.fit)
  pred2terms <- predict(mod2, newdata = newdat, type = "terms", se.fit = T)
  ndx <- nd1
  loc <- is.na(ndx$fit)
  ndx[loc, ]$fit <- nd2[loc, ]$fit
  ndx[loc, ]$sd <- nd2[loc, ]$sd
  res = list(nd1 = nd1, nd2 = nd2, pred1terms = pred1terms, pred2terms = pred2terms)
  save(res, file = paste0(fname, "_", modlab, "_predictions.RData"))

  coefsx <- get.coefsx(ndx, mn, bcorr = bcorr, cell_areas)
  coefs2 <- get.coefsx(nd2, mn, bcorr = bcorr, cell_areas)
  dev.new(width = 12, height = 14)
  par(mfrow = c(2, 1), mar = c(4.2, 5, 2, 2))
  yqx <- as.numeric(names(coefsx))
  yq2 <- as.numeric(names(coefs2))
  yqall <- seq(min(yqx), max(yqx), 0.25)
  plot(yqall, coefsx[match(yqall, yqx)], type = "l", ylim = c(0, 4), xlab = "Year - quarter", ylab = "Index")
  lines(yqall, coefs2[match(yqall, yq2)], col = 2)
  legend("topright", legend = c("Area x time", "Area + time"), lty = 1, col = 1:2)
  plot(yqall, coefsx[match(yqall, yqx)]/coefs2[match(yqall, yqx)], type = "p", ylim = c(0, 3), xlab = "Year - quarter", ylab = "Ratio of models, area x time / area + time")
  savePlot(paste(fname, modlab, "compare_types", sep = "_"), type = "png")

  dev.new()
  par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))  # plot diagnostics
  plotdiags(mod1$residuals, ti = paste(fname, modlab))
  savePlot(paste(fname, modlab, "diags1", sep = "_"), type = "png")
  dev.new()
  par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))  # plot diagnostics
  plotdiags(mod2$residuals, ti = paste(fname, modlab))
  savePlot(paste(fname, modlab, "diags2", sep = "_"), type = "png")
  if (is.null(mod1$xlevels$vessid))
    dovess <- FALSE else dovess <- TRUE

  if (is.null(mod1$xlevels$vessid))
    dovess <- FALSE else dovess <- TRUE
  plot_effects_IO(mod1, indat = dat, dovess = dovess, addcl = addcl, dohbf = dohbf)
  savePlot(paste0(fname, "_", modlab, "_logCx_effects.png"), type = "png")
  plot_effects_IO(mod2, indat = dat, dovess = dovess, addcl = addcl, dohbf = dohbf)
  savePlot(paste0(fname, "_", modlab, "_logC_effects.png"), type = "png")

  # plot_effects_IOx(mod1,indat=dat,dovess,dohbf=dohbf); savePlot(paste0(fname,'_',modlab,'_effects.png'),type='png')
  if (!keepd)
    mod1$data <- mod2$data <- NULL
  save(mod1, mod2, file = paste0(fname, "_", modlab, "_model.RData"))
  gc()
  graphics.off()
}

#' Extract and save summary of GLM results from model object.
#'
#' The function extacts and saves the summary of the results from a glm object.
#' @param mod The model object.
#' @param fname The file name to save to.
#'
storesumm <- function(mod, fname) {
  summry <- summary(mod)
  save(summry, file = paste(fname, ".RData"))
  a <- capture.output(summry)
  cat(a, file = paste(fname, ".txt"), sep = "\n", append = F)
}

