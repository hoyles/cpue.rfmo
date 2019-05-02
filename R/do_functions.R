#' Run delta lognormal models with interactions.
#'
#' The function runs delta lognormal glm models with interactions.
#' @param dat Input dataset.
#' @param dohbf Include HBF in the models.
#' @param addboat Include vess in the models.
#' @param addcl Include clust in the models.
#' @param nhbf Number of knots in the hbf spline.
#' @param runsp Species of interest.
#' @param fname Filename for saving.
#' @param modlab Label for plots and filenames.
#' @param keepd If FALSE, delete the data from the model object before saving.
#' @param lat5xqtr Include lat5 by qtr interaction in the model.
#' @param lat5xyr Include lat5 by yr interaction in the model.
#' @param bcorr Include lognormal bias correction.
#' @param cell_areas Cell areas to use when aggregating densities across space.
#'
do_deltalogx <- function(dat, dohbf = F, addboat = F, addcl = addcl, nhbf = 3, runsp, fname, modlab, keepd = TRUE, lat5xqtr = T, lat5xyr = T, bcorr = FALSE, cell_areas) {
  datpos <- dat[dat[, runsp] > 0, ]
  fmla1.bin <- make_formula_IOx(runsp, modtype = "deltabin", dohbf = dohbf, addboat = addboat, addcl = T, nhbf = nhbf, lat5xqtr = lat5xqtr, lat5xyr = lat5xyr)
  fmla1.pos <- make_formula_IOx(runsp, modtype = "deltapos", dohbf = dohbf, addboat = addboat, addcl = T, nhbf = nhbf, lat5xqtr = lat5xqtr, lat5xyr = lat5xyr)
  fmla1.bin_ncl <- make_formula_IOx(runsp, modtype = "deltabin", dohbf = dohbf, addboat = addboat, addcl = F, nhbf = nhbf, lat5xqtr = lat5xqtr, lat5xyr = lat5xyr)
  fmla1.pos_ncl <- make_formula_IOx(runsp, modtype = "deltapos", dohbf = dohbf, addboat = addboat, addcl = F, nhbf = nhbf, lat5xqtr = lat5xqtr, lat5xyr = lat5xyr)
  fmla2.bin <- make_formula_IOx(runsp, modtype = "deltabin", dohbf = dohbf, addboat = addboat, addcl = T, nhbf = nhbf, lat5xqtr = F, lat5xyr = F)
  fmla2.pos <- make_formula_IOx(runsp, modtype = "deltapos", dohbf = dohbf, addboat = addboat, addcl = T, nhbf = nhbf, lat5xqtr = F, lat5xyr = F)
  fmla2.bin_ncl <- make_formula_IOx(runsp, modtype = "deltabin", dohbf = dohbf, addboat = addboat, addcl = F, nhbf = nhbf, lat5xqtr = F, lat5xyr = F)
  fmla2.pos_ncl <- make_formula_IOx(runsp, modtype = "deltapos", dohbf = dohbf, addboat = addboat, addcl = F, nhbf = nhbf, lat5xqtr = F, lat5xyr = F)
  datpos$.wtt <- mk_wts(datpos, wttype = "area")
  if (lu(dat$clust) > 1) {
    modelbin1 <- glm(as.formula(fmla1.bin), data = dat, family = "binomial", model = keepd)
    modelbin2 <- glm(as.formula(fmla2.bin), data = dat, family = "binomial", model = keepd)
  } else {
    modelbin1 <- glm(as.formula(fmla1.bin_ncl), data = dat, family = "binomial", model = keepd)
    modelbin2 <- glm(as.formula(fmla2.bin_ncl), data = dat, family = "binomial", model = keepd)
  }
  if (lu(datpos$clust) > 1) {
    modelpos1 <- glm(as.formula(fmla1.pos), family = "gaussian", data = datpos, weights = datpos$.wtt, model = keepd)
    modelpos2 <- glm(as.formula(fmla2.pos), family = "gaussian", data = datpos, weights = datpos$.wtt, model = keepd)
  } else {
    modelpos1 <- glm(as.formula(fmla1.pos_ncl), family = "gaussian", data = datpos, weights = datpos$.wtt, model = keepd)
    modelpos2 <- glm(as.formula(fmla2.pos_ncl), family = "gaussian", data = datpos, weights = datpos$.wtt, model = keepd)
  }
  summarize_and_store_dlx(modb1 = modelbin1, modb2 = modelbin2, modp1 = modelpos1, modp2 = modelpos2, dat = dat, datpos = datpos, fname, modlab = modlab, dohbf = dohbf, addcl = addcl, keepd = keepd, bcorr, cell_areas=cell_areas)
}

#' Run lognormal positive models with interactions.
#'
#' The function runs lognormal positive glm models with interactions.
#' @param dat Input dataset.
#' @param dohbf Include HBF in the models.
#' @param addboat Include vess in the models.
#' @param addcl Include clust in the models.
#' @param nhbf Number of knots in the hbf spline.
#' @param runsp Species of interest.
#' @param fname Filename for saving.
#' @param modlab Label for plots and filenames.
#' @param keepd If FALSE, delete the data from the model object before saving.
#' @param lat5xqtr Include lat5 by qtr interaction in the model.
#' @param lat5xyr Include lat5 by yr interaction in the model.
#' @param bcorr Include lognormal bias correction.
#' @param cell_areas Cell areas to use when aggregating densities across space.
#'
do_lognCx <- function(dat, dohbf = F, addboat = F, addcl = addcl, nhbf = 3, runsp, fname, modlab, keepd = TRUE, lat5xqtr = T, lat5xyr = T, bcorr = FALSE, cell_areas) {
  mn <- with(dat, 0.1 * mean(get(runsp)/hooks))
  fmla1 <- make_formula_IOx(runsp, modtype = "logn", dohbf = dohbf, addboat = addboat, addcl = T, nhbf = nhbf, lat5xqtr = lat5xqtr, lat5xyr = lat5xyr)
  fmla1.ncl <- make_formula_IOx(runsp, modtype = "logn", dohbf = dohbf, addboat = addboat, addcl = F, nhbf = nhbf, lat5xqtr = lat5xqtr, lat5xyr = lat5xyr)
  fmla2 <- make_formula_IOx(runsp, modtype = "logn", dohbf = dohbf, addboat = addboat, addcl = T, nhbf = nhbf, lat5xqtr = F, lat5xyr = F)
  fmla2.ncl <- make_formula_IOx(runsp, modtype = "logn", dohbf = dohbf, addboat = addboat, addcl = F, nhbf = nhbf, lat5xqtr = F, lat5xyr = F)
  dat$.wtt <- mk_wts(dat, wttype = "area")
  if (lu(dat$clust) > 1) {
    model1 <- glm(as.formula(fmla1), data = dat, family = "gaussian", weights = dat$.wtt, model = keepd)
    model2 <- glm(as.formula(fmla2), data = dat, family = "gaussian", weights = dat$.wtt, model = keepd)
  } else {
    model1 <- glm(as.formula(fmla1.ncl), data = dat, family = "gaussian", weights = dat$.wtt, model = keepd)
    model2 <- glm(as.formula(fmla2.ncl), data = dat, family = "gaussian", weights = dat$.wtt, model = keepd)
  }
  summarize_and_store_logCx(mod1 = model1, mod2 = model2, dat, fname, modlab, addcl, dohbf, keepd, mn, bcorr, cell_areas=cell_areas)
}

#' Run delta lognormal models.
#'
#' The function runs delta lognormal models.
#' @param dat Input dataset.
#' @param dohbf Include HBF in the models.
#' @param addboat Include vess in the models.
#' @param addcl Include clust in the models.
#' @param nhbf Number of knots in the hbf spline.
#' @param runsp Species of interest.
#' @param fname Filename for saving.
#' @param modlab Label for plots and filenames.
#' @param keepd If FALSE, delete the data from the model object before saving.
#' @param dohook Include hooks in the model on the RHS if TRUE.
#'
do_deltalog <- function(dat, dohbf = F, addboat = F, addcl = addcl, nhbf = 3, runsp, fname, modlab, keepd = TRUE, dohook = TRUE) {
  datpos <- dat[dat[, runsp] > 0, ]
  if (lu(dat$clust) > 1) {
    fmla.bin <- make_formula_IO(runsp, modtype = "deltabin", dohbf = dohbf, addboat = addboat, addcl = T, nhbf = nhbf, dohook = dohook)
    fmla.pos <- make_formula_IO(runsp, modtype = "deltapos", dohbf = dohbf, addboat = addboat, addcl = T, nhbf = nhbf, dohook = dohook)
    addcl <- TRUE
  } else {
    fmla.bin <- make_formula_IO(runsp, modtype = "deltabin", dohbf = dohbf, addboat = addboat, addcl = F, nhbf = nhbf, dohook = dohook)
    fmla.pos <- make_formula_IO(runsp, modtype = "deltapos", dohbf = dohbf, addboat = addboat, addcl = F, nhbf = nhbf, dohook = dohook)
    addcl <- FALSE
  }
  datpos$.wtt <- mk_wts(datpos, wttype = "area")
  modelbin <- glm(as.formula(fmla.bin), data = dat, family = "binomial", model = keepd)
  modelpos <- glm(as.formula(fmla.pos), family = "gaussian", data = datpos, weights = datpos$.wtt, model = keepd)
  summarize_and_store_dl(modb = modelbin, modp = modelpos, dat = dat, datpos = datpos, fname, modlab = modlab, dohbf = dohbf,
                         addcl = addcl, keepd = keepd)
}

# do_deltalog_sep <- function(dat, dohbf = F, addboat = F, addcl = addcl, nhbf = 3, runsp, fname, modlab, keepd = TRUE, dohook = TRUE) {
#   datpos <- dat[dat[, runsp] > 0, ]
#   a <- table(dat[,runsp]==0, dat$vessid)
#   datbin <
#   if (lu(dat$clust) > 1) {
#     fmla.bin <- make_formula_IO(runsp, modtype = "deltabin", dohbf = dohbf, addboat = addboat, addcl = T, nhbf = nhbf, dohook = dohook)
#     fmla.pos <- make_formula_IO(runsp, modtype = "deltapos", dohbf = dohbf, addboat = addboat, addcl = T, nhbf = nhbf, dohook = dohook)
#     addcl <- TRUE
#   } else {
#     fmla.bin <- make_formula_IO(runsp, modtype = "deltabin", dohbf = dohbf, addboat = addboat, addcl = F, nhbf = nhbf, dohook = dohook)
#     fmla.pos <- make_formula_IO(runsp, modtype = "deltapos", dohbf = dohbf, addboat = addboat, addcl = F, nhbf = nhbf, dohook = dohook)
#     addcl <- FALSE
#   }
#   datpos$.wtt <- mk_wts(datpos, wttype = "area")
#   modelbin <- glm(as.formula(fmla.bin), data = dat, family = "binomial", model = keepd)
#   modelpos <- glm(as.formula(fmla.pos), family = "gaussian", data = datpos, weights = datpos$.wtt, model = keepd)
#   summarize_and_store_dl(modb = modelbin, modp = modelpos, dat = dat, datpos = datpos, fname, modlab = modlab, dohbf = dohbf,
#                          addcl = addcl, keepd = keepd)
# }


#' Run standardization models.
#'
#' The function runs delta lognormal models.
#' @param runpars The 'runpars' object with all variables.
#' @param doflags The flags to include in the model runs.
#' @param regstr The name of the regional structure to be run.
#' @param maxyr Include no times after the start of this year.
#' @param do_early If TRUE, run the additional pre-1979 and post-1979 standardizations.
#' @param stdlabs The variables from the clustered dataset to make available to the standardization.
#' @param projdir Base folder for the project. The fleet and joint folders are under this one.
#' @param twlimit The earliest time limit for including Taiwanese data in the model.
#' @param jplimit Range of data to include from Japan: region and final year.
#' @param keepd Keep data in the model object if TRUE.
#' @param krlimit Range of data to include from Korea: list with first year, last year, and region.
#'
run_standardization <- function(runpars, doflags, regstr, maxyr, stdlabs, projdir, twlimit = 2005, jplimit = list(reg=4, yr=0), keepd = TRUE, krlimit=NA) {
  rp <- runpars[[regstr]]
  runsp <- rp$runsp
  addcl <- rp$addcl
  dohbf <- rp$dohbf

  if(is.null(rp$do_lognC)) do_lognC <- TRUE else do_lognC <- rp$do_lognC
  if(is.null(rp$do_deltalog)) do_deltalog <- TRUE else do_deltalog <- rp$do_deltalog

  if(is.null(rp$do_vessallyr)) do_vessallyr <- TRUE else do_vessallyr <- rp$do_vessallyr
  if(is.null(rp$do_early)) do_early <- TRUE else do_early <- rp$do_early
  if(is.null(rp$do_late))  do_late  <- TRUE else do_late  <- rp$do_late

  if(is.null(rp$ylall)) ylall <- NA else ylall <- rp$ylall
  if(is.null(rp$dat_lims)) dat_lims <- NA else dat_lims <- rp$dat_lims
  if(is.null(rp$discards)) discards <- NA else discards <- rp$discards
  if(length(doflags)==1) onefl <- doflags else onefl <- NA

  jdat <- data.frame()
  if("strsmp" %in% names(rp))  strsmp <- rp$strsmp else strsmp <- NA

  for (flag in doflags) {
    for (r in rp$doregs) {
      datfl <- paste0(projdir,flag,"/clustering/",paste(flag,regstr,r,sep = "_"),".RData")
      if(file.exists(datfl)) {
        load(datfl)
        dataset$flag <- flag
        dataset$qtr <- revtrunc(defactor(dataset$yrqtr))
        jdat <- rbind(jdat,dataset[,stdlabs])
        rm(dataset)
      }
    }
  }
  jdat <- jdat[jdat$yrqtr < maxyr,]
  jdat$vessidx <- jdat$vessid
  jdat$vessid <- paste0(jdat$flag,jdat$vessid)
  jdat$vessid <- as.factor(jdat$vessid)
  jdat <- jdat[jdat$yrqtr > twlimit | jdat$flag != "TW",]

  vars <- c("vessid","hooks","yrqtr","latlong","hbf")

  # remove data as specified in runpars, with jplimit, krlimit, and the more general dat_lims
  jdat2 <- jdat[jdat$yrqtr < jplimit$yr | !jdat$reg %in% jplimit$reg | jdat$flag != "JP",]
  if(!is.na(krlimit)) {
    jdat2 <- jdat2[(jdat2$yrqtr > krlimit$yr[1] & jdat2$yrqtr < krlimit$yr[2]) |
                     !jdat2$reg %in% krlimit$reg | jdat2$flag != "KR",]
  }
  if(!is.na(dat_lims)) {
    a <- jdat2
    for(dlrow in 1:length(dat_lims)) {
      a <- a[eval(parse(text=dat_lims[dlrow])),]
    }
    jdat2 <- a
  }

  if(!is.na(discards)) {
    jdat2 <- adjust_discards(jdat2, discards, regstr, runsp)
  }

  # Start of the loop through regions
  for (runreg in rp$doregs) {

    glmdat <- select_data_IO2(jdat2,runreg = runreg,runpars = rp, mt = "deltabin",vars = vars, yrlims = ylall, oneflag = onefl)
    if (!is.na(strsmp) & nrow(glmdat) > 60000)
      glmdat <- samp_strat_data(glmdat, strsmp)

    glmdat$.wtt   <- mk_wts(glmdat,wttype = "area")

    if (do_early) {
      glmdat5279 <- select_data_IO2(jdat2,runreg = runreg, runpars = rp, mt = "deltabin",vars = vars, yrlims=c(1952,1980), oneflag = onefl)
      if (!is.na(strsmp) & nrow(glmdat5279) > 60000)
        glmdat5279 <- samp_strat_data(glmdat5279, strsmp)

      a <- jdat2[jdat2$vessid != "JP1",]
      glmdat79nd <- select_data_IO2(a,runreg = runreg, runpars = rp, mt = "deltabin",vars = vars, yrlims=c(1979,maxyr), oneflag = onefl)
      if (!is.na(strsmp) & nrow(glmdat79nd) > 60000)
        glmdat79nd <- samp_strat_data(glmdat79nd, strsmp)
      glmdat5279$.wtt   <- mk_wts(glmdat5279,wttype="area")
      glmdat79nd$.wtt   <- mk_wts(glmdat79nd,wttype="area")
    }

  if(do_lognC) {
    fmla.oplogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = T,nhbf = 3, dohook = rp$dohook)
    fmla.oplogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = F,nhbf = 3, dohook = rp$dohook)
    fmla.boatlogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = T,nhbf = 3, dohook = rp$dohook)
    fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = F,nhbf = 3, dohook = rp$dohook)
    glmdat$mn <- with(glmdat,0.1 *  mean(get(runsp)/hooks))

    modlab = "lognC_novess_allyrs"; fname <- paste0("Joint_",regstr,"_R",runreg)
    if (lu(glmdat$clust) > 1)
    { model <- glm(as.formula(fmla.oplogn),     data = glmdat, weights = glmdat$.wtt, family = "gaussian", model = keepd);gc() } else
    { model <- glm(as.formula(fmla.oplogn_ncl), data = glmdat, weights = glmdat$.wtt, family = "gaussian", model = keepd);gc() }
    summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

    if (do_vessallyr) {
      modlab = "lognC_boat_allyrs"; fname <- paste0("Joint_",regstr,"_R",runreg)
      if (lu(glmdat$clust) > 1)
      { model <- glm(as.formula(fmla.boatlogn),    data = glmdat, weights = glmdat$.wtt, family = "gaussian", model = keepd);gc() } else
      { model <- glm(as.formula(fmla.boatlogn_ncl),data = glmdat, weights = glmdat$.wtt, family = "gaussian", model = keepd);gc() }
      summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)
    }

    if (do_early) {
      modlab="lognC_novess_5279"; fname <- paste0("Joint_",regstr,"_R",runreg)
      glmdat5279$mn <- with(glmdat5279,0.1* mean(get(runsp)/hooks))
      if(lu(glmdat5279$clust) > 1)
      { model <- glm(as.formula(fmla.oplogn),     data=glmdat5279, weights = glmdat5279$.wtt, family="gaussian");gc() } else
      { model <- glm(as.formula(fmla.oplogn_ncl), data=glmdat5279, weights = glmdat5279$.wtt, family="gaussian");gc() }
      summarize_and_store(mod=model,dat=glmdat5279,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)
    }

    if (do_late) {
      modlab="lognC_vessid_79nd"; fname <- paste0("Joint_",regstr,"_R",runreg)
      glmdat79nd$mn <- with(glmdat79nd,0.1* mean(get(runsp)/hooks))
      if(lu(glmdat79nd$clust) > 1)
      { model <- glm(as.formula(fmla.boatlogn),     data = glmdat79nd, weights = glmdat79nd$.wtt, family="gaussian");gc() } else
      { model <- glm(as.formula(fmla.boatlogn_ncl), data = glmdat79nd, weights = glmdat79nd$.wtt, family="gaussian");gc() }
      summarize_and_store(mod=model,dat=glmdat79nd,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)
    }
  }

    # delta lognormal
    if(do_deltalog) {
      modlab = "dellog_novess_allyrs"; fname <- paste0("Joint_", regstr,"_R",runreg);
      do_deltalog(dat = glmdat,dohbf = dohbf,addboat = F,addcl = addcl,nhbf = 3,runsp = runsp,
                  fname = fname,modlab = modlab, keepd = keepd, dohook = rp$dohook)

      if (do_vessallyr) {
        modlab = "dellog_boat_allyrs"; fname <- paste0("Joint_",regstr,"_R",runreg)
        do_deltalog(dat = glmdat,dohbf = dohbf,addboat = T,addcl = addcl,nhbf = 3,runsp = runsp,
                    fname = fname,modlab = modlab, keepd = keepd, dohook = rp$dohook)
      }

      if (do_early) {
        modlab="dellog_novess_5279"; fname <- paste0("Joint_",regstr,"_R",runreg)
        do_deltalog(dat=glmdat5279,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,
                    fname=fname,modlab=modlab, keepd = keepd, dohook = rp$dohook)
      }

      if (do_late) {
        modlab="dellog_vessid_79nd"; fname <- paste0("Joint_",regstr,"_R",runreg)
        do_deltalog(dat=glmdat79nd,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,
                    fname=fname,modlab=modlab, keepd = keepd, dohook = rp$dohook)
      }
    }
    graphics.off()
  }
}


#' Adjust the input data for reported rates of discarding.
#'
#' The function takes discard rates by flag, regional structure, region and year, and divides the catch by 1 / (1 - discard rate).
#' @param dat Input dataset
#' @param discards Data frame containing the discards.
#' @param regstr The current regional structure.
#' @param runsp The species of interest.
#' @return Modified dataset.
#'
adjust_discards <- function(dat, discards, regstr, runsp) {
  datkey <- paste(dat$flag, regstr, dat$reg, floor(dat$yrqtr))
  disckey <- paste(discards$flag, discards$regstr, discards$reg, discards$year)
  rate <- discards$rate[match(datkey, disckey)]
  rate[is.na(rate)] <- 0
  adj <- 1 / (1 - rate)
  dat$pre_disc <- dat[,runsp]
  dat[,runsp] <- dat[,runsp] * adj
  dat <- dat[rate < 1,]
  return(dat)
}
