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
#'
do_deltalogx <- function(dat, dohbf = F, addboat = F, addcl = addcl, nhbf = 3, runsp, fname, modlab, keepd = TRUE, lat5xqtr = T, lat5xyr = T, bcorr = FALSE) {
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
  summarize_and_store_dlx(modb1 = modelbin1, modb2 = modelbin2, modp1 = modelpos1, modp2 = modelpos2, dat = dat, datpos = datpos, fname, modlab = modlab, dohbf = dohbf, addcl = addcl, keepd = keepd, bcorr, cell_areas)
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
#'
do_lognCx <- function(dat, dohbf = F, addboat = F, addcl = addcl, nhbf = 3, runsp, fname, modlab, keepd = TRUE, lat5xqtr = T, lat5xyr = T, bcorr = FALSE) {
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
  summarize_and_store_logCx(mod1 = model1, mod2 = model2, dat, fname, modlab, addcl, dohbf, keepd, mn, bcorr, cell_areas)
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
do_deltalog <- function(dat, dohbf = F, addboat = F, addcl = addcl, nhbf = 3, runsp, fname, modlab, keepd = TRUE, dohook = dohook) {
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

