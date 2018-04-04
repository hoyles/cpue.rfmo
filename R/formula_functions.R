#' Create formula object to use in the glm.
#'
#' This function creates a formula object for the glm, based on the input parameters. Developed for Japanese data in the WCPO.
#' @param runsp Species of interest.
#' @param modtype logn (lognormal constant), deltabin(binomial), deltapos (lognormal positive), or qp (quasi-Poisson).
#' @param addboat Include vess if TRUE.
#' @param splitboat Include splitvess if TRUE.
#' @param addmain Include mainline if TRUE.
#' @param addbranch Include branchline if TRUE.
#' @param addother Include other if TRUE.
#' @param addalb include alb_cat if TRUE.
#' @return Modified dataset.
#'
make_formula <- function(runsp, modtype, addboat, splitboat = F, addmain = F, addbranch = F, addother = F, addalb = F) {
  fmla <- "~ as.factor(yrqtr) + as.factor(latlong) + poly(hbf, 6)"
  modhead <- switch(modtype, logn = paste("log(", runsp, "/hooks + 0.01)"), deltabin = paste(runsp, " !=  0"), deltapos = paste("log((", runsp, ")/hooks)"), qp = runsp, propn = runsp)
  fmla <- paste(modhead, fmla)
  if (modtype %in% c("deltabin", "qp"))
    fmla <- paste(fmla, "+ ns(hooks, 6)")
  if (addboat & !splitboat)
    fmla <- paste(fmla, "+ as.factor(vessid)")
  if (addboat & splitboat)
    fmla <- paste(fmla, "+ as.factor(splitvess)")
  if (addmain)
    fmla <- paste(fmla, "+ as.factor(mainline) + as.factor(mainline):ns(hbf, 6)")
  if (addbranch)
    fmla <- paste(fmla, "+ as.factor(branchline)")
  if (addother)
    fmla <- paste(fmla, "+ as.factor(other)")
  if (addalb)
    fmla <- paste(fmla, "+ as.factor(alb_cat)")
  return(fmla)
}

#' Create formula object to use in the glm.
#'
#' This function creates a formula object for the glm, based on the input parameters. Developed for analyses in the IO.
#' @param runsp Species of interest.
#' @param modtype logn (lognormal constant), deltabin(binomial), deltapos (lognormal positive), qp (quasi-Poisson), propn (proportion, which uses runsp directly), negbin (negative binomial), or weibull (Weibull).
#' @param addboat Include vess if TRUE.
#' @param dohbf Include hbf if TRUE, with nhbf knots in spline.
#' @param splitboat Include splitvess if TRUE.
#' @param addcl Include clust if TRUE.
#' @param addpca Include PCA if TRUE.
#' @param nhbf Number of knots in hbf spline, if dohbf is TRUE.
#' @return Modified dataset.
#'
make_formula_IO <- function(runsp, modtype, addboat, dohbf = T, splitboat = F, addcl = F, addpca = NA, nhbf = 6) {
  fmla <- "~ yrqtr + latlong"
  if (dohbf)
    fmla <- paste0(fmla, " + ns(hbf, ", nhbf, ")")
  modhead <- switch(modtype, logn = paste0("log(", runsp, "/hooks + mn)"), deltabin = paste0(runsp, " !=  0"), deltapos = paste0("log((", runsp, ")/hooks)"), qp = runsp, propn = runsp, negbin = runsp, weibull = paste0("Surv(", runsp, ")"))
  fmla <- paste(modhead, fmla)
  # if (modtype %in% c('deltabin', 'qp')) fmla <- paste(fmla, '+ ns(hooks, 10)')
  fmla <- paste(fmla, "+ ns(hooks, 10)")
  if (addboat & !splitboat)
    fmla <- paste(fmla, "+ vessid")
  if (addboat & splitboat)
    fmla <- paste(fmla, "+ splitvess")
  if (addcl)
    fmla <- paste(fmla, "+ clust")
  if (!is.na(addpca))
    fmla <- paste(fmla, "+", addpca)
  # fmla <- as.formula(fmla)
  return(fmla)
}

#' Create formula object to use in the glm, with interactions.
#'
#' This function creates a formula object for the glm, based on the input parameters. Developed for analyses in the IO with interaction models.
#' @param runsp Species of interest.
#' @param modtype logn (lognormal constant), deltabin(binomial), deltapos (lognormal positive), qp (quasi-Poisson), propn (proportion, which uses runsp directly), negbin (negative binomial), or weibull (Weibull).
#' @param addboat Include vess if TRUE.
#' @param dohbf Include hbf if TRUE, with nhbf knots in spline.
#' @param splitboat Include splitvess if TRUE.
#' @param addcl Include clust if TRUE.
#' @param addpca Include PCA if TRUE.
#' @param nhbf Number of knots in hbf spline, if dohbf is TRUE.
#' @param lat5xqtr Include lat5:qtr if TRUE.
#' @param lat5xyr Include lat5:op_yr if TRUE.
#' @return Modified dataset.
#'
make_formula_IOx <- function(runsp, modtype, addboat, dohbf = T, splitboat = F, addcl = F, addpca = NA, nhbf = 6, lat5xqtr = F, lat5xyr = F) {
  fmla <- "~ yrqtr + latlong"
  if (lat5xqtr)
    fmla <- paste(fmla, "+ lat5:qtr")
  if (lat5xyr)
    fmla <- paste(fmla, "+ lat5:op_yr")
  if (dohbf)
    fmla <- paste0(fmla, " + ns(hbf, ", nhbf, ")")
  modhead <- switch(modtype, logn = paste0("log(", runsp, "/hooks + mn)"), deltabin = paste0(runsp, " !=  0"), deltapos = paste0("log((", runsp, ")/hooks)"), qp = runsp, propn = runsp, negbin = runsp, weibull = paste0("Surv(", runsp, ")"))
  fmla <- paste(modhead, fmla)
  # if (modtype %in% c('deltabin', 'qp')) fmla <- paste(fmla, '+ ns(hooks, 10)')
  fmla <- paste(fmla, "+ ns(hooks, 10)")
  if (addboat & !splitboat)
    fmla <- paste(fmla, "+ vessid")
  if (addboat & splitboat)
    fmla <- paste(fmla, "+ splitvess")
  if (addcl)
    fmla <- paste(fmla, "+ clust")
  if (!is.na(addpca))
    fmla <- paste(fmla, "+", addpca)
  return(fmla)
}

