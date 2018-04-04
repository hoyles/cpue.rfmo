#' Make newdat object and predict responses from the glm.
#'
#' Function to make newdat object and predict responses from the glm.
#' @param model GLM object to predict for.
#' @param datx Original dataset input to the glm.
#' @return List object comprising newdat (new dataset), predresp (predicted repsonses) and predterms (individual components of the response).
#'
make_newdat <- function(model, datx) {
  vessidx <- Mode(datx$vessid)
  latlongx <- Mode(datx$latlong)
  newdat <- expand.grid(yrqtr = sort(unique(datx$yrqtr)), vessid = vessidx, latlong = latlongx, hooks = median(datx$hooks), hbf = median(datx$hbf))
  predresp <- predict(model, newdata = newdat, type = "response", se.fit = T)
  predterms <- predict(model, newdata = newdat, type = "terms", se.fit = T)
  return(list(newdat = newdat, predresp = predresp, predterms = predterms))
}

#' Make newdat object and predict responses from the glm.
#'
#' Function to make newdat object and predict responses from the glm. Originally developed for Japanese data in the IO.
#' @param model GLM object to predict for.
#' @param datx Original dataset input to the glm.
#' @return List object comprising newdat (new dataset), predresp (predicted responses) and predterms (individual components of the response).
#'
make_newdat_JP <- function(model, datx) {
  vessidx <- Mode(datx$vessid)
  latlongx <- Mode(datx$latlong)
  newdat <- expand.grid(yrqtr = sort(unique(datx$yrqtr)), vessid = vessidx, latlong = latlongx, hooks = median(datx$hooks), moon = 0.5, hbf = median(datx$hbf,
                                                                                                                                                     na.rm = T))
  predresp <- predict(model, newdata = newdat, type = "response", se.fit = T)
  predterms <- predict(model, newdata = newdat, type = "terms", se.fit = T)
  return(list(newdat = newdat, predresp = predresp, predterms = predterms))
}

#' Make newdat object and predict responses from the glm.
#'
#' Function to make newdat object and predict responses from the glm. Originally developed for Taiwanese data in the IO.
#' @param model GLM object to predict for.
#' @param datx Original dataset input to the glm.
#' @return List object comprising newdat (new dataset), predresp (predicted responses) and predterms (individual components of the response).
#'
make_newdat_TW <- function(model, datx) {
  vessidx <- Mode(datx$vessid)
  latlongx <- Mode(datx$latlong)
  newdat <- expand.grid(yrqtr = sort(unique(datx$yrqtr)), vessid = vessidx, latlong = latlongx, hooks = median(datx$hooks), bt1 = sort(unique(datx$bt1))[1], bt2 = sort(unique(datx$bt2))[1], bt3 = sort(unique(datx$bt3))[1], bt4 = sort(unique(datx$bt4))[1], bt5 = sort(unique(datx$bt5))[1], moon = 0.5)
  predresp <- predict(model, newdata = newdat, type = "response", se.fit = T)
  predterms <- predict(model, newdata = newdat, type = "terms", se.fit = T)
  return(list(newdat = newdat, predresp = predresp, predterms = predterms))
}

#' Make newdat object.
#'
#' Function to make newdat object and predict responses from the glm with time area interactions. Originally developed for joint analyses in the IO.
#' @param mod GLM object to predict for.
#' @param dat Original dataset input to the glm.
#' @return newdat.
#'
make_newdat_x <- function(mod, dat) {
  vessidx <- Mode(dat$vessid)
  latlongx <- Mode(dat$latlong)
  ndtxt <- "expand.grid("
  if (isTRUE(grep("clust", mod$formula) >= 1))
    ndtxt <- paste0(ndtxt, "clust=Mode(dat$clust),")
  if (isTRUE(grep("hbf", mod$formula) >= 1))
    ndtxt <- paste0(ndtxt, "hbf=median(dat$hbf),")
  if (isTRUE(grep("lat5", mod$formula) >= 1))
    ndtxt <- paste0(ndtxt, "latlong=sort(unique(dat$latlong)),")
  ndtxt <- paste0(ndtxt, "yrqtr=sort(unique(dat$yrqtr)),hooks=median(dat$hooks),vessid=vessidx)")
  newdat <- eval(parse(text = ndtxt))
  yqloc <- match(newdat$yrqtr, dat$yrqtr)
  newdat$op_yr <- dat[yqloc, ]$op_yr
  newdat$qtr <- dat[yqloc, ]$qtr
  newdat$lat5 <- dat[match(newdat$latlong, dat$latlong), ]$lat5
  return(newdat)
}

#' Make newdat object and predict responses from the glm.
#'
#' Function to make newdat object and predict responses from the glm.
#' @param model GLM object to predict for.
#' @param datx Original dataset input to the glm.
#' @return List object comprising newdat (new dataset), predresp (predicted responses) and predterms (individual components of the response).
#'
make_newdat2 <- function(model, datx) {
  vessidx <- Mode(datx$vessid)
  latlongx <- Mode(datx$latlong)
  newdat <- expand.grid(yrqtr = sort(unique(datx$yrqtr)), vessid = vessidx, latlong = latlongx, hooks = median(datx$hooks), hbf = median(datx$hbf),
                        moon = 0.5)
  predresp <- predict(model, newdata = newdat, type = "response", se.fit = T)
  predterms <- predict(model, newdata = newdat, type = "terms", se.fit = T)
  return(list(newdat = newdat, predresp = predresp, predterms = predterms))
}

#' Make newdat object and predict responses from the glm.
#'
#' Function to make newdat object and predict responses from the glm.
#' @param model GLM object to predict for.
#' @param datx Original dataset input to the glm.
#' @return List object comprising newdat (new dataset), predresp (predicted responses) and predterms (individual components of the response).
#'
make_newdat3 <- function(model, datx) {
  vessidx <- Mode(datx$vessid)
  latlongx <- Mode(datx$latlong)
  nms <- names(datx)
  if ("hbf" %in% nms & "clust" %in% nms)
    newdat <- expand.grid(yrqtr = sort(unique(datx$yrqtr)), vessid = vessidx, latlong = latlongx, hooks = median(datx$hooks), hbf = median(datx$hbf), clust = Mode(datx$clust))
  if ("hbf" %in% nms & !"clust" %in% nms)
    newdat <- expand.grid(yrqtr = sort(unique(datx$yrqtr)), vessid = vessidx, latlong = latlongx, hooks = median(datx$hooks), hbf = median(datx$hbf))
  if (!"hbf" %in% nms & "clust" %in% nms)
    newdat <- expand.grid(yrqtr = sort(unique(datx$yrqtr)), vessid = vessidx, latlong = latlongx, hooks = median(datx$hooks), clust = Mode(datx$clust))
  if (!"hbf" %in% nms & !"clust" %in% nms)
    newdat <- expand.grid(yrqtr = sort(unique(datx$yrqtr)), vessid = vessidx, latlong = latlongx, hooks = median(datx$hooks))
  predresp <- predict(model, newdata = newdat, type = "response", se.fit = T)
  predterms <- predict(model, newdata = newdat, type = "terms", se.fit = T)
  return(list(newdat = newdat, predresp = predresp, predterms = predterms))
}

#' Make newdat object, generalised.
#'
#' Function to make newdat object with a generalised function applicable to all input datasets.
#' @param dat Original dataset input to the glm.
#' @return newdat.
#'
get_base_newdat <- function(dat) {
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
  return(newdat)
}

