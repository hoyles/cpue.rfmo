#' Boxplots of set characteristics by cluster.
#'
#' This function plots boxplots of set characteristics by cluster.
#' @param dat Input dataset
#' @param cl Cluster type to select from dataset.
#' @param ti Plot main title.
#' @param outL Include outliers if TRUE.
#' @param dohbf Boxplot HBF by cluster.
#' @param lat5 Use 'lat' variable if FALSE, otherwise 'lat5'.
#' @param regtype Deprecated, not used.
#' @param r Deprecated, not used.
#'
boxplots_CL <- function(dat, cl = "kmeans", ti = "", outL = T, dohbf = T, lat5 = F, regtype = regtype, r = r) {
    dev.new(30, 20)
    par(mfrow = c(2, 3), mar = c(3, 3, 3, 3), oma = c(0, 0, 2, 0))
    wd = table(dat[, cl])
    boxplot(dat$op_yr ~ dat[, cl], width = wd, main = "Year", outline = outL, pars = list(boxwex = 1))
    if (lat5) {
        boxplot(dat$lat5 ~ dat[, cl], width = wd, main = "Lat", outline = outL, pars = list(boxwex = 1))
        boxplot(dat$lon5 ~ dat[, cl], width = wd, main = "Lon", outline = outL, pars = list(boxwex = 1))
    } else {
        boxplot(dat$lat ~ dat[, cl], width = wd, main = "Lat", outline = outL, pars = list(boxwex = 1))
        boxplot(dat$lon ~ dat[, cl], width = wd, main = "Lon", outline = outL, pars = list(boxwex = 1))
    }
    boxplot(dat$hooks ~ dat[, cl], width = wd, main = "Hooks", outline = outL, pars = list(boxwex = 1))
    if (dohbf)
        boxplot(dat$hbf ~ dat[, cl], width = wd, main = "HBF", outline = outL, pars = list(boxwex = 1))
    if (!is.null(dat$op_mon)) boxplot(dat$op_mon ~ dat[, cl], width = wd, main = "Months", outline = outL, pars = list(boxwex = 1))
    title(paste(gsub("_", " ", ti), cl), outer = T, line = 1, cex.main = 1.5)
    savePlot(paste0(ti, "_boxplots_CL", cl, ".png"), type = "png")
}

#' Beanplots of set characteristics by cluster.
#'
#' This function plots beanplots of set characteristics by cluster.
#' @param dat Input dataset
#' @param cl Cluster type to select from dataset.
#' @param ti Plot main title.
#' @param outL Deprecated, not used.
#' @param dohbf Boxplot HBF by cluster.
#' @param lat5 Use 'lat' variable if FALSE, otherwise 'lat5'.
#' @param regtype Deprecated, not used.
#' @param r Deprecated, not used.
#'
boxplots_CL_bean <- function(dat, cl = "kmeans", ti = "", outL = T, dohbf = T, lat5 = F, regtype = regtype, r = r) {
    dev.new(30, 20)
    par(mfrow = c(2, 3), mar = c(3, 3, 3, 3), oma = c(0, 0, 2, 0))
    wd = table(dat[, cl])
    beanplot(dat$op_yr ~ dat[, cl], bw = 0.5, what = c(1, 1, 1, 0), ylim = c(min(dat$op_yr, na.rm = T) - 0.5, max(dat$op_yr, na.rm = T) + 0.5), log = "",
        main = "Year")
    if (lat5) {
        beanplot(dat$lat5 ~ dat[, cl], bw = 1.25, what = c(1, 1, 1, 0), ylim = c(min(dat$lat5, na.rm = T) - 2.5, max(dat$lat5, na.rm = T) + 2.5),
            log = "", main = "Lat")
        beanplot(dat$lon5 ~ dat[, cl], bw = 1.25, what = c(1, 1, 1, 0), ylim = c(min(dat$lon5, na.rm = T) - 2.5, max(dat$lon5, na.rm = T) + 2.5),
            log = "", main = "Lon")
    } else {
        beanplot(dat$lat ~ dat[, cl], bw = 0.5, what = c(1, 1, 1, 0), ylim = c(min(dat$lat, na.rm = T) - 0.5, max(dat$lat, na.rm = T) + 0.5), log = "",
            main = "Lat")
        beanplot(dat$lon ~ dat[, cl], bw = 0.5, what = c(1, 1, 1, 0), ylim = c(min(dat$lon, na.rm = T) - 0.5, max(dat$lon, na.rm = T) + 0.5), log = "",
            main = "Lon")
    }
    beanplot(dat$hooks ~ dat[, cl], bw = 50, what = c(1, 1, 1, 0), ylim = c(min(dat$hooks, na.rm = T) - 0.5, max(dat$hooks, na.rm = T) + 0.5), log = "",
        main = "Hooks")
    if (dohbf)
        beanplot(dat$hbf ~ dat[, cl], bw = 0.5, what = c(1, 1, 1, 0), ylim = c(min(dat$hbf, na.rm = T) - 0.5, max(dat$hbf, na.rm = T) + 0.5), log = "",
            main = "HBF")
    if (!is.null(dat$op_mon)) beanplot(dat$op_mon ~ dat[, cl], bw = 0.5, what = c(1, 1, 1, 0), log = "", main = "Months", ylim = c(0.5, 12.5))
    title(paste(gsub("_", " ", ti), cl), outer = T, line = 1, cex.main = 1.5)
    savePlot(paste0(ti, "_boxplots_CL", cl, "_bean.png"), type = "png")
}

#' Boxplots of PCA by set characteristics.
#'
#' This function plots beanplots of set characteristics by cluster.
#' @param dat Input dataset
#' @param nPCA Number of PCA variables to plot.
#' @param ti Plot main title.
#' @param dohbf Boxplot PCA by HBF.
#' @param lat5 Use 'lat' variable if FALSE, otherwise 'lat5'.
#' @param regtype Deprecated, not used.
#' @param r Deprecated, not used.
#'
boxplots_PCA <- function(dat, nPCA = 3, ti = "", dohbf = T, lat5 = F, regtype = "regY", r = r) {
    for (ppc in paste0("PC", 1:nPCA)) {
        pp <- with(dat, get(ppc))
        dev.new(width = 30, height = 20)
        par(mfrow = c(2, 3))
        boxplot(pp ~ yrqtr, data = dat, main = "YQ")
        if (lat5) {
            boxplot(pp ~ (floor(lat5)), data = dat, main = "LAT")
            boxplot(pp ~ (floor(lon5)), data = dat, main = "LON")
        } else {
            boxplot(pp ~ (floor(lat)), data = dat, main = "LAT")
            boxplot(pp ~ (floor(lon)), data = dat, main = "LON")
        }
        if (dohbf)
            boxplot(pp ~ (floor(hbf)), data = dat, main = "HBF")
        boxplot(pp ~ eval(100 * floor(hooks/100)), data = dat, main = "Hooks")
        boxplot(pp ~ vessid, data = dat, main = "Vessel")
        title(paste(gsub("_", " ", ti), ppc), outer = T, line = -1, cex.main = 1.5)
        savePlot(paste0(ti, "_boxplots_", ppc, ".png"), type = "png")
    }
}

#' Boxplots of species CPUE by cluster.
#'
#' This function plots boxplots of set characteristics by cluster.
#' @param dat Input dataset
#' @param cl Cluster type to select from dataset.
#' @param ti Plot main title.
#' @param outL Include outliers if TRUE.
#' @param nsp Number of species used in clustering.
#' @param regtype Deprecated, not used.
#' @param r Deprecated, not used.
#' @param allsp Vector of species to use in the cluster plots.
#'
boxplots_spCL <- function(dat, cl = "kmeans", ti = "", outL = T, nsp = 13, regtype = regtype, r = r, allsp) {
  if (nsp %in% c(5,6)) {
    dev.new(30, 20)
    par(mfrow = c(2, 3), mar = c(3, 3, 3, 3))
  }
  if (nsp %in% c(7,8)) {
    dev.new(30, 20)
    par(mfrow = c(2, 4), mar = c(3, 3, 3, 3))
  }
  if (nsp %in% 9) {
    dev.new(30, 20)
    par(mfrow = c(3, 3), mar = c(3, 3, 3, 3))
  }
  if (nsp %in% 10:12) {
    dev.new(30, 20)
    par(mfrow = c(3, 4), mar = c(3, 3, 3, 3))
  }
  if (nsp %in% 13:15) {
    dev.new(30, 20)
    par(mfrow = c(3, 5), mar = c(3, 3, 3, 3))
  }
  wd = table(dat[, cl])
    for (sp in allsp) {
        boxplot(dat[, sp]/dat$hooks ~ dat[, cl], width = wd, main = toupper(sp), outline = outL, pars = list(boxwex = 1))
    }
    title(paste(ti, cl), outer = T, line = -1, cex.main = 1.5)
    savePlot(paste0(ti, "_boxplots_spCL", cl, ".png"), type = "png")
}

#' Boxplots of species composition by cluster.
#'
#' This function plots boxplots of species composition by cluster.
#' @param dat Input dataset
#' @param cl Cluster type to select from dataset.
#' @param ti Plot main title.
#' @param outL Include outliers if TRUE.
#' @param nsp Number of species used in clustering.
#' @param regtype Deprecated, not used.
#' @param r Deprecated, not used.
#' @param allsp Vector of species to use in the cluster plots.
#'
boxplots_spCL_comp <- function(dat, cl = "kmeans", ti = "", outL = T, nsp = 13, regtype = regtype, r = r, allsp) {
  if (nsp %in% c(5,6)) {
    dev.new(30, 20)
    par(mfrow = c(2, 3), mar = c(3, 3, 3, 3))
  }
  if (nsp %in% c(7,8)) {
    dev.new(30, 20)
    par(mfrow = c(2, 4), mar = c(3, 3, 3, 3))
  }
  if (nsp %in% 9) {
    dev.new(30, 20)
    par(mfrow = c(3, 3), mar = c(3, 3, 3, 3))
  }
  if (nsp %in% 10:12) {
    dev.new(30, 20)
    par(mfrow = c(3, 4), mar = c(3, 3, 3, 3))
  }
  if (nsp %in% 13:15) {
    dev.new(30, 20)
    par(mfrow = c(3, 5), mar = c(3, 3, 3, 3))
  }
  wd = table(dat[, cl])
    for (sp in allsp) {
        boxplot(dat[, sp]/dat$Total ~ dat[, cl], width = wd, main = toupper(sp), outline = outL, pars = list(boxwex = 1), ylim = c(0, 1))
    }
    title(paste(gsub("_", " ", ti), cl), outer = T, line = -1, cex.main = 1.5)
    savePlot(paste0(ti, "_boxplots_spCLcomp", cl, ".png"), type = "png")
}

#' Boxplots of species composition by principal component.
#'
#' This function plots boxplots of species composition by principal component.
#' @param dat Input dataset
#' @param nPCA Number of principal components to plot.
#' @param ti Plot main title.
#' @param outL Include outliers if TRUE.
#' @param nsp Number of species used in clustering.
#' @param regtype Deprecated, not used.
#' @param r Deprecated, not used.
#' @param allsp Vector of species to use in the PCA plots.
#'
boxplots_spPCA <- function(dat, nPCA = 6, ti = "", outL = T, nsp = 13, regtype = "regY", r = r, allsp) {
    for (ppc in paste0("PC", 1:nPCA)) {
        pp <- with(dat, get(ppc))
        notdone <- T
        bk <- quantile(pp, seq(0, 1, length.out = 11), include.lowest = TRUE)
        if (length(bk) == length(unique(bk)))
            ppq <- cut(pp, breaks = bk) else ppq <- cut(pp, breaks = 11)
        if (nsp %in% c(5,6)) {
          dev.new(30, 20)
          par(mfrow = c(2, 3), mar = c(3, 3, 3, 3))
        }
        if (nsp %in% c(7,8)) {
          dev.new(30, 20)
          par(mfrow = c(2, 4), mar = c(3, 3, 3, 3))
        }
        if (nsp %in% 9) {
          dev.new(30, 20)
          par(mfrow = c(3, 3), mar = c(3, 3, 3, 3))
        }
        if (nsp %in% 10:12) {
          dev.new(30, 20)
          par(mfrow = c(3, 4), mar = c(3, 3, 3, 3))
        }
        if (nsp %in% 13:15) {
          dev.new(30, 20)
          par(mfrow = c(3, 5), mar = c(3, 3, 3, 3))
        }
        for (sp in allsp) {
            boxplot(dat[, sp]/dat$Total ~ ppq, main = toupper(sp), outline = outL, ylim = c(0, 1))
        }
        title(paste(gsub("_", " ", ti), ppc), outer = T, line = 1, cex.main = 1.5)
        savePlot(paste0(ti, "_boxplots_spPCA", ppc, ".png"), type = "png")
    }
}

#' Boxplots of species composition by principal component.
#'
#' This function plots boxplots of species composition by principal component.
#' @param dat Input dataset
#' @param nPCA Number of principal components to plot.
#' @param ti Plot main title.
#' @param outL Include outliers if TRUE.
#' @param nsp Number of species used in clustering.
#' @param regtype Deprecated, not used.
#' @param r Deprecated, not used.
#' @param allsp Vector of species to use in the PCA plots.
#'
boxplots_spTPCA <- function(dat, nPCA = 6, ti = "", outL = T, nsp = 13, regtype = "regY", r = r, allsp) {
    for (ppc in paste0("TPC", 1:nPCA)) {
        pp <- with(dat, get(ppc))
        ppq <- cut(pp, breaks = quantile(pp, seq(0, 1, length.out = 11), include.lowest = TRUE))
        if (nsp %in% c(5,6)) {
          dev.new(30, 20)
          par(mfrow = c(2, 3), mar = c(3, 3, 3, 3))
        }
        if (nsp %in% c(7,8)) {
          dev.new(30, 20)
          par(mfrow = c(2, 4), mar = c(3, 3, 3, 3))
        }
        if (nsp %in% 9) {
          dev.new(30, 20)
          par(mfrow = c(3, 3), mar = c(3, 3, 3, 3))
        }
        if (nsp %in% 10:12) {
          dev.new(30, 20)
          par(mfrow = c(3, 4), mar = c(3, 3, 3, 3))
        }
        if (nsp %in% 13:15) {
          dev.new(30, 20)
          par(mfrow = c(3, 5), mar = c(3, 3, 3, 3))
        }
        for (sp in allsp) {
            boxplot(dat[, sp]/dat$Total ~ ppq, main = toupper(sp), outline = outL, ylim = c(0, 1))
        }
        title(paste(gsub("_", " ", ti), ppc), outer = T, line = 1, cex.main = 1.5)
        savePlot(paste0(ti, "_boxplots_spTPCA", ppc, ".png"), type = "png")
    }
}

#' Boxplots of principal components by set characteristics.
#'
#' This function plots boxplots of principal components by set characteristics.
#' @param dat Input dataset
#' @param nPCA Number of principal components to plot.
#' @param ti Plot main title.
#' @param dohbf If TRUE boxplot by HBF.
#' @param lat5 If TRUE use lat5 instead of lat.
#' @param regtype Deprecated, not used.
#' @param r Deprecated, not used.
#'
boxplots_TPCA <- function(dat, nPCA = 6, ti = "", dohbf = T, lat5 = F, regtype = "regY", r = r) {
    for (ppc in paste0("TPC", 1:nPCA)) {
        pp <- with(dat, get(ppc))
        dev.new(20, 14)
        par(mfrow = c(2, 4), oma = c(0, 0, 2, 0))
        boxplot(pp ~ yrqtr, data = dat, main = "YQ")
        if (lat5) {
            boxplot(pp ~ (floor(lat5)), data = dat, main = "LAT")
            boxplot(pp ~ (floor(lon5)), data = dat, main = "LON")
        } else {
            boxplot(pp ~ (floor(lat)), data = dat, main = "LAT")
            boxplot(pp ~ (floor(lon)), data = dat, main = "LON")
        }
        if (dohbf)
            boxplot(pp ~ (floor(hbf)), data = dat, main = "HBF")
        boxplot(pp ~ eval(100 * floor(hooks/100)), data = dat, main = "Hooks")
        boxplot(pp ~ vessid, data = dat, main = "Vessel")
        title(paste(gsub("_", " ", ti), ppc), outer = T, line = 1, cex.main = 1.5)
        savePlot(paste0(ti, "_boxplots_", ppc, ".png"), type = "png")
    }
}

#' Maps of PCA distribution.
#'
#' This function plots maps of PCA spatial distribution.
#' @param ddd Input dataset
#' @param nPCA Number of Principal component plots.
#' @param ti Plot main title.
#' @param lat5 If TRUE indicates that data are 5x5 rather than 1x1.
#' @param regtype Deprecated, not used.
#' @param r Deprecated, not used.
#'
mapPCA <- function(ddd, nPCA = 6, ti = "", lat5 = F, regtype = "regY", r = r) {
    dev.new(20, 14)
    par(mfrow = c(2, 2))
    for (ppc in paste0("PC", 1:nPCA)) {
        pp <- with(ddd, get(ppc))
        if (lat5) {
            pcm <- with(ddd, tapply(pp, list(floor(lon5), floor(lat5)), mean))
        } else {
            pcm <- with(ddd, tapply(pp, list(floor(lon), floor(lat)), mean))
        }
        lonn <- as.numeric(dimnames(pcm)[[1]]) + 0.5
        latn <- as.numeric(dimnames(pcm)[[2]]) + 0.5
        image(lonn, latn, pcm)
        contour(lonn, latn, pcm, add = T)
        map(database = "world", add = T)
        title(ppc, line = -2, cex.main = 1.5)
    }
    title(paste(gsub("_", " ", ti)), outer = T, line = -1, cex.main = 1.5)
    savePlot(paste0(ti, "_map_PCs", ".png"), type = "png")
}

#' Maps of TCPA distribution.
#'
#' This function plots maps of TCPA distribution.
#' @param ddd Input dataset
#' @param nPCA Number of Principal component plots.
#' @param ti Plot main title.
#' @param lat5 If TRUE indicates that data are 5x5 rather than 1x1.
#' @param regtype Deprecated, not used.
#' @param r Deprecated, not used.
#'
mapTPCA <- function(ddd, nPCA = 6, ti = "", lat5 = F, regtype = "regY", r = r) {
    dev.new(20, 14)
    par(mfrow = c(2, 2))
    for (ppc in paste0("TPC", 1:nPCA)) {
        pp <- with(ddd, get(ppc))
        if (lat5) {
            pcm <- with(ddd, tapply(pp, list(floor(lon5), floor(lat5)), mean))
        } else {
            pcm <- with(ddd, tapply(pp, list(floor(lon), floor(lat)), mean))
        }
        lonn <- as.numeric(dimnames(pcm)[[1]]) + 0.5
        latn <- as.numeric(dimnames(pcm)[[2]]) + 0.5
        image(lonn, latn, pcm)
        contour(lonn, latn, pcm, add = T)
        map(database = "world", add = T)
        title(ppc, line = -2, cex.main = 1.5)
    }
    title(paste(regtype, r, ti), outer = T, line = -1, cex.main = 1.5)
    savePlot(paste0(ti, "_map_TPCs", ".png"), type = "png")
}

#' Beanplots of species composition by cluster.
#'
#' This function plots beanplots of set characteristics by cluster.
#' @param dat Input dataset
#' @param cl Cluster type to select from dataset.
#' @param ti Plot main title.
#' @param outL Include outliers if TRUE.
#' @param nsp Number of species used in clustering.
#' @param regtype Deprecated, not used.
#' @param r Deprecated, not used.
#' @param allsp Vector of species to use in the cluster plots.
#'
beanplots_spCL_comp <- function(dat, cl = "kmeans", ti = "", outL = T, nsp = 13, regtype = regtype, r = r, allsp) {
  if (nsp %in% c(5,6)) {
    dev.new(30, 20)
    par(mfrow = c(2, 3), mar = c(3, 3, 3, 3))
  }
  if (nsp %in% c(7,8)) {
    dev.new(30, 20)
        par(mfrow = c(2, 4), mar = c(3, 3, 3, 3))
    }
  if (nsp %in% 9) {
    dev.new(30, 20)
    par(mfrow = c(3, 3), mar = c(3, 3, 3, 3))
  }
  if (nsp %in% 10:12) {
    dev.new(30, 20)
    par(mfrow = c(3, 4), mar = c(3, 3, 3, 3))
  }
  if (nsp %in% 13:15) {
    dev.new(30, 20)
        par(mfrow = c(3, 5), mar = c(3, 3, 3, 3))
    }
    wd = table(dat[, cl])
    for (sp in allsp) {
        beanplot(dat[, sp]/dat$Total ~ dat[, cl], cutmin = 0, cutmax = 1, bw = 0.01, what = c(1, 1, 1, 0), ylim = c(0, 1), log = "", main = toupper(sp))
        # boxplot(dat[, sp]/dat$Total ~ dat[, cl], width = wd, main = toupper(sp), outline = outL, pars = list(boxwex = 1), ylim = c(0, 1))
    }
    title(paste(gsub("_", " ", ti), cl), outer = T, line = -1, cex.main = 1.5)
    nm <- paste0(ti, "_beanplots_spCLcomp", cl)
    savePlot(paste0(nm, ".png"), type = "png")
}

#' Maps of cluster distribution.
#'
#' This function plots maps of cluster spatial distribution.
#' @param ddd Input dataset
#' @param cl Cluster type to select from dataset.
#' @param ti Plot main title.
#' @param lat5 If TRUE indicates that data are 5x5 rather than 1x1.
#' @param regtype Deprecated, not used.
#' @param ncl Number of clusters.
#' @param r Deprecated, not used.
#'
map_clusters <- function(ddd, cl = "hclustcl", ti = "", lat5 = F, regtype = "regY", ncl, r = r) {
    if (ncl <= 4) {
        dev.new(20, 14)
        par(mfrow = c(2, 2), mar = c(3, 3, 3, 3), oma = c(0, 0, 2, 0))
    }
    if (ncl %in% c(5, 6)) {
        dev.new(20, 20)
        par(mfrow = c(3, 2), mar = c(3, 3, 3, 3), oma = c(0, 0, 2, 0))
    }
    for (clx in 1:ncl) {
        if (lat5) {
            add5 = 2.5
            lo <- floor(ddd$lon5)
            la <- floor(ddd$lat5)
        } else {
            add5 = 0.5
            lo <- floor(ddd$lon)
            la <- floor(ddd$lat)
        }
        lo <- factor(lo, levels = seq(min(lo, na.rm = T), max(lo, na.rm = T), 2 * add5))
        la <- factor(la, levels = seq(min(la, na.rm = T), max(la, na.rm = T), 2 * add5))
        clm <- tapply(ddd[, cl] == clx, list(lo, la), mean)
        lonn <- as.numeric(levels(lo))
        latn <- as.numeric(levels(la))
        ylm <- range(latn)
        ylm[1] <- max(ylm[1], -50)
        image(lonn, latn, clm, ylim = ylm)
        contour(lonn, latn, clm, add = T)
        map(database = "world", add = T, fill = T)
        title(paste(cl, clx), line = 0.5, cex.main = 1.5)
    }
    title(paste(gsub("_", " ", ti), "cluster map"), outer = T, line = -2, cex.main = 1.5)
    savePlot(paste0(ti, "_mapclust_", cl, ".png"), type = "png")
}

# 'Plot the mean catch per year of each species by region.
#'
#' Use this plot when deciding which species to cluster.
#' @param indat Input dataset
#' @param reg_struc The name of the regional structure to be used.
#' @param splist Vector of variable names to include in the plot.
#' @param flag Flag to identify the fleet, used in the filename of the plot.
#' @param mfr The par(mfrow) variable used to define the figure layout. Defaults to c(5,3).
#'
plot_spfreqyq <- function(indat, reg_struc, splist, flag, mfr = c(5,3)){
  doreg <- sort(unique(indat[,reg_struc]))
  for (r in doreg) {
    dev.new(15,12);
    par(mfrow = mfr, mar = c(3,4,2,1), oma = c(0,0,2,0))
    a <- indat[indat[,reg_struc] == r,]
    for (sp in splist) plot(sort(unique(a$yrqtr)),tapply(a[,sp], a$yrqtr, mean), main = toupper(sp), ylab = "Mean catch (mt)", pch = 19, cex.axis = 1.3, cex.lab = 1.3)
    title(paste(reg_struc, "Region", r ), outer = TRUE)
    savePlot(filename = paste("spfreq", flag, reg_struc, "R", r, "allyrs", sep = "_"), type = "png")
  }
}

