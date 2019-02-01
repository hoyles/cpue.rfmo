#' Generate clusters, PCs, and associated figures
#'
#' @description Top level function that calls functions to make clusters and principal components (PCA), and to plot the results, and associates clusters and PCs with each row of the original dataset.
#' @param r The region number.
#' @param ddd The dataset to be clustered.
#' @param allsp Vector of species codes to use in catch composition.
#' @param allabs Vector of variable names.
#' @param regtype Label to identify which regional structure to use for region numbers.
#' @param ncl Number of clusters to use.
#' @param plotPCA Generate PCA plots if TRUE.
#' @param clustid Name of variable to aggregate across for trip-level clustering.
#' @param allclust Generate plots for all clustering methods if TRUE.
#' @param flag Fleet code to use in figure titles.
#' @param dbh If TRUE, create plots of HBF by cluster.
#' @param cllist cllist
#' @param fnhead File name head
#' @param ll5 If TRUE, ll5 variables are used instead of 1 degree versions
#' @param covarnames Names of covariates to return with dataset.
#' @return The original dataset with added columns for the clusters and PCAs;
#'
clust_PCA_run <- function(r, ddd, allsp, allabs, regtype = "regY", ncl, plotPCA = T, clustid = "tripidmon", allclust = F, flag = "JP", dbh = TRUE,
    cllist = NA, fnhead = "", ll5=F, covarnames = c("yrqtr", "latlong", "hooks", "hbf", "vessid", "callsign", "Total", "lat", "lon", "lat5", "lon5", "moon", "op_yr", "op_mon")) {
    datr <- ddd[with(ddd, get(regtype)) == r, allabs]
    datr$reg <- as.data.frame(datr[, regtype])
    spec_dat <- datr[, allsp]  #extract catch composition
    spec_dat$sum <- apply(spec_dat, 1, sum)
    datr <- datr[spec_dat$sum > 0, ]
    for (sp in allsp) {
      if (sum(datr[,sp], na.rm = T) == 0) datr[1,sp] <- 1
    }
    pcaset <- PCA_by_set(datr, Screeplotname = paste0(fnhead, " ", "screeplot set"), allsp)
    pcatrp <- PCA_by_trip(datr, Screeplotname = paste0(fnhead, " ", "screeplot trip"), allsp, clustid = clustid)
    #################### Clustering
    datr <- data.frame(datr)
    cldat <- make_clusters(setdat = data.frame(datr), spp = allsp, ncl = ncl, titx = paste0(flag, " ", regtype, r, " "), setclust = F, tripid = clustid,
        fname = fnhead, regtype = regtype)
    plot_km_deviance_trip(ddd = datr, allsp, r = r, ti = fnhead, regtype = regtype, tripid = clustid)

    # outputs
    datprop <- cldat$setdat
    datprop[, allsp] <- cldat$setdat[, allsp]/apply(cldat$setdat[, allsp], 1, sum)
    a1 <- aggregate(formula = as.formula(paste0("cbind(", paste(allsp, collapse = ", "), ") ~ kcltrp")), data = datprop, FUN = mean)
    names(a1)[1] <- "cluster"
    a1$ctype <- "kcltrp"
    a2 <- aggregate(formula = as.formula(paste0("cbind(", paste(allsp, collapse = ", "), ") ~ clrtrp")), data = datprop, FUN = mean)
    names(a2)[1] <- "cluster"
    a2$ctype <- "clrtrp"
    a3 <- aggregate(formula = as.formula(paste0("cbind(", paste(allsp, collapse = ", "), ") ~ hcltrp")), data = datprop, FUN = mean)
    names(a3)[1] <- "cluster"
    a3$ctype <- "hcltrp"
    a4 <- aggregate(formula = as.formula(paste0("cbind(", paste(allsp, collapse = ", "), ") ~ kclset")), data = datprop, FUN = mean)
    names(a4)[1] <- "cluster"
    a4$ctype <- "kclset"
    a5 <- aggregate(formula = as.formula(paste0("cbind(", paste(allsp, collapse = ", "), ") ~ FT")), data = datprop, FUN = mean)
    names(a5)[1] <- "cluster"
    a5$ctype <- "FT"
    a6 <- aggregate(formula = as.formula(paste0("cbind(", paste(allsp, collapse = ", "), ") ~ clrset")), data = datprop, FUN = mean)
    names(a6)[1] <- "cluster"
    a6$ctype <- "clrset"
    a <- rbind(a1, a2, a3, a4, a5, a6)
    write.csv(a, file = paste(fnhead, "cluster proportions", clustid, ".csv", sep = "_"))

    ###### Create datasets ###################################################### covarnames <- c('yrqtr', 'latlong', 'hooks', 'hbf', 'vessid', 'callsign',
    ###### 'Total', 'lat', 'lon', 'lat5', 'lon5', 'reg', 'moon', 'op_yr', 'op_mon')
    covarnames <- c(covarnames, "reg")
    dataset <- cbind(datr[, covarnames], datr[, allsp], pcaset$pcs[, 1:3], pcaset$pcsBin[, 1:3], pcatrp$pcs[, 1:3], pcatrp$pcsBin[, 1:3], cldat$setdat[,
        c("FT", "kcltrp", "clrtrp", "hcltrp", "kclset", "clrset", "hclset")])
    if (!is.na(cllist[1]))
        dataset <- cbind(datr[, covarnames], datr[, allsp], cldat$setdat[, cllist])
    ti = paste(fnhead, clustid, sep = "_")
    # Examine factors associated with each cluster or PCA
    boxplots_spCL_comp(dat = dataset, cl = "hcltrp", ti = ti, outL = F, nsp = length(allsp), regtype = regtype, r = r, allsp)
    beanplots_spCL_comp(dat = dataset, cl = "hcltrp", ti = ti, outL = F, nsp = length(allsp), regtype = regtype, r = r, allsp)
    boxplots_CL(dat = dataset, cl = "hcltrp", ti = ti, outL = F, dohbf = dbh, lat5 = ll5, regtype = regtype, r = r)
    boxplots_CL_bean(dat = dataset, cl = "hcltrp", ti = ti, outL = F, dohbf = dbh, lat5 = ll5, regtype = regtype, r = r)
    map_clusters(ddd = dataset, cl = "hcltrp", ti = ti, lat5 = ll5, regtype = regtype, r = r, ncl = ncl)
    if (allclust) {
        boxplots_spCL_comp(dat = dataset, cl = "kcltrp", ti = ti, outL = F, nsp = length(allsp), regtype = regtype, r = r, allsp)
        boxplots_spCL_comp(dat = dataset, cl = "clrtrp", ti = ti, outL = F, nsp = length(allsp), regtype = regtype, r = r, allsp)
        boxplots_spCL_comp(dat = dataset, cl = "kclset", ti = ti, outL = F, nsp = length(allsp), regtype = regtype, r = r, allsp)
        boxplots_spCL_comp(dat = dataset, cl = "clrset", ti = ti, outL = F, nsp = length(allsp), regtype = regtype, r = r, allsp)
        boxplots_CL(dat = dataset, cl = "kcltrp", ti = ti, outL = F, dohbf = dbh, lat5 = ll5, regtype = regtype, r = r)
        boxplots_CL(dat = dataset, cl = "clrtrp", ti = ti, outL = F, dohbf = dbh, lat5 = ll5, regtype = regtype, r = r)
        boxplots_CL(dat = dataset, cl = "FT", ti = ti, outL = F, dohbf = dbh, lat5 = ll5, regtype = regtype, r = r)
        boxplots_CL(dat = dataset, cl = "kclset", ti = ti, outL = F, dohbf = dbh, lat5 = ll5, regtype = regtype, r = r)
        boxplots_CL(dat = dataset, cl = "clrset", ti = ti, outL = F, dohbf = dbh, lat5 = ll5, regtype = regtype, r = r)
    }
    graphics.off()

    if (plotPCA) {
        boxplots_PCA(dat = dataset, nPCA = 4, ti = ti, dohbf = dbh, lat5 = ll5, regtype = regtype, r = r)
        boxplots_spPCA(dat = dataset, nPCA = 4, ti = ti, nsp = 8, regtype = regtype, r = r, allsp)
        mapPCA(ddd = dataset, nPCA = 4, ti = ti, lat5 = ll5, regtype = regtype, r = r)
        boxplots_TPCA(dat = dataset, nPCA = 4, ti = ti, dohbf = dbh, lat5 = ll5)
        boxplots_spTPCA(dat = dataset, nPCA = 4, ti = ti, nsp = 8, allsp)
        mapTPCA(ddd = dataset, nPCA = 4, ti = ti, lat5 = ll5)
        graphics.off()
    }
    return(invisible(dataset))
}

#' Run clustering code across all regions and save the results.
#' @param indat The input dataset.
#' @param reg_struc The name of the regional structure to be used.
#' @param allsp Vector of variable names to include in the species composition clustering.
#' @param allabs Vector of variable names to pass to the clust_PCA_run function.
#' @param ncl Vector of number of clusters to identify by region. Default of 'lst' uses the ncl values specified in reg_struc.
#' @param plotPCA Generate PCA plots if TRUE.
#' @param clustid Name of variable to aggregate across for trip-level clustering.
#' @param allclust Generate plots for all clustering methods if TRUE.
#' @param flag Fleet code to use in figure titles.
#' @param dohbf Include HBF in cluster plots if TRUE.
#' @param cvnames Names of covariates to return with dataset.
#' @param rgl A list specifying, for each regional structure, the regions to run and how many clusters to select in each region.
#' @return Nothing is returned but each dataset is saved.
#'
run_clustercode_byreg <- function(indat, reg_struc, allsp, allabs, ncl="lst", plotPCA=F, clustid="tripidmon", allclust=F, flag, dohbf = TRUE, cvnames, rgl) {
  if (ncl == "lst") ncl <- rgl[[reg_struc]]$ncl
  for(r in rgl[[reg_struc]]$allreg) {
    fnh <- paste(flag,reg_struc,r,sep="_")
    dataset <- clust_PCA_run(r=r,ddd=indat,allsp=allsp,allabs=allabs,regtype=reg_struc,ncl=ncl[r], plotPCA=F, clustid="tripidmon", allclust=F, flag=flag, dbh = dohbf, fnhead=fnh,covarnames=cvnames)
    save(dataset,file=paste0(fnh,".RData"))
  }
}

#' Principal components analysis of species composition by 'trip'
#' Run PCA at the trip level using normalised species composition data.
#' @param datr The input dataset.
#' @param Screeplotname File name of the diagnostic Scree plot.
#' @param allsp Species codes / variable names to use in the PCA.
#' @param clustid Name of variable to aggregate across for trip-level clustering.
#' @return A list of two objects based on standard PCA and binomial PCA.
#'
PCA_by_trip <- function(datr, Screeplotname = "screeplot set", allsp, clustid) {
    spec_dat <- datr[, allsp]  #extract catch composition
    spec_dat$sum <- apply(spec_dat, 1, sum)
    nspec <- length(allsp)
    dat2 <- datr[spec_dat$sum > 0, ]
    mmult_dat <- spec_dat[spec_dat$sum > 0, ]
    dat2$TRIP_NUM <- dat2[,clustid]
    tpagg <- aggregate_by_trip(dat2, allsp)
    pca_tpdat <- (sqrt(sqrt(tpagg[, allsp])))  ## prepare species composition for PCA
    pca <- prcomp(pca_tpdat, scale = T)  #run PCA
    pr_pca <- predict(pca, pca_tpdat)  # Predict PCA Loadings
    pcs <- data.frame(pr_pca[, 1:nspec])
    names(pcs) <- paste0("T", names(pcs))
    setpcs <- pcs[match(dat2$TRIP_NUM, tpagg$TRIP_NUM), ]
    eigtp = summary(pca)$sdev^2  # get Eigenvalue
    OCtesttp = nScree(eigtp)  # run Optimal Coordinates test
   windows()
    plotnScree(OCtesttp)
    par(col = "black")
    savePlot(Screeplotname, type = "png")
    nPC = min(OCtesttp$Components$nkaiser, OCtesttp$Components$noc)  # retain number of PCs in combination with Eigenvalue > 1
    pcsBin = ifelse(pcs > 0.5, 1, 0)  ### GLM - DPC variable as binary PCs
    dimnames(pcsBin)[[2]] <- paste0("B", names(pcs))
    setpcsBin <- pcsBin[match(dat2$TRIP_NUM, tpagg$TRIP_NUM), ]
    return(PCAtrip = list(pcs = setpcs, pcsBin = setpcsBin))
}

#' Principal components analysis of species composition by 'set'.
#' Run PCA at the set level using normalised species composition data.
#' @param datr The input dataset.
#' @param Screeplotname File name of the diagnostic Scree plot.
#' @param allsp Species codes / variable names to use in the PCA.
#' @return A list of two objects based on standard PCA and binomial PCA.
#'
PCA_by_set <- function(datr, Screeplotname = "screeplot set", allsp) {
  spec_dat <- datr[, allsp]  #extract catch composition
    spec_dat$sum <- apply(spec_dat, 1, sum)
    nspec <- length(allsp)
    dat2 <- datr[spec_dat$sum > 0, ]
    mmult_dat <- spec_dat[spec_dat$sum > 0, ]
    pca_dat <- (sqrt(sqrt(mmult_dat[, 1:nspec]/mmult_dat$sum)))  ## prepare species composition for PCA
    apply(spec_dat, 2, sum)
    pca <- prcomp(pca_dat, scale = T)  #run PCA
    pr_pca <- predict(pca, pca_dat)  # Predict PCA Loadings
    pcs <- data.frame(pr_pca[, 1:nspec])
    eig = summary(pca)$sdev^2  # get Eigenvalue
    OCtest = nScree(eig)  # run Optimal Coordinates test
    windows()
    plotnScree(OCtest)
    par(col = "black")
    savePlot(Screeplotname, type = "png")
    nPC = min(OCtest$Components$nkaiser, OCtest$Components$noc)  # retain number of PCs in combination with Eigenvalue > 1
    pcsBin = ifelse(pcs > 0.5, 1, 0)  ### GLM - DPC variable as binary PCs
    dimnames(pcsBin)[[2]] <- paste0("B", names(pcs))
    return(PCAset = list(pcs = pcs, pcsBin = pcsBin))
}

#' Cluster dataset based on species composition.
#' Run cluster analysis at both set and trip levels, using three methods: hclust, clara, and kmeans.
#' @param setdat The input dataset, at the set level.
#' @param spp The species codes to use for clustering.
#' @param ncl Number of clusters to use.
#' @param titx Title for plots.
#' @param setclust Do clustering at set level if TRUE.
#' @param tripid Name of variable used to aggreate for trip-level clustering.
#' @param fname File name header for saving plots.
#' @param regtype Region type code.
#' @return A list of four objects: d, the normalised trip-level dataset; fit, the hclust results; clarax, the clara trip level results; setdat, all the clustering results, as additional columns added to the input dataset.
#'
make_clusters <- function(setdat, spp, ncl = 5, titx = "", setclust = T, tripid = "tripid", fname = "", regtype = "regY") {
    setdat$TRIP_NUM <- as.vector(setdat[, tripid])
    spec_dat <- setdat[, spp]  #extract catch composition
    spec_dat$sum <- apply(spec_dat, 1, sum)
    nspec <- length(spp)
    dat2 <- setdat[spec_dat$sum > 0, ]
    mmult_dat <- spec_dat[spec_dat$sum > 0, ]
    clus_dat <- mmult_dat[, 1:nspec]/mmult_dat$sum  # raw proportions
    FT = kmeans(clus_dat, ncl)$cluster

    aset <- na.omit(setdat[, spp])
    aset <- scale(aset[, spp])  # rescaled data

    indat <- aggregate_by_trip(dat2, spp)
    atrp <- na.omit(indat)
    # NbClust(atrp[, spp], method = 'ward.D');flush.console()
    atrp <- scale(atrp[, spp])
    dtrp <- dist(atrp, method = "euclidean")
    fittrp <- hclust(dtrp, method = "ward.D")
    plot(fittrp, labels = FALSE, hang = -1, main = paste(titx, "trip"))  # display dendogram  #looks like 3 (or 4)
    grptrp <- cutree(fittrp, k = ncl)  # cut tree into ncl clusters
    print(table(grptrp))
    rect.hclust(fittrp, k = ncl, border = "red")
    savePlot(paste0(fname, "_hclusters_trip.png"), type = "png")
    if (setclust) {
        dset <- dist(aset, method = "euclidean")  # distance matrix
        fitset <- hclust(dset, method = "ward.D")
        windows()
        plot(fitset, labels = FALSE, hang = -1, main = paste(titx, "set"))  # display dendogram  #looks like 3 (or 4)
        grpset <- cutree(fitset, k = ncl)  # cut tree into ncl clusters
        print(table(grpset))
        rect.hclust(fitset, k = ncl, border = "red")
        savePlot(paste0(fname, "_hclusters_trip.png"), type = "png")
    }
    claratrp <- clara(atrp, ncl)  #clustering based upon the percent of spp in total catch of tuna
    claraset <- clara(aset, ncl)  #clustering based upon the percent of spp in total catch of tuna
    kmtrp <- kmeans(atrp, centers = ncl, iter.max = 60)
    kmset <- kmeans(aset, centers = ncl, iter.max = 60)
    setdat$kcltrp <- kmtrp$cluster[match(setdat$TRIP_NUM, indat$TRIP_NUM)]
    setdat$clrtrp <- claratrp$clustering[match(setdat$TRIP_NUM, indat$TRIP_NUM)]
    setdat$hcltrp <- grptrp[match(setdat$TRIP_NUM, indat$TRIP_NUM)]
    setdat$kclset <- kmset$cluster
    setdat$FT <- FT
    setdat$clrset <- claraset$clustering
    if (setclust)
        setdat$hclset <- grpset else setdat$hclset <- NA
    return(list(d = dtrp, fit = fittrp, clarax = claratrp, setdat = setdat))
}

#' Make various types of id for use in aggregating data before clustering.
#' The function makes multiple passes through the dataset.
#' @param dat The input dataset, at the set level.
#' @return dat, the input dataset with the following variables added: latlong1, latlong2, clid, aggid, jnt_clid.
#'
make_clid <- function(dat) {
    dat$latlong2 <- paste(2 * floor(dat$lat/2), 2 * floor(dat$lon/2), sep = "_")
    dat$latlong1 <- paste(dat$lat, dat$lon, sep = "_")
    # 1
    dat$clid <- NA
    dat$aggid <- paste(dat$op_yr, dat$dmy, dat$latlong1)
    a <- table(dat$aggid)
    usea <- a[a > 14 & a < 1000]
    dat$clid[dat$aggid %in% names(usea)] <- dat$aggid[dat$aggid %in% names(usea)]
    # 2
    dat$aggid <- paste(dat$op_yr, week(dat$dmy), dat$latlong1)
    dat$aggid[!is.na(dat$clid)] <- 0
    a <- table(dat$aggid)
    usea <- a[a > 10 & a < 1000]
    dat$clid[dat$aggid %in% names(usea)] <- dat$aggid[dat$aggid %in% names(usea)]
    # 3
    dat$aggid <- paste(dat$op_yr, week(dat$dmy), dat$latlong2)
    dat$aggid[!is.na(dat$clid)] <- 0
    a <- table(dat$aggid)
    usea <- a[a > 10 & a < 1000]
    dat$clid[dat$aggid %in% names(usea)] <- dat$aggid[dat$aggid %in% names(usea)]
    # 4
    dat$aggid <- paste(dat$op_yr, week(dat$dmy), dat$latlong)
    dat$aggid[!is.na(dat$clid)] <- 0
    a <- table(dat$aggid)
    usea <- a[a > 10 & a < 1000]
    dat$clid[dat$aggid %in% names(usea)] <- dat$aggid[dat$aggid %in% names(usea)]
    # 5
    dat$aggid <- paste(dat$op_yr, dat$op_mon, dat$latlong)
    dat$aggid[!is.na(dat$clid)] <- 0  # where clid is already set, change aggid to 0
    a <- table(dat$aggid)
    usea <- a[a < 1000]
    dat$clid[dat$aggid %in% names(usea)] <- dat$aggid[dat$aggid %in% names(usea)]
    dat$aggid <- NULL

    dat$jnt_clid <- dat$clid
    dat$jnt_clid[dat$vessid > 1] <- dat$tripidmon[dat$vessid > 1]
    return(dat)
}

#' Aggregate the input dataset by yearqtr, latlong, and hbf.
#' @param dat The input dataset, at the set level.
#' @param sp The species code of the species of interest.
#' @return taball, the aggregated dataset.
#'
aggregate_data <- function(dat, sp) {
  tab1 <- aggregate(dat[, 3], list(dat$yrqtr, dat$latlong, dat$hbf), sum)
  tab2 <- aggregate(dat$hooks, list(dat$yrqtr, dat$latlong, dat$hbf), sum)
  taball <- cbind(tab1[, 1:4], tab2[, 4])
  names(taball) <- c("yrqtr", "latlong", "hbf", sp, "hooks")
  taball$yrqtr <- as.numeric(as.character(taball$yrqtr))
  taball$latlong <- as.character(taball$latlong)
  taball$hbf <- as.numeric(as.character(taball$hbf))
  taball[, 4] <- as.numeric(as.character(taball[, 4]))
  taball$hooks <- as.numeric(as.character(taball$hooks))
  return(taball)
}

#' Aggregate by trip.
#'
#' The function aggregates the input dataset by trip using the variable TRIP_NUM.
#' @param dat The input dataset, at the set level.
#' @param flds The fields to aggregate by summation.
#' @return The aggregated dataset.
#'
aggregate_by_trip <- function(dat, flds) {
  TRIP_NUM <- NULL
  dt1 <- data.table(dat)
  setkeyv(dt1, cols = "TRIP_NUM")
  df2 <- dt1[, lapply(.SD, mean, na.rm = T), by = list(TRIP_NUM), .SDcols = flds]
  df2 <- data.frame(df2)
  df3 <- cbind(TRIP_NUM = df2[, 1], df2[, flds]/apply(df2[, flds], 1, sum))
  return(df3)
}

