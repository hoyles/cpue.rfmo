#' Plot quarterly indices.
#'
#' The function takes a data frame and plots it.
#' @param a The input data frame.
#' @param vartype Type of analysis, to use in the plot title.
#' @param mdti Period and type of run, to use in plot title.
#' @param regstr Name of regional structure, to use in file name.
#' @param runreg Region number, to use in file name.
#'
doplot_cpue <- function(a,vartype, mdti, regstr, runreg) {
  plot(a$yq,a$pr/mean(a$pr,na.rm=T),xlab="Year-quarter",ylab="Relative CPUE",main=paste(vartype,mdti),type="l",ylim=c(0,3))
  points(a$yq,a$pr/mean(a$pr,na.rm=T), cex=0.7)
  points(a$yq,a$ul/mean(a$pr,na.rm=T),pch="-",col=3)
  points(a$yq,a$ll/mean(a$pr,na.rm=T),pch="-",col=2)
  mtext(paste0(regstr," R",runreg),side=3,outer=T,line=-2)
}

#' Plot annual indices.
#'
#' The function takes a data frame and plots it.
#' @param a The input data frame.
#' @param vartype Type of analysis, to use in the plot title.
#' @param mdti Period and type of run, to use in plot title.
#' @param regstr Name of regional structure, to use in file name.
#' @param runreg Region number, to use in file name.
#'
doplot_yr_cpue <- function(a,vartype, mdti, regstr, runreg) {
  aa <- data.frame(yr = seq(min(defactor(a$yr), na.rm = TRUE), max(defactor(a$yr), na.rm = TRUE), 1))
  mpr <- mean(a$pr, na.rm = TRUE)
  aa$pr <- a$pr[match(aa$yr, a$yr)]
  aa$pr <- aa$pr /mpr
  plot(aa$yr,aa$pr,xlab="Year",ylab="Relative CPUE",main=paste(vartype,mdti),type="l",ylim=c(0,3))
  points(aa$yr,aa$pr, cex=0.7)
  mtext(paste0(regstr," R",runreg),side=3,outer=T,line=-2)
}

#' Prepare indices, save files and plots.
#'
#' The function takes a list of directories with output file.
#' It goes through them and sets up dataframes of the yera-quarterly indices.
#' It generates annual indices by regressing pr ~ yr + qtr.
#' It saves .csv files of all indices, and generates and saves index plots.
#' @param resdirs Directories to work through.
#' @param reg_strs Names of the regional structures to work through.
#' @param rgl A list of the regions to plot in each regional structure.
#' @param vartypes One or both of 'lognC' and 'dellog'.
#' @param yr1 First year of the early model, for titles.
#' @param outdirname The default name of the folder to create for the outputs.
#'
prep_indices <- function(resdirs, reg_strs, rgl, vartypes = c("lognC","dellog"), yr1=1952, outdirname = "outputs/",
                         mdt_all = c("novess_allyrs","boat_allyrs","novess_5279","vessid_79nd"),
                         mdti_all = c(paste0(yr1,"-present no vessid"),paste0(yr1,"-present vessid"),paste0(yr1,"-1979 no vessid"),"1979-present vessid")) {
  for (resdir in resdirs) {
    outdir <- paste0(resdir,outdirname)
    dir.create(outdir)
    setwd(resdir)
    for(regstr in reg_strs) {
      for(runreg in rgl[[regstr]]) {
        for(vartype in vartypes) {
          windows(height=12,width=12);par(mfrow=c(2,2),mar=c(4,4,3,1))
          qtrdev <- dev.cur()
          windows(height=12,width=12);par(mfrow=c(2,2),mar=c(4,4,3,1))
          yrdev <- dev.cur()
          saveit <- FALSE
          for(mdn in 1:4) {
            # mdt <- c("novess_allyrs","boat_allyrs","novess_5279","vessid_79nd")[mdn]
            # mdti <- c(paste0(yr1,"-present no vessid"),paste0(yr1,"-present vessid"),paste0(yr1,"-1979 no vessid"),"1979-present vessid")[mdn]
            mdt <- mdt_all[mdn]
            mdti <- mdti_all[mdn]
            modtype <- paste(vartype,mdt,sep="_")
            fname <- paste0("Joint_",regstr,"_R",runreg)
            if(vartype != "dellog") {
              if(file.exists(paste0(fname,"_",modtype,"_predictions.RData"))) {
                load(paste0(fname,"_",modtype,"_predictions.RData"))
                xx <- data.frame(yq=as.numeric(as.character(nd$newdat$yrqtr)))
                xx$pr1 <- switch(vartype,lognC=exp(nd$predresp$fit),negbC=nd$predresp$fit)
                if(vartype=="lognC") {
                  xx$pr <- exp(apply(nd$predterms$fit,1,sum)+attr(nd$predterms$fit,"constant"))
                  xx$cv <- nd$predterms$se.fit[,1]
                  xx$ll <- exp(apply(nd$predterms$fit,1,sum)+attr(nd$predterms$fit,"constant")-1.96*nd$predterms$se.fit[,1])
                  xx$ul <- exp(apply(nd$predterms$fit,1,sum)+attr(nd$predterms$fit,"constant")+1.96*nd$predterms$se.fit[,1])
                } else {
                  xx$pr <- exp(apply(nd$predterms$fit,1,sum)+attr(nd$predterms$fit,"constant"))
                  xx$cv <- nd$predterms$se.fit[,1]
                  xx$ll <- exp(apply(nd$predterms$fit,1,sum)+attr(nd$predterms$fit,"constant")-1.96*nd$predterms$se.fit[,1])
                  xx$ul <- exp(apply(nd$predterms$fit,1,sum)+attr(nd$predterms$fit,"constant")+1.96*nd$predterms$se.fit[,1])
                }
                a <- data.frame(yq=seq(min(xx$yq),max(xx$yq),0.25))
                a <- cbind(yq=a$yq,xx[match(a$yq,xx$yq),3:6])
                a[,c(2,4,5)] <- a[,c(2,4,5)]/mean(a[,2],na.rm=TRUE)
                a$yr <- as.factor(floor(a$yq))
                a$qtr <- as.factor(a$yq - floor(a$yq))
                dev.set(which = qtrdev)
                doplot_cpue(a,vartype, mdti, regstr, runreg)
                dev.set(which = yrdev)
                mod <- glm(pr ~ yr + qtr,data=a)
                nd <- data.frame(yr=sort(unique(a$yr[!is.na(a$pr)])),qtr=levels(a$qtr)[2])
                nd$pr <- predict(mod,newdat=nd,se.fit=FALSE)
                doplot_yr_cpue(nd,vartype, mdti, regstr, runreg)
                write.csv(a,file=paste0(outdir,fname,"_",modtype,"_yq.csv"))
                write.csv(nd,file=paste0(outdir,fname,"_",modtype,"_yr.csv"))
                saveit <- TRUE
              }}
            if(vartype=="dellog") {
              if(file.exists(paste0(fname,"_pos_",modtype,"_predictions.RData"))) {
                ndpos <- pcoefs <- NULL
                load(paste0(fname,"_pos_",modtype,"_predictions.RData"))
                load(paste0(fname,"_",modtype,"_indices.RData"))
                xx <- data.frame(yq=as.numeric(as.character(ndpos$newdat$yrqtr)))
                xx$pr <- pcoefs
                xx$ln.cv <- ndpos$predterms$se.fit[,1]
                xx$ll <- exp(log(pcoefs) - 1.96*ndpos$predterms$se.fit[,1])
                xx$ul <- exp(log(pcoefs) + 1.96*ndpos$predterms$se.fit[,1])

                a <- data.frame(yq=seq(min(xx$yq,na.rm=T),max(xx$yq,na.rm=T),0.25))
                a <- cbind(yq=a$yq,xx[match(a$yq,xx$yq),2:5])
                a[,c(2,4:5)] <- a[,c(2,4:5)]/mean(a[,2],na.rm=TRUE)
                a$yr <- as.factor(floor(a$yq))
                a$qtr <- as.factor(a$yq - floor(a$yq))
                dev.set(which = qtrdev)
                doplot_cpue(a,vartype, mdti, regstr, runreg)
                mod <- glm(pr ~ yr + qtr,data=a)
                nd <- data.frame(yr=sort(unique(a$yr[!is.na(a$pr)])),qtr=levels(a$qtr)[2])
                nd$pr <- predict(mod,newdat=nd,se.fit=FALSE)
                dev.set(which = yrdev)
                doplot_yr_cpue(nd,vartype, mdti, regstr, runreg)
                write.csv(a,file=paste0(outdir,fname,"_",modtype,"_yq.csv"))
                write.csv(nd,file=paste0(outdir,fname,"_",modtype,"_yr.csv"))
                saveit <- TRUE
              }}
          }
          if(saveit) {
            dev.set(which = qtrdev)
            savePlot(filename=paste0(outdir,fname,"_",vartype,"_comp_yq.png"),type="png")
            dev.set(which = yrdev)
            savePlot(filename=paste0(outdir,fname,"_",vartype,"_comp_yr.png"),type="png")
            graphics.off()
          }
        }}
    }}
}


#' Plot median residuals by cluster and year-quarter.
#'
#' The function takes the residuals extracted from a model object and plots their medians by year-quarter and cluster.
#' @param a The model data.
#' @param b The model residuals.
#' @param mf The mfrow parameter, to set up the figure.
#' @param outdir Directory to save the results in.
#' @param fname File identifier, for the file name.
#' @param modtype Either dellog or logC, to determine the filename.
#'
resplot_med_by_yq_cl <- function(a,b,mf,outdir,fname,modtype) {
  windows(10,10);par(mfrow=mf,mar=c(4,4,2,1),oma=c(0,0,2,0))
  for (cl in sort(unique(a$clust))) {
    loc <- as.character(a$clust)==cl
    bb <- tapply(b[loc],as.numeric(as.character(a$yrqtr[loc])),median)
    plot(as.numeric(names(bb)),bb,xlab="Year-quarter",ylab="Median of residuals",main=cl)
    mtext(paste(fname,modtype),side=3,outer=T,line=0)
  }
  savePlot(paste0(outdir,fname,"_",modtype,"_medres_by_yq_cl.png"),type="png")
}

#' Plot all residuals by cluster and year-quarter.
#'
#' The function takes the residuals extracted from a model object and boxplots them by year-quarter and cluster.
#' @param a The model data.
#' @param b The model residuals.
#' @param mf The mfrow parameter, to set up the figure.
#' @param outdir Directory to save the results in.
#' @param fname File identifier, for the file name.
#' @param modtype Either dellog or logC, to determine the filename.
#'
resplot_all_by_yq_cl <- function(a,b,mf,outdir,fname,modtype) {
  windows(10,10);par(mfrow=mf,mar=c(4,4,2,1),oma=c(0,0,2,0))
  for (cl in sort(unique(a$clust))) {
    loc <- as.character(a$clust)==cl
    boxplot(b[loc] ~ as.numeric(as.character(a$yrqtr[loc])),xlab="Year-quarter",ylab="Residuals",main=cl)
    mtext(paste(fname,modtype),side=3,outer=T,line=0)
  }
  savePlot(paste0(outdir,fname,"_",modtype,"_allres_by_yq_cl.png"),type="png")
}

#' Map median residuals by cluster.
#'
#' The function takes the residuals extracted from a model object and maps their medians by spatial cell and cluster.
#' @param a The model data.
#' @param b The model residuals.
#' @param mf The mfrow parameter, to set up the figure.
#' @param outdir Directory to save the results in.
#' @param fname File identifier, for the file name.
#' @param modtype Either dellog or logC, to determine the filename.
#'
resplot_med_by_map_cl <- function(a,b,mf,outdir,fname,modtype) {
  windows(10,10);par(mfrow=mf,mar=c(2,2,2,0),oma=c(0,0,2,0))
  for (cl in sort(unique(a$clust))) {
    loc <- as.character(a$clust)==cl
    bb <- tapply(b[loc],as.character(a$latlong[loc]),median)
    ll <- as.numeric(unlist(strsplit(names(bb),"_")))
    bb2 <- data.frame(med=bb)
    bb2$lat <- ll[seq(1,length(ll),2)]
    bb2$lon <- ll[seq(2,length(ll),2)]
    bb3 <- tapply(bb2$med,list(bb2$lon,bb2$lat),mean)
    if(min(dim(bb3)) > 1) {
      image(sort(unique(bb2$lon)),sort(unique(bb2$lat)),bb3,xlab="Lon",ylab="Lat",main=cl,breaks=seq(-1.5,1.5,length.out=31),col=heat.colors(30))
      contour(sort(unique(bb2$lon)),sort(unique(bb2$lat)),bb3,add=T,breaks=seq(-5,5,length.out=101))
      map(add=TRUE,fill=TRUE)
      mtext(paste(fname,modtype),side=3,outer=T,line=0)
    }
  }
  savePlot(paste0(outdir,fname,"_",modtype,"_medres_by_map_cl.png"),type="png")
}

#' Plot median residuals by fleet and year-quarter.
#'
#' The function takes the residuals extracted from a model object and plots their medians by year-quarter and cluster.
#' @param a The model data.
#' @param b The model residuals.
#' @param mf The mfrow parameter, to set up the figure.
#' @param outdir Directory to save the results in.
#' @param fname File identifier, for the file name.
#' @param modtype Either dellog or logC, to determine the filename.
#'
resplot_med_by_yq_flg <- function(a,b,mf,outdir,fname,modtype) {
  windows(10,10);par(mfrow=mf,mar=c(4,4,2,1),oma=c(0,0,2,0))
  xrange=range(as.numeric(as.character(a$yrqtr)))
  for (fl in sort(unique(a$flag))) {
    loc <- as.character(a$flag)==fl
    bb <- tapply(b[loc],as.numeric(as.character(a$yrqtr[loc])),median)
    plot(as.numeric(names(bb)),bb,xlim=xrange,xlab="Year-quarter",ylab="Median of residuals",main=switch(fl,J="JP",K="KR",T="TW",S="SY", U = "US"))
    mtext(paste(fname,modtype),side=3,outer=T,line=0)
  }
  savePlot(paste0(outdir,fname,"_",modtype,"_medres_by_yq_flg.png"),type="png")
}

#' Plot all residuals by fleet and year-quarter.
#'
#' The function takes the residuals extracted from a model object and boxplots them by year-quarter and fleet.
#' @param a The model data.
#' @param b The model residuals.
#' @param mf The mfrow parameter, to set up the figure.
#' @param outdir Directory to save the results in.
#' @param fname File identifier, for the file name.
#' @param modtype Either dellog or logC, to determine the filename.
#'
resplot_all_by_yq_flg <- function(a,b,mf,outdir,fname,modtype) {
  windows(10,10);par(mfrow=mf,mar=c(4,4,2,1),oma=c(0,0,2,0))
  xrange=range(as.numeric(as.character(a$yrqtr)))
  for (fl in sort(unique(a$flag))) {
    loc <- as.character(a$flag)==fl
    boxplot(b[loc] ~ as.numeric(as.character(a$yrqtr[loc])),xlab="Year-quarter",ylab="Residuals",main=switch(fl,J="JP",K="KR",T="TW",S="SY"))
    mtext(paste(fname,modtype),side=3,outer=T,line=0)
  }
  savePlot(paste0(outdir,fname,"_",modtype,"_allres_by_yq_flg.png"),type="png")
}

#' Map median residuals by fleet.
#'
#' The function takes the residuals extracted from a model object and maps their medians by spatial cell and fleet.
#' @param a The model data.
#' @param b The model residuals.
#' @param mf The mfrow parameter, to set up the figure.
#' @param outdir Directory to save the results in.
#' @param fname File identifier, for the file name.
#' @param modtype Either dellog or logC, to determine the filename.
#'
resplot_med_by_map_flg <- function(a,b,mf,outdir,fname,modtype) {
  windows(10,10);par(mfrow=mf,mar=c(4,4,2,1),oma=c(0,0,2,0))
  for (fl in sort(unique(a$flag))) {
    loc <- as.character(a$flag)==fl
    bb <- tapply(b[loc],as.character(a$latlong[loc]),median)
    ll <- as.numeric(unlist(strsplit(names(bb),"_")))
    bb2 <- data.frame(med=bb)
    bb2$lat <- ll[seq(1,length(ll),2)]
    bb2$lon <- ll[seq(2,length(ll),2)]
    bb3 <- tapply(bb2$med,list(bb2$lon,bb2$lat),mean)
    if(min(dim(bb3)) > 1) {
      image(sort(unique(bb2$lon)),sort(unique(bb2$lat)),bb3,xlab="Lon",ylab="Lat",main=switch(fl,J="JP",K="KR",T="TW",S="SY"),breaks=seq(-1.5,1.5,length.out=31),col=heat.colors(30))
      contour(sort(unique(bb2$lon)),sort(unique(bb2$lat)),bb3,add=T,breaks=seq(-5,5,length.out=101))
      map(add=TRUE,fill=TRUE)
      mtext(paste(fname,modtype),side=3,outer=T,line=0)
    }
  }
  savePlot(paste0(outdir,fname,"_",modtype,"_medres_by_map_flg.png"),type="png")
}

