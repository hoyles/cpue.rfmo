########################################
# Plots
########################################
# Joint standardization
projdir <- "~/IATTC/2019_CPUE/"

jointdir <- paste0(projdir, "joint/")
JPdir <- paste0(projdir, "JP/")
KRdir <- paste0(projdir, "KR/")
TWdir <- paste0(projdir, "TW/")

alldirs <- c(paste0(jointdir,"analyses/cl0_hb0_hk1/"),
             paste0(jointdir,"analyses/cl1_hb0_hk1/"),
             paste0(jointdir,"analyses/cl0_hb1_hk1/"),
             paste0(jointdir,"analyses/cl1_hb1_hk1/"),
             paste0(JPdir,"analyses/cl0_hb0_hk1/"),
             paste0(JPdir,"analyses/cl1_hb0_hk1/"),
             paste0(JPdir,"analyses/cl0_hb1_hk1/"),
             paste0(JPdir,"analyses/cl1_hb1_hk1/"),
             paste0(KRdir,"analyses/cl0_hb0_hk1/"),
             paste0(KRdir,"analyses/cl1_hb0_hk1/"),
             paste0(KRdir,"analyses/cl0_hb1_hk1/"),
             paste0(KRdir,"analyses/cl1_hb1_hk1/"),
             paste0(TWdir,"analyses/cl0_hb0_hk1/"),
             paste0(TWdir,"analyses/cl1_hb0_hk1/"),
             paste0(TWdir,"analyses/cl0_hb1_hk1/"),
             paste0(TWdir,"analyses/cl1_hb1_hk1/"))

alldirs <- c(paste0(jointdir,"analyses/y_cl1_hb0_hk1/"),
             paste0(jointdir,"analyses/y_cl0_hb1_hk1/"),
             paste0(JPdir,"analyses/y_cl1_hb0_hk1/"),
             paste0(JPdir,"analyses/y_cl0_hb1_hk1/"),
             paste0(KRdir,"analyses/y_cl1_hb0_hk1/"),
             paste0(KRdir,"analyses/y_cl0_hb1_hk1/"),
             paste0(TWdir,"analyses/y_cl1_hb0_hk1/"),
             paste0(TWdir,"analyses/y_cl0_hb1_hk1/"))


reglist <- list("regBepo" = c(1:4))
splist <- list("regBepo" = "yft")

library("beanplot")
library("cluster")
library("data.table")
library("date")
library("influ")
library("MASS")
library("mgcv")
library("nFactors")
library("plyr")
library("dplyr")
library("randomForest")
library("splines")
library("survival")
library("cpue.rfmo")
library("maps")
library("maptools")
library("mapdata")
library("plotrix")

library("cpue.rfmo")

mdtn <- "novess_allyrs"
mdtv <- "boat_allyrs"
md5279 <- "novess_5279"
md79nd <- "vessid_79nd"
keepd <- TRUE
yr1 = 1979

mdt_all <- c("novess_allyrs","boat_allyrs","novess_5279","vessid_79nd")
mdti_all <- c(paste0(yr1,"-present no vessid"),paste0(yr1,"-present vessid"),paste0(yr1,"-1979 no vessid"),"1979-present vessid")
#reg_strs <- c("regY","regY2","regA4","regA5")
reg_strs <- c("regBepo")
#reg_strs <- c("regY","regY2")


#resdir <- alldirs[3]; mdn=4; regstr = "regB"; runreg = 1; vartype = "dellog"; # numbers for testing

prep_indices(resdirs=alldirs, reg_strs=c("regBepo"), reglist, vartypes = c("lognC","dellog"), yr1=1979, nplots = 2)

#prep_indices(resdirs=natdirs[2], reg_strs=c("regA4","regA5"), reglist, vartypes = c("lognC","dellog"), yr1=1952)


# # Overlay national plots for comparison. ICCAT.
# resdirs_flags <- c(paste0(projdir, "joint/analyses/std_cl_hbf/outputs_test/"),
#                    paste0(JPdir,"analyses/std_cl_JPonly_hbf/outputs_test/"),
#                    paste0(KRdir,"analyses/std_cl_KRonly_hbf/outputs_test/"),
#                    paste0(TWdir,"analyses/std_cl_TWonly_hbf/outputs_test/"),
#                    paste0(USdir,"analyses/std_cl_USonly_hbf/outputs_test/"))
# labs <- c("Joint", "JP", "KR", "TW", "US")
#
# reslist <- list()
# for(r in 1:3) {
#   for(md in c("boat_allyrs", "novess_allyrs")) {
#     for(dirnum in 1:5) {
#       if(md == "boat_allyrs" & labs[dirnum] %in% c("Joint", "JP")) {
#         md2 <- "vessid_79nd"
#       } else { md2 <- md }
#       fn <- paste0(resdirs_flags[dirnum],"Joint_regB_R",r,"_dellog_",md2,"yr.csv")
#       if (file.exists(fn)) {
#         a <- read.csv(fn)
#         if(dirnum == 1) {
#           aa <- data.frame(yr = seq(min(a$yr, na.rm = TRUE), max(a$yr, na.rm = TRUE)))
#           aa$pr <- a$pr[match(aa$yr, a$yr)]
#         } else {
#           aa <- cbind(aa,px=a$pr[match(aa$yr, a$yr)])
#         }
#         print(paste("X",fn))
#       } else {
#         print(paste("0",fn))
#       }
#     }
#     # equalise means
#     allin <- aa
#     for (dirnum in 1:5) {
#       if(dirnum == 1) {
#         aa <- data.frame(yr = seq(min(a$yr, na.rm = TRUE), max(a$yr, na.rm = TRUE)))
#         aa$pr <- a$pr[match(aa$yr, a$yr)]
#         windows()
#         plot(aa$yr, aa$pr, type = "b", xlab = "Year-quarter", ylab = "Relative CPUE", main = paste("Region", r, " ", md2), ylim = c(0, 3))
#         legend("topright", legend = labs, col = 1:5, pch = 1:5, lty=1)
#       } else {
#         lines(yrs, a$pr[match(yrs, a$yr)], col = dirnum, pch = dirnum, type = "b")
#       }
#   }
#   fnf <- paste0(projdir, "joint/analyses/std_cl_hbf/outputs_test/plot2_compare_yr_R", r,"_", md)
#   savePlot(file = fnf, type = "png")
# }

## -----------------------------------
# Run influence plots.
  ## -----------------------------------



fl = rep(c("jnt", "JP", "KR", "TW"), each = 4)
for(dirnum in 1:16) {
  resdir <- alldirs[dirnum]
  outdir <- paste0(resdir,"/influ/")
  dir.create(outdir)
  setwd(outdir)
  for(regstr in reg_strs) {
    for (r in reglist[[regstr]]) {
      for (mdi in 1:4) {
        md <- mdt_all[mdi]
        nm_dat <- c("glmdat", "glmdat", "glmdat5279", "glmdat79nd")[mdi]
        nm_wt <- c("wtt.all", "wtt.all", "wtt.5279", "wtt.79nd")[mdi]
        rm(mod)
        rm(infve)
        fn <- paste0(alldirs[dirnum],"Joint_", regstr,"_R",r,"_lognC_",md,"_model.RData")
        if (file.exists(fn)) {
          load(fn)
          runsp <- splist[[regstr]]
          assign(nm_dat, mod$data)
          assign(nm_wt, mk_wts(mod$data,wttype="area"))
          mn <- with(mod$data,0.1 * mean(get(runsp)/hooks))
          # check variables
          doltln <- isTRUE(grep("latlong", mod$formula)[1] > 0)
          dohbf  <- isTRUE(grep("hbf", mod$formula)[1] > 0)
          dohook <- isTRUE(grep("hooks", mod$formula)[1] > 0)
          dovess <- isTRUE(grep("vessid", mod$formula)[1] > 0)
          doclust <- isTRUE(grep("clust", mod$formula)[1] > 0)
          infve=Influence$new(mod)
          infve$calc()
          windows()
          fname <- paste0(regstr,"_R",r,"_",fl[dirnum],"_",runsp,"_",md,".png")
          infve$stanPlot();savePlot(paste0("Stanplot", fname),type="png")
          infve$stepPlot();savePlot(paste0("stepPlot", fname), type = "png")
          infve$influPlot();savePlot(paste0("influPlot", fname), type = "png")
          if (doltln) { infve$cdiPlot('latlong');savePlot(paste0("influPlot_latlong", fname),type="png") }
          if (dovess) { infve$cdiPlot('vessid'); savePlot(paste0("influPlot_vessid", fname),type="png") }
          if (dohook) { infve$cdiPlot('ns(hooks, 10)');savePlot(paste0("influPlot_hooks", fname),type="png") }
          if (dohbf)  { infve$cdiPlot('ns(hbf, 3)');savePlot(paste0("influPlot_hbf", fname),type="png") }
          if (doclust) { infve$cdiPlot('clust');savePlot(paste0("influPlot_cl", fname),type="png") }
          graphics.off()
        }
      }
    }
  }
}
getwd()

# with(glmdat[as.numeric(as.character(glmdat$yrqtr)) > 1970,], tapply(hbf, list(yrqtr, hbf), length))
# a <- glmdat[as.numeric(as.character(glmdat$yrqtr)) > 1975,]
# a$yrqtr <- defactor(a$yrqtr)
# tapply(a$hbf, list(floor(a$yrqtr), a$hbf), length)
#
# a <- glmdat
# a$yrqtr <- defactor(glmdat$yrqtr)
# a$yr <- floor(a$yrqtr)
# a <- a[a$yr %in% 1975:1985,]
# a$vessid <- as.character(a$vessid)
# table(floor(a$yr), a$vessid=="JP2")


##########################################
# Diagnostics / residual plots ###########
##########################################

# resdirs <- c(paste0(jointdir,"analyses/std_cl_nohbf_spl/"),
#              paste0(jointdir,"analyses/std_cl_nohbf/"),
#              paste0(jointdir,"analyses/std_nocl_hbf/"),
#              paste0(jointdir,"analyses/std_nocl_hbf_spl/"),
#              paste0(jointdir,"analyses/intx_nocl_hbf/"),
#              paste0(jointdir,"analyses/intx_cl_nohbf/")) # change these folders to match your own

mdt="boat_allyrs"; mdt="vessid_79nd"; mdt="novess_5279"; mdt="novess_allyrs";
vartype="lognC"

reg_strs <- c("regBepo")


for (resdir in alldirs[5:16]) {
  outdir <- paste0(resdir,"/resids/")
  dir.create(outdir)
  for(regstr in reg_strs) {
    sp <- splist[[regstr]]
    for(runreg in reglist[[regstr]]) {
      for(vartype in c("lognC")) {
        for(mdn in 1:4) {
          mdt <- c("novess_allyrs","boat_allyrs","novess_5279","vessid_79nd")[mdn]
          mdti <- c("1952-present no vessid","1952-present vessid","1952-1979 no vessid","1979-present vessid")[mdn]
          modtype <- paste(vartype,mdt,sep="_")
          fname <- paste0("Joint_",regstr,"_R",runreg)
          if(file.exists(paste0(resdir,fname,"_",modtype,"_model.RData"))){
            load(paste0(resdir,fname,"_",modtype,"_model.RData"))
 #           if(resdir %in% alldirs[5:6]) mod <- mod1
            a <- mod$data
            b <- mod$residuals
            ncl <- length(unique(a$clust))
            if(ncl > 0) {
              mf <- c(5,5)
              if (ncl <= 20) mf <- c(5,4)
              if (ncl <= 16) mf <- c(4,4)
              if (ncl <= 9) mf <- c(3,3)
              if (ncl <= 4) mf <- c(2,2)
              if (ncl == 1) mf <- c(1,1)
              resplot_med_by_yq_cl(a,b,mf,outdir,fname,modtype)
              resplot_all_by_yq_cl(a,b,mf,outdir,fname,modtype)
              resplot_med_by_map_cl(a,b,mf,outdir,fname,modtype)
            }
            a$flag <- substring(as.character(a$vessid),1,1)
            nfl <- length(unique(a$flag))
            mf <- c(2,2)
            if (nfl == 1) mf <- c(1,1)
            resplot_med_by_yq_flg(a,b,mf,outdir,fname,modtype)
            resplot_all_by_yq_flg(a,b,mf,outdir,fname,modtype)
            resplot_med_by_map_flg(a,b,mf,outdir,fname,modtype)
            graphics.off()
          }
        }
      }
    }
  }
}


# Spatial temporal residual plots
for (resdir in alldirs) {
  for (regstr in reg_strs) {
    for (r in reglist[[regstr]]) {
      outdir <- paste0(resdir,"spatial/")
      dir.create(outdir)
      runreg <- r
      mdn <- 2
      mdt <- c("novess_allyrs","boat_allyrs","novess_5279","vessid_79nd")[mdn]
      mdti <- c("1952-present no vessid","1952-present vessid","1952-1979 no vessid")[mdn]
      sp <- splist[[regstr]]
      vartype <- c("lognC")
      modtype <- paste(vartype,mdt,sep="_")
      fname <- paste0("Joint_",regstr,"_R",runreg)
      load(paste0(resdir,fname,"_",modtype,"_model.RData"))
      a <- mod$data
      b <- mod$residuals
      #for (fl in sort(unique(a$flag))) {
      #  loc <- as.character(a$flag)==fl
      loc <- TRUE
      ll <- as.numeric(unlist(strsplit(as.character(a$latlong),"_")))
      a$lat <- ll[seq(1,length(ll),2)]
      a$lon <- ll[seq(2,length(ll),2)]
      bb <- tapply(b[loc],list(as.character(a$lat[loc]), a$yrqtr),median)
      str(bb)
      yq <- sort(unique(a$yrqtr))
      lats <- sort(as.numeric(dimnames(bb)[[1]]))
      ncl <- length(lats)
      mf <- c(5,5)
      if (ncl <= 20) mf <- c(5,4)
      if (ncl <= 16) mf <- c(4,4)
      if (ncl <= 9) mf <- c(3,3)
      if (ncl <= 4) mf <- c(2,2)
      if (ncl == 1) mf <- c(1,1)
      windows(18,14);par(mfrow=mf,mar=c(2,2,2,0),oma=c(0,0,2,0))
      for (lat in rev(lats)) {
        i <- match(lat, as.numeric(dimnames(bb)[[1]]))
        plot(yq, bb[i,], main = lat)
        mtext(paste(fname,modtype),side=3,outer=T,line=0)
      }
      savePlot(paste0(outdir,fname,"_lats.png"),type="png")

      bb <- tapply(b[loc],list(as.character(a$lon[loc]), a$yrqtr),median)
      lons <- sort(as.numeric(dimnames(bb)[[1]]))
      ncl <- length(lons)
      mf <- c(6,6)
      if (ncl <= 25) mf <- c(5,5)
      if (ncl <= 16) mf <- c(4,4)
      if (ncl <= 9) mf <- c(3,3)
      if (ncl <= 4) mf <- c(2,2)
      windows(18,14);par(mfrow=mf,mar=c(2,2,2,0),oma=c(0,0,2,0))
      for (lon in (lons)) {
        i <- match(lon, as.numeric(dimnames(bb)[[1]]))
        lonnm <- lon # for EPO
        lonnm[lonnm < 0] <- lonnm[lonnm < 0] + 360 # for EPO
        plot(yq, bb[i,], main = lonnm) # for EPO
        mtext(paste(fname,modtype),side=3,outer=T,line=0)
      }
      savePlot(paste0(outdir,fname,"_lons.png"),type="png")

      bb <- tapply(b[loc],list(as.character(a$latlong[loc]), a$yrqtr),median)
      latlons <- dimnames(bb)[[1]]
      res <- data.frame(ll = latlons)
      res$est <- NA
      for (i in 1:length(latlons)) {
        bb2 <- bb[i,]
        bbg <- bb2[!is.na(bb2)]
        yqe <- as.numeric(names(bbg))
        res$est[i] <- glm(bbg ~ yqe)$coefficients[2]
      }
      ll <- as.numeric(unlist(strsplit(as.character(res$ll),"_")))
      res$lat <- ll[seq(1,length(ll),2)]
      res$lon <- ll[seq(2,length(ll),2)]
      res$lon[res$lon < 0] <- res$lon[res$lon < 0] + 360 # for EPO
      bb3 <- tapply(res$est,list(res$lon,res$lat),mean)
      windows(width = 18, height = 14)
      image(sort(unique(res$lon)),sort(unique(res$lat)),bb3,xlab="Lon",ylab="Lat",main=paste0(fname," Residual trends by cell"),breaks = seq(-0.07, 0.075, length.out = 31), col=heat.colors(30))
      contour(sort(unique(res$lon)),sort(unique(res$lat)),100*bb3,add=T,levels=seq(-10,10,length.out=41))
      map(database = "world2", add=TRUE,fill=TRUE) # world2 for EPO
      savePlot(paste0(outdir,fname,"_Res trends.png"), type = "png")
      # Table of strata sample sizes

      a <- mod$data
      samps <- tapply(a$latlong, list(a$latlong, a$yrqtr), length)
      windows()
      table(samps)
      hist(samps, breaks = seq(0, 1000, 1), xlim = c(0, max(samps, na.rm = TRUE)), main = paste0(fname," Histogram of sample sizes by stratum"))
      savePlot(paste0(outdir,fname,"ss_by_stratum_R", r, ".png"), type = "png")
      graphics.off()
    }
  }
}

#########################################################
# Replaced by the more efficient code above.
#
# for (resdir in alldirs[1:2]) {
#   outdir <- paste0(resdir,"/diags/")
#   dir.create(outdir)
#   for(regstr in reg_strs) {
#     sp <- splist[[regstr]]
#     for(runreg in reglist[[regstr]]) {
#       for(vartype in c("lognC")) {
#         for(mdn in 1:4) {
#           mdt <- c("novess_allyrs","boat_allyrs","novess_5279","vessid_79nd")[mdn]
#           mdti <- c("1952-present no vessid","1952-present vessid","1952-1979 no vessid","1979-present vessid")[mdn]
#           modtype <- paste(vartype,mdt,sep="_")
#           fname <- paste0("Joint_",regstr,"_R",runreg)
#           if(file.exists(paste0(resdir,fname,"_",modtype,"_model.RData"))){
#             load(paste0(resdir,fname,"_",modtype,"_model.RData"))
#             if(resdir %in% alldirs[5:6]) mod <- mod1
#             a <- mod$data
#             b <- mod$residuals
#             ncl <- length(unique(a$clust))
#             mf <- c(5,5)
#             if (ncl <= 20) mf <- c(5,4)
#             if (ncl <= 16) mf <- c(4,4)
#             if (ncl <= 9) mf <- c(3,3)
#             if (ncl <= 4) mf <- c(2,2)
#             if (ncl == 1) mf <- c(1,1)
#             windows(10,10);par(mfrow=mf,mar=c(4,4,2,1),oma=c(0,0,2,0))
#             for (cl in sort(unique(a$clust))) {
#               loc <- as.character(a$clust)==cl
#               bb <- tapply(b[loc],as.numeric(as.character(a$yrqtr[loc])),median)
#               plot(as.numeric(names(bb)),bb,xlab="Year-quarter",ylab="Median of residuals",main=cl)
#               mtext(paste(fname,modtype),side=3,outer=T,line=0)
#             }
#             savePlot(paste0(outdir,fname,"_",modtype,"_medres_by_yq_cl.png"),type="png")
#           }
#         }
#         graphics.off()
#       }
#     }
#   }
# }
#
#  for (resdir in alldirs[1:3]) {
#   outdir <- paste0(resdir,"/diags/")
#   dir.create(outdir)
#   for(regstr in reg_strs) {
#     sp <- splist[[regstr]]
#     for(runreg in reglist[[regstr]]) {
#       for(vartype in c("lognC")) {
#         for(mdn in 1:4) {
#           mdt <- c("novess_allyrs","boat_allyrs","novess_5279","vessid_79nd")[mdn]
#           mdti <- c("1952-present no vessid","1952-present vessid","1952-1979 no vessid","1979-present vessid")[mdn]
#           modtype <- paste(vartype,mdt,sep="_")
#           fname <- paste0("Joint_",regstr,"_R",runreg)
#           if(file.exists(paste0(resdir,fname,"_",modtype,"_model.RData"))){
#             load(paste0(resdir,fname,"_",modtype,"_model.RData"))
#             if(resdir %in% alldirs[5:6]) mod <- mod1
#             a <- mod$data
#             b <- mod$residuals
#             a$flag <- substring(as.character(a$vessid),1,1)
#             nfl <- length(unique(a$flag))
#             mf <- c(2,2)
#             if (nfl == 1) mf <- c(1,1)
#             windows(10,10);par(mfrow=mf,mar=c(4,4,2,1),oma=c(0,0,2,0))
#             xrange=range(as.numeric(as.character(a$yrqtr)))
#             for (fl in sort(unique(a$flag))) {
#               loc <- as.character(a$flag)==fl
#               bb <- tapply(b[loc],as.numeric(as.character(a$yrqtr[loc])),median)
#               plot(as.numeric(names(bb)),bb,xlim=xrange,xlab="Year-quarter",ylab="Median of residuals",main=switch(fl,J="JP",K="KR",T="TW",S="SY", U = "US"))
#               mtext(paste(fname,modtype),side=3,outer=T,line=0)
#             }
#             savePlot(paste0(outdir,fname,"_",modtype,"_medres_by_yq_flg.png"),type="png")
#           }
#         }
#         graphics.off()
#       }
#     }
#   }
# }
#
# for (resdir in alldirs[1:3]) {
#   outdir <- paste0(resdir,"/diags/")
#   dir.create(outdir)
#   for(regstr in reg_strs) {
#     sp <- splist[[regstr]]
#     for(runreg in reglist[[regstr]]) {
#       for(vartype in c("lognC")) {
#         for(mdn in 1:4) {
#           mdt <- c("novess_allyrs","boat_allyrs","novess_5279","vessid_79nd")[mdn]
#           mdti <- c("1952-present no vessid","1952-present vessid","1952-1979 no vessid","1979-present vessid")[mdn]
#           modtype <- paste(vartype,mdt,sep="_")
#           fname <- paste0("Joint_",regstr,"_R",runreg)
#           if(file.exists(paste0(resdir,fname,"_",modtype,"_model.RData"))){
#             load(paste0(resdir,fname,"_",modtype,"_model.RData"))
#             if(resdir %in% alldirs[5:6]) mod <- mod1
#             a <- mod$data
#             b <- mod$residuals
#             a$flag <- substring(as.character(a$vessid),1,1)
#             nfl <- length(unique(a$flag))
#             mf <- c(2,2)
#             if (nfl == 1) mf <- c(1,1)
#             windows(10,10);par(mfrow=mf,mar=c(4,4,2,1),oma=c(0,0,2,0))
#             xrange=range(as.numeric(as.character(a$yrqtr)))
#             for (fl in sort(unique(a$flag))) {
#               loc <- as.character(a$flag)==fl
#               boxplot(b[loc] ~ as.numeric(as.character(a$yrqtr[loc])),xlab="Year-quarter",ylab="Residuals",main=switch(fl,J="JP",K="KR",T="TW",S="SY"))
#               mtext(paste(fname,modtype),side=3,outer=T,line=0)
#             }
#             savePlot(paste0(outdir,fname,"_",modtype,"_allres_by_yq_flg.png"),type="png")
#           }
#         }
#         graphics.off()
#       }
#     }
#   }
# }
#
# for (resdir in alldirs[1:2]) {
#   outdir <- paste0(resdir,"/diags/")
#   dir.create(outdir)
#   for(regstr in reg_strs) {
#     sp <- splist[[regstr]]
#     for(runreg in reglist[[regstr]]) {
#       for(vartype in c("lognC")) {
#         for(mdn in 1:4) {
#           mdt <- c("novess_allyrs","boat_allyrs","novess_5279","vessid_79nd")[mdn]
#           mdti <- c("1952-present no vessid","1952-present vessid","1952-1979 no vessid","1979-present vessid")[mdn]
#           modtype <- paste(vartype,mdt,sep="_")
#           fname <- paste0("Joint_",regstr,"_R",runreg)
#           if(file.exists(paste0(resdir,fname,"_",modtype,"_model.RData"))){
#             load(paste0(resdir,fname,"_",modtype,"_model.RData"))
#             if(resdir %in% alldirs[5:6]) mod <- mod1
#             a <- mod$data
#             b <- mod$residuals
#             ncl <- length(unique(a$clust))
#             mf <- c(5,5)
#             if (ncl <= 20) mf <- c(5,4)
#             if (ncl <= 16) mf <- c(4,4)
#             if (ncl <= 9) mf <- c(3,3)
#             if (ncl <= 4) mf <- c(2,2)
#             if (ncl == 1) mf <- c(1,1)
#             windows(10,10);par(mfrow=mf,mar=c(4,4,2,1),oma=c(0,0,2,0))
#             for (cl in sort(unique(a$clust))) {
#               loc <- as.character(a$clust)==cl
#               boxplot(b[loc] ~ as.numeric(as.character(a$yrqtr[loc])),xlab="Year-quarter",ylab="Residuals",main=cl)
#               mtext(paste(fname,modtype),side=3,outer=T,line=0)
#             }
#             savePlot(paste0(outdir,fname,"_",modtype,"_allres_by_yq_cl.png"),type="png")
#           }
#         }
#         graphics.off()
#       }
#     }
#   }
# }
#
# library(maps)
# library(maptools)
# library(mapdata)
#
# for (resdir in alldirs[1:3]) {
#   outdir <- paste0(resdir,"/diags/")
#   dir.create(outdir)
#   for(regstr in reg_strs) {
#     sp <- splist[[regstr]]
#     for(runreg in reglist[[regstr]]) {
#       for(vartype in c("lognC")) {
#         for(mdn in 1:4) {
#           mdt <- c("novess_allyrs","boat_allyrs","novess_5279","vessid_79nd")[mdn]
#           mdti <- c("1952-present no vessid","1952-present vessid","1952-1979 no vessid","1979-present vessid")[mdn]
#           modtype <- paste(vartype,mdt,sep="_")
#           fname <- paste0("Joint_",regstr,"_R",runreg)
#           if(file.exists(paste0(resdir,fname,"_",modtype,"_model.RData"))){
#             load(paste0(resdir,fname,"_",modtype,"_model.RData"))
#             if(resdir %in% alldirs[5:6]) mod <- mod1
#             a <- mod$data
#             b <- mod$residuals
#             a$flag <- substring(as.character(a$vessid),1,1)
#             nfl <- length(unique(a$flag))
#             mf <- c(2,2)
#             if (nfl == 1) mf <- c(1,1)
#             windows(10,10);par(mfrow=mf,mar=c(4,4,2,1),oma=c(0,0,2,0))
#             for (fl in sort(unique(a$flag))) {
#               loc <- as.character(a$flag)==fl
#               bb <- tapply(b[loc],as.character(a$latlong[loc]),median)
#               ll <- as.numeric(unlist(strsplit(names(bb),"_")))
#               bb2 <- data.frame(med=bb)
#               bb2$lat <- ll[seq(1,length(ll),2)]
#               bb2$lon <- ll[seq(2,length(ll),2)]
#               bb3 <- tapply(bb2$med,list(bb2$lon,bb2$lat),mean)
#               if(min(dim(bb3)) > 1) {
#                 image(sort(unique(bb2$lon)),sort(unique(bb2$lat)),bb3,xlab="Lon",ylab="Lat",main=switch(fl,J="JP",K="KR",T="TW",S="SY"),breaks=seq(-1.5,1.5,length.out=31),col=heat.colors(30))
#                 contour(sort(unique(bb2$lon)),sort(unique(bb2$lat)),bb3,add=T,breaks=seq(-5,5,length.out=101))
#                 map(add=TRUE,fill=TRUE)
#                 mtext(paste(fname,modtype),side=3,outer=T,line=0)
#               }
#             }
#             savePlot(paste0(outdir,fname,"_",modtype,"_medres_by_map_flg.png"),type="png")
#           }
#         }
#         graphics.off()
#       }
#     }
#   }
# }
#
# for (resdir in alldirs[1:2]) {
#   outdir <- paste0(resdir,"/diags/")
#   dir.create(outdir)
#   for(regstr in reg_strs) {
#     sp <- splist[[regstr]]
#     for(runreg in reglist[[regstr]]) {
#       for(vartype in c("lognC")) {
#         for(mdn in 1:4) {
#           mdt <- c("novess_allyrs","boat_allyrs","novess_5279","vessid_79nd")[mdn]
#           mdti <- c("1952-present no vessid","1952-present vessid","1952-1979 no vessid","1979-present vessid")[mdn]
#           modtype <- paste(vartype,mdt,sep="_")
#           fname <- paste0("Joint_",regstr,"_R",runreg)
#           if(file.exists(paste0(resdir,fname,"_",modtype,"_model.RData"))){
#             load(paste0(resdir,fname,"_",modtype,"_model.RData"))
#             if(resdir %in% alldirs[5:6]) mod <- mod1
#             a <- mod$data
#             b <- mod$residuals
#             ncl <- length(unique(a$clust))
#             mf <- c(5,5)
#             if (ncl <= 20) mf <- c(5,4)
#             if (ncl <= 16) mf <- c(4,4)
#             if (ncl <= 9) mf <- c(3,3)
#             if (ncl <= 4) mf <- c(2,2)
#             if (ncl == 1) mf <- c(1,1)
#             windows(10,10);par(mfrow=mf,mar=c(2,2,2,0),oma=c(0,0,2,0))
#             for (cl in sort(unique(a$clust))) {
#               loc <- as.character(a$clust)==cl
#               bb <- tapply(b[loc],as.character(a$latlong[loc]),median)
#               ll <- as.numeric(unlist(strsplit(names(bb),"_")))
#               bb2 <- data.frame(med=bb)
#               bb2$lat <- ll[seq(1,length(ll),2)]
#               bb2$lon <- ll[seq(2,length(ll),2)]
#               bb3 <- tapply(bb2$med,list(bb2$lon,bb2$lat),mean)
#               if(min(dim(bb3)) > 1) {
#                 image(sort(unique(bb2$lon)),sort(unique(bb2$lat)),bb3,xlab="Lon",ylab="Lat",main=cl,breaks=seq(-1.5,1.5,length.out=31),col=heat.colors(30))
#                 contour(sort(unique(bb2$lon)),sort(unique(bb2$lat)),bb3,add=T,breaks=seq(-5,5,length.out=101))
#                 map(add=TRUE,fill=TRUE)
#                 mtext(paste(fname,modtype),side=3,outer=T,line=0)
#               }
#             }
#             savePlot(paste0(outdir,fname,"_",modtype,"_medres_by_map_cl.png"),type="png")
#           }
#         }
#         graphics.off()
#       }
#     }
#   }
# }
#
#

############ End of residual plots ##################

############ Start of comparison plots ##############

jntests <- paste0(jointdir, "analyses/cl0_hb1_hk1/outputs/" )
TWests <- paste0(TWdir, "analyses/cl0_hb1_hk1/outputs/" )
JPests <- paste0(JPdir, "analyses/cl0_hb1_hk1/outputs/" )
KRests <- paste0(KRdir, "analyses/cl0_hb1_hk1/outputs/" )

jntR1 <- read.csv(paste0(jntests, "Joint_regBepo_R1_dellog_boat_allyrs_yq.csv"))
TWR1 <- read.csv(paste0(TWests, "Joint_regBepo_R1_dellog_boat_allyrs_yq.csv"))
JPR1 <- read.csv(paste0(JPests, "Joint_regBepo_R1_dellog_boat_allyrs_yq.csv"))
KRR1 <- read.csv(paste0(KRests, "Joint_regBepo_R1_dellog_boat_allyrs_yq.csv"))

windows(10,10)
plot(TWR1$yq, TWR1$pr, type = "l", col = 2, xlab = "Year-quarter", ylab = "ests",ylim = c(0, 3), xlim = c(1979, 2018))
lines(JPR1$yq, JPR1$pr, type = "l", col = 1)
lines(KRR1$yq, KRR1$pr, type = "l", col = 3)
lines(jntR1$yq, jntR1$pr, type = "l", col = 4, lwd=2)
legend("topright", legend = c("JP", "TW","KR", "joint"), col = 1:4, lty = 1, lwd = c(1,1,1,2))
savePlot("compare_indices all R1", type = "png")

matchyq <- function(a,b) {
  a$pr/b$pr[match(a$yq, b$yq)]
}

windows(10,10)
plot(TWR1$yq, matchyq(TWR1, jntR1), type = "l", col = 2, xlab = "Year-quarter", ylab = "ests",ylim = c(0, 2), xlim = c(1979, 2018))
lines(JPR1$yq, matchyq(JPR1, jntR1), type = "l", col = 1)
lines(KRR1$yq, matchyq(KRR1, jntR1), type = "l", col = 3)
lines(jntR1$yq, matchyq(jntR1, jntR1), type = "l", col = 4, lwd=2)
legend("topright", legend = c("JP", "TW","KR", "joint"), col = 1:4, lty = 1, lwd = c(1,1,1,2))
savePlot("compare_indices ratios all R1", type = "png")

jntests011 <- paste0(jointdir, "analyses/cl0_hb1_hk1/outputs/" )
jntests001 <- paste0(jointdir, "analyses/cl0_hb0_hk1/outputs/" )
jntests101 <- paste0(jointdir, "analyses/cl1_hb0_hk1/outputs/" )
jntests111 <- paste0(jointdir, "analyses/cl1_hb1_hk1/outputs/" )
jt1_011 <- read.csv(paste0(jntests011, "Joint_regBepo_R1_dellog_boat_allyrs_yq.csv"))
jt1_001 <- read.csv(paste0(jntests001, "Joint_regBepo_R1_dellog_boat_allyrs_yq.csv"))
jt1_101 <- read.csv(paste0(jntests101, "Joint_regBepo_R1_dellog_boat_allyrs_yq.csv"))
jt1_111 <- read.csv(paste0(jntests111, "Joint_regBepo_R1_dellog_boat_allyrs_yq.csv"))

windows(10,10)
plot(jt1_011$yq, jt1_011$pr, type = "l", col = 1, lwd=2, xlab = "Year-quarter", ylab = "ests",ylim = c(0, 3), xlim = c(1979, 2018))
lines(jt1_001$yq, jt1_001$pr, type = "l", col = 2)
lines(jt1_101$yq, jt1_101$pr, type = "l", col = 3, lwd=1)
lines(jt1_111$yq, jt1_111$pr, type = "l", col = 4, lwd=1)
legend("topright", legend = c("cl0_hb1_hk1", "cl0_hb0_hk1","cl1_hb0_hk1", "cl1_hb1_hk1"), col = 1:4, lty = 1, lwd = c(2,1,1,1))
savePlot("compare_indices jnt R1", type = "png")

matchyq <- function(a,b) {
  a$pr/b$pr[match(a$yq, b$yq)]
}

windows(10,10)
plot(jt1_011$yq, matchyq(jt1_011,jt1_011), type = "l", col = 1, lwd=2, xlab = "Year-quarter", ylab = "ests",ylim = c(0.5, 1.5), xlim = c(1979, 2018))
lines(jt1_011$yq, matchyq(jt1_001,jt1_011), type = "l", col = 2)
lines(jt1_011$yq, matchyq(jt1_101,jt1_011), type = "l", col = 3)
lines(jt1_011$yq, matchyq(jt1_111,jt1_011), type = "l", col = 4)
legend("topright", legend = c("cl0_hb1_hk1", "cl0_hb0_hk1","cl1_hb0_hk1", "cl1_hb1_hk1"), col = 1:4, lty = 1, lwd = c(2,1,1,1))
savePlot("compare_ratios jnt R1", type = "png")



#########################################################################

library(readxl)
a <- excel_sheets("~/../Google Drive/My papers/IOTC/WPTT/2015-17/IOTC-2015-WPTT17-DATA03 - std_cpue_JP_LL_yft_2015.xlsx")
dat <- read_excel("~/../Google Drive/My papers/IOTC/WPTT/2015-17/IOTC-2015-WPTT17-DATA03 - std_cpue_JP_LL_yft_2015.xlsx",sheet=4)
dat$yrqtr <- dat$yr + dat$qt/4 - 0.125
YR2 <- dat[dat$LT5LN5=="Y" & dat$area==2,]
YR3 <- dat[dat$LT5LN5=="Y" & dat$area==3,]
YR4 <- dat[dat$LT5LN5=="Y" & dat$area==4,]
YR5 <- dat[dat$LT5LN5=="Y" & dat$area==5,]
for (r in c(2,5,3,4)) {
  a <- get(paste0("YR",r))
  a$relcpue = a$cpue_p / mean(a$cpue_p,na.rm=TRUE)
  assign(paste0("YR",r),a)
}


a <- excel_sheets("~/../Google Drive/My papers/IOTC/WPTT/2013-15/JPN_LL_BET_standaridized_CPUE_rev2.xlsx")
dat2 <- read_excel("~/../Google Drive/My papers/IOTC/WPTT/2013-15/JPN_LL_BET_standaridized_CPUE_rev2.xlsx",sheet=5,skip=1)
names(dat2)[c(5,10,15)] = "relcpue"
dat3 <- rbind(cbind(dat2[,1:5],rr=1),cbind(dat2[,6:10],rr=2),cbind(dat2[,11:15],rr=34))
dat3$yrqtr <- dat3$year + dat3$quarter/4 - 0.125
BR1 <- dat3[dat3$rr==1,]
BR2 <- dat3[dat3$rr==2,]
BR34 <- dat3[dat3$rr==34,]


windows(10,10);par(mfrow=c(2,2))
for (r in c(2,5,3,4)) {
  with(get(paste0("YR",r)),plot(yrqtr,relcpue,xlab="Year-quarter",ylab="",main =paste("Region",r),type="l"))
}

windows(10,10);par(mfrow=c(2,2))
for (r in c(1,2,34)) {
  with(get(paste0("BR",r)),plot(yrqtr,relcpue,xlab="Year-quarter",ylab="",main =paste("Region",r),type="l"))
}

BNR1 <- read.csv("~/../OneDrive/CPUE_LL_2016/2016_secondmeet/joint/std_nocl_xTW_hbf/outputs/Joint_regB2_R1_dellog_boat_allyrs.csv")
BNR2 <- read.csv("~/../OneDrive/CPUE_LL_2016/2016_secondmeet/joint/std_nocl_xTW_hbf/outputs/Joint_regB2_R2_dellog_boat_allyrs.csv")
BNR3 <- read.csv("~/../OneDrive/CPUE_LL_2016/2016_secondmeet/joint/std_xTW/outputs/Joint_regB2_R3_dellog_boat_allyrs.csv")
BNR4 <- read.csv("~/../OneDrive/CPUE_LL_2016/2016_secondmeet/joint/std_xTW/outputs/Joint_regB2_R4_dellog_boat_allyrs.csv")

YNR2 <- read.csv("~/../OneDrive/CPUE_LL_2016/2016_secondmeet/joint/std_nocl_xTW_hbf/outputs/Joint_regY_R2_dellog_boat_allyrs.csv")
YNR5 <- read.csv("~/../OneDrive/CPUE_LL_2016/2016_secondmeet/joint/std_nocl_xTW_hbf/outputs/Joint_regY_R5_dellog_boat_allyrs.csv")
YNR3 <- read.csv("~/../OneDrive/CPUE_LL_2016/2016_secondmeet/joint/std_xTW/outputs/Joint_regY_R3_dellog_boat_allyrs.csv")
YNR4 <- read.csv("~/../OneDrive/CPUE_LL_2016/2016_secondmeet/joint/std_xTW/outputs/Joint_regY_R4_dellog_boat_allyrs.csv")


resdirs <- c(paste0(KRdir,"analyses/std_cl_KRonly_hbf/"),
             paste0(JPdir,"analyses/std_cl_JPonly_hbf/"),
             paste0(TWdir,"analyses/std_cl_TWonly_hbf/"),
             paste0(SYdir,"analyses/std_cl_SYonly_hbf/"),
             paste0(projdir, "joint/analyses/std_cl_hbf/"),
             paste0(projdir, "joint/analyses/std_cl_hbf_nohook/"),
             paste0(projdir,"joint/analyses/std_cl_hbf_nohook_yqll5/"))


r1_nh_st <- read.csv(paste0(resdirs[6], "outputs_test/Joint_regB_R1_dellog_novess_5279.csv"))
r1_nh_nd <- read.csv(paste0(resdirs[6], "outputs_test/Joint_regB_R1_dellog_vessid_79nd.csv"))
r1_yl5_st <- read.csv(paste0(resdirs[7], "outputs_test/Joint_regB_R1_dellog_novess_5279.csv"))
r1_yl5_nd <- read.csv(paste0(resdirs[7], "outputs_test/Joint_regB_R1_dellog_vessid_79nd.csv"))

r2_nh_st <- read.csv(paste0(resdirs[6], "outputs_test/Joint_regB_R2_dellog_novess_5279.csv"))
r2_nh_nd <- read.csv(paste0(resdirs[6], "outputs_test/Joint_regB_R2_dellog_vessid_79nd.csv"))
r2_yl5_st <- read.csv(paste0(resdirs[7], "outputs_test/Joint_regB_R2_dellog_novess_5279.csv"))
r2_yl5_nd <- read.csv(paste0(resdirs[7], "outputs_test/Joint_regB_R2_dellog_vessid_79nd.csv"))

r3_nh_st <- read.csv(paste0(resdirs[6], "outputs_test/Joint_regB_R3_dellog_novess_5279.csv"))
r3_nh_nd <- read.csv(paste0(resdirs[6], "outputs_test/Joint_regB_R3_dellog_vessid_79nd.csv"))
r3_yl5_st <- read.csv(paste0(resdirs[7], "outputs_test/Joint_regB_R3_dellog_novess_5279.csv"))
r3_yl5_nd <- read.csv(paste0(resdirs[7], "outputs_test/Joint_regB_R3_dellog_vessid_79nd.csv"))



windows(10,10);par(mfrow=c(3,2))
for (rr in c(1:3)) {
  for (tm in c("st", "nd")) {
    a1 <- get(paste0("r",rr,"_nh_",tm))
    a2 <- get(paste0("r",rr,"_yl5_",tm))
    plot(a1$yq,a1$pr,xlab="yrqtr",ylab="Relative CPUE",main=paste("Region",rr),type="l")
    lines(a2$yq,a2$pr,col=2)
  }
}
legend("topright",legend=c("YQLL1 indices","YQLL5 indices"),lwd=1,col=c(1,2))
savePlot(paste0(jointdir,"yqll5_comps1.png"),type="png")


windows(10,10);par(mfrow=c(3,2), oma = c(0,0,1,0))
for (rr in c(1:3)) {
  for (tm in c("st", "nd")) {
    a1 <- get(paste0("r",rr,"_nh_",tm))
    a2 <- get(paste0("r",rr,"_yl5_",tm))
    yql <- sort(unique(a1$yq))
    rat <- a2$pr[match(yql, a2$yq)] / a1$pr[match(yql, a1$yq)]
    plot(yql,rat,xlab="yrqtr",ylab="Relative CPUE",main=paste("Region",rr),type="l", ylim = c(0.5,2))
  }
}
#legend("bottomleft",legend=c("Ratio YQLL1 / YQLL5"),lwd=1,col=c(1))
title(c("Ratio YQLL1 / YQLL5"), outer = TRUE)
savePlot(paste0(jointdir,"yqll5_ratios.png"),type="png")


windows(10,10);par(mfrow=c(2,2))
for(rr in c(2,5,3,4)) {
  a1 <- get(paste0("YR",rr))
  a2 <- get(paste0("YNR",rr))
  a1$cp <- a1$relcpue / mean(a1$relcpue[a1$yrqtr > 1980 & a1$yrqtr < 2000],na.rm=TRUE)
  a2$cp <- a2$pr / mean(a2$pr[a2$yq > 1980 & a2$yq < 2000],na.rm=TRUE)
  yql <- sort(unique(a1$yrqtr))
  div = a2$cp[match(yql,a2$yq)]/a1$cp[match(yql,a1$yrqtr)]
  plot(yql,div,xlab="yrqtr",ylab="Ratio of CPUE",main=paste("Region",rr),type="l",xlim=c(1952,2016),ylim=c(0,5))
}
savePlot(paste0(basedir,"YFT_2015_ratio.png"),type="png")

windows(10,10);par(mfrow=c(2,2))
for(rr in c(1,2,3,4)) {
  if(rr <  3) a1 <- get(paste0("BR",rr))
  if(rr >= 3) a1 <- get("BR34")
  a2 <- get(paste0("BNR",rr))
  a1$cp <- a1$relcpue / mean(a1$relcpue[a1$yrqtr > 1980 & a1$yrqtr < 2000],na.rm=TRUE)
  a2$cp <- a2$pr / mean(a2$pr[a2$yq > 1980 & a2$yq < 2000],na.rm=TRUE)
  plot(a1$yrqtr,a1$cp,xlab="yrqtr",ylab="Relative CPUE",main=paste("Region",rr),type="l",xlim=c(1952,2016),ylim=c(0,3))
  lines(a2$yq,a2$cp,col=2)
  if(rr==1) legend("topright",legend=c("2015 JP indices","2016 joint indices"),lwd=1,col=c(1,2))
}
savePlot(paste0(basedir,"BET_2015_comps.png"),type="png")

windows(10,10);par(mfrow=c(2,2))
for(rr in c(1,2,3,4)) {
  if(rr <  3) a1 <- get(paste0("BR",rr))
  if(rr >= 3) a1 <- get("BR34")
  a2 <- get(paste0("BNR",rr))
  a1$cp <- a1$relcpue / mean(a1$relcpue[a1$yrqtr > 1980 & a1$yrqtr < 2000],na.rm=TRUE)
  a2$cp <- a2$pr / mean(a2$pr[a2$yq > 1980 & a2$yq < 2000],na.rm=TRUE)
  yql <- sort(unique(a1$yrqtr))
  div = a2$cp[match(yql,a2$yq)]/a1$cp[match(yql,a1$yrqtr)]
  plot(yql,div,xlab="yrqtr",ylab="Ratio of CPUE",main=paste("Region",rr),type="l",xlim=c(1952,2016),ylim=c(0,4))
}
savePlot(paste0(basedir,"BET_2015_ratio.png"),type="png")



########################################
# Plots
########################################
basedir <- "~/../OneDrive/CPUE_LL_2016/2016_secondmeet/jointx/"
Rdir <-  paste0(basedir,"../Rfiles/")
resdirs <- c(paste0(basedir,"std_xTW/"),
             paste0(basedir,"std_nocl_JPonly_hbf/"),
             paste0(basedir,"std_nocl_alldat_xTW_hbf/"),
             paste0(basedir,"std_nocl_xTW_hbf/"),
             paste0(basedir,"std_nocl_xTW_nohbf/"))

resd2 <- c(paste0("std_xTW/"),
           paste0("std_nocl_JPonly_hbf/"),
           paste0("std_nocl_alldat_xTW_hbf/"),
           paste0("std_nocl_xTW_hbf/"),
           paste0("std_nocl_xTW_nohbf/"))

#install.packages("survival")
#install.packages("beanplot")
library(beanplot)
library(cluster)
library(data.table)
#install.packages("date")
library("date")
#library(influ)
library(MASS)
library("mgcv")
library("nFactors")
library(plyr)
#install.packages("dplyr")
library(dplyr)
library(splines)
library(survival)
setwd(basedir)
source("../RFiles/support_functions.r")
getwd()

windows();par(mfrow=c(2,2))
for(rr in c(2,5,3,4)) {
  ic=1
  dores=c(1,4)
  plot(1979:2016,1979:2016,type="n",ylim=c(0,4),xlab="Years",ylab="",main=rr)
  for (d in resdirs[dores]) {
    a <- read.csv(paste0(d,"outputs/","Joint_regY_R",rr,"_dellog_boat_allyrsyr.csv"))
    lines(a$yr,a$pr,col=ic,lwd=2)
    ic=ic+1
  }
  legend("topright",legend=resd2[dores],col=1:4,lwd=1)
}
savePlot("YFT_MSE_options.png",type="png")

windows();par(mfrow=c(2,2))
for(rr in c(1,2,3,4)) {
  ic=1
  dores=c(1,4)
  plot(1979:2016,1979:2016,type="n",ylim=c(0,4),xlab="Years",ylab="",main=rr)
  for (d in resdirs[dores]) {
    a <- read.csv(paste0(d,"outputs/","Joint_regB2_R",rr,"_dellog_boat_allyrsyr.csv"))
    lines(a$yr,a$pr,col=ic,lwd=2)
    ic=ic+1
  }
  legend("topright",legend=resd2[dores],col=1:4,lwd=1)
}
savePlot("BET_MSE_options.png",type="png")

# Plot surface
r = 3
load(paste0(resdirs[7], paste0("Joint_regB_R", r, "_lognC_vessid_79nd_model.RData")))
a <- mod$coefficients
a <- a[grep("latlong", names(a))]
lln <- names(a)
lln <- gsub("latlong", "", lln)
requireNamespace("maps")

ll <- as.numeric(unlist(strsplit(lln,"_")))
lats <- ll[seq(1,length(ll),2)]
lons <- ll[seq(2,length(ll),2)]
bb <- data.frame(lats = lats, lons = lons)
bb$coef <- a
bb$expco <- exp(bb$coef)

library(maps)
library(maptools)
bb2 <- with(bb, tapply(expco,list(lons, lats),median))
lt1 <- sort(as.numeric(unique(bb$lats)))
ln1 <- sort(as.numeric(unique(bb$lons)))
ltx <- c(lt1 - 2.5, 25)
lnx <- c(ln1 - 2.5, 15)

windows(width = 14, height = 10)
dim(bb2)
image(ln1, lt1, bb2, ylab = "Lat", xlab = "Long", main = paste("Relative density in Region", r))
contour(ln1, lt1, bb2, levels = seq(0,3,.2), add = TRUE, col = 4)
map(add = TRUE, fill = TRUE)
savePlot(paste0(jointdir, "Relative density R",r), type = "png")


# Predict overall
m1 <- load(paste0(resdirs[7], paste0("Joint_regB_R", 1, "_lognC_vessid_79nd_model.RData")))
m2 <- load(paste0(resdirs[7], paste0("Joint_regB_R", 2, "_lognC_vessid_79nd_model.RData")))
m3 <- load(paste0(resdirs[7], paste0("Joint_regB_R", 3, "_lognC_vessid_79nd_model.RData")))

