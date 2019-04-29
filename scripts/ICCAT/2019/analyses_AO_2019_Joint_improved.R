# Joint standardization
########################################################
options(device = windows)
projdir <- "~/ICCAT/2019_YFT/"
Rdir <- paste0(projdir, "Rfiles/")


brdir <- paste0(projdir, "BR/")
krdir <- paste0(projdir, "KR/")
twdir <- paste0(projdir, "TW/")
jpdir <- paste0(projdir, "JP/")
usdir <- paste0(projdir, "US/")
jointdir <- paste0(projdir, "joint/")

jntalysis_dir <- paste0(jointdir, "analyses/")
dir.create(jointdir)
dir.create(jntalysis_dir)

#install.packages("survival")
#install.packages("stringr")
library(stringr)
library("date")
library(splines)
library("maps")
library("mapdata")
library("maptools")
library("lunar")
library("mgcv")
library(randomForest)
library(influ)
library("nFactors")
library(plyr)
library(dplyr)
library(data.table)
library(cluster)
library(beanplot)
library(survival)

library(cpue.rfmo)

#clkeepCN_Y1 <- list("yft" = list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepJP_Y1 <- list("yft" = list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepBR_Y1 <- list("yft" = list(c(0),c(1,2,3,4,5),c(1,2,3,4)))
clkeepKR_Y1 <- list("yft" = list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepTW_Y1 <- list("yft" = list(c(1,2,3),c(1,2,3),c(1,2,3,4)))
clkeepUS_Y1 <- list("yft" = list(c(1,2,3,4),c(1,2,3,4),c(0)))
clk_Y1 <- list(JP = clkeepJP_Y1, BR = clkeepBR_Y1, KR = clkeepKR_Y1,TW = clkeepTW_Y1,US = clkeepUS_Y1)

clkeepJP_Y2 <- list("yft" = list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepBR_Y2 <- list("yft" = list(c(0),c(1,2,3,4,5),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4,5),c(0)))
clkeepKR_Y2 <- list("yft" = list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepTW_Y2 <- list("yft" = list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepUS_Y2 <- list("yft" = list(c(1,2,3,4),c(1,2,3,4),c(0),c(0),c(0),c(1,2,3,4)))
clk_Y2 <- list(JP = clkeepJP_Y2, BR = clkeepBR_Y2, KR = clkeepKR_Y2,TW = clkeepTW_Y2,US = clkeepUS_Y2)


std_splist <- c("alb","bet","yft")
stdlabs <- c("vessid","yrqtr","latlong","op_yr","hbf","hooks",std_splist,"lat","lon","lat5","lon5","reg","hcltrp","flag")

## ---------------------------------------------
# Run various standardization scenarios.
## ---------------------------------------------

# The runpars define the approach to be used in this run
regY1_minss <- list(minq_byreg = c(3,5,3), minvess=c(20,60,20), minll=c(20,50,20), minyrqtr = c(20,50,20), minyqll = c(3,5,3))

regY2_minss <- list(minq_byreg = c(3,5,3,3,5,3), minvess=c(20,40,20,20,40,20), minll=c(20,40,20,20,40,20), minyrqtr = c(20,40,20,20,40,20), minyqll = c(3,5,3,3,5,3))

regscaleminss <- list(minq_byreg = c(1,1,1), minvess=c(1,1,1), minll=c(10,10,10), minyrqtr = c(5,5,5), minyqll = c(3,3,3))

runpars <- list()
runpars[["regY1"]] <-list(runsp = "yft", regtype2 = "Y1", clk = clk_Y1, doregs = 1:3, addcl = TRUE, dohbf = TRUE, dohook = TRUE,
                          do_lognC = TRUE, do_deltalog = FALSE,
                          do_early = TRUE, do_late = TRUE, do_vessallyr = FALSE,
                          cltype = "hcltrp", minss = regY1_minss, strsmp = 30)

runpars[["regY2"]] <-list(runsp = "yft", regtype2 = "Y2", clk = clk_Y2, doregs = c(2:5), addcl = TRUE, dohbf = TRUE, dohook = TRUE,
                          do_lognC = TRUE, do_deltalog = FALSE,
                          do_early = TRUE, do_late = TRUE, do_vessallyr = FALSE,
                          cltype = "hcltrp", minss = regY2_minss, strsmp = 30)

runpars[["regscale"]] <-list(runsp = "yft", regtype2 = "Y1", clk = clk_Y1, doregs = 1:3, addcl = TRUE, dohbf = TRUE, dohook = TRUE,
                          do_lognC = TRUE, do_deltalog = FALSE,
                          do_early = TRUE, do_late = TRUE, do_vessallyr = TRUE,
                          cltype = "hcltrp", minss = regscaleminss, strsmp = 15)

regstr <- "regY1"; runreg <- 2; keepd <- TRUE; doflags <- "TW"
maxyr <- 2019

# with clusters, hooks, hbf
resdir <- paste0(jntalysis_dir,"cl1_hb1_hk1/")
dir.create(resdir)
setwd(resdir)

options(error = recover)

run_standardization(runpars, doflags = c("JP","KR","TW","BR","US"), regstr = "regY1", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2006 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW","BR","US"), regstr = "regY2", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

# with clusters, hooks, hbf
resdir <- paste0(jntalysis_dir,"cl1_hb1_hk1/")
dir.create(resdir)
setwd(resdir)


# no clusters, with hooks, hbf
resdir <- paste0(jntalysis_dir,"cl0_hb1_hk1/")
dir.create(resdir)
setwd(resdir)



#################################################
###### Get medians and make newdats ############

flaglist <- list(jp="JP",kr="KR",tw="TW",sy="SY",all=c("JP","KR","TW","SY"))
reglist <- list("regB2" = c(1,2,3,4),"regY" = c(2,3,4,5),"regY2" = c(2,7),"regB3" = c(1,5))

datmeds <- list()
for(regtype in c("regB2","regY","regY2","regB3")) {
  print(regtype)
  datmeds[[regtype]] <- regtype
  runsp <- switch(regtype,regB2="bet",regY="yft",regY2="yft",regB3="bet")
  clk <- switch(regtype,regB2=clk_B2,regY=clk_Y,regY2=clk_Y2,regB3=clk_B3)
  jdat <- data.frame()
  for(flag in c("JP", "KR", "TW", "SY")) {
    for(r in runpars[[runsp]]$doregs) {
      load(paste0(projdir,flag,"/clustering/",paste(flag,regtype,r,sep="_"),".RData"))
      dataset$flag <- flag
      dataset$qtr <- revtrunc(defactor(dataset$yrqtr))
      jdat <- rbind(jdat,dataset[,allabs])
      rm(dataset)
    }
  }
  jdat <- jdat[jdat$yrqtr < maxyr,]
  jdat$vessidx <- jdat$vessid
  jdat$vessid <- paste0(jdat$flag,jdat$vessid)
  jdat$vessid <- as.factor(jdat$vessid)
  jdat$lat5 <- as.factor(jdat$lat5)
  jdat$op_yr <- as.factor(jdat$op_yr)
  jdat$qtr <- as.factor(jdat$qtr)
  jdat <- jdat[jdat$yrqtr > 2005 | jdat$flag != "TW",]

  for(fl in  1:5) {
    flagl <- flaglist[[fl]]
    datmeds[[regtype]][[fl]] <- flagl
    print(c(fl, flagl))
    for(runreg in reglist[[regtype]]) {
      dd <- jdat[jdat$flag %in% flagl,]
      minqtrs <- minqtrs_byreg[runreg]
      glmdat <- select_data_JointIO(dd,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,minvess=30,minll=30,minyrqtr=30,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA)
      if(dim(glmdat)[1] > 0) {
        glmdat_meds <- get_base_newdat(glmdat)
        if((length(flagl) > 1 | flagl == "JP")[1]) {
          glmdat5279 <- select_data_JointIO(dd,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars, minvess=30,minll=30,minyrqtr=30,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1952,1980))
          glmdat5279_meds <- get_base_newdat(glmdat5279)
        } else glmdat5279_meds <- ""
        a <- dd[dd$vessid != "JP1",]
        glmdat79nd <- select_data_JointIO(a,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars, minvess=30,minll=30,minyrqtr=30,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1979,maxyr))
        glmdat79nd_meds <- get_base_newdat(glmdat79nd)
        datmeds[[regtype]][[fl]][[runreg]] <- list(flag=flagl, glmdat_meds, glmdat5279_meds, glmdat79nd_meds)
      } else datmeds[[regtype]][[fl]][[runreg]] <- list(flag=flagl, 0,0,0)
    }
  }
}
save(datmeds, file = paste0(projdir, "data_medians.RData"))

# ############ --------------------------------------------------
# Do all-region analysis for a limited number of years

run_reg_scaling(runpars= runpars["regscale"], doflags = c("JP","KR","TW","BR","US"), regstr = "regY1", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2006 , jplimit = list(reg=2, yr=3005))


run_reg_scaling <- function(runpars, yr_range = c(2000, 2018), doflags, regstr, maxyr, stdlabs, projdir, twlimit = 2005, jplimit = list(reg=4, yr=0), keepd = TRUE, krlimit=NA) {

  rp <- runpars[["regscale"]]
  runsp <- rp$runsp
  addcl <- rp$addcl
  dohbf <- rp$dohbf

  if(is.null(rp$do_lognC)) do_lognC <- TRUE else do_lognC <- rp$do_lognC
  if(is.null(rp$do_deltalog)) do_deltalog <- TRUE else do_deltalog <- rp$do_deltalog

  if(is.null(rp$do_vessallyr)) do_vessallyr <- TRUE else do_vessallyr <- rp$do_vessallyr
  if(is.null(rp$do_early)) do_early <- TRUE else do_early <- rp$do_early
  if(is.null(rp$do_late))  do_late  <- TRUE else do_late  <- rp$do_late

  if(is.null(rp$ylall)) ylall <- NA else ylall <- rp$ylall
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
  jdat <- jdat[jdat$yrqtr > yr_range[1] &  jdat$yrqtr < yr_range[2],]
  jdat$vessidx <- jdat$vessid
  jdat$vessid <- paste0(jdat$flag,jdat$vessid)
  jdat$vessid <- as.factor(jdat$vessid)
  jdat <- jdat[jdat$yrqtr > twlimit | jdat$flag != "TW",]

  vars <- c("vessid","hooks","yrqtr","latlong","hbf")
    jdat2 <- jdat[jdat$yrqtr < jplimit$yr | !jdat$reg %in% jplimit$reg | jdat$flag != "JP",]
    if(!is.na(krlimit)) {
      jdat2 <- jdat2[(jdat2$yrqtr > krlimit$yr[1] & jdat2$yrqtr < krlimit$yr[2]) |
                       !jdat2$reg %in% krlimit$reg | jdat2$flag != "KR",]
    }
    glmdat <- data.frame()
    for (runreg in rp$doregs) {
      datx <- select_data_IO2(jdat2,runreg = runreg,runpars = rp, mt = "deltabin",vars = vars, yrlims = ylall, oneflag = onefl)
      glmdat <- rbind(glmdat, datx)
    }

  #  if (!is.na(strsmp) & nrow(glmdat) > 60000)
  #    glmdat <- samp_strat_data(glmdat, strsmp)

    glmdat$.wtt   <- mk_wts(glmdat,wttype = "area")

    fmla.oplogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = T,nhbf = 3, dohook = rp$dohook)
    fmla.oplogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = F,nhbf = 3, dohook = rp$dohook)
    fmla.boatlogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = T,nhbf = 3, dohook = rp$dohook)
    fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = F,nhbf = 3, dohook = rp$dohook)
    glmdat$mn <- with(glmdat,0.1 *  mean(get(runsp)/hooks))

    modlab = "lognC_novess_regscale"; fname <- paste0("Joint_",regstr)
    if (lu(as.character(glmdat$clust)) > 1)
    { model <- glm(as.formula(fmla.oplogn),     data = glmdat, weights = glmdat$.wtt, family = "gaussian", model = keepd);gc() } else
    { model <- glm(as.formula(fmla.oplogn_ncl), data = glmdat, weights = glmdat$.wtt, family = "gaussian", model = keepd);gc() }

    summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);
  #  rm(model)

    if (do_vessallyr) {
      modlab = "lognC_vess_regscale"; fname <- paste0("Joint_",regstr)
      if (lu(glmdat$clust) > 1)
      { model <- glm(as.formula(fmla.boatlogn),    data = glmdat, weights = glmdat$.wtt, family = "gaussian", model = keepd);gc() } else
      { model <- glm(as.formula(fmla.boatlogn_ncl),data = glmdat, weights = glmdat$.wtt, family = "gaussian", model = keepd);gc() }
      summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);

      # Predict
      newdat <- expand.grid(latlong = levels(glmdat$latlong),
                            yrqtr = levels(glmdat$yrqtr),
                            vessid = levels(glmdat$vessid)[1],
                            hbf = median(glmdat$hbf),
                            clust = levels(glmdat$clust)[1],
                            hooks = median(glmdat$hooks))
      newdat$cpue <- exp(predict.glm(model, newdata = newdat, type = "response")) - mn
      # newdat$cpue4 <- exp(predict.glm(model4, newdata = newdat, type = "response")) - mn
      # newdat$cpue5 <- exp(predict.glm(model5, newdata = newdat, type = "response")) - mn
      # newdat$cpue5 <- exp(predict.glm(model5, newdata = newdat, type = "response")) - mn
      # a <- substring(as.character(newdat$latlong), 3)

      newdat$reg <- jdat2$reg[match(newdat$latlong, jdat2$latlong)]
      newdat$area <- 1
      load("C:/Users/simon/Documents/GitHub/cpue.rfmo/cell_areas.RData")
      cell_areas$ln2 <- cell_areas$ln
      cell_areas$ln2[cell_areas$ln2 > 180] <- cell_areas$ln[cell_areas$ln2 > 180] - 360
      cell_areas$latlong2 <- paste(cell_areas$lt+2.5,cell_areas$ln2+2.5, sep="_")

      match(as.character(newdat$latlong)[1:5], cell_areas$latlong2)

      newdat$area <- cell_areas[match(as.character(newdat$latlong), cell_areas$latlong2), "areax"]
      tapply(newdat$cpue*newdat$area, newdat$reg, sum)
      # newdat$area[is.na(newdat$area)] <- 0

      # include all latlong, plus all lat*lon interactions where lat5 < 0 and lon5 > 40 & lon5 < 100
      # newdat7 <- expand.grid(lat5 = sort(unique(glmdat$lat5)), lon5 = sort(unique(glmdat$lon5)), qtr = sort(unique(glmdat$qtr)), yr = levels(glmdat$yr), fl = levels(glmdat$fl)[1])
      # newdat7 <- setup_IO_regions(newdat7, regY2=TRUE, regA4 = TRUE)
      # newdat7$reg <- newdat7[,spreg2]
      # newdat7 <- newdat7[newdat7$reg %in% doreg,]
      # newdat7$latlong <- paste(newdat7$reg, newdat7$lat5, newdat7$lon5, sep = "_")
      # newdat7$reg_qtr <- paste(newdat7$reg, newdat7$qtr, sep = "_")
      # newdat7 <- newdat7[newdat7$reg_qtr %in% sort(unique(glmdat$reg_qtr)),]    # remove reg_qtr values that can't be predicted because no data were in the model
      # newdat7$cpue7 <- exp(predict.gam(model7, newdata = newdat7, type = "response")) - mn
      # a <- substring(as.character(newdat7$latlong), 3)
      # newdat7$area <- cell_areas[match(a, cell_areas$latlong), "areax"]
      # newdat7 <- newdat7[newdat7$latlong %in% newdat6$latlong | (newdat7$lat5 < 0 & newdat7$lon5 > 40 & newdat7$lon5 < 100),]

      # a <- newdat7[!newdat7$latlong %in% newdat6$latlong,]
      # colnames(a)[colnames(a)=="cpue7"] <- "cpue6"
      # newdat8 <- rbind(newdat6, a[,colnames(newdat6)])
      # colnames(newdat8)[colnames(newdat8)=="cpue6"] <- "cpue8"

      llmn23 <- tapply(newdat$cpue, paste(newdat$reg, newdat$latlong), mean)*3000
      regx <- substring(names(llmn23),1,1)
      wts2 <- tapply(llmn23, regx, sum)
      wts2 <- wts2 / max(wts2)
      assign(x = paste0(sp, "_wts2_",pdnm[pd]), value = wts2)

      latlongx <- substring(names(llmn23), 3)
      areax <- cell_areas[match(latlongx, cell_areas$latlong2), "areax"]
      areax[is.na(areax)] <- 0
      wts3 <- tapply(llmn23 * areax, regx, sum)
      wts3 <- wts3 / max(wts3)
      assign(x = paste0(sp, "_wts3_",pdnm[pd]), value = wts3)

      llmn4 <- tapply(newdat$cpue4, newdat$latlong, mean)*3000
      regx <- substring(names(llmn4),1,1)
      wts4 <- tapply(llmn4 * areax, regx, sum)
      wts4 <- wts4 / max(wts4)
      assign(x = paste0(sp, "_wts4_",pdnm[pd]), value = wts4)

      llmn5 <- tapply(newdat$cpue5, newdat$latlong, mean)*3000
      regx <- substring(names(llmn5),1,1)
      wts5 <- tapply(llmn5 * areax, regx, sum)
      wts5 <- wts5 / max(wts5)
      assign(x = paste0(sp, "_wts5_",pdnm[pd]), value = wts5)

      llmn6 <- tapply(newdat6$cpue6, newdat6$latlong, mean)*3000
      regx6 <- substring(names(llmn6),1,1)
      latlongx6 <- substring(names(llmn6), 3)
      areax6 <- cell_areas[match(latlongx6, cell_areas$latlong), "areax"]
      areax6[is.na(areax6)] <- 0
      wts6 <- tapply(llmn6 * areax6, regx6, sum)
      wts6 <- wts6 / max(wts6)
      assign(x = paste0(sp, "_wts6_",pdnm[pd]), value = wts6)

      llmn7 <- tapply(newdat7$cpue7, newdat7$latlong, mean)*3000
      regx7 <- substring(names(llmn7),1,1)
      latlongx7 <- substring(names(llmn7), 3)
      areax7 <- cell_areas[match(latlongx7, cell_areas$latlong), "areax"]
      areax7[is.na(areax7)] <- 0
      wts7 <- tapply(llmn7 * areax7, regx7, sum)
      wts7 <- wts7 / max(wts7)
      assign(x = paste0(sp, "_wts7_",pdnm[pd]), value = wts7)

      llmn8 <- tapply(newdat8$cpue8, newdat8$latlong, mean)*3000
      regx8 <- substring(names(llmn8),1,1)
      latlongx8 <- substring(names(llmn8), 3)
      areax8 <- cell_areas[match(latlongx8, cell_areas$latlong), "areax"]
      areax8[is.na(areax8)] <- 0
      wts8 <- tapply(llmn8 * areax8, regx8, sum)
      wts8 <- wts8 / max(wts8)
      assign(x = paste0(sp, "_wts8_",pdnm[pd]), value = wts8)

      mcalc1 <- tapply(glmdat$sp/glmdat$Effort, glmdat$latlong, mean)*3000
      regx <- substring(names(mcalc1),1,1)
      mwts <- tapply(mcalc1, regx, sum)
      mwts <- mwts / max(mwts)
      assign(x = paste0(sp, "_wts_mean_",pdnm[pd]), value = mwts/max(mwts))
      rm(model)
    }
  }
  graphics.off()
}
