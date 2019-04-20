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
regY2_minss <- list(minq_byreg = c(3,5,3,3,5,3), minvess=c(20,40,10,20,40,20), minll=c(20,40,20,20,40,20), minyrqtr = c(20,40,10,20,40,20), minyqll = c(3,5,3,3,5,3))

runpars <- list()
runpars[["regY1"]] <-list(runsp = "yft", regtype2 = "Y1", clk = clk_Y1, doregs = 1:3, addcl = TRUE, dohbf = TRUE, dohook = TRUE, cltype = "hcltrp", minss = regY1_minss, strsmp = 30)
runpars[["regY2"]] <-list(runsp = "yft", regtype2 = "Y2", clk = clk_Y2, doregs = c(1,2,4,5,6), addcl = TRUE, dohbf = TRUE, dohook = TRUE, cltype = "hcltrp", minss = regY2_minss, strsmp = 30)

regstr <- "regY1"; runreg <- 2; keepd <- TRUE; doflags <- "TW"
maxyr <- 2019

# with clusters, hooks, hbf
resdir <- paste0(jntalysis_dir,"cl1_hb1_hk1/")
dir.create(resdir)
setwd(resdir)

options(error = recover)

run_standardization(runpars, doflags = c("JP","KR","TW","BR","US"), regstr = "regY1", maxyr = 2019, do_early = TRUE, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW","BR","US"), regstr = "regY2", maxyr = 2019, do_early = TRUE, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

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
