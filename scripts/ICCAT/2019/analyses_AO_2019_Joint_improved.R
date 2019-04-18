# Joint standardization
########################################################

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
clkeepKR_Y1 <- list("yft" = list(c(1,2,3,4),c(1,2,3,4),c(1,2,3)))
clkeepTW_Y1 <- list("yft" = list(c(4),c(2,3),c(0)))
clkeepUS_Y1 <- list("yft" = list(c(2,3),c(1,3),c(0)))
clk_Y1 <- list(JP = clkeepJP_Y1,KR = clkeepKR_Y1,TW = clkeepTW_Y1,US = clkeepUS_Y1)


std_splist <- c("alb","bet","yft")
stdlabs <- c("vessid","yrqtr","latlong","op_yr","hbf","hooks",std_splist,"lat","lon","lat5","lon5","reg","hcltrp","flag")

## ---------------------------------------------
# Run various standardization scenarios.
## ---------------------------------------------

# The runpars define the approach to be used in this run
regY1_minss <- list(minq_byreg = c(3,5,3), minvess=c(30,60,30), minll=c(30,50,30), minyrqtr = c(30,50,30), minyqll = c(3,5,3))
regY2_minss <- list(minq_byreg = c(3,5,3,3,5,3), minvess=c(30,60,30,30,60,30), minll=c(30,50,30,30,50,30), minyrqtr = c(30,50,30,30,50,30), minyqll = c(3,5,3,3,5,3))

runpars <- list()
runpars[["regY1"]] <-list(runsp = "yft", regtype2 = "Y1", clk = clk_Y1, doregs = 1:3, addcl = TRUE, dohbf = TRUE, dohook = TRUE, cltype = "hcltrp", minss = regY1_minss, strsmp = 30)
runpars[["regY2"]] <-list(runsp = "yft", regtype2 = "Y2", clk = clk_Y2, doregs = 1:6, addcl = TRUE, dohbf = TRUE, dohook = TRUE, cltype = "hcltrp", minss = regY1_minss, strsmp = 30)

regstr <- "regY1"; runreg <- 2; keepd <- TRUE; doflags <- "TW"
maxyr <- 2019

# with clusters, hooks, hbf
resdir <- paste0(jntalysis_dir,"cl1_hb1_hk1/")
dir.create(resdir)
setwd(resdir)

options(error = recover)

run_standardization(runpars, doflags = c("JP","KR","TW","BR","US"), regstr = "regY1", maxyr = 2019, do_early = TRUE, stdlabs = stdlabs, projdir = projdir, twlimit=0 , jplimit = list(reg=2, yr=3005))
run_standardization(runpars, doflags = c("JP","KR","TW","BR","US"), regstr = "regY2", maxyr = 2019, do_early = TRUE, stdlabs = stdlabs, projdir = projdir, twlimit=0 , jplimit = list(reg=2, yr=3005))

# with clusters, hooks, hbf
resdir <- paste0(jntalysis_dir,"cl1_hb1_hk1/")
dir.create(resdir)
setwd(resdir)

# runpars[["regY"]] <- list(runsp = "yft", regtype2 =  "Y", clk = clk_Y,  doregs = 2:5, addcl = TRUE, dohbf = TRUE, dohook = TRUE, cltype = "hcltrp", minss = regY_minss, strsmp = 30)
# run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regY",  maxyr = 2018, do_early = TRUE, stdlabs = stdlabs, projdir = projdir)
# runpars[["regY2"]] <-list(runsp = "yft", regtype2 = "Y2", clk = clk_Y2, doregs = c(2,7), addcl = TRUE, dohbf = TRUE, dohook = TRUE, cltype = "hcltrp", minss = regY2_minss, strsmp = 30)
# run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regY2", maxyr = 2018, do_early = TRUE, stdlabs = stdlabs, projdir = projdir)
runpars[["regA4"]] <-list(runsp = "alb", regtype2 = "A4", clk = clk_A4, doregs = 1:4, addcl = TRUE, dohbf = TRUE, dohook = TRUE, cltype = "hcltrp", minss = regA4_minss, strsmp = 30)
run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regA4", maxyr = 2018, do_early = TRUE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=4, yr=0))
runpars[["regA5"]] <-list(runsp = "alb", regtype2 = "A5", clk = clk_A5, doregs = 1,   addcl = TRUE, dohbf = TRUE, dohook = TRUE, cltype = "hcltrp", minss = regA5_minss, strsmp = 30)
run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regA5", maxyr = 2018, do_early = TRUE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=4, yr=0))

# no clusters, with hooks, hbf
resdir <- paste0(jntalysis_dir,"cl0_hb1_hk1/")
dir.create(resdir)
setwd(resdir)

clkeepJP_A4 <- list("alb"=list(c(1,2,3), c(1,2,3,5), c(2,4), c(2,4)))
clkeepKR_A4 <- list("alb"=list(c(1,2,3,5), c(1,2,3,4), c(1,3,4), c(2,4)))
clkeepTW_A4 <- list("alb"=list(c(1,2,3), c(1:4), c(1,2), c(1:5)))
clk_A4 <- list(JP=clkeepJP_A4,KR=clkeepKR_A4,TW=clkeepTW_A4)

clkeepJP_A5 <- list("alb"=list(c(2,3,4)))
clkeepKR_A5 <- list("alb"=list(c(5)))
clkeepTW_A5 <- list("alb"=list(c(1,2,4)))
clk_A5 <- list(JP=clkeepJP_A5,KR=clkeepKR_A5,TW=clkeepTW_A5)

clkeepJP_Y <- list("yft"=list(c(1,2,3,4), c(1,2,3,4), c(1,2,4), c(1,3), c(1,2,4),c(1,2,3,4)))
clkeepKR_Y <- list("yft"=list(c(1,3),c(1,2,3,4),c(1,2,3),c(2),c(1,3,4),c(1,2,3,4)))
clkeepTW_Y <- list("yft"=list(c(1,2,3), c(1,2,3,4), c(1,2,3), c(1,2,3,4),   c(1,2,3,4,5)), c(1,2,3,4,5))
clk_Y <- list(JP=clkeepJP_Y,KR=clkeepKR_Y,TW=clkeepTW_Y)

clkeepJP_Y2 <- list("yft"=list(c(0),c(1,2,3,4,5),c(1,2,3),c(1,2,3,4),c(1,2,3,4),c(0),c(1,2,3)))
clkeepKR_Y2 <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(1,2,3,4),c(1,2,3,4),c(0),c(1,2,3,4)))
clkeepTW_Y2 <- list("yft"=list(c(0), c(1,2,3,4),c(0), c(0), c(0), c(0), c(1,2,3,4)))
clk_Y2 <- list(JP=clkeepJP_Y2,KR=clkeepKR_Y2,TW=clkeepTW_Y2)

# runpars[["regY"]] <- list(runsp = "yft", regtype2 =  "Y", clk = clk_Y,  doregs = 2:5, addcl = FALSE, dohbf = TRUE, dohook = TRUE, cltype = "hcltrp", minss = regY_minss, strsmp = 30)
# run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regY",  maxyr = 2018, do_early = TRUE, stdlabs = stdlabs, projdir = projdir)
# runpars[["regY2"]] <-list(runsp = "yft", regtype2 = "Y2", clk = clk_Y2, doregs = c(2,7), addcl = FALSE, dohbf = TRUE, dohook = TRUE, cltype = "hcltrp", minss = regY2_minss, strsmp = 30)
# run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regY2", maxyr = 2018, do_early = TRUE, stdlabs = stdlabs, projdir = projdir)
runpars[["regA4"]] <-list(runsp = "alb", regtype2 = "A4", clk = clk_A4, doregs = 1:4, addcl = FALSE, dohbf = TRUE, dohook = TRUE, cltype = "hcltrp", minss = regA4_minss, strsmp = 30)
run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regA4", maxyr = 2018, do_early = TRUE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=4, yr=0))
runpars[["regA5"]] <-list(runsp = "alb", regtype2 = "A5", clk = clk_A5, doregs = 1,   addcl = FALSE, dohbf = TRUE, dohook = TRUE, cltype = "hcltrp", minss = regA5_minss, strsmp = 30)
run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regA5", maxyr = 2018, do_early = TRUE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=4, yr=0))

# ------------------------------

maxyr = 2018; keepd = TRUE;
for(runsp in c("bet", "yft")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  jdat <- data.frame()
  for(flag in c("JP","KR","TW", "SY")) {
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
  jdat <- jdat[jdat$yrqtr > 2005 | jdat$flag != "TW",]

  vars <- c("vessid","hooks","yrqtr","latlong","hbf")
  for(runreg in runpars[[runsp]]$doregs) {
    minqtrs <- minqtrs_byreg[runreg]
    glmdat <- select_data_JointIO(jdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,minvess=100,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA)
    if(nrow(glmdat)>60000) glmdat <- samp_strat_data(glmdat,40)
    glmdat5279 <- select_data_JointIO(jdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,minvess=50,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1952,1980))
    if(nrow(glmdat5279)>60000) glmdat5279 <- samp_strat_data(glmdat5279,40)
    a <- jdat[jdat$vessid != "JP1",]
    glmdat79nd <- select_data_JointIO(a,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,minvess=50,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1979,maxyr))
    if(nrow(glmdat79nd)>60000) glmdat79nd <- samp_strat_data(glmdat79nd,40)
    wtt.all   <- mk_wts(glmdat,wttype="area")
    wtt.5279   <- mk_wts(glmdat5279,wttype="area")
    wtt.79nd   <- mk_wts(glmdat79nd,wttype="area")
    fmla.oplogn <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=F,addcl=T,nhbf=3)
    fmla.oplogn_ncl <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=F,addcl=F,nhbf=3)
    fmla.boatlogn <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=T,addcl=T,nhbf=3)
    fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=T,addcl=F,nhbf=3)
    mn <- with(glmdat,0.1* mean(get(runsp)/hooks))

    modlab="lognC_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if(lu(glmdat$clust) > 1)
    { model <- glm(fmla.oplogn,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() } else
    { model <- glm(fmla.oplogn_ncl,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if(lu(glmdat$clust) > 1)
    { model <- glm(fmla.boatlogn,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() } else
    { model <- glm(fmla.boatlogn_ncl,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    mn <- with(glmdat5279,0.1* mean(get(runsp)/hooks))
    if(lu(glmdat5279$clust) > 1)
    { model <- glm(fmla.oplogn,data=glmdat5279,weights=wtt.5279,family="gaussian", model = keepd);gc() } else
    { model <- glm(fmla.oplogn_ncl,data=glmdat5279,weights=wtt.5279,family="gaussian", model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat5279,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    mn <- with(glmdat79nd,0.1* mean(get(runsp)/hooks))
    if(lu(glmdat79nd$clust) > 1)
    { model <- glm(fmla.boatlogn,    data=glmdat79nd,weights=wtt.79nd,family="gaussian");gc() } else
    { model <- glm(fmla.boatlogn_ncl,data=glmdat79nd,weights=wtt.79nd,family="gaussian");gc() }
    summarize_and_store(mod=model,dat=glmdat79nd,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    # delta lognormal
    modlab="dellog_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg);
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat5279,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat79nd,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    graphics.off()
  }
}

### run main effects nocl hbf
##
resdir <- paste0(jntalysis_dir,"std_nocl_hbf/")
dir.create(resdir)
setwd(resdir)

runpars <- list()
runpars[["bet"]] <- list(regtype = "regB2", regtype2 = "B2", clk = clk_B2, doregs = 1:4, addcl = FALSE, dohbf = TRUE, cltype = "hcltrp")
runpars[["yft"]] <- list(regtype = "regY",  regtype2 = "Y",  clk = clk_Y,  doregs = 2:5, addcl = FALSE, dohbf = TRUE, cltype = "hcltrp")

runreg=2; runsp="yft"

minqtrs_byreg = c(8,8,2,2,5,5,5,5)
vars <- c("vessid","hooks","yrqtr","latlong")

maxyr = 2018; keepd = TRUE;
#for(runsp in c("bet","yft")) {
for(runsp in c("yft")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  jdat <- data.frame()
  for(flag in c("JP","KR","TW")) {
    for(r in runpars[[runsp]]$doregs) {
      load(paste0(projdir,flag,"/clustering/",paste(flag,regtype,r,sep="_"),".RData"))
      dataset$qtr <- revtrunc(defactor(dataset$yrqtr))
      dataset$flag <- flag
      jdat <- rbind(jdat,dataset[,allabs])
      rm(dataset)
    }
  }
  jdat <- jdat[jdat$yrqtr < maxyr,]
  jdat$vessidx <- jdat$vessid
  jdat$vessid <- paste0(jdat$flag,jdat$vessid)
  jdat$vessid <- as.factor(jdat$vessid)
  jdat <- jdat[jdat$yrqtr > 2005 | jdat$flag != "TW",]

  vars <- c("vessid","hooks","yrqtr","latlong","hbf")
  #  for(runreg in runpars[[runsp]]$doregs) {
  for(runreg in 3:5) {
    minqtrs <- minqtrs_byreg[runreg]
    glmdat <- select_data_JointIO(jdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,minvess=100,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA)
    if(nrow(glmdat)>60000) glmdat <- samp_strat_data(glmdat,40)
    glmdat5279 <- select_data_JointIO(jdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,minvess=50,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1952,1980))
    if(nrow(glmdat5279)>60000) glmdat5279 <- samp_strat_data(glmdat5279,40)
    a <- jdat[jdat$vessid != "JP1",]
    glmdat79nd <- select_data_JointIO(a,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars, minvess=50,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1979,maxyr))
    if(nrow(glmdat79nd)>60000) glmdat79nd <- samp_strat_data(glmdat79nd,40)
    wtt.all   <- mk_wts(glmdat,wttype="area")
    wtt.5279   <- mk_wts(glmdat5279,wttype="area")
    wtt.79nd   <- mk_wts(glmdat79nd,wttype="area")
    fmla.oplogn <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=F,addcl=T,nhbf=3)
    fmla.oplogn_ncl <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=F,addcl=F,nhbf=3)
    fmla.boatlogn <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=T,addcl=T,nhbf=3)
    fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=T,addcl=F,nhbf=3)
    mn <- with(glmdat,0.1* mean(get(runsp)/hooks))

    modlab="lognC_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if(lu(glmdat$clust) > 1)
    { model <- glm(fmla.oplogn,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() } else
    { model <- glm(fmla.oplogn_ncl,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if(lu(glmdat$clust) > 1)
    { model <- glm(fmla.boatlogn,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() } else
    { model <- glm(fmla.boatlogn_ncl,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    mn <- with(glmdat5279,0.1* mean(get(runsp)/hooks))
    if(lu(glmdat5279$clust) > 1)
    { model <- glm(fmla.oplogn,data=glmdat5279,weights=wtt.5279,family="gaussian", model = keepd);gc() } else
    { model <- glm(fmla.oplogn_ncl,data=glmdat5279,weights=wtt.5279,family="gaussian", model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat5279,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    mn <- with(glmdat79nd,0.1* mean(get(runsp)/hooks))
    if(lu(glmdat79nd$clust) > 1)
    { model <- glm(fmla.boatlogn,    data=glmdat79nd,weights=wtt.79nd,family="gaussian");gc() } else
    { model <- glm(fmla.boatlogn_ncl,data=glmdat79nd,weights=wtt.79nd,family="gaussian");gc() }
    summarize_and_store(mod=model,dat=glmdat79nd,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    # delta lognormal
    modlab="dellog_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg);
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat5279,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat79nd,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    graphics.off()
  }
}

### run split Y2 and B3 CL, noHBF
#DONE
resdir <- paste0(jntalysis_dir,"std_cl_nohbf_spl/")
dir.create(resdir)
setwd(resdir)

runpars <- list()
runpars[["bet"]] <- list(regtype = "regB3", regtype2 = "B3", clk = clk_B3, doregs = c(1,5), addcl = TRUE, dohbf = FALSE, cltype = "hcltrp")
runpars[["yft"]] <- list(regtype = "regY2", regtype2 = "Y2", clk = clk_Y2, doregs = c(2,7), addcl = TRUE, dohbf = FALSE, cltype = "hcltrp")

minqtrs_byreg = c(8,8,2,2,5,5,5,5)
vars <- c("vessid","hooks","yrqtr","latlong")
maxyr = 2018; keepd = TRUE;
for(runsp in c("bet", "yft")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  jdat <- data.frame()
  for(flag in c("JP","KR","TW","SY")) {
    for(r in runpars[[runsp]]$doregs) {
      load(paste0(projdir,flag,"/clustering/",paste(flag,regtype,r,sep="_"),".RData"))
      dataset$qtr <- revtrunc(defactor(dataset$yrqtr))
      dataset$flag <- flag
      jdat <- rbind(jdat,dataset[,allabs])
      rm(dataset)
    }
  }
  jdat <- jdat[jdat$yrqtr < maxyr,]
  jdat$vessidx <- jdat$vessid
  jdat$vessid <- paste0(jdat$flag,jdat$vessid)
  jdat$vessid <- as.factor(jdat$vessid)
  jdat <- jdat[jdat$yrqtr > 2005 | jdat$flag != "TW",]

  vars <- c("vessid","hooks","yrqtr","latlong","hbf")
  for(runreg in runpars[[runsp]]$doregs) {
    minqtrs <- minqtrs_byreg[runreg]
    glmdat <- select_data_JointIO(jdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars, minvess=100,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA)
    if(nrow(glmdat)>60000) glmdat <- samp_strat_data(glmdat,40)
    glmdat5279 <- select_data_JointIO(jdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,minvess=50,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1952,1980))
    if(nrow(glmdat5279)>60000) glmdat5279 <- samp_strat_data(glmdat5279,40)
    a <- jdat[jdat$vessid != "JP1",]
    glmdat79nd <- select_data_JointIO(a,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars, minvess=50,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1979,maxyr))
    if(nrow(glmdat79nd)>60000) glmdat79nd <- samp_strat_data(glmdat79nd,40)
    wtt.all   <- mk_wts(glmdat,wttype="area")
    wtt.5279   <- mk_wts(glmdat5279,wttype="area")
    wtt.79nd   <- mk_wts(glmdat79nd,wttype="area")
    fmla.oplogn <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=F,addcl=T,nhbf=3)
    fmla.oplogn_ncl <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=F,addcl=F,nhbf=3)
    fmla.boatlogn <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=T,addcl=T,nhbf=3)
    fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=T,addcl=F,nhbf=3)
    mn <- with(glmdat,0.1* mean(get(runsp)/hooks))

    modlab="lognC_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if(lu(glmdat$clust) > 1)
    { model <- glm(fmla.oplogn,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() } else
    { model <- glm(fmla.oplogn_ncl,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if(lu(glmdat$clust) > 1)
    { model <- glm(fmla.boatlogn,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() } else
    { model <- glm(fmla.boatlogn_ncl,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    mn <- with(glmdat5279,0.1* mean(get(runsp)/hooks))
    if(lu(glmdat5279$clust) > 1)
    { model <- glm(fmla.oplogn,data=glmdat5279,weights=wtt.5279,family="gaussian", model = keepd);gc() } else
    { model <- glm(fmla.oplogn_ncl,data=glmdat5279,weights=wtt.5279,family="gaussian", model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat5279,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    mn <- with(glmdat79nd,0.1* mean(get(runsp)/hooks))
    if(lu(glmdat79nd$clust) > 1)
    { model <- glm(fmla.boatlogn,    data=glmdat79nd,weights=wtt.79nd,family="gaussian");gc() } else
    { model <- glm(fmla.boatlogn_ncl,data=glmdat79nd,weights=wtt.79nd,family="gaussian");gc() }
    summarize_and_store(mod=model,dat=glmdat79nd,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    # delta lognormal
    modlab="dellog_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg);
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat5279,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat79nd,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    graphics.off()
  }
}

### run split Y2 and B3 noCL HBF
resdir <- paste0(jntalysis_dir,"std_nocl_hbf_spl/")
dir.create(resdir)
setwd(resdir)

runpars <- list()
runpars[["bet"]] <- list(regtype = "regB3", regtype2 = "B3", clk = clk_B3, doregs = c(1,5), addcl = FALSE, dohbf = TRUE, cltype = "hcltrp")
runpars[["yft"]] <- list(regtype = "regY2", regtype2 = "Y2", clk = clk_Y2, doregs = c(2,7), addcl = FALSE, dohbf = TRUE, cltype = "hcltrp")

minqtrs_byreg = c(8,8,2,2,5,5,5,5)
vars <- c("vessid","hooks","yrqtr","latlong")
maxyr = 2018; keepd = TRUE;
for(runsp in c("bet", "yft")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  jdat <- data.frame()
  for(flag in c("JP","KR","TW")) {
    for(r in runpars[[runsp]]$doregs) {
      load(paste0(projdir,flag,"/clustering/",paste(flag,regtype,r,sep="_"),".RData"))
      dataset$qtr <- revtrunc(defactor(dataset$yrqtr))
      dataset$flag <- flag
      jdat <- rbind(jdat,dataset[,allabs])
      rm(dataset)
    }
  }
  jdat <- jdat[jdat$yrqtr < maxyr,]
  jdat$vessidx <- jdat$vessid
  jdat$vessid <- paste0(jdat$flag,jdat$vessid)
  jdat$vessid <- as.factor(jdat$vessid)
  jdat <- jdat[jdat$yrqtr > 2005 | jdat$flag != "TW",]

  vars <- c("vessid","hooks","yrqtr","latlong","hbf")
  for(runreg in runpars[[runsp]]$doregs) {
    minqtrs <- minqtrs_byreg[runreg]
    glmdat <- select_data_JointIO(jdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,minvess=100,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA)
    if(nrow(glmdat)>60000) glmdat <- samp_strat_data(glmdat,40)
    glmdat5279 <- select_data_JointIO(jdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,minvess=50,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1952,1980))
    if(nrow(glmdat5279)>60000) glmdat5279 <- samp_strat_data(glmdat5279,40)
    a <- jdat[jdat$vessid != "JP1",]
    glmdat79nd <- select_data_JointIO(a,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,minvess=50,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1979,maxyr))
    if(nrow(glmdat79nd)>60000) glmdat79nd <- samp_strat_data(glmdat79nd,40)
    wtt.all   <- mk_wts(glmdat,wttype="area")
    wtt.5279   <- mk_wts(glmdat5279,wttype="area")
    wtt.79nd   <- mk_wts(glmdat79nd,wttype="area")
    fmla.oplogn <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=F,addcl=T,nhbf=3)
    fmla.oplogn_ncl <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=F,addcl=F,nhbf=3)
    fmla.boatlogn <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=T,addcl=T,nhbf=3)
    fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=T,addcl=F,nhbf=3)
    mn <- with(glmdat,0.1* mean(get(runsp)/hooks))

    modlab="lognC_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if(lu(glmdat$clust) > 1)
    { model <- glm(fmla.oplogn,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() } else
    { model <- glm(fmla.oplogn_ncl,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if(lu(glmdat$clust) > 1)
    { model <- glm(fmla.boatlogn,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() } else
    { model <- glm(fmla.boatlogn_ncl,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    mn <- with(glmdat5279,0.1* mean(get(runsp)/hooks))
    if(lu(glmdat5279$clust) > 1)
    { model <- glm(fmla.oplogn,data=glmdat5279,weights=wtt.5279,family="gaussian", model = keepd);gc() } else
    { model <- glm(fmla.oplogn_ncl,data=glmdat5279,weights=wtt.5279,family="gaussian", model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat5279,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    mn <- with(glmdat79nd,0.1* mean(get(runsp)/hooks))
    if(lu(glmdat79nd$clust) > 1)
    { model <- glm(fmla.boatlogn,    data=glmdat79nd,weights=wtt.79nd,family="gaussian");gc() } else
    { model <- glm(fmla.boatlogn_ncl,data=glmdat79nd,weights=wtt.79nd,family="gaussian");gc() }
    summarize_and_store(mod=model,dat=glmdat79nd,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    # delta lognormal
    modlab="dellog_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg);
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat5279,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat79nd,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    graphics.off()
  }
}

## Interaction models
load(file=paste0(projdir,"cell_areas.RData"))

resdir <- paste0(jntalysis_dir,"intx_cl_nohbf/")
dir.create(resdir)
setwd(resdir)

allabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","alb","bet","yft","hcltrp",
            "Total","lat","lon","lat5","lon5","reg","flag","qtr")

runpars <- list()
runpars[["bet"]] <- list(regtype = "regB2", regtype2 = "B2", clk = clk_B2, doregs = 1:4, addcl = TRUE, dohbf = FALSE, cltype = "hcltrp")
runpars[["yft"]] <- list(regtype = "regY",  regtype2 = "Y",  clk = clk_Y,  doregs = 2:5, addcl = TRUE, dohbf = FALSE, cltype = "hcltrp")
vars <- c("vessid","hooks","yrqtr","latlong", "lat5", "qtr", "op_yr")
minqtrs_byreg = c(8,8,2,2,5,5,5,5);maxyr = 2018; keepd = TRUE;

for(runsp in c("bet", "yft")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
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

  for(runreg in runpars[[runsp]]$doregs) {
    minqtrs <- minqtrs_byreg[runreg]
    glmdat <- select_data_JointIO(jdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,minvess=90,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA)
    if(nrow(glmdat)>60000) glmdat <- samp_strat_data(glmdat,30)
    glmdat5279 <- select_data_JointIO(jdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,minvess=90,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1952,1980))
    if(nrow(glmdat5279)>60000) glmdat5279 <- samp_strat_data(glmdat5279,30)
    a <- jdat[jdat$vessid != "JP1",]
    glmdat79nd <- select_data_JointIO(a,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,minvess=90,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1979,maxyr))
    if(nrow(glmdat79nd)>60000) glmdat79nd <- samp_strat_data(glmdat79nd,30)

    #  t1 <- Sys.time()
    # lognormal constant
    modlab="lognC_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_lognCx(dat=glmdat, dohbf, addboat=F, addcl, nhbf=3,runsp, fname, modlab, keepd = keepd, lat5xqtr = T, lat5xyr = T, bcorr=F)

    #  t2 <- Sys.time()
    modlab="lognC_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_lognCx(dat=glmdat, dohbf, addboat=T, addcl, nhbf=3,runsp, fname, modlab, keepd = keepd, lat5xqtr = T, lat5xyr = T, bcorr=F)

    #  t3 <- Sys.time()
    modlab="lognC_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_lognCx(dat=glmdat5279, dohbf, addboat=F, addcl, nhbf=3,runsp, fname, modlab, keepd = keepd, lat5xqtr = T, lat5xyr = T, bcorr=F)

    #    t4 <- Sys.time()
    modlab="lognC_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_lognCx(dat=glmdat79nd, dohbf, addboat=F, addcl, nhbf=3,runsp, fname, modlab, keepd = keepd, lat5xqtr = T, lat5xyr = T, bcorr=F)

    #    t5 <- Sys.time()
    # delta lognormal
    # modlab="dellog_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg);
    # do_deltalogx(dat=glmdat,dohbf, addboat=F,addcl, nhbf=3,runsp, fname, modlab, keepd = keepd, lat5xqtr = T, lat5xyr = T, bcorr=F)
    #
    # # modlab="dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    # # do_deltalogx(dat=glmdat,dohbf, addboat=T,addcl, nhbf=3,runsp, fname, modlab, keepd = keepd, lat5xqtr = T, lat5xyr = T, bcorr=F)
    #
    # modlab="dellog_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    # do_deltalogx(dat=glmdat5279,dohbf, addboat=F,addcl, nhbf=3,runsp, fname, modlab, keepd = keepd, lat5xqtr = T, lat5xyr = T, bcorr=F)
    #
    # modlab="dellog_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    # do_deltalogx(dat=glmdat79nd,dohbf, addboat=F,addcl, nhbf=3,runsp, fname, modlab, keepd = keepd, lat5xqtr = T, lat5xyr = T, bcorr=F)

    graphics.off()
  }
}

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
