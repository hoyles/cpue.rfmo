projdir <- "~/IOTC/2017_CPUE/"
sydir <- paste0(projdir, "SY/")
datadir1 <- paste0(sydir, "data/catch_effort/")
jalysis_dir <- paste0(sydir, "analyses/")
syfigs <- paste0(sydir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
setwd(jalysis_dir)
#install.packages("survival")
library("date")
library(splines)
library("maps")
library("mapdata")
library("maptools")
library("data.table")
library("lunar")
library(lubridate)
library(readr)
library(plyr)
library(dplyr)
library(dtplyr)
library(tm)

source(paste0(Rdir,"support_functions.r"))

# nms <- c("op_yr","op_mon","op_day","lat","latcode","lon","loncode","callsign",
#       "hbf","hooks","sbt","alb","bet","yft","swo","mls","bum","blm","trip_st","sas","shk","prefecture","vesselname","logbookid")
# wdths <- c(4,2,2,2,1,3,1,6,3,6,3,3,3,3,3,3,3,3,8,3,4,3,30,9)
# cc <- "iiiiiiiciiiiiiiiiiiiicci"
# posses <- cumsum(c(1,wdths))
# cc <- "iiiiiiiciiiiiiiiiiiiicci"
# cbind(nms,wdths,unlist(strsplit(cc,"")))
#
# a <- read_fwf(file=paste0(datadir1,"/JPNLL_20170524.dat"),fwf_widths(wdths),col_types=cc,n_max=20);gc()
# names(a) <- nms
# a
# dat5216 <- read_fwf(file=paste0(datadir1,"/JPNLL_20170524.dat"),fwf_widths(wdths),col_types=cc)
# problems(dat5216)
# names(dat5216) <- nms
# table(dat5216$trip_st==0,dat5216$op_yr)
# table(dat5216$op_yr)
#
# rawdat <- dat5216
# pd1 <- dataprep_JPIO(rawdat)
# pd2 <- setup_IO_regions(pd1, regY=T, regY1=T, regB=T, regB1=T, regB2=T)
#
# clndat <- dataclean_JPIO(rawdat)
# prepdat1 <- dataprep_JPIO(clndat)
# prepdat <- setup_IO_regions(prepdat1, regY=T, regY1=T, regB=T, regB1=T, regB2=T)
# save(pd1, pd2, prepdat, file="prepdat.RData")
#
# dat <- make_clid(prepdat)
# dat <- make_lbidmon(dat)
# save(dat,file="JPdat.RData")
# load(file="JPdat.RData")


######################################################
# Start the analysis proper
########################
#Clustering

#library(R4MFCL)
library(maps)
library(mapdata)
library("mgcv")
library(randomForest)
library(influ)
library("nFactors")
library(data.table)
library(plyr)
library(dplyr)
library(cluster)
library(splines)
library(boot)
library(beanplot)

projdir <- "~/IOTC/2017_CPUE/"
sydir <- paste0(projdir, "JP/")
datadir1 <- paste0(sydir, "data/")
jalysis_dir <- paste0(sydir, "analyses/")
Rdir <- paste0(projdir, "Rfiles/")
clusdir <- paste0(sydir, "clustering/")
setwd(clusdir)

load(file=paste0(jalysis_dir,"JPdat.RData"))
source(paste0(Rdir,"support_functions.r"))
str(dat)

rm(dat2,prepdat,prepdat1,pd1,pd2,clndat,dat5214,rawdat,dataset,llv,dat9415b,dat9415hd,a5,lnk,a2,a0,a)

gc()
allsp <- c("alb","bet","yft","swo","mls","bum","blm","sbt")
allabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tripid","tripidmon","clid","jnt_clid","lbid_mon","moon","alb","bet","yft","swo",
            "mls","bum","blm","sbt","Total","dmy","lat","lon","lat5","lon5","regY","regY1","regB","regB1","regB2")
dat <- data.frame(dat)
str(dat[,allabs])

nclY=c(1,4,4,5,4,1)
nclB2=c(5,5,4,4)
flag="JP"
cvn <- c("yrqtr","latlong","hooks","hbf","vessid","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")
r=4


regtype="regY"
for(r in 2:5) {
  fnh <- paste(flag,regtype,r,sep="_")
  dataset <- clust_PCA_run(r=r,ddd=dat,allsp=allsp,allabs=allabs,regtype=regtype,ncl=nclY[r],plotPCA=F,clustid="lbid_mon",allclust=F,flag=flag,fnhead=fnh,covarnames=cvn)
  save(dataset,file=paste0(fnh,".RData"))
}
regtype="regB2"
for(r in 1:length(nclB2)) {
  fnh <- paste(flag,regtype,r,sep="_")
  dataset <- clust_PCA_run(r=r,ddd=dat,allsp=allsp,allabs=allabs,regtype=regtype,ncl=nclB2[r],plotPCA=F,clustid="lbid_mon",allclust=F,flag=flag,fnhead=fnh,covarnames=cvn)
  save(dataset,file=paste0(fnh,".RData"))
}


##################################################
# SY only, clusters, HBF
#
# Based on joint standardization analyses from 2016

projdir <- "~/IOTC/2017_CPUE/"
sydir <- paste0(projdir, "SY/")
datadir1 <- paste0(sydir, "data/")
jalysis_dir <- paste0(sydir, "analyses/")
Rdir <- paste0(projdir, "Rfiles/")
clusdir <- paste0(sydir, "clustering/")
resdir <- paste0(jalysis_dir,"std_cl_SYonly_nohbf/")
dir.create(resdir)
Rdir <-  paste0(projdir,"Rfiles/")
setwd(resdir)

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

source(paste0(Rdir, "support_functions.r"))

allabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","alb","bet","yft","hcltrp",
            "Total","lat","lon","lat5","lon5","reg","flag")


clkeepJP_Y <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(1,2,5),c(1,2,3,4),c(0)))
clkeepKR_Y <- list()
clkeepTW_Y <- list()
clkeepSY_Y <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(2),c(1,2,3,4),c(0)))
clk_Y <- list(JP=clkeepJP_Y,KR=clkeepKR_Y,TW=clkeepTW_Y,SY=clkeepSY_Y)

clkeepJP_B2 <- list("bet"=list(c(1,2,3,4,5),c(1,2,3,4,5),c(1,2,3,4),c(1,2,3,4)))
clkeepKR_B2 <- list()
clkeepTW_B2 <- list()
clkeepSY_B2 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2),c(1,2,4)))
clk_B2 <- list(JP=clkeepJP_B2,KR=clkeepKR_B2,TW=clkeepTW_B2,SY=clkeepSY_B2)

runpars <- list()
runpars[["bet"]] <- list(regtype = "regB2", regtype2 = "B2", clk = clk_B2, doregs = 1:4, addcl = TRUE, dohbf = FALSE, cltype = "hcltrp")
runpars[["yft"]] <- list(regtype = "regY",  regtype2 = "Y",  clk = clk_Y,  doregs = c(2,3,5), addcl = TRUE, dohbf = FALSE, cltype = "hcltrp")

runreg=3; runsp="yft"

maxyr = 2017; keepd = TRUE; maxqtrs=200; minqtrs_byreg = c(8,8,2,2,5,5,5,5); addbranch<-F;addother=F;addalb=F
for(runsp in c("bet", "yft")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  jdat <- data.frame()
  for(flag in c("SY")) {
    for(r in runpars[[runsp]]$doregs) {
      load(paste0(projdir,flag,"/clustering/",paste(flag,regtype,r,sep="_"),".RData"))
      dataset$flag <- flag
      jdat <- rbind(jdat,dataset[,allabs])
      rm(dataset)
    }
  }
  jdat <- jdat[jdat$yrqtr < 2017,]
  jdat$vessidx <- jdat$vessid
  jdat$vessid <- paste0(jdat$flag,jdat$vessid)
  jdat$vessid <- as.factor(jdat$vessid)
  jdat <- jdat[jdat$yrqtr > 2005 | jdat$flag != "TW",]

  vars <- c("vessid","hooks","yrqtr","latlong","hbf")
  for(runreg in runpars[[runsp]]$doregs) {
    minqtrs <- minqtrs_byreg[runreg]
    glmdat <- select_data_JointIO(jdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,maxqtrs=maxqtrs,
                                  minvess=50,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA)
    if(nrow(glmdat)>60000) glmdat <- samp_strat_data(glmdat,60)
    wtt.all   <- mk_wts(glmdat,wttype="area")
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

    # delta lognormal
    modlab="dellog_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg);
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    graphics.off()
  }
}

