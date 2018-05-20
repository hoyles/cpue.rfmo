projdir <- "~/IOTC/2017_CPUE/"
twdir <- paste0(projdir, "TW/")
datadir <- paste0(twdir, "data/newfileadd2014-2016/")
twylisis_dir <- paste0(twdir, "analyses/")
twfigs <- paste0(twdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
setwd(twylisis_dir)
#install.packages("date")
#install.packages("maps")
#install.packages("mapdata")
#install.packages("maptools")
#install.packages("mapproj")
#install.packages("data.table")
#install.packages("lunar")
library("date")
library(lubridate)
library("maps")
library("mapdata")
library("maptools")
library("data.table")
library("lunar")
library(readr)
library(plyr)
library(dplyr)
library(splines)

search()
source(paste0(Rdir,"support_functions.r"))

# a <- list.files(pattern="model")
# f=a[1]
# for (f in a) {
#     load(f)
#     x <- ls()[3]
#     summ <- summary(get(x))
#     save(summ,file=paste0("summ",f,".RData") )
#     rm(summ,x)
#     ls()
#     }

#"alb","bet","yft","sbt",      "swo","mls","bum","blm",      "skj","sha","oth","sfa",
#"alb","bet","yft","sbf","ott","swo","mls","bum","blm","otb","skj","sha","oth",

nms2 <- c("callsign","op_yr","op_mon","op_day","op_area","hbf","hooks","alb","bet","yft","pbf","sbf","ott","swo",
          "mls","bum","blm","otb","skj","sha","oth","alb_w","bet_w","yft_w","pbf_w","sbf_w","ott_w","swo_w",
          "mls_w","bum_w","blm_w","otb_w","skj_w","sha_w","oth_w","sst","bait1","bait2","bait3","bait4","bait5","hookdp","target",
          "NS","op_lat","EW","op_lon","cpr","embark_yr","embark_mn","embark_dd","op_start_yr","op_start_mn","op_start_dd",
          "op_end_yr","op_end_mn","op_end_dd","debark_yr","debark_mn","debark_dd","oil","foc","rem")

#wdths2 <- c(5,4,2,2,4,3,5,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,2,1,1,1,1,1,3,2,1,2,1,3,2,8,1,8,1,8,1,8,5,5,8)
wdths2 <- c(5,4,2,2,4,3,5,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,2,1,1,1,1,1,3,2,1,2,1,3,2,4,2,2,4,2,2,4,2,2,4,2,2,5,5,11)
cc2 <- "ciiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiciiiicccccccccccccccc"
substr(cc2,47,47)
a <- read_fwf(file=paste0(datadir,"/LOG2005.IND"),fwf_widths(wdths2),col_types=cc2,n_max=20);gc()
names(a) <- nms2
a
cbind(nms2,wdths2,cumsum(c(wdths2)),unlist(strsplit(cc2,"")))

# yy <- 1979:2013;yy <- paste0("/LOG",yy,".ind")
# readfun1 <- function(ff) {
#   read.fwf(paste0(datadir1,ff),widths=wdths,col.names=nms,buffersize=200000,stringsAsFactors=F)
#   }
# a <- lapply(yy,readfun1)
# dat1 <- data.frame()
# for(i in 1:length(yy)) {
#   dat1 <- rbind(dat1,a[[i]])
#   print(i)
#   flush.console()
#   }
#save(dat1,file="dat1.RData")
#load("dat1.RData")

yy <- 1979:2016;yy <- paste0("/LOG",yy,".IND")
readfun1 <- function(ff) {
  read_fwf(paste0(datadir,ff),fwf_widths(wdths2),col_types=cc2)
}
a <- lapply(yy,readfun1)
system.time({ dat1 <- ldply(a, data.frame) })
names(dat1) <- nms2
save(dat1,file="dat1.RData")
load(paste0(twylisis_dir, "dat1.RData"))

str(dat1)
table(dat1$op_yr)
table(is.na(dat1$embark_yr),dat1$op_yr)
table(is.na(dat1$debark_yr),dat1$op_yr)
table(is.na(dat1$op_start_yr),dat1$op_yr)
table(is.na(dat1$op_end_yr),dat1$op_yr)
table(is.na(dat1$target),dat1$op_yr)
table(is.na(dat1$op_lon),dat1$op_yr)
table(is.na(dat1$hbf),dat1$op_yr)
table(dat1$op_yr,dat1$op_yr==1)

prepdat1 <- dataprep_TW(dat1)
prepdat <- setup_IO_regions(prepdat1,  regY=T, regY1=T, regY2=T, regB=T, regB3=T, regB2=T, regA=T, regA1=T, regA2=T, regA3=T, regA4=T, regA5=T)
datold <- dataclean_TW(prepdat)
save(datold,file="TWdat_old.RData")
dat <- dataclean_TW(prepdat,rmssp=T)
save(dat,file="TWdat.RData")
load(file="TWdat.RData")
getwd()

table(dat$op_lon,dat$lon,useNA="always")
table(dat$op_lon,useNA="always")
table(dat$lon,useNA="always")

a <- unique(paste(dat$lat,dat$lon))
a0 <- dat[match(a,paste(dat$lat,dat$lon)),c("lat","lon","regY","regB","regY1","regB1","regB2","regA","regA1","regA2","regA3","regA4","regA5")]
windows(width=15,height=10)
for(fld in c("regY","regB","regY1","regB1","regB2","regA","regA1","regA2","regA3","regA4","regA5")) {
  reg <- with(a0,get(fld))
  plot(a0$lon,a0$lat,type="n",xlab="Longitude",ylab="Latitude",main=fld)
  text(a0$lon,a0$lat,labels=reg,cex=0.6,col=reg+1)
  map(add=T)
  savePlot(paste0("map_",fld),type="png")
}

table(is.na(dat$embark_dmy),dat$op_yr)
head(dat)

table(prepdat$alb)   # ask sets with 910 alb
table(prepdat$bet)   # ask set with 461 bet
table(prepdat$yft)   # ask set with 1038
table(prepdat$sbt)   # ask set with 380
table(prepdat$ott)   # ask sets with 186
table(prepdat$swo)   # ask sets with 269
table(prepdat$mls)   # ask set with 454
table(prepdat$bum)   # ask set with 130
table(prepdat$blm)   # ask set with 75 blm
table(prepdat$otb)   # ask sets with 150
table(prepdat$skj)   # ask set with 143
table(prepdat$sha)   # ask majority of sets (=719211) with 0 sha. Also one set with 663
table(prepdat$oth)   # ask sets with 3059! But most (=636641) have 0.
table(prepdat$hbf,useNA="always")  # 6408 with NA! All in 1973-75
table(prepdat$hbf,prepdat$yr,useNA="always")  #
a <- table(dat$yr,round(dat$hbf,0),useNA="always")
write.csv(a,"table hbf by year.csv")


# data exploration
#install.packages("rpart")
library(rpart)
a <- dat[dat$regY%in% c(2,5),]
dim(a)
a$betcpue <- a$bet/a$hooks
a$albcpue <- a$alb/a$hooks
a$yftcpue <- a$yft/a$hooks
a$sbtcpue <- a$sbt/a$hooks
a$swocpue <- a$swo/a$hooks
a$othcpue <- a$oth/a$hooks
a$mlscpue <- a$mls/a$hooks
a$blmcpue <- a$blm/a$hooks
a$bumcpue <- a$bum/a$hooks
simplemod <- rpart(a$betcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$albcpue + a$othcpue + a$mlscpue + a$blmcpue + a$bumcpue)
windows(width=11,height=7)
plot(simplemod)
text(simplemod)


########################
#Clustering

#library(R4MFCL)
library(maps)
#library(sdhpkg)
library(mapdata)
#install.packages("mgcv")
library("mgcv")
#install.packages('randomForest')
library(randomForest)
library(influ)
#install.packages("nFactors")
#install.packages("Rcpp")
library("nFactors")
library(data.table)
library(plyr)
#install.packages('dplyr')
library(dplyr)
library(cluster)
library(splines)
library(boot)
library(beanplot)

projdir <- "~/IOTC/2017_CPUE/"
twdir <- paste0(projdir, "TW/")
twylisis_dir <- paste0(twdir, "analyses/")
Rdir <- paste0(projdir, "Rfiles/")
clustdir <- paste0(twdir,"clustering/")
setwd(clustdir)
load(file="../analyses/TWdat.RData")
source(paste0(Rdir,"support_functions.r"))

allsp <- c("alb","bet","yft","ott","swo","mls","bum","blm","otb","skj","sha","oth","sbt")

allabs <- c("vessid","callsign","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tripid","tripidmon","moon","bt1","bt2","bt3","bt4","bt5","alb","bet","yft","ott","swo",
            "mls","bum","blm","otb","skj","sha","oth","sbt","Total","alb_w","bet_w","yft_w",
            "ott_w","swo_w","mls_w","bum_w","blm_w","otb_w","skj_w","sha_w","oth_w","sbt_w","sst","dmy",
            "embark_dmy","debark_dmy","op_start_dmy","op_end_dmy","lat","lon","lat5","lon5",
            "regY","regY2","regB","regB3","regB2","regA","regA1","regA2","regA3","regA4","regA5")

table(dat$regY,dat$lon5)

##########
# All years included, YFT regions
rm(datold,pd,prepdat,dat1,dat2,ds,dat_std,junk,a1,a2,a3,a4,aprep,simplemod,rwd,llvall,d2,cld,astd,llvstd,llx,llvold,vvv,llv2)

# nclA2=c(4,4,4,4)
# nclA3=c(4,3,3,3)
# nclA5=c(5)
nclY=c(1,5,4,3,5,1)
nclB2=c(5,5,4,4)
nclY2=c(1,5,4,3,5,1,5)
nclB3=c(5,5,4,4,5)
flag="TW"
r=2


cvn <- c("yrqtr","latlong","hooks","hbf","vessid","callsign","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")
regtype="regY"
for(r in 2:5) {
  fnh <- paste(flag,regtype,r,sep="_")
  dataset <- clust_PCA_run(r=r,ddd=dat,allsp=allsp,allabs=allabs,regtype=regtype,ncl=nclY[r],plotPCA=F,clustid="tripidmon",allclust=F,flag=flag,fnhead=fnh,covarnames=cvn)
  save(dataset,file=paste0(fnh,".RData"))
}
regtype="regY2"
for(r in c(2,7)) {
  fnh <- paste(flag,regtype,r,sep="_")
  dataset <- clust_PCA_run(r=r,ddd=dat,allsp=allsp,allabs=allabs,regtype=regtype,ncl=nclY2[r],plotPCA=F,clustid="tripidmon",allclust=F,flag=flag,fnhead=fnh,covarnames=cvn)
  save(dataset,file=paste0(fnh,".RData"))
}

regtype="regB2"
for(r in 1:length(nclB2)) {
  fnh <- paste(flag,regtype,r,sep="_")
  dataset <- clust_PCA_run(r=r,ddd=dat,allsp=allsp,allabs=allabs,regtype=regtype,ncl=nclB2[r],plotPCA=F,clustid="tripidmon",allclust=F,flag=flag,fnhead=fnh,covarnames=cvn)
  save(dataset,file=paste0(fnh,".RData"))
}
regtype="regB3"
for(r in c(1,5)) {
  fnh <- paste(flag,regtype,r,sep="_")
  dataset <- clust_PCA_run(r=r,ddd=dat,allsp=allsp,allabs=allabs,regtype=regtype,ncl=nclB3[r],plotPCA=F,clustid="tripidmon",allclust=F,flag=flag,fnhead=fnh,covarnames=cvn)
  save(dataset,file=paste0(fnh,".RData"))
}

# regtype="regA3"
# for(r in 1:4) {
#   fnh <- paste(flag,regtype,r,sep="_")
#   dataset <- clust_PCA_run(r=r,ddd=dat,allsp=allsp,allabs=allabs,regtype=regtype,ncl=nclA3[r],plotPCA=F,clustid="tripidmon",allclust=F,flag=flag,fnhead=fnh,covarnames=cvn)
#   save(dataset,file=paste0(fnh,".RData"))
# }
# regtype="regA2"
# for(r in 1:length(nclA2)) {
#   fnh <- paste(flag,regtype,r,sep="_")
#   dataset <- clust_PCA_run(r=r,ddd=dat,allsp=allsp,allabs=allabs,regtype=regtype,ncl=nclA2[r],plotPCA=F,clustid="tripidmon",allclust=F,flag=flag,fnhead=fnh,covarnames=cvn)
#   save(dataset,file=paste0(fnh,".RData"))
# }
# regtype="regA5"
# for(r in 1:length(nclA5)) {
#   fnh <- paste(flag,regtype,r,sep="_")
#   dataset <- clust_PCA_run(r=r,ddd=dat,allsp=allsp,allabs=allabs,regtype=regtype,ncl=nclA5[r],plotPCA=F,clustid="tripidmon",allclust=F,flag=flag,fnhead=fnh,covarnames=cvn)
#   save(dataset,file=paste0(fnh,".RData"))
# }

######################################


#################### Run standardization models for each species ##########################
#***********************************************
#  RUN MULTISPECIES STANDARDIZATION PROCEFURES #
#***********************************************

projdir <- "~/IOTC/2017_CPUE/"
twdir <- paste0(projdir, "TW/")
twylisis_dir <- paste0(twdir, "analyses/")
Rdir <- paste0(projdir, "Rfiles/")
clustdir <- paste0(twdir,"clustering/")
std_dir <- paste0(twdir,"std/")
setwd(std_dir)

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
load(file=paste0(twylisis_dir, "TWdat.RData"))

allabs <- c("vessid","callsign","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tripid","tripidmon","moon","bt1","bt2","bt3","bt4","bt5","alb","bet","yft","ott","swo",
            "mls","bum","blm","otb","skj","sha","oth","sbt","Total","alb_w","bet_w","yft_w",
            "ott_w","swo_w","mls_w","bum_w","blm_w","otb_w","skj_w","sha_w","oth_w","sbt_w","sst","dmy",
            "embark_dmy","debark_dmy","op_start_dmy","op_end_dmy","lat","lon","lat5","lon5","regY","regB","regY","regB","regY1","regB1","regA","regA1","regA2","regA3")
dat <- data.frame(dat)

#cbind(names(dataset), allabs)

clkeepJP_Y <- list()
clkeepKR_Y <- list()
clkeepTW_Y <- list("yft"=list(c(0),c(1,2,3,4,5),c(1,2,3),c(1,2),c(1,2,3,4,5)),c(0))
clkeepSY_Y <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(2),c(1,2,3,4),c(0)))
clk_Y <- list(JP=clkeepJP_Y,KR=clkeepKR_Y,TW=clkeepTW_Y,SY=clkeepSY_Y)

clkeepJP_B2 <- list()
clkeepKR_B2 <- list()
clkeepTW_B2 <- list("bet"=list(c(1,2,3,4,5),c(1,2,3,4,5),c(2,3),c(1,2,3,4)))
clkeepSY_B2 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2),c(1,2,4)))
clk_B2 <- list(JP=clkeepJP_B2,KR=clkeepKR_B2,TW=clkeepTW_B2,SY=clkeepSY_B2)

runpars <- list()
runpars[["bet"]] <- list(regtype = "regB2", regtype2 = "B2", clk = clk_B2, doregs = 1:4, addcl = TRUE, dohbf = FALSE, cltype = "hcltrp")
runpars[["yft"]] <- list(regtype = "regY",  regtype2 = "Y",  clk = clk_Y,  doregs = 2:5, addcl = TRUE, dohbf = FALSE, cltype = "hcltrp")

#for(nm in allabs) print(c(nm, dataset[1,nm]))

runsp="bet"; runreg = 4
maxyr = 2017; maxqtrs=200; minqtrs_byreg = c(8,8,2,2,5,5,5,5); keepd = TRUE; addbranch<-F;addother=F;addalb=F
for(runsp in c("bet", "yft")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  tdat <- data.frame()
  for(flag in c("TW")) {
    for(r in runpars[[runsp]]$doregs) {
      load(paste0(projdir,flag,"/clustering/",paste(flag,regtype,r,sep="_"),".RData"))
      dataset$flag <- flag
      tdat <- rbind(tdat,dataset)
      rm(dataset)
    }
  }
  tdat <- tdat[tdat$yrqtr < 2017,]
  tdat$vessidx <- tdat$vessid
  tdat$vessid <- paste0(tdat$flag,tdat$vessid)
  tdat$vessid <- as.factor(tdat$vessid)

  vars <- c("vessid","hooks","yrqtr","latlong","hbf")
  for(runreg in runpars[[runsp]]$doregs) {
    minqtrs <- minqtrs_byreg[runreg]
    glmdat <- select_data_JointIO(tdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,maxqtrs=maxqtrs,
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
    #  dat=glmdat; dohbf=dohbf; addboat=F; addcl=addcl; nhbf=3; runsp=runsp; fname=fname; modlab=modlab;  keepd = keepd
    modlab="dellog_novess_allyrs"; fname <- paste0("Joint_", regtype,"_R",runreg);
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    graphics.off()
  }
}

