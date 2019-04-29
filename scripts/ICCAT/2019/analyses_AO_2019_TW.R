projdir <- "~/ICCAT/2019_YFT/"
twdir <- paste0(projdir, "TW/")
datadir <- paste0(twdir, "data/")
twalysis_dir <- paste0(twdir, "analyses/")
twfigs <- paste0(twdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
dir.create(twdir)
dir.create(datadir)
dir.create(twalysis_dir)
dir.create(twfigs)

setwd(twalysis_dir)

library("date")
library("splines")
library("maps")
library("mapdata")
library("maptools")
library("data.table")
library("lunar")
library("lubridate")
library("readr")
library("plyr")
library("dplyr")
library("dtplyr")
library("tm")

#install.packages("devtools")
library(devtools)
# This new library replaces the 'support functions.r' file.
#install_github("hoyles/cpue.rfmo")

library("cpue.rfmo")

#source(paste0(Rdir,"support_functions.r"))
#xsd # stop!

# Test data format

# ===================================================================================
# Please keep the data format consistent between years and for the ICCAT + IOTC analyses.

#nms2 <- c( "callsign","op_yr","op_mon","op_day","op_area","hbf","hooks","alb","bet","yft","bft","sbt","ott","swo","mls","bum","blm","otb","skj","sha","oth","alb_w","bet_w","yft_w","bft_w","sbt_w","ott_w","swo_w","mls_w","bum_w","blm_w","otb_w","skj_w","sha_w","oth_w","sst","bait1","bait2","bait3","bait4","bait5","hookdp","target","group","NS","op_lat","EW","op_lon","cpr","embark_yr","embark_mn","embark_dd","op_start_yr","op_start_mn","op_start_dd","op_end_yr","op_end_mn","op_end_dd","debark_yr","debark_mn","debark_dd","oil","foc","rem")
nms2 <- c( "callsign","op_yr","op_mon","op_day","op_area","hbf","hooks","alb","bet","yft","bft","sbt","ott","swo","mls","bum","blm","otb","skj","sha","oth","alb_w","bet_w","yft_w","bft_w","sbt_w","ott_w","swo_w","mls_w","bum_w","blm_w","otb_w","skj_w","sha_w","oth_w","sst","bait1","bait2","bait3","bait4","bait5","hookdp","target","group","NS","op_lat","EW","op_lon")
#wdths2 <- c(5,4,2,2,4,3,5,rep(4,14),rep(5,14),2,1,1,1,1,1,3,3,5,2,2,1,3,2,4,2,2,4,2,2,4,2,2,4,2,2,5,5,11)
wdths2 <- c(5,4,2,2,4,3,5,rep(4,14),rep(5,14),2,1,1,1,1,1,3,3,5,2,2,1,3) # Changed cpr width to 1 instead of 2.
#cc2 <- "ciiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiicccccccccccccccc"
cc2 <-  "ciiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiicciiii"
sum(wdths2)
length(wdths2)


# Check data loading
a <- read_fwf(file=paste0(datadir,"LOG1991A.ATL"),fwf_widths(wdths2),col_types=cc2)
a <- read_fwf(file=paste0(datadir,"LOG2008A.ATL 1x1 bft sbt"),fwf_widths(wdths2),col_types=cc2,n_max=20)
names(a) <- nms2
as.data.frame(problems(a))
a[960:980,28:48]

#a <- read_csv(file = paste0(datadir,"/LOG1981A.ATL"),fwf_widths(wdths2),col_types = cc2,n_max = 20);gc()
# names(a) <- nms2
# a <- data.frame(a)
# a
# cbind(nms2,wdths2,cumsum(c(wdths2)),unlist(strsplit(cc2,"")))

# Load data
yy <- c(1981:1992,2018);yy <- paste0("/LOG",yy,"A.ATL")
readfun1 <- function(ff) {
  read_fwf(paste0(datadir,ff),fwf_widths(wdths2),col_types = cc2)
}
a1 <- lapply(yy,readfun1)

yy <- 1993:2017;yy <- paste0("/LOG",yy,"A.ATL 1x1 bft sbt")
readfun1 <- function(ff) {
  read_fwf(paste0(datadir,ff),fwf_widths(wdths2),col_types = cc2)
}
a2 <- lapply(yy,readfun1)


system.time({dat_early <- ldply(a1, data.frame)})
names(dat_early) <- nms2
system.time({dat_late <- ldply(a2, data.frame)})
names(dat_late) <- nms2

dat1a <- filter(dat_early, op_day != 99 & !is.na(op_day))
dat1b <- filter(dat_late, op_day != 99 & !is.na(op_day))
dat1 <- rbind(dat1a, dat1b)

save(dat1,file = paste0(twalysis_dir, "dat1.RData"))
load(paste0(twalysis_dir, "dat1.RData"))

rm(dat1a, dat1b, dat_early, dat_late, dat2, dat2b, dat)
# Check data
# str(dat1)
# summary(dat1)
# table(dat1$op_yr)
# table(is.na(dat1$embark_yr),dat1$op_yr)
# table(is.na(dat1$debark_yr),dat1$op_yr)
# table(is.na(dat1$op_start_yr),dat1$op_yr)
# table(is.na(dat1$op_end_yr),dat1$op_yr)
# table(is.na(dat1$target),dat1$op_yr)
# table(is.na(dat1$op_lon),dat1$op_yr)
# table(is.na(dat1$hbf),dat1$op_yr)
# table(dat1$op_yr,dat1$op_yr == 1)



# Prepare data
splist <- c("alb","bet","yft","bft","sbt","ott","swo","mls","bum","blm","otb","skj","sha","oth")
splist %in% names(dat1)

prepdat1 <- dataprep_TW(dat1, alldat = F, region = "AO", splist = splist, nomlist = "noms2")
prepdat <- setup_AO_regions(prepdat1,  regB = TRUE, regB1 = TRUE,  regY = TRUE, regY1 = TRUE, regY2 = TRUE)
prepdat <- prepdat[prepdat$lon < 30,]
#splist = c("alb", "bet", "yft", "ott", "swo", "mls", "bum", "blm", "otb", "skj", "sha", "oth", "sbt")
datold <- dataclean_TW(prepdat, rmssp = F, splist = splist)
save(datold,file = "TWdat_old.RData")
dat <-    dataclean_TW(prepdat, rmssp = T, splist = splist)
save(dat,file = "TWdat.RData")
load(file = "TWdat.RData")
getwd()


dim(dat1)
dim(prepdat1)
dim(prepdat)
dim(dat)
dim(datold)

# Check data
table(dat$op_lon,dat$lon,useNA = "always")
table(dat$op_lon,useNA = "always")
table(dat$lon,useNA = "always")

# Data map
a <- unique(paste(dat$lat,dat$lon))
a0 <- dat[match(a,paste(dat$lat,dat$lon)),c("lat","lon","regB","regB1","regY","regY1", "regY2")]
for (fld in c("regB","regB1","regY","regY1", "regY2")) {
dev.new(width = 15,height = 10)
  reg <- with(a0,get(fld))
  plot(a0$lon,a0$lat,type = "n",xlab = "Longitude",ylab = "Latitude",main = fld)
  text(a0$lon,a0$lat,labels = reg,cex = 0.6,col = reg + 1)
  map(add = T)
  savePlot(paste0("map_",fld),type = "png")
}

table(is.na(dat$embark_dmy),dat$op_yr)
head(dat)

table(prepdat$alb)
prepdat[prepdat$alb == 3450,]
table(prepdat$bet)
prepdat[prepdat$bet == 1429,]
prepdat[prepdat$bet == 1704,]
prepdat[prepdat$bet == 1173,]
prepdat[prepdat$bet > 1000,] # Sets with v large catches are aggregated, note no. of hooks.
table(dat$bet)
str(dat)
table(dat$bft)
table(dat$sbt)
table(prepdat$yft)
table(prepdat$sbt)
table(prepdat$pbf)
table(prepdat$whm)
table(dat1$whm)
table(prepdat$ott)
table(prepdat$swo)
table(prepdat$mls)
table(prepdat$bum)
table(prepdat$blm)
table(prepdat$otb)
table(prepdat$skj)
table(prepdat$sha)   # majority of sets (=719211) with 0 sha. Also one set with 663
table(prepdat$oth)   # set with 2002.
prepdat[prepdat$oth > 1800,]
table(prepdat$hbf,useNA = "always")
table(prepdat$hbf,prepdat$yr,useNA = "always")
a <- table(dat$yr,round(dat$hbf,0),useNA = "always")
write.csv(a,"table hbf by year.csv")


# Plot and explore data
#install.packages("rpart")
library(rpart)
a <- dat[dat$regB %in% c(1:3),]
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
simplemod <- rpart(a$yftcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$albcpue + a$othcpue + a$mlscpue + a$blmcpue + a$bumcpue)
dev.new(width = 11,height = 7)
plot(simplemod)
text(simplemod)
savePlot("yft_rpart_tree", type = "png")

########################
#Clustering

library("date")
library("splines")
library("maps")
library("mapdata")
library("maptools")
library("lunar")
library("mgcv")
library("randomForest")
library("influ")
library("nFactors")
library("plyr")
library("dplyr")
library("data.table")
library("cluster")
library("beanplot")
library("cpue.rfmo")

projdir <- "~/ICCAT/2019_YFT/"
twdir <- paste0(projdir, "TW/")
datadir1 <- paste0(twdir, "data/")
twalysis_dir <- paste0(twdir, "analyses/")
twfigs <- paste0(twdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")

clustdir_all <- paste0(twdir,"clustering_all/")
dir.create(clustdir_all)
setwd(clustdir_all)
load(file = "../analyses/TWdat.RData")

tw_allsp <- c("alb","bet","bft","yft","ott","swo","mls", "blm", "bum", "otb", "skj", "sha", "oth", "sbt")
flag = "TW"
for (r in c(1:3)) {
  dev.new(15,12); par(mfrow = c(5,3), mar = c(3,2,2,1), oma = c(0,0,2,0))
  a <- dat[dat$regB == r,]
  for (sp in tw_allsp) plot(sort(unique(a$yrqtr)),tapply(a[,sp], a$yrqtr, mean), main = sp)
  title(paste("Region", r ), outer = TRUE)
  savePlot(filename = paste("freq",flag,"Region", r, "allyrs", sep = "_"), type = "png")
}

use_allsp_allyrs <- c("alb","bet","yft","ott","swo","mls","bum","otb")

allabs <- c("vessid","callsign","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tripid","tripidmon","moon","bt1","bt2","bt3","bt4","bt5",use_allsp_allyrs,"Total","sst","dmy","lat","lon","lat5","lon5","regY","regY1","regY2")

##########
# All years included, YFT regions
rm(datold,pd,prepdat,dat1,dat2,ds,dat_std,junk,a1,a2,a3,a4,aprep,simplemod,rwd,llvall,d2,cld,astd,llvstd,llx,llvold,vvv,llv2)

nclY1 = c(3,4,3)
flag = "TW"

cvn <- c("yrqtr","latlong","hooks","hbf","vessid","callsign","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")
regtype = "regY1"
for (r in c(1,2,3)) {
  fnh <- paste(flag,regtype,r,sep = "_")
  dataset <- clust_PCA_run(r = r,ddd = dat,allsp = use_allsp_allyrs,allabs = allabs,regtype = regtype,ncl = nclY1[r],plotPCA = F,clustid = "tripidmon",allclust = F,flag = flag,fnhead = fnh,covarnames = cvn)
  save(dataset,file = paste0(fnh,".RData"))
}

# --------------
clustdir_1995 <- paste0(twdir,"clustering_1995/")
dir.create(clustdir_1995)
setwd(clustdir_1995)

use_allsp_1995 <- c("alb","bet","yft","swo","mls", "bum", "otb", "sha", "oth", "sbt")
allabs <- c("vessid","callsign","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tripid","tripidmon","moon","bt1","bt2","bt3","bt4","bt5",use_allsp_1995,"Total","sst","dmy","lat","lon","lat5","lon5","regY1", "regY2")
dat95 <- dat[dat$yrqtr > 1995,]

for (r in c(1:3)) {
  dev.new(15,12); par(mfrow = c(5,3), mar = c(3,2,2,1), oma = c(0,0,2,0))
  a <- dat95[dat95$regY1 == r,]
  for (sp in tw_allsp) plot(sort(unique(a$yrqtr)),tapply(a[,sp], a$yrqtr, mean), main = sp)
  title(paste("Region", r ), outer = TRUE)
  savePlot(filename = paste("freq",flag,"Region", r, "1995", sep = "_"), type = "png")
}

nclY1 = c(3,3,4)
cvn <- c("yrqtr","latlong","hooks","hbf","vessid","callsign","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")
regtype = "regY1"
for (r in c(1,2,3)) {
  fnh <- paste(flag,regtype,r,sep = "_")
  dataset <- clust_PCA_run(r = r,ddd = dat95,allsp = use_allsp_1995,allabs = allabs,regtype = regtype,ncl = nclY1[r],plotPCA = F,clustid = "tripidmon",allclust = F,flag = flag,fnhead = fnh,covarnames = cvn)
  save(dataset,file = paste0(fnh,".RData"))
}

nclY2 = c(4,4,4,4,4,4)
cvn <- c("yrqtr","latlong","hooks","hbf","vessid","callsign","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")
regtype = "regY2"
for (r in c(1,2,3,4,5,6)) {
  fnh <- paste(flag,regtype,r,sep = "_")
  dataset <- clust_PCA_run(r = r,ddd = dat95,allsp = use_allsp_1995,allabs = allabs,regtype = regtype,ncl = nclY2[r],plotPCA = F,clustid = "tripidmon",allclust = F,flag = flag,fnhead = fnh,covarnames = cvn)
  save(dataset,file = paste0(fnh,".RData"))
}

######################################
# --------------
clustdir_2005 <- paste0(twdir,"clustering/")
dir.create(clustdir_2005)
setwd(clustdir_2005)

use_allsp_2005 <- c("alb","bet","yft","swo","mls", "bum", "otb", "sha", "oth", "sbt")
allabs <- c("vessid","callsign","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tripid","tripidmon","moon","bt1","bt2","bt3","bt4","bt5",use_allsp_2005,"Total","sst","dmy","lat","lon","lat5","lon5","regY1", "regY2")
dat5 <- dat[dat$yrqtr > 2005,]

for (r in c(1:3)) {
  dev.new(15,12); par(mfrow = c(5,3), mar = c(3,2,2,1), oma = c(0,0,2,0))
  a <- dat5[dat5$regY1 == r,]
  for (sp in tw_allsp) plot(sort(unique(a$yrqtr)),tapply(a[,sp], a$yrqtr, mean), main = sp)
  title(paste("Region", r ), outer = TRUE)
  savePlot(filename = paste("freq",flag,"Region", r, "2005", sep = "_"), type = "png")
}

nclY1 = c(3,4,4)
cvn <- c("yrqtr","latlong","hooks","hbf","vessid","callsign","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon","regY1","regY2")
regtype = "regY1"
for (r in c(1,2,3)) {
  fnh <- paste(flag,regtype,r,sep = "_")
  dataset <- clust_PCA_run(r = r,ddd = dat5,allsp = use_allsp_2005,allabs = allabs,regtype = regtype,ncl = nclY1[r],plotPCA = F,clustid = "tripidmon",allclust = F,flag = flag,fnhead = fnh,covarnames = cvn)
  save(dataset,file = paste0(fnh,".RData"))
}

######################################


#################### Run standardization models for each species ##########################
#***********************************************
#  RUN MULTISPECIES STANDARDIZATION PROCEFURES #
#***********************************************
#
# R1 - 4 clusters. 1 = alb+yft, 2 = alb+yft, 3=alb+otb, 4=bet+swo. Use 4.
# R2 - 3 clusters. 1=alb, 2=bet+yft+oth+sha, 3=bet. Use 2,3
# R3 - Maybe none

library("date")
library("splines")
library("maps")
library("mapdata")
library("maptools")
library("lunar")
library("mgcv")
library("randomForest")
library("influ")
library("nFactors")
library("plyr")
library("dplyr")
library("data.table")
library("cluster")
library("beanplot")
library("cpue.rfmo")

projdir <- "~/ICCAT/2019_YFT/"
twdir <- paste0(projdir, "TW/")
datadir1 <- paste0(twdir, "data/")
twalysis_dir <- paste0(twdir, "analyses/")
twfigs <- paste0(twdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")

std_dir <- paste0(twalysis_dir,"std_cl_TWonly_95hbf/")
dir.create(std_dir)
setwd(std_dir)

load(file = paste0(twalysis_dir, "TWdat.RData"))

use_splist <- use_allsp_1995
stdlabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks",use_splist,"lat","lon","lat5","lon5","reg","hcltrp","flag")

dat <- data.frame(dat)

#clkeepCN_Y1 <- list("yft" = list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepJP_Y1 <- list("yft" = list(c(1,2,4),c(1,2,3,4),c(1,2,3)))
clkeepKR_Y1 <- list("yft" = list(c(0),c(1,2,3,4),c(1,2,3)))
clkeepTW_Y1 <- list("yft" = list(c(1,2,3),c(1,2,3),c(1,2,3,4)))
clkeepUS_Y1 <- list("yft" = list(c(2,3),c(1,3),c(0)))
clk_Y1 <- list(JP = clkeepJP_Y1,KR = clkeepKR_Y1,TW = clkeepTW_Y1,US = clkeepUS_Y1)

runpars <- list()
runpars[["yft"]] <- list(regtype = "regY1", regtype2 = "Y1", clk = clk_Y1, doregs = 1:3, addcl = TRUE, dohbf = TRUE, cltype = "hcltrp")

runsp <- "yft"; runreg <- 2
maxyr <- 2019; maxqtrs <- 200; minqtrs_byreg <- c(5,5,5); keepd <- TRUE
for (runsp in c("yft")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  jdat <- data.frame()
  for (flag in c("TW")) {
    for (r in runpars[[runsp]]$doregs) {
      load(paste0(projdir,flag,"/clustering/",paste(flag,regtype,r,sep = "_"),".RData"))
      dataset$flag <- flag
      jdat <- rbind(jdat,dataset[,stdlabs])
      rm(dataset)
    }
  }
  jdat <- jdat[jdat$yrqtr < maxyr,]
  jdat$vessidx <- jdat$vessid
  jdat$vessid <- paste0(jdat$flag,jdat$vessid)
  jdat$vessid <- as.factor(jdat$vessid)

  vars <- c("vessid","hooks","yrqtr","latlong","hbf")
  for (runreg in runpars[[runsp]]$doregs) {
    minqtrs <- minqtrs_byreg[runreg]
    glmdat <- select_data_JointIO(jdat,runreg = runreg,clk = clk,minqtrs = minqtrs,runsp = runsp,mt = "deltabin",vars = vars,maxqtrs = maxqtrs, minvess = 50,minll = 50,minyrqtr = 50,addcl = addcl,cltype = cltype,addpca = NA,samp = NA,strsmp = NA)
    if (nrow(glmdat) > 60000) glmdat <- samp_strat_data(glmdat,60)
    wtt.all   <- mk_wts(glmdat,wttype = "area")
    fmla.oplogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = T,nhbf = 3)
    fmla.oplogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = F,nhbf = 3)
    fmla.boatlogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = T,nhbf = 3)
    fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = F,nhbf = 3)
    mn <- with(glmdat,0.1 *  mean(get(runsp)/hooks))

    modlab = "lognC_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if (lu(glmdat$clust) > 1)
    { model <- glm(fmla.oplogn,data = glmdat,weights = wtt.all,family = "gaussian", model = keepd);gc() } else
    { model <- glm(fmla.oplogn_ncl,data = glmdat,weights = wtt.all,family = "gaussian", model = keepd);gc() }
    summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

    modlab = "lognC_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if (lu(glmdat$clust) > 1)
    { model <- glm(fmla.boatlogn,data = glmdat,weights = wtt.all,family = "gaussian", model = keepd);gc() } else
    { model <- glm(fmla.boatlogn_ncl,data = glmdat,weights = wtt.all,family = "gaussian", model = keepd);gc() }
    summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

    # delta lognormal
    modlab = "dellog_novess_allyrs"; fname <- paste0("Joint_", regtype,"_R",runreg);
    do_deltalog(dat = glmdat,dohbf = dohbf,addboat = F,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

    modlab = "dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat = glmdat,dohbf = dohbf,addboat = T,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

    graphics.off()
  }
}

#######################------------------------------------

std_dir <- paste0(twalysis_dir,"std_cl_TWonly_95hbf/")
dir.create(std_dir)
setwd(std_dir)

use_splist <- use_allsp_1995
stdlabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks",use_splist,"lat","lon","lat5","lon5","reg","hcltrp","flag")

dat <- data.frame(dat)

#clkeepCN_Y2 <- list("yft" = list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepJP_Y2 <- list("yft" = list(c(1,2,4),c(1,2,3,4),c(1,2,3),c(0),c(0),c(0)))
clkeepKR_Y2 <- list("yft" = list(c(0),c(1,2,3,4),c(1,2,3),c(0),c(0),c(0)))
clkeepTW_Y2 <- list("yft" = list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepUS_Y2 <- list("yft" = list(c(2,3),c(1,3),c(0),c(0),c(0),c(0)))
clk_Y2 <- list(JP = clkeepJP_Y2, KR = clkeepKR_Y2, TW = clkeepTW_Y2, US = clkeepUS_Y2)

runpars <- list()
runpars[["yft"]] <- list(regtype = "regY2", regtype2 = "Y2", clk = clk_Y2, doregs = 4:6, addcl = TRUE, dohbf = TRUE, cltype = "hcltrp")

runsp <- "yft"; runreg <- 2
maxyr <- 2018; maxqtrs <- 200; minqtrs_byreg <- c(5,5,5,5,5,5); keepd <- TRUE
for (runsp in c("yft")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  jdat <- data.frame()
  for (flag in c("TW")) {
    for (r in runpars[[runsp]]$doregs) {
      load(paste0(projdir,flag,"/clustering/",paste(flag,regtype,r,sep = "_"),".RData"))
      dataset$flag <- flag
      jdat <- rbind(jdat,dataset[,stdlabs])
      rm(dataset)
    }
  }
  jdat <- jdat[jdat$yrqtr < maxyr,]
  jdat$vessidx <- jdat$vessid
  jdat$vessid <- paste0(jdat$flag,jdat$vessid)
  jdat$vessid <- as.factor(jdat$vessid)

  vars <- c("vessid","hooks","yrqtr","latlong","hbf")
  for (runreg in runpars[[runsp]]$doregs) {
    minqtrs <- minqtrs_byreg[runreg]
    glmdat <- select_data_JointIO(jdat,runreg = runreg,clk = clk,minqtrs = minqtrs,runsp = runsp,mt = "deltabin",vars = vars,maxqtrs = maxqtrs, minvess = 50,minll = 50,minyrqtr = 50,addcl = addcl,cltype = cltype,addpca = NA,samp = NA,strsmp = NA)
    if (nrow(glmdat) > 60000) glmdat <- samp_strat_data(glmdat,60)
    wtt.all   <- mk_wts(glmdat,wttype = "area")
    fmla.oplogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = T,nhbf = 3)
    fmla.oplogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = F,nhbf = 3)
    fmla.boatlogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = T,nhbf = 3)
    fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = F,nhbf = 3)
    mn <- with(glmdat,0.1 *  mean(get(runsp)/hooks))

    modlab = "lognC_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if (lu(glmdat$clust) > 1)
    { model <- glm(fmla.oplogn,data = glmdat,weights = wtt.all,family = "gaussian", model = keepd);gc() } else
    { model <- glm(fmla.oplogn_ncl,data = glmdat,weights = wtt.all,family = "gaussian", model = keepd);gc() }
    summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

    modlab = "lognC_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if (lu(glmdat$clust) > 1)
    { model <- glm(fmla.boatlogn,data = glmdat,weights = wtt.all,family = "gaussian", model = keepd);gc() } else
    { model <- glm(fmla.boatlogn_ncl,data = glmdat,weights = wtt.all,family = "gaussian", model = keepd);gc() }
    summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

    # delta lognormal
    modlab = "dellog_novess_allyrs"; fname <- paste0("Joint_", regtype,"_R",runreg);
    do_deltalog(dat = glmdat,dohbf = dohbf,addboat = F,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

    modlab = "dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat = glmdat,dohbf = dohbf,addboat = T,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

    graphics.off()
  }
}

