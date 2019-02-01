projdir <- "~/IATTC/2019_CPUE/"
twdir <- paste0(projdir, "TW/")
datadir1 <- paste0(twdir, "data/")
twalysis_dir <- paste0(twdir, "analyses/")
twfigs <- paste0(twdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
dir.create(twdir)
dir.create(datadir1)
dir.create(twalysis_dir)
dir.create(twfigs)

setwd(twalysis_dir)

#install.packages("RColorBrewer")
#install.packages("colorspace")
library(RColorBrewer)
library(tidyverse)
library(stringi)
library(htmlwidgets)
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
library("colorspace")

#install.packages("devtools")
library(devtools)
# This new library replaces the 'support functions.r' file.
#install_github("hoyles/cpue.rfmo")

library("cpue.rfmo")

#source(paste0(Rdir,"support_functions.r"))
#xsd # stop!

# ===================================================================================
# Please keep the data format consistent between years and for the ICCAT + IOTC analyses.

nms2 <- c( "callsign","op_yr","op_mon","op_day","lon5","lat5","lon1","lat1", "hooks","hbf","alb","bet","yft","ott","swo","mls","bum","blm","otb","skj","sha","oth","alb_w","bet_w","yft_w","ott_w","swo_w","mls_w","bum_w","blm_w","otb_w","skj_w","sha_w","oth_w")
# is tun == ott?
# is bil == otb?
# is skx == sha?
# pbf is missing but probably caught to some extent. In tun / ott?

# All these are missing but availabkle in logbooks. Request any of them? such as bait , oil, foc, rem.
# "sst","bait1","bait2","bait3","bait4","bait5","hookdp","target","group","NS","op_lat","EW","op_lon","cpr","embark_yr","embark_mn","embark_dd","op_start_yr","op_start_mn","op_start_dd","op_end_yr","op_end_mn","op_end_dd","debark_yr","debark_mn","debark_dd","oil","foc","rem")

DataFile1<-"Logbook_PAC.csv"
indat1<-read_csv(paste0(datadir1,DataFile1))
names(indat1)  <- nms2

indat2 <- type.convert(indat1)

save(indat2,file = paste0(twalysis_dir, "dat1.RData"))
load(paste0(twalysis_dir, "dat1.RData"))


# Prepare data
splist <- c("alb", "bet","yft", "ott", "swo", "mls", "bum", "blm", "otb", "skj", "sha", "oth")
prepdat1 <- dataprep_TW_EPO(indat2, alldat = F, region = "EPO", splist = splist)
prepdat <- setup_EPO_regions(prepdat1,  regBall = TRUE, regBepo = TRUE, regBwcpo = TRUE)

dat <- dataclean_TW_EPO(prepdat, rmssp = F, splist = splist)
save(dat,file = "TWdat_all.RData")
save(dat,file = "TWdat.RData")
dat <-    dataclean_TW_EPO(prepdat, rmssp = T, splist = splist)
save(dat,file = "TWdat_nossp.RData")

#----------------------------------------------
load(file = "TWdat.RData")


table(prepdat$regBall)

a <- with(prepdat1, tapply(hooks, list(lon,lat), sum, na.rm = TRUE))
aa <- dim(a)
windows(width = 15,height = 10)
image(as.numeric(rownames(a)),as.numeric(colnames(a)), log(a), xlab = "Longitude",ylab = "Latitude")
contour(as.numeric(rownames(a)),as.numeric(colnames(a)), log(a), add = TRUE)
map("world2", add = T, fill = TRUE)
abline(v=210, col = "slate grey", lwd=2)
lines(c(250, 250), c(-50, 24.5), lwd = 2, col = "slate grey", lty = 1)
lines(c(210, 290), c(-10, -10), lwd = 2, col = "slate grey", lty = 1)
lines(c(210, 250), c(10, 10), lwd = 2, col = "slate grey", lty = 1)
savePlot("TW_effort_loghooks.png", type = "png")

windows(width=20, height = 12); par(mfrow = c(3,4), mar = c(2,2,2,0.5)+.1, oma = c(3,3,0,0))
dy <- 5 * floor(prepdat$op_yr/5)
for(dec in seq(1960, 2015, 5)) {
  declab <- paste(dec, "to", dec+ 4)
  ad <- prepdat[dy == dec,]
  a <- with(ad, tapply(hooks, list(lonf,latf), sum, na.rm = TRUE))
  aa <- dim(a)
  image(as.numeric(rownames(a)),as.numeric(colnames(a)), log(a), xlab = "Longitude",ylab = "Latitude", xlim = c(120, 285), ylim = c(-45, 46), main = declab, xaxt="n")
  axis(1, at = c(160, 210, 260), labels = c(-200, -150, -100))
#  contour(as.numeric(rownames(a)),as.numeric(colnames(a)), log(a), add = TRUE)
  map("world2", add = T, fill = TRUE)
  abline(v=210, col = "slate grey", lwd = 1, lty=1)
  lines(c(250, 250), c(-50, 25), lwd = 1, col = "slate grey", lty = 1)
  lines(c(210, 290), c(-10, -10), lwd = 1, col = "slate grey", lty = 1)
  lines(c(210, 250), c(10, 10), lwd = 1, col = "slate grey", lty = 1)
}
mtext("Latitude", side = 1, outer = TRUE, line = 1)
mtext("Longitude", side = 2, outer = TRUE, line = 1)
savePlot("TW_effort_by_5yrs.png", type = "png")

table(prepdat$tonnage, useNA = "always")
#   levs <- cbind(c("0", "1", "2", "3", "4", "5", "6", "7", "8"),
# c(" < 5", "5 -10", "10 -20", "20 -50", "50 -100", "100 -200", "200 -500", "500 -1000"," >= 1000"))

a <- as.data.frame(prepdat)
head(a)

#----------------------------------------------
# check and clean the data

# ensure that the number of fish is less then or equal the number of hooks
hist(prepdat$hooks); summary(prepdat$hooks)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##KOR:
##240    2350    2560    2620    2860    8814
#TWN:
#170    1540    2700    2509    3100    7568
# another aspect to consider is exclude hooks(x) : x >= 1,000 and x < 5,500
table(prepdat$hooks<=1000)
# FALSE   TRUE
# 566522  19110
table(prepdat$hooks>5500)
# FALSE   TRUE
# 585622     10
hist(prepdat$Total); summary(prepdat$Total)
##KOR:
##Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
##0.00    17.00    29.00    38.21    48.00 2137.0
#TWN:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.00   26.00   44.00   60.77   75.00 1211.00
#are there outliers?
table(prepdat$Total>prepdat$hooks)
#FALSE
#585632
table(is.na(prepdat$Total))
#FALSE
#585632
which(prepdat$Total>prepdat$hooks)
#integer(0)
table(is.na(prepdat$op_yr))
#FALSE
#585632
table(prepdat$op_yr<1975)
#92815 cases before 1975
#FALSE   TRUE
#492817  92815
hist(prepdat$hbf)
table(prepdat$hbf) #
#3     4     5     6     7     8     9    10    11    12    13    14    15    16    17
#1   468   149   122   560    82  8266 34245 26957 16507  5409  7563 28957 57346 97649
#18    19    20    21    22    23    24    25    26    27    28    29    30    38   127
#18451  7704  2226   987   953   986   341  5237   583   868    83   413    28     5     1
# Maybe is shallow water, they use less HBF, DO NOT REMOVE
# 127 is data entry error, should be 17, changed
#prepdat<-prepdat[prepdat$hbf>2,] #Nothing was removed here:
dim(prepdat)
#[1] 585632     47
###Vessels:
sort(unique(prepdat$callsign))
# [1] 20269 31672 35339 35748 37656 37701 37957 37986 38540 38542 38544 38555 38587 38596
# [15] 38608 38614 38654 38656 38657 38660 38668 38674 38676 38698 38699 38701 38713 38719
# [29] 38721 38729 38737 38745 38750 38850 38858 38867 38924 38957 38960 38986 39005 39362
# [43] 40058 40144 40146 40303 40323 40326 40343 40348 40354 40357 40437 41248 41299 41368...

length(unique(prepdat$callsign))
#[1] 942
#942 Unique Vessel IDs

lenzero <- function(x) sum(x > 0)
prepdat$nsp <- apply(prepdat[, splist], 1, lenzero)
table(prepdat$nsp, useNA="always")

# Plot single species distribution
a <- with(prepdat, tapply(nsp > 1, list(lon,lat), mean, na.rm = TRUE))
aa <- dim(a)
windows(width = 15,height = 10)
image(as.numeric(rownames(a)),as.numeric(colnames(a)), (a), xlab = "Longitude",ylab = "Latitude")
contour(as.numeric(rownames(a)),as.numeric(colnames(a)), (a), add = TRUE)
map("world2", add = T, fill = TRUE)
abline(v=210, col = "slate grey", lwd=2)
lines(c(250, 250), c(-50, 24.5), lwd = 2, col = "slate grey", lty = 1)
lines(c(210, 290), c(-10, -10), lwd = 2, col = "slate grey", lty = 1)
lines(c(210, 250), c(10, 10), lwd = 2, col = "slate grey", lty = 1)
savePlot("TW_single_species.png", type = "png")

windows(width=20, height = 12); par(mfrow = c(3,4), mar = c(2,2,2,0.5)+.1, oma = c(3,3,0,0))
dy <- 5 * floor(prepdat$op_yr/5)
for(dec in seq(1960, 2015, 5)) {
  declab <- paste(dec, "to", dec+ 4)
  ad <- prepdat[dy == dec,]
  a <- with(ad, tapply(nsp > 1, list(lonf,latf), mean, na.rm = TRUE))
  aa <- dim(a)
  image(as.numeric(rownames(a)),as.numeric(colnames(a)), (a), xlab = "Longitude",ylab = "Latitude", xlim = c(120, 285), ylim = c(-45, 46), main = declab, xaxt="n")
  axis(1, at = c(160, 210, 260), labels = c(-200, -150, -100))
  #  contour(as.numeric(rownames(a)),as.numeric(colnames(a)), log(a), add = TRUE)
  map("world2", add = T, fill = TRUE)
  abline(v=210, col = "slate grey", lwd = 1, lty=1)
  lines(c(250, 250), c(-50, 25), lwd = 1, col = "slate grey", lty = 1)
  lines(c(210, 290), c(-10, -10), lwd = 1, col = "slate grey", lty = 1)
  lines(c(210, 250), c(10, 10), lwd = 1, col = "slate grey", lty = 1)
}
mtext("Latitude", side = 1, outer = TRUE, line = 1)
mtext("Longitude", side = 2, outer = TRUE, line = 1)
savePlot("TW_single_species_by_5yrs.png", type = "png")

barplot(table(prepdat$nsp))
str(prepdat)

a <- as.data.frame(prepdat[is.na(prepdat$lon),])

#----------------------------------------------

#splist = c("alb", "bet", "yft", "ott", "swo", "mls", "bum", "blm", "otb", "skj", "sha", "oth")

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
a0 <- dat[match(a,paste(dat$lat,dat$lon)),c("lat","lon","regBepo","regBwcpo")]
for (fld in c("regBepo","regBwcpo")) {
windows(width = 15,height = 10)
  reg <- with(a0,get(fld))
  plot(a0$lon,a0$lat,type = "n",xlab = "Longitude",ylab = "Latitude",main = fld)
  text(a0$lon,a0$lat,labels = reg,cex = 0.6,col = reg + 1)
  map("world2", add = T, fill = TRUE)
  savePlot(paste0("map_",fld),type = "png")
}

table(is.na(dat$embark_dmy),dat$op_yr)
head(dat)

table(prepdat$alb)
a <- table(prepdat$Total - prepdat$hooks)
barplot(a)
a[as.numeric(names(a)) > -1000]
prepdat[prepdat$alb == 3450,]
table(prepdat$bet)
prepdat[prepdat$bet == 1429,]
prepdat[prepdat$bet == 1704,]
prepdat[prepdat$bet == 1173,]
prepdat[prepdat$bet > 1000,] # Sets with v large catches are aggregated, note no. of hooks.
table(dat$bet)
str(dat)
table(dat$bft)
table(prepdat$yft)
table(prepdat$mls)
table(dat$mls)
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
table(prepdat$hbf,prepdat$op_yr,useNA = "always")
a <- table(dat$op_yr,round(dat$hbf,0),useNA = "always")
write.csv(a,"table hbf by year.csv")


# Plot and explore data
#install.packages("rpart")
library(rpart)
a <- as.data.frame(dat[dat$regBepo %in% c(1,3),])
dim(a)
a$betcpue <- a$bet/a$hooks
a$albcpue <- a$alb/a$hooks
a$yftcpue <- a$yft/a$hooks
a$swocpue <- a$swo/a$hooks
a$othcpue <- a$oth/a$hooks
a$mlscpue <- a$mls/a$hooks
a$blmcpue <- a$blm/a$hooks
a$bumcpue <- a$bum/a$hooks
simplemod <- rpart(a$betcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$albcpue + a$othcpue + a$mlscpue + a$blmcpue + a$bumcpue)
windows(width = 11,height = 7)
plot(simplemod)
text(simplemod)

library(randomForest)
table(is.na(a$lat))
a <- as.data.frame(dat[dat$regBepo %in% c(1) & !is.na(dat$hbf) & !is.na(dat$lon),])
a$betcpue <- a$bet/a$hooks
simplefor <- randomForest(a$betcpue ~ a$lon5 + a$lat5 + a$yrqtr + a$swocpue + a$albcpue + a$othcpue + a$mlscpue + a$blmcpue + a$bumcpue)
print(simplefor)
windows(width = 11,height = 7)
varImpPlot(simplefor)
text(varImpPlot,main = NULL)
savePlot("Rforest bet cpue",type = "png")
betfor2 <- randomForest(a$betcpue ~ a$lon + a$lat + a$yrqtr + a$hooks + a$hbf + a$moon)
windows(width = 11,height = 7)
varImpPlot(betfor2)
savePlot("Rforest bet",type = "png")
save(betfor2, file="betfor2.RData")


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

projdir <- "~/IATTC/2019_CPUE/"
twdir <- paste0(projdir, "TW/")
datadir1 <- paste0(twdir, "data/")
twalysis_dir <- paste0(twdir, "analyses/")
twfigs <- paste0(twdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")

str(dat)
clustdir_all <- paste0(twdir,"clustering_all/")
dir.create(clustdir_all)
setwd(clustdir_all)
load(file = "../analyses/TWdat.RData")

tw_allsp <- c("alb","bet","yft","ott","swo","mls", "blm", "bum", "otb", "skj", "sha", "oth")
use_allsp_allyrs <- c("alb","bet","yft","swo","mls","bum","otb","skj")

allabs <- c("vessid","callsign","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tripidmon","moon",use_allsp_allyrs,"Total","dmy","lat","lon","lat5","lon5","regBepo")

flag = "TW"; allreg <- c(1,2,3,4)
for (r in allreg) {
  windows(15,12); par(mfrow = c(4,3), mar = c(3,2,2,1), oma = c(0,0,2,0))
  a <- as.data.frame(dat[dat$regBepo == r,])
  for (sp in tw_allsp) plot(sort(unique(a$yrqtr)),tapply(a[,sp], a$yrqtr, mean), main = sp)
  title(paste("Region", r ), outer = TRUE)
  savePlot(filename = paste("freq",flag,"Region", r, "allyrs", sep = "_"), type = "png")
}

##########
# All years included, YFT regions
rm(datold,pd,prepdat,dat1,dat2,ds,dat_std,junk,a1,a2,a3,a4,aprep,simplemod,rwd,llvall,d2,cld,astd,llvstd,llx,llvold,vvv,llv2)
doreg <- c(1,2,3,4)
nclB = c(5,4,4,5)
flag = "TW"

cvn <- c("yrqtr","latlong","hooks","hbf","vessid","callsign","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")
regtype = "regBepo"
for (r in doreg) {
  rnm <- r
  fnh <- paste(flag,regtype,r,sep = "_")
  dataset <- clust_PCA_run(r,ddd = dat,allsp = use_allsp_allyrs,allabs = allabs,regtype = regtype,ncl = nclB[r],plotPCA = F,clustid = "tripidmon",allclust = F,flag = flag,fnhead = fnh,covarnames = cvn)
  save(dataset,file = paste0(fnh,".RData"))
}

# --------------
clustdir_2005 <- paste0(twdir,"clustering/")
dir.create(clustdir_2005)
setwd(clustdir_2005)

use_allsp_2005 <- c("alb","bet","yft","swo","mls", "bum", "otb", "sha", "oth")
allabs <- c("vessid","callsign","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tripid","tripidmon","moon","bt1","bt2","bt3","bt4","bt5",use_allsp_2005,"Total","sst","dmy","lat","lon","lat5","lon5","regBepo", "regBwcpo")
dat5 <- dat[dat$yrqtr > 2005,]

for (r in c(1:3)) {
  windows(15,12); par(mfrow = c(5,3), mar = c(3,2,2,1), oma = c(0,0,2,0))
  a <- dat5[dat5$regBepo == r,]
  for (sp in tw_allsp) plot(sort(unique(a$yrqtr)),tapply(a[,sp], a$yrqtr, mean), main = sp)
  title(paste("Region", r ), outer = TRUE)
  savePlot(filename = paste("freq",flag,"Region", r, "2005", sep = "_"), type = "png")
}

nclB = c(4,3,4)
cvn <- c("yrqtr","latlong","hooks","hbf","vessid","callsign","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon","regBepo","regBwcpo")
regtype = "regB"
for (r in c(1,2,3)) {
  fnh <- paste(flag,regtype,r,sep = "_")
  dataset <- clust_PCA_run(r = r,ddd = dat5,allsp = use_allsp_2005,allabs = allabs,regtype = regtype,ncl = nclB[r],plotPCA = F,clustid = "tripidmon",allclust = F,flag = flag,fnhead = fnh,covarnames = cvn)
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

projdir <- "~/IATTC/2019_CPUE/"
twdir <- paste0(projdir, "TW/")
datadir1 <- paste0(twdir, "data/Simon_new data_April 20/")
twalysis_dir <- paste0(twdir, "analyses/")
twfigs <- paste0(twdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")

std_dir <- paste0(twalysis_dir,"std_cl_TWonly_nohbf/")
dir.create(std_dir)
setwd(std_dir)

load(file = paste0(twalysis_dir, "TWdat.RData"))

use_splist <- use_allsp_2005
stdlabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks",use_splist,"lat","lon","lat5","lon5","reg","hcltrp","flag")

dat <- data.frame(dat)

#clkeepCN_B <- list("bet" = list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepJP_B <- list("bet" = list(c(1,2,4),c(1,2,3,4),c(1,2,3)))
clkeepKR_B <- list("bet" = list(c(0),c(1,2,3,4),c(1,2,3)))
clkeepTW_B <- list("bet" = list(c(4),c(2,3),c(0)))
clkeepUS_B <- list("bet" = list(c(2,3),c(1,3),c(0)))
clk_B <- list(JP = clkeepJP_B,KR = clkeepKR_B,TW = clkeepTW_B,US = clkeepUS_B)

runpars <- list()
runpars[["bet"]] <- list(regtype = "regBepo", regtype2 = "B", clk = clk_B, doregs = 2, addcl = TRUE, dohbf = FALSE, cltype = "hcltrp")

runsp <- "bet"; runreg <- 2
maxyr <- 2018; maxqtrs <- 200; minqtrs_byreg <- c(5,5,5); keepd <- TRUE
for (runsp in c("bet")) {
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

std_dir <- paste0(twalysis_dir,"std_cl_TWonly_hbf/")
dir.create(std_dir)
setwd(std_dir)

load(file = paste0(twalysis_dir, "TWdat.RData"))

use_splist <- use_allsp_2005
stdlabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks",use_splist,"lat","lon","lat5","lon5","reg","hcltrp","flag")

dat <- data.frame(dat)

#clkeepCN_B <- list("bet" = list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepJP_B <- list("bet" = list(c(1,2,4),c(1,2,3,4),c(1,2,3)))
clkeepKR_B <- list("bet" = list(c(0),c(1,2,3,4),c(1,2,3)))
clkeepTW_B <- list("bet" = list(c(4),c(2,3),c(0)))
clkeepUS_B <- list("bet" = list(c(2,3),c(1,3),c(0)))
clk_B <- list(JP = clkeepJP_B,KR = clkeepKR_B,TW = clkeepTW_B,US = clkeepUS_B)

runpars <- list()
runpars[["bet"]] <- list(regtype = "regBepo", regtype2 = "B", clk = clk_B, doregs = 2, addcl = TRUE, dohbf = TRUE, cltype = "hcltrp")

runsp <- "bet"; runreg <- 2
maxyr <- 2018; maxqtrs <- 200; minqtrs_byreg <- c(5,5,5); keepd <- TRUE
for (runsp in c("bet")) {
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

