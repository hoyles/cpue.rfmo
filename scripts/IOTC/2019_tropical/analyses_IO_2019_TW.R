# Set up directories
projdir <- "~/IOTC/2019_CPUE_ALB/"
twdir <- paste0(projdir, "TW/")
datadir <- paste0(twdir, "data/1979-2017/")
datadir_oil <- paste0(twdir, "data/revisedoildatav2/")
datadir_2019 <- paste0(twdir, "data/")
twylisis_dir <- paste0(twdir, "analyses/")
twfigs <- paste0(twdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
setwd(twylisis_dir)

# Install any missing packages
#install.packages("date")
#install.packages("maps")
#install.packages("mapdata")
#install.packages("maptools")
#install.packages("data.table")
#install.packages("lunar")
#install.packages("dtplyr")
#install.packages("tm")
#install.packages("devtools")
#install.packages("tidyverse")
#devtools::install_github("hadley/readr")

# Load packages
library("date")
library(splines)
library("maps")
library("mapdata")
library("maptools")
library("data.table")
library("lunar")
library(lubridate)
library(plyr)
library(dplyr)
library(dtplyr)
library(tm)
library(devtools)
library(tidyverse)
#library(readr)

# The new library 'cpue.rfmo' replaces the 'support functions.r' file.
# The command 'install_github("hoyles/cpue.rfmo")' should install cpue.rfmo, but currently doesn't work,
# possibly due to a bug in devtools with private github repositories, or something else, I'm still looking.
# The current workaround is either:
# a) download cpue.rfmo from github and compile it into a package, following the instructions here:
# http://kbroman.org/pkg_primer/pages/build.html. This is the best approach; or
# b) download cpue.rfmo from github, and install from the binary package (cpue.rfmo_0.1.0.zip) in the top dir.
# Check first that cpue.rfmo has been recompiled to match the latest source code, which may not be the case.
# c) ask me to email you a copy of the binary package, then install it.

library(cpue.rfmo) # This will produce warnings (usually 19) but they can be ignored.

##################

# Load data. This section will only need to be changed if the data format changes.

# Set up names, since we need the same names across all fleets
nms <- c("callsign","op_yr","op_mon","op_day","op_area","hbf","hooks","alb","bet","yft","pbf","sbf","ott","swo","mls","bum","blm","otb","skj","sha","oth","alb_w","bet_w","yft_w","pbf_w","sbf_w","ott_w","swo_w","mls_w","bum_w","blm_w","otb_w","skj_w","sha_w","oth_w","sst","bait1","bait2","bait3","bait4","bait5","hookdp","target","NS","op_lat","EW","op_lon","cpr","embark_yr","embark_mn","embark_dd","op_start_yr","op_start_mn","op_start_dd","op_end_yr","op_end_mn","op_end_dd","debark_yr","debark_mn","debark_dd","oilv","foc","rem")
nms_oil <- c("callsign","op_yr","op_mon","op_day","op_area","hbf","hooks","alb","bet","yft","pbf","sbf","ott","swo","mls","bum","blm","otb","skj","sha","oth","oil","alb_w","bet_w","yft_w","pbf_w","sbf_w","ott_w","swo_w","mls_w","bum_w","blm_w","otb_w","skj_w","sha_w","oth_w","oil_w","sst","bait1","bait2","bait3","bait4","bait5","hookdp","target","NS","op_lat","EW","op_lon","cpr","embark_yr","embark_mn","embark_dd","op_start_yr","op_start_mn","op_start_dd","op_end_yr","op_end_mn","op_end_dd","debark_yr","debark_mn","debark_dd","oilv","foc","rem")

wdths     <- c(5,4,2,2,4,3,5,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,2,1,1,1,1,1,3,2,1,2,1,3,2,4,2,2,4,2,2,4,2,2,4,2,2,5,5,11)
cc     <- "ciiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiciiiicccccccccccccccc"
wdths_oil <- c(5,4,2,2,4,3,5,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,2,1,1,1,1,1,3,2,1,2,1,3,2,4,2,2,4,2,2,4,2,2,4,2,2,5,5,11)
cc_oil <- "ciiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiciiiicccccccccccccccc"

cbind(c(0,0,wdths), wdths_oil)
c(0,0,wdths)- wdths_oil

# Load data from Sheng-Ping's file
# datNE <- read.csv(paste0(datadir_2019,"CE_Cluster_NE.csv"), stringsAsFactors=FALSE)
# datNW <- read.csv(paste0(datadir_2019,"CE_Cluster_NW.csv"), stringsAsFactors=FALSE)
# datSE <- read.csv(paste0(datadir_2019,"CE_Cluster_SE.csv"), stringsAsFactors=FALSE)
# datSW <- read.csv(paste0(datadir_2019,"CE_Cluster_SW.csv"), stringsAsFactors=FALSE)
#
# rawdat <- rbind(datNE, datNW, datSE, datSW)
# cbind(nms, nms_oil, names(rawdat))
#
# nms <- c("callsign","op_yr","op_mon","op_day","op_area","hbf","hooks","alb","bet","yft","pbf","sbf","ott","swo","mls","bum","blm","otb","skj","sha","oth","alb_w","bet_w","yft_w","pbf_w","sbf_w","ott_w","swo_w","mls_w","bum_w","blm_w","otb_w","skj_w","sha_w","oth_w","sst","bait1","bait2","bait3","bait4","bait5","hookdp","target","NS","op_lat","EW","op_lon","cpr","embark_yr","embark_mn","embark_dd","op_start_yr","op_start_mn","op_start_dd","op_end_yr","op_end_mn","op_end_dd","debark_yr","debark_mn","debark_dd","oilv","foc","rem")
# nms2 <- c("op_yr","op_mon","")

# Load names and widths provided by TW; compare locations and widths
ffo <- read.csv(paste0(datadir,"outcodeP2.csv"), stringsAsFactors=FALSE)
ffo_oil <- read.csv(paste0(datadir_oil,"outcodeP2oil.csv"), stringsAsFactors=FALSE)
cbind(nms, ffo$item, nms_oil, ffo_oil$item)
wdths==ffo$width; sum(wdths!=ffo$width)
wdths_oil==ffo_oil$width; sum(wdths_oil!=ffo_oil$width)

# Load example to check processing
a <- read_fwf(file=paste0(datadir,"LOG2005.IND"),col_positions=fwf_widths(wdths),col_types=cc,n_max=20);gc()
names(a); dim(a)
names(a) <- nms
head(data.frame(a))
cbind(nms,wdths,cumsum(c(wdths)),unlist(strsplit(cc,"")))

ao <- read_fwf(file=paste0(datadir_oil,"LOG2005.IND"),fwf_widths(wdths_oil),col_types=cc_oil,n_max=20);gc()
names(ao); dim(ao)
names(ao) <- nms_oil
head(as.data.frame(ao))

# Load all the files
yy <- 1979:2017;yy <- paste0("/LOG",yy,".IND")
readfun1 <- function(ff) {
  read_fwf(paste0(datadir,ff),fwf_widths(wdths),col_types=cc)
}
readfun_oil <- function(ff) {
  read_fwf(paste0(datadir_oil,ff),fwf_widths(wdths_oil),col_types=cc_oil)
}
a <- lapply(yy,readfun1)
a_oil <- lapply(yy,readfun_oil)
system.time({ dat1 <- ldply(a, data.frame) })
system.time({ dat1_oil <- ldply(a_oil, data.frame) })
names(dat1) <- nms
names(dat1_oil) <- nms_oil
save(dat1,dat1_oil, file="dat1.RData")
load(paste0(twylisis_dir, "dat1.RData"))

makekey <- function(x) {
  paste(a$vessid, a$op

}

table(dat1[dat1$op_yr==2017,]$oth)

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

splist1 <- c("alb", "bet","yft", "ott", "swo", "mls", "bum", "blm", "otb", "skj", "sha", "oth", "pbf", "sbt")
splist_oil <- c("alb", "bet","yft", "ott", "swo", "mls", "bum", "blm", "otb", "skj", "sha", "oth", "pbf", "sbt", "oil")
# initial data preparation. Failures to parse (19, 167, 352) due to bad dates (Feb 29, April 31 etc)

# ========================
# Prepare and check the data
# ========================

prepdat1 <-     dataprep_TW(dat1,     splist = splist1)
prepdat1_oil <- dataprep_TW(dat1_oil, splist = splist_oil)

prepdat <- setup_IO_regions(prepdat1,  regY=T, regY1=T, regY2=T, regB=T, regB1 = T, regB2=T, regB3=T, regA=T, regA1=T, regA2=T, regA3=T, regA4=T, regA5=T)
prepdat_oil <- setup_IO_regions(prepdat1_oil,  regY=T, regY1=T, regY2=T, regB=T, regB1 = T, regB2=T, regB3=T, regA=T, regA1=T, regA2=T, regA3=T, regA4=T, regA5=T)
datold <-     dataclean_TW(prepdat, splist = splist1)
datold_oil <- dataclean_TW(prepdat_oil, splist = splist_oil)
save(datold,datold_oil, file="TWdat_old.RData")

splist2 <- splist1[splist1 != "pbf"] # remove 'pbf' which was removed by 'dataprep'
splist_oil2 <- splist_oil[splist_oil != "pbf"]
dat <-     dataclean_TW(prepdat, rmssp=T, splist = splist2)
dat_oil <- dataclean_TW(prepdat_oil, rmssp=T, splist = splist_oil2)
save(dat, dat_oil, file="TWdat.RData")
getwd()

windows(12,9)
plot(1979:2017,tapply(dat_oil$oil, list(dat_oil$op_yr), mean), ylim = c(0, 90), xlim = c(1990, 2018))
points(1979:2017,tapply(dat_oil$oth, list(dat_oil$op_yr), mean), col = 2, pch = 2)
points(1979:2017,tapply(dat$oth, list(dat$op_yr), mean), col = 3, pch = 3)
points(1979:2017,tapply(dat_oil$oth + dat_oil$oil, list(dat_oil$op_yr), mean), col = 4, pch = 4)
legend("topleft", legend = c("dat_oil$oil","dat_oil$oth", "dat$oth", "dat_oil$oil + dat_oil$oth"), pch=1:4, col = 1:4)
savePlot("oil_v_oth_1", type = "png")

windows(12,9)
o1 <- tapply(dat$oth, list(dat$op_yr), mean)
o2 <- tapply(dat_oil$oth + dat_oil$oil, list(dat_oil$op_yr), mean)
plot(1979:2017,o1 - o2, xlim = c(2000, 2018))
legend("bottomleft", legend = "dat$oth - (dat_oil$oth + dat_oil$oil)", col=1, pch = 1)
savePlot("oil_v_oth_2", type = "png")

# It looks like the best option is to use oth + oil
dat_oil$ot2 <- dat_oil$oth + dat_oil$oil
dat <- dat_oil

save(dat, file = "../analyses/TW_newdat.RData")

# ===================================================================================
# check, plot and explore the data
# ===================================================================================
load(file="../analyses/TW_newdat.RData")

#table(dat$op_lon,dat$lon,useNA="always")
table(dat$op_lon,useNA="always")
table(dat$lon,useNA="always")

# Make maps to check regional patterns
a <- unique(paste(dat$lat,dat$lon))
a0 <- dat[match(a,paste(dat$lat,dat$lon)),c("lat","lon","regY","regY1","regY2","regB","regB1","regB2","regB3","regA","regA1","regA2","regA3","regA4","regA5")]
windows(width=15,height=10)
for(fld in c("regY","regY1","regY2","regB","regB1","regB2","regB3","regA","regA1","regA2","regA3","regA4","regA5")) {
  reg <- with(a0,get(fld))
  plot(a0$lon,a0$lat,type="n",xlab="Longitude",ylab="Latitude",main=fld)
  text(a0$lon,a0$lat,labels=reg,cex=0.6,col=reg+1)
  maps::map(database = "world", add=T, fill = F)
  savePlot(paste0("map_",fld),type="png")
}

table(is.na(dat$embark_dmy),dat$op_yr)
head(dat)

# Look for outliers. Individual sets with high catch are not a problem.
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

# Exploratory regression trees. These are not particularly useful.
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


# ===================================================================================
# Start the analysis proper
# ===================================================================================
#Clustering

library("maps")
library("mapdata")
library("mgcv")
library("randomForest")
library("influ")
library("nFactors")
library("data.table")
library("plyr")
library("dplyr")
library("cluster")
library("splines")
library("boot")
library("beanplot")
library("cpue.rfmo")

projdir <- "~/IOTC/2019_CPUE_ALB/"
twdir <- paste0(projdir, "TW/")
twylisis_dir <- paste0(twdir, "analyses/")
Rdir <- paste0(projdir, "Rfiles/")
clustdir <- paste0(twdir,"clustering/")
setwd(clustdir)
#load(file="../analyses/TWdat.RData")
load(file="../analyses/TW_newdat.RData")

tw_allsp <- c("alb","bet","yft","ott","swo","mls","blm", "bum", "otb", "skj", "sha", "ot2", "sbt")

# Plot the mean catch per year of each species by region, to use when deciding which species to cluster
# plot_spfreqyq(indat = dat, reg_struc = "regY", splist = tw_allsp, flag = "TW", mfr = c(5,3))
# plot_spfreqyq(indat = dat, reg_struc = "regY2", splist = tw_allsp, flag = "TW", mfr = c(5,3))
plot_spfreqyq(indat = dat, reg_struc = "regA4", splist = tw_allsp, flag = "TW", mfr = c(5,3))
plot_spfreqyq(indat = dat, reg_struc = "regA5", splist = tw_allsp, flag = "TW", mfr = c(5,3))

# Put chosen species here
use_sp <- c("alb","bet","yft","swo","mls","blm", "bum","ot2","sbt")
# Variables to use
allabs <- c("vessid","callsign","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tripid","tripidmon","moon","bt1","bt2","bt3","bt4","bt5",use_sp,"Total","sst","dmy","lat","lon","lat5","lon5", "regY","regY2","regB","regB3","regB2","regA","regA1","regA2","regA3","regA4","regA5")


##########
# All years included, YFT regions
rm(a,dat_oil,datold,pd,prepdat,dat1,dat2,ds,dat_std,junk,a1,a2,a3,a4,aprep,simplemod,rwd,llvall,d2,cld,astd,llvstd,llx,llvold,vvv,llv2, o1, o2,r)

# Determine the number of clusters. Come back and edit this.
reglist <- list()
reglist$regA4 <- list(allreg = 1:4, ncl = c(4,4,3,5))
reglist$regA5 <- list(allreg = 1,   ncl = 5)
reglist$regB2 <- list(allreg = 1:4, ncl = c(5,5,4,4))
reglist$regB3 <- list(allreg = 1:5, ncl = c(5,5,4,4,5))
reglist$regY <-  list(allreg = 1:6, ncl = c(3,4,4,4,5,5))
reglist$regY2 <- list(allreg = c(2,7), ncl = c(3,4,4,4,5,5,4))

flag="TW"

# Covariates to pass to next stage
cvn <- c("yrqtr","latlong","hooks","hbf","vessid","callsign","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")

# Do the clustering and save the results for later (we also need to decide on the ALB regional structures below)
run_clustercode_byreg(indat=dat, reg_struc = "regA4", allsp=use_sp, allabs=allabs, flag=flag, cvnames = cvn, rgl = reglist)
run_clustercode_byreg(indat=dat, reg_struc = "regA5", allsp=use_sp, allabs=allabs, flag=flag, cvnames = cvn, rgl = reglist)
# run_clustercode_byreg(indat=dat, reg_struc = "regY", allsp=use_sp, allabs=allabs, flag=flag, cvnames = cvn, rgl = reglist)
# run_clustercode_byreg(indat=dat, reg_struc = "regY2", allsp=use_sp, allabs=allabs, flag=flag, cvnames = cvn, rgl = reglist)

clkeepTW_A4 <- list("alb"=list(c(1:4), c(1:4), c(1,2), c(1:5)))
clk_A4 <- list(TW=clkeepTW_A4)

clkeepTW_A5 <- list("alb"=list(c(1:5)))
clk_A5 <- list(TW=clkeepTW_A5)

# clkeepTW_Y <- list("yft"=list(c(1,2,3), c(1,2,3,4), c(1,2,3), c(1,2,3,4),   c(1,2,3,4,5)), c(1,2,3,4,5))
# clk_Y <- list(TW=clkeepTW_Y)
#
# clkeepTW_Y2 <- list("yft"=list(c(0), c(1,2,3,4),c(0), c(0), c(0), c(0), c(1,2,3,4)))
# clk_Y2 <- list(TW=clkeepTW_Y2)
#
# clkeepTW_B2 <- list("bet"=list(c(1,2,3,4,5),c(1,2,3,4,5),c(2,3),c(1,2,3,4)))
# clk_B2 <- list(TW=clkeepTW_B2)
#
# clkeepTW_B3 <- list("bet"=list(c(1,2,3,4,5),c(1,2,3,4,5),c(2,3),c(1,2,3,4),c(1,2,3,4,5)))
# clk_B3 <- list(TW=clkeepTW_B3)

# ========================================================
# Standardizations, TW only
# ========================================================

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
library("survival")

library("cpue.rfmo")


projdir <- "~/IOTC/2019_CPUE_ALB/"
twdir <- paste0(projdir, "TW/")
twylisis_dir <- paste0(twdir, "analyses/")
Rdir <- paste0(projdir, "Rfiles/")
clustdir <- paste0(twdir,"clustering/")

std_splist <- c("alb","bet","yft")
stdlabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks",std_splist,"lat","lon","lat5","lon5","reg","hcltrp","flag")

## ---------------------------------------------
# Run various standardization scenarios. Only one here now, and for bigeye instead of YFT and ALB.
# There are some new ones from the Madrid meeting which I will set up later.
# I can't test the code without data, so apologies if some of the changes in Madrid have broken this code.
# We can fix it in Keelung.
## ---------------------------------------------

# With clusters, and hbf

cl1_hb0_hk1_dir <- paste0(twdir,"cl_nohb_hk/")
dir.create(cl1_hb0_hk1_dir)
setwd(cl1_hb0_hk1_dir)

# The runpars define the approach to be used in this run
regA4_minss <- list(minq_byreg = c(3,2,5,5), minvess=c(60,40,100,100), minll=c(30,20,50,50), minyrqtr = c(30,20,50,50), minyqll = c(3,3,5,5))
regA5_minss <- list(minq_byreg = c(5), minvess=c(100), minll=c(50), minyrqtr = c(50), minyqll = c(5))
regB3_minss <- list(minq_byreg = c(5,5,5,3,5), minvess=c(100,100,100,60,100), minll=c(50,50,50,30,50), minyrqtr = c(50,50,50,30,50), minyqll = c(5,5,5,3,5))
regY_minss <-  list(minq_byreg = c(2,5,5,2,5,2), minvess=c(40,100,100,40,100,40), minll=c(20,50,50,20,50,20), minyrqtr = c(20,50,50,20,50,20), minyqll = c(3,5,5,3,5,3))
regY2_minss <- list(minq_byreg = c(2,5,5,2,5,2,5), minvess=c(40,100,100,40,100,40,100), minll=c(20,50,50,20,50,20,50), minyrqtr = c(20,50,50,20,50,20,50), minyqll = c(3,5,5,3,5,3,5))

runpars <- list()
runpars[["regA4"]] <-list(runsp = "alb", regtype2 = "A4", clk = clk_A4, doregs = 1:4, addcl = TRUE, dohbf = FALSE, dohook = TRUE, cltype = "hcltrp", minss = regA4_minss)
runpars[["regA5"]] <-list(runsp = "alb", regtype2 = "A5", clk = clk_A5, doregs = 1,   addcl = TRUE, dohbf = TRUE, dohook = TRUE, cltype = "hcltrp", minss = regA5_minss)
runpars[["regY"]] <- list(runsp = "yft", regtype2 =  "Y", clk = clk_Y,  doregs = 2:5, addcl = TRUE, dohbf = FALSE, dohook = TRUE, cltype = "hcltrp", minss = regY_minss)
runpars[["regY2"]] <-list(runsp = "yft", regtype2 = "Y2", clk = clk_Y2, doregs = c(2,7), addcl = TRUE, dohbf = FALSE, dohook = TRUE, cltype = "hcltrp", minss = regY2_minss)

regstr <- "regA4"; runreg <- 2; keepd <- TRUE; doflags <- "TW"
maxyr <- 2018

run_standardization(runpars, doflags = "TW", regstr = "regY",  maxyr = 2018, do_early = FALSE)
run_standardization(runpars, doflags = "TW", regstr = "regY2", maxyr = 2018, do_early = FALSE)


##### Finish here  #### -----------------------------------------------------------------
for (regstr in c("regA4")) {
  rp <- runpars[[regstr]]
  runsp <- rp$runsp
  addcl <- rp$addcl
  dohbf <- rp$dohbf
  jdat <- data.frame()
  for (flag in doflags) {
    for (r in rp$doregs) {
      load(paste0(projdir,flag,"/clustering/",paste(flag,regstr,r,sep = "_"),".RData"))
      dataset$flag <- flag
      dataset$qtr <- revtrunc(defactor(dataset$yrqtr))
      jdat <- rbind(jdat,dataset[,stdlabs])
      rm(dataset)
    }
  }
  jdat <- jdat[jdat$yrqtr < maxyr,]
  jdat$vessidx <- jdat$vessid
  jdat$vessid <- paste0(jdat$flag,jdat$vessid)
  jdat$vessid <- as.factor(jdat$vessid)

  vars <- c("vessid","hooks","yrqtr","latlong","hbf")
  for (runreg in rp$doregs) {
    glmdat <- select_data_IO2(jdat,runreg = runreg,runpars = rp, mt = "deltabin",vars = vars)
    if (nrow(glmdat) > 60000) glmdat <- samp_strat_data(glmdat,60)
    wtt.all   <- mk_wts(glmdat,wttype = "area")
    fmla.oplogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = T,nhbf = 3, dohook = rp$dohook)
    fmla.oplogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = F,nhbf = 3, dohook = rp$dohook)
    fmla.boatlogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = T,nhbf = 3, dohook = rp$dohook)
    fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = F,nhbf = 3, dohook = rp$dohook)
    mn <- with(glmdat,0.1 *  mean(get(runsp)/hooks))

    modlab = "lognC_novess_allyrs"; fname <- paste0("Joint_",regstr,"_R",runreg)
    if (lu(glmdat$clust) > 1)
    { model <- glm(fmla.oplogn,data = glmdat,weights = wtt.all,family = "gaussian", model = keepd);gc() } else
    { model <- glm(fmla.oplogn_ncl,data = glmdat,weights = wtt.all,family = "gaussian", model = keepd);gc() }
    summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

    modlab = "lognC_boat_allyrs"; fname <- paste0("Joint_",regstr,"_R",runreg)
    if (lu(glmdat$clust) > 1)
    { model <- glm(fmla.boatlogn,data = glmdat,weights = wtt.all,family = "gaussian", model = keepd);gc() } else
    { model <- glm(fmla.boatlogn_ncl,data = glmdat,weights = wtt.all,family = "gaussian", model = keepd);gc() }
    summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

    # delta lognormal
    modlab = "dellog_novess_allyrs"; fname <- paste0("Joint_", regstr,"_R",runreg);
    do_deltalog(dat = glmdat,dohbf = dohbf,addboat = F,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd, dohook = rp$dohook)

    modlab = "dellog_boat_allyrs"; fname <- paste0("Joint_",regstr,"_R",runreg)
    do_deltalog(dat = glmdat,dohbf = dohbf,addboat = T,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd, dohook = rp$dohook)

    graphics.off()
  }
}

