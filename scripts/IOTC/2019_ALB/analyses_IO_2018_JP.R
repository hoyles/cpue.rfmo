# Set up directories
projdir <- "~/IOTC/2019_CPUE_ALB/"
jpdir <- paste0(projdir, "JP/")
datadir1 <- paste0(jpdir, "data/")
jalysis_dir <- paste0(jpdir, "analyses/")
jpfigs <- paste0(jpdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
setwd(jalysis_dir)

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
library(tidyverse)
library(plyr)
library(dplyr)
library(dtplyr)
library(tm)
library(devtools)

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
nms <- c("op_yr","op_mon","op_day","lat","latcode","lon","loncode","callsign",
      "hbf","hooks","sbt","alb","bet","yft","swo","mls","bum","blm","trip_st","sas","shk","prefecture","vesselname","logbookid")
wdths <- c(4,2,2,2,1,3,1,6,3,6,3,3,3,3,3,3,3,3,8,3,4,3,30,9)
cc <- "iiiiiiiciiiiiiiiiiiiicci"
posses <- cumsum(c(1,wdths))
cc <- "iiiiiiiciiiiiiiiiiiiicci"
cbind(nms,wdths,unlist(strsplit(cc,"")))

# Load initial test segment of data
a <- read_fwf(file=paste0(datadir1,"/JPNLL_IO_20190104.dat"),fwf_widths(wdths),col_types=cc,n_max=20);gc()
names(a)
names(a) <- nms
head(data.frame(a))

# Load the entire dataset
dat5217 <- read_fwf(file=paste0(datadir1,"/JPNLL_IO_20190104.dat"),fwf_widths(wdths),col_types=cc)
a <- problems(dat5217) # Report problems
a[160:200,]
names(dat5217) <- nms
table(is.na(dat5217$trip_st),dat5217$op_yr)
table(dat5217$op_yr)

# Prepare and check the data
rawdat <- as.data.frame(dat5217)
pd1 <- dataprep_JPIO(rawdat)
pd2 <- setup_IO_regions(pd1, regY=F, regY1=F, regY2=F, regB=F, regB3=F, regB2=F, regA=T, regA1=T, regA2=T, regA3=T, regA4=T, regA5=T)

jp_allsp <- c("sbt","alb","bet","yft","swo","mls","bum","blm","sas","shk")

clndat <- dataclean_JPIO(rawdat, splist = jp_allsp)
prepdat1 <- dataprep_JPIO(clndat)
prepdat <- setup_IO_regions(prepdat1,  regY=F, regY1=F, regY2=F, regB=F, regB1 = F, regB2=F, regB3=F, regA=T, regA1=T, regA2=T, regA3=T, regA4=T, regA5=T)
save(pd1, pd2, prepdat, file="prepdat.RData")

dat <- make_lbidmon(prepdat)
save(dat,file="JPdat.RData")
load(file="JPdat.RData")


# ===================================================================================
# Plot and explore the data
# ===================================================================================
table(dat$op_yr)

# Explore performance of various trip id variables
windows(); par(mfrow = c(2,2))
a3 <- table(as.character(dat$tripidmon[dat$op_yr < 1979]))
hist(a3,nclass=max(a3)/80,main="tripidmon frequency before 1979",xlab="Sets per tripidmon")
a2 <- table(as.character(dat$tripidmon[dat$op_yr >= 1979]))
hist(a2,nclass=max(a2),main="tripidmon frequency from 1979",xlab="Sets per tripidmon")
a4 <- table(as.character(dat$lbid_mon[dat$op_yr < 1979]))
hist(a4,nclass=max(a3)/80,main="lbid_mon frequency before 1979",xlab="Sets per tripidmon")
a5 <- table(as.character(dat$lbid_mon[dat$op_yr >= 1979]))
hist(a5,nclass=max(a3)/80,main="lbid_mon frequency after 1979",xlab="Sets per tripidmon")
savePlot(file="Alternate cluster versions",type="png")
windows()
hist(a2[a2>31])

# Explore vessel identifiers
xfun <- function(x) sum(x > 0)
a <- table(dat$vessid,dat$op_yr)
apply(a,2,xfun)

vnm <- (dat$vesselname)
library(tm)
tm_map(vnm, function(x) content_transformer(iconv(enc2utf8, sub = "byte")))

a <- table(dat$vesselname,dat$op_yr)
apply(a,2,xfun)

# Check a few specific instances where there have been problems in the past. Consider checking for similar problems in new data.
a <- dat[dat$tripidmon=="461 1971 1",]
a <- data.frame(a)
a[,1:15]
a <-unique(dat$callsign)
as.data.frame(dat[match(a,dat$callsign),c("callsign","vessid")])
table(dat$callsign,useNA="always")
table(dat$vessid,useNA="always")

# Make maps to check regional patterns
a <- unique(paste(dat$lat,dat$lon))
a0 <- dat[match(a,paste(dat$lat,dat$lon)),c("lat","lon","regY", "regY1", "regY2", "regB", "regB1", "regB2", "regB3", "regA", "regA1", "regA2", "regA3", "regA4","regA5")]
windows(width=15,height=10)
for(fld in c("regY", "regY1", "regY2", "regB", "regB1", "regB2", "regB3", "regA", "regA1", "regA2", "regA3", "regA4","regA5")) {
  reg <- with(a0,get(fld))
  plot(a0$lon,a0$lat,type="n",xlab="Longitude",ylab="Latitude",main=fld)
  text(a0$lon,a0$lat,labels=reg,cex=0.6,col=reg+1)
  map(add=T)
  savePlot(paste0("map_",fld),type="png")
  }

# Plot effort proportions by yr & region, indicating proportions of strata with > 5000 hooks, i.e. at least 2 sets.
regYord <- c(1,2,3,6,5,4)
windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,2,2,1))
for (r in regYord) {
  llv <- pd2[pd2$regY==r,]
  yq <- seq(1952.125,2015.875,0.25)
  llv$yrqtr <- factor(llv$yrqtr,levels=yq)
  a <- aggregate(hooks ~ lat5 + lon5 + yrqtr,sum,data=llv)
  b <- a[a$hooks > 5000,]
  b <- tapply(b$hooks,b$yrqtr,sum)
  a <- tapply(a$hooks,a$yrqtr,sum)
  yqa <- a[match(yq,names(a))]
  yqb <- b[match(yq,names(b))]
  yqb[is.na(yqb)] <- 0
  ab <- yqb/yqa
  plot(names(ab),ab,ylim=c(0,1),main=paste("Region",r))
}
savePlot(filename="Clean strata hooks",type="png")

# time series of sets
windows(width=15,height=9)
hist(prepdat$dmy,breaks="days",freq=T,xlab="Date",main="Sets per day")
savePlot(file="sets_per_day.png",type="png")
hist(prepdat$dmy,breaks="months",freq=T,xlab="Date",main="Sets per month")
savePlot(file="sets_per_month.png",type="png")
table(prepdat$dmy)

# Map the effort with circles
a <- aggregate(dat$hooks,list(dat$lat5,dat$lon5),sum,na.rm=T)
windows(width=11,height=9)
symbols(x=a[,2],y=a[,1],circles=.0002*sqrt(a[,3]),inches=F,bg=2,fg=2,xlab="Longitude",ylab="Latitude",ylim=c(-50,25),xlim=c(15,145))
map(add=T,interior=F,fill=T)
savePlot(file="map_hooks.png",type="png")

# Dbn of hooks per set
hist(dat$hooks, nclass=60,xlab="Hooks per set")   # ask if very large # hooks is okay
savePlot("Hook histogram.png",type="png")

# Look for outliers. Individual sets with high catch are not a problem.
table(prepdat$alb)   # ask sets with 990 alb
table(prepdat$bet)   # ask set with 461 bet
table(prepdat$yft)   # ask set with 1038
table(prepdat$sbt)   # ask sets with 102 pbf
#table(prepdat$ott)   # ask sets with 186
table(prepdat$swo)   # ask sets with 269
table(prepdat$mls)   # ask set with 454
table(prepdat$bum)   # ask set with 130
table(prepdat$blm)   # ask set with 75 blm
#table(prepdat$otb)   # ask sets with 150
#table(prepdat$skj)   # ask set with 143
#table(prepdat$sha)   # ask majority of sets (=719211) with 0 sha. Also one set with 663
#table(prepdat$oth)   # ask sets with 3059! But most (=636641) have 0.

# Look for patterns in hbf, including NA
table(prepdat$hbf,useNA="always")  # 6408 with NA! All in 1973-75
table(dat$hbf,dat$op_yr,useNA="always")  #
table(dat$op_yr,is.na(dat$hbf))  #
dat <- dat[is.na(dat$hbf)==FALSE | dat$op_yr < 1976,]
a <- table(dat$op_yr,round(dat$hbf,0),useNA="always")
write.csv(a,"table hbf by year.csv")

# Map the effort at 5 degree scale
a <- log(table(dat$lon5,dat$lat5))
windows(width=13,height=10)
image(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,xlab="Longitude",ylab="Latitude")
map("worldHires",add=T, interior=F,fill=F)
savePlot("Setmap_logscale.png",type="png")

# Map the effort at 1 degree scale
a <- with(dat[!is.na(dat$lat) & dat$yrqtr,],log(table(lon,lat)))
windows(width=15,height=10)
image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,xlab="Longitude",ylab="Latitude",ylim=c(-55,30),xlim=c(15,150))
map("worldHires",add=T, interior=F,fill=T)
savePlot("Setmap_logscale_1deg.png",type="png")

# Mean fishing location, yrqtr scale
windows(width=15,height=10);par(mfrow=c(1,2))
ax <- tapply(dat$yrqtr,dat$yrqtr,mean); ay=tapply(dat$lat5,dat$yrqtr,mean)
plot(ax,ay,xlab="yr",ylab="Mean latitude",type="n")
a <- 4*(.125+dat$yrqtr-floor(dat$yrqtr))
a <- tapply(a,dat$yrqtr,mean)
text(ax,ay,a,cex=0.7)
ax=tapply(dat$lon5,dat$yrqtr,mean);ay=tapply(dat$yrqtr,dat$yrqtr,mean)
plot(ax,ay,ylab="yr",xlab="Mean longitude",type="n")
text(ax,ay,a,cex=0.7)
savePlot("mean_fishing_location1.png",type="png")

# Mean fishing location, year scale
windows(width=15,height=10);par(mfrow=c(1,2))
plot(tapply(dat$op_yr,dat$op_yr,mean),tapply(dat$lat5,dat$op_yr,mean),xlab="yr",ylab="Mean latitude")
plot(tapply(dat$lon5,dat$op_yr,mean),tapply(dat$op_yr,dat$op_yr,mean),ylab="yr",xlab="Mean longitude")
savePlot("mean_fishing_location2.png",type="png")

# Store summaries of hbf by region and through time
write.csv(table(round(dat$hbf,0),dat$regY,useNA="always"),file="hbf by region.csv")
write.csv(table(round(dat$hbf,0),floor(dat$yrqtr/5)*5,dat$regY,useNA="always"),file="hbf by region by 5 years.csv")

# Map the average HBF by 5 yr period
windows(20,14);par(mfrow=c(3,3),mar=c(2,2,2,2))
for(y in seq(1975,2015,5)) {
  a <- dat[floor(dat$yrqtr/5)*5==y & dat$lon5 < 125 & dat$lat5 < 25,]
  a <- tapply(a$hbf,list(a$lon5,a$lat5),mean,na.rm=T)
  image(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,main=y,zlim=c(6,24),col=heat.colors(30),xlab="Lon",ylab="Lat",ylim=c(-45,25))
  contour(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,add=T,levels=seq(0,26,2))
  map("world",add=T, interior=F,fill=T) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
}
savePlot("mean_HBF_5.png",type="png")

# Map the average HBF by 5 yr period
windows(20,14);par(mfrow=c(3,3),mar=c(2,2,2,2))
for(y in seq(1975,2015,5)) {
  a <- dat[floor(dat$yrqtr/5)*5==y & dat$lon5 < 125 & dat$lat5 < 25,]
  a <- tapply(a$hbf,list(a$lon,a$lat),mean,na.rm=T)
  image(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,main=y,zlim=c(6,24),col=heat.colors(30),xlab="Lon",ylab="Lat",ylim=c(-45,25))
  contour(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,add=T,levels=seq(0,26,2))
  map("world",add=T, interior=F,fill=T) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
}
savePlot("mean_HBF_1.png",type="png")

# Map the average HBF by 5 yr period & qtr
qqs <- c(0.125,0.375,0.625,0.875)
for(qq in 1:4) {
  windows(20,14);par(mfrow=c(3,3),mar=c(2,2,2,2),oma=c(0,0,1,0))
  for(y in seq(1975,2015,5)) {
    a <- dat[dat$yrqtr %in% (qqs[qq]+y:(y+4)) & dat$lon5 < 125 & dat$lat5 < 25,]
    a <- tapply(a$hbf,list(a$lon5,a$lat5),mean,na.rm=T)
    image(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,main=y,zlim=c(6,24),col=heat.colors(30),xlab="Lon",ylab="Lat",xlim=c(20,120),ylim=c(-45,25))
    contour(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,add=T,levels=seq(0,26,2))
    map("world",add=T, interior=F,fill=T) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
  }
  title(paste("Quarter",qq),outer=T,line=0)
  savePlot(paste0("mean_HBF5_q",qq,".png"),type="png")
}
qqs <- c(0.125,0.375,0.625,0.875)
for(qq in 1:4) {
  windows(20,14);par(mfrow=c(3,3),mar=c(2,2,2,2),oma=c(0,0,1,0))
  for(y in seq(1975,2015,5)) {
    a <- dat[dat$yrqtr %in% (qqs[qq]+y:(y+4)) & dat$lon5 < 125 & dat$lat5 < 25,]
    a <- tapply(a$hbf,list(a$lon,a$lat),mean,na.rm=T)
    image(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,main=y,zlim=c(6,24),col=heat.colors(30),xlab="Lon",ylab="Lat",xlim=c(20,120),ylim=c(-45,25))
    contour(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,add=T,levels=seq(0,26,2))
    map("world",add=T, interior=F,fill=T) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
  }
  title(paste("Quarter",qq),outer=T,line=0)
  savePlot(paste0("mean_HBF1_q",qq,".png"),type="png")
}

write.csv(table(dat$lat5,dat$lon5),file="ops by lat-long.csv")
write.csv(table(dat$lat5,dat$lon5,5*floor(dat$yrqtr/5)),file="ops by lat-long-5yr.csv")

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
#a$othcpue <- a$oth/a$hooks
a$mlscpue <- a$mls/a$hooks
a$blmcpue <- a$blm/a$hooks
a$bumcpue <- a$bum/a$hooks
#simplemod <- rpart(a$betcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$albcpue + a$othcpue + a$mlscpue + a$blmcpue + a$bumcpue)
simplemod <- rpart(a$albcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$betcpue + a$yftcpue + a$mlscpue + a$blmcpue + a$bumcpue)
windows(width=11,height=7)
plot(simplemod)
text(simplemod)
savePlot("Rpart alb cpue",type="png")
simplemod <- rpart(a$yftcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$albcpue + a$betcpue + a$mlscpue + a$blmcpue + a$bumcpue)
windows(width=11,height=7)
plot(simplemod)
text(simplemod)
savePlot("Rpart yft cpue",type="png")

# Exploratory random forests. Take too long and too much memory. Better if data were subsampled first.
library(randomForest)
simplefor <- randomForest(a$albcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$betcpue + a$yftcpue + a$mlscpue + a$blmcpue + a$bumcpue)
print(simplefor)
windows(width=11,height=7)
plot(importance)
text(varImpPlot,main=NULL)
savePlot("Rforest alb cpue",type="png")
simplefor <- randomForest(a$yftcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$albcpue + a$betcpue + a$mlscpue + a$blmcpue + a$bumcpue)
print(simplefor)
windows(width=11,height=7)
plot(importance)
text(varImpPlot,main=NULL)
savePlot("Rforest yft cpue",type="png")



# ===================================================================================
# Start the analysis proper
# ===================================================================================
#Clustering

library("date")
library("lubridate")
library("maps")
library("mapdata")
library("lunar")
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
jpdir <- paste0(projdir, "JP/")
datadir1 <- paste0(jpdir, "data/")
jalysis_dir <- paste0(jpdir, "analyses/")
Rdir <- paste0(projdir, "Rfiles/")
clusdir <- paste0(jpdir, "clustering/")
setwd(clusdir)
load(file=paste0(jalysis_dir,"JPdat.RData"))
str(dat)

rm(dat2,prepdat,prepdat1,pd1,pd2,clndat,dat5214,rawdat,dataset,llv,dat9415b,dat9415hd,a5,lnk,a2,a0,a)
gc()

# Set up input variables for clustering and standardization
dat <- data.frame(dat)
jp_allsp <-  c("alb","bet","yft","swo","mls","bum","blm","sbt","sas","shk")

# Plot the mean catch per year of each species by region, to use when deciding which species to cluster
# plot_spfreqyq(indat = dat, reg_struc = "regY", splist = jp_allsp, flag = "JP", mfr = c(4,3))
# plot_spfreqyq(indat = dat, reg_struc = "regY2", splist = jp_allsp, flag = "JP", mfr = c(4,3))
plot_spfreqyq(indat = dat, reg_struc = "regA4", splist = jp_allsp, flag = "JP", mfr = c(4,3))
plot_spfreqyq(indat = dat, reg_struc = "regA5", splist = jp_allsp, flag = "JP", mfr = c(4,3))
graphics.off()

# Put chosen species here
use_splist <- c("alb","bet","yft","swo","mls","bum","blm","sbt")
# Variables to use
allabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tripid","tripidmon","lbid_mon","moon",use_splist,"Total","dmy","lat","lon","lat5","lon5","regA","regA1","regA2","regA3","regA4","regA5")
str(dat[,allabs])

# Determine the number of clusters. Come back and edit this.
reglist <- list()
reglist$regA4 <- list(allreg = 1:4, ncl = c(4,5,4,4))
reglist$regA5 <- list(allreg = 1,   ncl = 4)
reglist$regB2 <- list(allreg = 1:4, ncl = c(5,5,4,4))
reglist$regB3 <- list(allreg = 1:5, ncl = c(5,5,4,4,5))
reglist$regY <-  list(allreg = 6, ncl = c(4,4,4,4,4,4))
reglist$regY2 <- list(allreg = c(2,7), ncl = c(4,5,5,5,5,5,4))

flag="JP"

# Covariates to pass to next stage
cvn <- c("yrqtr","latlong","hooks","hbf","vessid","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")
r=4

# Do the clustering and save the results for later (we also need to decide on the ALB regional structures below)
# run_clustercode_byreg(indat=dat, reg_struc = "regY", allsp=use_splist, allabs=allabs,clustid="lbid_mon", flag=flag, cvnames = cvn, rgl = reglist)
# run_clustercode_byreg(indat=dat, reg_struc = "regY2", allsp=use_splist, allabs=allabs,clustid="lbid_mon", flag=flag, cvnames = cvn, rgl = reglist)
run_clustercode_byreg(indat=dat, reg_struc = "regA4", allsp=use_splist, allabs=allabs,clustid="lbid_mon", flag=flag, cvnames = cvn, rgl = reglist)
run_clustercode_byreg(indat=dat, reg_struc = "regA5", allsp=use_splist, allabs=allabs,clustid="lbid_mon", flag=flag, cvnames = cvn, rgl = reglist)

# ========================================================
# Standardizations, Japan only
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
jpdir <- paste0(projdir, "JP/")
datadir1 <- paste0(jpdir, "data/")
jalysis_dir <- paste0(jpdir, "analyses/")
Rdir <- paste0(projdir, "Rfiles/")

# Define the clusters to be used. Will need to set this up after checking the cluster allocations
clkeepJP_A4 <- list("alb"=list(c(1,2,3,4), c(1,2,3,4,5), c(1,2,3,4), c(1,2,3,4)))
clk_A4 <- list(JP=clkeepJP_A4)

clkeepJP_A5 <- list("alb"=list(c(2,3,4)))
clk_A5 <- list(JP=clkeepJP_A5)

clkeepJP_Y <- list("yft"=list(c(1,2,3,4), c(1,2,3,4), c(1,2,4), c(1,2,3), c(1,2,3,4),c(1,2,3,4)))
clk_Y <- list(JP=clkeepJP_Y)

clkeepJP_Y2 <- list("yft"=list(c(0),c(1,2,3,4,5),c(1,2,3),c(1,2,3,4),c(1,2,3,4),c(0),c(1,2,3,4)))
clk_Y2 <- list(JP=clkeepJP_Y2)

minqtrs_Y  <- c(1,8,2,2,5,1)
minqtrs_Y2  <- c(1,7,2,2,5,1,7)
minqtrs_B2 <- c(8,8,2,2)
minqtrs_B3 <- c(7,8,2,2,7)

use_splist <- c("alb","bet","yft")
stdlabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","moon",use_splist,"Total","lat","lon","lat5","lon5","hcltrp","reg","flag")

# clkeepJP_A2 <- list("alb"=list(c(2,4),c(3),c(3,4),c(1,3)))
# clkeepKR_A2 <- list("alb"=list(c(3,4),c(3),c(3,4),c(4)))
# clkeepTW_A2 <- list("alb"=list(c(1),c(1),c(1,2),c(1,4)))
#
# clkeepJP_A5 <- list("alb"=list(c(2,4)))
# clkeepKR_A5 <- list("alb"=list(c(5)))
# clkeepTW_A5 <- list("alb"=list(c(1,2,4)))

## ---------------------------------------------
# Run various standardization scenarios. Only one here now,
# but there are some new ones from the Madrid meeting which I will set up later.
# I can't test the code without data, so apologies if some of the changes in Madrid have broken this code.
# We can fix it in Keelung.
## ---------------------------------------------

# With clusters, and hbf

resdir <- paste0(jalysis_dir,"std_cl_JPonly_hbf/")
dir.create(resdir)
setwd(resdir)

# The runpars define the approach to be used in this run
regA4_minss <- list(minq_byreg = c(3,2,5,5), minvess=c(60,40,100,100), minll=c(30,20,50,50), minyrqtr = c(30,20,50,50), minyqll = c(3,3,5,5))
regA5_minss <- list(minq_byreg = c(5), minvess=c(100), minll=c(50), minyrqtr = c(50), minyqll = c(5))
regB3_minss <- list(minq_byreg = c(5,5,5,3,5), minvess=c(100,100,100,60,100), minll=c(50,50,50,30,50), minyrqtr = c(50,50,50,30,50), minyqll = c(5,5,5,3,5))
regY2_minss <- list(minq_byreg = c(2,5,5,2,5,2,5), minvess=c(40,100,100,40,100,40,100), minll=c(20,50,50,20,50,20,50), minyrqtr = c(20,50,50,20,50,20,50), minyqll = c(3,5,5,3,5,3,5))

runpars <- list()
runpars[["regA4"]] <- list(runsp = "alb", regtype2 = "A4", clk = clk_A4, doregs = 1:4, addcl = TRUE, dohbf = FALSE, dohook = TRUE, cltype = "hcltrp", minss = regA4_minss)
runpars[["regB3"]] <- list(runsp = "bet", regtype2 = "B3", clk = clk_B3, doregs = 1:5, addcl = TRUE, dohbf = FALSE, dohook = TRUE, cltype = "hcltrp", minss = regB3_minss)
runpars[["regY2"]] <- list(runsp = "yft", regtype2 = "Y2", clk = clk_Y2, doregs = c(2:5,7), addcl = TRUE, dohbf = FALSE, dohook = TRUE, cltype = "hcltrp", minss = regY2_minss)
runpars[["regA5"]] <- list(runsp = "alb", regtype2 = "A5", clk = clk_A5, doregs = 1,   addcl = TRUE, dohbf = TRUE, dohook = TRUE, cltype = "hcltrp", minss = regA5_minss)

regstr <- "regY2"; runreg <- 7; keepd <- TRUE; doflags <- "JP" # Values used for testing
maxyr <- 2018
for (regstr in c("regY2")) {
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
  jdat <- jdat[jdat$yrqtr > 2005 | jdat$flag != "TW",]

  vars <- c("vessid","hooks","yrqtr","latlong","hbf")
  for (runreg in rp$doregs) {
    glmdat <-     select_data_IO2(jdat,runreg = runreg, runpars = rp, mt = "deltabin",vars = vars)
    if(nrow(glmdat)>60000) glmdat <- samp_strat_data(glmdat,60)
    glmdat5279 <- select_data_IO2(jdat,runreg = runreg, runpars = rp, mt = "deltabin",vars = vars, yrlims=c(1952,1980))
    if(nrow(glmdat5279)>60000) glmdat5279 <- samp_strat_data(glmdat5279,60)
    a <- jdat[jdat$vessid != "JP1",]
    glmdat79nd <- select_data_IO2(jdat,runreg = runreg, runpars = rp, mt = "deltabin",vars = vars, yrlims=c(1979,maxyr))
    if(nrow(glmdat79nd)>60000) glmdat79nd <- samp_strat_data(glmdat79nd,60)
    wtt.all   <- mk_wts(glmdat,wttype="area")
    wtt.5279   <- mk_wts(glmdat5279,wttype="area")
    wtt.79nd   <- mk_wts(glmdat79nd,wttype="area")
    fmla.oplogn <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=F,addcl=T,nhbf=3, dohook = rp$dohook)
    fmla.oplogn_ncl <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=F,addcl=F,nhbf=3, dohook = rp$dohook)
    fmla.boatlogn <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=T,addcl=T,nhbf=3, dohook = rp$dohook)
    fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=T,addcl=F,nhbf=3, dohook = rp$dohook)
    mn <- with(glmdat,0.1* mean(get(runsp)/hooks))

    modlab="lognC_novess_allyrs"; fname <- paste0("Joint_",regstr,"_R",runreg)
    if(lu(glmdat$clust) > 1)
    { model <- glm(fmla.oplogn,data=glmdat,weights=wtt.all,family="gaussian");gc() } else
    { model <- glm(fmla.oplogn_ncl,data=glmdat,weights=wtt.all,family="gaussian");gc() }
    summarize_and_store(mod=model,dat=glmdat,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_boat_allyrs"; fname <- paste0("Joint_",regstr,"_R",runreg)
    if(lu(glmdat$clust) > 1)
    { model <- glm(fmla.boatlogn,data=glmdat,weights=wtt.all,family="gaussian");gc() } else
    { model <- glm(fmla.boatlogn_ncl,data=glmdat,weights=wtt.all,family="gaussian");gc() }
    summarize_and_store(mod=model,dat=glmdat,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_novess_5279"; fname <- paste0("Joint_",regstr,"_R",runreg)
    mn <- with(glmdat5279,0.1* mean(get(runsp)/hooks))
    if(lu(glmdat5279$clust) > 1)
    { model <- glm(fmla.oplogn,data=glmdat5279,weights=wtt.5279,family="gaussian");gc() } else
    { model <- glm(fmla.oplogn_ncl,data=glmdat5279,weights=wtt.5279,family="gaussian");gc() }
    summarize_and_store(mod=model,dat=glmdat5279,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_vessid_79nd"; fname <- paste0("Joint_",regstr,"_R",runreg)
    mn <- with(glmdat79nd,0.1* mean(get(runsp)/hooks))
    if(lu(glmdat79nd$clust) > 1)
    { model <- glm(fmla.boatlogn,    data=glmdat79nd,weights=wtt.79nd,family="gaussian");gc() } else
    { model <- glm(fmla.boatlogn_ncl,data=glmdat79nd,weights=wtt.79nd,family="gaussian");gc() }
    summarize_and_store(mod=model,dat=glmdat79nd,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    # delta lognormal
    modlab="dellog_novess_allyrs"; fname <- paste0("Joint_",regstr,"_R",runreg);
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd, dohook = rp$dohook)

    modlab="dellog_boat_allyrs"; fname <- paste0("Joint_",regstr,"_R",runreg)
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd, dohook = rp$dohook)

    modlab="dellog_novess_5279"; fname <- paste0("Joint_",regstr,"_R",runreg)
    do_deltalog(dat=glmdat5279,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd, dohook = rp$dohook)

    modlab="dellog_vessid_79nd"; fname <- paste0("Joint_",regstr,"_R",runreg)
    do_deltalog(dat=glmdat79nd,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd, dohook = rp$dohook)

    graphics.off()
  }
}


## ======================================================
# Ignore the code below for now. We may revisit it if we have time
# Test time-area
## ======================================================

###------------------------
for (regstr in c("regY2")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  jdat <- data.frame()
  for(flag in c("JP")) {
    for(r in runpars[[runsp]]$doregs) {
      load(paste0(projdir,flag,"/clustering/",paste(flag,regtype,r,sep="_"),".RData"))
      dataset$flag <- flag
      dataset$qtr <- revtrunc(defactor(dataset$yrqtr))
      jdat <- rbind(jdat,dataset[,allabs])
      rm(dataset)
    }
  }
  jdat <- jdat[jdat$yrqtr < 2018,]
  jdat$vessidx <- jdat$vessid
  jdat$vessid <- paste0(jdat$flag,jdat$vessid)
  jdat$vessid <- as.factor(jdat$vessid)
  jdat$lat5 <- as.factor(jdat$lat5)
  jdat$op_yr <- as.factor(jdat$op_yr)
  jdat$qtr <- as.factor(jdat$qtr)
  jdat <- jdat[jdat$yrqtr > 2005 | jdat$flag != "TW",]

  for(runreg in runpars[[runsp]]$doregs) {
    minqtrs <- minqtrs_byreg[runreg]
    glmdat <- select_data_JointIO(jdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,maxqtrs=maxqtrs, minvess=90,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA)
    if(nrow(glmdat)>60000) glmdat <- samp_strat_data(glmdat,30)
    glmdat5279 <- select_data_JointIO(jdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,maxqtrs=maxqtrs, minvess=90,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1952,1980))
    if(nrow(glmdat5279)>60000) glmdat5279 <- samp_strat_data(glmdat5279,30)
    a <- jdat[jdat$vessid != "JP1",]
    glmdat79nd <- select_data_JointIO(a,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,maxqtrs=maxqtrs,
                                      minvess=90,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1979,maxyr))
    if(nrow(glmdat79nd)>60000) glmdat79nd <- samp_strat_data(glmdat79nd,30)

  #  t1 <- Sys.time()
    # lognormal constant
    modlab="lognC_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_lognCx(dat=glmdat, dohbf, addboat=F, addcl, nhbf=3,runsp, fname, modlab, keepd = keepd, lat5xqtr = T, lat5xyr = T, bcorr=F, cell_areas=cell_areas)

  #  t2 <- Sys.time()
    # modlab="lognC_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    # do_lognCx(dat=glmdat, dohbf, addboat=T, addcl, nhbf=3,runsp, fname, modlab, keepd = keepd, lat5xqtr = T, lat5xyr = T, bcorr=F, cell_areas=cell_areas)

  #  t3 <- Sys.time()
    modlab="lognC_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_lognCx(dat=glmdat5279, dohbf, addboat=F, addcl, nhbf=3,runsp, fname, modlab, keepd = keepd, lat5xqtr = T, lat5xyr = T, bcorr=F, cell_areas=cell_areas)

#    t4 <- Sys.time()
    modlab="lognC_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_lognCx(dat=glmdat79nd, dohbf, addboat=F, addcl, nhbf=3,runsp, fname, modlab, keepd = keepd, lat5xqtr = T, lat5xyr = T, bcorr=F, cell_areas=cell_areas)

#    t5 <- Sys.time()
    # delta lognormal
    modlab="dellog_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg);
    do_deltalogx(dat=glmdat,dohbf, addboat=F,addcl, nhbf=3,runsp, fname, modlab, keepd = keepd, lat5xqtr = T, lat5xyr = T, bcorr=F, cell_areas=cell_areas)

    # modlab="dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    # do_deltalogx(dat=glmdat,dohbf, addboat=T,addcl, nhbf=3,runsp, fname, modlab, keepd = keepd, lat5xqtr = T, lat5xyr = T, bcorr=F, cell_areas=cell_areas)

    modlab="dellog_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalogx(dat=glmdat5279,dohbf, addboat=F,addcl, nhbf=3,runsp, fname, modlab, keepd = keepd, lat5xqtr = T, lat5xyr = T, bcorr=F, cell_areas=cell_areas)

    modlab="dellog_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalogx(dat=glmdat79nd,dohbf, addboat=F,addcl, nhbf=3,runsp, fname, modlab, keepd = keepd, lat5xqtr = T, lat5xyr = T, bcorr=F, cell_areas=cell_areas)

    graphics.off()
  }
}

