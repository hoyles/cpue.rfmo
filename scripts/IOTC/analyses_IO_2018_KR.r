# Set up directories
projdir <- "~/IOTC/2017_CPUE/"
krdir <- paste0(projdir, "KR/")
datadir <- paste0(krdir, "data/")
kralysis_dir <- paste0(krdir, "analyses/")
krfigs <- paste0(krdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
setwd(kralysis_dir)

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
library(readr)
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
rawdat <- read.table(paste0(datadir,"IO KOR LL OP data_20170629.txt"), header = TRUE, sep="\t", stringsAsFactors = FALSE)
str(rawdat)

# Set up standard names, the same as for other fleets
nms <- c("op_yr","op_mon","VESSEL_CD","VESSEL_NAME","DATE","Lat01","NS","Long01","EW","hooks","floats",
       "alb","bet","blm","bum","mls","oth","sbt","sfa","sha","skj","swo","yft","Total")
cbind(names(rawdat),nms)
names(rawdat) <- nms
head(rawdat)
str(rawdat)
#storedat <- rawdat
#rawdat <- storedat

# Prepare and check the data
prepdat <- dataprep_KR(rawdat)
head(prepdat)
prepdat2 <- setup_IO_regions(prepdat,  regY=T, regY1=T, regY2=T, regB=T, regB3=T, regB2=T, regA=T, regA1=T, regA2=T, regA3=T, regA4=T, regA5=T)
head(prepdat2)
dat <- dataclean_KR(prepdat2, yearlim = 2017)
save(prepdat,dat,file="KRdat.RData")

load(file="KRdat.RData")
str(dat)
summary(dat)
table(dat$vessid,dat$op_yr)
table(dat$op_yr,dat$vessid)
table(dat$op_yr,is.na(dat$vessid))
table(dat$op_yr,(dat$hooks > 0))

# ===================================================================================
# Plot and explore the data
# ===================================================================================
# Plot time distribution of sets
windows(width=15,height=9)
hist(prepdat$dmy,breaks="days",freq=T,xlab="Date",main="Sets per day")
savePlot(file="sets_per_day.png",type="png")
table(prepdat$dmy)


# Check a few specific instances where there have been problems in the past. Consider checking for similar problems in new data.
with(dat[dat$op_yr %in% c(1984:1985),] ,hist(dmy,breaks="days",freq=T,xlab="Date",main="Sets per day"))
with(dat[dat$dmy > as.Date("1982-1-1") & dat$dmy < as.Date("1985-2-1"),] ,hist(dmy,breaks="days",freq=T,xlab="Date",main="Sets per day"))
with(dat[dat$dmy > as.Date("1984-1-1") & dat$dmy < as.Date("1985-2-1"),] ,table(dmy))
a <- dat[dat$dmy >= as.Date("1984-02-29") & dat$dmy < as.Date("1984-03-13") & !is.na(dat$dmy),]
a[order(a$dmy),]
a <- dat[grep("ZA240",dat$VESSEL_CD),]
plot(a$lon,a$lat,type="b")
text(jitter(a$lon),jitter(a$lat),a$dmy)
a <- dat[grep("ZA000",dat$VESSEL_CD),]
a <- a[a$op_yr==1984,]
plot(a$lon,a$lat,type="b")
text(jitter(a$lon),jitter(a$lat),a$dmy)
na <- length(a$lat)
dista <- ((a[2:na,"lat"]-a[1:(na-1),"lat"])^2 + (a[2:na,"lon"]-a[1:(na-1),"lon"])^2)^0.5
timea <- (a[2:na,"dmy"]-a[1:(na-1),"dmy"])
kperday <- 111 * dista/as.numeric(timea)
cbind(dista,timea,kperday)

hist(prepdat$hooks, nclass=200)   # ask if very large # hooks is okay
savePlot("Hook histogram.png",type="png")
#hist(prepdat$hooks, nclass=200,xlim=c(0,1200),ylim=c(0,500))
hist(prepdat$floats, nclass=200)   # ask if the sets with 1200 floats are reasonable
savePlot("floats histogram.png",type="png")

# Look for outliers. Individual sets with high catch are not a problem.
table(prepdat$alb)   # ask if the set with 488 alb
table(prepdat$bet)   # ask if the set with 334 bet
table(prepdat$blm)   # ask if the set with 58 blm
table(prepdat$bum)   # ask if the set with 122
table(prepdat$skj)   # ask if the set with 107
table(prepdat$pbf)   # ask if the sets with any pbf
table(prepdat$sha)   # ask if the majority of sets with 0 sha
table(prepdat$mls)   # ask if the set with 371
table(prepdat$sbt)   # ask if the set with 164
table(prepdat$yft)   # ask if the set with 917
table(prepdat$sfa)   # ask if the sets with 330
table(prepdat$floats,useNA="always")  # 6408 with NA! All in 1973-75
table(is.na(prepdat$floats),prepdat$op_yr,useNA="always")  # 6408 with NA!
table(round(prepdat$hbf,0),prepdat$op_yr,useNA="always")  # Looks like 1971-76 have few sets with full data and may not be usable
a <- table(prepdat$op_yr,round(prepdat$hbf,0),useNA="always")
write.csv(a,"table hbf by year.csv")

# Map the effort
a <- aggregate(dat$hooks,list(dat$lat5,dat$lon5),sum,na.rm=T)
windows(width=11,height=9)
symbols(x=a[,2],y=a[,1],circles=.0002*sqrt(a[,3]),inches=F,bg=2,fg=2,xlab="Longitude",ylab="Latitude")
map(add=T,interior=F,fill=T)
savePlot(file="map_hooks.png",type="png")

table(dat$EW) # Some data with 2
a <- log(table(dat$lon,dat$lat))
windows(width=15,height=10)
image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a)
map("world2Hires",add=T) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
# But also some data near Indonesia, Cambodia, & on land in Africa
savePlot("Setmap_logscale.png",type="png")

a <- tapply(dat$regB,list(dat$lon,dat$lat),mean)
windows(width=15,height=10)
image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,col=2:6)
map("world2Hires",add=T) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
savePlot("regbet.png",type="png")

a <- tapply(dat$regB2,list(dat$lon,dat$lat),mean)
windows(width=15,height=10)
image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,col=1:6)
map("world2Hires",add=T) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
savePlot("regbet2.png",type="png")

a <- tapply(dat$regB3,list(dat$lon,dat$lat),mean)
windows(width=15,height=10)
image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,col=1:6)
map("world2Hires",add=T) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
savePlot("regbet3.png",type="png")

a <- tapply(dat$regY2,list(dat$lon,dat$lat),mean)
windows(width=15,height=10)
image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,col=1:7)
map("world2Hires",add=T) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
savePlot("regyft2.png",type="png")

a <- tapply(dat$regY,list(dat$lon,dat$lat),mean)
windows(width=15,height=10)
image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,col=1:6)
map("world2Hires",add=T) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
savePlot("regyft.png",type="png")

# a <- tapply(dat$regA3,list(dat$lon,dat$lat),mean)
# windows(width=15,height=10)
# image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,col=1:6)
# map("world2Hires",add=T) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
# savePlot("regA3.png",type="png")

# Mean fishing location, year scale
windows(width=15,height=10);par(mfrow=c(1,2))
plot(tapply(dat$op_yr,dat$op_yr,mean),tapply(dat$lat,dat$op_yr,mean),xlab="yr",ylab="Mean latitude")
plot(tapply(dat$lon,dat$op_yr,mean),tapply(dat$op_yr,dat$op_yr,mean),ylab="yr",xlab="Mean longitude")
savePlot("mean_fishing_location 2.png",type="png")
# Mean fishing location, yrqtr scale
plot(tapply(dat$yrqtr,dat$yrqtr,mean),tapply(dat$lat,dat$yrqtr,mean),xlab="yr",ylab="Mean latitude")
plot(tapply(dat$lon,dat$yrqtr,mean),tapply(dat$yrqtr,dat$yrqtr,mean),ylab="yr",xlab="Mean longitude")
savePlot("mean_fishing_location 1.png",type="png")

# Store summaries of hbf by region and through time
write.csv(table(round(dat$hbf,0),dat$regY,useNA="always"),file="hbf by region.csv")
write.csv(table(round(dat$hbf,0),floor(dat$yrqtr/5)*5,dat$regY,useNA="always"),file="hbf by region by 5 years.csv")

# Map mean hbf by 5yr period
windows(20,20);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(y in seq(1975,2015,5)) {
  a <- dat[floor(dat$yrqtr/5)*5==y & dat$lon < 125 & dat$lat < 25,]
  a <- tapply(a$hbf,list(a$lon,a$lat),mean,na.rm=T)
  image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,main=y,zlim=c(6,24),col=heat.colors(30),xlab="Lon",ylab="Lat",ylim=c(-45,25))
  contour(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,add=T,levels=seq(0,26,2))
  map("world2",add=TRUE, fill = TRUE) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
}
savePlot("mean_HBF.png",type="png")
windows(20,20);par(mfrow=c(2,2))
for(y in c(1975,1985,1995,2005)) {
  a <- dat[dat$yrqtr>y & dat$yrqtr < y+10  & dat$lon < 125,]
  a <- tapply(a$hbf,list(a$lon,a$lat),mean)
  image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,main=y,zlim=c(6,24),col=heat.colors(30),xlab="Lon",ylab="Lat",xlim=c(20,125),ylim=c(-45,25))
  contour(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,add=T,levels=seq(0,26,1),col="dark blue")
  map("world2",add=T, fill = TRUE) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
}
savePlot("mean_HBF_10yr.png",type="png")

write.csv(table(dat$lat5,dat$lon5),file="ops by lat-long.csv")
write.csv(table(dat$lat5,dat$lon5,5*floor(dat$yrqtr/5)),file="ops by lat-long-5yr.csv")

# Exploratory regression trees. These are not particularly useful.
library(rpart)
a <- dat[dat$regY%in% c(2,5),]
dim(a)
a$betcpue <- a$bet/a$hooks
a$albcpue <- a$alb/a$hooks
a$yftcpue <- a$yft/a$hooks
a$sbfcpue <- a$sbt/a$hooks
a$swocpue <- a$swo/a$hooks
a$sfacpue <- a$sfa/a$hooks
a$mlscpue <- a$mls/a$hooks
a$blmcpue <- a$blm/a$hooks
a$bumcpue <- a$bum/a$hooks
simplemod <- rpart(a$betcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$albcpue + a$sfacpue + a$mlscpue + a$blmcpue + a$bumcpue)
windows(width=11,height=7)
plot(simplemod)
text(simplemod)

# Species composition maps. These are very useful. Should be moved to the figures script.
a5 <- aggregate(cbind(bet,yft,alb,swo,blm,bum,sbt,sfa,mls,sha,oth,Total,Total2,hooks) ~ lon5 + lat5 + eval(5*floor((op_yr)/5)),data=dat,FUN=sum)
a <- aggregate(cbind(bet,yft,alb,swo,blm,bum,sbt,sfa,mls,sha,oth,Total,Total2,hooks) ~ lon + lat + eval(5*floor((op_yr)/5)),data=dat,FUN=sum)
names(a)[3] <- names(a5)[3] <- "decade"
names(a5)[1:2] <- c("lon","lat")

windows(width=20,height=20);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015,5)) plot_catchmap(indat=a5,vbl=a5$bet/(a5$bet+a5$yft),dcd=d,latlim=c(-18,10),lonlim=c(40,120),ti="BET / BET + YFT")
savePlot("PropBET in YFT_BET5",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$bet/(a5$Total),dcd=d,latlim=c(-45,10),lonlim=c(40,120),ti="BET / Total")
savePlot("PropBET in Total5",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$swo/(a5$Total),dcd=d,latlim=c(-45,10),lonlim=c(20,120),ti="SWO / Total",brk2=seq(0,1,.05))
savePlot("PropSWO in Total5",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$alb/(a5$Total),dcd=d,latlim=c(-45,00),lonlim=c(20,120),ti="ALB / Total",brk2=seq(0,1,.1))
savePlot("PropALB in Total5",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$sfa/(a5$Total),dcd=d,latlim=c(-45,10),lonlim=c(20,120),ti="SFA / Total",brk2=seq(0,1,.05))
savePlot("PropSFA in Total5",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$sha/(a5$Total),dcd=d,latlim=c(-45,10),lonlim=c(20,120),ti="SHA / Total",brk2=seq(0,1,.05))
savePlot("PropSHA in Total5",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$yft/(a5$Total),dcd=d,latlim=c(-45,10),lonlim=c(20,120),ti="YFT / Total")
savePlot("PropYFT in Total5",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$mls/(a5$Total),dcd=d,latlim=c(-45,10),lonlim=c(20,120),ti="MLS / Total",brk2=seq(0,1,.05))
savePlot("PropMLS in Total5",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$sbt/(a5$Total),dcd=d,latlim=c(-45,10),lonlim=c(20,120),ti="sbt / Total",brk2=seq(0,1,.05))
savePlot("PropSBF in Total5",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$blm/(a5$Total),dcd=d,latlim=c(-45,10),lonlim=c(20,120),ti="BLM / Total",brk2=seq(0,1,.05))
savePlot("PropBLM in Total5",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$bum/(a5$Total),dcd=d,latlim=c(-45,10),lonlim=c(20,120),ti="BUM / Total",brk2=seq(0,1,.05))
savePlot("PropBUM in Total5",type="png")

windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$bet/(a$bet+a$yft),dcd=d,latlim=c(-18,10),lonlim=c(40,120),ti="BET / BET + YFT")
savePlot("PropBET in YFT_BET",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$bet/(a$Total),dcd=d,latlim=c(-45,10),lonlim=c(40,120),ti="BET / Total")
savePlot("PropBET in Total",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$swo/(a$Total),dcd=d,latlim=c(-45,10),lonlim=c(20,120),ti="SWO / Total",brk2=seq(0,1,.05))
savePlot("PropSWO in Total",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$alb/(a$Total),dcd=d,latlim=c(-45,00),lonlim=c(20,120),ti="ALB / Total",brk2=seq(0,1,.1))
savePlot("PropALB in Total",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$sfa/(a$Total),dcd=d,latlim=c(-45,10),lonlim=c(20,120),ti="SFA / Total",brk2=seq(0,1,.05))
savePlot("PropSFA in Total",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$sha/(a$Total),dcd=d,latlim=c(-45,10),lonlim=c(20,120),ti="SHA / Total",brk2=seq(0,1,.05))
savePlot("PropSHA in Total",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$yft/(a$Total),dcd=d,latlim=c(-45,10),lonlim=c(20,120),ti="YFT / Total")
savePlot("PropYFT in Total",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$mls/(a$Total),dcd=d,latlim=c(-45,10),lonlim=c(20,120),ti="MLS / Total",brk2=seq(0,1,.05))
savePlot("PropMLS in Total",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$sbt/(a$Total),dcd=d,latlim=c(-45,10),lonlim=c(20,120),ti="sbt / Total",brk2=seq(0,1,.05))
savePlot("PropSBF in Total",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$blm/(a$Total),dcd=d,latlim=c(-45,10),lonlim=c(20,120),ti="BLM / Total",brk2=seq(0,1,.05))
savePlot("PropBLM in Total",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$bum/(a$Total),dcd=d,latlim=c(-45,10),lonlim=c(20,120),ti="BUM / Total",brk2=seq(0,1,.05))
savePlot("PropBUM in Total",type="png")

########### relative to YBA
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2010,10)) plot_catchmap(indat=a,vbl=a$bet/(a$Total2),dcd=d,latlim=c(-45,20),lonlim=c(40,120),ti="BET / YBA Total")
savePlot("PropBET in altTotal",type="png")
for(d in seq(1975,2010,10)) plot_catchmap(indat=a,vbl=a$alb/(a$Total2),dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="ALB / YBA Total",brk2=seq(0,1,.1))
savePlot("PropALB in altTotal",type="png")
for(d in seq(1975,2010,10)) plot_catchmap(indat=a,vbl=a$yft/(a$Total2),dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="YFT / YBA Total")
savePlot("PropYFT in altTotal",type="png")

windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$bet/(a5$Total2),dcd=d,latlim=c(-45,20),lonlim=c(40,120),ti="BET / YBA Total")
savePlot("PropBET in altTotal5",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$alb/(a5$Total2),dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="ALB / YBA Total",brk2=seq(0,1,.1))
savePlot("PropALB in altTotal5",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$yft/(a5$Total2),dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="YFT / YBA Total")
savePlot("PropYFT in altTotal5",type="png")

#Catch maps
a5 <- aggregate(cbind(bet,yft,alb,swo,blm,bum,sbt,sfa,mls,sha,skj,oth,Total,Total2,hooks) ~ lon5 + lat5 + eval(5*floor((op_yr)/5)),data=dat,FUN=sum)
a <- aggregate(cbind(bet,yft,alb,swo,blm,bum,sbt,sfa,mls,sha,skj,oth,Total,Total2,hooks) ~ lon + lat + eval(5*floor((op_yr)/5)),data=dat,FUN=sum)
names(a)[3] <- names(a5)[3] <- "decade"
names(a5)[1:2] <- c("lon","lat")

windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$bet,dcd=d,latlim=c(-45,20),lonlim=c(40,120),ti="BET Catch")
savePlot("Catchmap_BET",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$swo,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SWO Catch")
savePlot("Catchmap_SWO",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$alb,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="ALB Catch")
savePlot("Catchmap_ALB",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$sfa,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SFA Catch")
savePlot("Catchmap_sfa",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$sha,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SHA Catch")
savePlot("Catchmap_SHA",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$yft,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="YFT Catch")
savePlot("Catchmap_YFT",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$mls,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="MLS Catch")
savePlot("Catchmap_MLS",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$sbt,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SBT Catch")
savePlot("Catchmap_SBT",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$blm,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="BLM Catch")
savePlot("Catchmap_BLM",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$bum,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="BUM Catch")
savePlot("Catchmap_BUM",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$oth,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="OTH Catch")
savePlot("Catchmap_OTH",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$skj,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SKJ Catch")
savePlot("Catchmap_SKJ",type="png")

windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$bet,dcd=d,latlim=c(-45,20),lonlim=c(40,120),ti="BET Catch")
savePlot("Catchmap5_BET",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$swo,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SWO Catch")
savePlot("Catchmap5_SWO",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$alb,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="ALB Catch")
savePlot("Catchmap5_ALB",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$sfa,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SFA Catch")
savePlot("Catchmap5_sfa",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$sha,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SHA Catch")
savePlot("Catchmap5_SHA",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$yft,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="YFT Catch")
savePlot("Catchmap5_YFT",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$mls,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="MLS Catch")
savePlot("Catchmap5_MLS",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$sbt,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SBT Catch")
savePlot("Catchmap5_SBT",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$blm,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="BLM Catch")
savePlot("Catchmap5_BLM",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$bum,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="BUM Catch")
savePlot("Catchmap5_BUM",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$oth,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="OTH Catch")
savePlot("Catchmap5_OTH",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$skj,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SKJ Catch")
savePlot("Catchmap5_SKJ",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)

#CPUE maps
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$bet,vb2=a$hooks,dcd=d,latlim=c(-45,20),lonlim=c(40,120),ti="BET CPUE")
savePlot("Cpuemap_BET",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$swo,vb2=a$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SWO CPUE")
savePlot("Cpuemap_SWO",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$alb,vb2=a$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="ALB CPUE")
savePlot("Cpuemap_ALB",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$sfa,vb2=a$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SFA CPUE")
savePlot("Cpuemap_sfa",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$sha,vb2=a$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SHA CPUE")
savePlot("Cpuemap_SHA",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$yft,vb2=a$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="YFT CPUE")
savePlot("Cpuemap_YFT",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$mls,vb2=a$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="MLS CPUE")
savePlot("Cpuemap_MLS",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$sbt,vb2=a$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SBT CPUE")
savePlot("Cpuemap_SBT",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$blm,vb2=a$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="BLM CPUE")
savePlot("Cpuemap_BLM",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$bum,vb2=a$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="BUM CPUE")
savePlot("Cpuemap_BUM",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$oth,vb2=a$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="OTH CPUE")
savePlot("Cpuemap_OTH",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$skj,vb2=a$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SKJ CPUE")
savePlot("Cpuemap_SKJ",type="png")

windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$bet,vb2=a5$hooks,dcd=d,latlim=c(-45,20),lonlim=c(40,120),ti="BET CPUE")
savePlot("CPUEmap5_BET",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$swo,vb2=a5$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SWO CPUE")
savePlot("CPUEmap5_SWO",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$alb,vb2=a5$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="ALB CPUE")
savePlot("CPUEmap5_ALB",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$sfa,vb2=a5$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SFA CPUE")
savePlot("CPUEmap5_sfa",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$sha,vb2=a5$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SHA CPUE")
savePlot("CPUEmap5_SHA",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$yft,vb2=a5$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="YFT CPUE")
savePlot("CPUEmap5_YFT",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$mls,vb2=a5$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="MLS CPUE")
savePlot("CPUEmap5_MLS",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$sbt,vb2=a5$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SBT CPUE")
savePlot("CPUEmap5_SBT",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$blm,vb2=a5$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="BLM CPUE")
savePlot("CPUEmap5_BLM",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$bum,vb2=a5$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="BUM CPUE")
savePlot("CPUEmap5_BUM",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$oth,vb2=a5$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="OTH CPUE")
savePlot("CPUEmap5_OTH",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$skj,vb2=a5$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SKJ CPUE")
savePlot("CPUEmap5_SKJ",type="png")

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

projdir <- "~/IOTC/2017_CPUE/"
krdir <- paste0(projdir, "KR/")
datadir <- paste0(krdir, "data/")
kralysis_dir <- paste0(krdir, "analyses/")
krfigs <- paste0(krdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
clustdir <- paste0(krdir,"clustering/")

setwd(clustdir)
load(file=paste0(kralysis_dir, "KRdat.RData"))

# Set up input variables for clustering and standardization
dat <- data.frame(dat)
kr_splist <-  c("alb","bet","blm","bum","mls","oth","sbt","sfa","sha","skj","swo","yft")

# Plot the mean catch per year of each species by region, to use when deciding which species to cluster
for (r in c(2:7)) {
  windows(15,12); par(mfrow = c(4,3), mar = c(3,2,2,1), oma = c(0,0,2,0))
  a <- dat[dat$regY2 == r,]
  for (sp in kr_splist) plot(sort(unique(a$yrqtr)),tapply(a[,sp], a$yrqtr, mean), main = sp)
  title(paste("Region", r ), outer = TRUE)
  savePlot(filename = paste("freq",flag,"Region", r, sep = "_"), type = "png")
}
for (r in c(2:7)) {
  windows(15,12); par(mfrow = c(4,3), mar = c(3,2,2,1), oma = c(0,0,2,0))
  a <- dat[dat$regB3 == r,]
  for (sp in kr_splist) plot(sort(unique(a$yrqtr)),tapply(a[,sp], a$yrqtr, mean), main = sp)
  title(paste("Region", r ), outer = TRUE)
  savePlot(filename = paste("freq",flag,"Region", r, sep = "_"), type = "png")
}
for (r in c(2:7)) {
  windows(15,12); par(mfrow = c(4,3), mar = c(3,2,2,1), oma = c(0,0,2,0))
  a <- dat[dat$regA4 == r,]
  for (sp in kr_splist) plot(sort(unique(a$yrqtr)),tapply(a[,sp], a$yrqtr, mean), main = sp)
  title(paste("Region", r ), outer = TRUE)
  savePlot(filename = paste("freq",flag,"Region", r, sep = "_"), type = "png")
}

# Put chosen species here
use_splist <- c("alb","bet","yft","swo","mls","bum","blm","sbt","sas")

# Variables to use
allabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tripid","tripidmon","lbid_mon","moon",use_splist,"Total","dmy","lat","lon","lat5","lon5","regY","regY1","regY2","regB","regB1","regB2","regA","regA1","regA2","regA3")
str(dat[,allabs])

# Determine the number of clusters. Come back and edit this.
nclY=c(1,4,4,5,4,1)
nclY2=c(1,4,4,5,4,1,4)
nclB2=c(5,5,4,4)
nclB3=c(5,5,4,4,5)
nclA4=c(5,5,4,4)
nclA5=c(5)
flag="KR"

# Covariates to pass to next stage
cvn <- c("yrqtr","latlong","hooks","hbf","vessid","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")
r=4

# Do the clustering and save the results for later (we also need to decide on the ALB regional structures below)
regtype="regY2"
for(r in c(2:5,7)) {
  fnh <- paste(flag,regtype,r,sep="_")
  dataset <- clust_PCA_run(r=r,ddd=dat,allsp=allsp,allabs=allabs,regtype=regtype,ncl=nclY2[r],plotPCA=F,clustid="lbid_mon",allclust=F,flag=flag,fnhead=fnh,covarnames=cvn)
  save(dataset,file=paste0(fnh,".RData"))
}
regtype="regB3"
for(r in 1:length(nclB3)) {
  fnh <- paste(flag,regtype,r,sep="_")
  dataset <- clust_PCA_run(r=r,ddd=dat,allsp=allsp,allabs=allabs,regtype=regtype,ncl=nclB3[r],plotPCA=F,clustid="lbid_mon",allclust=F,flag=flag,fnhead=fnh,covarnames=cvn)
  save(dataset,file=paste0(fnh,".RData"))
}
regtype="regA4"
for(r in 1:length(nclA3)) {
  fnh <- paste(flag,regtype,r,sep="_")
  dataset <- clust_PCA_run(r=r,ddd=dat,allsp=allsp,allabs=allabs,regtype=regtype,ncl=nclA4[r],plotPCA=F,clustid="lbid_mon",allclust=F,flag=flag,fnhead=fnh,covarnames=cvn)
  save(dataset,file=paste0(fnh,".RData"))
}
regtype="regA5"
for(r in 1:length(nclA5)) {
  fnh <- paste(flag,regtype,r,sep="_")
  dataset <- clust_PCA_run(r=r,ddd=dat,allsp=allsp,allabs=allabs,regtype=regtype,ncl=nclA5[r],plotPCA=F,clustid="lbid_mon",allclust=F,flag=flag,fnhead=fnh,covarnames=cvn)
  save(dataset,file=paste0(fnh,".RData"))
}


# ========================================================
# Standardizations, Korea only
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

projdir <- "~/IOTC/2017_CPUE/"
krdir <- paste0(projdir, "KR/")
datadir <- paste0(krdir, "data/")
kralysis_dir <- paste0(krdir, "analyses/")
krfigs <- paste0(krdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
clustdir <- paste0(krdir,"clustering/")

# Define the clusters to be used. Will need to set this up after checking the cluster allocations
clkeepJP_A5 <- list("alb"=list(c(2,4)))
clkeepKR_A5 <- list("alb"=list(c(5)))
clkeepTW_A5 <- list("alb"=list(c(1,2,4)))

clkeepJP_Y <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(1,2,5),c(1,2,3,4),c(0)))
clkeepKR_Y <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(2,3,5),c(1,2,3,4),c(0)))
clkeepTW_Y <- list("yft"=list(c(0),c(1,2,3,4,5),c(1,2,3),c(1,2),c(1,2,3,4,5)),c(0))
clkeepSY_Y <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(2),c(1,2,3,4),c(0)))
clk_Y <- list(JP=clkeepJP_Y,KR=clkeepKR_Y,TW=clkeepTW_Y,SY=clkeepSY_Y)

clkeepJP_B2 <- list("bet"=list(c(1,2,3,4,5),c(1,2,3,4,5),c(1,2,3,4),c(1,2,3,4)))
clkeepKR_B2 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepTW_B2 <- list("bet"=list(c(1,2,3,4,5),c(1,2,3,4,5),c(2,3),c(1,2,3,4)))
clkeepSY_B2 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2),c(1,2,4)))
clk_B2 <- list(JP=clkeepJP_B2, KR=clkeepKR_B2, TW=clkeepTW_B2, SY=clkeepSY_B2)

clkeepJP_Y2 <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(1,2,5),c(1,2,3,4),c(0),c(1,2,3,4)))
clkeepKR_Y2 <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(2,3,5),c(1,2,3,4),c(0),c(1,2,3,4)))
clkeepTW_Y2 <- list("yft"=list(c(0),c(1,2,3,4,5),c(1,2,3),c(1,2),c(1,2,3,4,5),c(0),c(1,2,3,4,5)))
clkeepSY_Y2 <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(2),c(1,2,3,4),c(0),c(1,2,3,4)))
clk_Y2 <- list(JP=clkeepJP_Y2,KR=clkeepKR_Y2,TW=clkeepTW_Y2,SY=clkeepSY_Y2)

clkeepJP_B3 <- list("bet"=list(c(1,2,3,4,5),c(1,2,3,4,5),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4,5)))
clkeepKR_B3 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepTW_B3 <- list("bet"=list(c(1,2,3,4,5),c(1,2,3,4,5),c(2,3),c(1,2,3,4),c(1,2,3,4,5)))
clkeepSY_B3 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2),c(1,2,4),c(1,2,3,4)))
clk_B3 <- list(JP=clkeepJP_B3,KR=clkeepKR_B3,TW=clkeepTW_B3,SY=clkeepSY_B3)

minqtrs_Y  <- c(1,8,2,2,5,1)
minqtrs_Y2  <- c(1,7,2,2,5,1,7)
minqtrs_B2 <- c(8,8,2,2)
minqtrs_B3 <- c(7,8,2,2,7)

use_splist <- c("alb","bet","yft")
stdlabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","moon",use_splist,"Total","lat","lon","lat5","lon5","hcltrp","reg","flag")

## ---------------------------------------------
# Run various standardization scenarios. Only one here now, and for bigeye instead of YFT and ALB.
# There are some new ones from the Madrid meeting which I will set up later.
# I can't test the code without data, so apologies if some of the changes in Madrid have broken this code.
# We can fix it in Keelung.
## ---------------------------------------------

# With clusters, and hbf
std_dir <- paste0(krdir,"std/")
setwd(std_dir)

# The runpars define the approach to be used in this run
runpars <- list()
runpars[["bet"]] <- list(regtype = "regB2", regtype2 = "B2", clk = clk_B2, doregs = 1:4, addcl = TRUE, dohbf = FALSE, cltype = "hcltrp")
runpars[["yft"]] <- list(regtype = "regY",  regtype2 = "Y",  clk = clk_Y,  doregs = 2:5, addcl = TRUE, dohbf = FALSE, cltype = "hcltrp")

runsp <- "bet"; runreg <- 2
maxyr <- 2018; maxqtrs <- 200; minqtrs_byreg <- c(8,8,2,2,5,5,5,5); keepd <- TRUE
for (runsp in c("bet")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  jdat <- data.frame()
  for (flag in c("KR")) {
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
  jdat <- jdat[jdat$yrqtr > 2005 | jdat$flag != "TW",]

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

