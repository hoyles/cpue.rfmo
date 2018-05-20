projdir <- "~/IOTC/2017_CPUE/"
krdir <- paste0(projdir, "KR/")
datadir <- paste0(krdir, "data/")
kralysis_dir <- paste0(krdir, "analyses/")
krfigs <- paste0(krdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
setwd(kralysis_dir)
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

rawdat <- read.table(paste0(datadir,"IO KOR LL OP data_20170629.txt"), header = TRUE, sep="\t", stringsAsFactors = FALSE)
str(rawdat)
a <- c("op_yr","op_mon","VESSEL_CD","VESSEL_NAME","DATE","Lat01","NS","Long01","EW","hooks","floats",
       "alb","bet","blm","bum","mls","oth","sbt","sfa","sha","skj","swo","yft","Total")
cbind(names(rawdat),a)
names(rawdat) <- a
head(rawdat)
str(rawdat)
#storedat <- rawdat
#rawdat <- storedat
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

windows(width=15,height=9)
hist(prepdat$dmy,breaks="days",freq=T,xlab="Date",main="Sets per day")
savePlot(file="sets_per_day.png",type="png")
table(prepdat$dmy)

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


windows(width=15,height=10);par(mfrow=c(1,2))
plot(tapply(dat$op_yr,dat$op_yr,mean),tapply(dat$lat,dat$op_yr,mean),xlab="yr",ylab="Mean latitude")
plot(tapply(dat$lon,dat$op_yr,mean),tapply(dat$op_yr,dat$op_yr,mean),ylab="yr",xlab="Mean longitude")
savePlot("mean_fishing_location 2.png",type="png")
plot(tapply(dat$yrqtr,dat$yrqtr,mean),tapply(dat$lat,dat$yrqtr,mean),xlab="yr",ylab="Mean latitude")
plot(tapply(dat$lon,dat$yrqtr,mean),tapply(dat$yrqtr,dat$yrqtr,mean),ylab="yr",xlab="Mean longitude")
savePlot("mean_fishing_location 1.png",type="png")

write.csv(table(round(dat$hbf,0),dat$regY,useNA="always"),file="hbf by region.csv")
write.csv(table(round(dat$hbf,0),floor(dat$yrqtr/5)*5,dat$regY,useNA="always"),file="hbf by region by 5 years.csv")

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

#write.csv(table(dat$ncrew,dat$reg),file="crew by region.csv")
#write.csv(table(dat$ncrew,floor(dat$yrqtr/10)*10),file="crew by decade.csv")
#write.csv(table(dat$ncrew,dat$fishingcat,useNA="ifany"),file="crew by fishingcat.csv")
write.csv(table(dat$lat5,dat$lon5),file="ops by lat-long.csv")
write.csv(table(dat$lat5,dat$lon5,5*floor(dat$yrqtr/5)),file="ops by lat-long-5yr.csv")

# data exploration
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

# Species composition maps
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

########################
#Clustering

#library(R4MFCL)
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
library(maps)
#install.packages("date")
library(date)
#library(sdhpkg)
library(mapdata)
library(maptools)
library(mgcv)
#install.packages("NbClust")
library(NbClust)

projdir <- "~/IOTC/2017_CPUE/"
krdir <- paste0(projdir, "KR/")
datadir <- paste0(krdir, "data/")
kralysis_dir <- paste0(krdir, "analyses/")
krfigs <- paste0(krdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
clustdir <- paste0(krdir,"clustering/")

setwd(clustdir)
load(file=paste0(kralysis_dir, "KRdat.RData"))
source(paste0(Rdir, "support_functions.r"))

allsp <- c("alb","bet","blm","bum","mls","oth","sbt","sfa","sha","skj","swo","yft")

allabs <- c("op_yr","op_mon","VESSEL_CD","VESSEL_NAME","DATE","Lat01","NS","Long01","EW","hooks",
            "floats","alb","bet","blm","bum","mls","oth","sbt","sfa","sha","skj",
            "swo","yft","Total","dmy","hbf","moon","lat","lon","lat5","lon5","yrqtr","latlong","vessid","tripidmon",
            "regY","regY2","regB","regB2", "regB3")


dat <- data.frame(dat)

#dat[,allabs]
cbind(names(dat), allabs)

nclY=c(1,4,4,5,4,1)
nclY2=c(1,4,4,4,5,1,4)
nclA2=c(4,4,4,4)
nclA3=c(4,4,4,4)
nclA5=c(5)
nclB2=c(4,4,4,4)
nclB3=c(4,4,4,4,4)
flag="KR"
cvn <- c("yrqtr","latlong","hooks","hbf","vessid","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")
r=1

str(dat[,cvn[]])
cbind(names(dat), allabs)

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


####### Create datasets ######################################################
#covarnames <- c("yrqtr","latlong","hooks","hbf","vessid","Total","lat","lon")
#dataset <- cbind(dat2[,covarnames],mmult_dat,pcs[,1:6],setpcs_tp[,1:6],pcsBin[,1:6],setpcsBin_tp[,1:6],FT,cldat$setdat[,c("kcltrp","clrtrp","hcltrp","kclset","clrset","hclset")])

#################### Run standardization models for each species ##########################
#***********************************************
#  RUN MULTISPECIES STANDARDIZATION PROCEFURES #
#***********************************************

# max number of node points for GAM splines
kn = 2
sp="bet"
#sp="yft"
# run standardization models for each species
#for(sp in c("bet","yft")) {
dataset$y <- dataset[,sp]
mn <- with(dataset,0.1* mean(y/hooks))

# fitting year-effect (Nominal CPUE)
#m_base <- glm(y~as.factor(yy),data=dataset,family=Tweedie(p=p))
#m_base <- gam(log((y/hooks)+mn)~as.factor(yqx),data=dataset,family=gaussian) # runs much faster, and residual distribution looks better
#gam.check(m_base)
table(dataset$hcltrp, useNA="always")
dataset$hcltrp <- as.factor(dataset$hcltrp)

m_gam <- gam(log((y/hooks)+mn)~as.factor(yrqtr) + vessid + latlong + s(hooks,k=30) + s(hbf,k=15) + hcltrp,data=dataset,family=gaussian)
windows();par(mfrow=c(2,2))
gam.check(m_gam);savePlot(paste0("R",r,sp," gamcheck.png"),type="png")
windows(width=20,height=14)
plot.gam(m_gam,pages=1,all.terms=TRUE)
savePlot(paste0("gamplot_base_R",r,sp,".png"),type="png")
graphics.off()
#  # PCA set level
#  modgam_PCA <- gam(log((y/hooks)+mn) ~ as.factor(yrqtr) + vessid + latlong + s(hooks,k=30) + s(hbf,k=30) + s(PC1,k=30) + s(PC2,k=30) + s(PC3,k=30),data=dataset,family=gaussian)
#  # BPCA set level
#  modgam_BPCA <- gam(log((y/hooks)+mn) ~ as.factor(yrqtr) + vessid + latlong + s(hooks,k=30) + s(hbf,k=30) + as.factor(BPC1) + as.factor(BPC2) + as.factor(BPC3),data=dataset,family=gaussian)
#  # PCA trip level
#  modgam_TPCA <- gam(log((y/hooks)+mn) ~ as.factor(yrqtr) + vessid + latlong + s(hooks,k=30) + s(hbf,k=30) + s(TPC1,k=30) + s(TPC2,k=30) + s(TPC3,k=30),data=dataset,family=gaussian)
#  # PCA set level drop species of interest
#  save(m_gam,modgam_PCA,modgam_BPCA,modgam_TPCA,file="models_PCA.RData")
#  dat_drop <- doPCA_drop(dat=datx[datx$ssn==1,],plotScree=T,sp=sp)
#  modgam_PCA_drop <- gam(log((y/hooks)+mn) ~ as.factor(yrqtr) + vessid + latlong + s(hooks,k=30) + s(hbf,k=30) + s(PC1,k=30) + s(PC2,k=30) + s(PC3,k=30),data=dat_drop$dat,family=gaussian)
#
#  # kmeans clusters unscaled data set level
#  modgam_kcl1 <- gam(log((y/hooks)+mn) ~ as.factor(yrqtr) + vessid + latlong + s(hooks,k=30) + s(hbf,k=30) + as.factor(FT),data=dataset,family=gaussian)
#  # kmeans clusters scaled data set level
#  modgam_kcls <- gam(log((y/hooks)+mn) ~ as.factor(yrqtr) + vessid + latlong + s(hooks,k=30) + s(hbf,k=30) + as.factor(kclset),data=dataset,family=gaussian)
#  # clara clusters scaled data set level
#  modgam_clrs <- gam(log((y/hooks)+mn) ~ as.factor(yrqtr) + vessid + latlong + s(hooks,k=30) + s(hbf,k=30) + as.factor(clrset),data=dataset,family=gaussian)
#  # Ward hclust clusters scaled data set level
#  modgam_hcls <- gam(log((y/hooks)+mn) ~ as.factor(yrqtr) + vessid + latlong + s(hooks,k=30) + s(hbf,k=30) + as.factor(hclset),data=dataset,family=gaussian)
#  # Ward hclust clusters scaled data trip level
#  modgam_hclt <- gam(log((y/hooks)+mn) ~ as.factor(yrqtr) + vessid + latlong + s(hooks,k=30) + s(hbf,k=30) + as.factor(hcltrp),data=dataset,family=gaussian)
#  save(m_gam,modgam_PCA,modgam_BPCA,modgam_TPCA,modgam_PCA_drop,modgam_kcl1,modgam_kcls,modgam_clrs,modgam_hcls,modgam_hclt,file=paste("mgam",sp,"log.RData",sep="_"))
#}
#
#model <- glm(log((y/hooks)+mn)~as.factor(yrqtr) + vessid + latlong + ns(hooks,df=18) + ns(hbf,df=14),data=dataset,family=gaussian)
#
#table(floor(dataset$hbf))

lsos()
#load("mgam_yft_log.RData")
#load("mgam_alb_log.RData")
#load("mgam_BSH_log.RData")

# Predict year effects from each model
# Test w/o species of interest
#for(sp in c("alb","yft","BSH")) {
#  load(paste0("mgam_",sp,"_log.RData"))
#  make_comp_plots(dataset,m_gam,modgam_PCA,modgam_BPCA,modgam_TPCA,modgam_PCA_drop,modgam_kcl1,modgam_kcls,modgam_clrs,modgam_hcls,modgam_hclt)
#  savePlot(paste0("compare2 std-PCA-cluster_drop R",r,sp,".png"),type="png")
#  make_comp_ratio_plots(dataset,m_gam,modgam_PCA,modgam_BPCA,modgam_TPCA,modgam_PCA_drop,modgam_kcl1,modgam_kcls,modgam_clrs,modgam_hcls,modgam_hclt)
#  savePlot(paste0("compare2 std-PCA-cluster_drop_ratios R",r,sp,".png"),type="png")
#  }


#plot.gam(modgam_PCA,pages=1,all.terms=T)
#savePlot(paste0("gamplot_PCA_",sp,".png"),type="png")
#
#apply(dataset[,18:(17+29)],2,sum)
## Examine factors associated with each cluster or PCA
#boxplots_spCL(dat=dataset,cl="FT",ti="outL",outL=F)
#boxplots_spCL(dat=dataset,cl="kcltrp",ti="outL",outL=F)
#boxplots_spCL(dat=dataset,cl="hcltrp",ti="outL",outL=F)
#boxplots_spCL(dat=dataset,cl="clrtrp",ti="outL",outL=F)
#boxplots_spCL_comp(dat=dataset,cl="FT",ti="outL",outL=F)
#boxplots_spCL_comp(dat=dataset,cl="kcltrp",ti="outL",outL=F)
#boxplots_spCL_comp(dat=dataset,cl="hcltrp",ti="outL",outL=F)
#boxplots_spCL_comp(dat=dataset,cl="clrtrp",ti="outL",outL=F)
#boxplots_CL(dat=dataset,cl="kcltrp",ti="outL",outL=F)
#boxplots_CL(dat=dataset,cl="hcltrp",ti="outL",outL=F)
#boxplots_CL(dat=dataset,cl="clrtrp",ti="outL",outL=F)
#boxplots_CL(dat=dataset,cl="FT",ti="outL",outL=F)
#boxplots_CL(dat=dataset,cl="kclset",ti="outL",outL=F)
#boxplots_CL(dat=dataset,cl="clrset",ti="outL",outL=F)
#
#boxplots_PCA(dat=dataset,nPCA=3)
#boxplots_spPCA(dat=dataset,nPCA=3,ti="")
#boxplots_spPCA(dat=dataset,nPCA=3,ti="outL",outL=F)
#mapPCA(dat=dataset,nPCA=3,ti="regY2")
#boxplots_TPCA(dat=dataset,nPCA=3)
#boxplots_spTPCA(dat=dataset,nPCA=3,ti="")
#boxplots_spTPCA(dat=dataset,nPCA=3,ti="outL",outL=F)
#mapTPCA(dat=dataset,nPCA=3,ti="regY2")


# influence plots to sus out why allsp_PCA is so different
#summary(modgam_PCA)
dataset$yrqtr<- as.factor(dataset$yrqtr)
dataset$latlong<- as.factor(dataset$latlong)
dataset$reg=r
dat2 <- select_data_JPIO(indat=dataset,runreg=2,runsp=sp,mt,minqtrs=1,maxqtrs=500,llstrat=5,addcl=NA,addpca=NA,samp=1)
dat2$y <- dat2[,sp]
mn <- with(dat2,0.1* mean(y/hooks))

modglm <- glm(log((y/hooks)+mn) ~ yrqtr + vessid + latlong + ns(hooks,df=11) + ns(hbf,df=7) + ns(moon,df=4),data=dat2,family=gaussian)
mod_novess <- glm(log((y/hooks)+mn) ~ yrqtr + latlong + ns(hooks,df=11) + ns(hbf,df=7) + ns(moon,df=4),data=dat2,family=gaussian)
summ <- summary(modglm)
summ_novess <- summary(mod_novess)
save(summ,summ_novess,file=paste0("summary full R",r,sp,".RData"))
newdat_pred <- make_newdat2(modglm,datx=dat2)
newdat_pred_novess <- make_newdat2(mod_novess,datx=dat2)
save(newdat_pred,newdat_pred_novess,file=paste0("newdat_pred full R",r,sp,".RData"))
#modglm_PCA <- glm(log((y/hooks)+mn) ~ yrqtr + vessid + latlong + ns(hooks,df=11) + ns(hbf,df=28) + ns(PC1,df=10) + ns(PC2,df=17) + ns(PC3,df=23),data=dataset,family=gaussian)
#dat_drop <- doPCA_drop(dat=datx[datx$ssn==1,],plotScree=T,sp=sp)
#dat_drop$dat$BAIT<- as.factor(dat_drop$dat$BAIT)
#dat_drop$dat$yqx<- as.factor(dat_drop$dat$yqx)
#modglm_PCA_drop <- glm(log((y/hooks)+mn) ~ yqx + VSL + latlon1 + ns(hooks,df=12) + ns(HPB,df=16) + ns(LGHTS,df=21) + BAIT + ns(STIME,df=16) + ns(MAINL,df=15) + ns(PC1,df=10) + ns(PC2,df=17) + ns(PC3,df=23) + ns(PC4,df=28) + ns(PC5,df=25) + ns(PC6,df=24),data=dat_drop$dat,family=gaussian)
#xxx <- dataset[dataset$yy<2002 & dataset$HPB<15,c("y","yy","hooks","VSL","latlon1","hooks","HPB","LGHTS","BAIT","STIME","MAINL","PC1","PC2","PC3","PC4","PC5","PC6")]
#xxx <- dataset[dataset$yy<2002 & dataset$HPB<15,]
#xxx$BAIT <- as.factor(xxx$BAIT)
#xxx$yqx <- as.factor(xxx$yqx)
#test <- glm(log((y/hooks)+mn) ~ yqx + VSL + latlon1 + hooks + HPB + LGHTS + BAIT + STIME + MAINL + PC1 + PC2 + PC3 + PC4 + PC5 + PC6,data=xxx,family=gaussian)
#test <- glm(log((y/hooks)+mn) ~ as.factor(yy) + VSL + latlon1 + hooks + HPB + LGHTS + as.factor(BAIT) + STIME + MAINL + PC1 + PC2 + PC3 + PC4 + PC5 + PC6,data=xxx,family=gaussian)
#summary(log(xxx$y/xxx$hooks + mn))
table(dataset$vessid,dataset$op_yr)


infyr=Influence$new(modglm)
infyr$calc()
a <- infyr$summary
write.csv(a,file=paste0("infyr_summary_R",r,sp,".csv"))
infyr$stanPlot();savePlot(paste0("Stanplot_baseglm_R",r,sp,".png"),type="png")
infyr$stepPlot();savePlot(paste0("stepPlot_baseglm_R",r,sp,".png"),type="png")
infyr$influPlot();savePlot(paste0("influPlot_baseglm_R",r,sp,".png"),type="png")
infyr$cdiPlot('latlong');savePlot(paste0("influPlot_latlong_baseglm_R",r,sp,".png"),type="png")
infyr$cdiPlot('vessid');savePlot(paste0("influPlot_vessid_baseglm_R",r,sp,".png"),type="png")
infyr$cdiPlot('ns(hooks, df = 11)');savePlot(paste0("influPlot_hooks_baseglm_R",r,sp,".png"),type="png")
infyr$cdiPlot('ns(hbf, df = 7)');savePlot(paste0("influPlot_lhbf_baseglm_R",r,sp,".png"),type="png")
infyr$cdiPlot('ns(moon, df = 4)');savePlot(paste0("influPlot_moon_baseglm_R",r,sp,".png"),type="png")

#infyrPCA=Influence$new(modglm_PCA)
#infyrPCA$calc()
#infyrPCA$summary
#infyrPCA$stanPlot()
#infyrPCA$stepPlot()
#infyrPCA$influPlot()
#infyrPCA$cdiPlot('ns(PC1, df = 10)')
#infyrPCA$cdiPlot('ns(PC2, df = 17)')
#infyrPCA$cdiPlot('ns(PC3, df = 23)')
#infyrPCA_drop=Influence$new(modglm_PCA_drop)
#infyrPCA_drop$calc()
#infyrPCA_drop$summary
#infyrPCA_drop$stanPlot()
#infyrPCA_drop$stepPlot()
#infyrPCA_drop$influPlot()
#infyrPCA_drop$cdiPlot('ns(PC1, df = 10)')
#infyrPCA_drop$cdiPlot('ns(PC2, df = 17)')
#infyrPCA_drop$cdiPlot('ns(PC3, df = 23)')
#infyrPCA_drop$cdiPlot('ns(PC4, df = 28)')

datearly <- dataset[as.numeric(as.character(dataset$yrqtr)) < 1998 & dataset$hbf < 17.5,]
datearly$yrqtr<- as.factor(datearly$yrqtr)
datearly$latlong<- as.factor(datearly$latlong)
mn <- with(datearly,0.1* mean(y/hooks))
modglm_early <- glm(log((y/hooks)+mn) ~ yrqtr + vessid + latlong + ns(hooks,df=11) + ns(hbf,df=10), data=datearly,family=gaussian)
mod_novess <- glm(log((y/hooks)+mn) ~ yrqtr + latlong + ns(hooks,df=11) + ns(hbf,df=10),data=datearly,family=gaussian)
summ <- summary(modglm_early)
summ_novess <- summary(mod_novess)
save(summ,summ_novess,file=paste0("summary early R",r,sp,".RData"))
newdat_pred <- make_newdat(modglm_early,datx=datearly)
newdat_pred_novess <- make_newdat(mod_novess,datx=datearly)
save(newdat_pred,newdat_pred_novess,file=paste0("newdat_pred early R",r,sp,".RData"))

infyr_early=Influence$new(modglm_early)
infyr_early$calc()
graphics.off()
a <- infyr_early$summary
write.csv(a,file=paste0("infyr_early_summary_R",r,sp,".csv"))
infyr_early$stanPlot();savePlot(paste0("Stanplot_early_baseglm_R",r,sp,".png"),type="png")
infyr_early$stepPlot();savePlot(paste0("stepPlot_early_baseglm_R",r,sp,".png"),type="png")
infyr_early$influPlot();savePlot(paste0("influPlot_early_baseglm_R",r,sp,".png"),type="png")
infyr_early$cdiPlot('latlong');savePlot(paste0("influPlot_early_latlong_baseglm_R",r,sp,".png"),type="png")
infyr_early$cdiPlot('vessid');savePlot(paste0("influPlot_early_vessid_baseglm_R",r,sp,".png"),type="png")
infyr_early$cdiPlot('ns(hooks, df = 11)');savePlot(paste0("influPlot_early_hooks_baseglm_R",r,sp,".png"),type="png")
infyr_early$cdiPlot('ns(hbf, df = 10)');savePlot(paste0("influPlot_early_lhbf_baseglm_R",r,sp,".png"),type="png")

datlate <- dataset[as.numeric(as.character(dataset$yrqtr)) > 2000,]
mn <- with(datlate,0.1* mean(y/hooks))
datlate$vessid <- as.factor(as.character(datlate$vessid))
datlate$yrqtr <- as.factor(as.character(datlate$yrqtr))
datlate$latlong <- as.factor(as.character(datlate$latlong))
modglm_late <- glm(log((y/hooks)+mn) ~ yrqtr + vessid + latlong + ns(hooks,df=11) + ns(hbf,df=10),data=datlate,family=gaussian)
summ <- summary(modglm_late)
save(summ,file=paste0("summary late R",r,sp,".RData"))
newdat_pred <- make_newdat(modglm_late,datx=datlate)
save(newdat_pred,file=paste0("newdat_pred late R",r,sp,".RData"))

infyr_late=Influence$new(modglm_late)
infyr_late$calc()
graphics.off()
a <- infyr_late$summary
write.csv(a,file=paste0("infyr_late_summary_R",r,sp,".csv"))
infyr_late$stanPlot();savePlot(paste0("Stanplot_late_baseglm_R",r,sp,".png"),type="png")
infyr_late$stepPlot();savePlot(paste0("stepPlot_late_baseglm_R",r,sp,".png"),type="png")
infyr_late$influPlot();savePlot(paste0("influPlot_late_baseglm_R",r,sp,".png"),type="png")
infyr_late$cdiPlot('latlong');savePlot(paste0("influPlot_late_latlong_baseglm_R",r,sp,".png"),type="png")
infyr_late$cdiPlot('vessid');savePlot(paste0("influPlot_late_vessid_baseglm_R",r,sp,".png"),type="png")
infyr_late$cdiPlot('ns(hooks, df = 11)');savePlot(paste0("influPlot_late_hooks_baseglm_R",r,sp,".png"),type="png")
infyr_late$cdiPlot('ns(hbf, df = 10)');savePlot(paste0("influPlot_late_lhbf_baseglm_R",r,sp,".png"),type="png")

##################################################
# Korea only, clusters, HBF
#
# Based on joint standardization analyses from 2016

projdir <- "~/IOTC/2017_CPUE/"
krdir <- paste0(projdir, "KR/")
datadir <- paste0(krdir, "data/")
kralysis_dir <- paste0(krdir, "analyses/")
krfigs <- paste0(krdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
resdir <- paste0(kralysis_dir,"std_cl_KRonly_hbf/")
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


#clkeepJP_Y <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(1,2,5),c(1,2,3,4),c(0)))
clkeepJP_Y <- list()
clkeepKR_Y <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(2,3,5),c(1,2,3,4),c(0)))
clkeepTW_Y <- list()
clkeepSY_Y <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(2),c(1,2,3,4),c(0)))
clk_Y <- list(JP=clkeepJP_Y,KR=clkeepKR_Y,TW=clkeepTW_Y,SY=clkeepSY_Y)

#clkeepJP_B2 <- list("bet"=list(c(1,2,3,4,5),c(1,2,3,4,5),c(1,2,3,4),c(1,2,3,4)))
clkeepJP_B2 <- list()
clkeepKR_B2 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepTW_B2 <- list()
clkeepSY_B2 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2),c(1,2,4)))
clk_B2 <- list(JP=clkeepJP_B2,KR=clkeepKR_B2,TW=clkeepTW_B2,SY=clkeepSY_B2)

runpars <- list()
runpars[["bet"]] <- list(regtype = "regB2", regtype2 = "B2", clk = clk_B2, doregs = 1:2, addcl = TRUE, dohbf = TRUE, cltype = "hcltrp")
runpars[["yft"]] <- list(regtype = "regY",  regtype2 = "Y",  clk = clk_Y,  doregs = c(2,3,5), addcl = TRUE, dohbf = TRUE, cltype = "hcltrp")

runreg=1; runsp="bet"

maxyr = 2017; keepd = TRUE; maxqtrs=200; minqtrs_byreg = c(8,8,2,2,5,5,5,5); addbranch<-F;addother=F;addalb=F
for(runsp in c("bet", "yft")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  jdat <- data.frame()
  for(flag in c("KR")) {
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
    a <- jdat[jdat$vessid != "KR1",]

    wtt.all   <- mk_wts(glmdat,wttype="area")
    fmla.oplogn <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=F,addcl=T,nhbf=3)
    fmla.oplogn_ncl <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=F,addcl=F,nhbf=3)
    fmla.boatlogn <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=T,addcl=T,nhbf=3)
    fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=T,addcl=F,nhbf=3)
    mn <- with(glmdat,0.1* mean(get(runsp)/hooks))

    modlab="lognC_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if(lu(glmdat$clust) > 1)
    { model <- glm(fmla.oplogn,data=glmdat,weights=wtt.all,family="gaussian", y = keepd, model = keepd);gc() } else
    { model <- glm(fmla.oplogn_ncl,data=glmdat,weights=wtt.all,family="gaussian", y = keepd, model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if(lu(glmdat$clust) > 1)
    { model <- glm(fmla.boatlogn,data=glmdat,weights=wtt.all,family="gaussian", y = keepd, model = keepd);gc() } else
    { model <- glm(fmla.boatlogn_ncl,data=glmdat,weights=wtt.all,family="gaussian", y = keepd, model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    # delta lognormal
    modlab="dellog_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg);
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)
  }
}

# debug(summarize_and_store_dl)
# debug(do_deltalog)

# Test some clustering options
datr <- data.frame(datr)
tripid <- "tripidmon"
datr$TRIP_NUM <- as.vector(datr[,tripid])
ncl=3
spec_dat <- datr[,allsp] #extract catch composition
spec_dat$sum <- apply(spec_dat, 1,sum)
nspec <- length(allsp)
dat2 <- datr[spec_dat$sum > 0,]
mmult_dat <- spec_dat[spec_dat$sum>0,]
clus_dat <- mmult_dat[,1:nspec]/mmult_dat$sum  # raw proportions
FT = kmeans(clus_dat,ncl)$cluster

aset <- na.omit(datr[,allsp])
aset <- scale(aset[,allsp])     # rescaled data

indat <- aggregate_by_trip(dat2,allsp)
atrp <- na.omit(indat);
#NbClust(atrp[,allspp],method="ward.D");flush.console()
atrp <- scale(atrp[,allsp])

dtrp <- dist(atrp, method = "euclidean");
hclustxx <- function(x) hclust(x,method="ward.D")
heatmap.2(atrp,
          hclustfun=hclustxx,
          cexRow=0.5, cexCol=0.95, # decrease font size of row/column labels
          scale="none", # we have already scaled the data
          trace="none") # cleaner heatmap


fittrp <- hclust(dtrp, method="ward.D")
plot(fittrp, labels = FALSE, hang=-1,  main = paste(titx,"trip")) # display dendogram  #looks like 3 (or 4)
grptrp <- cutree(fittrp, k=ncl) # cut tree into ncl clusters
print(table(grptrp))
rect.hclust(fittrp, k=ncl, border="red")

