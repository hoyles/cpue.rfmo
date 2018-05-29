setwd(jpfigs)
regYord <- c(1,2,3,6,5,4)
regBord <- c(1,3,2,4)
regA2ord <- c(1,3,2,4)
#####################################
#Prepare figures
# Logsheets per year by region
# Species composition maps
a <-  aggregate(cbind(bet,yft,alb,sbt,swo,mls,blm,bum,Total,hooks) ~ lon + lat + eval(5*floor((op_yr+5)/5)-5),data=dat[!is.na(dat$lon),],FUN=sum)
a5 <- aggregate(cbind(bet,yft,alb,sbt,swo,mls,blm,bum,Total,hooks) ~ lon5 + lat5 + eval(5*floor((op_yr)/5)),data=dat,FUN=sum)
names(a)[3] <- names(a5)[3] <- "decade"
names(a5)[1:2] <- c("lon","lat")

windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap(indat=a,vbl=a$bet/(a$bet+a$yft),dcd=d,latlim=c(-18,10),lonlim=c(40,120),ti="BET / BET + YFT")
savePlot("PropBET in YFT_BET5",type="png")
windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap(indat=a,vbl=a$bet/(a$Total),dcd=d,latlim=c(-45,20),lonlim=c(40,120),ti="BET / Total")
savePlot("PropBET in Total",type="png")
windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap(indat=a,vbl=a$swo/(a$Total),dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SWO / Total",brk2=seq(0,1,.05))
savePlot("PropSWO in Total",type="png")
windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap(indat=a,vbl=a$alb/(a$Total),dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="ALB / Total",brk2=seq(0,1,.1))
savePlot("PropALB in Total",type="png")
windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap(indat=a,vbl=a$yft/(a$Total),dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="YFT / Total")
savePlot("PropYFT in Total",type="png")
windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap(indat=a,vbl=a$mls/(a$Total),dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="MLS / Total",brk2=seq(0,1,.05))
savePlot("PropMLS in Total",type="png")
windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap(indat=a,vbl=a$sbt/(a$Total),dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SBT / Total",brk2=seq(0,1,.05))
savePlot("PropSBT in Total",type="png")
windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap(indat=a,vbl=a$blm/(a$Total),dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="BLM / Total",brk2=seq(0,1,.05))
savePlot("PropBLM in Total",type="png")
windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap(indat=a,vbl=a$bum/(a$Total),dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="BUM / Total",brk2=seq(0,1,.05))
savePlot("PropBUM in Total",type="png")

windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap(indat=a5,vbl=a5$bet/(a5$bet+a5$yft),dcd=d,latlim=c(-18,10),lonlim=c(40,120),ti="BET / BET + YFT")
savePlot("PropBET in YFT_BET5",type="png")
windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap(indat=a5,vbl=a5$bet/(a5$Total),dcd=d,latlim=c(-45,20),lonlim=c(40,120),ti="BET / Total")
savePlot("PropBET in Total5",type="png")
windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap(indat=a5,vbl=a5$swo/(a5$Total),dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SWO / Total",brk2=seq(0,1,.05))
savePlot("PropSWO in Total5",type="png")
windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap(indat=a5,vbl=a5$alb/(a5$Total),dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="ALB / Total",brk2=seq(0,1,.1))
savePlot("PropALB in Total5",type="png")
windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap(indat=a5,vbl=a5$yft/(a5$Total),dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="YFT / Total")
savePlot("PropYFT in Total5",type="png")
windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap(indat=a5,vbl=a5$mls/(a5$Total),dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="MLS / Total",brk2=seq(0,1,.05))
savePlot("PropMLS in Total5",type="png")
windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap(indat=a5,vbl=a5$sbt/(a5$Total),dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="SBT / Total",brk2=seq(0,1,.05))
savePlot("PropSBT in Total5",type="png")
windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap(indat=a5,vbl=a5$blm/(a5$Total),dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="BLM / Total",brk2=seq(0,1,.05))
savePlot("PropBLM in Total5",type="png")
windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap(indat=a5,vbl=a5$bum/(a5$Total),dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="BUM / Total",brk2=seq(0,1,.05))
savePlot("PropBUM in Total5",type="png")

a <-  aggregate(cbind(bet,yft,alb,sbt,swo,mls,blm,bum,Total,hooks) ~ lon + lat + op_yr,data=dat[!is.na(dat$lon),],FUN=sum)
names(a)[3] <- names(a5)[3] <- "decade"
windows(width=20,height=16);par(mfrow=c(3,3),mar=c(2,2,2,2))
for(d in seq(1973,1981,1)) { plot_catchmap2(indat=a,vbl=a$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="Effort") }
savePlot("Effort in 1970s",type="png")
windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(2004,2016,1)) { plot_catchmap2(indat=a,vbl=a$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="Effort") }
savePlot("Effort in 2000s",type="png")



windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,2,2,1),oma=c(0,0,1,0))
for (r in regYord) {
  llv <- dat[dat$regY==r,]
  hist(llv$dmy,breaks=seq(min(dat$dmy),max(dat$dmy),1),freq=T,xlab="Date",main=paste0("R",r))
  }
title("Sets per day",outer=T,line=0)
savePlot(filename=paste("Sets per day by region",sep=""),type="png")

# windows(height=12,width=12); par(mfcol=c(2,2),mar=c(3,2,2,1),oma=c(0,0,1,0))
# for (r in regA2ord) {
#   llv <- dat[dat$regA2==r,]
#   hist(llv$dmy,breaks=seq(min(dat$dmy),max(dat$dmy),1),freq=T,xlab="Date",main=paste0("R",r))
#   }
# title("Sets per day",outer=T,line=0)
# savePlot(filename=paste("Sets per day by region ALB2",sep=""),type="png")

for (r in regYord) {
  llv <- dat[dat$regY==r,]
  windows(height=12,width=15);par(mfrow=c(3,3),oma=c(0,0,1,0))
  for(dec in seq(1975,2015,5)) {
    a <- llv[llv$op_yr >= dec & llv$op_yr < dec+5,]
    hist(a$hooks,breaks=seq(-10,45010,50),xlim=c(0,5000),main=dec)
    }
  title(paste0("Hooks per set R",r),outer=T,line=0)
  savePlot(filename=paste0("Histogram hooks by decade R",r),type="png")
}

# for (r in regA2ord) {
#   llv <- dat[dat$regA2==r,]
#   windows(height=12,width=15);par(mfrow=c(3,3),oma=c(0,0,1,0))
#   for(dec in seq(1975,2015,5)) {
#     a <- llv[llv$op_yr >= dec & llv$op_yr < dec+5,]
#     hist(a$hooks,breaks=seq(-10,45010,50),xlim=c(0,5000),main=dec)
#     }
#   title(paste0("Hooks per set R",r),outer=T,line=0)
#   savePlot(filename=paste0("Histogram hooks by decade R",r,"ALB2"),type="png")
# }

windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,2,2,1))
for (r in regYord) {
  llv <- dat[dat$regY==r,]
  a <- table(llv$vessid,llv$op_yr)
  a <- apply(a>0,1,sum)
  print(table(a))
  a <- tapply(llv$op_yr,llv$op_yr,length)
  plot(as.numeric(names(a)),a,xlab="yr",ylab="Logsheet records",main=paste("YFT Region",r),xlim=c(1952,2017))
  }
savePlot(filename=paste("Number of records",sep=""),type="png")
# windows(height=12,width=12); par(mfcol=c(2,2),mar=c(3,2,2,1))
# for (r in regA2ord) {
#   llv <- dat[dat$regA2==r,]
#   a <- table(llv$vessid,llv$op_yr)
#   a <- apply(a>0,1,sum)
#   print(table(a))
#   a <- tapply(llv$op_yr,llv$op_yr,length)
#   plot(as.numeric(names(a)),a,xlab="yr",ylab="Logsheet records",main=paste("ALB Region",r),xlim=c(1952,2016))
#   }
# savePlot(filename=paste("Number of records ALB2",sep=""),type="png")
#
windows(height=12,width=14);par(mfrow=c(4,2))
for(dec in seq(1950,2010,10)) {
  a <- dat[dat$op_yr >= dec & dat$op_yr < dec+10,]
  hist(a$hooks,breaks=seq(-10,45010,50),xlim=c(0,5000),main=dec)
  }
savePlot(filename=paste0("Histogram hooks by decade"),type="png")

# Vessels per year by region
windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,2,2,1))
for (r in regYord) {
  llv <- dat[dat$regY==r,]
  a <- tapply(llv$vessid,llv$op_yr,lu)
  plot(names(a),a,xlab="Year",ylab="",main=paste("YFT Region",r),xlim=range(dat$op_yr))
  }
savePlot(filename=paste("Unique vessels by year",sep=""),type="png")
# windows(height=12,width=12); par(mfcol=c(2,2),mar=c(3,2,2,1))
# for (r in regA2ord) {
#   llv <- dat[dat$regA2==r,]
#   a <- tapply(llv$vessid,llv$op_yr,lu)
#   plot(names(a),a,xlab="Year",ylab="",main=paste("ALB Region",r),xlim=range(dat$op_yr))
#   }
# savePlot(filename=paste("Unique vessels by year ALB2",sep=""),type="png")

# Time distribution of vessels
vy <- list()
windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,2,2,1))
for (r in regYord) {
  llv <- dat[dat$regY==r,]
  vess <- as.numeric(unique(llv$vessid))
  minyr <- maxyr <- tonn <- vess
  vvv <- llv[order(llv$vessid,llv$op_yr),]
  minyr <- as.numeric(unlist(vvv[match(vess,as.numeric(vvv$vessid)),"op_yr"]))
#  tonn <- vvv[match(vess,as.numeric(vvv$vessid)),"tonnage"]
  vvv <- llv[order(llv$vessid,-llv$op_yr),]
  maxyr <- as.numeric(unlist(vvv[match(vess,as.numeric(vvv$vessid)),"op_yr"]))
  vessyrs <- data.frame(vess=vess,minyr=as.numeric(minyr),maxyr=as.numeric(maxyr),stringsAsFactors=F)
  vessyrs <- vessyrs[order(-floor(vessyrs$minyr),-floor(vessyrs$maxyr)),]
  vy[[r]] <- vessyrs
  plot(1:length(vess),1:length(vess),xlim=c(1978,2016),type="n",xlab="Years",ylab="Vessel",main=paste("YFT Region",r))
  for (i in 1:length(vess)) {
    lines(c(floor(vessyrs[i,]$minyr),floor(vessyrs[i,]$maxyr)),c(i,i))
    }
  }
savePlot(filename=paste("Time distribution of vessels 1",sep=""),type="png")
# Time distribution of vessels 2
windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,2,2,1))
for (r in regYord) {
  vessyrs <- vy[[r]]
  vessyrs <- vessyrs[order(-floor(vessyrs$maxyr)),]
  llv <- dat[dat$regY==r,]
  vess <- unique(llv$vessid)
  plot(1:length(vess),1:length(vess),xlim=c(1975,2016),type="n",xlab="Years",ylab="Vessel",main=paste("YFT Region",r))
  for (i in 1:length(vess)) {
    lines(c(floor(vessyrs[i,]$minyr),floor(vessyrs[i,]$maxyr)),c(i,i))
    }
  }
savePlot(filename=paste("Time distribution of vessels 2",sep=""),type="png")
windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,2,2,1)) # Runs slow
for (r in regYord) {
  vessyrs <- vy[[r]]
  llv <- dat[dat$regY==r,]
  vess <- as.numeric(unique(llv$vessid))
  minyr <- vess
  plot(1:length(vess),1:length(vess),xlim=c(1975,2016),type="n",xlab="Years",ylab="Vessel",main=paste("YFT Region",r))
  for (i in 1:length(vess)) {
    a <- floor(llv[vessyrs[i,1]==as.numeric(llv$vessid),]$op_yr)
    pp <- unique(a)
    pp2 <- tapply(a,a,length)
#    points(pp,rep(i,length(pp)),cex=0.6,pch=3)
    symbols(pp,rep(i,length(pp)),sqrt(pp2)/40, add = T, inches =FALSE)
    }
  }
savePlot(filename=paste("Time distribution of vessels 4",sep=""),type="png")
# ## Time distribution of vessels ALB 1
# vy <- list()
# windows(height=12,width=12); par(mfcol=c(2,2),mar=c(3,2,2,1))
# for (r in regA2ord) {
#   llv <- dat[dat$regA2==r,]
#   vess <- as.numeric(unique(llv$vessid))
#   minyr <- maxyr <- tonn <- vess
#   vvv <- llv[order(llv$vessid,llv$op_yr),]
#   minyr <- as.numeric(unlist(vvv[match(vess,as.numeric(vvv$vessid)),"op_yr"]))
# #  tonn <- vvv[match(vess,as.numeric(vvv$vessid)),"tonnage"]
#   vvv <- llv[order(llv$vessid,-llv$op_yr),]
#   maxyr <- as.numeric(unlist(vvv[match(vess,as.numeric(vvv$vessid)),"op_yr"]))
#   vessyrs <- data.frame(vess=vess,minyr=as.numeric(minyr),maxyr=as.numeric(maxyr),stringsAsFactors=F)
#   vessyrs <- vessyrs[order(-floor(vessyrs$minyr),-floor(vessyrs$maxyr)),]
#   vy[[r]] <- vessyrs
#   plot(1:length(vess),1:length(vess),xlim=c(1978,2016),type="n",xlab="Years",ylab="Vessel",main=paste("ALB Region",r))
#   for (i in 1:length(vess)) {
#     lines(c(floor(vessyrs[i,]$minyr),floor(vessyrs[i,]$maxyr)),c(i,i))
#     }
#   }
# savePlot(filename=paste("Time distribution of vessels 1 ALB2",sep=""),type="png")
# Time distribution of vessels ALB 2
# windows(height=12,width=12); par(mfcol=c(2,2),mar=c(3,2,2,1))
# for (r in regA2ord) {
#   vessyrs <- vy[[r]]
#   vessyrs <- vessyrs[order(-floor(vessyrs$maxyr)),]
#   llv <- dat[dat$regA2==r,]
#   vess <- unique(llv$vessid)
#   plot(1:length(vess),1:length(vess),xlim=c(1975,2016),type="n",xlab="Years",ylab="Vessel",main=paste("ALB Region",r))
#   for (i in 1:length(vess)) {
#     lines(c(floor(vessyrs[i,]$minyr),floor(vessyrs[i,]$maxyr)),c(i,i))
#     }
#   }
# savePlot(filename=paste("Time distribution of vessels 2 ALB2",sep=""),type="png")
# windows(height=12,width=12); par(mfcol=c(2,2),mar=c(3,2,2,1)) # Runs slow
# for (r in regA2ord) {
#   vessyrs <- vy[[r]]
#   llv <- dat[dat$regA2==r,]
#   vess <- as.numeric(unique(llv$vessid))
#   minyr <- vess
#   plot(1:length(vess),1:length(vess),xlim=c(1975,2016),type="n",xlab="Years",ylab="Vessel",main=paste("ALB Region",r))
#   for (i in 1:length(vess)) {
#     a <- floor(llv[vessyrs[i,1]==as.numeric(llv$vessid),]$op_yr)
#     pp <- unique(a)
#     pp2 <- tapply(a,a,length)
# #    points(pp,rep(i,length(pp)),cex=0.6,pch=3)
#     symbols(pp,rep(i,length(pp)),sqrt(pp2)/40, add = T, inches =FALSE)
#     }
#   }
# savePlot(filename=paste("Time distribution of vessels 4 ALB2",sep=""),type="png")
#
# # Effort by region
# windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1))
# for (r in regYord) {
#   llv <- dat[dat$regY==r,]
#   a <- tapply(llv$hooks,llv$yrqtr,sum)
#   plot(names(a),a,xlab="Year",ylab="Hooks",main=paste("YFT Region",r),xlim=range(dat$yrqtr))
#   }
# savePlot(filename="Effort by region by yrqtr",type="png")
# windows(height=12,width=12); par(mfcol=c(2,2),mar=c(3,4,2,1))
# for (r in regA2ord) {
#   llv <- dat[dat$regA2==r,]
#   a <- tapply(llv$hooks,llv$yrqtr,sum)
#   plot(names(a),a,xlab="Year",ylab="Hooks",main=paste("ALB Region",r),xlim=range(dat$yrqtr))
#   }
# savePlot(filename="Effort by region by yrqtr ALB2",type="png")

# Sets by region
windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1))
for (r in regYord) {
  llv <- dat[dat$regY==r,]
  a <- tapply(llv$hooks,llv$yrqtr,length)
  plot(names(a),a,xlab="Year",ylab="Sets",main=paste("YFT Region",r),xlim=range(dat$yrqtr))
  }
savePlot(filename="Sets by region by yrqtr",type="png")
# windows(height=12,width=12); par(mfcol=c(2,2),mar=c(3,4,2,1))
# for (r in regA2ord) {
#   llv <- dat[dat$regA2==r,]
#   a <- tapply(llv$hooks,llv$yrqtr,length)
#   plot(names(a),a,xlab="Year",ylab="Sets",main=paste("ALB Region",r),xlim=range(dat$yrqtr))
#   }
# savePlot(filename="Sets by region by yrqtr ALB2",type="png")

# total effort by region and yearqtr
e <- tapply(dat$hooks,list(dat$yrqtr,dat$regY),sum)
write.table(file="effort by region and yrqtr.csv",e,sep=",")
# e <- tapply(dat$hooks,list(dat$yrqtr,dat$regA2),sum)
# write.table(file="effort by region and yrqtr ALB2.csv",e,sep=",")

# Tonnage - should be a stacked histogram, but no time
#a <- unique(dat$vessid)
#a <- dat[match(a,dat$vessid),]
#windows(height=14,width=12); par(mfrow=c(2,1),mar=c(3,4,2,1))
#hist(a[a$newfishingcat==1,]$tonnage,breaks=seq(0,500,20),ylim=c(0,300),xlab="GRT (metric tonnes)",ylab="Number of vessels",main="Vessels",col=4,density=15,angle=135)
#hist(a[a$newfishingcat==2,]$tonnage,breaks=seq(0,500,20),col=1,density=25,angle=45,add=T)
#legend("topright",legend=c("Distant water","Offshore"),col=c(4,1),density=c(25,15),angle=c(45,135))
#
#hist(dat[dat$newfishingcat==1,]$tonnage,breaks=seq(0,500,20),xlab="GRT (metric tonnes)",,ylab="Number of sets",main="Sets",col=4,density=15,angle=135)
#hist(dat[dat$newfishingcat==2,]$tonnage,breaks=seq(0,500,20),xlab="GRT (metric tonnes)",main="",col=1,density=25,angle=45,add=T)
#legend("topright",legend=c("Distant water","Offshore"),col=c(4,1),density=c(25,15),angle=c(45,135))
#savePlot(file="plot tonnage by fishingcat",type="png")

# Effort coverage by region
# Load JPLLagg from SPC database
#ll <- read.table("D:/SPC_NRIFSF_data2011/QUERY091.csv",sep=",",header=T)
#ll$region <- rep(0, length(ll$hhooks))
#ll$region <- ifelse(ll$latd > 20 & ll$latd < 40 & ll$lond > 110 & ll$lond < 170, 1, ll$region)
#ll$region <- ifelse(ll$latd > 20 & ll$latd < 40 & ll$lond > 170 & ll$lond < 210, 2, ll$region)
##ll$region <- ifelse(ll$latd > -10 & ll$latd < 20 & ll$lond > 135 & ll$lond < 170, 3, ll$region)
#ll$region <- ifelse(ll$latd > -10 & ll$latd < 20 & ll$lond > 110 & ll$lond < 170, 3, ll$region)
#ll$region <- ifelse(ll$latd > -10 & ll$latd < 20 & ll$lond > 170 & ll$lond < 210, 4, ll$region)
#ll$region <- ifelse(ll$latd > -35 & ll$latd < -10 & ll$lond > 140 & ll$lond < 170, 5, ll$region)
#ll$region <- ifelse(ll$latd > -35 & ll$latd < -10 & ll$lond > 170 & ll$lond < 210, 6, ll$region)
###INDO/PH region
###ll$region <- ifelse(ll$LATD > -10 & ll$LATD < 20 & ll$LOND > 110 & ll$LOND < 135, 7, ll$region)
#ll <- ll[ll$region > 0,]
#ll$yrqtr <- ll$yy + ll$qtr/4 - 0.125
#effort <- tapply(ll$hhooks, list(ll$yrqtr, ll$region), sum)*100
#albc <- tapply(ll$alb_no, list(ll$yrqtr, ll$region), sum)
#betc <- tapply(ll$bet_no, list(ll$yrqtr, ll$region), sum)
#yftc <- tapply(ll$yft_no, list(ll$yrqtr, ll$region), sum)
#swoc <- tapply(ll$swo_no, list(ll$yrqtr, ll$region), sum)

#windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1))
#for (r in regYord) {
#  llv <- dat[dat$regY==r,]
#  e <- tapply(llv$hooks,llv$yrqtr,sum)
#  agge <- effort[,r]
#  plot(names(e),e/agge[match(names(e),names(agge))],xlab="Year",ylab="hooks",main=paste("YFT Region",r),ylim=c(0,3))
#  abline(h=1,lty=2)
#  }
#savePlot(filename="Effort coverage by region by yrqtr",type="png")
#windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1))
#for (r in regYord) {
#  llv <- dat[dat$regY==r,]
#  alb<- tapply(llv$alb,llv$yrqtr,sum)
#  agge <- albc[,r]
#  plot(names(alb),alb/agge[match(names(alb),names(agge))],xlab="Year",ylab="ALB catch",main=paste("YFT Region",r),ylim=c(0,3))
#  abline(h=1,lty=2)
#  }
#savePlot(filename="ALB coverage by region by yrqtr",type="png")
#windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1))
#for (r in regYord) {
#  llv <- dat[dat$regY==r,]
#  bet <- tapply(llv$bet,llv$yrqtr,sum)
#  agge <- betc[,r]
#  plot(names(bet),bet/agge[match(names(bet),names(agge))],xlab="Year",ylab="BET catch",main=paste("YFT Region",r),ylim=c(0,3))
#  abline(h=1,lty=2)
#  }
#savePlot(filename="BET coverage by region by yrqtr",type="png")
#windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1))
#for (r in regYord) {
#  llv <- dat[dat$regY==r,]
#  yft <- tapply(llv$yft,llv$yrqtr,sum)
#  agge <- yftc[,r]
#  plot(names(yft),yft/agge[match(names(yft),names(agge))],xlab="Year",ylab="YFT catch",main=paste("YFT Region",r),ylim=c(0,3))
#  abline(h=1,lty=2)
#  }
#savePlot(filename="YFT coverage by region by yrqtr",type="png")
#windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1))
#for (r in regYord) {
#  llv <- dat[dat$regY==r,]
#  swo <- tapply(llv$swo,llv$yrqtr,sum)
#  agge <- swoc[,r]
#  plot(names(swo),swo/agge[match(names(swo),names(agge))],xlab="Year",ylab="swo catch",main=paste("YFT Region",r),ylim=c(0,3))
#  abline(h=1,lty=2)
#  }
#savePlot(filename="SWO coverage by region by yrqtr",type="png")

windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1))
for (r in regYord) {
  cld <- dat[dat$regY==r,]
  rwd <- pd2[pd2$regY==r,]
  cle <- tapply(cld$hooks,factor(cld$yrqtr,levels=unique(c(rwd$yrqtr,cld$yrqtr))),sum)
  cle[is.na(cle)] <- 0
  rwe <- tapply(rwd$hooks,factor(rwd$yrqtr,levels=unique(c(rwd$yrqtr,cld$yrqtr))),sum)
  rwe[is.na(rwe)] <- 0
  plot(names(cle),cle/rwe[match(names(cle),names(rwe))],xlab="Year",ylab="Proportion of hooks",main=paste("YFT Region",r),ylim=c(0,1.1),xlim=range(prepdat$yrqtr))
  }
savePlot(filename="Cleaned effort proportion by region by yrqtr",type="png")
windows(height=12,width=12); par(mfcol=c(2,2),mar=c(3,4,2,1))
for (r in regA2ord) {
  cld <- dat[dat$regA2==r,]
  rwd <- pd1[pd1$regA2==r,]
  cle <- tapply(cld$hooks,factor(cld$yrqtr,levels=unique(c(rwd$yrqtr,cld$yrqtr))),sum)
  cle[is.na(cle)] <- 0
  rwe <- tapply(rwd$hooks,factor(rwd$yrqtr,levels=unique(c(rwd$yrqtr,cld$yrqtr))),sum)
  rwe[is.na(rwe)] <- 0
  plot(names(cle),cle/rwe[match(names(cle),names(rwe))],xlab="Year",ylab="Proportion of hooks",main=paste("ALB Region",r),ylim=c(0,1.1),xlim=range(prepdat$yrqtr))
  }
savePlot(filename="Cleaned effort proportion by region by yrqtr ALB2",type="png")

# Catch by region
windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1))
for (r in regYord) {
  llv <- dat[dat$regY==r,]
  yft <- tapply(llv$yft,llv$yrqtr,sum)
  bet <- tapply(llv$bet,llv$yrqtr,sum)
  alb <- tapply(llv$alb,llv$yrqtr,sum)
  sbt <- tapply(llv$sbt,llv$yrqtr,sum)
  maxy <- max(c(yft,bet,alb,sbt))
  plot(names(yft),yft,ylim=c(1,maxy),xlab="Year",ylab="Catch",main=paste("YFT Region",r),xlim=range(dat$yrqtr))
  points(names(bet),bet,col=2,pch=2)
  points(names(alb),alb,col=3,pch=3)
  if(r==1) legend("topleft",legend=c("Yellowfin","Bigeye","Albacore"),col=c(1,2,3),pch=c(1,2,3))
  }
savePlot(filename="Catch by region allsp by yrqtr 1",type="png")
windows(height=12,width=12); par(mfcol=c(2,2),mar=c(3,4,2,1))
for (r in regA2ord) {
  llv <- dat[dat$regA2==r,]
  yft <- tapply(llv$yft,llv$yrqtr,sum)
  bet <- tapply(llv$bet,llv$yrqtr,sum)
  alb <- tapply(llv$alb,llv$yrqtr,sum)
#  swo <- tapply(llv$swo,llv$yrqtr,sum)
  sbt <- tapply(llv$sbt,llv$yrqtr,sum)
  maxy <- max(c(yft,bet,alb,sbt))
  plot(names(yft),yft,ylim=c(1,maxy),xlab="Year",ylab="Catch",main=paste("ALB Region",r),xlim=range(dat$yrqtr))
  points(names(bet),bet,col=2,pch=2)
  points(names(alb),alb,col=3,pch=3)
  points(names(sbt),sbt,col=4,pch=4)
  if(r==1) legend("topleft",legend=c("Yellowfin","Bigeye","Albacore","Southern bluefin"),col=c(1,2,3,4),pch=c(1,2,3,4))
  }
savePlot(filename="Catch by region allsp by yrqtr 1 ALB2",type="png")

windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1))
for (r in regYord) {
  llv <- dat[dat$regY==r,]
  yft <- tapply(llv$yft,llv$yrqtr,sum)
  bet <- tapply(llv$bet,llv$yrqtr,sum)
  alb <- tapply(llv$alb,llv$yrqtr,sum)
#  swo <- tapply(llv$swo,llv$yrqtr,sum)
  sbt <- tapply(llv$sbt,llv$yrqtr,sum)
  maxy <- max(c(yft,bet,alb,sbt))
  plot(names(yft),yft+1,ylim=c(1,maxy),xlab="Year",ylab="Catch",main=paste("YFT Region",r),xlim=range(dat$yrqtr),log="y")
  points(names(bet),bet+1,col=2,pch=2)
  points(names(alb),alb+1,col=3,pch=3)
#  points(names(swo),swo+1,col=4,pch=4)
#  points(names(sbt),sbt+1,col=4,pch=4)
  if(r==1) legend("topleft",legend=c("Yellowfin","Bigeye","Albacore"),col=c(1,2,3),pch=c(1,2,3))
  }
savePlot(filename="Catch by region allsp by yrqtr log1",type="png")
windows(height=12,width=12); par(mfcol=c(2,2),mar=c(3,4,2,1))
for (r in regA2ord) {
  llv <- dat[dat$regA2==r,]
  yft <- tapply(llv$yft,llv$yrqtr,sum)
  bet <- tapply(llv$bet,llv$yrqtr,sum)
  alb <- tapply(llv$alb,llv$yrqtr,sum)
#  swo <- tapply(llv$swo,llv$yrqtr,sum)
  sbt <- tapply(llv$sbt,llv$yrqtr,sum)
  maxy <- max(c(yft,bet,alb,sbt))
  plot(names(yft),yft+1,ylim=c(1,maxy),xlab="Year",ylab="Catch",main=paste("YFT Region",r),xlim=range(dat$yrqtr),log="y")
  points(names(bet),bet+1,col=2,pch=2)
  points(names(alb),alb+1,col=3,pch=3)
#  points(names(swo),swo+1,col=4,pch=4)
  points(names(sbt),sbt+1,col=4,pch=4)
  if(r==1) legend("topleft",legend=c("Yellowfin","Bigeye","Albacore","Southern bluefin"),col=c(1,2,3,4),pch=c(1,2,3,4))
  }
savePlot(filename="Catch by region allsp by yrqtr log1 ALB2",type="png")

windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1))
for (r in regYord) {
  llv <- dat[dat$regY==r,]
  swo <- tapply(llv$swo,llv$yrqtr,sum)
  mls <- tapply(llv$mls,llv$yrqtr,sum)
  blm <- tapply(llv$blm,llv$yrqtr,sum)
  bum <- tapply(llv$bum,llv$yrqtr,sum)
  maxy <- max(c(swo,mls,blm,bum))
  if(r==3) maxy=maxy*10
  plot(names(swo),swo+1,ylim=c(1,maxy),xlab="Year",ylab="Catch",main=paste("YFT Region",r),xlim=range(dat$yrqtr),log="y")
  points(names(mls),mls+1,col=2,pch=2)
  points(names(blm),blm+1,col=3,pch=3)
  points(names(bum),bum+1,col=4,pch=4)
  if(r==3) legend("topleft",legend=c("Swordfish","Striped marlin","Black marlin","Blue marlin"),col=1:4,pch=1:4)
  }
savePlot(filename="Catch by region allsp by yrqtr log2",type="png")

windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1))
for (r in regYord) {
  llv <- dat[dat$regY==r,]
  swo <- tapply(llv$swo,llv$yrqtr,sum)
  mls <- tapply(llv$mls,llv$yrqtr,sum)
  blm <- tapply(llv$blm,llv$yrqtr,sum)
  bum <- tapply(llv$bum,llv$yrqtr,sum)
  maxy <- max(c(swo,mls,blm,bum))
#  if(r==3) maxy=maxy*2
  plot(names(swo),swo,ylim=c(1,maxy),xlab="Year",ylab="Catch",main=paste("YFT Region",r),xlim=range(dat$yrqtr))
  points(names(mls),mls,col=2,pch=2)
  points(names(blm),blm,col=3,pch=3)
  points(names(bum),bum,col=4,pch=4)
  if(r==2) legend("topleft",legend=c("Swordfish","Striped marlin","Black marlin","Blue marlin"),col=1:4,pch=1:4)
  }
savePlot(filename="Catch by region allsp by yrqtr 2",type="png")

### CPUE
windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1))
for (r in regYord) {
  llv <- dat[dat$regY==r,]
  eff <- tapply(llv$hooks,llv$yrqtr,sum)
  yft <- tapply(llv$yft,llv$yrqtr,sum)
  bet <- tapply(llv$bet,llv$yrqtr,sum)
  alb <- tapply(llv$alb,llv$yrqtr,sum)
  sbt <- tapply(llv$sbt,llv$yrqtr,sum)
  yft <- 100*yft/eff
  bet <- 100*bet/eff
  alb <- 100*alb/eff
  sbt <- 100*sbt/eff
  maxy <- max(c(yft,bet,alb,sbt))
#  if(r==1) maxy=maxy*100
  plot(names(yft),yft,ylim=c(1e-5,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("YFT Region",r),log="y",xlim=range(dat$yrqtr))
  points(names(bet),bet,col=2,pch=2)
  points(names(alb),alb,col=3,pch=3)
#  points(names(sbt),sbt,col=4,pch=4)
#  if(r==1) legend("bottomleft",legend=c("Yellowfin","Bigeye","Albacore","Southern Bluefin"),col=c(1,2,3,4),pch=c(1,2,3,4))
  if(r==1) legend("bottomleft",legend=c("Yellowfin","Bigeye","Albacore"),col=c(1,2,3),pch=c(1,2,3))
  }
savePlot(filename="CPUE by region allsp by yrqtr log1",type="png")
windows(height=12,width=12); par(mfcol=c(2,2),mar=c(3,4,2,1))
for (r in regA2ord) {
  llv <- dat[dat$regA2==r,]
  eff <- tapply(llv$hooks,llv$yrqtr,sum)
  yft <- tapply(llv$yft,llv$yrqtr,sum)
  bet <- tapply(llv$bet,llv$yrqtr,sum)
  alb <- tapply(llv$alb,llv$yrqtr,sum)
  sbt <- tapply(llv$sbt,llv$yrqtr,sum)
  yft <- 100*yft/eff
  bet <- 100*bet/eff
  alb <- 100*alb/eff
  sbt <- 100*sbt/eff
  maxy <- max(c(yft,bet,alb,sbt))
#  if(r==1) maxy=maxy*100
  plot(names(yft),yft,ylim=c(1e-5,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("ALB Region",r),log="y",xlim=range(dat$yrqtr))
  points(names(bet),bet,col=2,pch=2)
  points(names(alb),alb,col=3,pch=3)
  points(names(sbt),sbt,col=4,pch=4)
#  if(r==1) legend("bottomleft",legend=c("Yellowfin","Bigeye","Albacore","Southern Bluefin"),col=c(1,2,3,4),pch=c(1,2,3,4))
  if(r==2) legend("topleft",legend=c("Swordfish","Striped marlin","Black marlin","Blue marlin"),col=1:4,pch=1:4)
  }
savePlot(filename="CPUE by region allsp by yrqtr log1 ALB2",type="png")

windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1))
for (r in regYord) {
  llv <- dat[dat$regY==r,]
  eff <- tapply(llv$hooks,llv$yrqtr,sum)
  swo <- tapply(llv$swo,llv$yrqtr,sum)
  mls <- tapply(llv$mls,llv$yrqtr,sum)
  blm <- tapply(llv$blm,llv$yrqtr,sum)
  bum <- tapply(llv$bum,llv$yrqtr,sum)
  swo <- 100*swo/eff
  mls <- 100*mls/eff
  blm <- 100*blm/eff
  bum <- 100*bum/eff
  maxy <- max(c(swo,mls,blm,bum))
  plot(names(swo),swo,ylim=c(1e-5,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("YFT Region",r),log="y",xlim=range(dat$yrqtr))
  points(names(mls),mls,col=2,pch=2)
  points(names(blm),blm,col=3,pch=3)
  points(names(bum),bum,col=4,pch=4)
  if(r==1) legend("bottomleft",legend=c("Swordfish","Striped marlin","Black marlin","Blue marlin"),col=1:4,pch=1:4)
  }
savePlot(filename="CPUE by region allsp by yrqtr log2",type="png")
windows(height=12,width=12); par(mfcol=c(2,2),mar=c(3,4,2,1))
for (r in regA2ord) {
  llv <- dat[dat$regA2==r,]
  eff <- tapply(llv$hooks,llv$yrqtr,sum)
  swo <- tapply(llv$swo,llv$yrqtr,sum)
  mls <- tapply(llv$mls,llv$yrqtr,sum)
  blm <- tapply(llv$blm,llv$yrqtr,sum)
  bum <- tapply(llv$bum,llv$yrqtr,sum)
  swo <- 100*swo/eff
  mls <- 100*mls/eff
  blm <- 100*blm/eff
  bum <- 100*bum/eff
  maxy <- max(c(swo,mls,blm,bum))
  plot(names(swo),swo,ylim=c(1e-5,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("ALB Region",r),log="y",xlim=range(dat$yrqtr))
  points(names(mls),mls,col=2,pch=2)
  points(names(blm),blm,col=3,pch=3)
  points(names(bum),bum,col=4,pch=4)
  if(r==1) legend("bottomleft",legend=c("Swordfish","Striped marlin","Black marlin","Blue marlin"),col=1:4,pch=1:4)
  }
savePlot(filename="CPUE by region allsp by yrqtr log2 ALB2",type="png")

# Nominal CPUE - not logged
windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1))
for (r in regYord) {
  llv <- dat[dat$regY==r,]
  eff <- tapply(llv$hooks,llv$yrqtr,sum)
  yft <- tapply(llv$yft,llv$yrqtr,sum)
  bet <- tapply(llv$bet,llv$yrqtr,sum)
  alb <- tapply(llv$alb,llv$yrqtr,sum)
  sbt <- tapply(llv$sbt,llv$yrqtr,sum)
  yft <- 100*yft/eff
  bet <- 100*bet/eff
  alb <- 100*alb/eff
  sbt <- 100*sbt/eff
  maxy <- max(c(yft,bet,alb,sbt))
#  if(r==1) maxy=maxy*100
  plot(names(yft),yft,ylim=c(1e-5,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("YFT Region",r),xlim=range(dat$yrqtr))
  points(names(bet),bet,col=2,pch=2)
  points(names(alb),alb,col=3,pch=3)
#  points(names(sbt),sbt,col=4,pch=4)
#  if(r==3) legend("topright",legend=c("Yellowfin","Bigeye","Albacore","Southern Bluefin"),col=c(1,2,3,4),pch=c(1,2,3,4))
  if(r==3) legend("topright",legend=c("Yellowfin","Bigeye","Albacore"),col=c(1,2,3),pch=c(1,2,3))
  }
savePlot(filename="CPUE by region allsp by yrqtr 1",type="png")

windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1))
for (r in regYord) {
  llv <- dat[dat$regY==r,]
  eff <- tapply(llv$hooks,llv$yrqtr,sum)
  swo <- tapply(llv$swo,llv$yrqtr,sum)
  mls <- tapply(llv$mls,llv$yrqtr,sum)
  blm <- tapply(llv$blm,llv$yrqtr,sum)
  bum <- tapply(llv$bum,llv$yrqtr,sum)
  swo <- 100*swo/eff
  mls <- 100*mls/eff
  blm <- 100*blm/eff
  bum <- 100*bum/eff
  maxy <- max(c(swo,mls,blm,bum))
  plot(names(swo),swo,ylim=c(1e-5,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("YFT Region",r),xlim=range(dat$yrqtr))
  points(names(mls),mls,col=2,pch=2)
  points(names(blm),blm,col=3,pch=3)
  points(names(bum),bum,col=4,pch=4)
  if(r==3) legend("topright",legend=c("Swordfish","Striped marlin","Black marlin","Blue marlin"),col=1:4,pch=1:4)
  }
savePlot(filename="CPUE by region allsp by yrqtr 2",type="png")


# 5 degree squares fished
dimu <- function(x) { length(unique(x)) }
windows(height=14,width=12)
par (mfcol=c(3,2),mar=c(3,4,2,1))
for (r in regYord) {
  llv <- dat[dat$regY == r,]
  yq <- sort(unique(llv$yrqtr))
  strats <- tapply(paste(llv$lat5,llv$lon5),llv$yrqtr,dimu)
  plot(yq, strats, type="p", xlim=range(dat$yrqtr),pch=1,col=1,ylim=c(0,max(strats)),
           cex=1,ylab="5 x 5 spatial strata with reported effort",main=paste("YFT Region",r))
#  mtext(side=3, paste("YFT Region", r),line=0.5)
  }
savePlot("Number of spatial strata",type="png")
windows(height=12,width=12)
par (mfcol=c(2,2),mar=c(3,4,2,1))
for (r in regA2ord) {
  llv <- dat[dat$regA2 == r,]
  yq <- sort(unique(llv$yrqtr))
  strats <- tapply(paste(llv$lat5,llv$lon5),llv$yrqtr,dimu)
  plot(yq, strats, type="p", xlim=range(dat$yrqtr),pch=1,col=1,ylim=c(0,max(strats)),
           cex=1,ylab="5 x 5 spatial strata with reported effort",main=paste("ALB Region",r))
#  mtext(side=3, paste("YFT Region", r),line=0.5)
  }
savePlot("Number of spatial strata ALB2",type="png")

# Proportion sets with zero catches
windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1))
for (r in regYord) {
  llv <- dat[dat$regY==r,]
  ay <- tapply(llv$yft==0,llv$yrqtr,sum)
  a <- tapply(llv$hooks,llv$yrqtr,length)
  ab <- tapply(llv$bet==0,llv$yrqtr,sum)
  ay <- 100*ay/a
  ab <- 100*ab/a
  maxy <- max(c(ay,ab))
  plot(names(ay),ay,xlim=range(dat$yrqtr),ylim=c(0,100),xlab="Year",ylab="Proportion of zero catches",main=paste("YFT Region",r))
  points(names(ab),ab,col=2,pch=2)
  if(r==6) legend("topright",legend=c("Yellowfin","Bigeye"),col=c(1,2),pch=c(1,2))
  }
savePlot(filename="Proportion zeroes by region by yrqtr",type="png")

# Proportion sets with zero catches allspp
windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1))
for (r in regYord) {
  llv <- dat[dat$regY==r,]
  ay <- tapply(llv$yft==0,llv$yrqtr,sum)
  a <- tapply(llv$hooks,llv$yrqtr,length)
  ab <- tapply(llv$bet==0,llv$yrqtr,sum)
  aalb <- tapply(llv$alb==0,llv$yrqtr,sum)
  asbt <- tapply(llv$sbt==0,llv$yrqtr,sum)
  ay <- 100*ay/a
  ab <- 100*ab/a
  aalb <- 100*aalb/a
  asbt <- 100*asbt/a
  maxy <- max(c(ay,ab,asbt,aalb))
  plot(names(ay),ay,xlim=range(dat$yrqtr),ylim=c(0,100),xlab="Year",ylab="Proportion of zero catches",main=paste("YFT Region",r))
  points(names(ab),ab,col=2,pch=2)
  points(names(aalb),aalb,col=3,pch=3)
#  points(names(asbt),asbt,col=4,pch=4)
#  if(r==6) legend("topleft",legend=c("Yellowfin","Bigeye","Albacore","Southern bluefin"),col=c(1,2,3,4),pch=c(1,2,3,4))
  if(r==6) legend("topleft",legend=c("Yellowfin","Bigeye","Albacore"),col=c(1,2,3),pch=c(1,2,3))
    }
savePlot(filename="Proportion zeroes by region by yrqtr allspp",type="png")
windows(height=12,width=12); par(mfcol=c(2,2),mar=c(3,4,2,1))
for (r in regA2ord) {
  llv <- dat[dat$regA2==r,]
  ay <- tapply(llv$yft==0,llv$yrqtr,sum)
  a <- tapply(llv$hooks,llv$yrqtr,length)
  ab <- tapply(llv$bet==0,llv$yrqtr,sum)
  aalb <- tapply(llv$alb==0,llv$yrqtr,sum)
  asbt <- tapply(llv$sbt==0,llv$yrqtr,sum)
  ay <- 100*ay/a
  ab <- 100*ab/a
  aalb <- 100*aalb/a
  asbt <- 100*asbt/a
  maxy <- max(c(ay,ab,asbt,aalb))
  plot(names(ay),ay,xlim=range(dat$yrqtr),ylim=c(0,100),xlab="Year",ylab="Proportion of zero catches",main=paste("ALB Region",r))
  points(names(ab),ab,col=2,pch=2)
  points(names(aalb),aalb,col=3,pch=3)
  points(names(asbt),asbt,col=4,pch=4)
  if(r==6) legend("topleft",legend=c("Yellowfin","Bigeye","Albacore","Southern bluefin"),col=c(1,2,3,4),pch=c(1,2,3,4))
#  if(r==6) legend("topleft",legend=c("Yellowfin","Bigeye","Albacore"),col=c(1,2,3),pch=c(1,2,3))
    }
savePlot(filename="Proportion zeroes by region by yrqtr allspp ALB2",type="png")

windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1))
for (r in regYord) {
  llv <- dat[dat$regY==r,]
  aswo <- tapply(llv$swo==0,llv$yrqtr,sum)
  a <- tapply(llv$hooks,llv$yrqtr,length)
  amls <- tapply(llv$mls==0,llv$yrqtr,sum)
  ablm <- tapply(llv$blm==0,llv$yrqtr,sum)
  abum <- tapply(llv$bum==0,llv$yrqtr,sum)
  aswo <- 100*aswo/a
  amls <- 100*amls/a
  ablm <- 100*ablm/a
  abum <- 100*abum/a
  maxy <- max(c(aswo,amls,abum,ablm))
  plot(names(aswo),aswo,xlim=range(dat$yrqtr),ylim=c(0,100),xlab="Year",ylab="Proportion of zero catches",main=paste("YFT Region",r))
  points(names(amls),amls,col=2,pch=2)
  points(names(ablm),ablm,col=3,pch=3)
  points(names(abum),abum,col=4,pch=4)
  if(r==4) legend("bottomright",legend=c("Swordfish","Striped marlin","Blue marlin","Black marlin"),col=1:4,pch=1:4)
    }
savePlot(filename="Proportion zeroes by region by yrqtr spp2",type="png")
windows(height=12,width=12); par(mfcol=c(2,2),mar=c(3,4,2,1))
for (r in regA2ord) {
  llv <- dat[dat$regA2==r,]
  aswo <- tapply(llv$swo==0,llv$yrqtr,sum)
  a <- tapply(llv$hooks,llv$yrqtr,length)
  amls <- tapply(llv$mls==0,llv$yrqtr,sum)
  ablm <- tapply(llv$blm==0,llv$yrqtr,sum)
  abum <- tapply(llv$bum==0,llv$yrqtr,sum)
  aswo <- 100*aswo/a
  amls <- 100*amls/a
  ablm <- 100*ablm/a
  abum <- 100*abum/a
  maxy <- max(c(aswo,amls,abum,ablm))
  plot(names(aswo),aswo,xlim=range(dat$yrqtr),ylim=c(0,100),xlab="Year",ylab="Proportion of zero catches",main=paste("ALB Region",r))
  points(names(amls),amls,col=2,pch=2)
  points(names(ablm),ablm,col=3,pch=3)
  points(names(abum),abum,col=4,pch=4)
  if(r==4) legend("bottomright",legend=c("Swordfish","Striped marlin","Blue marlin","Black marlin"),col=1:4,pch=1:4)
    }
savePlot(filename="Proportion zeroes by region by yrqtr spp2 ALB2",type="png")


# Maps of effort through time
library(maps)
#install.packages("mapproj")
library("mapproj")
library(mapdata)
windows(width=16,height=20); par(mfrow=c(5,3),mar=c(2,2,2,0))
for(yr in seq(1950,2015,by=5)) {
  plot_IO(plot_title=yr,sp="YFT")
  a <- dat[dat$yrqtr > yr & dat$yrqtr < yr+5,]
  lats <- sort(unique(a$lat5))
  lons <- sort(unique(a$lon5))
  a <- table(a$lat5,a$lon5)
  for (i in 1:length(lats)) {
    symbols(lons,rep(lats[i],length(lons)),circles=sqrt(a[i,])/20,col=1,add = T, inches =F)
    }
  }
savePlot("Plot map sets", type="png")

# Maps of mean HPB through time
windows(width=16,height=20); par(mfrow=c(5,3),mar=c(2,2,2,1))
for(yr in seq(1955,2015,by=5)) {
  plot_IO(plot_title=yr,sp="YFT")
  a <- dat[dat$yrqtr > yr & dat$yrqtr < yr+5,]
  lats <- sort(unique(a$lat5))
  lons <- sort(unique(a$lon5))
  a <- tapply(a$hbf,list(a$lat5,a$lon5),mean,na.rm=T)
  a[is.nan(a)] <- NA
  for (i in 1:length(lats)) {
    text(lons+2.5,rep(lats[i],length(lons))+2.5,floor(a[i,]),col=1,add = T,cex=0.9)
    }
  }
savePlot("Plot map mean HBF", type="png")


# Maps of median HPB through time
windows(width=16,height=20); par(mfrow=c(5,3),mar=c(2,2,2,1))
for(yr in seq(1955,2015,by=5)) {
  plot_IO(plot_title=yr,sp="YFT")
  a <- dat[dat$yrqtr > yr & dat$yrqtr < yr+5,]
  lats <- sort(unique(a$lat5))
  lons <- sort(unique(a$lon5))
  a <- tapply(round(a$hbf,0),list(a$lat5,a$lon5),median,na.rm=T)
  for (i in 1:length(lats)) {
    text(lons+2.5,rep(lats[i],length(lons))+2.5,(a[i,]),col=1,cex=.9)
    }
  }
savePlot("Plot map median HBF", type="png")

# Plot hbf by region by year
windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,4,2,1),oma=c(1,1,3,1)); r<-3; hh <- 18
  b<- dat
  for (r in regYord) {
    a <- b[b$regY==r,]
    yrs <- sort(unique(a$op_yr))
    a <- table(floor(a$hbf),a$op_yr)
    plot(1,1, type="n", xlab="Year", ylab="HBF", ylim = c(1,25), xlim=c(1952,2017),main=paste("YFT Region",r))
    ilist <- as.numeric(row.names(a))
    for(i in 1:length(ilist)){
      symbols(yrs , rep(ilist[i], length(yrs)), sqrt(a[i,]) / max(sqrt(a)), add = T, inches =FALSE)
      if(trunc(i/5) == i/5){
      lines(c(1952,2014), rep(i,2), lty=3)}
      }
    }
  savePlot(paste("plot hbf by region by yr"),type="png")

# Proportion zero by HBF by region by year
countzero <- function(a) {
  z <- sum(a==0)
  tot <- length(a)
  pz <- z/tot ; return(pz)
  }

windows(height=14,width=17); par(mfcol=c(3,2),mar=c(4,4,2,2))
for (r in regYord) {
  a <- dat[dat$regY==r,]
  yrs <- sort(unique(a$op_yr))
  a <- tapply(a$bet,list(a$hbf,a$op_yr),countzero)
  plot(1,1, type="n", xlab="Year", ylab="HBF", ylim = c(1,25), xlim=range(dat$yrqtr),main=paste("YFT Region",r))
  ilist <- as.numeric(row.names(a))
  for(i in 1:length(ilist)){
    symbols(yrs , rep(ilist[i], length(yrs)), .8*sqrt(a[i,]), add = T, inches =FALSE)
    }
  }
savePlot("plot pzero bet by hbf by region by yr",type="png")

windows(height=14,width=17); par(mfcol=c(3,2),mar=c(4,4,2,2))
for (r in regYord) {
  a <- dat[dat$regY==r,]
  yrs <- sort(unique(a$op_yr))
  a <- tapply(a$yft,list(a$hbf,a$op_yr),countzero)
  plot(1,1, type="n", xlab="Year", ylab="HBF", ylim = c(1,25), xlim=range(dat$yrqtr),main=paste("YFT Region",r))
  ilist <- as.numeric(row.names(a))
  for(i in 1:length(ilist)){
    symbols(yrs , rep(ilist[i], length(yrs)), .8*sqrt(a[i,]), add = T, inches =FALSE)
    }
  }
savePlot("plot pzero yft by hbf by region by yr",type="png")

windows(height=14,width=17); par(mfcol=c(3,2),mar=c(4,4,2,2))
for (r in regYord) {
  a <- dat[dat$regY==r,]
  yrs <- sort(unique(a$op_yr))
  a <- tapply(a$alb,list(a$hbf,a$op_yr),countzero)
  plot(1,1, type="n", xlab="Year", ylab="HBF", ylim = c(1,25), xlim=range(dat$yrqtr),main=paste("YFT Region",r))
  ilist <- as.numeric(row.names(a))
  for(i in 1:length(ilist)){
    symbols(yrs , rep(ilist[i], length(yrs)), .8*sqrt(a[i,]), add = T, inches =FALSE)
    }
  }
savePlot("plot pzero alb by hbf by region by yr",type="png")

windows(height=14,width=17); par(mfcol=c(3,2),mar=c(4,4,2,2))
for (r in regYord) {
  a <- dat[dat$regY==r,]
  yrs <- sort(unique(a$op_yr))
  a <- tapply(a$swo,list(a$hbf,a$op_yr),countzero)
  plot(1,1, type="n", xlab="Year", ylab="HBF", ylim = c(1,25), xlim=range(dat$yrqtr),main=paste("YFT Region",r))
  ilist <- as.numeric(row.names(a))
  for(i in 1:length(ilist)){
    symbols(yrs , rep(ilist[i], length(yrs)), .8*sqrt(a[i,]), add = T, inches =FALSE)
    }
  }
savePlot("plot pzero swo by hbf by region by yr",type="png")

# Proportion of zero catches by HBF and year
plot_pzero_ll <- function(a,sp,la,lo) {
 # if(la == -30) browser()
  yrs <- sort(unique(a$op_yr))
  a <- tapply(a[,sp],list(a$hbf,a$op_yr),countzero)
  plot(1,1, type="n", xlab="Year", ylab="HBF", ylim = c(1,24), xlim=c(1955,2014),main=paste(la+2.5,lo+2.5,sep=", "))
  ilist <- as.numeric(row.names(a))
  if(length(ilist) > 0) {
    for(i in 1:length(ilist)){
      symbols(yrs , rep(ilist[i], length(yrs)), sqrt(a[i,]), add = T, inches =FALSE)
      }
    }
  }

d2 <- data.frame(dat[dat$lon5 < 75 & dat$lat5 > -20 & dat$lat5 <= 5,])
for(sp in c("alb","bet","blm","bum","mls","sbt","swo","yft"))
  {
  windows(height=14,width=24); par(mfrow=c(5,7),mar=c(2,2,1,0),oma=c(0,0,3,0))
  for (la in seq(5,-15,by=-5)) {
    for (lo in seq(40,70,by=5)) {
      a <- d2[d2$lat5==la & d2$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_pzero_ll(a,sp,la,lo) else plot(1,1,type="n")
      }
    }
  title(sp,outer=T,line=1)
  savePlot(paste("plot pzero",sp,"by hbf by R2 latlong2"),type="png")
}

d2 <- data.frame(dat[dat$lon5 >= 75 & dat$lat5 > -20 & dat$lat5 <= 5,])
for(sp in c("alb","bet","blm","bum","mls","sbt","swo","yft"))
  {
  windows(height=14,width=24); par(mfrow=c(5,8),mar=c(2,2,1,0),oma=c(0,0,3,0))
  for (la in seq(5,-15,by=-5)) {
    for (lo in seq(75,110,by=5)) {
      a <- d2[d2$lat5==la & d2$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_pzero_ll(a,sp,la,lo) else plot(1,1,type="n")
      }
    }
  title(sp,outer=T,line=1)
  savePlot(paste("plot pzero",sp,"by hbf by R5 latlong2"),type="png")
}


#R 1,2,5,6
pzero_hbf_sp_yq <- function(sp,laseq,loseq,fname,ti="") {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- data.frame(dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,])
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_pzero_ll(a,sp,la,lo) else plot(1,1,type="n")
      }
    }
  title(ti,outer=T)
  savePlot(fname,type="png")
  }
pzero_hbf_sp_yq("bet",laseq=seq(5,-15,-5),loseq=seq(40,70,5),fname="plot pzero bet by hbf by R2 latlong",ti="Bigeye Region 1")
pzero_hbf_sp_yq("yft",laseq=seq(5,-15,-5),loseq=seq(40,70,5),fname="plot pzero yft by hbf by R2 latlong",ti="Yellowfin Region 1")
pzero_hbf_sp_yq("alb",laseq=seq(5,-15,-5),loseq=seq(40,70,5),fname="plot pzero alb by hbf by R2 latlong",ti="Albacore Region 1")


#R 1,2,3,4,5,6
#load("alldatraw.RData")
#dat <- dataclean(dat,allHBF=T)
#dat <- dataprep(dat)
#dat <- dat[dat$regY > 0 & dat$regY <=6,c("tonnage","fishingcat","ncrew","target","mainline","branchline","yr","dat_mon","dat_day","lat","lon","hbf",
#          "hooks","bet","yft","alb","lat5","lon5","regY","subreg","vessid","yrqtr","latlong","cstart_yr","cstart_mon","cstart_day")]

#plot_hbf_5x5 <- function(a,la,lo) {
#  yrs <- sort(unique(a$yr))
#  a <- tapply(a$hooks,list(a$hbf,a$yr),sum)
#  plot(1,1, type="n", xlab="Year", ylab="HBF", ylim = c(1,25), xlim=c(1955, 2010),main=paste(la+2.5,lo+2.5,sep=", "))
#  ilist <- as.numeric(row.names(a))
#  if(length(ilist) > 0) {
#    for(i in 1:length(ilist)){
#      symbols(yrs , rep(ilist[i], length(yrs)), sqrt(a[i,])/400, add = T, inches =F)
#    }
#  }
#}
#hbf_sp_yq <- function(sp,laseq,loseq,fname,ti="") {
#  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
#  b <- dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,]
#  for (la in laseq) {
#    for (lo in loseq) {
#      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
#      if(dim(a)[[1]] >0)  plot_hbf_5x5(a,la,lo) else plot(1,1,type="n")
#      }
#    }
#  title(ti,outer=T)
#  savePlot(fname,type="png")
#  }
#hbf_sp_yq(laseq=seq(5,-15,-5),loseq=seq(40,70,5),fname="plot hbf by R2 latlong",ti="HBF Region 2")
#
#load("dat.RData")

# Proportion of zero catches
plot_pzero_both <- function(a,la,lo) {
  cx <- 0.8
  yrs <- sort(unique(a$yrqtr))
  alb <- tapply(a$alb,list(a$yrqtr),countzero)
  yft <- tapply(a$yft,list(a$yrqtr),countzero)
  bet <- tapply(a$bet,list(a$yrqtr),countzero)
  sbt <- tapply(a$sbt,list(a$yrqtr),countzero)
  plot(1,1, type="n", xlab="yr", ylab="p(zero catch)", ylim = c(0,1), xlim=range(dat$yrqtr),main=paste(la,", ",lo,sep=""))
  points(yrs, yft,cex=cx)
  points(yrs,bet,col=2,pch=2,cex=cx)
  points(yrs,alb,col=3,pch=3,cex=cx)
#  points(yrs,sbt,col=4,pch=4,cex=cx)
  }
pzero_all_sp_yq <- function(laseq,loseq,fname,ti="") {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- data.frame(dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,])
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_pzero_both(a,la,lo) else plot(1,1,type="n",ylim=c(0,1))
      if(la==laseq[1] & lo==loseq[1]) legend("topright",legend=c("YFT","BET","ALB"),col=1:3,pch=1:3)
      }
    }
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

pzero_all_sp_yq(laseq=seq(20,10,-5)+2.5,loseq=seq(45,75,5)+2.5,fname="plot pzero YBA by R1 latlong",ti="Probability of zero catch Region 1")
pzero_all_sp_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(45,75,5)+2.5,fname="plot pzero YBA by R2 latlong",ti="Probability of zero catch Region 2")
pzero_all_sp_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(75,110,5)+2.5,fname="plot pzero YBA by R5 latlong",ti="Probability of zero catch Region 5")
pzero_all_sp_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(20,60,5)+2.5,fname="plot pzero YBA by R3 latlong",ti="Probability of zero catch Region 3")
pzero_all_sp_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(65,120,5)+2.5,fname="plot pzero YBA by R4 latlong",ti="Probability of zero catch Region 4")
pzero_all_sp_yq(laseq=seq(20,10,-5)+2.5,loseq=seq(75,100,5)+2.5,fname="plot pzero YBA by R6 latlong",ti="Probability of zero catch Region 6")

plot_pzero_billf <- function(a,la,lo) {
  cx <- 0.8
  yrs <- sort(unique(a$yrqtr))
  swo <- tapply(a$swo,list(a$yrqtr),countzero)
  mls <- tapply(a$mls,list(a$yrqtr),countzero)
  blm <- tapply(a$blm,list(a$yrqtr),countzero)
  bum <- tapply(a$bum,list(a$yrqtr),countzero)
  plot(1,1, type="n", xlab="yr", ylab="p(zero catch)", ylim = c(0,1), xlim=range(dat$yrqtr),main=paste(la+2.5,", ",lo+2.5,sep=""))
  points(yrs, swo,cex=cx)
  points(yrs,mls,col=2,pch=2,cex=cx)
  points(yrs,blm,col=3,pch=3,cex=cx)
  points(yrs,bum,col=4,pch=4,cex=cx)
#  points(yrs,swo,col=4,pch=4,cex=cx)
  }
pzero_billf_yq <- function(laseq,loseq,fname,ti="") {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- data.frame(dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,])
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_pzero_billf(a,la,lo) else plot(1,1,type="n",ylim=c(0,1))
      if(la==laseq[1] & lo==loseq[1]) legend("topright",legend=c("SWO","MLS","BLM","BUM"),col=1:4,pch=1:4)
      }
    }
  title(ti,outer=T)
  savePlot(fname,type="png")
  }
pzero_billf_yq(laseq=seq(20,10,-5)+2.5,loseq=seq(45,70,5)+2.5,fname="plot pzero billf by R1 latlong",ti="Probability of zero catch Region 1")
pzero_billf_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(40,70,5)+2.5,fname="plot pzero billf by R2 latlong",ti="Probability of zero catch Region 2")
pzero_billf_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(75,110,5)+2.5,fname="plot pzero billf by R5 latlong",ti="Probability of zero catch Region 5")
pzero_billf_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(20,60,5)+2.5,fname="plot pzero billf by R3 latlong",ti="Probability of zero catch Region 3")
pzero_billf_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(65,120,5)+2.5,fname="plot pzero billf by R4 latlong",ti="Probability of zero catch Region 4")
pzero_billf_yq(laseq=seq(20,10,-5)+2.5,loseq=seq(75,100,5)+2.5,fname="plot pzero billf by R6 latlong",ti="Probability of zero catch Region 6")


# Albacore catch distribution histograms
write.csv(table(dat$hbf,floor(dat$yrqtr/5)*5,dat$regY),file="hbf by region by 5 years.csv")
windows();par(mfcol=c(3,2))
for(i in regYord) {
  a <- dat[dat$regY==i,]
  hist(a$alb,breaks=c(seq(0,1000,1)),xlab="Albacore catch",xlim=c(0,100),main=paste("YFT Region",i))
}
savePlot("hist alb by regY",type="png")
write.csv(table(dat$hbf,floor(dat$yrqtr/5)*5,dat$regA2),file="hbf by region by 5 years ALB2.csv")
windows();par(mfcol=c(2,2))
for(i in regA2ord) {
  a <- dat[dat$regA2==i,]
  hist(a$alb,breaks=c(seq(0,1000,1)),xlab="Albacore catch",xlim=c(0,100),main=paste("ALB Region",i))
}
savePlot("hist alb by regA2 ALB2",type="png")


# Proportions of CPUE over 20 per 1000 hooks
count_over_n <- function(a,n) {
  z <- sum(a>n)
  tot <- length(a)
  pz <- z/tot ; return(pz)
  }
plot_pcpue <- function(a,la,lo,cpue) {
  cx<-0.9
  yrs <- sort(unique(a$yrqtr))
  alb <- tapply(a$alb/a$hooks,list(a$yrqtr),count_over_n,cpue/1000)
  yft <- tapply(a$yft/a$hooks,list(a$yrqtr),count_over_n,cpue/1000)
  bet <- tapply(a$bet/a$hooks,list(a$yrqtr),count_over_n,cpue/1000)
  plot(1,1, type="n", xlab="Year", ylab="p(zero catch)", ylim = c(0,1), xlim=range(dat$yrqtr),main=paste(la+2.5,lo+2.5,sep=", "))
  points(yrs,yft,col=1,pch=1,cex=cx)
  points(yrs,bet,col=2,pch=2,cex=cx)
  points(yrs, alb,col=3,pch=3,cex=cx)
  }
pcpue_all_sp_yq <- function(cpue,laseq,loseq,fname,ti="") {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,]
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_pcpue(a,la,lo,cpue) else plot(1,1,type="n",ylim=c(0,1))
      }
    }
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

pcpue_all_sp_yq(cpue=10,laseq=seq(20,10,-5)+2.5,loseq=seq(45,70,5)+2.5,fname="plot pcpue allsp by R1 latlong",ti="Probability of cpue > 1/100 hooks Region 1")
pcpue_all_sp_yq(cpue=10,laseq=seq(5,-15,-5)+2.5,loseq=seq(40,70,5)+2.5,fname="plot pcpue allsp by R2 latlong",ti="Probability of cpue > 1/100 hooks Region 2")
pcpue_all_sp_yq(cpue=10,laseq=seq(5,-15,-5)+2.5,loseq=seq(75,110,5)+2.5,fname="plot pcpue allsp by R5 latlong",ti="Probability of cpue > 1/100 hooks Region 5")
pcpue_all_sp_yq(cpue=10,laseq=seq(-20,-40,-5)+2.5,loseq=seq(20,60,5)+2.5,fname="plot pcpue allsp by R3 latlong",ti="Probability of cpue > 1/100 hooks Region 3")
pcpue_all_sp_yq(cpue=10,laseq=seq(-20,-40,-5)+2.5,loseq=seq(65,120,5)+2.5,fname="plot pcpue allsp by R4 latlong",ti="Probability of cpue > 1/100 hooks Region 4")
pcpue_all_sp_yq(cpue=10,laseq=seq(20,10,-5)+2.5,loseq=seq(75,100,5)+2.5,fname="plot pcpue allsp by R6 latlong",ti="Probability of cpue > 1/100 hooks Region 6")

# Catch
plot_catch <- function(a,la,lo,ymax) {
  cx <- 0.9
  yrs <- sort(unique(a$op_yr))
  yft <- tapply(a$yft,list(a$op_yr),sum)
  bet <- tapply(a$bet,list(a$op_yr),sum)
  alb <- tapply(a$alb,list(a$op_yr),sum)
  sbt <- tapply(a$sbt,list(a$op_yr),sum)
  plot(1,1, type="n", xlab="Year", ylab="Catch (numbers)", ylim = c(0,ymax), xlim=range(dat$yrqtr),main=paste(la,lo,sep=", "))
  lines(yrs,yft,col=1,cex=cx)
  lines(yrs,bet,col=2,lty=2,cex=cx)
  lines(yrs,alb,col=3,lty=3,cex=cx)
#  lines(yrs,sbt,col=4,lty=4,cex=cx)
  }
catch_all_sp_yq <- function(laseq,loseq,fname,ti="",ymax) {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,]
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_catch(a,la,lo,ymax) else plot(1,1,type="n")
      }
    }
#  legend("topright",legend=c("Yellowfin","Bigeye","Albacore","Southern bluefin"),lty=c(1:3),col=c(1:3))
  legend("topright",legend=c("Yellowfin","Bigeye","Albacore"),lty=c(1:3),col=c(1:3))
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

catch_all_sp_yq(laseq=seq(20,10,-5)+2.5,loseq=seq(45,70,5)+2.5,ymax=100000,fname="plot catch YBA by R1 latlong",ti="CatchRegion 1")
catch_all_sp_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(40,70,5)+2.5,ymax=100000,fname="plot catch YBA by R2 latlong",ti="Catch Region 2")
catch_all_sp_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(75,110,5)+2.5,ymax=50000,fname="plot catch YBA by R5 latlong",ti="Catch Region 5")
catch_all_sp_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(20,60,5)+2.5,ymax=80000,fname="plot catch YBA by R3 latlong",ti="Catch Region 3")
catch_all_sp_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(65,120,5)+2.5,ymax=60000,fname="plot catch YBA by R4 latlong",ti="Catch Region 4")
catch_all_sp_yq(laseq=seq(20,10,-5)+2.5,loseq=seq(75,100,5)+2.5,ymax=60000,fname="plot catch YBA by R6 latlong",ti="Catch Region 6")

plot_catch <- function(a,la,lo,ymax) {
  cx <- 0.9
  yrs <- sort(unique(a$op_yr))
  swo <- tapply(a$swo,list(a$op_yr),sum)
  mls <- tapply(a$mls,list(a$op_yr),sum)
  blm <- tapply(a$blm,list(a$op_yr),sum)
  bum <- tapply(a$bum,list(a$op_yr),sum)
  plot(1,1, type="n", xlab="Year", ylab="Catch (numbers)", ylim = c(0,ymax), xlim=range(dat$yrqtr),main=paste(la+2.5,lo+2.5,sep=", "))
  lines(yrs,swo,col=1,cex=cx)
  lines(yrs,mls,col=2,lty=2,cex=cx)
  lines(yrs,blm,col=3,lty=3,cex=cx)
  lines(yrs,bum,col=4,lty=4,cex=cx)
  }
catch_all_sp_yq <- function(laseq,loseq,fname,ti="",ymax) {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- data.frame(dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,])
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_catch(a,la,lo,ymax) else plot(1,1,type="n")
      }
    }
  legend("topright",legend=c("SWO","MLS","BLM","BUM"),lty=c(1:4),col=c(1:4))
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

catch_all_sp_yq(laseq=seq(20,10,-5)+2.5,loseq=seq(45,70,5)+2.5,ymax=10000,fname="plot catch billf by R1 latlong",ti="CatchRegion 1")
catch_all_sp_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(40,70,5)+2.5,ymax=10000,fname="plot catch billf by R2 latlong",ti="Catch Region 2")
catch_all_sp_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(75,110,5)+2.5,ymax=10000,fname="plot catch billf by R5 latlong",ti="Catch Region 5")
catch_all_sp_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(20,60,5)+2.5,ymax=10000,fname="plot catch billf by R3 latlong",ti="Catch Region 3")
catch_all_sp_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(65,120,5)+2.5,ymax=10000,fname="plot catch billf by R4 latlong",ti="Catch Region 4")
catch_all_sp_yq(laseq=seq(20,10,-5)+2.5,loseq=seq(75,100,5)+2.5,ymax=10000,fname="plot catch billf by R6 latlong",ti="Catch Region 6")


plot_effort <- function(a,la,lo,ymax) {
  cx <- 0.9
  yrs <- sort(unique(a$op_yr))
  effort <- tapply(a$hooks,list(a$op_yr),sum)
  plot(1,1, type="n", xlab="Year", ylab="Catch (numbers)", ylim = c(0,ymax), xlim=range(dat$yrqtr),main=paste(la+2.5,lo+2.5,sep=", "))
  lines(yrs,effort,col=1,cex=cx)
  }
effort_all_fc_yq <- function(laseq,loseq,fname,ti="",ymax) {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- data.frame(dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,])
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_effort(a,la,lo,ymax) else plot(1,1,type="n")
      }
    }
#  legend("topright",legend=c("Offshore","Distant water"),lty=c(1,2),col=c(1,2))
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

effort_all_fc_yq(laseq=seq(20,10,-5)+2.5,loseq=seq(45,70,5)+2.5,ymax=10000000,fname="plot effort by R1 latlong",ti="Effort Region 1")
effort_all_fc_yq(laseq=seq(5,-15,-5+2.5),loseq=seq(40,70,5)+2.5,ymax=20000000,fname="plot effort by R2 latlong",ti="Effort Region 2")
effort_all_fc_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(75,110,5)+2.5,ymax=10000000,fname="plot effort by R5 latlong",ti="Effort Region 5")
effort_all_fc_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(20,60,5)+2.5,ymax=10000000,fname="plot effort by R3 latlong",ti="Effort Region 3")
effort_all_fc_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(65,120,5)+2.5,ymax=10000000,fname="plot effort by R4 latlong",ti="Effort Region 4")
effort_all_fc_yq(laseq=seq(20,10,-5)+2.5,loseq=seq(75,100,5)+2.5,ymax=10000000,fname="plot effort by R6 latlong",ti="Effort Region 6")


# Median CPUE
minmaxy <- function(a,maxy) min(a,maxy,na.rm=T)
plot_median_cpue <- function(a,la,lo) {
  cx=0.9
  yrs <- sort(unique(a$yrqtr))
  alb <- tapply(a$alb/a$hooks,list(a$yrqtr),median)
  yft <- tapply(a$yft/a$hooks,list(a$yrqtr),median)
  bet <- tapply(a$bet/a$hooks,list(a$yrqtr),median)
  maxy=0.03
  plot(1,1, type="n", xlab="Year", ylab="Median CPUE", ylim = c(0,maxy), xlim=range(dat$yrqtr),main=paste(la+2.5,lo+2.5,sep=", "))
  points(yrs,sapply(yft,minmaxy,maxy),col=1,pch=1,cex=cx)
  points(yrs,sapply(bet,minmaxy,maxy),col=2,pch=2,cex=cx)
  points(yrs,sapply(alb,minmaxy,maxy),col=3,pch=3,cex=cx)
  }
median_all_sp_yq_fc <- function(cpue,laseq,loseq,fname,ti="",fc="both") {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- data.frame(dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,])
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
#  browser()
      if(dim(a)[[1]] >0)  plot_median_cpue(a,la,lo) else plot(1,1,type="n",axes=F,ylab="",xlab="")
      }
    }
  title(ti,outer=T)
  savePlot(fname,type="png")
  }
# Median CPUE
minmaxy <- function(a,maxy) min(a,maxy,na.rm=T)
plot_median_cpue <- function(a,la,lo) {
  cx=0.9
  yrs <- sort(unique(a$yrqtr))
  alb <- tapply(a$alb/a$hooks,list(a$yrqtr),median)
  yft <- tapply(a$yft/a$hooks,list(a$yrqtr),median)
  bet <- tapply(a$bet/a$hooks,list(a$yrqtr),median)
  sbt <- tapply(a$sbt/a$hooks,list(a$yrqtr),median)
  maxy=0.03
  plot(1,1, type="n", xlab="Year", ylab="Median CPUE", ylim = c(0,maxy), xlim=range(dat$yrqtr),main=paste(la+2.5,lo+2.5,sep=", "))
  points(yrs,sapply(yft,minmaxy,maxy),col=1,pch=1,cex=cx)
  points(yrs,sapply(bet,minmaxy,maxy),col=2,pch=2,cex=cx)
  points(yrs,sapply(alb,minmaxy,maxy),col=3,pch=3,cex=cx)
  points(yrs,sapply(sbt,minmaxy,maxy),col=4,pch=4,cex=cx)
  }

median_all_sp_yq_fc(laseq=seq(20,10,-5)+2.5,loseq=seq(45,70,5)+2.5,fname="plot median YBAS by R1 latlong",ti="Effort Region 1")
median_all_sp_yq_fc(laseq=seq(5,-15,-5)+2.5,loseq=seq(40,70,5)+2.5,fname="plot median YBAS by R2 latlong",ti="Effort Region 2")
median_all_sp_yq_fc(laseq=seq(5,-15,-5)+2.5,loseq=seq(75,110,5)+2.5,fname="plot median YBAS by R5 latlong",ti="Effort Region 5")
median_all_sp_yq_fc(laseq=seq(-20,-50,-5)+2.5,loseq=seq(20,60,5)+2.5,fname="plot median YBAS by R3 latlong",ti="Effort Region 3")
median_all_sp_yq_fc(laseq=seq(-20,-50,-5)+2.5,loseq=seq(65,120,5)+2.5,fname="plot median YBAS by R4 latlong",ti="Effort Region 4")
median_all_sp_yq_fc(laseq=seq(20,10,-5)+2.5,loseq=seq(75,100,5)+2.5,fname="plot median YBAS by R6 latlong",ti="Effort Region 6")


# Mean CPUE
minmaxy <- function(a,maxy) min(a,maxy,na.rm=T)
plot_mean_cpue <- function(a,la,lo) {
  cx=0.9
  yrs <- sort(unique(a$yrqtr))
  alb <- tapply(a$alb/a$hooks,list(a$yrqtr),mean)
  yft <- tapply(a$yft/a$hooks,list(a$yrqtr),mean)
  bet <- tapply(a$bet/a$hooks,list(a$yrqtr),mean)
  maxy=0.03
  plot(1,1, type="n", xlab="Year", ylab="Mean CPUE", ylim = c(0,maxy), xlim=range(dat$yrqtr),main=paste(la+2.5,lo+2.5,sep=", "))
  points(yrs,sapply(yft,minmaxy,maxy),col=1,pch=1,cex=cx)
  points(yrs,sapply(bet,minmaxy,maxy),col=2,pch=2,cex=cx)
  points(yrs,sapply(alb,minmaxy,maxy),col=3,pch=3,cex=cx)
  }
mean_all_sp_yq_fc <- function(cpue,laseq,loseq,fname,ti="") {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- data.frame(dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,])
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_mean_cpue(a,la,lo) else plot(1,1,type="n")
      }
    }
  title(ti,outer=T)
  savePlot(fname,type="png")
  }


mean_all_sp_yq_fc(laseq=seq(20,10,-5)+2.5,loseq=seq(45,70,5)+2.5,fname="plot mean YBA by R1 latlong",ti="Effort Region 1")
mean_all_sp_yq_fc(laseq=seq(5,-15,-5)+2.5,loseq=seq(40,70,5)+2.5,fname="plot mean YBA by R2 latlong",ti="Effort Region 2")
mean_all_sp_yq_fc(laseq=seq(5,-15,-5)+2.5,loseq=seq(75,110,5)+2.5,fname="plot mean YBA by R5 latlong",ti="Effort Region 5")
mean_all_sp_yq_fc(laseq=seq(-20,-50,-5)+2.5,loseq=seq(20,60,5)+2.5,fname="plot mean YBA by R3 latlong",ti="Effort Region 3")
mean_all_sp_yq_fc(laseq=seq(-20,-50,-5)+2.5,loseq=seq(65,120,5)+2.5,fname="plot mean YBA by R4 latlong",ti="Effort Region 4")
mean_all_sp_yq_fc(laseq=seq(20,10,-5)+2.5,loseq=seq(75,100,5)+2.5,fname="plot mean YBA by R6 latlong",ti="Effort Region 6")

windows();par(mfrow=c(2,2))
a=(tapply(jpdataset$alb/jpdataset$hooks,jpdataset$lat,mean))
plot(as.numeric(names(a)),a,xlab="Latitude",ylab="ALB CPUE")
savePlot("CPUE by latitude",type="png")
a=tapply(jpdataset$alb/length(unique(jpdataset$op_yr)),jpdataset$lat,sum)
plot(as.numeric(names(a)),a,xlab="Latitude",ylab="ALB catch per year")
savePlot("Catch by latitude",type="png")
a=tapply(jpdataset$hooks,jpdataset$lat,sum)
plot(as.numeric(names(a)),a,xlab="Latitude",ylab="Hooks")
savePlot("Hooks by latitude",type="png")
savePlot("ALB by latitude")


# Bait type by region through time
#allt <- sort(unique(dat$tonnage))
#for (tt in 4:6) {
#  windows(height=14,width=18); par(mfcol=c(3,2),mar=c(3,4,2,1),oma=c(0,0,3,0))
#  baitn=paste0("bait",1:5)
#  for (r in regYord) {
#    plot(1:2,1:2,type="n",col=1,xlim=c(1979,2014),xlab="Year",ylab="Proportion of effort",main=paste("Region",r),ylim=c(0,1))
#    llv <- dat[dat$regY==r & dat$tonnage==allt[tt],]
#    for (i in 1:5) {
#      llv$btt <- with(llv,get(baitn[i]))
#      a <- tapply(llv$hooks,list(llv$yrqtr,is.na(llv$btt)|llv$btt==0),sum)
#      tota <- apply(a,1,sum,na.rm=T)
#      tota[tota==0]<-1
#      a[is.na(a)==T]<-0
#      lines(as.numeric(names(tota)),a[,1]/tota,type="p",col=i,pch=i,lty=1)
#      }
#    if(r==1) legend("bottomleft",legend=c("Pacific saury","Mackerel", "Squid", "Milkfish", "Other"),col=1:5,pch=1:5,lty=1)
#    }
#  title(paste("Tonnage",allt[tt]),outer=T)
#  savePlot(filename=paste("Bait type by region by yrqtr",tt),type="png")
#}


###################################################################################### stop here
###################################################################################### stop here
###################################################################################### stop here
###################################################################################### stop here
# Joint analyses
jointdir <- "D:/Simon/joint"
setwd(jointdir)
load("D:/Simon/KR/KRdat.RData")
krdat <- dat;rm(dat)
load("D:/Simon/Taiwan/TWdat.RData")
twdat <- dat;rm(dat)
load("D:/Simon/JP/JPdat.RData")
jpdat <- dat;rm(dat)


ls()
windows();par(mfrow=c(3,3))
for(y in 2000:2008) {
  a <- jpdat[jpdat$regY==2 & jpdat$op_yr==y,]
  x <- table(a$bet)
  plot(as.numeric(names(x)),x/sum(x),xlim=c(0,50),type="l",main=y,ylim=c(0,0.07),lwd=2,xlab="BET per set",ylab="frequency")
  a <- twdat[twdat$regY==2 & twdat$op_yr==y,]
  x <- table(a$bet)
  lines(as.numeric(names(x)),x/sum(x),col=2,lwd=2)
  a <- krdat[krdat$regY==2 & krdat$op_yr==y,]
  x <- table(a$bet)
  lines(as.numeric(names(x)),x/sum(x),col=3,lwd=2)
  }
legend("topright",legend=c("JP","TW","KR"),col=1:3,lwd=2)
savePlot("BET catch per set by flag and year",type="png")

ls()
windows();par(mfrow=c(3,3))
for(y in 2000:2008) {
  a <- jpdat[jpdat$regY==2 & jpdat$op_yr==y,]
  x <- table(a$yft)
  plot(as.numeric(names(x)),x/sum(x),xlim=c(0,50),type="l",main=y,ylim=c(0,0.07),lwd=2,xlab="BET per set",ylab="frequency")
  a <- twdat[twdat$regY==2 & twdat$op_yr==y,]
  x <- table(a$yft)
  lines(as.numeric(names(x)),x/sum(x),col=2,lwd=2)
  a <- krdat[krdat$regY==2 & krdat$op_yr==y,]
  x <- table(a$yft)
  lines(as.numeric(names(x)),x/sum(x),col=3,lwd=2)
  }
legend("topright",legend=c("JP","TW","KR"),col=1:3,lwd=2)
savePlot("YFT catch per set by flag and year",type="png")

a <-  aggregate(cbind(bet,yft,alb,sbt,ott,swo,mls,blm,bum,otb,sha,skj,oth,Total,hooks) ~ op_yr + op_mon + vessid,data=twdat[!is.na(twdat$lon),],FUN=sum)
sfb <- with(a,bet==0 & alb==0 & yft==0 & ott==0 & swo==0 & blm==0 & bum==0 & sha==0 & oth!=0 & otb==0 & mls==0 & sbt==0 & skj==0)
table(sfb)
a[sfb==T,]

table(twdat$rem)
sfb <- with(twdat,bet==0 & alb==0 & yft==0 & ott==0 & swo==0 & blm==0 & bum==0 & sha==0 & oth==0 & otb==0 & mls==0 & sbt==0 & skj==0)
table(sfb)

windows(height=14,width=17);par(mfrow=c(4,6),mar=c(2,1,1,1))
for(y in 1977:2000) {
  a <- jpdat[jpdat$regY==2 & jpdat$op_yr==y,]
  x <- table(a$bet)
  plot(as.numeric(names(x)),x/sum(x),xlim=c(0,50),type="l",main=y,ylim=c(0,0.07),lwd=2,xlab="BET per set",ylab="frequency")
  a <- twdat[twdat$regY==2 & twdat$op_yr==y,]
  x <- table(a$bet)
  lines(as.numeric(names(x)),x/sum(x),col=2,lwd=2)
  a <- krdat[krdat$regY==2 & krdat$yr==y,]
  x <- table(a$bet)
  lines(as.numeric(names(x)),x/sum(x),col=3,lwd=2)
  }
legend("topright",legend=c("JP","TW","KR"),col=1:3,lwd=2)
savePlot("BET catch per set by flag and year_77-00",type="png")
getwd()

windows(height=14,width=17);par(mfrow=c(4,6),mar=c(2,1,1,1))
for(y in 1977:2000) {
  a <- jpdat[jpdat$regY==2 & jpdat$op_yr==y,]
  x <- table(a$yft)
  plot(as.numeric(names(x)),x/sum(x),xlim=c(0,50),type="l",main=y,ylim=c(0,0.07),lwd=2,xlab="BET per set",ylab="frequency")
  a <- twdat[twdat$regY==2 & twdat$op_yr==y,]
  x <- table(a$yft)
  lines(as.numeric(names(x)),x/sum(x),col=2,lwd=2)
  a <- krdat[krdat$regY==2 & krdat$op_yr==y,]
  x <- table(a$yft)
  lines(as.numeric(names(x)),x/sum(x),col=3,lwd=2)
  }
legend("topright",legend=c("JP","TW","KR"),col=1:3,lwd=2)
savePlot("YFT catch per set by flag and year_77-00",type="png")
getwd()

source("../Rfiles/support_functions.r")
library("date")
library(splines)
library("maps")
library("mapdata")
library("maptools")
library(boot)

krdat <- data.frame(krdat)
jpdat <- data.frame(jpdat)
twdat <- data.frame(twdat)

allabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tripidmon","alb","bet","yft","swo",
          "mls","bum","blm","sbt","Total","dmy","lat","lon","lat5","lon5","regY","regB")

krdat$vessid <- paste0("K",as.character(krdat$vessid))
jpdat$vessid <- paste0("J",as.character(jpdat$vessid))
twdat$vessid <- paste0("T",as.character(twdat$vessid))
jdat <-  rbind(krdat[krdat$yrqtr < 2014,allabs],
                jpdat[,allabs],
                twdat[twdat$yrqtr > 2005,allabs])
jdat$vessid <- as.factor(jdat$vessid)

#rm(krdat,jpdat,twdat)

regYord <- c(1,2,3,6,5,4)
windows(height=14,width=12); par(mfcol=c(3,2),mar=c(3,2,2,1),oma=c(0,0,1,0))
for (r in regYord) {
  llv <- jdat[jdat$regY==r,]
  hist(llv$dmy,breaks="days",freq=T,xlab="Date",main=paste0("R",r))
  }
title("Sets per day",outer=T,line=0)
savePlot(filename=paste("Joint sets per day by region",sep=""),type="png")

# 5 degree squares fished
dimu <- function(x) { length(unique(x)) }
windows(height=14,width=12)
par (mfcol=c(3,2),mar=c(3,4,2,1))
for (r in regYord) {
  llv <- jdat[jdat$regY == r,]
  yq <- sort(unique(llv$yrqtr))
  strats <- tapply(paste(llv$lat5,llv$lon5),llv$yrqtr,dimu)
  plot(yq, strats, type="p", xlim=range(jdat$yrqtr),pch=1,col=1,ylim=c(0,max(strats)),
           cex=1,ylab="5 x 5 spatial strata with reported effort",main=paste("YFT Region",r))
#  mtext(side=3, paste("YFT Region", r),line=0.5)
  }
savePlot("Number of spatial strata",type="png")


minqtrs=8; maxqtrs=200; runreg <- 5;runsp="bet"
maxqtrs=200; minqtrs_byreg = c(8,8,8,8,8,8,8,8,8)
for(runreg in c(2,5)) {
  for(runsp in c("bet","yft"))
    {
    minqtrs <- minqtrs_byreg[runreg]
    jdat$reg <- jdat$regY
    a <- select_data_JointIO(jdat,runreg=runreg,minqtrs=minqtrs,runsp=runsp,mt="deltabin",addcl=NA,addpca=NA)
    if(nrow(a)>2000) {
      n <- make_strat(a)
      glmdat.bin <- samp_data(a,n,150)
      }
    mn <- with(glmdat.bin,0.1* mean(get(runsp)/hooks))
    glmdat.pos <- glmdat.bin[glmdat.bin[,3]>0,]
    wtt.opbin.area   <- mk_wts(glmdat.bin,wttype="area")
    wtt.oppos.area   <- mk_wts(glmdat.pos,wttype="area")
    wtt.opbin.equal  <- mk_wts(glmdat.bin,wttype="equal")
    fname <- paste0("Joint_",runsp,"_R",runreg,"eq ",minqtrs,"-",maxqtrs,"_qtrs")
    fmla.opbin <- make_formula_IO(runsp,modtype="deltabin",dohbf=T,addboat=F)
    fmla.oppos <- make_formula_IO(runsp,modtype="deltapos",dohbf=T,addboat=F)
    fmla.oplogn <- make_formula_IO(runsp,modtype="logn",dohbf=T,addboat=F)
    fmla.boatbin <- make_formula_IO(runsp,modtype="deltabin",dohbf=T,addboat=T)
    fmla.boatpos <- make_formula_IO(runsp,modtype="deltapos",dohbf=T,addboat=T)
    fmla.boatlogn <- make_formula_IO(runsp,modtype="logn",dohbf=T,addboat=T)
    abin <- length(unique(glmdat.bin$yrqtr))
    apos <- length(unique(glmdat.pos$yrqtr))
    model.opbin.equal  <- glm(fmla.opbin,data=glmdat.bin,family="binomial");gc()
    abin <- length(unique(glmdat.bin$yrqtr))
    coefs.opbin.equal <- get.bin.coefs(model.opbin.equal,abin,glmdat.bin)
    summ.opbin.equal <- summary(model.opbin.equal)
    save(file=paste(fname," summ.opbin.equal.RData",sep=""),summ.opbin.equal);rm(summ.opbin.equal);gc()
    save(file=paste(fname," model.opbin.equal.RData",sep=""),model.opbin.equal);rm(model.opbin.equal);gc()
    model.opboatbin.equal  <- glm(fmla.boatbin,data=glmdat.bin,weights=wtt.opbin.equal,family="binomial");gc()
    coefs.opboatbin.equal <- get.bin.coefs(model.opboatbin.equal,abin,glmdat.bin)
    plot_effects_IO(model=model.opboatbin.equal,indat=glmdat.bin,dohbf=T); savePlot(paste(fname,"bin_effects.png",sep=""),type="png")
    summ.opboatbin.equal <- summary(model.opboatbin.equal)
    save(file=paste(fname," summ.opboatbin.equal.RData",sep=""),summ.opboatbin.equal);rm(summ.opboatbin.equal);gc()
    save(file=paste(fname," model.opboatbin.equal.RData",sep=""),model.opboatbin.equal);rm(model.opboatbin.equal);gc()

    model.oppos.area   <- glm(fmla.oppos,data=glmdat.pos,weights=wtt.oppos.area,family="gaussian");gc()
    coefs.oppos.area <- get.coefs(model.oppos.area,apos)
    summ.model.oppos.area <- summary(model.oppos.area)
    save(summ.model.oppos.area,file="summ.model.oppos.area.RData");rm(summ.model.oppos.area);gc()
    save(file=paste(fname," model.oppos.area.RData",sep=""),model.oppos.area);rm(model.oppos.area);gc()
    model.opboatpos.area   <- glm(fmla.boatpos,data=glmdat.pos,weights=wtt.oppos.area,family="gaussian");gc()
    coefs.opboatpos.area <- get.coefs(model.opboatpos.area,apos)
    windows(); par(mfrow=c(2,1),mar=c(4,4,3,1))                           # plot diagnostics
    plotdiags(model.opboatpos.area$residuals,ti="Model including vessel")
    savePlot(paste(fname,"_diags.png",sep=""),type="png")
    plot_effects_IO(model=model.opboatpos.area,indat=glmdat.pos,dohbf=T); savePlot(paste(fname,"pos_effects.png",sep=""),type="png")
    summ.opboatpos.area <- summary(model.opboatpos.area)
    save(file=paste(fname," summ.opboatpos.area.RData",sep=""),summ.opboatpos.area);rm(summ.opboatpos.area);gc()
    save(file=paste(fname," model.opboatpos.area.RData",sep=""),model.opboatpos.area);rm(model.opboatpos.area);gc()
    mn <- with(glmdat.bin,0.1* mean(get(runsp)/hooks))
    model.opboatlogn.area   <- glm(fmla.boatlogn,data=glmdat.bin,weights=wtt.opbin.area,family="gaussian");gc()
    coefs.opboatlogn.area <- get.coefs(model.opboatlogn.area,abin)
    windows(); par(mfrow=c(2,1),mar=c(4,4,3,1))                           # plot diagnostics
    plotdiags(model.opboatlogn.area$residuals,ti="Model including vessel")
    savePlot(paste(fname,"_diags_logn.png",sep=""),type="png")
    plot_effects_IO(model.opboatlogn.area,indat=glmdat.bin,dohbf=T); savePlot(paste(fname,"logn_effects_.png",sep=""),type="png")
    save(file=paste(fname," model.opboatlogn.area.RData",sep=""),model.opboatlogn.area);rm(model.opboatlogn.area);gc()

    opyr <- sort(unique(glmdat.pos$yrqtr))
    fishlab <- switch(runsp,yft="Yellowfin",bet="Bigeye"); methlab <- switch(mt,deltabin="Delta-binomial",deltapos="Delta-positive",logl="Lognormal(+0.5)")
    a <- match(names(coefs.oppos.area),names(coefs.opbin.equal))             # make op.area indices
    coefs.op.area2 <- coefs.opbin.equal[a] * coefs.oppos.area
    a <- match(names(coefs.opboatpos.area),names(coefs.opboatbin.equal))             # make opboat.area indices
    coefs.opboat.area2 <- coefs.opboatbin.equal[a] * coefs.opboatpos.area
    save(file=paste(fname,"indices.RData",sep=""),list=ls(pattern="coefs."))

    plot_agg_slope_ratio(coefs.op.area2, coefs.opboat.area2, opyr,opyr,titl=paste("Region",runreg,fishlab,"op area vs opboat area2 d_logn"),  lab1="op area", lab2="opboat area2",fname)  # plot results
    rm(list=ls(pattern="coefs."))
    graphics.off()
    }
  }


krdat <- krdat[krdat$regY==5,allabs]
spec_dat <- krdat[,allsp] #extract catch composition
spec_dat$sum <- apply(spec_dat, 1,sum)
krdat <- krdat[spec_dat$sum > 0,]

twdat <- twdat[twdat$regY==5,allabs]
spec_dat <- twdat[,allsp] #extract catch composition
spec_dat$sum <- apply(spec_dat, 1,sum)
twdat <- twdat[spec_dat$sum > 0,]

jpdat <- jpdat[jpdat$regY==5,allabs]
spec_dat <- jpdat[,allsp] #extract catch composition
spec_dat$sum <- apply(spec_dat, 1,sum)
jpdat <- jpdat[spec_dat$sum > 0,]

krcldat <- make_clusters(setdat=data.frame(krdat),spp=allsp,ncl=ncll[r],titx=paste0("KR Region ",5,dec),setclust=F,tripid="tripidmon",fname=paste0("KRclus",dec))
twcldat <- make_clusters(setdat=data.frame(twdat),spp=allsp,ncl=ncll[r],titx=paste0("TW Region ",5,dec),setclust=F,tripid="tripidmon",fname=paste0("TWclus",dec))
jpcldat <- make_clusters(setdat=data.frame(jpdat),spp=allsp,ncl=ncll[r],titx=paste0("JP Region ",5,dec),setclust=F,tripid="tripidmon",fname=paste0("JPclus",dec))
covarnames <- c("yrqtr","latlong","hooks","hbf","vessid","Total","lat","lon","lat5","lon5","op_yr","regY")
krdataset <- cbind(krdat[,covarnames],krdat[,allsp],krcldat$setdat[,c("FT","kcltrp","clrtrp","hcltrp","kclset","clrset","hclset")])
twdataset <- cbind(twdat[,covarnames],twdat[,allsp],twcldat$setdat[,c("FT","kcltrp","clrtrp","hcltrp","kclset","clrset","hclset")])
jpdataset <- cbind(jpdat[,covarnames],jpdat[,allsp],jpcldat$setdat[,c("FT","kcltrp","clrtrp","hcltrp","kclset","clrset","hclset")])
krdataset$clust <- paste0("K",krdataset$FT)
twdataset$clust <- paste0("T",twdataset$FT)
jpdataset$clust <- paste0("J",jpdataset$FT)
jdataset <- rbind(krdataset,twdataset,jpdataset)
jdataset$vessid <- as.factor(jdataset$vessid)
jdataset$clust <- as.factor(jdataset$clust)
jdataset$reg <- jdataset$regY

  # Add cluster
minqtrs=8; maxqtrs=200; runreg <- 5;runsp="bet"
minqtrs_byreg = c(8,8,8,8,8,8,8,8,8)
for(runreg in c(5)) {
  for(runsp in c("yft"))
    {

    minqtrs <- minqtrs_byreg[runreg]
    jdataset$reg <- jdataset$regY
    a <- select_data_JointIO(jdataset,runreg=runreg,minqtrs=minqtrs,runsp=runsp,mt="deltabin",addpca=NA,addcl="clust")
    if(nrow(a)>2000) {
      n <- make_strat(a)
      glmdat.bin <- samp_data(a,n,30)
      }
    mn <- with(glmdat.bin,0.1* mean(get(runsp)/hooks))
    glmdat.pos <- glmdat.bin[glmdat.bin[,3]>0,]
    wtt.opbin.area   <- mk_wts(glmdat.bin,wttype="area")
    wtt.oppos.area   <- mk_wts(glmdat.pos,wttype="area")
    wtt.opbin.equal  <- mk_wts(glmdat.bin,wttype="equal")
    fname <- paste0("clustJoint_",runsp,"_R",runreg,"eq ",minqtrs,"-",maxqtrs,"_qtrs")
#    fmla.opbin <- make_formula_IO(runsp,modtype="deltabin",dohbf=T,addboat=F,addcl="clust")
#    fmla.oppos <- make_formula_IO(runsp,modtype="deltapos",dohbf=T,addboat=F,addcl="clust")
    fmla.oplogn <- make_formula_IO(runsp,modtype="logn",dohbf=T,addboat=F)
    fmla.oplogn.clust <- make_formula_IO(runsp,modtype="logn",dohbf=T,addboat=F,addcl="clust")
    fmla.boatbin <- make_formula_IO(runsp,modtype="deltabin",dohbf=T,addboat=T)
    fmla.boatpos <- make_formula_IO(runsp,modtype="deltapos",dohbf=T,addboat=T)
    fmla.boatbin.clust <- make_formula_IO(runsp,modtype="deltabin",dohbf=T,addboat=T,addcl="clust")
    fmla.boatpos.clust <- make_formula_IO(runsp,modtype="deltapos",dohbf=T,addboat=T,addcl="clust")
    fmla.boatlogn <- make_formula_IO(runsp,modtype="logn",dohbf=T,addboat=T)
    fmla.boatlogn.clust <- make_formula_IO(runsp,modtype="logn",dohbf=T,addboat=T,addcl="clust")
    abin <- length(unique(glmdat.bin$yrqtr))
    apos <- length(unique(glmdat.pos$yrqtr))
#    model.opbin.equal  <- glm(fmla.opbin,data=glmdat.bin,family="binomial");gc()
    abin <- length(unique(glmdat.bin$yrqtr))
#    coefs.opbin.equal <- get.bin.coefs(model.opbin.equal,abin,glmdat.bin)
#    summ.opbin.equal <- summary(model.opbin.equal)
#    save(file=paste(fname," summ.opbin.equal.RData",sep=""),summ.opbin.equal);rm(summ.opbin.equal);gc()
#    save(file=paste(fname," model.opbin.equal.RData",sep=""),model.opbin.equal);rm(model.opbin.equal);gc()
    model.opboatbin.equal  <- glm(fmla.boatbin,data=glmdat.bin,weights=wtt.opbin.equal,family="binomial");gc()
    model.opboatbin.clust  <- glm(fmla.boatbin.clust,data=glmdat.bin,weights=wtt.opbin.equal,family="binomial");gc()
    coefs.opboatbin.equal <- get.bin.coefs(model.opboatbin.equal,abin,glmdat.bin)
    coefs.opboatbin.clust <- get.bin.coefs(model.opboatbin.clust,abin,glmdat.bin)
#    plot_effects_IO(model=model.opboatbin.clust,indat=glmdat.bin,dohbf=T); savePlot(paste(fname,"bin_effects_clust.png",sep=""),type="png")
    summ.opboatbin.equal <- summary(model.opboatbin.equal)
    summ.opboatbin.clust <- summary(model.opboatbin.clust)
    save(file=paste(fname," summ.opboatbin.equal.RData",sep=""),summ.opboatbin.equal,summ.opboatbin.clust);rm(summ.opboatbin.equal,summ.opboatbin.clust);gc()
    save(file=paste(fname," model.opboatbin.equal.RData",sep=""),model.opboatbin.equal,model.opboatbin.clust);rm(model.opboatbin.equal,model.opboatbin.clust);gc()

#    model.oppos.area   <- glm(fmla.oppos,data=glmdat.pos,weights=wtt.oppos.area,family="gaussian");gc()
#    coefs.oppos.area <- get.coefs(model.oppos.area,apos)
#    summ.model.oppos.area <- summary(model.oppos.area)
#    save(summ.model.oppos.area,file="summ.model.oppos.area.RData");rm(summ.model.oppos.area);gc()
#    save(file=paste(fname," model.oppos.area.RData",sep=""),model.oppos.area);rm(model.oppos.area);gc()
    model.opboatpos.area   <- glm(fmla.boatpos,data=glmdat.pos,weights=wtt.oppos.area,family="gaussian");gc()
    coefs.opboatpos.area <- get.coefs(model.opboatpos.area,apos)
    model.opboatpos.area.clust   <- glm(fmla.boatpos.clust,data=glmdat.pos,weights=wtt.oppos.area,family="gaussian");gc()
    coefs.opboatpos.area.clust <- get.coefs(model.opboatpos.area.clust,apos)
    windows(); par(mfrow=c(2,1),mar=c(4,4,3,1))
    # plot diagnostics
    plotdiags(model.opboatpos.area.clust$residuals,ti="Model including vessel and cluster")
    savePlot(paste(fname,"_diags.png",sep=""),type="png")
    plot_effects_IO(model=model.opboatpos.area.clust,indat=glmdat.pos,dohbf=T); savePlot(paste(fname,"pos_effects_clust.png",sep=""),type="png")
    summ.opboatpos.area <- summary(model.opboatpos.area)
    summ.opboatpos.area.clust <- summary(model.opboatpos.area.clust)
    save(file=paste(fname," summ.opboatpos.area.clust.RData",sep=""),summ.opboatpos.area,summ.opboatpos.area.clust);rm(summ.opboatpos.area,summ.opboatpos.area.clust);gc()
    save(file=paste(fname," model.opboatpos.area.RData",sep=""),model.opboatpos.area,model.opboatpos.area.clust);rm(model.opboatpos.area.clust,model.opboatpos.area);gc()
    mn <- with(glmdat.bin,0.1* mean(get(runsp)/hooks))
    model.opboatlogn.area   <- glm(fmla.boatlogn,data=glmdat.bin,weights=wtt.opbin.area,family="gaussian");gc()
    coefs.opboatlogn.area <- get.coefs(model.opboatlogn.area,abin)

    model.opboatlogn.clust   <- glm(fmla.boatlogn.clust,data=glmdat.bin,weights=wtt.opbin.area,family="gaussian");gc()
    coefs.opboatlogn.clust <- get.coefs(model.opboatlogn.clust,abin)
    windows(); par(mfrow=c(2,1),mar=c(4,4,3,1))                           # plot diagnostics
    plotdiags(model.opboatlogn.clust$residuals,ti="Model including vessel")
    savePlot(paste(fname,"_diags_logn.png",sep=""),type="png")
    plot_effects_IO(model.opboatlogn.clust,indat=glmdat.bin,dohbf=T); savePlot(paste(fname,"logn_effects_.png",sep=""),type="png")
    save(file=paste(fname," model.opboatlogn.clust.RData",sep=""),model.opboatlogn.clust);rm(model.opboatlogn.clust);gc()

    opyr <- sort(unique(glmdat.pos$yrqtr))
    fishlab <- switch(runsp,yft="Yellowfin",bet="Bigeye"); methlab <- switch(mt,deltabin="Delta-binomial",deltapos="Delta-positive",logl="Lognormal(+0.5)")

    plot_agg_slope_ratio(coefs.opboatlogn.area, coefs.opboatlogn.clust, opyr,opyr,titl=paste("Region",runreg,fishlab,"opboat  vs opboat cluster d_logn"),  lab1="op boat", lab2="opboat cluster",fname)  # plot results

    a <- match(names(coefs.opboatpos.area),names(coefs.opboatbin.equal))             # make opboat.area indices
    coefs.opboat.area2 <- coefs.opboatbin.equal[a] * coefs.opboatpos.area
    a <- match(names(coefs.opboatpos.area.clust),names(coefs.opboatbin.clust))             # make opboat.area indices
    coefs.opboat.area2.clust <- coefs.opboatbin.clust[a] * coefs.opboatpos.area.clust

    save(file=paste(fname,"indices.RData",sep=""),list=ls(pattern="coefs."))
    rm(list=ls(pattern="coefs."))
    graphics.off()
    }
  }

