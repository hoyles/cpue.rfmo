projdir <- "~/ICCAT/2018_Bigeye/"
krdir <- paste0(projdir, "KR/")
datadir1 <- paste0(krdir, "data/")
kralysis_dir <- paste0(krdir, "analyses/")
krfigs <- paste0(krdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")

setwd(krfigs)

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
library(devtools)

library("cpue.rfmo")

load(file = paste0(kralysis_dir,"KRdat.RData"))


make_reg_windows <- function(type=1) {
  if(type == 1) windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
  if(type == 2) windows(height=12,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
}
regBord <- c(2,3)
#####################################

# Species composition maps
a5 <- aggregate(cbind(alb,bet,bft,blm,bum,mls,oth,sfa,sha,skj,swo,yft,Total,Total2,hooks) ~ lon5 + lat5 + eval(5*floor((op_yr)/5)),data=dat,FUN=sum)
a <- aggregate(cbind(alb,bet,bft,blm,bum,mls,oth,sfa,sha,skj,swo,yft,Total,Total2,hooks) ~ lon + lat + eval(5*floor((op_yr)/5)),data=dat,FUN=sum)
names(a)[3] <- names(a5)[3] <- "decade"
names(a5)[1:2] <- c("lon","lat")

# Should reconfigure all of these to run as a loop
windows(width=20,height=20);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015,5)) plot_catchmap(indat=a5,vbl=a5$bet/(a5$bet+a5$yft),dcd=d,latlim=c(-15,25),lonlim=c(-80,20),ti="BET / BET + YFT")
savePlot("PropBET in YFT_BET5",type="png")
windows(width=20,height=20);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$bet/(a5$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BET / Total")
savePlot("PropBET in Total5",type="png")
windows(width=20,height=20);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$swo/(a5$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SWO / Total",brk2=seq(0,1,.05))
savePlot("PropSWO in Total5",type="png")
windows(width=20,height=20);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$alb/(a5$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="ALB / Total",brk2=seq(0,1,.1))
savePlot("PropALB in Total5",type="png")
windows(width=20,height=20);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$sfa/(a5$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SFA / Total",brk2=seq(0,1,.05))
savePlot("PropSFA in Total5",type="png")
windows(width=20,height=20);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$sha/(a5$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SHA / Total",brk2=seq(0,1,.05))
savePlot("PropSHA in Total5",type="png")
windows(width=20,height=20);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$yft/(a5$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="YFT / Total")
savePlot("PropYFT in Total5",type="png")
windows(width=20,height=20);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$mls/(a5$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="MLS / Total",brk2=seq(0,1,.05))
savePlot("PropMLS in Total5",type="png")
windows(width=20,height=20);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$bft/(a5$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="bft / Total",brk2=seq(0,1,.05))
savePlot("PropSBF in Total5",type="png")
windows(width=20,height=20);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$blm/(a5$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BLM / Total",brk2=seq(0,1,.05))
savePlot("PropBLM in Total5",type="png")
windows(width=20,height=20);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$bum/(a5$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BUM / Total",brk2=seq(0,1,.05))
savePlot("PropBUM in Total5",type="png")

windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$bet/(a$bet+a$yft),dcd=d,latlim=c(-15,25),lonlim=c(-80,20),ti="BET / BET + YFT")
savePlot("PropBET in YFT_BET",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$bet/(a$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BET / Total")
savePlot("PropBET in Total",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$swo/(a$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SWO / Total",brk2=seq(0,1,.05))
savePlot("PropSWO in Total",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$alb/(a$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="ALB / Total",brk2=seq(0,1,.1))
savePlot("PropALB in Total",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$sfa/(a$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SFA / Total",brk2=seq(0,1,.05))
savePlot("PropSFA in Total",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$sha/(a$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SHA / Total",brk2=seq(0,1,.05))
savePlot("PropSHA in Total",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$yft/(a$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="YFT / Total")
savePlot("PropYFT in Total",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$mls/(a$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="MLS / Total",brk2=seq(0,1,.05))
savePlot("PropMLS in Total",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$bft/(a$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="bft / Total",brk2=seq(0,1,.05))
savePlot("PropSBF in Total",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$blm/(a$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BLM / Total",brk2=seq(0,1,.05))
savePlot("PropBLM in Total",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a,vbl=a$bum/(a$Total),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BUM / Total",brk2=seq(0,1,.05))
savePlot("PropBUM in Total",type="png")

########### relative to YBA
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2010,10)) plot_catchmap(indat=a,vbl=a$bet/(a$Total2),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BET / YBA Total")
savePlot("PropBET in altTotal",type="png")
for(d in seq(1975,2010,10)) plot_catchmap(indat=a,vbl=a$alb/(a$Total2),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="ALB / YBA Total",brk2=seq(0,1,.1))
savePlot("PropALB in altTotal",type="png")
for(d in seq(1975,2010,10)) plot_catchmap(indat=a,vbl=a$yft/(a$Total2),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="YFT / YBA Total")
savePlot("PropYFT in altTotal",type="png")

windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$bet/(a5$Total2),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BET / YBA Total")
savePlot("PropBET in altTotal5",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$alb/(a5$Total2),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="ALB / YBA Total",brk2=seq(0,1,.1))
savePlot("PropALB in altTotal5",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap(indat=a5,vbl=a5$yft/(a5$Total2),dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="YFT / YBA Total")
savePlot("PropYFT in altTotal5",type="png")

#Catch maps
a5 <- aggregate(cbind(alb,bet,bft,blm,bum,mls,oth,sfa,sha,skj,swo,yft,Total,Total2,hooks) ~ lon5 + lat5 + eval(5*floor((op_yr)/5)),data=dat,FUN=sum)
a <- aggregate(cbind(alb,bet,bft,blm,bum,mls,oth,sfa,sha,skj,swo,yft,Total,Total2,hooks) ~ lon + lat + eval(5*floor((op_yr)/5)),data=dat,FUN=sum)
names(a)[3] <- names(a5)[3] <- "decade"
names(a5)[1:2] <- c("lon","lat")

windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$bet,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BET Catch")
savePlot("Catchmap_BET",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$swo,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SWO Catch")
savePlot("Catchmap_SWO",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$alb,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="ALB Catch")
savePlot("Catchmap_ALB",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$sfa,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SFA Catch")
savePlot("Catchmap_sfa",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$sha,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SHA Catch")
savePlot("Catchmap_SHA",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$yft,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="YFT Catch")
savePlot("Catchmap_YFT",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$mls,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="MLS Catch")
savePlot("Catchmap_MLS",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$bft,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="bft Catch")
savePlot("Catchmap_bft",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$blm,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BLM Catch")
savePlot("Catchmap_BLM",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$bum,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BUM Catch")
savePlot("Catchmap_BUM",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$oth,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="OTH Catch")
savePlot("Catchmap_OTH",type="png")
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a,vbl=a$skj,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SKJ Catch")
savePlot("Catchmap_SKJ",type="png")

windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$bet,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BET Catch")
savePlot("Catchmap5_BET",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$swo,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SWO Catch")
savePlot("Catchmap5_SWO",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$alb,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="ALB Catch")
savePlot("Catchmap5_ALB",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$sfa,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SFA Catch")
savePlot("Catchmap5_sfa",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$sha,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SHA Catch")
savePlot("Catchmap5_SHA",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$yft,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="YFT Catch")
savePlot("Catchmap5_YFT",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$mls,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="MLS Catch")
savePlot("Catchmap5_MLS",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$bft,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="bft Catch")
savePlot("Catchmap5_bft",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$blm,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BLM Catch")
savePlot("Catchmap5_BLM",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$bum,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BUM Catch")
savePlot("Catchmap5_BUM",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$oth,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="OTH Catch")
savePlot("Catchmap5_OTH",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_catchmap2(indat=a5,vbl=a5$skj,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SKJ Catch")
savePlot("Catchmap5_SKJ",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)

#CPUE maps
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$bet,vb2=a$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BET CPUE")
savePlot("Cpuemap_BET",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$swo,vb2=a$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SWO CPUE")
savePlot("Cpuemap_SWO",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$alb,vb2=a$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="ALB CPUE")
savePlot("Cpuemap_ALB",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$sfa,vb2=a$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SFA CPUE")
savePlot("Cpuemap_sfa",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$sha,vb2=a$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SHA CPUE")
savePlot("Cpuemap_SHA",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$yft,vb2=a$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="YFT CPUE")
savePlot("Cpuemap_YFT",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$mls,vb2=a$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="MLS CPUE")
savePlot("Cpuemap_MLS",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$bft,vb2=a$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="bft CPUE")
savePlot("Cpuemap_bft",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$blm,vb2=a$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BLM CPUE")
savePlot("Cpuemap_BLM",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$bum,vb2=a$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BUM CPUE")
savePlot("Cpuemap_BUM",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$oth,vb2=a$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="OTH CPUE")
savePlot("Cpuemap_OTH",type="png")
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a,vb1=a$skj,vb2=a$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SKJ CPUE")
savePlot("Cpuemap_SKJ",type="png")

windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$bet,vb2=a5$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BET CPUE", delta=5)
savePlot("CPUEmap5_BET",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$swo,vb2=a5$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SWO CPUE", delta=5)
savePlot("CPUEmap5_SWO",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$alb,vb2=a5$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="ALB CPUE", delta=5)
savePlot("CPUEmap5_ALB",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$sfa,vb2=a5$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SFA CPUE", delta=5)
savePlot("CPUEmap5_sfa",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$sha,vb2=a5$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SHA CPUE", delta=5)
savePlot("CPUEmap5_SHA",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$yft,vb2=a5$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="YFT CPUE", delta=5)
savePlot("CPUEmap5_YFT",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$mls,vb2=a5$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="MLS CPUE", delta=5)
savePlot("CPUEmap5_MLS",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$bft,vb2=a5$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="bft CPUE", delta=5)
savePlot("CPUEmap5_bft",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$blm,vb2=a5$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BLM CPUE", delta=5)
savePlot("CPUEmap5_BLM",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$bum,vb2=a5$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="BUM CPUE", delta=5)
savePlot("CPUEmap5_BUM",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$oth,vb2=a5$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="OTH CPUE", delta=5)
savePlot("CPUEmap5_OTH",type="png")
windows(width=20,height=15);par(mfrow=c(3,3), mar = c(4,4,2,1)+.1)
for(d in seq(1975,2015, 5)) plot_cpuemap2(indat=a5,vb1=a5$skj,vb2=a5$hooks,dcd=d,latlim=c(-45,40),lonlim=c(-80,20),ti="SKJ CPUE", delta=5)
savePlot("CPUEmap5_SKJ",type="png")

#####################################
#Prepare figures
# Logsheets per year by region
windows(height=14,width=12); par(mfcol=c(2,1),mar=c(2,1,2,1),oma=c(0,0,1,0))
xlims <- range(dat$dmy,na.rm=T)
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  hist(llv$dmy,breaks="days",freq=T,xlab="Date",main=paste0("R",r),xlim=xlims)
  }
title("Sets per day",outer=T,line=0)
savePlot(filename=paste("Sets per day by region",sep=""),type="png")

for (r in regBord) {
  llv <- dat[dat$regB==r,]
  windows(height=12,width=14);par(mfrow=c(4,2),oma=c(0,0,1,0))
  for(dec in seq(1975,2010,5)) {
    a <- llv[llv$op_yr >= dec & llv$op_yr < dec+5,]
    hist(a$hooks,breaks=seq(-10,45010,50),xlim=c(0,5000),main=dec)
    }
  title(paste0("Hooks per set R",r),outer=T,line=0)
  savePlot(filename=paste0("Histogram hooks by decade R",r),type="png")
}

windows(height=12,width=14);par(mfrow=c(4,2))
for(dec in seq(1975,2010,5)) {
  a <- dat[dat$op_yr >= dec & dat$op_yr < dec+5,]
  hist(a$hooks,breaks=seq(-10,45010,50),xlim=c(0,5000),main=dec)
  }
savePlot(filename=paste0("Histogram hooks by decade"),type="png")

windows(height=14,width=12); par(mfcol=c(2,1),mar=c(2,1,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  a <- table(llv$vessid,llv$op_yr)
  a <- apply(a>0,1,sum)
  print(table(a))

  a <- tapply(llv$op_yr,llv$op_yr,length)
  plot(as.numeric(names(a)),a,xlab="yr",ylab="Logsheet records",main=paste("Region",r),xlim=c(1975,2015))
  }
savePlot(filename=paste("Number of records",sep=""),type="png")

lu <- function(x) {
  length(unique(x))
  }

# Vessels per year by region
windows(height=14,width=12); par(mfcol=c(2,1),mar=c(2,1,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  a <- tapply(llv$vessid,llv$op_yr,lu)
  plot(names(a),a,xlab="Year",ylab="",main=paste("Region",r))
  }
savePlot(filename=paste("Unique vessels by year",sep=""),type="png")

# Time distribution of vessels
vy <- list()
windows(height=14,width=12); par(mfcol=c(2,1),mar=c(2,1,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  vess <- as.numeric(unique(llv$vessid))
  minyr <- maxyr <- vess
#  for (i in 1:length(vess)) {
#    minyr[i] <- min(llv[vess[i]==as.numeric(llv$vessid),]$op_yr)
#    maxyr[i] <- max(llv[vess[i]==as.numeric(llv$vessid),]$op_yr)
#    }
  vvv <- llv[order(llv$vessid,llv$op_yr),]
  minyr <- vvv[match(vess,as.numeric(vvv$vessid)),"op_yr"]
  vvv <- llv[order(llv$vessid,-llv$op_yr),]
  maxyr <- vvv[match(vess,as.numeric(vvv$vessid)),"op_yr"]
  vessyrs <- data.frame(vess=vess,minyr=as.numeric(minyr),maxyr=as.numeric(maxyr),stringsAsFactors=F)
  vessyrs <- vessyrs[order(-floor(vessyrs$minyr),-floor(vessyrs$maxyr)),]
  vy[[r]] <- vessyrs
  plot(1:length(vess),1:length(vess),xlim=c(1975,2015),type="n",xlab="Years",ylab="Vessel",main=paste("Region",r))
  for (i in 1:length(vess)) {
    lines(c(floor(vessyrs[i,]$minyr),floor(vessyrs[i,]$maxyr)),c(i,i))
    }
  }
savePlot(filename=paste("Time distribution of vessels 1",sep=""),type="png")

windows(height=14,width=12); par(mfcol=c(2,1),mar=c(2,1,2,1))
for (r in regBord) {
  vessyrs <- vy[[r]]
  vessyrs <- vessyrs[order(-floor(vessyrs$maxyr)),]
  llv <- dat[dat$regB==r,]
  vess <- unique(llv$vessid)
  plot(1:length(vess),1:length(vess),xlim=c(1975,2015),type="n",xlab="Years",ylab="Vessel",main=paste("Region",r))
  for (i in 1:length(vess)) {
    lines(c(floor(vessyrs[i,]$minyr),floor(vessyrs[i,]$maxyr)),c(i,i))
    }
  }
savePlot(filename=paste("Time distribution of vessels 2",sep=""),type="png")


windows(height=14,width=12); par(mfcol=c(2,1),mar=c(2,1,2,1))
for (r in regBord) {
  vessyrs <- vy[[r]]
  llv <- dat[dat$regB==r,]
  vess <- as.numeric(unique(llv$vessid))
  minyr <- vess
  plot(1:length(vess),1:length(vess),xlim=c(1975,2015),type="n",xlab="Years",ylab="Vessel",main=paste("Region",r))
  for (i in 1:length(vess)) {
    a <- floor(llv[vessyrs[i,1]==as.numeric(llv$vessid),]$op_yr)
    pp <- unique(a)
    pp2 <- tapply(a,a,length)
#    points(pp,rep(i,length(pp)),cex=0.6,pch=3)
    symbols(pp,rep(i,length(pp)),sqrt(pp2)/40, add = T, inches =FALSE)
    }
  }
savePlot(filename=paste("Time distribution of vessels 4",sep=""),type="png")

# Effort by region
windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  a <- tapply(llv$hooks,llv$yrqtr,sum)
  plot(names(a),a,xlab="Year",ylab="hooks",main=paste("Region",r),xlim=range(dat$yrqtr))
  }
savePlot(filename="Effort by region by yrqtr",type="png")

# Sets by region
windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  a <- tapply(llv$hooks,llv$yrqtr,length)
  plot(names(a),a,xlab="Year",ylab="Sets",main=paste("Region",r),xlim=range(dat$yrqtr))
  }
savePlot(filename="Sets by region by yrqtr",type="png")

## Sets by region by yrqtr by fishingcat
#windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
#for (r in regBord) {
#  llv <- dat[dat$regB==r,]
#  a <- tapply(llv$hooks,llv$yrqtr,length)
#  plot(names(a),a,xlab="Year",ylab="Sets",main=paste("Region",r),col=1,pch=16)
#  llv <- dat[dat$regB==r & dat$newfishingcat==1,]
#  a <- tapply(llv$hooks,llv$yrqtr,length)
#  points(names(a),a,col=2,pch=2)
#  llv <- dat[dat$regB==r & dat$newfishingcat==2,]
#  a <- tapply(llv$hooks,llv$yrqtr,length)
#  points(names(a),a,col=4,pch=4)
#  }
#legend("topright",legend=c("Total","Offshore","Distant water"),col=c(1,2,4),pch=c(16,2,4))
#savePlot(filename="Sets by region by yrqtr by fishingcat",type="png")

# total effort by region and yearqtr
e <- tapply(dat$hooks,list(dat$yrqtr,dat$regB),sum)
write.table(file="effort by region and yrqtr.csv",e,sep=",")

dat$lonfac <- factor(dat$lon,levels=seq(min(dat$lon),max(dat$lon),1))
dat$latfac <- factor(dat$lat,levels=seq(min(dat$lat),max(dat$lat),1))
levels(dat$latfac)
levels(dat$lonfac)

windows(height=16,width=14); par(mfrow=c(4,2),mar=c(3,4,2,1))
ax <- expand.grid(latfac=unique(dat$latfac),lonfac=unique(dat$lonfac),hooks=NA)
bs <- tapply(ax$hooks,list(ax$lonfac,ax$latfac),length)
for(dec in seq(1975,2010,5)) {
  a <- dat[dat$op_yr >= dec & dat$op_yr < dec+5 & dat$regB %in% c(2,5),]
  x <- tapply(a$hooks,list(a$lonfac,a$latfac),sum)
  image(as.numeric(dimnames(bs)[[1]]),as.numeric(dimnames(bs)[[2]]),bs,col="lightblue",xlab="Longitude",ylab="Latitude",xlim=c(-80,20),ylim=c(-15,25),main=dec)
  image(as.numeric(dimnames(x)[[1]]),as.numeric(dimnames(x)[[2]]),x,xlab="Longitude",ylab="Latitude", xlim=c(-80,20),main=dec,add=T,breaks=exp(seq(log(min(x,na.rm=T)),log(max(x*1.01,na.rm=T)),length.out=13)))
  contour(as.numeric(dimnames(x)[[1]]),as.numeric(dimnames(x)[[2]]),x,add=T,
    levels=c(10,10^2,10^3,10^4,10^5,10^6,10^7,10^8,10^9,10^10,10^1,10^12))
  map("world",add=T, interior=F,fill=T)
  }
savePlot(filename=paste0("Map hooks by decade log"),type="png")


windows(height=16,width=14); par(mfrow=c(4,2),mar=c(3,4,2,1))
ax <- expand.grid(latfac=unique(dat$latfac),lonfac=unique(dat$lonfac),hooks=NA)
bs <- tapply(ax$hooks,list(ax$lonfac,ax$latfac),length)
for(dec in seq(1975,2010,5)) {
  a <- dat[dat$op_yr >= dec & dat$op_yr < dec+5 & dat$regB %in% c(2,5),]
  x <- tapply(a$hooks,list(a$lonfac,a$latfac),length)
  image(as.numeric(dimnames(bs)[[1]]),as.numeric(dimnames(bs)[[2]]),bs,col="lightblue",xlab="Longitude",ylab="Latitude",
    xlim=c(-80,20),ylim=c(-15,25),main=dec)
  image(as.numeric(dimnames(x)[[1]]),as.numeric(dimnames(x)[[2]]),x,xlab="Longitude",ylab="Latitude",
    xlim=c(-80,20),main=dec,add=T,breaks=exp(seq(0,13,length.out=13)))
  contour(as.numeric(dimnames(x)[[1]]),as.numeric(dimnames(x)[[2]]),x,add=T,
    levels=c(10,10^2,10^3,10^4,10^5,10^6,10^7,10^8,10^9,10^10,10^1,10^12))
  map("world",add=T, interior=F,fill=T)
  }
savePlot(filename=paste0("Map sets by decade"),type="png")

windows(height=16,width=14); par(mfrow=c(4,2),mar=c(3,4,2,1))
ax <- expand.grid(latfac=unique(dat$latfac),lonfac=unique(dat$lonfac),hooks=NA)
bs <- tapply(ax$hooks,list(ax$lonfac,ax$latfac),length)
for(dec in seq(1975,2010,5)) {
  a <- dat[dat$op_yr >= dec & dat$op_yr < dec+5,c("latfac","lonfac","hooks")]
  x <- tapply(a$hooks,list(a$lonfac,a$latfac),sum,na.rm=T)
  image(as.numeric(dimnames(bs)[[1]]),as.numeric(dimnames(bs)[[2]]),bs,col="lightblue",xlab="Longitude",ylab="Latitude",xlim=c(-80,20),ylim=c(-45,40),main=dec)
  image(as.numeric(dimnames(x)[[1]]),as.numeric(dimnames(x)[[2]]),x,
    add=T, breaks=exp(seq(log(min(x,na.rm=T)),log(max(x*1.01,na.rm=T)),length.out=13)))
  contour(as.numeric(dimnames(x)[[1]]),as.numeric(dimnames(x)[[2]]),x,add=T,
    levels=c(10,10^2,10^3,10^4,10^5,10^6,10^7,10^8,10^9,10^10,10^1,10^12))
  map("world",add=T, interior=F,fill=T)
  }
savePlot(filename=paste0("Map hooks all by decade log"),type="png")


windows(height=16,width=14); par(mfrow=c(4,2),mar=c(3,4,2,1))
ax <- expand.grid(latfac=unique(dat$latfac),lonfac=unique(dat$lonfac),hooks=NA)
bs <- tapply(ax$hooks,list(ax$lonfac,ax$latfac),length)
for(dec in seq(1975,2010,5)) {
  a <- dat[dat$op_yr >= dec & dat$op_yr < dec+5,c("latfac","lonfac","hooks")]
  x <- tapply(a$hooks,list(a$lonfac,a$latfac),length)
  image(as.numeric(dimnames(bs)[[1]]),as.numeric(dimnames(bs)[[2]]),bs,col="lightblue",xlab="Longitude",ylab="Latitude",
    xlim=c(-80,20),ylim=c(-45,40),main=dec)
  image(as.numeric(dimnames(x)[[1]]),as.numeric(dimnames(x)[[2]]),x,add=T,breaks=exp(seq(0,13,length.out=13)))
  contour(as.numeric(dimnames(x)[[1]]),as.numeric(dimnames(x)[[2]]),x,add=T,
    levels=c(10,10^2,10^3,10^4,10^5,10^6,10^7,10^8,10^9,10^10,10^1,10^12))
  map("world",add=T, interior=F,fill=T)
  }
savePlot(filename=paste0("Map sets all by decade"),type="png")

windows(height=16,width=20); par(mfrow=c(6,6),mar=c(1,1,2,1))
llv <- dat
ax <- expand.grid(latfac=unique(dat$latfac),lonfac=unique(dat$lonfac),hooks=NA)
bs <- tapply(ax$hooks,list(ax$lonfac,ax$latfac),length)
for(dec in seq(1979,2014,1)) {
  a <- llv[llv$op_yr >= dec & llv$op_yr < dec+1,]
  x <- tapply(a$hooks,list(a$lonfac,a$latfac),length)
  image(as.numeric(dimnames(bs)[[1]]),as.numeric(dimnames(bs)[[2]]),bs,col="lightblue",xlab="Longitude",ylab="Latitude",
    xlim=c(-80,20),ylim=c(-45,40))
  image(as.numeric(dimnames(x)[[1]]),as.numeric(dimnames(x)[[2]]),x,xlab="Longitude",ylab="Latitude",
    xlim=c(-80,20),add=T,breaks=exp(seq(0,11,length.out=13)))
  title(main=dec,line=0.3)
  contour(as.numeric(dimnames(x)[[1]]),as.numeric(dimnames(x)[[2]]),x,add=T,
    levels=c(10,10^2,10^3,10^4,10^5,10^6,10^7,10^8,10^9,10^10,10^1,10^12))
  map("world",add=T, interior=F,fill=T)
  }
savePlot(filename=paste0("Map sets by year"),type="png")


lenzero <- function(x) sum(x > 0)
dat$ssp <- apply(dat[,c("alb","bet","bft","blm","bum","mls","oth","sfa","sha","skj","swo","yft")],1,lenzero)
table(dat$ssp)

windows(height=14,width=12); par(mfcol=c(2,1),mar=c(5,4,2,6)+.1)
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  a <- tapply(llv$ssp==1,llv$op_yr,mean)
  plot(names(a),a,xlab="",type="l",ylim=c(0,1),ylab="",axes=F)
  mtext("Year",side=1,col=1,line=2.5,cex=0.8)
  box()
  axis(side=1)
  axis(side=2)
  mtext(text="Proportion single species",side=2,line=2.5,cex=0.8)
  par(new=T)
  a <- tapply(llv$op_yr,llv$op_yr,length)
  plot(names(a),a,axes=F,xlab="",ylab="",col=2,col.axis=2,type="p")
  axis(side=4,col="red",col.axis=2,las=1)
  mtext(text="Number of sets",side=4,line=4,col="red",cex=0.8)
  if(r==5) legend("topright",legend=c("Proportion single species","Number of sets"),col=c(1,2),lty=c(1,0),pch=c(NA,1))
  }
savePlot(file="proportion_single_species",type="png")

prepdat <- setup_AO_regions(prepdat, regB = TRUE, regB1 = TRUE)
windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  cld <- dat[dat$regB==r,]
  rwd <- prepdat[prepdat$regB==r,]
  cle <- tapply(cld$hooks,factor(cld$yrqtr,levels=unique(c(rwd$yrqtr,cld$yrqtr))),sum)
  cle[is.na(cle)] <- 0
  rwe <- tapply(rwd$hooks,factor(rwd$yrqtr,levels=unique(c(rwd$yrqtr,cld$yrqtr))),sum)
  rwe[is.na(rwe)] <- 0
  plot(names(cle),cle/rwe[match(names(cle),names(rwe))],xlab="Year",ylab="Proportion of hooks",main=paste("Region",r),ylim=c(0,1.1),xlim=range(prepdat$yrqtr))
  }
savePlot(filename="Cleaned effort proportion by region by yrqtr",type="png")

# Target by region through time

# Catch by region
windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  yft <- tapply(llv$yft,llv$yrqtr,sum)
  bet <- tapply(llv$bet,llv$yrqtr,sum)
  alb <- tapply(llv$alb,llv$yrqtr,sum)
  bft <- tapply(llv$bft,llv$yrqtr,sum)
  maxy <- max(c(yft,bet,alb,bft))
  plot(names(yft),yft+1,ylim=c(1,maxy),xlab="Year",ylab="Catch",main=paste("Region",r),xlim=range(dat$yrqtr),log="y")
  points(names(bet),bet+1,col=2,pch=2)
  points(names(alb),alb+1,col=3,pch=3)
#  points(names(swo),swo+1,col=4,pch=4)
  points(names(bft),bft+1,col=4,pch=4)
  if(r==3) legend("topright",legend=c("Yellowfin","Bigeye","Albacore","Bluefin"),col=c(1,2,3,4),pch=c(1,2,3,4))
  }
savePlot(filename="Catch by region allsp by yrqtr log1",type="png")

windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  swo <- tapply(llv$swo,llv$yrqtr,sum)
  mls <- tapply(llv$mls,llv$yrqtr,sum)
  blm <- tapply(llv$blm,llv$yrqtr,sum)
  bum <- tapply(llv$bum,llv$yrqtr,sum)
  sfa <- tapply(llv$sfa,llv$yrqtr,sum)
  maxy <- max(c(swo,mls,blm,bum,sfa))
  plot(names(swo),swo+1,ylim=c(1,maxy),xlab="Year",ylab="Catch",main=paste("Region",r),xlim=range(dat$yrqtr),log="y")
  points(names(mls),mls+1,col=2,pch=2)
  points(names(blm),blm+1,col=3,pch=3)
  points(names(bum),bum+1,col=5,pch=5)
  points(names(sfa),sfa+1,col=5,pch=5)
  if(r==3) legend("topright",legend=c("Swordfish","Striped marlin","Black marlin","Blue marlin","Sailfish"),col=1:5,pch=1:5)
  }
savePlot(filename="Catch by region allsp by yrqtr log2",type="png")

windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  oth <- tapply(llv$oth,llv$yrqtr,sum)
  sha <- tapply(llv$sha,llv$yrqtr,sum)
  skj <- tapply(llv$skj,llv$yrqtr,sum)
  maxy <- max(c(oth,sha,skj))
  plot(names(oth),oth+1,ylim=c(1,maxy),xlab="Year",ylab="Catch",main=paste("Region",r),xlim=range(dat$yrqtr),log="y")
  points(names(sha),sha+1,col=2,pch=2)
  points(names(skj),skj+1,col=3,pch=3)
  if(r==3) legend("topright",legend=c("Other","Shark","Skipjack"),col=1:3,pch=1:3)
  }
savePlot(filename="Catch by region allsp by yrqtr log3",type="png")

#Catch
windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  yft <- tapply(llv$yft,llv$yrqtr,sum)
  bet <- tapply(llv$bet,llv$yrqtr,sum)
  alb <- tapply(llv$alb,llv$yrqtr,sum)
  bft <- tapply(llv$bft,llv$yrqtr,sum)
  maxy <- max(c(yft,bet,alb,bft))
  plot(names(yft),yft,ylim=c(1,maxy),xlab="Year",ylab="Catch",main=paste("Region",r),xlim=range(dat$yrqtr))
  points(names(bet),bet,col=2,pch=2)
  points(names(alb),alb,col=3,pch=3)
#  points(names(swo),swo+1,col=4,pch=4)
  points(names(bft),bft,col=4,pch=4)
  if(r==3) legend("topright",legend=c("Yellowfin","Bigeye","Albacore", "Bluefin"),col=c(1,2,3,4),pch=c(1,2,3,4))
  }
savePlot(filename="Catch by region allsp by yrqtr 1",type="png")

windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  swo <- tapply(llv$swo,llv$yrqtr,sum)
  mls <- tapply(llv$mls,llv$yrqtr,sum)
  blm <- tapply(llv$blm,llv$yrqtr,sum)
  bum <- tapply(llv$bum,llv$yrqtr,sum)
  sfa <- tapply(llv$sfa,llv$yrqtr,sum)
  maxy <- max(c(swo,mls,blm,bum,sfa))
  plot(names(swo),swo+1,ylim=c(1,maxy),xlab="Year",ylab="Catch",main=paste("Region",r),xlim=range(dat$yrqtr))
  points(names(mls),mls,col=2,pch=2)
  points(names(blm),blm,col=3,pch=3)
  points(names(bum),bum,col=5,pch=5)
  points(names(sfa),sfa,col=5,pch=5)
  if(r==3) legend("topright",legend=c("Swordfish","Striped marlin","Black marlin","Blue marlin","Sailfish"),col=1:5,pch=1:5)
  }
savePlot(filename="Catch by region allsp by yrqtr 2",type="png")

windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  oth <- tapply(llv$oth,llv$yrqtr,sum)
  sha <- tapply(llv$sha,llv$yrqtr,sum)
  skj <- tapply(llv$skj,llv$yrqtr,sum)
  maxy <- max(c(oth,sha,skj))
  plot(names(oth),oth,ylim=c(1,maxy),xlab="Year",ylab="Catch",main=paste("Region",r),xlim=range(dat$yrqtr))
  points(names(sha),sha,col=2,pch=2)
  points(names(skj),skj,col=3,pch=3)
  if(r==3) legend("topright",legend=c("Other","Shark","Skipjack"),col=1:3,pch=1:3)
  }
savePlot(filename="Catch by region allsp by yrqtr 3",type="png")

### CPUE
windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  eff <- tapply(llv$hooks,llv$yrqtr,sum)
  yft <- tapply(llv$yft,llv$yrqtr,sum)
  bet <- tapply(llv$bet,llv$yrqtr,sum)
  alb <- tapply(llv$alb,llv$yrqtr,sum)
  bft <- tapply(llv$bft,llv$yrqtr,sum)
  yft <- 100*yft/eff
  bet <- 100*bet/eff
  alb <- 100*alb/eff
  bft <- 100*bft/eff
  maxy <- max(c(yft,bet,alb,bft))
  plot(names(yft),yft,ylim=c(1e-5,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("Region",r),log="y",xlim=range(dat$yrqtr))
  points(names(bet),bet,col=2,pch=2)
  points(names(alb),alb,col=3,pch=3)
  points(names(bft),bft,col=4,pch=4)
  if(r==3) legend("topright",legend=c("Yellowfin","Bigeye","Albacore","Bluefin"),col=c(1,2,3,4),pch=c(1,2,3,4))
  }
savePlot(filename="CPUE by region allsp by yrqtr log1",type="png")

windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  eff <- tapply(llv$hooks,llv$yrqtr,sum)
  swo <- tapply(llv$swo,llv$yrqtr,sum)
  mls <- tapply(llv$mls,llv$yrqtr,sum)
  blm <- tapply(llv$blm,llv$yrqtr,sum)
  bum <- tapply(llv$bum,llv$yrqtr,sum)
  sfa <- tapply(llv$sfa,llv$yrqtr,sum)
  swo <- 100*swo/eff
  mls <- 100*mls/eff
  blm <- 100*blm/eff
  bum <- 100*bum/eff
  sfa <- 100*sfa/eff
  maxy <- max(c(swo,mls,blm,bum,sfa))
  plot(names(swo),swo,ylim=c(1e-5,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("Region",r),log="y",xlim=range(dat$yrqtr))
  points(names(mls),mls,col=2,pch=2)
  points(names(blm),blm,col=3,pch=3)
  points(names(bum),bum,col=4,pch=4)
  points(names(sfa),sfa,col=5,pch=5)
  if(r==3) legend("topright",legend=c("Swordfish","Striped marlin","Black marlin","Blue marlin","Sailfish"),col=1:5,pch=1:5)
  }
savePlot(filename="CPUE by region allsp by yrqtr log2",type="png")

windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  eff <- tapply(llv$hooks,llv$yrqtr,sum)
  oth <- tapply(llv$oth,llv$yrqtr,sum)
  sha <- tapply(llv$sha,llv$yrqtr,sum)
  skj <- tapply(llv$skj,llv$yrqtr,sum)
  oth <- 100*oth/eff
  skj <- 100*skj/eff
  sha <- 100*sha/eff
  maxy <- max(c(oth,sha,skj))
  plot(names(oth),oth,ylim=c(1e-5,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("Region",r),log="y",xlim=range(dat$yrqtr))
  points(names(sha),sha,col=2,pch=2)
  points(names(skj),skj,col=3,pch=3)
  if(r==3) legend("topright",legend=c("Other","Shark","Skipjack"),col=1:3,pch=1:3)
  }
savePlot(filename="CPUE by region allsp by yrqtr log3",type="png")

# CPUE by region
windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  yft <- tapply(llv$yft,llv$yrqtr,sum)
  eff <- tapply(llv$hooks,llv$yrqtr,sum)
  bet <- tapply(llv$bet,llv$yrqtr,sum)
  alb <- tapply(llv$alb,llv$yrqtr,sum)
  bft <- tapply(llv$bft,llv$yrqtr,sum)
  yft <- 100*yft/eff
  bet <- 100*bet/eff
  alb <- 100*alb/eff
  bft <- 100*bft/eff
  maxy <- max(c(yft,bet,alb,bft))
  plot(names(yft),yft,ylim=c(1e-4,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("Region",r),xlim=range(dat$yrqtr))
  points(names(bet),bet,col=2,pch=2)
  points(names(alb),alb,col=3,pch=3)
  points(names(bft),bft,col=4,pch=4)
  if(r==3) legend("topright",legend=c("Yellowfin","Bigeye","Albacore","bft"),col=c(1,2,3,4),pch=c(1,2,3,4))
}
savePlot(filename="CPUE nominal allsp by region by yrqtr ",type="png")

windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  eff <- tapply(llv$hooks,llv$yrqtr,sum)
  blm <- tapply(llv$blm,llv$yrqtr,sum)
  bum <- tapply(llv$bum,llv$yrqtr,sum)
  mls <- tapply(llv$mls,llv$yrqtr,sum)
  swo <- tapply(llv$swo,llv$yrqtr,sum)
  sfa <- tapply(llv$sfa,llv$yrqtr,sum)
  blm <- 100*blm/eff
  bum <- 100*bum/eff
  mls <- 100*mls/eff
  swo <- 100*swo/eff
  sfa <- 100*sfa/eff
  maxy <- max(c(blm,bum,mls,swo,sfa))
  plot(names(swo),swo,ylim=c(0,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("Region",r),xlim=range(dat$yrqtr))
  points(names(mls),mls,col=2,pch=2)
  points(names(blm),blm,col=3,pch=3)
  points(names(bum),bum,col=4,pch=4)
  points(names(sfa),sfa,col=5,pch=5)
  if(r==3) legend("topright",legend=c("Swordfish","Striped marlin","Black marlin","Blue marlin","Sailfish"),col=c(1,2,3,4,5),pch=c(1,2,3,4,5))
}
savePlot(filename="CPUE nominal allsp by region by yrqtr 2",type="png")

windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  eff <- tapply(llv$hooks,llv$yrqtr,sum)
  oth <- tapply(llv$oth,llv$yrqtr,sum)
  sha <- tapply(llv$sha,llv$yrqtr,sum)
  skj <- tapply(llv$skj,llv$yrqtr,sum)
  oth <- 100*oth/eff
  skj <- 100*skj/eff
  sha <- 100*sha/eff
  maxy <- max(c(oth,sha,skj))
  plot(names(oth),oth,ylim=c(1e-5,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("Region",r),xlim=range(dat$yrqtr))
  points(names(sha),sha,col=2,pch=2)
  points(names(skj),skj,col=3,pch=3)
  if(r==3) legend("topright",legend=c("Other","Shark","Skipjack"),col=1:3,pch=1:3)
  }
savePlot(filename="CPUE nominal by region allsp by yrqtr 3",type="png")

## CPUE by region by fishingcat
#windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1),oma=c(0,0,3,0))
#for (fc in 1:2) {
#  for (r in regBord) {
#    llv <- dat[dat$regB==r & dat$newfishingcat==fc,]
#    yft <- tapply(llv$yft,llv$yrqtr,sum)
#    eff <- tapply(llv$hooks,llv$yrqtr,sum)
#    bet <- tapply(llv$bet,llv$yrqtr,sum)
#    alb <- tapply(llv$alb,llv$yrqtr,sum)
#    swo <- tapply(llv$swo,llv$yrqtr,sum)
#    yft <- 100*yft/eff
#    bet <- 100*bet/eff
#    alb <- 100*alb/eff
#    swo <- 100*swo/eff
#    maxy <- max(c(yft,bet,alb,swo))
#    plot(names(yft),yft,ylim=c(0,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("Region",r))
#    points(names(bet),bet,col=2,pch=2)
#    points(names(alb),alb,col=3,pch=3)
#    points(names(swo),swo,col=4,pch=4)
#    }
#  legend("topright",legend=c("Yellowfin","Bigeye","Albacore","Swordfish"),col=c(1,2,3,4),pch=c(1,2,3,4))
#  title(switch(fc,"Offshore","Distant water"),outer=T)
#  savePlot(filename=paste("CPUE nominal allsp by region by yrqtr",fc),type="png")
#  }

# 5 degree squares fished
dimu <- function(x) { length(unique(x)) }
windows(height=14,width=12)
par (mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB == r,]
  yq <- sort(unique(llv$yrqtr))
  strats <- tapply(paste(llv$lat5,llv$lon5),llv$yrqtr,dimu)
  plot(yq, strats, type="p", xlim=range(dat$yrqtr),pch=1,col=1,ylim=c(0,max(strats)),
           cex=1,ylab="5 x 5 spatial strata with reported effort",main=paste("Region",r))
#  mtext(side=3, paste("Region", r),line=0.5)
  }
savePlot("Number of spatial strata",type="png")

# Proportion sets with zero catches
windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  ay <- tapply(llv$yft==0,llv$yrqtr,sum)
  a <- tapply(llv$hooks,llv$yrqtr,length)
  ab <- tapply(llv$bet==0,llv$yrqtr,sum)
  ay <- 100*ay/a
  ab <- 100*ab/a
  maxy <- max(c(ay,ab))
  plot(names(ay),ay,xlim=range(dat$yrqtr),ylim=c(0,maxy),xlab="Year",ylab="Proportion of zero catches",main=paste("Region",r))
  points(names(ab),ab,col=2,pch=2)
  if(r==3) legend("topright",legend=c("Yellowfin","Bigeye"),col=c(1,2),pch=c(1,2))
  }
savePlot(filename="Proportion zeroes by region by yrqtr",type="png")

# Proportion sets with zero catches allspp
windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  ay <- tapply(llv$yft==0,llv$yrqtr,sum)
  a <- tapply(llv$hooks,llv$yrqtr,length)
  ab <- tapply(llv$bet==0,llv$yrqtr,sum)
  aalb <- tapply(llv$alb==0,llv$yrqtr,sum)
  abft <- tapply(llv$bft==0,llv$yrqtr,sum)
  ay <- 100*ay/a
  ab <- 100*ab/a
  aalb <- 100*aalb/a
  abft <- 100*abft/a
  maxy <- max(c(ay,ab,abft,aalb))
  plot(names(ay),ay,xlim=range(dat$yrqtr),ylim=c(0,maxy),xlab="Year",ylab="Proportion of zero catches",main=paste("Region",r))
  points(names(ab),ab,col=2,pch=2)
  points(names(aalb),aalb,col=3,pch=3)
  points(names(abft),abft,col=4,pch=4)
  if(r==3) legend("topright",legend=c("Yellowfin","Bigeye","Albacore","Bluefin"),col=c(1,2,3,4),pch=c(1,2,3,4))
    }
savePlot(filename="Proportion zeroes by region by yrqtr allspp",type="png")

windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  ay <- tapply(llv$yft==0,llv$yrqtr,sum)
  a <- tapply(llv$hooks,llv$yrqtr,length)
  ab <- tapply(llv$bet==0,llv$yrqtr,sum)
  aalb <- tapply(llv$alb==0,llv$yrqtr,sum)
  abft <- tapply(llv$bft==0,llv$yrqtr,sum)
  ay <- 100*ay/a
  ab <- 100*ab/a
  aalb <- 100*aalb/a
  abft <- 100*abft/a
  maxy <- max(c(ay,ab,abft,aalb))
  plot(names(ay),ay,xlim=range(dat$yrqtr),ylim=c(0,maxy),xlab="Year",ylab="Proportion of zero catches",main=paste("Region",r))
  points(names(ab),ab,col=2,pch=2)
  points(names(aalb),aalb,col=3,pch=3)
  points(names(abft),abft,col=4,pch=4)
  if(r==3) legend("topright",legend=c("Yellowfin","Bigeye","Albacore","Bluefin"),col=c(1,2,3,4),pch=c(1,2,3,4))
    }
savePlot(filename="Proportion zeroes by region by yrqtr allspp",type="png")

windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  aswo <- tapply(llv$swo==0,llv$yrqtr,sum)
  a <- tapply(llv$hooks,llv$yrqtr,length)
  amls <- tapply(llv$mls==0,llv$yrqtr,sum)
  ablm <- tapply(llv$blm==0,llv$yrqtr,sum)
  abum <- tapply(llv$bum==0,llv$yrqtr,sum)
  asfa <- tapply(llv$sfa==0,llv$yrqtr,sum)
  aswo <- 100*aswo/a
  amls <- 100*amls/a
  ablm <- 100*ablm/a
  abum <- 100*abum/a
  asfa <- 100*asfa/a
  maxy <- max(c(aswo,amls,abum,ablm,asfa))
  plot(names(aswo),aswo,xlim=range(dat$yrqtr),ylim=c(0,maxy),xlab="Year",ylab="Proportion of zero catches",main=paste("Region",r))
  points(names(amls),amls,col=2,pch=2)
  points(names(ablm),ablm,col=3,pch=3)
  points(names(abum),abum,col=4,pch=4)
  points(names(asfa),asfa,col=5,pch=5)
  if(r==3) legend("topright",legend=c("Swordfish","Striped marlin","Blue marlin","Black marlin","Sailfish"),col=1:5,pch=1:5)
    }
savePlot(filename="Proportion zeroes by region by yrqtr spp2",type="png")

windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1))
for (r in regBord) {
  llv <- dat[dat$regB==r,]
  a <- tapply(llv$hooks,llv$yrqtr,length)
  oth <- tapply(llv$oth==0,llv$yrqtr,sum)
  sha <- tapply(llv$sha==0,llv$yrqtr,sum)
  skj <- tapply(llv$skj==0,llv$yrqtr,sum)
  sfa <- tapply(llv$sfa==0,llv$yrqtr,sum)
  oth <- 100*oth/a
  sha <- 100*sha/a
  skj <- 100*skj/a
  sfa <- 100*sfa/a
  maxy <- max(c(oth,sha,skj,sfa))
  plot(names(oth),oth,xlim=range(dat$yrqtr),ylim=c(0,maxy),xlab="Year",ylab="Proportion of zero catches",main=paste("Region",r))
  points(names(sha),sha,col=2,pch=2)
  points(names(skj),skj,col=3,pch=3)
  points(names(sfa),sfa,col=4,pch=4)
  if(r==3) legend("topright",legend=c("Other species","Sharks","Skipjack","Sailfish"),col=1:4,pch=1:4)
    }
savePlot(filename="Proportion zeroes by region by yrqtr spp3",type="png")

## Proportion sets with zero catches allspp by fishingcat
#windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1),oma=c(0,0,3,0))
#for (fc in 1:2) {
#  for (r in regBord) {
#    llv <- dat[dat$regB==r & dat$newfishingcat==fc,]
#    ay <- tapply(llv$yft==0,llv$yrqtr,sum)
#    a <- tapply(llv$hooks,llv$yrqtr,length)
#    ab <- tapply(llv$bet==0,llv$yrqtr,sum)
#    aalb <- tapply(llv$alb==0,llv$yrqtr,sum)
#    aswo <- tapply(llv$swo==0,llv$yrqtr,sum)
#    ay <- 100*ay/a
#    ab <- 100*ab/a
#    aalb <- 100*aalb/a
#    aswo <- 100*aswo/a
#    maxy <- max(c(ay,ab,aswo,aalb))
#    plot(names(ay),ay,ylim=c(0,maxy),xlab="Year",ylab="Proportion of zero catches",main=paste("Region",r))
#    points(names(ab),ab,col=2,pch=2)
#    points(names(aalb),aalb,col=3,pch=3)
#    points(names(aswo),aswo,col=4,pch=4)
#    }
#  legend("topright",legend=c("Yellowfin","Bigeye","Albacore","Swordfish"),col=c(1,2,3,4),pch=c(1,2,3,4))
#  title(switch(fc,"Offshore","Distant water"),outer=T)
#  savePlot(filename=paste("Proportion zeroes by region by yrqtr allspp",fc),type="png")
#}

# Maps of effort through time
library(maps)
library(mapproj)
library(mapdata)
windows(width=20,height=14); par(mfrow=c(3,3),mar=c(3,4,2,1))
for(yr in seq(1975,2015,by=5)) {
  plot_AO(plot_title=yr,sp="Effort")
  a <- dat[dat$yrqtr > yr & dat$yrqtr < yr+5,]
  lats <- sort(unique(a$lat5))
  lons <- sort(unique(a$lon5))
  a <- table(a$lat5,a$lon5)
  for (i in 1:length(lats)) {
    symbols(lons+2.5,rep(lats[i],length(lons))+2.5,circles=sqrt(a[i,])/20,col=1,add = T, inches =F)
    }
  }
savePlot("Plot map sets", type="png")

#for (fc in 1:2) {
#  windows(width=14,height=12); par(mfcol=c(2,1),mar=c(3,4,2,1),oma=c(0,0,3,0))
#  a2 <- dat[dat$newfishingcat==fc,]
#  for(yr in seq(1950,2009,by=10)) {
#    plot.pacific(plot_title=yr)
#    a <- a2[a2$yrqtr > yr & a2$yrqtr < yr+10,]
#    lats <- sort(unique(a$lat5))
#    lons <- sort(unique(a$lon5))
#    a <- table(a$lat5,a$lon5)
#    for (i in 1:length(lats)) {
#      symbols(lons+2.5,rep(lats[i],length(lons))+2.5,circles=sqrt(a[i,])/30,col=1,add = T, inches =F)
#      }
#    }
#  title(switch(fc,"Offshore","Distant water"),outer=T)
#  savePlot(paste("Plot map sets",fc), type="png")
#  }
# #   symbols(pp,rep(i,length(pp)),sqrt(pp2)/40, add = T, inches =FALSE)

# Maps of mean HPB through time
windows(width=20,height=14); par(mfrow=c(3,3),mar=c(3,4,2,1))
for(yr in seq(1975,2015,by=5)) {
  plot_AO(plot_title=yr,sp="YFT")
  a <- dat[dat$yrqtr > yr & dat$yrqtr < yr+5,]
  lats <- sort(unique(a$lat5))
  lons <- sort(unique(a$lon5))
  a <- tapply(a$hbf,list(a$lat5,a$lon5),mean,na.rm=T)
  for (i in 1:length(lats)) {
    text(lons+2.5,rep(lats[i],length(lons))+2.5,floor(a[i,]),col=1,add = T)
    }
  }
savePlot("Plot map mean HBF", type="png")

## Maps of mean HPB through time by FC
#for (fc in 1:2) {
#  windows(width=14,height=12); par(mfcol=c(2,1),mar=c(3,4,2,1),oma=c(0,0,3,0))
#  a2 <- dat[dat$newfishingcat==fc,]
#  for(yr in seq(1950,2002,by=10)) {
#    plot.pacific(plot_title=yr)
#    a <- a2[a2$yrqtr > yr & a2$yrqtr < yr+10,]
#    lats <- sort(unique(a$lat5))
#    lons <- sort(unique(a$lon5))
#    a <- tapply(a$hbf,list(a$lat5,a$lon5),mean,na.rm=T)
#    for (i in 1:length(lats)) {
#      text(lons+2.5,rep(lats[i],length(lons))+2.5,floor(a[i,]),col=1,add = T)
#      }
#    }
#  title(switch(fc,"Offshore","Distant water"),outer=T)
#  savePlot(paste("Plot map mean HBF",fc), type="png")
#  }

# Maps of median HPB through time
windows(width=20,height=14); par(mfrow=c(3,3),mar=c(3,4,2,1))
for(yr in seq(1975,2015,by=5)) {
  plot_AO(plot_title=yr,sp="YFT")
  a <- dat[dat$yrqtr > yr & dat$yrqtr < yr+10,]
  lats <- sort(unique(a$lat5))
  lons <- sort(unique(a$lon5))
  a <- tapply(round(a$hbf,0),list(a$lat5,a$lon5),median,na.rm=T)
  for (i in 1:length(lats)) {
    text(lons+2.5,rep(lats[i],length(lons))+2.5,(a[i,]),col=1)
    }
  }
savePlot("Plot map median HBF", type="png")

# Plot hbf by region by year
  windows(height=14,width=12); par(mfcol=c(2,1),mar=c(3,4,2,1),oma=c(1,1,3,1)); r<-3; hh <- 18
  b<- dat
  for (r in regBord) {
    a <- b[b$regB==r,]
    yrs <- sort(unique(a$yr))
    a <- table(floor(a$hbf),a$yr)
    plot(1,1, type="n", xlab="Year", ylab="HBF", ylim = c(1,25), xlim=range(dat$yrqtr),main=paste("Region",r))
    ilist <- as.numeric(row.names(a))
    for(i in 1:length(ilist)){
      symbols(yrs , rep(ilist[i], length(yrs)), sqrt(a[i,]) / max(sqrt(a)), add = T, inches =FALSE)
      if(trunc(i/5) == i/5){
      lines(c(1950,2009), rep(i,2), lty=3)}
      }
    }
  savePlot(paste("plot hbf by region by yr"),type="png")

# Proportion zero by HBF by region by year
countzero <- function(a) {
  z <- sum(a==0)
  tot <- length(a)
  pz <- z/tot ; return(pz)
  }

windows(height=14,width=12); par(mfcol=c(2,1),mar=c(4,4,2,2))
for (r in regBord) {
  a <- dat[dat$regB==r,]
  yrs <- sort(unique(a$op_yr))
  a <- tapply(a$bet,list(a$hbf,a$op_yr),countzero)
  plot(1,1, type="n", xlab="Year", ylab="HBF", ylim = c(1,25), xlim=range(dat$yrqtr),main=paste("Region",r))
  ilist <- as.numeric(row.names(a))
  for(i in 1:length(ilist)){
    symbols(yrs , rep(ilist[i], length(yrs)), .8*sqrt(a[i,]), add = T, inches =FALSE)
    }
  }
savePlot("plot pzero bet by hbf by region by yr",type="png")

windows(height=14,width=12); par(mfcol=c(2,1),mar=c(4,4,2,2))
for (r in regBord) {
  a <- dat[dat$regB==r,]
  yrs <- sort(unique(a$op_yr))
  a <- tapply(a$yft,list(a$hbf,a$op_yr),countzero)
  plot(1,1, type="n", xlab="Year", ylab="HBF", ylim = c(1,25), xlim=range(dat$yrqtr),main=paste("Region",r))
  ilist <- as.numeric(row.names(a))
  for(i in 1:length(ilist)){
    symbols(yrs , rep(ilist[i], length(yrs)), .8*sqrt(a[i,]), add = T, inches =FALSE)
    }
  }
savePlot("plot pzero yft by hbf by region by yr",type="png")

windows(height=14,width=12); par(mfcol=c(2,1),mar=c(4,4,2,2))
for (r in regBord) {
  a <- dat[dat$regB==r,]
  yrs <- sort(unique(a$op_yr))
  a <- tapply(a$alb,list(a$hbf,a$op_yr),countzero)
  plot(1,1, type="n", xlab="Year", ylab="HBF", ylim = c(1,25), xlim=range(dat$yrqtr),main=paste("Region",r))
  ilist <- as.numeric(row.names(a))
  for(i in 1:length(ilist)){
    symbols(yrs , rep(ilist[i], length(yrs)), .8*sqrt(a[i,]), add = T, inches =FALSE)
    }
  }
savePlot("plot pzero alb by hbf by region by yr",type="png")

windows(height=14,width=12); par(mfcol=c(2,1),mar=c(4,4,2,2))
for (r in regBord) {
  a <- dat[dat$regB==r,]
  yrs <- sort(unique(a$op_yr))
  a <- tapply(a$swo,list(a$hbf,a$op_yr),countzero)
  plot(1,1, type="n", xlab="Year", ylab="HBF", ylim = c(1,25), xlim=range(dat$yrqtr),main=paste("Region",r))
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
  plot(1,1, type="n", xlab="Year", ylab="HBF", ylim = c(1,23), xlim=range(dat$yrqtr),main=paste(la+2.5,lo+2.5,sep=", "))
  ilist <- as.numeric(row.names(a))
  if(length(ilist) > 0) {
    for(i in 1:length(ilist)){
      symbols(yrs , rep(ilist[i], length(yrs)), sqrt(a[i,]), add = T, inches =FALSE)
      }
    }
  }

for(sp in c("alb","bet","blm","bum","mls","oth","bft","sfa","sha","skj","swo","yft"))
  {
  names(dat)
  windows(height=14,width=24); par(mfrow=c(5,7),mar=c(2,2,1,0),oma=c(0,0,3,0))
  for (la in seq(7.5,-12.5,by=-5)) {
    for (lo in seq(-77.5,17.5,by=5)) {
      a <- dat[dat$lat5==la & dat$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_pzero_ll(a,sp,la,lo) else plot(1,1,type="n")
      }
    }
  title(sp,outer=T,line=1)
  savePlot(paste("plot pzero",sp,"by hbf by R2 latlong2"),type="png")
}

for(sp in c("alb","bet","blm","bum","mls","oth","bft","sfa","sha","skj","swo","yft"))
  {
  names(dat)
  windows(height=14,width=24); par(mfrow=c(5,8),mar=c(2,2,1,0),oma=c(0,0,3,0))
  for (la in seq(7.5,-12.5,by=-5)) {
    for (lo in seq(77.5,112.5,by=5)) {
      a <- dat[dat$lat5==la & dat$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_pzero_ll(a,sp,la,lo) else plot(1,1,type="n")
      }
    }
  title(sp,outer=T,line=1)
  savePlot(paste("plot pzero",sp,"by hbf by R5 latlong2"),type="png")
}


#R 1,2,5,6
pzero_hbf_sp_yq <- function(sp,laseq,loseq,fname,ti="") {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,]
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_pzero_ll(a,sp,la,lo) else plot(1,1,type="n")
      }
    }
  title(ti,outer=T)
  savePlot(fname,type="png")
  }
pzero_hbf_sp_yq("bet",laseq=seq(5,-15,-5)+2.5,loseq=seq(40,70,5)+2.5,fname="plot pzero bet by hbf by R2 latlong",ti="Bigeye Region 1")
pzero_hbf_sp_yq("yft",laseq=seq(5,-15,-5)+2.5,loseq=seq(40,70,5)+2.5,fname="plot pzero yft by hbf by R2 latlong",ti="Yellowfin Region 1")
pzero_hbf_sp_yq("alb",laseq=seq(5,-15,-5)+2.5,loseq=seq(40,70,5)+2.5,fname="plot pzero alb by hbf by R2 latlong",ti="Albacore Region 1")


#R 1,2,3,4,5,6
#load("alldatraw.RData")
#dat <- dataclean(dat,allHBF=T)
#dat <- dataprep(dat)
#dat <- dat[dat$regB > 0 & dat$regB <=6,c("tonnage","fishingcat","ncrew","target","mainline","branchline","op_yr","dat_mon","dat_day","lat","lon","hbf",
#          "hooks","bet","yft","alb","lat5","lon5","regB","subreg","vessid","yrqtr","latlong","cstart_yr","cstart_mon","cstart_day")]

#plot_hbf_5x5 <- function(a,la,lo) {
#  yrs <- sort(unique(a$op_yr))
#  a <- tapply(a$hooks,list(a$hbf,a$op_yr),sum)
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
#  swo <- tapply(a$swo,list(a$yrqtr),countzero)
  plot(1,1, type="n", xlab="yr", ylab="p(zero catch)", ylim = c(0,1), xlim=range(dat$yrqtr),main=paste(la,", ",lo,sep=""))
  points(yrs, yft,cex=cx)
  points(yrs,bet,col=2,pch=2,cex=cx)
  points(yrs,alb,col=3,pch=3,cex=cx)
#  points(yrs,swo,col=4,pch=4,cex=cx)
  }
pzero_all_sp_yq <- function(laseq,loseq,fname,ti="") {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,]
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_pzero_both(a,la,lo) else plot(1,1,type="n")
      }
    }
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

pzero_all_sp_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(40,70,5)+2.5,fname="plot pzero YBA by R2 latlong",ti="Probability of zero catch Region 2")
pzero_all_sp_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(75,110,5)+2.5,fname="plot pzero YBA by R5 latlong",ti="Probability of zero catch Region 5")
pzero_all_sp_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(20,60,5)+2.5,fname="plot pzero YBA by R3 latlong",ti="Probability of zero catch Region 3")
pzero_all_sp_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(65,120,5)+2.5,fname="plot pzero YBA by R4 latlong",ti="Probability of zero catch Region 4")

plot_pzero_billf <- function(a,la,lo) {
  cx <- 0.8
  yrs <- sort(unique(a$yrqtr))
  swo <- tapply(a$swo,list(a$yrqtr),countzero)
  mls <- tapply(a$mls,list(a$yrqtr),countzero)
  blm <- tapply(a$blm,list(a$yrqtr),countzero)
  bum <- tapply(a$bum,list(a$yrqtr),countzero)
  sfa <- tapply(a$sfa,list(a$yrqtr),countzero)
#  swo <- tapply(a$swo,list(a$yrqtr),countzero)
  plot(1,1, type="n", xlab="yr", ylab="p(zero catch)", ylim = c(0,1), xlim=range(dat$yrqtr),main=paste(la,", ",lo,sep=""))
  points(yrs, swo,cex=cx)
  points(yrs,mls,col=2,pch=2,cex=cx)
  points(yrs,blm,col=3,pch=3,cex=cx)
  points(yrs,bum,col=4,pch=4,cex=cx)
  points(yrs,sfa,col=5,pch=5,cex=cx)
#  points(yrs,swo,col=4,pch=4,cex=cx)
  }
pzero_billf_yq <- function(laseq,loseq,fname,ti="") {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,]
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_pzero_billf(a,la,lo) else plot(1,1,type="n")
      if(la==laseq[1] & lo==loseq[1]) legend("topright",legend=c("SWO","MLS","BLM","BUM","SFA"),col=1:5,pch=1:5)
      }
    }
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

pzero_billf_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(40,70,5)+2.5,fname="plot pzero billf by R2 latlong",ti="Probability of zero catch Region 2")
pzero_billf_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(75,110,5)+2.5,fname="plot pzero billf by R5 latlong",ti="Probability of zero catch Region 5")
pzero_billf_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(20,60,5)+2.5,fname="plot pzero billf by R3 latlong",ti="Probability of zero catch Region 3")
pzero_billf_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(65,120,5)+2.5,fname="plot pzero billf by R4 latlong",ti="Probability of zero catch Region 4")

plot_pzero_sha <- function(a,la,lo) {
  cx <- 0.8
  yrs <- sort(unique(a$yrqtr))
  oth <- tapply(a$oth,list(a$yrqtr),countzero)
  sha <- tapply(a$sha,list(a$yrqtr),countzero)
  skj <- tapply(a$skj,list(a$yrqtr),countzero)
#  swo <- tapply(a$swo,list(a$yrqtr),countzero)
  plot(1,1, type="n", xlab="yr", ylab="p(zero catch)", ylim = c(0,1), xlim=range(dat$yrqtr),main=paste(la,", ",lo,sep=""))
  points(yrs, oth,cex=cx)
  points(yrs,sha,col=2,pch=2,cex=cx)
  points(yrs,skj,col=3,pch=3,cex=cx)
#  points(yrs,swo,col=4,pch=4,cex=cx)
  }
pzero_all_sha <- function(laseq,loseq,fname,ti="") {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,]
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_pzero_sha(a,la,lo) else plot(1,1,type="n")
      if(la==laseq[1] & lo==loseq[1]) legend("topright",legend=c("OTH","SHA","SKJ"),col=1:3,pch=1:3)
      }
    }
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

pzero_all_sha(laseq=seq(5,-15,-5)+2.5,loseq=seq(40,70,5)+2.5,fname="plot pzero SHA by R2 latlong",ti="Probability of zero catch Region 2")
pzero_all_sha(laseq=seq(5,-15,-5)+2.5,loseq=seq(75,110,5)+2.5,fname="plot pzero SHA by R5 latlong",ti="Probability of zero catch Region 5")
pzero_all_sha(laseq=seq(-20,-40,-5)+2.5,loseq=seq(20,60,5)+2.5,fname="plot pzero SHA by R3 latlong",ti="Probability of zero catch Region 3")
pzero_all_sha(laseq=seq(-20,-40,-5+2.5),loseq=seq(65,120,5)+2.5,fname="plot pzero SHA by R4 latlong",ti="Probability of zero catch Region 4")


# Albacore catch distribution histograms
write.csv(table(dat$hbf,floor(dat$yrqtr/5)*5,dat$regB),file="hbf by region by 5 years.csv")
windows();par(mfcol=c(2,1))
for(i in regBord) {
  a <- dat[dat$regB==i,]
  hist(a$alb,nclass=50,xlab="Albacore catch",xlim=c(0,250),main=paste("Region",i))
}
savePlot("hist alb by regB",type="png")


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
  plot(1,1, type="n", xlab="Year", ylab="p(zero catch)", ylim = c(0,1), xlim=range(dat$yrqtr),main=paste(la,lo,sep=", "))
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
      if(dim(a)[[1]] >0)  plot_pcpue(a,la,lo,cpue) else plot(1,1,type="n")
      }
    }
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

pcpue_all_sp_yq(cpue=10,laseq=seq(5,-15,-5)+2.5,loseq=seq(40,70,5)+2.5,fname="plot pcpue allsp by R2 latlong",ti="Probability of cpue > 1/100 hooks Region 2")
pcpue_all_sp_yq(cpue=10,laseq=seq(5,-15,-5)+2.5,loseq=seq(75,110,5)+2.5,fname="plot pcpue allsp by R3 latlong",ti="Probability of cpue > 1/100 hooks Region 3")
pcpue_all_sp_yq(cpue=10,laseq=seq(-20,-40,-5)+2.5,loseq=seq(20,60,5)+2.5,fname="plot pcpue allsp by R4 latlong",ti="Probability of cpue > 1/100 hooks Region 4")
pcpue_all_sp_yq(cpue=10,laseq=seq(-20,-40,-5+2.5),loseq=seq(65,120,5)+2.5,fname="plot pcpue allsp by R5 latlong",ti="Probability of cpue > 1/100 hooks Region 5")


# Catch
dat$op_yr <- as.factor(dat$op_yr)
plot_catch <- function(a,la,lo,ymax) {
  cx <- 0.9
  yrs <- as.numeric(levels(a$op_yr))
  yft <- tapply(a$yft,list(a$op_yr),sum)
  bet <- tapply(a$bet,list(a$op_yr),sum)
  alb <- tapply(a$alb,list(a$op_yr),sum)
  bft <- tapply(a$bft,list(a$op_yr),sum)
  plot(1,1, type="n", xlab="Year", ylab="Catch (numbers)", ylim = c(0,ymax), xlim=range(dat$yrqtr),main=paste(la,lo,sep=", "))
  lines(yrs,yft,col=1,cex=cx)
  lines(yrs,bet,col=2,lty=2,cex=cx)
  lines(yrs,alb,col=3,lty=3,cex=cx)
  lines(yrs,bft,col=4,lty=4,cex=cx)
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
  legend("topright",legend=c("Yellowfin","Bigeye","Albacore","bft"),lty=c(1:4),col=c(1:4))
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

catch_all_sp_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(40,70,5)+2.5,ymax=60000,fname="plot catch YBA by R2 latlong",ti="Catch Region 2")
catch_all_sp_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(75,110,5)+2.5,ymax=20000,fname="plot catch YBA by R5 latlong",ti="Catch Region 5")
catch_all_sp_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(20,60,5)+2.5,ymax=20000,fname="plot catch YBA by R3 latlong",ti="Catch Region 3")
catch_all_sp_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(65,120,5)+2.5,ymax=10000,fname="plot catch YBA by R4 latlong",ti="Catch Region 4")

plot_catch <- function(a,la,lo,ymax) {
  cx <- 0.9
  yrs <- as.numeric(levels(a$op_yr))
  swo <- tapply(a$swo,list(a$op_yr),sum)
  bum <- tapply(a$bum,list(a$op_yr),sum)
  blm <- tapply(a$blm,list(a$op_yr),sum)
  mls <- tapply(a$mls,list(a$op_yr),sum)
  sfa <- tapply(a$sfa,list(a$op_yr),sum)
  plot(yrs,yrs, type="n", xlab="Year", ylab="Catch (numbers)", ylim = c(0,ymax), main=paste(la,lo,sep=", "))
  lines(yrs,swo,col=1,cex=cx)
  lines(yrs,bum,col=2,lty=2,cex=cx)
  lines(yrs,blm,col=3,lty=3,cex=cx)
  lines(yrs,mls,col=4,lty=4,cex=cx)
  lines(yrs,sfa,col=4,lty=4,cex=cx)
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
  legend("topright",legend=c("Swordfish","Blue marlin","Black marlin","Striped marlin","Sailfish"),lty=c(1:5),col=c(1:5))
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

catch_all_sp_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(40,70,5)+2.5,ymax=5000,fname="plot catch billf by R2 latlong",ti="Catch Region 2")
catch_all_sp_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(75,110,5)+2.5,ymax=1500,fname="plot catch billf by R5 latlong",ti="Catch Region 5")
catch_all_sp_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(20,60,5)+2.5,ymax=1000,fname="plot catch billf by R3 latlong",ti="Catch Region 3")
catch_all_sp_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(65,120,5)+2.5,ymax=1000,fname="plot catch billf by R4 latlong",ti="Catch Region 4")

plot_catch <- function(a,la,lo,ymax) {
  cx <- 0.9
  yrs <- as.numeric(levels(a$op_yr))
  oth <- tapply(a$oth,list(a$op_yr),sum)
  sha <- tapply(a$sha,list(a$op_yr),sum)
  skj <- tapply(a$skj,list(a$op_yr),sum)
  plot(yrs,yrs, type="n", xlab="Year", ylab="Catch (numbers)", ylim = c(0,ymax), main=paste(la,lo,sep=", "))
  lines(yrs,oth,col=1,cex=cx)
  lines(yrs,sha,col=2,lty=2,cex=cx)
  lines(yrs,skj,col=3,lty=3,cex=cx)
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
  legend("topright",legend=c("Other species","Sharks","Skipjack"),lty=c(1:3),col=c(1:3))
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

catch_all_sp_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(40,70,5)+2.5,ymax=2000,fname="plot catch SHA by R2 latlong",ti="Catch Region 2")
catch_all_sp_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(75,110,5)+2.5,ymax=1500,fname="plot catch SHA by R5 latlong",ti="Catch Region 5")
catch_all_sp_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(20,60,5)+2.5,ymax=2000,fname="plot catch SHA by R3 latlong",ti="Catch Region 3")
catch_all_sp_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(65,120,5)+2.5,ymax=5000,fname="plot catch SHA by R4 latlong",ti="Catch Region 4")

plot_effort <- function(a,la,lo,ymax) {
  cx <- 0.9
  yrs <- as.numeric(levels(a$op_yr))
  effort <- tapply(a$hooks,list(a$op_yr),sum)
  plot(1,1, type="n", xlab="Year", ylab="Catch (numbers)", ylim = c(0,ymax), xlim=range(dat$yrqtr),main=paste(la,lo,sep=", "))
  points(yrs,effort,col=1,cex=cx)
  }
effort_all_fc_yq <- function(laseq,loseq,fname,ti="",ymax) {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,]
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_effort(a,la,lo,ymax) else plot(1,1,type="n")
      }
    }
  legend("topright",legend=c("Offshore","Distant water"),lty=c(1,2),col=c(1,2))
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

effort_all_fc_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(40,70,5)+2.5,ymax=5000000,fname="plot effort YBA by R2 latlong",ti="Effort Region 2")
effort_all_fc_yq(laseq=seq(5,-15,-5)+2.5,loseq=seq(75,110,5)+2.5,ymax=2000000,fname="plot effort YBA by R5 latlong",ti="Effort Region 5")
effort_all_fc_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(20,60,5)+2.5,ymax=1000000,fname="plot effort YBA by R3 latlong",ti="Effort Region 3")
effort_all_fc_yq(laseq=seq(-20,-40,-5)+2.5,loseq=seq(65,120,5)+2.5,ymax=500000,fname="plot effort YBA by R4 latlong",ti="Effort Region 4")


# Median CPUE
plot_median_cpue <- function(a,la,lo) {
  cx=0.9
  yrs <- as.numeric(levels(a$yrqtr))
  alb <- tapply(a$alb/a$hooks,list(a$yrqtr),median)
  yft <- tapply(a$yft/a$hooks,list(a$yrqtr),median)
  bet <- tapply(a$bet/a$hooks,list(a$yrqtr),median)
  bft <- tapply(a$bft/a$hooks,list(a$yrqtr),median)
  plot(yrs,yrs, type="n", xlab="Year", ylab="p(zero catch)", ylim = c(0,0.04), main=paste(la,lo,sep=", "))
  points(yrs,yft,col=1,pch=1,cex=cx)
  points(yrs,bet,col=2,pch=2,cex=cx)
  points(yrs, alb,col=3,pch=3,cex=cx)
  points(yrs, bft,col=4,pch=4,cex=cx)
  }
median_all_sp_yq_fc <- function(cpue,laseq,loseq,fname,ti="",fc="both") {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,]
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
#  browser()
      if(dim(a)[[1]] >0)  plot_median_cpue(a,la,lo) else plot(1,1,type="n")
      }
    }
  legend("topright",legend=c("Yellowfin","Bigeye","Albacore","Bluefin"),col=c(1,2,3,4),pch=c(1,2,3,4))
  title(ti,outer=T)
  savePlot(fname,type="png")
  }
dat$yrqtr <- as.factor(dat$yrqtr)
median_all_sp_yq_fc(laseq=seq(5,-15,-5)+2.5,loseq=seq(40,70,5)+2.5,fname="plot median YBA by R2 latlong",ti="Median cpue Region 2")
median_all_sp_yq_fc(laseq=seq(5,-15,-5)+2.5,loseq=seq(75,110,5)+2.5,fname="plot median YBA by R5 latlong",ti="Median cpue Region 5")
median_all_sp_yq_fc(laseq=seq(-20,-40,-5)+2.5,loseq=seq(20,60,5)+2.5,fname="plot median YBA by R3 latlong",ti="Median cpue Region 3")
median_all_sp_yq_fc(laseq=seq(-20,-40,-5)+2.5,loseq=seq(65,120,5)+2.5,fname="plot median YBA by R4 latlong",ti="Median cpue Region 4")

# Mean CPUE
plot_mean_cpue <- function(a,la,lo) {
  cx=0.9
  yrs <- sort(unique(a$yrqtr))
  alb <- tapply(a$alb/a$hooks,list(a$yrqtr),mean)
  yft <- tapply(a$yft/a$hooks,list(a$yrqtr),mean)
  bet <- tapply(a$bet/a$hooks,list(a$yrqtr),mean)
  plot(1,1, type="n", xlab="Year", ylab="p(zero catch)", ylim = c(0,0.04), xlim=range(dat$yrqtr),main=paste(la,lo,sep=", "))
  points(yrs,yft,col=1,pch=1,cex=cx)
  points(yrs,bet,col=2,pch=2,cex=cx)
  points(yrs, alb,col=3,pch=3,cex=cx)
  }
mean_all_sp_yq_fc <- function(cpue,laseq,loseq,fname,ti="") {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,]
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_mean_cpue(a,la,lo) else plot(1,1,type="n")
      }
    }
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

mean_all_sp_yq_fc(laseq=seq(5,-15,-5)+2.5,loseq=seq(40,70,5)+2.5,fname="plot mean YBA by R2 latlong",ti="Mean cpue Region 2")
mean_all_sp_yq_fc(laseq=seq(5,-15,-5)+2.5,loseq=seq(75,110,5)+2.5,fname="plot mean YBA by R5 latlong",ti="Mean cpue Region 5")
mean_all_sp_yq_fc(laseq=seq(-20,-40,-5)+2.5,loseq=seq(20,60,5)+2.5,fname="plot mean YBA by R3 latlong",ti="Mean cpue Region 3")
mean_all_sp_yq_fc(laseq=seq(-20,-40,-5)+2.5,loseq=seq(65,120,5)+2.5,fname="plot mean YBA by R4 latlong",ti="Mean cpue Region 4")

