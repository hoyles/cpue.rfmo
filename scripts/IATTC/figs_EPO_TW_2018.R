regBWord <- c(1,2,3,4,5,6)
regBEord <- c(1,2,3,4)
regBPOord <- c(1,2,15,3,4,11,12,5,6,13,14)
#####################################

map_EPO <- function() {
  axis(1, at = c(160, 210, 260), labels = c(-200, -150, -100))
  map("world2", add = T, interior=F, fill = TRUE)
  abline(v=210, col = "slate grey", lwd = 2, lty=1)
  lines(c(250, 250), c(-70, 25), lwd = 2, col = "slate grey", lty = 1)
  lines(c(110, 280), c(-10, -10), lwd = 2, col = "slate grey", lty = 1)
  lines(c(210, 250), c(10, 10), lwd = 2, col = "slate grey", lty = 1)
  lines(c(140, 210), c(-40, -40), lwd = 2, col = "slate grey", lty = 1)
  lines(c(140, 150), c(-20, -20), lwd = 2, col = "slate grey", lty = 1)
  lines(c(140, 150), c(-15, -15), lwd = 2, col = "slate grey", lty = 1)
  lines(c(155, 160), c( -5,  -5), lwd = 2, col = "slate grey", lty = 1)
  lines(c(140, 155), c(  0,   0), lwd = 2, col = "slate grey", lty = 1)
  lines(c(140, 210), c( 10,  10), lwd = 2, col = "slate grey", lty = 1)
  lines(c(110, 140), c( 20,  20), lwd = 2, col = "slate grey", lty = 1)
  lines(c(120, 210), c( 50,  50), lwd = 2, col = "slate grey", lty = 1)
  lines(c(110, 110), c(-10,  20), lwd = 2, col = "slate grey", lty = 1)
  lines(c(120, 120), c( 20,  26), lwd = 2, col = "slate grey", lty = 1)
  lines(c(140, 140), c(-40,  20), lwd = 2, col = "slate grey", lty = 1)
  lines(c(150, 150), c(-20, -15), lwd = 2, col = "slate grey", lty = 1)
  lines(c(155, 155), c(-5,    0), lwd = 2, col = "slate grey", lty = 1)
  lines(c(160, 160), c(-10,  -5), lwd = 2, col = "slate grey", lty = 1)
  lines(c(170, 170), c(-40,  50), lwd = 2, col = "slate grey", lty = 1)
  lines(c(210, 210), c(-40,  50), lwd = 2, col = "slate grey", lty = 1)
  text(c(140,190,150,185,155,190,130,143,143), c(25,25,5,0,-30,-30,10,-4.5,-15.6), labels = 1:9, cex=1.6, font = 2, col = 4)
  text(c(230,260,230,260,230), c(0,0,-30,-30,25), labels = c(1:4,0), cex=1.6, font = 2, col = 1)
}

a <- log(table(dat$lon5,dat$lat5))
windows(width=15,height=10)
image(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,xlab="Longitude",ylab="Latitude", xaxt="n", col = rev(brewer.pal(n=7, name = 'RdGy')))
map_EPO()
savePlot("Setmap_logscale.png",type="png")

a <- with(dat[!is.na(dat$lat) & dat$yrqtr >= 2010,],log(table(lonf,latf)))
windows(width=15,height=10)
image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,xlab="Longitude",ylab="Latitude", xaxt = "n", col = )
map_EPO()
savePlot("Setmap_logscale_2010-present.png",type="png")

a <- with(dat[!is.na(dat$lat) & dat$yrqtr & dat$yrqtr >= 1990,],log(table(lonf,latf)))
windows(width=15,height=10)
#image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,xlab="Longitude",ylab="Latitude", xaxt = "n", col = (brewer.pal(n=7, name = 'YlOrRd')))
image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,xlab="Longitude",ylab="Latitude", xaxt = "n")
map_EPO()
savePlot("Setmap_logscale_1990-present.png",type="png")


a <- aggregate(regBall ~ lonf + latf, data = dat[!is.na(dat$lat) & dat$yrqtr,], FUN = Mode)
windows(width=20, height=15)
plot(1:5, 1:5, type="n", xlim = range(defactor(a$lonf)), ylim = range(defactor(a$latf)), xaxt="n")
text(defactor(a$lonf), defactor(a$latf), a$regB, cex = 0.5)
#image(as.numeric(dimnames(a)[[1]])+.5, as.numeric(dimnames(a)[[2]])+.5, a, col=2:6, xlab="Longitude", ylab="Latitude", xaxt = "n")
map_EPO()
savePlot("regbet.png",type="png")

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

windows(width=15,height=10);par(mfrow=c(1,2))
dat_epo <- dat[dat$lon > 210,]
ax <- tapply(dat_epo$yrqtr,dat_epo$yrqtr,mean); ay=tapply(dat_epo$lat5,dat_epo$yrqtr,mean)
plot(ax,ay,xlab="yr",ylab="Mean latitude",type="n")
a <- 4*(.125+dat_epo$yrqtr-floor(dat_epo$yrqtr))
a <- tapply(a,dat_epo$yrqtr,mean)
text(ax,ay,a,cex=0.7)
ax=tapply(dat_epo$lon5,dat_epo$yrqtr,mean);ay=tapply(dat_epo$yrqtr,dat_epo$yrqtr,mean)
plot(ax,ay,ylab="yr",xlab="Mean longitude",type="n")
text(ax,ay,a,cex=0.7)
savePlot("mean_fishing_location_EPO1.png",type="png")

write.csv(table(round(dat$hbf,0),dat$regB,useNA="always"),file="hbf by region.csv")
write.csv(table(round(dat$hbf,0),floor(dat$yrqtr/5)*5,dat$regB,useNA="always"),file="hbf by region by 5 years.csv")

windows(20,14);par(mfrow=c(2,3), mar = c(4,4,3,1)+.1)
for(y in seq(1995,2015,5)) {
  a <- dat[floor(dat$yrqtr/5)*5==y ,]
  a <- tapply(a$hbf,list(a$lon5,a$lat5),mean,na.rm=T)
  image(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,main=y,zlim=c(6,24),col=heat.colors(30),xlab="Lon",ylab="Lat",xlim=c(140,290),ylim=c(-45,40),xaxt="n")
  contour(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,add=T,levels=seq(0,26,2))
  map_EPO()
}
savePlot("mean_HBF.png",type="png")

qqs <- c(0.125,0.375,0.625,0.875)
for(qq in 1:4) {
  windows(20,14);par(mfrow=c(2,3), mar = c(4,4,3,1)+.1)
  for(y in seq(1995,2015,5)) {
    a <- dat[dat$yrqtr %in% (qqs[qq]+y:(y+4)),]
    a <- tapply(a$hbf,list(a$lon5,a$lat5),mean,na.rm=T)
    image(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,main=y,zlim=c(6,24),col=heat.colors(30),xlab="Lon",ylab="Lat",xlim=c(140,290),ylim=c(-45,40),xaxt="n")
    contour(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,add=T,levels=seq(0,26,2))
    map_EPO()
    title(paste("Quarter",qq),outer=T,line=-1)
  }
  savePlot(paste0("mean_HBF_q",qq,".png"),type="png")
  }


#write.csv(table(dat$ncrew,dat$reg),file="crew by region.csv")
#write.csv(table(dat$ncrew,floor(dat$yrqtr/10)*10),file="crew by decade.csv")
#write.csv(table(dat$ncrew,dat$fishingcat,useNA="ifany"),file="crew by fishingcat.csv")
write.csv(table(dat$lat5,dat$lon5),file="ops by lat-long.csv")
write.csv(table(dat$lat5,dat$lon5,5*floor(dat$yrqtr/5)),file="ops by lat-long-5yr.csv")
# Species composition maps
a <-  aggregate(cbind(bet,yft,alb,ott,swo,mls,blm,bum,otb,sha,skj,oth,Total,Total2,hooks) ~ lon + lat + eval(5*floor((op_yr+5)/5)-5),data=dat[!is.na(dat$lon),],FUN=sum)
a5 <- aggregate(cbind(bet,yft,alb,ott,swo,mls,blm,bum,otb,sha,skj,oth,Total,Total2,hooks) ~ lon5 + lat5 + eval(10*floor((op_yr)/10)),data=dat,FUN=sum)
names(a)[3] <- names(a5)[3] <- "decade"
names(a5)[1:2] <- c("lon","lat")

windows(width=20,height=15);par(mfrow=c(2,2))
for(d in seq(1995,2010,5)) plot_catchmap(indat=a,vbl=a$bet/(a$bet+a$yft),dcd=d,latlim=c(-18,10),lonlim=c(150,290),ti="BET / BET + YFT")
savePlot("PropBET in YFT_BET5",type="png")
for(d in seq(1995,2010,5)) plot_catchmap(indat=a,vbl=a$bet/(a$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BET / Total")
savePlot("PropBET in Total",type="png")
for(d in seq(1995,2010,5)) plot_catchmap(indat=a,vbl=a$swo/(a$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SWO / Total",brk2=seq(0,1,.05))
savePlot("PropSWO in Total",type="png")
for(d in seq(1995,2010,5)) plot_catchmap(indat=a,vbl=a$alb/(a$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="ALB / Total",brk2=seq(0,1,.1))
savePlot("PropALB in Total",type="png")
for(d in seq(1995,2010,5)) plot_catchmap(indat=a,vbl=a$otb/(a$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SFA / Total",brk2=seq(0,1,.05))
savePlot("PropOTB in Total",type="png")
for(d in seq(1995,2010,5)) plot_catchmap(indat=a,vbl=a$sha/(a$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SHA / Total",brk2=seq(0,1,.05))
savePlot("PropSHA in Total",type="png")
for(d in seq(1995,2010,5)) plot_catchmap(indat=a,vbl=a$yft/(a$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="YFT / Total")
savePlot("PropYFT in Total",type="png")
for(d in seq(1995,2010,5)) plot_catchmap(indat=a,vbl=a$mls/(a$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="MLS / Total",brk2=seq(0,1,.05))
savePlot("PropMLS in Total",type="png")
for(d in seq(1995,2010,5)) plot_catchmap(indat=a,vbl=a$blm/(a$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BLM / Total",brk2=seq(0,1,.05))
savePlot("PropBLM in Total",type="png")
for(d in seq(1995,2010,5)) plot_catchmap(indat=a,vbl=a$bum/(a$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BUM / Total",brk2=seq(0,1,.05))
savePlot("PropBUM in Total",type="png")
for(d in seq(1995,2010,5)) plot_catchmap(indat=a,vbl=a$oth/(a$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="OTH / Total",brk2=seq(0,1,.05))
savePlot("PropOTH in Total",type="png")
for(d in seq(1995,2010,5)) plot_catchmap(indat=a,vbl=a$skj/(a$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SKJ / Total",brk2=seq(0,1,.05))
savePlot("PropSKJ in Total",type="png")
for(d in seq(1995,2010,5)) plot_catchmap(indat=a,vbl=a$ott/(a$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="OTT / Total",brk2=seq(0,1,.05))
savePlot("PropOTT in Total",type="png")

windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap(indat=a5,vbl=a5$bet/(a5$bet+a5$yft),dcd=d,latlim=c(-18,10),lonlim=c(150,290),ti="BET / BET + YFT")
savePlot("PropBET in YFT_BET5",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap(indat=a5,vbl=a5$bet/(a5$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BET / Total")
savePlot("PropBET in Total5",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap(indat=a5,vbl=a5$swo/(a5$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SWO / Total",brk2=seq(0,1,.05))
savePlot("PropSWO in Total5",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap(indat=a5,vbl=a5$alb/(a5$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="ALB / Total",brk2=seq(0,1,.1))
savePlot("PropALB in Total5",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap(indat=a5,vbl=a5$otb/(a5$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SFA / Total",brk2=seq(0,1,.05))
savePlot("PropOTB in Total5",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap(indat=a5,vbl=a5$sha/(a5$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SHA / Total",brk2=seq(0,1,.05))
savePlot("PropSHA in Total5",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap(indat=a5,vbl=a5$yft/(a5$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="YFT / Total")
savePlot("PropYFT in Total5",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap(indat=a5,vbl=a5$mls/(a5$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="MLS / Total",brk2=seq(0,1,.05))
savePlot("PropMLS in Total5",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap(indat=a5,vbl=a5$blm/(a5$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BLM / Total",brk2=seq(0,1,.05))
savePlot("PropBLM in Total5",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap(indat=a5,vbl=a5$bum/(a5$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BUM / Total",brk2=seq(0,1,.05))
savePlot("PropBUM in Total5",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap(indat=a5,vbl=a5$oth/(a5$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="OTH / Total",brk2=seq(0,1,.05))
savePlot("PropOTH in Total5",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap(indat=a5,vbl=a5$skj/(a5$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SKJ / Total",brk2=seq(0,1,.05))
savePlot("PropSKJ in Total5",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap(indat=a5,vbl=a5$ott/(a5$Total),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="OTT / Total",brk2=seq(0,1,.05))
savePlot("PropOTT in Total5",type="png")

########### relative to YBA
windows(width=20,height=15);par(mfrow=c(2,2),mar=c(2,2,2,2))
for(d in seq(1995,2010,5)) plot_catchmap(indat=a,vbl=a$bet/(a$Total2),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BET / YBA Total")
savePlot("PropBET in altTotal",type="png")
for(d in seq(1995,2010,5)) plot_catchmap(indat=a,vbl=a$alb/(a$Total2),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="ALB / YBA Total",brk2=seq(0,1,.1))
savePlot("PropALB in altTotal",type="png")
for(d in seq(1995,2010,5)) plot_catchmap(indat=a,vbl=a$yft/(a$Total2),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="YFT / YBA Total")
savePlot("PropYFT in altTotal",type="png")

windows(width=20,height=15);par(mfrow=c(2,3),mar=c(2,2,2,2))
for(d in seq(1970,2010,10)) plot_catchmap(indat=a5,vbl=a5$bet/(a5$Total2),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BET / YBA Total")
savePlot("PropBET in altTotal5",type="png")
windows(width=20,height=15);par(mfrow=c(2,3),mar=c(2,2,2,2))
for(d in seq(1970,2010,10)) plot_catchmap(indat=a5,vbl=a5$alb/(a5$Total2),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="ALB / YBA Total",brk2=seq(0,1,.1))
savePlot("PropALB in altTotal5",type="png")
windows(width=20,height=15);par(mfrow=c(2,3),mar=c(2,2,2,2))
for(d in seq(1970,2010,10)) plot_catchmap(indat=a5,vbl=a5$yft/(a5$Total2),dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="YFT / YBA Total")
savePlot("PropYFT in altTotal5",type="png")

#Catch maps
windows(width=20,height=15);par(mfrow=c(2,2))
for(d in seq(1995,2010,5)) plot_catchmap2(indat=a,vbl=a$bet,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BET Catch")
savePlot("Catchmap_BET",type="png")
for(d in seq(1995,2010,5)) plot_catchmap2(indat=a,vbl=a$swo,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SWO Catch")
savePlot("Catchmap_SWO",type="png")
for(d in seq(1995,2010,5)) plot_catchmap2(indat=a,vbl=a$alb,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="ALB Catch")
savePlot("Catchmap_ALB",type="png")
for(d in seq(1995,2010,5)) plot_catchmap2(indat=a,vbl=a$otb,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SFA Catch")
savePlot("Catchmap_OTB",type="png")
for(d in seq(1995,2010,5)) plot_catchmap2(indat=a,vbl=a$sha,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SHA Catch")
savePlot("Catchmap_SHA",type="png")
for(d in seq(1995,2010,5)) plot_catchmap2(indat=a,vbl=a$yft,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="YFT Catch")
savePlot("Catchmap_YFT",type="png")
for(d in seq(1995,2010,5)) plot_catchmap2(indat=a,vbl=a$mls,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="MLS Catch")
savePlot("Catchmap_MLS",type="png")
for(d in seq(1995,2010,5)) plot_catchmap2(indat=a,vbl=a$blm,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BLM Catch")
savePlot("Catchmap_BLM",type="png")
for(d in seq(1995,2010,5)) plot_catchmap2(indat=a,vbl=a$bum,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BUM Catch")
savePlot("Catchmap_BUM",type="png")
for(d in seq(1995,2010,5)) plot_catchmap2(indat=a,vbl=a$oth,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="OTH Catch")
savePlot("Catchmap_OTH",type="png")
for(d in seq(1995,2010,5)) plot_catchmap2(indat=a,vbl=a$skj,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SKJ Catch")
savePlot("Catchmap_SKJ",type="png")
for(d in seq(1995,2010,5)) plot_catchmap2(indat=a,vbl=a$ott,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="OTT Catch")
savePlot("Catchmap_OTT",type="png")

windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap2(indat=a5,vbl=a5$bet,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BET Catch", delta=5)
savePlot("Catchmap5_BET",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap2(indat=a5,vbl=a5$swo,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SWO Catch", delta=5)
savePlot("Catchmap5_SWO",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap2(indat=a5,vbl=a5$alb,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="ALB Catch", delta=5)
savePlot("Catchmap5_ALB",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap2(indat=a5,vbl=a5$otb,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SFA Catch", delta=5)
savePlot("Catchmap5_OTB",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap2(indat=a5,vbl=a5$sha,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SHA Catch", delta=5)
savePlot("Catchmap5_SHA",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap2(indat=a5,vbl=a5$yft,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="YFT Catch", delta=5)
savePlot("Catchmap5_YFT",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap2(indat=a5,vbl=a5$mls,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="MLS Catch", delta=5)
savePlot("Catchmap5_MLS",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap2(indat=a5,vbl=a5$blm,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BLM Catch", delta=5)
savePlot("Catchmap5_BLM",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap2(indat=a5,vbl=a5$bum,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BUM Catch", delta=5)
savePlot("Catchmap5_BUM",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap2(indat=a5,vbl=a5$oth,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="OTH Catch", delta=5)
savePlot("Catchmap5_OTH",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap2(indat=a5,vbl=a5$skj,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SKJ Catch", delta=5)
savePlot("Catchmap5_SKJ",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_catchmap2(indat=a5,vbl=a5$ott,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="OTT Catch", delta=5)
savePlot("Catchmap5_OTT",type="png")

#CPUE maps
windows(width=20,height=15);par(mfrow=c(2,2))
for(d in seq(1995,2010,5)) plot_cpuemap2(indat=a,vb1=a$bet,vb2=a$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BET CPUE")
savePlot("Cpuemap_BET",type="png")
for(d in seq(1995,2010,5)) plot_cpuemap2(indat=a,vb1=a$swo,vb2=a$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SWO CPUE")
savePlot("Cpuemap_SWO",type="png")
for(d in seq(1995,2010,5)) plot_cpuemap2(indat=a,vb1=a$alb,vb2=a$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="ALB CPUE")
savePlot("Cpuemap_ALB",type="png")
for(d in seq(1995,2010,5)) plot_cpuemap2(indat=a,vb1=a$otb,vb2=a$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SFA CPUE")
savePlot("Cpuemap_OTB",type="png")
for(d in seq(1995,2010,5)) plot_cpuemap2(indat=a,vb1=a$sha,vb2=a$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SHA CPUE")
savePlot("Cpuemap_SHA",type="png")
for(d in seq(1995,2010,5)) plot_cpuemap2(indat=a,vb1=a$yft,vb2=a$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="YFT CPUE")
savePlot("Cpuemap_YFT",type="png")
for(d in seq(1995,2010,5)) plot_cpuemap2(indat=a,vb1=a$mls,vb2=a$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="MLS CPUE")
savePlot("Cpuemap_MLS",type="png")
for(d in seq(1995,2010,5)) plot_cpuemap2(indat=a,vb1=a$blm,vb2=a$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BLM CPUE")
savePlot("Cpuemap_BLM",type="png")
for(d in seq(1995,2010,5)) plot_cpuemap2(indat=a,vb1=a$bum,vb2=a$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BUM CPUE")
savePlot("Cpuemap_BUM",type="png")
for(d in seq(1995,2010,5)) plot_cpuemap2(indat=a,vb1=a$oth,vb2=a$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="OTH CPUE")
savePlot("Cpuemap_OTH",type="png")
for(d in seq(1995,2010,5)) plot_cpuemap2(indat=a,vb1=a$skj,vb2=a$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SKJ CPUE")
savePlot("Cpuemap_SKJ",type="png")
for(d in seq(1995,2010,5)) plot_cpuemap2(indat=a,vb1=a$ott,vb2=a$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="OTT CPUE")
savePlot("Cpuemap_OTT",type="png")

windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_cpuemap2(indat=a5,vb1=a5$bet,vb2=a5$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BET CPUE", delta=5)
savePlot("CPUEmap5_BET",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_cpuemap2(indat=a5,vb1=a5$swo,vb2=a5$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SWO CPUE", delta=5)
savePlot("CPUEmap5_SWO",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_cpuemap2(indat=a5,vb1=a5$alb,vb2=a5$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="ALB CPUE", delta=5)
savePlot("CPUEmap5_ALB",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_cpuemap2(indat=a5,vb1=a5$otb,vb2=a5$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SFA CPUE", delta=5)
savePlot("CPUEmap5_OTB",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_cpuemap2(indat=a5,vb1=a5$sha,vb2=a5$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SHA CPUE", delta=5)
savePlot("CPUEmap5_SHA",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_cpuemap2(indat=a5,vb1=a5$yft,vb2=a5$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="YFT CPUE", delta=5)
savePlot("CPUEmap5_YFT",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_cpuemap2(indat=a5,vb1=a5$mls,vb2=a5$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="MLS CPUE", delta=5)
savePlot("CPUEmap5_MLS",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_cpuemap2(indat=a5,vb1=a5$blm,vb2=a5$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BLM CPUE", delta=5)
savePlot("CPUEmap5_BLM",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_cpuemap2(indat=a5,vb1=a5$bum,vb2=a5$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="BUM CPUE", delta=5)
savePlot("CPUEmap5_BUM",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_cpuemap2(indat=a5,vb1=a5$oth,vb2=a5$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="OTH CPUE", delta=5)
savePlot("CPUEmap5_OTH",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_cpuemap2(indat=a5,vb1=a5$skj,vb2=a5$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="SKJ CPUE", delta=5)
savePlot("CPUEmap5_SKJ",type="png")
windows(width=20,height=15);par(mfrow=c(2,3))
for(d in seq(1970,2010,10)) plot_cpuemap2(indat=a5,vb1=a5$ott,vb2=a5$hooks,dcd=d,latlim=c(-40,35),lonlim=c(150,290),ti="OTT CPUE", delta=5)
savePlot("CPUEmap5_OTT",type="png")

windows();par(mfrow=c(2,2))
a=(tapply(dat$alb/dat$hooks,dat$lat,mean))
plot(as.numeric(names(a)),a,xlab="Latitude",ylab="ALB CPUE")
a=tapply(dat$alb/length(unique(dat$op_yr)),dat$lat,sum)
plot(as.numeric(names(a)),a,xlab="Latitude",ylab="ALB catch per year")
a=tapply(dat$hooks,dat$lat,sum)
plot(as.numeric(names(a)),a,xlab="Latitude",ylab="Hooks")
savePlot("TW ALB by latitude",type="png")


#Prepare figures
# Logsheets per year by region
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,2,2,1),oma=c(0,0,1,0))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  hist(llv$dmy,breaks="days",freq=T,xlab="Date",main=paste0("",r))
  }
title("Sets per day",outer=T,line=0)
savePlot(filename=paste("Sets per day by region",sep=""),type="png")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,2,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  a <- table(llv$vessid,llv$op_yr)
  a <- apply(a>0,1,sum)
  print(table(a))

  a <- tapply(llv$op_yr,llv$op_yr,length)
  plot(as.numeric(names(a)),a,xlab="yr",ylab="Logsheet records",main=paste("Region",r),xlim=c(1975,2015))
  }
savePlot(filename=paste("Number of records",sep=""),type="png")

windows(height=12,width=14);par(mfrow=c(3,3))
for(dec in seq(1975,2015,5)) {
  a <- dat[dat$op_yr >= dec & dat$op_yr < dec+5 & dat$lon > 210,]
  hist(a$hooks,breaks=seq(-10,45010,50),xlim=c(0,5000),main=dec)
  }
savePlot(filename=paste0("Histogram hooks by decade"),type="png")

for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  windows(height=12,width=14);par(mfrow=c(3,3),oma=c(0,0,1,0))
  for(dec in seq(1975,2015,5)) {
    a <- llv[llv$op_yr >= dec & llv$op_yr < dec+5,]
    hist(a$hooks,breaks=seq(-10,45010,50),xlim=c(0,5000),main=dec)
    }
  title(paste0("Hooks per set ",r),outer=T,line=0)
  savePlot(filename=paste0("Histogram hooks by decade ",r),type="png")
}

# Vessels per year by region
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,2,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  a <- tapply(llv$vessid,llv$op_yr,lu)
  plot(names(a),a,xlab="Year",ylab="",main=paste("Region",r))
  }
savePlot(filename=paste("Unique vessels by year",sep=""),type="png")

# Time distribution of vessels
vy <- list()
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,2,2,1))
for (r in regBEord) {
  llv <- as.data.frame(dat[dat$regB==r,])
  vess <- as.numeric(unique(llv$vessid))
  minyr <- maxyr <- tonn <- vess
#  for (i in 1:length(vess)) {
#    minyr[i] <- min(llv[vess[i]==as.numeric(llv$vessid),]$op_yr)
#    maxyr[i] <- max(llv[vess[i]==as.numeric(llv$vessid),]$op_yr)
#    }
  vvv <- llv[order(llv$vessid,llv$op_yr),]
  minyr <- vvv[match(vess,as.numeric(vvv$vessid)),"op_yr"]
  tonn <- vvv[match(vess,as.numeric(vvv$vessid)),"tonnage"]
  vvv <- llv[order(llv$vessid,-llv$op_yr),]
  maxyr <- vvv[match(vess,as.numeric(vvv$vessid)),"op_yr"]
  vessyrs <- data.frame(vess=vess,minyr=as.numeric(minyr),maxyr=as.numeric(maxyr),tonn=tonn,stringsAsFactors=F)
  vessyrs <- vessyrs[order(-floor(vessyrs$minyr),-floor(vessyrs$maxyr)),]
  vy[[r]] <- vessyrs
  plot(1:length(vess),1:length(vess),xlim=c(1965,2020),type="n",xlab="Years",ylab="Vessel",main=paste("Region",r))
  for (i in 1:length(vess)) {
    lines(c(floor(vessyrs[i,]$minyr),floor(vessyrs[i,]$maxyr)),c(i,i))
    }
  }
savePlot(filename=paste("Time distribution of vessels 1",sep=""),type="png")

# Time distribution of vessels
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,2,2,1))
for (r in regBEord) {
  vessyrs <- vy[[r]]
  llv <- as.data.frame(dat[dat$regB==r,])
  vess <- unique(llv$vessid)
  plot(1:length(vess),1:length(vess),xlim=c(1965,2020),type="n",xlab="Years",ylab="Vessel",main=paste("Region",r))
  for (i in 1:length(vess)) {
    lines(c(floor(vessyrs[i,]$minyr),floor(vessyrs[i,]$maxyr)),c(i,i),col=as.numeric(vessyrs[i,]$tonn))
    }
  }
a <- as.numeric(unique(tonn))
legend("bottomleft",legend=levels(tonn)[a],col=a,lty=1)
savePlot(filename=paste("Time distribution of vessels 1b",sep=""),type="png")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,2,2,1))
for (r in regBEord) {
  vessyrs <- vy[[r]]
  vessyrs <- vessyrs[order(-floor(vessyrs$maxyr)),]
  llv <- as.data.frame(dat[dat$regB==r,])
  vess <- unique(llv$vessid)
  plot(1:length(vess),1:length(vess),xlim=c(1965,2020),type="n",xlab="Years",ylab="Vessel",main=paste("Region",r))
  for (i in 1:length(vess)) {
    lines(c(floor(vessyrs[i,]$minyr),floor(vessyrs[i,]$maxyr)),c(i,i))
    }
  }
savePlot(filename=paste("Time distribution of vessels 2",sep=""),type="png")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,2,2,1))
for (r in regBEord) {
  vessyrs <- vy[[r]]
  vessyrs <- vessyrs[order(-floor(vessyrs$maxyr)),]
  llv <- as.data.frame(dat[dat$regB==r,])
  vess <- unique(llv$vessid)
  plot(1:length(vess),1:length(vess),xlim=c(1965,2020),type="n",xlab="Years",ylab="Vessel",main=paste("Region",r))
  for (i in 1:length(vess)) {
    lines(c(floor(vessyrs[i,]$minyr),floor(vessyrs[i,]$maxyr)),c(i,i),col=as.numeric(vessyrs[i,]$tonn))
    }
  }
a <- as.numeric(unique(tonn))
legend("topright",legend=levels(tonn)[a],col=a,lty=1)
savePlot(filename=paste("Time distribution of vessels 2b",sep=""),type="png")


windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,2,2,1)) # Runs slow
for (r in regBEord) {
  vessyrs <- vy[[r]]
  llv <- as.data.frame(dat[dat$regB==r,])
  vess <- as.numeric(unique(llv$vessid))
  minyr <- vess
  plot(1:length(vess),1:length(vess),xlim=c(1965,2020),type="n",xlab="Years",ylab="Vessel",main=paste("Region",r))
  for (i in 1:length(vess)) {
    a <- floor(llv[vessyrs[i,1]==as.numeric(llv$vessid),]$op_yr)
    pp <- unique(a)
    pp2 <- tapply(a,a,length)
#    points(pp,rep(i,length(pp)),cex=0.6,pch=3)
    symbols(pp,rep(i,length(pp)),sqrt(pp2)/40, add = T, inches =FALSE)
    }
  }
savePlot(filename=paste("Time distribution of vessels 4",sep=""),type="png")

############

# Effort by region
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  a <- tapply(llv$hooks,llv$yrqtr,sum)
  plot(names(a),a,xlab="Year",ylab="Hooks",main=paste("Region",r),xlim=range(dat$yrqtr))
  }
savePlot(filename="Effort by region by yrqtr",type="png")

# Sets by region
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  a <- tapply(llv$hooks,llv$yrqtr,length)
  plot(names(a),a,xlab="Year",ylab="Sets",main=paste("Region",r),xlim=range(dat$yrqtr))
  }
savePlot(filename="Sets by region by yrqtr",type="png")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  a <- tapply(llv$hooks,llv$yrqtr,sum)
  plot(names(a),a,xlab="Year",ylab="Hooks",main=paste("Region",r),xlim=range(dat$yrqtr))
  }
savePlot(filename="Effort by region by yrqtr",type="png")

# Sets by region by yrqtr by fishingcat
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
allt <- sort(unique(dat$tonnage))
nallt <- as.numeric(allt)
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  a <- tapply(llv$hooks,llv$yrqtr,length)
  plot(as.numeric(names(a)),a,xlab="Year",ylab="Sets",main=paste("Region",r),col=1,pch=16)
  for (tt in allt) {
    llv2 <- llv[llv$tonnage==tt,]
    a2 <- tapply(llv2$hooks,llv2$yrqtr,length)
#    points(as.numeric(names(a2)),a2,col=nallt[match(tt,allt)],pch=nallt[match(tt,allt)])
    lines(as.numeric(names(a2)),a2,col=nallt[match(tt,allt)])
    }
  if(r==1) legend("topleft",legend=allt,col=nallt,lty=1)
  }
savePlot(filename="Sets by region by yrqtr by tonnage",type="png")


# Sets by region by yrqtr by fishingcat
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
allt <- sort(unique(dat$tonnage))
nallt <- as.numeric(allt)
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  a <- tapply(llv$hooks,llv$yrqtr,sum)
  plot(as.numeric(names(a)),a,xlab="Year",ylab="Hooks",main=paste("Region",r),col=1,pch=16)
  for (tt in allt) {
    llv2 <- llv[llv$tonnage==tt,]
    a2 <- tapply(llv2$hooks,llv2$yrqtr,sum)
    lines(as.numeric(names(a2)),a2,col=nallt[match(tt,allt)])
    }
  if(r==1) legend("topleft",legend=allt,col=nallt,lty=1)
  }
savePlot(filename="Effort by region by yrqtr by tonnage",type="png")

# total effort by region and yearqtr
e <- tapply(dat$hooks,list(dat$yrqtr,dat$regB,dat$tonnage),sum)
write.table(file="effort by region tonnage and yrqtr YFT.csv",e,sep=",")

windows(height=16,width=14); par(mfrow=c(4,3),mar=c(3,4,2,1))
for(dec in seq(1965,2015,5)) {
  a <- dat[dat$op_yr >= dec & dat$op_yr < dec+5,]
  x <- tapply(a$hooks,list(a$lon5,a$lat5),sum)
  image(as.numeric(dimnames(x)[[1]]),as.numeric(dimnames(x)[[2]]),is.na(x),col="lightblue",xlab="Longitude",ylab="Latitude",xlim=c(140,300),main=dec)
  image(as.numeric(dimnames(x)[[1]]),as.numeric(dimnames(x)[[2]]),x,xlab="Longitude",ylab="Latitude",
    xlim=c(140,290),main=dec,add=T,breaks=exp(seq(log(min(x,na.rm=T)),log(max(x*1.01,na.rm=T)),length.out=13)))
  contour(as.numeric(dimnames(x)[[1]]),as.numeric(dimnames(x)[[2]]),x,add=T,
    levels=c(10,10^2,10^3,10^4,10^5,10^6,10^7,10^8,10^9,10^10,10^1,10^12))
  map("world2",add=T, interior=F,fill=T)
  }
savePlot(filename=paste0("Map hooks by decade"),type="png")



a <- tapply(dat$op_yr,dat$op_yr,length)
b <- tapply(datold$op_yr,datold$op_yr,length)
plot(names(a),(b-a)/b,type="l",ylim=c(0,1),ylab="Proportion single species")
savePlot(file="proportion_single_species",type="png")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,2,2,1),oma=c(0,0,1,0))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  llvold <- datold[datold$regB==r,]
  a <- tapply(llv$op_yr,llv$op_yr,length)
  b <- tapply(llvold$op_yr,llvold$op_yr,length)
  plot(names(a),(b-a)/b,type="l",ylim=c(0,1),ylab="Proportion single species",main=paste0("R",r))
  }
savePlot(file="proportion_single_species_region",type="png")


prepdat <- dataprep_TW(dat1)
dat_std <- dataclean_TW_std(prepdat)
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,2,2,1),oma=c(0,0,1,0))
for (r in regBEord) {
  llvstd <- dat_std[dat_std$regB==r,]
  llvall <- prepdat[prepdat$regB==r,]
  a <- tapply(llvstd$op_yr,llvstd$op_yr,length)
  b <- tapply(llvall$op_yr,llvall$op_yr,length)
  plot(names(a),(b-a)/b,type="l",ylim=c(0,1),ylab="Proportion cleaned standard",main=paste0("R",r))
  }
savePlot(file="proportion_total_clean_std_region",type="png")


allz <- rep(0,length(dat$bet))
allz[dat$alb==0 & dat$bet==0 & dat$yft==0] <- 1
b <- tapply(allz,list(dat$op_yr,dat$regB),mean)
regBEord <- c(1,2,3,6,5,4)
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,2,2,1),oma=c(0,0,1,0))
for (r in regBEord) {
  llv <- b[,r+1]
  plot(names(llv),llv,type="l",ylim=c(0,1),xlab="Year",ylab="Prop no main spp",main=paste0("R",r))
  }
savePlot(filename="Proportion no main spp",type="png")

allz <- rep(0,length(dat$prepbet))
totmain <- prepdat$alb + prepdat$bet + prepdat$yft
allz[prepdat$alb==totmain | prepdat$bet==totmain | prepdat$yft==totmain] <- 1
b <- tapply(allz,list(prepdat$op_yr,prepdat$regB),mean)
regBEord <- c(1,2,3,6,5,4)
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,2,2,1),oma=c(0,0,1,0))
for (r in regBEord) {
  llv <- b[,r+1]
  plot(names(llv),llv,type="l",ylim=c(0,1),xlab="Year",ylab="Prop one main spp",main=paste0("R",r))
  }
savePlot(filename="Proportion one main spp",type="png")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,2,2,1),oma=c(0,0,1,0))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  a <- tapply(llv$foc=="F",llv$op_yr,mean)
  plot(names(a),a,type="l",ylim=c(0,1),xlab="Year",ylab="Prop FOC",main=paste0("R",r))
  }
savePlot(filename="Proportion FOC vessels",type="png")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,2,2,1),oma=c(0,0,1,0))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  a <- tapply(llv$oil=="OF",llv$op_yr,mean)
  plot(names(a),a,type="l",ylim=c(0,1),xlab="Year",ylab="Prop FOC",main=paste0("R",r))
  }
savePlot(filename="Proportion oilfish vessels",type="png")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,2,2,1),oma=c(0,0,1,0))
regBEord <- c(1,2,3,6,5,4)
for (r in regBEord) {
  llv <- prepdat[prepdat$regB==r,]
  a <- tapply(llv$hooks < 1000,llv$op_yr,mean)
  plot(names(a),a,type="l",ylim=c(0,1),xlab="Year",ylab="Prop < 1000 hooks",main=paste0("R",r))
  }
savePlot(filename="Proportion less than 1000 hooks",type="png")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,2,2,1),oma=c(0,0,1,0))
outlnom <- unique(dat$rem)[-(1:2)]
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  a <- rep(0,length(llv$alb))
  a[llv$rem%in% outlnom] <- 1
  a <- tapply(a,llv$op_yr,mean)
  plot(names(a),a,type="l",ylim=c(0,1),xlab="Year",ylab="Prop outliers",main=paste0("R",r))
  }
savePlot(filename="Proportion outliers",type="png")
a <- table(dat$rem,dat$op_yr,useNA="always")
save(a,file="outlier types by year.RData")

plot(names(a),(b-a)/b,type="l",ylim=c(0,1),ylab="Proportion single species")
savePlot(file="proportion_single_species",type="png")

windows(width=15,height=9)
hist(prepdat$dmy,breaks="days",freq=T,xlab="Date",main="Sets per day")
savePlot(file="sets_per_day.png",type="png")
hist(prepdat$dmy,breaks="months",freq=T,xlab="Date",main="Sets per month")
savePlot(file="sets_per_month.png",type="png")
table(prepdat$dmy)

a <- aggregate(dat$hooks,list(dat$lat5,dat$lon5),sum,na.rm=T)
windows(width=11,height=9)
symbols(x=a[,2],y=a[,1],circles=.0002*sqrt(a[,3]),inches=F,bg=2,fg=2,xlab="Longitude",ylab="Latitude")
map(add=T,interior=F,fill=T)
savePlot(file="map_hooks.png",type="png")

hist(dat$hooks, nclass=60,xlab="Hooks per set")   # ask if very large # hooks is okay
savePlot("Hook histogram.png",type="png")
prepdat[prepdat$yft==1038,]
prepdat[prepdat$callsign=="61BUP" & prepdat$yrqtr==2005.375 & prepdat$op_mon==4,]
prepdat[prepdat$op_area=="8124" & prepdat$yrqtr==2005.375 & prepdat$op_mon==4,]



windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
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
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  yft <- tapply(llv$yft,llv$yrqtr,sum)
  bet <- tapply(llv$bet,llv$yrqtr,sum)
  alb <- tapply(llv$alb,llv$yrqtr,sum)
#  swo <- tapply(llv$swo,llv$yrqtr,sum)
  maxy <- max(c(yft,bet,alb))
  plot(names(yft),yft,ylim=c(1,maxy),xlab="Year",ylab="Catch",main=paste("Region",r),xlim=range(dat$yrqtr))
  points(names(bet),bet,col=2,pch=2)
  points(names(alb),alb,col=3,pch=3)
#  points(names(swo),swo+1,col=4,pch=4)
  if(r==1) legend("topleft",legend=c("Yellowfin","Bigeye","Albacore"),col=c(1,2,3),pch=c(1,2,3))
  }
savePlot(filename="Catch by region allsp by yrqtr 1",type="png")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  yft <- tapply(llv$yft,llv$yrqtr,sum)
  bet <- tapply(llv$bet,llv$yrqtr,sum)
  alb <- tapply(llv$alb,llv$yrqtr,sum)
#  swo <- tapply(llv$swo,llv$yrqtr,sum)
  maxy <- max(c(yft,bet,alb))
  plot(names(yft),yft+1,ylim=c(1,maxy),xlab="Year",ylab="Catch",main=paste("Region",r),xlim=range(dat$yrqtr),log="y")
  points(names(bet),bet+1,col=2,pch=2)
  points(names(alb),alb+1,col=3,pch=3)
#  points(names(swo),swo+1,col=4,pch=4)
  if(r==1) legend("topleft",legend=c("Yellowfin","Bigeye","Albacore"),col=c(1,2,3),pch=c(1,2,3))
  }
savePlot(filename="Catch by region allsp by yrqtr log1",type="png")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  swo <- tapply(llv$swo,llv$yrqtr,sum)
  mls <- tapply(llv$mls,llv$yrqtr,sum)
  blm <- tapply(llv$blm,llv$yrqtr,sum)
  bum <- tapply(llv$bum,llv$yrqtr,sum)
  otb <- tapply(llv$otb,llv$yrqtr,sum)
  maxy <- max(c(swo,mls,blm,bum,otb))
  if(r==3) maxy=maxy*10
  plot(names(swo),swo+1,ylim=c(1,maxy),xlab="Year",ylab="Catch",main=paste("Region",r),xlim=range(dat$yrqtr),log="y")
  points(names(mls),mls+1,col=2,pch=2)
  points(names(blm),blm+1,col=3,pch=3)
  points(names(bum),bum+1,col=5,pch=5)
  points(names(otb),otb+1,col=5,pch=5)
  if(r==3) legend("topleft",legend=c("Swordfish","Striped marlin","Black marlin","Blue marlin","Other (Sailfish)"),col=1:5,pch=1:5)
  }
savePlot(filename="Catch by region allsp by yrqtr log2",type="png")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  swo <- tapply(llv$swo,llv$yrqtr,sum)
  mls <- tapply(llv$mls,llv$yrqtr,sum)
  blm <- tapply(llv$blm,llv$yrqtr,sum)
  bum <- tapply(llv$bum,llv$yrqtr,sum)
  otb <- tapply(llv$otb,llv$yrqtr,sum)
  maxy <- max(c(swo,mls,blm,bum,otb))
#  if(r==3) maxy=maxy*2
  plot(names(swo),swo,ylim=c(1,maxy),xlab="Year",ylab="Catch",main=paste("Region",r),xlim=range(dat$yrqtr))
  points(names(mls),mls,col=2,pch=2)
  points(names(blm),blm,col=3,pch=3)
  points(names(bum),bum,col=5,pch=5)
  points(names(otb),otb,col=5,pch=5)
  if(r==2) legend("topleft",legend=c("Swordfish","Striped marlin","Black marlin","Blue marlin","Other (Sailfish)"),col=1:5,pch=1:5)
  }
savePlot(filename="Catch by region allsp by yrqtr 2",type="png")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  oth <- tapply(llv$oth,llv$yrqtr,sum)
  sha <- tapply(llv$sha,llv$yrqtr,sum)
  skj <- tapply(llv$skj,llv$yrqtr,sum)
  ott <- tapply(llv$ott,llv$yrqtr,sum)
  maxy <- max(c(oth,sha,skj,ott))
  plot(names(oth),oth+1,ylim=c(1,maxy),xlab="Year",ylab="Catch",main=paste("Region",r),xlim=range(dat$yrqtr),log="y")
  points(names(sha),sha+1,col=2,pch=2)
  points(names(skj),skj+1,col=3,pch=3)
  points(names(ott),ott+1,col=4,pch=4)
  if(r==3) legend("topleft",legend=c("Other (oilfish)","Shark","Skipjack","Other tuna"),col=1:4,pch=1:4)
  }
savePlot(filename="Catch by region allsp by yrqtr log3",type="png")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  oth <- tapply(llv$oth,llv$yrqtr,sum)
  sha <- tapply(llv$sha,llv$yrqtr,sum)
  skj <- tapply(llv$skj,llv$yrqtr,sum)
  ott <- tapply(llv$ott,llv$yrqtr,sum)
  maxy <- max(c(oth,sha,skj,ott))
  plot(names(oth),oth,ylim=c(1,maxy),xlab="Year",ylab="Catch",main=paste("Region",r),xlim=range(dat$yrqtr))
  points(names(sha),sha,col=2,pch=2)
  points(names(skj),skj,col=3,pch=3)
  points(names(ott),ott,col=4,pch=4)
  if(r==3) legend("topleft",legend=c("Other (oilfish)","Shark","Skipjack","Other tuna"),col=1:4,pch=1:4)
  }
savePlot(filename="Catch by region allsp by yrqtr 3",type="png")

### CPUE
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  eff <- tapply(llv$hooks,llv$yrqtr,sum)
  yft <- tapply(llv$yft,llv$yrqtr,sum)
  bet <- tapply(llv$bet,llv$yrqtr,sum)
  alb <- tapply(llv$alb,llv$yrqtr,sum)
  yft <- 100*yft/eff
  bet <- 100*bet/eff
  alb <- 100*alb/eff
  maxy <- max(c(yft,bet,alb))
#  if(r==1) maxy=maxy*100
  plot(names(yft),yft,ylim=c(1e-5,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("Region",r),log="y",xlim=range(dat$yrqtr))
  points(names(bet),bet,col=2,pch=2)
  points(names(alb),alb,col=3,pch=3)
  if(r==1) legend("bottomleft",legend=c("Yellowfin","Bigeye","Albacore"),col=c(1,2,3),pch=c(1,2,3))
  }
savePlot(filename="CPUE by region allsp by yrqtr log1",type="png")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  eff <- tapply(llv$hooks,llv$yrqtr,sum)
  swo <- tapply(llv$swo,llv$yrqtr,sum)
  mls <- tapply(llv$mls,llv$yrqtr,sum)
  blm <- tapply(llv$blm,llv$yrqtr,sum)
  bum <- tapply(llv$bum,llv$yrqtr,sum)
  otb <- tapply(llv$otb,llv$yrqtr,sum)
  swo <- 100*swo/eff
  mls <- 100*mls/eff
  blm <- 100*blm/eff
  bum <- 100*bum/eff
  otb <- 100*otb/eff
  maxy <- max(c(swo,mls,blm,bum,otb))
  plot(names(swo),swo,ylim=c(1e-5,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("Region",r),log="y",xlim=range(dat$yrqtr))
  points(names(mls),mls,col=2,pch=2)
  points(names(blm),blm,col=3,pch=3)
  points(names(bum),bum,col=4,pch=4)
  points(names(otb),otb,col=5,pch=5)
  if(r==1) legend("bottomleft",legend=c("Swordfish","Striped marlin","Black marlin","Blue marlin","Other (sailfish)"),col=1:5,pch=1:5)
  }
savePlot(filename="CPUE by region allsp by yrqtr log2",type="png")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  eff <- tapply(llv$hooks,llv$yrqtr,sum)
  oth <- tapply(llv$oth,llv$yrqtr,sum)
  sha <- tapply(llv$sha,llv$yrqtr,sum)
  skj <- tapply(llv$skj,llv$yrqtr,sum)
  ott <- tapply(llv$ott,llv$yrqtr,sum)
  oth <- 100*oth/eff
  skj <- 100*skj/eff
  sha <- 100*sha/eff
  ott <- 100*ott/eff
  maxy <- max(c(oth,sha,skj,ott))
  plot(names(oth),oth,ylim=c(1e-5,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("Region",r),log="y",xlim=range(dat$yrqtr))
  points(names(sha),sha,col=2,pch=2)
  points(names(skj),skj,col=3,pch=3)
  points(names(ott),ott,col=4,pch=4)
  if(r==3) legend("topleft",legend=c("Other (oilfish)","Shark","Skipjack","Other tunas"),col=1:4,pch=1:4)
  }
savePlot(filename="CPUE by region allsp by yrqtr log3",type="png")

# Nominal CPUE - not logged
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  eff <- tapply(llv$hooks,llv$yrqtr,sum)
  yft <- tapply(llv$yft,llv$yrqtr,sum)
  bet <- tapply(llv$bet,llv$yrqtr,sum)
  alb <- tapply(llv$alb,llv$yrqtr,sum)
  yft <- 100*yft/eff
  bet <- 100*bet/eff
  alb <- 100*alb/eff
  maxy <- max(c(yft,bet,alb))
#  if(r==1) maxy=maxy*100
  plot(names(yft),yft,ylim=c(1e-5,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("Region",r),xlim=range(dat$yrqtr))
  points(names(bet),bet,col=2,pch=2)
  points(names(alb),alb,col=3,pch=3)
  if(r==3) legend("topright",legend=c("Yellowfin","Bigeye","Albacore"),col=c(1,2,3),pch=c(1,2,3))
  }
savePlot(filename="CPUE by region allsp by yrqtr 1",type="png")

# Relative CPUE - not logged
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
pd <- prepdat[prepdat$yrqtr > 1978,]
pd$yq <- as.factor(pd$yrqtr)
ds <- dat_std[dat_std$yrqtr > 1978,]
ds$yq <- as.factor(ds$yrqtr)
for (r in regBEord) {
  aprep <- pd[pd$regB==r,]
  astd <- ds[ds$regB==r,]
  effraw <- tapply(aprep$hooks,aprep$yq,sum,na.rm=T)
  yftraw <- tapply(aprep$yft,aprep$yq,sum,na.rm=T)
  betraw <- tapply(aprep$bet,aprep$yq,sum,na.rm=T)
  effstd <- tapply(astd$hooks,astd$yq,sum,na.rm=T)
  yftstd <- tapply(astd$yft,astd$yq,sum,na.rm=T)
  betstd <- tapply(astd$bet,astd$yq,sum,na.rm=T)
  yftraw <- 100*yftraw/effraw
  betraw <- 100*betraw/effraw
  yftstd <- 100*yftstd/effstd
  betstd <- 100*betstd/effstd
  yftrat <- yftstd/yftraw
  betrat <- betstd/betraw
  yq <- as.numeric(names(yftraw))
  plot(yq,yftrat,ylim=c(0,2),pch=3,xlab="Year",ylab="Relative CPUE",main=paste("Region",r),xlim=range(dat$yrqtr))
  points(yq,betrat,col=2,pch=2)
  if(r==3) legend("topright",legend=c("Yellowfin","Bigeye"),col=c(1,2),pch=c(3,2))
  }
savePlot(filename="CPUE relative cleaned by region allsp by yrqtr 1",type="png")


windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  eff <- tapply(llv$hooks,llv$yrqtr,sum)
  swo <- tapply(llv$swo,llv$yrqtr,sum)
  mls <- tapply(llv$mls,llv$yrqtr,sum)
  blm <- tapply(llv$blm,llv$yrqtr,sum)
  bum <- tapply(llv$bum,llv$yrqtr,sum)
  otb <- tapply(llv$otb,llv$yrqtr,sum)
  swo <- 100*swo/eff
  mls <- 100*mls/eff
  blm <- 100*blm/eff
  bum <- 100*bum/eff
  otb <- 100*otb/eff
  maxy <- max(c(swo,mls,blm,bum,otb))
  plot(names(swo),swo,ylim=c(1e-5,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("Region",r),xlim=range(dat$yrqtr))
  points(names(mls),mls,col=2,pch=2)
  points(names(blm),blm,col=3,pch=3)
  points(names(bum),bum,col=4,pch=4)
  points(names(otb),otb,col=5,pch=5)
  if(r==3) legend("topright",legend=c("Swordfish","Striped marlin","Black marlin","Blue marlin","Other (sailfish)"),col=1:5,pch=1:5)
  }
savePlot(filename="CPUE by region allsp by yrqtr 2",type="png")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  eff <- tapply(llv$hooks,llv$yrqtr,sum)
  oth <- tapply(llv$oth,llv$yrqtr,sum)
  sha <- tapply(llv$sha,llv$yrqtr,sum)
  skj <- tapply(llv$skj,llv$yrqtr,sum)
  ott <- tapply(llv$ott,llv$yrqtr,sum)
  oth <- 100*oth/eff
  skj <- 100*skj/eff
  sha <- 100*sha/eff
  ott <- 100*ott/eff
  maxy <- max(c(oth,sha,skj,ott))
  plot(names(oth),oth,ylim=c(1e-5,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("Region",r),xlim=range(dat$yrqtr))
  points(names(sha),sha,col=2,pch=2)
  points(names(skj),skj,col=3,pch=3)
  points(names(ott),ott,col=4,pch=4)
  if(r==3) legend("topleft",legend=c("Other (oilfish)","Shark","Skipjack","Other tunas"),col=1:4,pch=1:4)
  }
savePlot(filename="CPUE by region allsp by yrqtr 3",type="png")


# CPUE by region by fishingcat
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1),oma=c(0,0,3,0))
for (tt in allt) {
  for (r in regBEord) {
    llv <- dat[dat$regB==r & dat$tonnage==tt,]
    if(dim(llv)[1] > 0) {
    yft <- tapply(llv$yft,llv$yrqtr,sum)
    eff <- tapply(llv$hooks,llv$yrqtr,sum)
    bet <- tapply(llv$bet,llv$yrqtr,sum)
    alb <- tapply(llv$alb,llv$yrqtr,sum)
    swo <- tapply(llv$swo,llv$yrqtr,sum)
    yft <- 100*yft/eff
    bet <- 100*bet/eff
    alb <- 100*alb/eff
    swo <- 100*swo/eff
    maxy <- max(c(yft,bet,alb,swo))
    plot(names(yft),yft,ylim=c(0,maxy),xlab="Year",ylab="Catch per hundred hooks",main=paste("Region",r))
    points(names(bet),bet,col=2,pch=2)
    points(names(alb),alb,col=3,pch=3)
    points(names(swo),swo,col=4,pch=4)
    if(r==1) legend("topleft",legend=c("Yellowfin","Bigeye","Albacore","Swordfish"),col=c(1,2,3,4),pch=c(1,2,3,4))
    } else {
    plot(1:2,1:2,type="n",axes=F,xlab="",ylab="")
    if(r==1) legend("topleft",legend=c("Yellowfin","Bigeye","Albacore","Swordfish"),col=c(1,2,3,4),pch=c(1,2,3,4))
    } }
  title(paste("Tonnage",tt),outer=T)
  ttx <- gsub("<","",tt)
  ttx <- gsub(">=","",ttx)
  savePlot(filename=paste("CPUE nominal allsp by region by yrqtr",ttx),type="png")
  }

# 5 degree squares fished
windows(height=14,width=12)
par (mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB == r,]
  yq <- sort(unique(llv$yrqtr))
  strats <- tapply(paste(llv$lat5,llv$lon5),llv$yrqtr,dimu)
  plot(yq, strats, type="p", xlim=range(dat$yrqtr),pch=1,col=1,ylim=c(0,max(strats)),
           cex=1,ylab="5 x 5 spatial strata with reported effort",main=paste("Region",r))
#  mtext(side=3, paste("Region", r),line=0.5)
  }
savePlot("Number of spatial strata",type="png")

# Proportion sets with zero catches
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  ay <- tapply(llv$yft==0,llv$yrqtr,sum)
  a <- tapply(llv$hooks,llv$yrqtr,length)
  ab <- tapply(llv$bet==0,llv$yrqtr,sum)
  ay <- 1*ay/a
  ab <- 1*ab/a
  maxy <- max(c(ay,ab))
  plot(names(ay),ay,xlim=range(dat$yrqtr),ylim=c(0,1),xlab="Year",ylab="Proportion of zero catches",main=paste("Region",r))
  points(names(ab),ab,col=2,pch=2)
  if(r==6) legend("topright",legend=c("Yellowfin","Bigeye"),col=c(1,2),pch=c(1,2))
  }
savePlot(filename="Proportion zeroes by region by yrqtr",type="png")

# Proportion sets with zero catches allspp
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  ay <- tapply(llv$yft==0,llv$yrqtr,sum)
  a <- tapply(llv$hooks,llv$yrqtr,length)
  ab <- tapply(llv$bet==0,llv$yrqtr,sum)
  aalb <- tapply(llv$alb==0,llv$yrqtr,sum)
  ay <- 1*ay/a
  ab <- 1*ab/a
  aalb <- 1*aalb/a
  maxy <- max(c(ay,ab,aalb))
  plot(names(ay),ay,xlim=range(dat$yrqtr),ylim=c(0,1),xlab="Year",ylab="Proportion of zero catches",main=paste("Region",r))
  points(names(ab),ab,col=2,pch=2)
  points(names(aalb),aalb,col=3,pch=3)
  if(r==6) legend("topright",legend=c("Yellowfin","Bigeye","Albacore"),col=c(1,2,3),pch=c(1,2,3))
    }
savePlot(filename="Proportion zeroes by region by yrqtr allspp",type="png")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  aswo <- tapply(llv$swo==0,llv$yrqtr,sum)
  a <- tapply(llv$hooks,llv$yrqtr,length)
  amls <- tapply(llv$mls==0,llv$yrqtr,sum)
  ablm <- tapply(llv$blm==0,llv$yrqtr,sum)
  abum <- tapply(llv$bum==0,llv$yrqtr,sum)
  aotb <- tapply(llv$otb==0,llv$yrqtr,sum)
  aswo <- 1*aswo/a
  amls <- 1*amls/a
  ablm <- 1*ablm/a
  abum <- 1*abum/a
  aotb <- 1*aotb/a
  maxy <- max(c(aswo,amls,abum,ablm,aotb))
  plot(names(aswo),aswo,xlim=range(dat$yrqtr),ylim=c(0,1),xlab="Year",ylab="Proportion of zero catches",main=paste("Region",r))
  points(names(amls),amls,col=2,pch=2)
  points(names(ablm),ablm,col=3,pch=3)
  points(names(abum),abum,col=4,pch=4)
  points(names(aotb),aotb,col=5,pch=5)
  if(r==4) legend("bottomleft",legend=c("Swordfish","Striped marlin","Blue marlin","Black marlin","Other (Sailfish)"),col=1:5,pch=1:5)
    }
savePlot(filename="Proportion zeroes by region by yrqtr spp2",type="png")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1))
for (r in regBEord) {
  llv <- dat[dat$regB==r,]
  a <- tapply(llv$hooks,llv$yrqtr,length)
  oth <- tapply(llv$oth==0,llv$yrqtr,sum)
  sha <- tapply(llv$sha==0,llv$yrqtr,sum)
  skj <- tapply(llv$skj==0,llv$yrqtr,sum)
  ott <- tapply(llv$ott==0,llv$yrqtr,sum)
  oth <- 1*oth/a
  sha <- 1*sha/a
  skj <- 1*skj/a
  ott <- 1*ott/a
  maxy <- max(c(oth,sha,skj,ott))
  plot(names(oth),oth,xlim=range(dat$yrqtr),ylim=c(0,1),xlab="Year",ylab="Proportion of zero catches",main=paste("Region",r))
  points(names(sha),sha,col=2,pch=2)
  points(names(skj),skj,col=3,pch=3)
  points(names(ott),ott,col=4,pch=4)
  if(r==4) legend("bottomleft",legend=c("Other (oilfish)","Sharks","Skipjack","Other tunas"),col=1:4,pch=1:4)
    }
savePlot(filename="Proportion zeroes by region by yrqtr spp3",type="png")

# Proportion sets with zero catches allspp by fishingcat
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1),oma=c(0,0,3,0))
allt <- sort(unique(dat$tonnage))
for (tt in 4:6) {
  for (r in regBEord) {
    llv <- dat[dat$regB==r & dat$tonnage==allt[tt],]
    ay <- tapply(llv$yft==0,llv$yrqtr,sum)
    a <- tapply(llv$hooks,llv$yrqtr,length)
    ab <- tapply(llv$bet==0,llv$yrqtr,sum)
    aalb <- tapply(llv$alb==0,llv$yrqtr,sum)
    aswo <- tapply(llv$swo==0,llv$yrqtr,sum)
    ay <- 1*ay/a
    ab <- 1*ab/a
    aalb <- 1*aalb/a
    aswo <- 1*aswo/a
    maxy <- max(c(ay,ab,aswo,aalb))
    plot(names(ay),ay,ylim=c(0,1),xlab="Year",ylab="Proportion of zero catches",main=paste("Region",r))
    points(names(ab),ab,col=2,pch=2)
    points(names(aalb),aalb,col=3,pch=3)
    points(names(aswo),aswo,col=4,pch=4)
    }
  legend("bottomleft",legend=c("Yellowfin","Bigeye","Albacore","Swordfish"),col=c(1,2,3,4),pch=c(1,2,3,4))
  title(paste("Tonnage",allt[tt]),outer=T)
  savePlot(filename=paste("Proportion zeroes by region by yrqtr allspp",tt),type="png")
}

# Maps of effort through time
library(maps)
#install.packages("mapproj")
library("mapproj")
library(mapdata)
windows(width=20,height=14); par(mfrow=c(3,3),mar=c(3,4,2,1))
for(yr in seq(1975,2010,by=5)) {
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

allt <- sort(unique(dat$tonnage))
for (tt in 4:6) {
  windows(width=14,height=14); par(mfrow=c(3,2),mar=c(3,4,2,1),oma=c(0,0,3,0))
  a2 <- dat[dat$tonnage==allt[tt],]
  for(yr in seq(1970,2014,by=10)) {
    plot_IO(plot_title=yr,sp="YFT")
    a <- a2[a2$yrqtr > yr & a2$yrqtr < yr+10,]
    if(dim(a)[1] > 0) {
    lats <- sort(unique(a$lat5))
    lons <- sort(unique(a$lon5))
    a <- table(a$lat5,a$lon5)
    for (i in 1:length(lats)) {
      symbols(lons+2.5,rep(lats[i],length(lons))+2.5,circles=sqrt(a[i,])/30,col=1,add = T, inches =F)
      }
    } }
  title(paste("Tonnage",allt[tt]),outer=T)
  savePlot(paste("Plot map sets",tt), type="png")
  }
#    symbols(pp,rep(i,length(pp)),sqrt(pp2)/40, add = T, inches =FALSE)

# Maps of mean HPB through time
windows(width=16,height=14); par(mfrow=c(2,2),mar=c(3,4,2,1))
for(yr in seq(1995,2010,by=5)) {
  plot_IO(plot_title=yr,sp="YFT")
  a <- dat[dat$yrqtr > yr & dat$yrqtr < yr+5,]
  lats <- sort(unique(a$lat5))
  lons <- sort(unique(a$lon5))
  a <- tapply(a$hbf,list(a$lat5,a$lon5),mean,na.rm=T)
  for (i in 1:length(lats)) {
    text(lons+2.5,rep(lats[i],length(lons))+2.5,floor(a[i,]),col=1,add = T)
    }
  }
savePlot("Plot map mean HBF", type="png")

# Maps of mean HPB through time by FC
allt <- sort(unique(dat$tonnage))
for (tt in 4:6) {
  windows(width=14,height=12); par(mfrow=c(2,2),mar=c(3,4,2,1),oma=c(0,0,3,0))
  a2 <- dat[dat$tonnage==allt[tt],]
  for(yr in seq(1995,2010,by=5)) {
    plot_IO(plot_title=yr,sp="YFT")
    a <- a2[a2$yrqtr > yr & a2$yrqtr < yr+10,]
    lats <- sort(unique(a$lat5))
    lons <- sort(unique(a$lon5))
    a <- tapply(a$hbf,list(a$lat5,a$lon5),mean,na.rm=T)
    for (i in 1:length(lats)) {
      text(lons+2.5,rep(lats[i],length(lons))+2.5,floor(a[i,]),col=1,add = T)
      }
    }
  title(paste("Tonnage",allt[tt]),outer=T)
  savePlot(paste("Plot map mean hbf",tt), type="png")
  }

# Maps of median HPB through time
windows(width=20,height=14); par(mfrow=c(3,3),mar=c(3,4,2,1))
for(yr in seq(1975,2010,by=5)) {
  plot_IO(plot_title=yr,sp="YFT")
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
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1),oma=c(1,1,3,1)); r<-3; hh <- 18
  b<- dat
  for (r in regBEord) {
    a <- b[b$regB==r & b$op_yr > 1990,]
    yrs <- sort(unique(a$op_yr))
    a <- table(floor(a$hbf),a$op_yr)
    plot(1,1, type="n", xlab="Year", ylab="HBF", ylim = c(1,25), xlim=c(1994,2014),main=paste("Region",r))
    ilist <- as.numeric(row.names(a))
    for(i in 1:length(ilist)){
      symbols(yrs , rep(ilist[i], length(yrs)), sqrt(a[i,]) / max(sqrt(a)), add = T, inches =FALSE)
      if(trunc(i/5) == i/5){
      lines(c(1990,2014), rep(i,2), lty=3)}
      }
    }
  savePlot(paste("plot hbf by region by yr"),type="png")

# Proportion zero by HBF by region by year
windows(height=14,width=12); par(mfrow=c(2,2),mar=c(4,4,2,2))
for (r in regBEord) {
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

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(4,4,2,2))
for (r in regBEord) {
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

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(4,4,2,2))
for (r in regBEord) {
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

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(4,4,2,2))
for (r in regBEord) {
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
plot_pzero_ll <- function(a,sp,la,lo,yr1=1990,yrnd=2014) {
 # if(la == -30) browser()
  yrs <- sort(unique(a$op_yr))
  a <- tapply(a[,sp],list(a$hbf,a$op_yr),countzero)
  plot(1,1, type="n", xlab="Year", ylab="HBF", ylim = c(1,24), xlim=c(yr1,yrnd),main=paste(la,lo,sep=", "))
  ilist <- as.numeric(row.names(a))
  if(length(ilist) > 0) {
    for(i in 1:length(ilist)){
      symbols(yrs , rep(ilist[i], length(yrs)), sqrt(a[i,]), add = T, inches =FALSE)
      }
    }
  }

#for(sp in c("alb","bet","blm","bum","mls","oth","otb","sha","skj","swo","yft","ott"))
d2 <- dat[dat$lon5 < 75 & dat$lat5 > -20 & dat$lat5 < 5,]
for(sp in c("otb","sha","skj","swo","yft","ott"))
  {
  windows(height=14,width=24); par(mfrow=c(5,7),mar=c(2,2,1,0),oma=c(0,0,3,0))
  for (la in seq(2.5,-17.5,by=-5)) {
    for (lo in seq(42.5,72.5,by=5)) {
      a <- d2[d2$lat5==la & d2$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_pzero_ll(a,sp,la,lo) else plot(1,1,type="n")
      }
    }
  title(sp,outer=T,line=1)
  savePlot(paste("plot pzero",sp,"by hbf by R2 latlong2"),type="png")
}

d2 <- dat[dat$lon5 > 75 & dat$lat5 > -20 & dat$lat5 < 5,]
for(sp in c("alb","bet","blm","bum","mls","oth","otb","sha","skj","swo","yft","ott"))
  {
  windows(height=14,width=24); par(mfrow=c(5,8),mar=c(2,2,1,0),oma=c(0,0,3,0))
  for (la in seq(2.5,-17.5,by=-5)) {
    for (lo in seq(77.5,112.5,by=5)) {
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
  b <- dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,]
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==(la) & b$lon5 == lo,]
      if(dim(a)[[1]] >0)  plot_pzero_ll(a,sp,la,lo) else plot(1,1,type="n")
      }
    }
  title(ti,outer=T)
  savePlot(fname,type="png")
  }
pzero_hbf_sp_yq("bet",seq(2.5,-17.5,by=-5),loseq=seq(77.5,112.5,by=5),fname="plot pzero bet by hbf by R2 latlong",ti="Bigeye Region 1")
pzero_hbf_sp_yq("yft",laseq=seq(2.5,-17.5,by=-5),loseq=seq(77.5,112.5,by=5),fname="plot pzero yft by hbf by R2 latlong",ti="Yellowfin Region 1")
pzero_hbf_sp_yq("alb",laseq=seq(2.5,-17.5,by=-5),loseq=seq(77.5,112.5,by=5),fname="plot pzero alb by hbf by R2 latlong",ti="Albacore Region 1")


# Proportion of zero catches
plot_pzero_both <- function(a,la,lo) {
  cx <- 0.8
  yrs <- sort(unique(a$yrqtr))
  alb <- tapply(a$alb,list(a$yrqtr),countzero)
  yft <- tapply(a$yft,list(a$yrqtr),countzero)
  bet <- tapply(a$bet,list(a$yrqtr),countzero)
  plot(1,1, type="n", xlab="yr", ylab="p(zero catch)", ylim = c(0,1), xlim=range(dat$yrqtr),main=paste(la,", ",lo,sep=""))
  points(yrs, yft,cex=cx)
  points(yrs,bet,col=2,pch=2,cex=cx)
  points(yrs,alb,col=3,pch=3,cex=cx)
  }
pzero_all_sp_yq <- function(laseq,loseq,fname,ti="") {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,]
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_pzero_both(a,la,lo) else plot(1,1,type="n",ylim=c(0,1))
      }
    }
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

pzero_all_sp_yq(laseq=seq(22.5,12.5,-5),loseq=seq(47.5,72.5,5),fname="plot pzero YBA by R1 latlong",ti="Probability of zero catch Region 1")
pzero_all_sp_yq(laseq=seq(7.5,-12.5,-5),loseq=seq(42.5,72.5,5),fname="plot pzero YBA by R2 latlong",ti="Probability of zero catch Region 2")
pzero_all_sp_yq(laseq=seq(7.5,-12.5,-5),loseq=seq(77.5,112.5,5),fname="plot pzero YBA by R5 latlong",ti="Probability of zero catch Region 5")
pzero_all_sp_yq(laseq=seq(-17.5,-37.5,-5),loseq=seq(22.5,62.5,5),fname="plot pzero YBA by R3 latlong",ti="Probability of zero catch Region 3")
pzero_all_sp_yq(laseq=seq(-17.5,-37.5,-5),loseq=seq(67.5,122.5,5),fname="plot pzero YBA by R4 latlong",ti="Probability of zero catch Region 4")
pzero_all_sp_yq(laseq=seq(22.5,12.5,-5),loseq=seq(77.5,102.5,5),fname="plot pzero YBA by R6 latlong",ti="Probability of zero catch Region 6")

plot_pzero_billf <- function(a,la,lo) {
  cx <- 0.8
  yrs <- sort(unique(a$yrqtr))
  swo <- tapply(a$swo,list(a$yrqtr),countzero)
  mls <- tapply(a$mls,list(a$yrqtr),countzero)
  blm <- tapply(a$blm,list(a$yrqtr),countzero)
  bum <- tapply(a$bum,list(a$yrqtr),countzero)
  otb <- tapply(a$otb,list(a$yrqtr),countzero)
#  swo <- tapply(a$swo,list(a$yrqtr),countzero)
  plot(1,1, type="n", xlab="yr", ylab="p(zero catch)", ylim = c(0,1), xlim=range(dat$yrqtr),main=paste(la,", ",lo,sep=""))
  points(yrs, swo,cex=cx)
  points(yrs,mls,col=2,pch=2,cex=cx)
  points(yrs,blm,col=3,pch=3,cex=cx)
  points(yrs,bum,col=4,pch=4,cex=cx)
  points(yrs,otb,col=5,pch=5,cex=cx)
#  points(yrs,swo,col=4,pch=4,cex=cx)
  }
pzero_billf_yq <- function(laseq,loseq,fname,ti="") {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,]
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_pzero_billf(a,la,lo) else plot(1,1,type="n",ylim=c(0,1))
      if(la==laseq[1] & lo==loseq[1]) legend("topright",legend=c("SWO","MLS","BLM","BUM","OTB"),col=1:5,pch=1:5)
      }
    }
  title(ti,outer=T)
  savePlot(fname,type="png")
  }
pzero_billf_yq(laseq=seq(22.5,12.5,-5),loseq=seq(47.5,72.5,5),fname="plot pzero billf by R1 latlong",ti="Probability of zero catch Region 1")
pzero_billf_yq(laseq=seq(7.5,-12.5,-5),loseq=seq(42.5,72.5,5),fname="plot pzero billf by R2 latlong",ti="Probability of zero catch Region 2")
pzero_billf_yq(laseq=seq(7.5,-12.5,-5),loseq=seq(77.5,112.5,5),fname="plot pzero billf by R5 latlong",ti="Probability of zero catch Region 5")
pzero_billf_yq(laseq=seq(-17.5,-37.5,-5),loseq=seq(22.5,62.5,5),fname="plot pzero billf by R3 latlong",ti="Probability of zero catch Region 3")
pzero_billf_yq(laseq=seq(-17.5,-37.5,-5),loseq=seq(67.5,122.5,5),fname="plot pzero billf by R4 latlong",ti="Probability of zero catch Region 4")
pzero_billf_yq(laseq=seq(22.5,12.5,-5),loseq=seq(77.5,102.5,5),fname="plot pzero billf by R6 latlong",ti="Probability of zero catch Region 6")

plot_pzero_sha <- function(a,la,lo) {
  cx <- 0.8
  yrs <- sort(unique(a$yrqtr))
  oth <- tapply(a$oth,list(a$yrqtr),countzero)
  sha <- tapply(a$sha,list(a$yrqtr),countzero)
  skj <- tapply(a$skj,list(a$yrqtr),countzero)
  ott <- tapply(a$ott,list(a$yrqtr),countzero)
  plot(1,1, type="n", xlab="yr", ylab="p(zero catch)", ylim = c(0,1), xlim=range(dat$yrqtr),main=paste(la,", ",lo,sep=""))
  points(yrs, oth,cex=cx)
  points(yrs,sha,col=2,pch=2,cex=cx)
  points(yrs,skj,col=3,pch=3,cex=cx)
  points(yrs,ott,col=4,pch=4,cex=cx)
  }
pzero_all_sha <- function(laseq,loseq,fname,ti="") {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,]
  for (la in laseq) {
    for (lo in loseq) {
      a <- b[b$lat5==la & b$lon5 %in% c(lo),]
      if(dim(a)[[1]] >0)  plot_pzero_sha(a,la,lo) else plot(1,1,type="n",ylim=c(0,1))
      if(la==laseq[1] & lo==loseq[1]) legend("topright",legend=c("OTH","SHA","SKJ","OTT"),col=1:3,pch=1:3)
      }
    }
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

pzero_all_sha(laseq=seq(22.5,12.5,-5),loseq=seq(47.5,72.5,5),fname="plot pzero SHA by R1 latlong",ti="Probability of zero catch Region 1")
pzero_all_sha(laseq=seq(7.5,-12.5,-5),loseq=seq(42.5,72.5,5),fname="plot pzero SHA by R2 latlong",ti="Probability of zero catch Region 2")
pzero_all_sha(laseq=seq(7.5,-12.5,-5),loseq=seq(77.5,112.5,5),fname="plot pzero SHA by R5 latlong",ti="Probability of zero catch Region 5")
pzero_all_sha(laseq=seq(-17.5,-37.5,-5),loseq=seq(22.5,62.5,5),fname="plot pzero SHA by R3 latlong",ti="Probability of zero catch Region 3")
pzero_all_sha(laseq=seq(-17.5,-37.5,-5),loseq=seq(67.5,122.5,5),fname="plot pzero SHA by R4 latlong",ti="Probability of zero catch Region 4")
pzero_all_sha(laseq=seq(22.5,12.5,-5),loseq=seq(77.5,102.5,5),fname="plot pzero SHA by R6 latlong",ti="Probability of zero catch Region 6")


# Albacore catch distribution histograms
write.csv(table(dat$hbf,floor(dat$yrqtr/5)*5,dat$regA2),file="hbf by region by 5 years.csv")
windows();par(mfcol=c(2,2))
for(i in regBEord) {
  a <- dat[dat$regA2==i,]
  hist(a$alb,nclass=50,xlab="Albacore catch",xlim=c(0,250),main=paste("ALB2 Region",i))
}
savePlot("hist alb by regA2",type="png")


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
  plot(1,1, type="n", xlab="Year", ylab="p(CPUE > .02)", ylim = c(0,1), xlim=range(dat$yrqtr),main=paste(la,lo,sep=", "))
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

pcpue_all_sp_yq(cpue=10,laseq=seq(22.5,12.5,-5),loseq=seq(47.5,72.5,5),fname="plot pcpue allsp by R1 latlong",ti="Probability of cpue > 1/100 hooks Region 1")
pcpue_all_sp_yq(cpue=10,laseq=seq(7.5,-12.5,-5),loseq=seq(42.5,72.5,5),fname="plot pcpue allsp by R2 latlong",ti="Probability of cpue > 1/100 hooks Region 2")
pcpue_all_sp_yq(cpue=10,laseq=seq(7.5,-12.5,-5),loseq=seq(77.5,112.5,5),fname="plot pcpue allsp by R5 latlong",ti="Probability of cpue > 1/100 hooks Region 5")
pcpue_all_sp_yq(cpue=10,laseq=seq(-17.5,-37.5,-5),loseq=seq(22.5,62.5,5),fname="plot pcpue allsp by R3 latlong",ti="Probability of cpue > 1/100 hooks Region 3")
pcpue_all_sp_yq(cpue=10,laseq=seq(-17.5,-37.5,-5),loseq=seq(67.5,122.5,5),fname="plot pcpue allsp by R4 latlong",ti="Probability of cpue > 1/100 hooks Region 4")
pcpue_all_sp_yq(cpue=10,laseq=seq(22.5,12.5,-5),loseq=seq(77.5,102.5,5),fname="plot pcpue allsp by R6 latlong",ti="Probability of cpue > 1/100 hooks Region 6")

# Catch
plot_catch <- function(a,la,lo,ymax) {
  cx <- 0.9
  yrs <- sort(unique(a$op_yr))
  yft <- tapply(a$yft,list(a$op_yr),sum)
  bet <- tapply(a$bet,list(a$op_yr),sum)
  alb <- tapply(a$alb,list(a$op_yr),sum)
  plot(1,1, type="n", xlab="Year", ylab="Catch (numbers)", ylim = c(0,ymax), xlim=range(dat$yrqtr),main=paste(la,lo,sep=", "))
  lines(yrs,yft,col=1,cex=cx)
  lines(yrs,bet,col=2,lty=2,cex=cx)
  lines(yrs,alb,col=3,lty=3,cex=cx)
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
  legend("topright",legend=c("Yellowfin","Bigeye","Albacore"),lty=c(1:3),col=c(1:3))
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

catch_all_sp_yq(laseq=seq(22.5,12.5,-5),loseq=seq(47.5,72.5,5),ymax=200000,fname="plot catch YBA by R1 latlong",ti="CatchRegion 1")
catch_all_sp_yq(laseq=seq(7.5,-12.5,-5),loseq=seq(42.5,72.5,5),ymax=100000,fname="plot catch YBA by R2 latlong",ti="Catch Region 2")
catch_all_sp_yq(laseq=seq(7.5,-12.5,-5),loseq=seq(77.5,112.5,5),ymax=60000,fname="plot catch YBA by R5 latlong",ti="Catch Region 5")
catch_all_sp_yq(laseq=seq(-17.5,-37.5,-5),loseq=seq(22.5,62.5,5),ymax=60000,fname="plot catch YBA by R3 latlong",ti="Catch Region 3")
catch_all_sp_yq(laseq=seq(-17.5,-37.5,-5),loseq=seq(67.5,122.5,5),ymax=60000,fname="plot catch YBA by R4 latlong",ti="Catch Region 4")
catch_all_sp_yq(laseq=seq(22.5,12.5,-5),loseq=seq(77.5,102.5,5),ymax=60000,fname="plot catch YBA by R6 latlong",ti="Catch Region 6")

plot_catch <- function(a,la,lo,ymax) {
  cx <- 0.9
  yrs <- sort(unique(a$op_yr))
  swo <- tapply(a$swo,list(a$op_yr),sum)
  mls <- tapply(a$mls,list(a$op_yr),sum)
  blm <- tapply(a$blm,list(a$op_yr),sum)
  bum <- tapply(a$bum,list(a$op_yr),sum)
  otb <- tapply(a$otb,list(a$op_yr),sum)
  plot(1,1, type="n", xlab="Year", ylab="Catch (numbers)", ylim = c(0,ymax), xlim=range(dat$yrqtr),main=paste(la,lo,sep=", "))
  lines(yrs,swo,col=1,cex=cx)
  lines(yrs,mls,col=2,lty=2,cex=cx)
  lines(yrs,blm,col=3,lty=3,cex=cx)
  lines(yrs,bum,col=4,lty=4,cex=cx)
  lines(yrs,otb,col=5,lty=5,cex=cx)
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
  legend("topright",legend=c("SWO","MLS","BLM","BUM","OTB"),lty=c(1:5),col=c(1:5))
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

catch_all_sp_yq(laseq=seq(22.5,12.5,-5),loseq=seq(47.5,72.5,5),ymax=5000,fname="plot catch billf by R1 latlong",ti="CatchRegion 1")
catch_all_sp_yq(laseq=seq(7.5,-12.5,-5),loseq=seq(42.5,72.5,5),ymax=20000,fname="plot catch billf by R2 latlong",ti="Catch Region 2")
catch_all_sp_yq(laseq=seq(7.5,-12.5,-5),loseq=seq(77.5,112.5,5),ymax=10000,fname="plot catch billf by R5 latlong",ti="Catch Region 5")
catch_all_sp_yq(laseq=seq(-17.5,-37.5,-5),loseq=seq(22.5,62.5,5),ymax=10000,fname="plot catch billf by R3 latlong",ti="Catch Region 3")
catch_all_sp_yq(laseq=seq(-17.5,-37.5,-5),loseq=seq(67.5,122.5,5),ymax=10000,fname="plot catch billf by R4 latlong",ti="Catch Region 4")
catch_all_sp_yq(laseq=seq(22.5,12.5,-5),loseq=seq(77.5,102.5,5),ymax=20000,fname="plot catch billf by R6 latlong",ti="Catch Region 6")

plot_catch <- function(a,la,lo,ymax) {
  cx <- 0.9
  yrs <- sort(unique(a$op_yr))
  oth <- tapply(a$oth,list(a$op_yr),sum)
  skj <- tapply(a$skj,list(a$op_yr),sum)
  sha <- tapply(a$skj,list(a$op_yr),sum)
  ott <- tapply(a$ott,list(a$op_yr),sum)
  plot(1,1, type="n", xlab="Year", ylab="Catch (numbers)", ylim = c(0,ymax), xlim=range(dat$yrqtr),main=paste(la,lo,sep=", "))
  lines(yrs,oth,col=1,cex=cx)
  lines(yrs,sha,col=2,lty=2,cex=cx)
  lines(yrs,skj,col=3,lty=3,cex=cx)
  lines(yrs,ott,col=4,lty=4,cex=cx)
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
  legend("topright",legend=c("OTH","SHA","SKJ","OTT"),lty=c(1:4),col=c(1:4))
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

catch_all_sp_yq(laseq=seq(22.5,12.5,-5),loseq=seq(47.5,72.5,5),ymax=10000,fname="plot catch sha by R1 latlong",ti="CatchRegion 1")
catch_all_sp_yq(laseq=seq(7.5,-12.5,-5),loseq=seq(42.5,72.5,5),ymax=15000,fname="plot catch sha by R2 latlong",ti="Catch Region 2")
catch_all_sp_yq(laseq=seq(7.5,-12.5,-5),loseq=seq(77.5,112.5,5),ymax=10000,fname="plot catch sha by R5 latlong",ti="Catch Region 5")
catch_all_sp_yq(laseq=seq(-17.5,-37.5,-5),loseq=seq(22.5,62.5,5),ymax=500000,fname="plot catch sha by R3 latlong",ti="Catch Region 3")
catch_all_sp_yq(laseq=seq(-17.5,-37.5,-5),loseq=seq(67.5,122.5,5),ymax=50000,fname="plot catch sha by R4 latlong",ti="Catch Region 4")
catch_all_sp_yq(laseq=seq(22.5,12.5,-5),loseq=seq(77.5,102.5,5),ymax=5000,fname="plot catch sha by R6 latlong",ti="Catch Region 6")


plot_effort <- function(a,la,lo,ymax) {
  cx <- 0.9
  yrs <- sort(unique(a$op_yr))
  effort <- tapply(a$hooks,list(a$op_yr),sum)
  plot(1,1, type="n", xlab="Year", ylab="Catch (numbers)", ylim = c(0,ymax), xlim=range(dat$yrqtr),main=paste(la,lo,sep=", "))
  lines(yrs,effort,col=1,cex=cx)
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
#  legend("topright",legend=c("Offshore","Distant water"),lty=c(1,2),col=c(1,2))
  title(ti,outer=T)
  savePlot(fname,type="png")
  }

effort_all_fc_yq(laseq=seq(22.5,12.5,-5),loseq=seq(47.5,72.5,5),ymax=10000000,fname="plot effort by R1 latlong",ti="Effort Region 1")
effort_all_fc_yq(laseq=seq(7.5,-12.5,-5),loseq=seq(42.5,72.5,5),ymax=20000000,fname="plot effort by R2 latlong",ti="Effort Region 2")
effort_all_fc_yq(laseq=seq(7.5,-12.5,-5),loseq=seq(77.5,112.5,5),ymax=10000000,fname="plot effort by R5 latlong",ti="Effort Region 5")
effort_all_fc_yq(laseq=seq(-17.5,-37.5,-5),loseq=seq(22.5,62.5,5),ymax=10000000,fname="plot effort by R3 latlong",ti="Effort Region 3")
effort_all_fc_yq(laseq=seq(-17.5,-37.5,-5),loseq=seq(67.5,122.5,5),ymax=10000000,fname="plot effort by R4 latlong",ti="Effort Region 4")
effort_all_fc_yq(laseq=seq(22.5,12.5,-5),loseq=seq(77.5,102.5,5),ymax=10000000,fname="plot effort by R6 latlong",ti="Effort Region 6")


# Median CPUE
minmaxy <- function(a,maxy) min(a,maxy,na.rm=T)
plot_median_cpue <- function(a,la,lo) {
  cx=0.9
  yrs <- sort(unique(a$yrqtr))
  alb <- tapply(a$alb/a$hooks,list(a$yrqtr),median)
  yft <- tapply(a$yft/a$hooks,list(a$yrqtr),median)
  bet <- tapply(a$bet/a$hooks,list(a$yrqtr),median)
  maxy=0.03
  plot(1,1, type="n", xlab="Year", ylab="Median CPUE", ylim = c(0,maxy), xlim=range(dat$yrqtr),main=paste(la,lo,sep=", "))
  points(yrs,sapply(yft,minmaxy,maxy),col=1,pch=1,cex=cx)
  points(yrs,sapply(bet,minmaxy,maxy),col=2,pch=2,cex=cx)
  points(yrs,sapply(alb,minmaxy,maxy),col=3,pch=3,cex=cx)
  }
median_all_sp_yq_fc <- function(cpue,laseq,loseq,fname,ti="",fc="both") {
  windows(height=14,width=24); par(mfrow=c(length(laseq),length(loseq)),mar=c(2,2,1,0),oma=c(1,1,4,1))
  b <- dat[dat$lat5 %in% laseq & dat$lon5 %in% loseq,]
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

median_all_sp_yq_fc(laseq=seq(22.5,12.5,-5),loseq=seq(47.5,72.5,5),fname="plot median YBA by R1 latlong",ti="Effort Region 1")
median_all_sp_yq_fc(laseq=seq(7.5,-12.5,-5),loseq=seq(42.5,72.5,5),fname="plot median YBA by R2 latlong",ti="Effort Region 2")
median_all_sp_yq_fc(laseq=seq(7.5,-12.5,-5),loseq=seq(77.5,112.5,5),fname="plot median YBA by R5 latlong",ti="Effort Region 5")
median_all_sp_yq_fc(laseq=seq(-17.5,-37.5,-5),loseq=seq(22.5,62.5,5),fname="plot median YBA by R3 latlong",ti="Effort Region 3")
median_all_sp_yq_fc(laseq=seq(-17.5,-37.5,-5),loseq=seq(67.5,122.5,5),fname="plot median YBA by R4 latlong",ti="Effort Region 4")
median_all_sp_yq_fc(laseq=seq(22.5,12.5,-5),loseq=seq(77.5,102.5,5),fname="plot median YBA by R6 latlong",ti="Effort Region 6")


# Mean CPUE
minmaxy <- function(a,maxy) min(a,maxy,na.rm=T)
plot_mean_cpue <- function(a,la,lo) {
  cx=0.9
  yrs <- sort(unique(a$yrqtr))
  alb <- tapply(a$alb/a$hooks,list(a$yrqtr),mean)
  yft <- tapply(a$yft/a$hooks,list(a$yrqtr),mean)
  bet <- tapply(a$bet/a$hooks,list(a$yrqtr),mean)
  maxy=0.03
  plot(1,1, type="n", xlab="Year", ylab="Mean CPUE", ylim = c(0,maxy), xlim=range(dat$yrqtr),main=paste(la,lo,sep=", "))
  points(yrs,sapply(yft,minmaxy,maxy),col=1,pch=1,cex=cx)
  points(yrs,sapply(bet,minmaxy,maxy),col=2,pch=2,cex=cx)
  points(yrs,sapply(alb,minmaxy,maxy),col=3,pch=3,cex=cx)
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


mean_all_sp_yq_fc(laseq=seq(22.5,12.5,-5),loseq=seq(47.5,72.5,5),fname="plot mean YBA by R1 latlong",ti="Effort Region 1")
mean_all_sp_yq_fc(laseq=seq(7.5,-12.5,-5),loseq=seq(42.5,72.5,5),fname="plot mean YBA by R2 latlong",ti="Effort Region 2")
mean_all_sp_yq_fc(laseq=seq(7.5,-12.5,-5),loseq=seq(77.5,112.5,5),fname="plot mean YBA by R5 latlong",ti="Effort Region 5")
mean_all_sp_yq_fc(laseq=seq(-17.5,-37.5,-5),loseq=seq(22.5,62.5,5),fname="plot mean YBA by R3 latlong",ti="Effort Region 3")
mean_all_sp_yq_fc(laseq=seq(-17.5,-37.5,-5),loseq=seq(67.5,122.5,5),fname="plot mean YBA by R4 latlong",ti="Effort Region 4")
mean_all_sp_yq_fc(laseq=seq(22.5,12.5,-5),loseq=seq(77.5,102.5,5),fname="plot mean YBA by R6 latlong",ti="Effort Region 6")

windows(height=14,width=12); par(mfrow=c(2,2),mar=c(3,4,2,1),oma=c(0,0,3,0))
allt <- sort(unique(dat$tonnage))
for (tt in 4:6) {
  for (r in regBEord) {
    llv <- dat[dat$regB==r & dat$tonnage==allt[tt],]
    ay <- tapply(llv$yft==0,llv$yrqtr,sum)
    a <- tapply(llv$hooks,llv$yrqtr,length)
    ab <- tapply(llv$bet==0,llv$yrqtr,sum)
    aalb <- tapply(llv$alb==0,llv$yrqtr,sum)
    aswo <- tapply(llv$swo==0,llv$yrqtr,sum)
    ay <- 100*ay/a
    ab <- 100*ab/a
    aalb <- 100*aalb/a
    aswo <- 100*aswo/a
    maxy <- max(c(ay,ab,aswo,aalb))
    plot(names(ay),ay,ylim=c(0,maxy),xlab="Year",ylab="Proportion of zero catches",main=paste("Region",r))
    points(names(ab),ab,col=2,pch=2)
    points(names(aalb),aalb,col=3,pch=3)
    points(names(aswo),aswo,col=4,pch=4)
    }
  legend("bottomleft",legend=c("Yellowfin","Bigeye","Albacore","Swordfish"),col=c(1,2,3,4),pch=c(1,2,3,4))
  title(paste("Tonnage",allt[tt]),outer=T)
  savePlot(filename=paste("Proportion zeroes by region by yrqtr allspp",tt),type="png")
}

table(dat$bait1,dat$op_yr,useNA="always")
table(dat$bait2,dat$op_yr,useNA="always")
table(dat$bait3,dat$op_yr,useNA="always")
table(dat$bait4,dat$op_yr,useNA="always")
table(dat$bait5,dat$op_yr,useNA="always")
table(dat$hookdp,dat$op_yr,useNA="always")

table(paste(dat$tonnage,dat$bait1),dat$op_yr,useNA="always")
table(paste(dat$tonnage,dat$bait2),dat$op_yr,useNA="always")
table(paste(dat$tonnage,dat$bait3),dat$op_yr,useNA="always")
table(paste(dat$tonnage,dat$bait4),dat$op_yr,useNA="always")
table(paste(dat$tonnage,dat$bait5),dat$op_yr,useNA="always")



table(dat$bt1,dat$op_yr,useNA="always")
table(dat$bt2,dat$op_yr,useNA="always")
table(dat$bt3,dat$op_yr,useNA="always")
table(dat$bt4,dat$op_yr,useNA="always")
table(dat$bt5,dat$op_yr,useNA="always")

allt <- sort(unique(dat$tonnage))
for (tt in 4:6) {
  windows(height=14,width=18); par(mfrow=c(2,2),mar=c(3,4,2,1),oma=c(0,0,3,0))
  baitn=paste0("bt",1:5)
  llx <- dat[dat$tonnage==allt[tt],]
  for (r in regBEord) {
    plot(1:2,1:2,type="n",col=1,xlim=c(1979,2014),xlab="Year",ylab="Proportion of effort",main=paste("Region",r),ylim=c(0,1))
    llv <- llx[llx$regB==r,]
    for (i in 1:5) {
      llv$btt <- with(llv,get(baitn[i]))
      a <- tapply(llv$hooks,list(llv$yrqtr,llv$btt),sum)
      tota <- apply(a,1,sum,na.rm=T)
      tota[tota==0]<-1
      a[is.na(a)==T]<-0
      points(as.numeric(names(tota)),(1-a[,1]/tota),col=i,pch=i,lty=1)
      }
    if(r==1) legend("bottomleft",legend=c("Pacific saury","Mackerel", "Squid", "Milkfish", "Other"),col=1:5,pch=1:5)
    }
  title(paste("Tonnage",allt[tt]),outer=T)
  savePlot(filename=paste("Bait type by region by yrqtr",tt),type="png")
}

table(dat$target,dat$op_yr,useNA="always")
# Target by region through time
allt <- sort(unique(dat$tonnage))
for (tt in 4:6) {
  windows(height=14,width=18); par(mfrow=c(2,2),mar=c(3,4,2,1),oma=c(0,0,3,0))
  llx <- dat[dat$tonnage==allt[tt] & dat$op_yr > 2005,]
  for (r in regBEord) {
    llv <- llx[llx$regB==r,]
    a <- tapply(llv$hooks,list(llv$yrqtr,factor(llv$target,levels=1:3)),sum)
    tota <- apply(a,1,sum,na.rm=T)
    tota[tota==0]<-1
    a[is.na(a)==T]<-0
    plot(names(tota),a[,1]/tota,type="b",col=1,xlim=c(2006,2014),xlab="Year",ylab="Proportion of effort",main=paste("Region",r),ylim=c(0,1))
    lines(names(tota),a[,2]/tota,type="b",col=2,pch=2,lty=1)
    lines(names(tota),a[,3]/tota,type="b",col=3,pch=3,lty=1)
    }
  legend("bottomright",legend=c("Bigeye","Albacore","Both"),col=c(1,2,3),pch=c(1,2,3),lty=c(1,1,1))
  title(paste("Tonnage",allt[tt]),outer=T)
  savePlot(filename=paste("Target by region by yrqtr",tt),type="png")
}

###################################################################################### stop here
###################################################################################### stop here
###################################################################################### stop here
###################################################################################### stop here


