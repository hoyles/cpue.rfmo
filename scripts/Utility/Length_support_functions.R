prep_len_data <- function(sz) {
  sz <- sz[!is.na(sz$latitude),]


  #  rng <- data.frame(st = c(1950, 1970, 1986), nd = c(1969, 1985, 2050), mlt = c(-1, 1, -1))
  rng <- data.frame(st = c(1950, 1970, 1988), nd = c(1969, 1989, 2050), mlt = c(-1, 1, -1)) # changed 28 August, not run yet
  sz$cls2 <- NA
  for(i in 1:length(rng$mlt)) {
    a <- sz[sz$year %in% rng[i,"st"]:rng[i,"nd"],]

    loc <- a$unit==7
    a$cls2[loc] <- a$size_class[loc] + rng[i,"mlt"] * 1

    loc <- a$unit==6  # 1cm bins
    a$cls2[loc] <- a$size_class[loc] + rng[i,"mlt"] * .5 # Bin indicates the middle
    loc <- a$unit==8  # 5cm bins
    a$cls2[loc] <- a$size_class[loc] + rng[i,"mlt"] * 2.5 # Bin indicates the middle
    loc <- a$unit==3  # 1kg bins
    a$cls2[loc] <- a$size_class[loc] + rng[i,"mlt"] * .5 # Bin indicates the middle

    sz[sz$year %in% rng[i,"st"]:rng[i,"nd"],] <- a
  }

  sz$lat <- as.numeric(as.character(sz$latitude))
  sz$lon <- as.numeric(as.character(sz$longitude))
  sz$latitude_code<-as.numeric(as.character(sz$latitude_code))
  sz$longitude_code<-as.numeric(as.character(sz$longitude_code))

  sz<-sz[!is.na(sz$lat) & !is.na(sz$lon),]
  sz<-sz[!is.na(sz$latitude_code) & !is.na(sz$longitude_code),]

  sz$lat[sz$latitude_code==2] <- sz$lat[sz$latitude_code==2] * -1
  sz$lon[sz$longitude_code==2] <- 360 - sz$lon[sz$longitude_code==2]
  #  sz <- sz[sz$longitude_code==1,]
  sz$lat[sz$resolution==1 & sz$latitude_code==1] <- sz$lat[sz$resolution==1 & sz$latitude_code==1] + 5
  sz$lon[sz$resolution==1 & sz$latitude_code==1] <- sz$lon[sz$resolution==1 & sz$latitude_code==1] + 10
  sz$lat[sz$resolution==2 & sz$latitude_code==1] <- sz$lat[sz$resolution==2 & sz$latitude_code==1] + 2.5
  sz$lon[sz$resolution==2 & sz$latitude_code==1] <- sz$lon[sz$resolution==2 & sz$latitude_code==1] + 5
  sz$lat[sz$resolution==3 & sz$latitude_code==1] <- sz$lat[sz$resolution==3 & sz$latitude_code==1] + 2.5
  sz$lon[sz$resolution==3 & sz$latitude_code==1] <- sz$lon[sz$resolution==3 & sz$latitude_code==1] + 2.5
  sz$lat[sz$resolution==4 & sz$latitude_code==1] <- sz$lat[sz$resolution==4 & sz$latitude_code==1] + .5
  sz$lon[sz$resolution==4 & sz$latitude_code==1] <- sz$lon[sz$resolution==4 & sz$latitude_code==1] + .5
  sz$lat[sz$resolution==1 & sz$latitude_code==2] <- sz$lat[sz$resolution==1 & sz$latitude_code==2] - 5
  sz$lon[sz$resolution==1 & sz$latitude_code==2] <- sz$lon[sz$resolution==1 & sz$latitude_code==2] + 10
  sz$lat[sz$resolution==2 & sz$latitude_code==2] <- sz$lat[sz$resolution==2 & sz$latitude_code==2] - 2.5
  sz$lon[sz$resolution==2 & sz$latitude_code==2] <- sz$lon[sz$resolution==2 & sz$latitude_code==2] + 5
  sz$lat[sz$resolution==3 & sz$latitude_code==2] <- sz$lat[sz$resolution==3 & sz$latitude_code==2] - 2.5
  sz$lon[sz$resolution==3 & sz$latitude_code==2] <- sz$lon[sz$resolution==3 & sz$latitude_code==2] + 2.5
  sz$lat[sz$resolution==4 & sz$latitude_code==2] <- sz$lat[sz$resolution==4 & sz$latitude_code==2] - .5
  sz$lon[sz$resolution==4 & sz$latitude_code==2] <- sz$lon[sz$resolution==4 & sz$latitude_code==2] + .5

  sz$lat1 <- sz$lat
  sz$lon1 <- sz$lon
  sz$lat1[sz$resolution %in% c(1,2,3)] <- NA
  sz$lon1[sz$resolution %in% c(1,2,3)] <- NA

  sz$lat5 <- 5 * floor(sz$lat/5) +2.5
  sz$lon5 <- 5 * floor(sz$lon/5) +2.5
  sz$lat5[sz$resolution %in% c(1,2)] <- NA
  sz$lon5[sz$resolution %in% c(1,2)] <- NA

  sz$lat510 <- 5 * floor(sz$lat/5) +2.5
  sz$lon510 <- 10 * floor(sz$lon/10) +5
  sz$lat510[sz$resolution %in% c(1)] <- NA
  sz$lon510[sz$resolution %in% c(1)] <- NA

  sz$lat2010 <- 10 * floor(sz$lat/10) + 5
  sz$lon2010 <- 20 * floor(sz$lon/20) + 10

  sz$quarter <- rep(c(.125,.375,.625,.875), each = 3)[sz$month]
  sz$yrqtr <- sz$year + sz$quarter
  sz$latlon5 <- as.factor(paste(sz$lat5,sz$lon5))
  sz$latlon1 <- as.factor(paste(sz$lat1,sz$lon1))
  sz$latlon510 <- as.factor(paste(sz$lat510,sz$lon510))
  sz$latlon2010 <- as.factor(paste(sz$lat2010,sz$lon2010))

  return(sz)
}

tabfun <- function(a, ppp) {
  a[is.na(a[,ppp]),ppp] <- 99
  tapply(a$num, a[,c(ppp,"year")], sum)
}
tabfun2 <- function(a, pp1, pp2) {
  a[is.na(a[,pp1]),pp1] <- 99
  a[is.na(a[,pp2]),pp2] <- 99
  tapply(a$num, a[,c(pp1,pp2)], sum)
}
tabfun3 <- function(a, pp1, pp2, pp3) {
  a[is.na(a[,pp1]),pp1] <- 99
  a[is.na(a[,pp2]),pp2] <- 99
  a[is.na(a[,pp3]),pp3] <- 99
  a$x <- apply(a[,c(pp2,pp3)], 1,paste, collapse="_")
  tapply(a$num, a[,c(pp1,"x")], sum)
}

tabplot <- function(a,pp1,key,legloc=c(1970, 1), main = "",neww=T, doleg = T) {
  b <- tabfun(a, ppp = pp1)
  a <- t(t(b)/(apply(b,2,sum,na.rm=T)))
  if(neww) windows()
  yrs <- as.numeric(colnames(a))
  plot(yrs,a[1,], type = "l", lty=1, ylim = c(0, 1),xlab = "Years", ylab = "Proportion", main=main)
  for(i in 2:dim(a)[1]) lines(yrs, a[i,], col = i)
  if(doleg) {
    if(length(legloc)==2)   legend(legloc[1], legloc[2], legend=key[["leg2"]], lty=1, col = 1:i)
    else legend(legloc, legend=key[["leg2"]], lty=1, col = 1:i)
  }
}

tabplot_nums <- function(a,pp1,key,legloc=c(1970, 1)) {
  a <- tabfun(a, ppp = pp1)
  windows()
  yrs <- as.numeric(colnames(a))
  plot(yrs,a[1,], type = "l", lty=1, ylim = c(0, max(a, na.rm=T)),xlab = "Years", ylab = "Individual measurements")
  for(i in 2:dim(a)[1]) lines(yrs, a[i,], col = i)
  if(length(legloc)==2)   legend(legloc[1], legloc[2], legend=key[["leg2"]], lty=1, col = 1:i)
  else legend(legloc, legend=key[["leg2"]], lty=1, col = 1:i)
}

tabplot2 <- function(a,pp1,legloc=c(1970, 1), main = "",neww=T, doleg = T, kk=F) {
  b <- tabfun(a, ppp = pp1)
  a <- t(t(b)/(apply(b,2,sum,na.rm=T)))
  if(neww) windows()
  yrs <- as.numeric(colnames(a))
  ros <- rownames(b)
  plot(yrs,a[1,], type = "b", lty=1, ylim = c(0, 1),xlab = "Years", ylab = "Proportion", main=main,cex=0.6)
  for(i in 2:dim(a)[1]) lines(yrs, a[i,], type = "b",col = match(ros[i],kk$k), pch=match(ros[i],kk$k), cex=0.6)
  kk2 <- kk$legs
  if(doleg) {
    if(length(legloc)==2)   legend(legloc[1], legloc[2], legend=kk2, lty=1, col = 1:length(kk$k))
    else legend(legloc, legend=kk2, lty=1, col = 1:length(kk$k), pch = 1:length(kk$k))
  }
}

defactor<- function(x) as.numeric(as.character(x))

plot_resp_image_resp <- function(dat,alat,alon,minlat=-50,maxlat=25,minlong=10,maxlong=220,minfrq=100,zl2=seq(40,800,by=5),
                                 ti="",fn=F,rng=c(41,55),respvar="len",sz_name="sz_plot",mapname="map_plot",yq_mode=F,
                                 llmode=F,lenplot=T,newwindow=T, sp="YFT", ylab = "Length(cm)") {
  dat$alat <- with(dat,get(alat))
  dat$alon <- with(dat,get(alon))
  dat  <- dat[dat$alat > minlat & dat$alon > minlong & dat$alon <= maxlong,]
  dat$ll <- interaction(dat$alat,dat$alon)
  a <- tapply(dat$num,dat$ll,sum, na.rm=TRUE)
  a <- a[a>minfrq & is.na(a)==F]
  dat <- dat[dat$ll %in% names(a),]
  indat <- dat[,c(respvar,"yrqtr","num","ll")]
  fmla <- paste(respvar,"~ as.factor(yrqtr) + ll")
  gc()
  print(fmla); flush.console()
  #  browser()
  model <- glm(fmla, data = indat, weights=num)
  if(fn!=F) save(model,file=paste(fn,".RData",sep=""))
  gc()
  if(llmode==F) { xt<-table(indat$ll); llmode<-names(which(xt == max(xt))) }
  # xt<-table(indat$flagfleet); ffmode<-names(which(xt == max(xt)))
  # xt<-table(indat$origin_id); oimode<-names(which(xt == max(xt)))
  # xt<-table(indat$schtype_id); scmode<-names(which(xt == max(xt)))
  # if(ffo_mode==F & !is.null(indat$flag_fleet_origin)) { xt<-table(indat$flag_fleet_origin); ffo_mode<-names(which(xt == max(xt))) }
  if(yq_mode==F) yq_mode <- median(indat$yrqtr,na.rm=T)
  if(lenplot) {
    newdat <- expand.grid(yrqtr=model$xlevels$`as.factor(yrqtr)`,ll=llmode)
    # if (!is.null(indat$flag_fleet_origin)) newdat <- expand.grid(yrqtr=sort(unique(indat$yrqtr)),ll=llmode,flag_fleet_origin=ffo_mode)
    # if(schtp) newdat <- expand.grid(yrqtr=sort(unique(indat$yrqtr)),ll=llmode)
    out2 <- predict(model, newdata=newdat,type = "response",se.fit=T)
    x11()
    yq <- defactor(newdat$yrqtr)
    plot(yq, out2$fit,type="p",xlab="Year",ylab=ylab,ylim=rng)
    segments(yq, out2$fit+out2$se.fit, yq, out2$fit-out2$se.fit,col="cadetblue")
    savePlot(sz_name,type="png")
  }

  if(newwindow) x11(width=17,height=12)
  #  browser()

#  newdat <- expand.grid(yrqtr=yq_mode,ll=unique(indat$ll))
  newdat <- expand.grid(yrqtr=yq_mode,ll=model$xlevels$ll)
  out2 <- predict(model, newdata=newdat,type = "response",se.fit=T)
  coefs <- out2$fit
  pll <- newdat$ll;
  lloc <- match(pll,dat$ll)
  plat <- dat$alat[lloc]; plon <- dat$alon[lloc]
  z <- tapply(coefs,list(plon,plat),mean)

  image(sort(unique(plon)), sort(unique(plat)), z, ylab="Latitude", xlab="Longitude",xlim=c(minlong,maxlong),ylim=c(minlat,maxlat),col=rev(heat.colors(12)),main=ti)
  contour(sort(unique(plon)), sort(unique(plat)), z, levels=zl2, add=TRUE,col=4,labcex = 0.9)
  # contour(sort(unique(dat$alon)), sort(unique(dat$alat)), z, levels=zl2, add=TRUE,col=4,labcex = 0.9)
  #map('world',  yaxt="n", xaxt="n", add=T, resolution=1, fill=TRUE)
  plot_IO(newm = F, lwdm = 2, axes = F, sp=sp)
  savePlot(mapname,type="png")

  print(model); flush.console()
}

plot_resp_image_resp_gam <- function(dat,alat,alon,minlat=-50,maxlat=25,minlong=10,maxlong=220,minfrq=100,zl2=seq(40,800,by=5), ti="",fn=F,rng=c(41,55),respvar="len",sz_name="sz_plot",mapname="map_plot",yq_mode=F, llmode=F,lenplot=T,newwindow=T, sp="YFT", ylab = "Length(cm)",initk=5, spmod=FALSE) {
  dat$alat <- with(dat,get(alat))
  dat$alon <- with(dat,get(alon))
  dat  <- dat[dat$alat > minlat & dat$alon > minlong & dat$alon <= maxlong,]
  dat$ll <- interaction(dat$alat,dat$alon)
  a <- tapply(dat$num,dat$ll,sum, na.rm=TRUE)
  a <- a[a>minfrq & is.na(a)==F]
  dat <- dat[dat$ll %in% names(a),]
  indat <- dat[,c(respvar,"yrqtr","num","ll","source","lat","lon","species")]
  fmla <- paste(respvar,"~ as.factor(yrqtr) + te(lon,lat, k=initk) +as.factor(source)")
  if(spmod) fmla <- paste(fmla,"+ as.factor(species)")
  fmla <- as.formula(fmla)
  gc()
  print(fmla); flush.console()
  model <- gam(fmla, data = indat, weights=num)
  if(fn!=F) save(model,file=paste(fn,".RData",sep=""))
  gc()
  if(llmode==F) { xt<-table(indat$ll); llmode<-names(which(xt == max(xt))) }
  if(yq_mode==F) yq_mode <- median(indat$yrqtr,na.rm=T)
  if(lenplot) {
    newdat <- expand.grid(yrqtr=model$xlevels$`as.factor(yrqtr)`,species=4,
                          lon = median(indat$lon), lat = median(indat$lat), source=1)
    out2 <- predict(model, newdata=newdat,type = "response",se.fit=T)
    windows()
    yq <- defactor(newdat$yrqtr)
    plot(yq, out2$fit,type="p",xlab="Year",ylab=ylab,ylim=rng)
    segments(yq, out2$fit+out2$se.fit, yq, out2$fit-out2$se.fit,col="cadetblue")
    savePlot(sz_name,type="png")
  }

  if(newwindow) x11(width=17,height=12)

  #  newdat <- expand.grid(yrqtr=yq_mode, ll=model$xlevels$ll,source=1)
  #  out2 <- predict(model, newdata=newdat,type = "response",se.fit=T)
  plot.gam(model,scheme=2, too.far=0.05)
  maps::map("world2",add=T, fill=T)
  savePlot(mapname,type="png")

  print(model); flush.console()
  return(model)
}
