projdir <- "~/../OneDrive_personal/OneDrive/Consulting/ICCAT/2018_Bigeye/"
#projdir <- "~/ICCAT/2018_Bigeye/"
cndir <- paste0(projdir, "CN/")
datadir1 <- paste0(cndir, "data/")
cnalysis_dir <- paste0(cndir, "analyses/")
cnfigs <- paste0(cndir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")

dir.create(cndir)
dir.create(cnfigs)
dir.create(cnalysis_dir)
setwd(cnalysis_dir)

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
library(readxl)

#install.packages("devtools")
library(devtools)
# This new library replaces the 'support functions.r' file.
#install_github("hoyles/cpue.rfmo")

library("cpue.rfmo")

#source(paste0(Rdir,"support_functions.r"))
#xsd # stop!

# ===================================================================================
# Please keep the data format consistent between years and for the ICCAT + IOTC analyses.

a <- read.csv(paste0(datadir1, "CHN logbook  ICCAT for BET.csv"))

head(a)

nms <- c("vessel","date","op_yr", "op_mon", "lat","lon","hooks","hbf", "alb","alb_wt", "bet","bet_wt", "yft","yft_wt")

# Prepare the data
rawdat <- a
names(rawdat) <- nms
head(rawdat)

#pd1 <- dataprep_JPIO(rawdat) # No changes between IO and AO functions
#pd2 <- setup_AO_regions(pd1, regB=T) # Later will also need YFT regions, and possibly alternative BET regions

# Clean the data
prepdat <- setup_AO_regions(rawdat, regB=T) # Later will also need YFT regions, and possibly alternative BET regions
hist(prepdat$hbf)
hist(prepdat$hooks)
prepdat$dmy <- as.Date(prepdat$date)
dat <- prepdat

a <- table(dat$hbf, useNA = "always")
names(a)[is.na(names(a))] <- "NA"
windows()
barplot(a, names.arg = names(a))
savePlot(file = paste0(cnfigs, "HBF_frequency"), type = "png")

a <- dat$hooks
a[is.na(a)] <- 0
windows()
a <- hist(a, nclass = 40)
savePlot(file = paste0(cnfigs, "hooks_frequency"), type = "png")

table(is.na(dat$hooks), dat$op_yr, useNA = "always")
table(is.na(dat$hooks), dat$vessel, useNA = "always")
table(is.na(dat$hooks), dat$yft, useNA = "always")
table(is.na(dat$hooks), dat$alb, useNA = "always")
table(is.na(dat$hooks), dat$bet, useNA = "always")



sum(year(prepdat$dmy) != prepdat$op_yr, na.rm = TRUE) # 0
table(year(prepdat$dmy), useNA = "always")
prepdat[is.na(year(prepdat$dmy)),] # feb 29 2010, not a leap year

clndat <- dataclean_CNIO(rawdat)
prepdat1 <- dataprep_CNIO(clndat)

save(prepdat, file="prepdat.RData")



dat <- make_clid(prepdat)
dat <- make_lbidmon(dat)
save(dat,file="CNdat.RData")
load(file="CNdat.RData")


# ===================================================================================
# Plot and explore the data
# ===================================================================================
# These plots are for checking the data. Not especially important.
table(dat$op_yr)
table(is.na(dat$clid))
table(dat$clid==0)
a <- table(dat$clid)
hist(a,nclass=max(a))
windows(height=10,width=10);par(mfrow=c(3,2))
max(a[a<100000])
a1 <- hist(a,nclass=max(a),main="cluster_id frequency",xlab="Sets per cluster_id")
plot(a1$breaks,c(a1$counts,0)*a1$breaks,xlim=c(0,max(a)),ylim=c(0,2e5),main="set frequency by cluster_id",
  xlab="Sets per cluster_id",ylab="Number of sets")

xfun <- function(x) sum(x > 0)
a <- table(dat$vessid,dat$op_yr)
apply(a,2,xfun)


vnm <- (dat$vesselname)
library("tm")
tm_map(vnm, function(x) content_transformer(iconv(enc2utf8, sub = "byte")))

a <- table(dat$vesselname,dat$op_yr)
apply(a,2,xfun)

# A potentially dodgy callsign?
a <- dat[dat$tripidmon=="461 1971 1",]
a <- data.frame(a)
a[,1:15]
a <-unique(dat$callsign)
as.data.frame(dat[match(a,dat$callsign),c("callsign","vessid")])
table(dat$callsign,useNA="always")
table(dat$vessid,useNA="always")

# Plot grid squares with sets by region, for each regional structure
a <- unique(paste(dat$lat,dat$lon))
a0 <- dat[match(a,paste(dat$lat,dat$lon)),c("lat","lon","regB")]
windows(width=15,height=10)
for(fld in c("regB")) {
  reg <- with(a0,get(fld))
  plot(a0$lon,a0$lat,type="n",xlab="Longitude",ylab="Latitude",main=fld)
  text(a0$lon,a0$lat,labels=reg,cex=0.6,col=reg+1)
  map(add=T)
  savePlot(paste0("map_",fld),type="png")
}

# Plot the effect of data cleaning on the number of sets
regYord <- c(1,2,3)
windows(height=12,width=12); par(mfcol=c(2,2),mar=c(3,2,2,1))
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

# Sets per day and per month
windows(width=15,height=9)
hist(prepdat$dmy,breaks="days",freq=T,xlab="Date",main="Sets per day")
savePlot(file="sets_per_day.png",type="png")
hist(prepdat$dmy,breaks="months",freq=T,xlab="Date",main="Sets per month")
savePlot(file="sets_per_month.png",type="png")
table(prepdat$dmy)

# Map of hook distribution, all time
a <- aggregate(dat$hooks,list(dat$lat5,dat$lon5),sum,na.rm=T)
windows(width=11,height=9)
symbols(x=a[,2],y=a[,1],circles=.0002*sqrt(a[,3]),inches=F,bg=2,fg=2,xlab="Longitude",ylab="Latitude",ylim=c(-50,25),xlim=c(15,145))
map(add=T,interior=F,fill=T)
savePlot(file="map_hooks.png",type="png")

# Histogram of hooks per set
hist(dat$hooks, nclass=60,xlab="Hooks per set")   # ask if very large # hooks is okay
savePlot("Hook histogram.png",type="png")

# Check catch distribtions for outliers. Probably no need to remove.
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
table(prepdat$hbf,useNA="always")  # 6408 with NA! All in 1973-75

table(dat$hbf,dat$op_yr,useNA="always")  #
table(dat$op_yr,is.na(dat$hbf))  #

# Store some results (aggregated to avoid data concerns) for later reporting.
dat <- dat[is.na(dat$hbf)==FALSE | dat$op_yr < 1976,]
a <- table(dat$op_yr,round(dat$hbf,0),useNA="always")
write.csv(a,"table hbf by year.csv")

# Set density map by 5 degree cell
table(clndat$loncode) # all good
a <- log(table(dat$lon5,dat$lat5))
windows(width=13,height=10)
image(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,xlab="Longitude",ylab="Latitude")
map("worldHires",add=T, interior=F,fill=F)
savePlot("Setmap_logscale.png",type="png")

# Set density map by 1 degree cell
a <- with(dat[!is.na(dat$lat) & dat$yrqtr,],log(table(lon,lat)))
windows(width=15,height=10)
image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,xlab="Longitude",ylab="Latitude",ylim=c(-55,30),xlim=c(15,150))
map("worldHires",add=T, interior=F,fill=T)
savePlot("Setmap_logscale_1deg.png",type="png")

a <- with(dat[!is.na(dat$lat) & dat$yrqtr,],tapply(regB,list(lon,lat),mean))
windows(width=15,height=10)
image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,col=2:6,xlab="Longitude",ylab="Latitude")
map("worldHires",add=T, interior=F,fill=T)
savePlot("regbet.png",type="png")

a <- tapply(dat$regY,list(dat$lon,dat$lat),mean,na.rm=T)
windows(width=15,height=10)
image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,col=1:6,xlab="Longitude",ylab="Latitude")
map("worldHires",add=T, interior=F,fill=T)
savePlot("regyft.png",type="png")

# Mean fishing location  by yearqtr
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

# Mean fishing location by year
windows(width=15,height=10);par(mfrow=c(1,2))
plot(tapply(dat$op_yr,dat$op_yr,mean),tapply(dat$lat5,dat$op_yr,mean),xlab="yr",ylab="Mean latitude")
plot(tapply(dat$lon5,dat$op_yr,mean),tapply(dat$op_yr,dat$op_yr,mean),ylab="yr",xlab="Mean longitude")
savePlot("mean_fishing_location2.png",type="png")

write.csv(table(round(dat$hbf,0),dat$regY,useNA="always"),file="hbf by region.csv")
write.csv(table(round(dat$hbf,0),floor(dat$yrqtr/5)*5,dat$regY,useNA="always"),file="hbf by region by 5 years.csv")

# Plot hbf. Change spatial selection criteria for AO.
windows(20,14);par(mfrow=c(3,3),mar=c(2,2,2,2))
for(y in seq(1975,2015,5)) {
  a <- dat[floor(dat$yrqtr/5)*5==y & dat$lon5 < 125 & dat$lat5 < 25,]
  a <- tapply(a$hbf,list(a$lon5,a$lat5),mean,na.rm=T)
  image(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,main=y,zlim=c(6,24),col=heat.colors(30),xlab="Lon",ylab="Lat",ylim=c(-45,25))
  contour(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,add=T,levels=seq(0,26,2))
  map("worldHires",add=T, interior=F,fill=T) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
  }
savePlot("mean_HBF.png",type="png")
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
  savePlot(paste0("mean_HBF_q",qq,".png"),type="png")
  }

#write.csv(table(dat$ncrew,dat$reg),file="crew by region.csv")
#write.csv(table(dat$ncrew,floor(dat$yrqtr/10)*10),file="crew by decade.csv")
#write.csv(table(dat$ncrew,dat$fishingcat,useNA="ifany"),file="crew by fishingcat.csv")
write.csv(table(dat$lat5,dat$lon5),file="ops by lat-long.csv")
write.csv(table(dat$lat5,dat$lon5,5*floor(dat$yrqtr/5)),file="ops by lat-long-5yr.csv")

# data exploration
#install.packages("rpart")
library("rpart")
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
simplemod <- rpart(a$betcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$albcpue + a$yftcpue + a$mlscpue + a$blmcpue + a$bumcpue)
windows(width=11,height=7)
plot(simplemod)
text(simplemod)
savePlot("Rpart bet cpue",type="png")
simplemod <- rpart(a$yftcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$albcpue + a$betcpue + a$mlscpue + a$blmcpue + a$bumcpue)
windows(width=11,height=7)
plot(simplemod)
text(simplemod)
savePlot("Rpart yft cpue",type="png")

library("randomForest") # These take a long time and use a lot of memory, but are useful.
simplefor <- randomForest(a$betcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$albcpue + a$yftcpue + a$mlscpue + a$blmcpue + a$bumcpue)
print(simplefor)
windows(width=11,height=7)
plot(importance)
text(varImpPlot,main=NULL)
savePlot("Rforest bet cpue",type="png")
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

projdir <- "~/ICCAT/2018_CPUE/"
cndir <- paste0(projdir, "CN/")
datadir1 <- paste0(cndir, "data/")
cnalysis_dir <- paste0(cndir, "analyses/")
Rdir <- paste0(projdir, "Rfiles/")
clusdir <- paste0(cndir, "clustering/")
setwd(clusdir)
load(file=paste0(cnalysis_dir,"CNdat.RData"))
str(dat)

# Remove redundant datasets, if present. Don't worry if not.
rm(dat2,prepdat,prepdat1,pd1,pd2,clndat,dat5214,rawdat,dataset,llv,dat9415b,dat9415hd,a5,lnk,a2,a0,a)

gc()
allsp <- c("alb","bet","yft","swo","mls","bum","blm","sbt")
allabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tripid","tripidmon","clid","jnt_clid","lbid_mon","moon","alb","bet","yft","swo", "mls","bum","blm","sbt","Total","dmy","lat","lon","lat5","lon5","regB")
dat <- data.frame(dat)
str(dat[,allabs])


nclB=c(4,4,4) # Number of bigeye clusters. Will need to be adjusted for each fleet.
flag="CN"
cvn <- c("yrqtr","latlong","hooks","hbf","vessid","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")
r=4


regtype="regB"
for(r in 1:length(nclB)) {
  fnh <- paste(flag,regtype,r,sep="_")
  dataset <- clust_PCA_run(r=r,ddd=dat,allsp=allsp,allabs=allabs,regtype=regtype,ncl=nclB[r],plotPCA=F,clustid="lbid_mon",allclust=F,flag=flag,fnhead=fnh,covarnames=cvn)
  save(dataset,file=paste0(fnh,".RData"))
}

##################################################
# Japan only, no clusters, HBF
#

resdir <- paste0(cnalysis_dir,"std_nocl_CNonly_hbf/")
dir.create(resdir)
setwd(resdir)

clkeepCN_B <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepJP_B <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepKR_B <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepTW_B <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepUS_B <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clk_B <- list(CN=clkeepCN_B,JP=clkeepJP_B,KR=clkeepKR_B,TW=clkeepTW_B,US=clkeepUS_B)

runpars <- list()
runpars[["bet"]] <- list(regtype = "regB", regtype2 = "B", clk = clk_B, doregs = 1:3, addcl = FALSE, dohbf = TRUE, cltype = "hcltrp")

keepd = FALSE; maxyr = 2018; maxqtrs=200; minqtrs_byreg = c(5,5,5);
for(runsp in c("bet")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  jdat <- data.frame()
  for(flag in c("CN")) {
    for(r in runpars[[runsp]]$doregs) {
      load(paste0(projdir,flag,"/clustering/",paste(flag,regtype,r,sep="_"),".RData"))
      dataset$flag <- flag
      jdat <- rbind(jdat,dataset[,allabs])
      rm(dataset)
    }
  }
  jdat <- jdat[jdat$yrqtr < maxyr,]
  jdat$vessidx <- jdat$vessid
  jdat$vessid <- paste0(jdat$flag,jdat$vessid)
  jdat$vessid <- as.factor(jdat$vessid)
  jdat <- jdat[jdat$yrqtr > 2005 | jdat$flag != "TW",]

  vars <- c("vessid","hooks","yrqtr","latlong","hbf")
  for(runreg in runpars[[runsp]]$doregs) {
      minqtrs <- minqtrs_byreg[runreg]
      glmdat <- select_data_JointIO(jdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars, maxqtrs = maxqtrs, minvess = 50, minll = 50, minyrqtr = 50, addcl = addcl, cltype = cltype, addpca = NA, samp = NA, strsmp = NA)
      if(nrow(glmdat)>60000) glmdat <- samp_strat_data(glmdat,60)
      glmdat5279 <- select_data_JointIO(jdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,maxqtrs=maxqtrs, minvess=50,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1952,1980))
      if(nrow(glmdat5279)>60000) glmdat5279 <- samp_strat_data(glmdat5279,60)
      a <- jdat[jdat$vessid != "CN1",]
      glmdat79nd <- select_data_JointIO(a,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,maxqtrs=maxqtrs, minvess=50,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1979,maxyr))
      if(nrow(glmdat79nd)>60000) glmdat79nd <- samp_strat_data(glmdat79nd,60)
      wtt.all   <- mk_wts(glmdat,wttype="area")
      wtt.5279   <- mk_wts(glmdat5279,wttype="area")
      wtt.79nd   <- mk_wts(glmdat79nd,wttype="area")
      fmla.oplogn <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=F,addcl=T,nhbf=3)
      fmla.oplogn_ncl <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=F,addcl=F,nhbf=3)
      fmla.boatlogn <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=T,addcl=T,nhbf=3)
      fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=T,addcl=F,nhbf=3)
      mn <- with(glmdat,0.1* mean(get(runsp)/hooks))

      modlab="lognC_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
      if(lu(glmdat$clust) > 1)
      { model <- glm(fmla.oplogn,data=glmdat,weights=wtt.all,family="gaussian");gc() } else
      { model <- glm(fmla.oplogn_ncl,data=glmdat,weights=wtt.all,family="gaussian");gc() }
      summarize_and_store(mod=model,dat=glmdat,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

      modlab="lognC_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
      if(lu(glmdat$clust) > 1)
      { model <- glm(fmla.boatlogn,data=glmdat,weights=wtt.all,family="gaussian");gc() } else
      { model <- glm(fmla.boatlogn_ncl,data=glmdat,weights=wtt.all,family="gaussian");gc() }
      summarize_and_store(mod=model,dat=glmdat,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

      modlab="lognC_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
      mn <- with(glmdat5279,0.1* mean(get(runsp)/hooks))
      if(lu(glmdat5279$clust) > 1)
      { model <- glm(fmla.oplogn,data=glmdat5279,weights=wtt.5279,family="gaussian");gc() } else
      { model <- glm(fmla.oplogn_ncl,data=glmdat5279,weights=wtt.5279,family="gaussian");gc() }
      summarize_and_store(mod=model,dat=glmdat5279,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

      modlab="lognC_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
      mn <- with(glmdat79nd,0.1* mean(get(runsp)/hooks))
      if(lu(glmdat79nd$clust) > 1)
      { model <- glm(fmla.boatlogn,    data=glmdat79nd,weights=wtt.79nd,family="gaussian");gc() } else
      { model <- glm(fmla.boatlogn_ncl,data=glmdat79nd,weights=wtt.79nd,family="gaussian");gc() }
      summarize_and_store(mod=model,dat=glmdat79nd,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

      # delta lognormal
      modlab="dellog_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg);
      do_deltalog(dat=glmdat,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

      modlab="dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
      do_deltalog(dat=glmdat,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

      modlab="dellog_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
      do_deltalog(dat=glmdat5279,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

      modlab="dellog_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
      do_deltalog(dat=glmdat79nd,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

      graphics.off()
    }
}

