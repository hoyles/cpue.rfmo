projdir <- "~/ICCAT/2019_YFT/"

brdir <- paste0(projdir, "BR/")
datadir <- paste0(brdir, "data/")
bralysis_dir <- paste0(brdir, "analyses/")
brfigs <- paste0(brdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
dir.create(bralysis_dir)
dir.create(brfigs)
setwd(bralysis_dir)

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
library("readxl")


# install.packages("devtools")
library(devtools)
# This new library replaces the 'support functions.r' file.
# install_github("hoyles/cpue.rfmo")

library("cpue.rfmo")

#source(paste0(Rdir,"support_functions.r"))
#xsd # stop!

# ===================================================================================
# Please keep the data format consistent between years and for the ICCAT + IOTC analyses.
load(paste0(datadir, "Pelagic_LL_BR_ver00.RData"))
# check the data
str(dt)
names(dt)

br_splist <- c("yft", "alb","bet", "swo", "sai", "whm", "bum", "bsh",
               "spx", "bth", "sma", "ocs", "fal", "ccs")


# Prepare and clean the data
prepdat2 <- setup_AO_regions(dt, regB = TRUE, regB1 = TRUE, regY = TRUE, regY1 = TRUE)

table(prepdat2$regB, prepdat2$regY)

head(prepdat2)
prepdat2 <- as.data.frame(prepdat2)
dat <- dataclean_BR(prepdat2, yearlim = 2018, splist = br_splist)
save(prepdat,dat,file = "BRdat.RData")


# ===================================================================================
# Plot and explore the data
# ===================================================================================
# These plots are for checking the data. Not especially important.
load(file = "BRdat.RData")
str(dat)
summary(dat)
table(dat$vessid,dat$op_yr)
table(dat$op_yr,dat$vessid)
table(dat$op_yr,is.na(dat$vessid))
table(dat$op_yr,(dat$hooks > 0))

# Sets per day
dev.new(width = 15,height = 9)
hist(prepdat$dmy,breaks = "days",freq = T,xlab = "Date",main = "Sets per day")
savePlot(filename = "sets_per_day.png",type = "png")
table(prepdat$dmy)

# a <- dat[grep("ZA240",dat$VESSEL_CD),]
# plot(a$lon,a$lat,type = "b")
# text(jitter(a$lon),jitter(a$lat),a$dmy)
# a <- dat[grep("ZA000",dat$VESSEL_CD),]
# a <- a[a$op_yr == 1984,]
# plot(a$lon,a$lat,type = "b")
# text(jitter(a$lon),jitter(a$lat),a$dmy)
# na <- length(a$lat)
# dista <- ((a[2:na,"lat"]-a[1:(na-1),"lat"])^2 + (a[2:na,"lon"]-a[1:(na-1),"lon"])^2)^0.5
# timea <- (a[2:na,"dmy"]-a[1:(na-1),"dmy"])
# kperday <- 111 * dista/as.numeric(timea)
# cbind(dista,timea,kperday)

dev.new()
hist(prepdat$hooks, nclass=200)   # ask if very large # hooks is okay
savePlot("Hook histogram.png",type="png")
#hist(prepdat$hooks, nclass=200,xlim=c(0,1200),ylim=c(0,500))
hist(prepdat$floats, nclass=200)   # ask if the sets with 1200 floats are reasonable
savePlot("floats histogram.png",type="png")

table(dat$alb)
table(dat$bet)
table(dat$bft)
table(dat$blm)
table(dat$bum)
table(dat$skj)
table(dat$sha)
table(dat$mls)
table(dat$whm)
dat$mls <- dat$mls + dat$whm
dat$whm <- NULL
table(dat$yft)
table(dat$sfa)
table(prepdat$floats,useNA = "always")  # 6408 with NA! All in 1973-75
table(is.na(prepdat$floats),prepdat$op_yr,useNA="always")  # 6408 with NA!
table(round(prepdat$hbf,0),prepdat$op_yr,useNA="always")  # Looks like 1971-76 have few sets with full data and may not be usable
a <- table(prepdat$op_yr,round(prepdat$hbf,0),useNA="always")
write.csv(a,"table hbf by year.csv")

# a <- dat
# a$lon5[a$lon5 > 180] <- a$lon5[a$lon5 > 180] - 360
a <- aggregate(dat$hooks,list(dat$lat5,dat$lon5),sum,na.rm=T)

dev.new(width = 11,height = 9)
symbols(x=a[,2],y=a[,1],circles=.001*sqrt(a[,3]),inches=F,bg=2,fg=2,xlab="Longitude",ylab="Latitude")
map("world",add=T,interior=F,fill=T)
savePlot(filename = "map_hooks.png",type = "png")

table(dat$EW) # Some data with 2
a <- log(table(dat$lon,dat$lat))
dev.new(width = 15,height = 10)
image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a)
map("worldHires",add = TRUE, fill = TRUE) # Diagonal stripe?!
savePlot("Setmap_logscale.png",type = "png")

a <- dat[dat$lon > 20,]
dim(a)
plot(a$lon, a$lat) # lat = -1 * lon
hist(a$lon)
table(a$VESSEL_NAME)
str(a)
table(a$EW)
b <- rawdat[rawdat$Long01 > 20 & rawdat$EW == 1,]
dim(b)
plot(b$Long01, b$Lat01) # the problem was in rawdat, i.e. the original data
table(a$op_yr)
table(a$op_mon)
summary(a)
table(dat$op_yr)
a1 <- dat[dat$op_yr >= 2014,]
table(a1$VESSEL_NAME)
table(a$VESSEL_NAME)
dev.new(12,12);par(mfrow = c(4,4), mar = c(4,2,3,1))
for (y in 2001:2016) with(dat[dat$op_yr == y,],plot(lon, lat, main = y))

a <- tapply(dat$regB,list(dat$lon,dat$lat),mean)
dev.new(width = 15,height = 10)
image(as.numeric(dimnames(a)[[1]]) + .5,as.numeric(dimnames(a)[[2]]) + .5,a,col = 1:4)
map("worldHires",add=T) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
savePlot("regbet.png",type="png")

# Plot grid squares with sets by region, for each regional structure
dev.new(width = 15,height = 10);par(mfrow=c(1,2))
plot(tapply(dat$op_yr,dat$op_yr,mean),tapply(dat$lat,dat$op_yr,mean),xlab = "yr",ylab = "Mean latitude")
plot(tapply(dat$lon,dat$op_yr,mean),tapply(dat$op_yr,dat$op_yr,mean),ylab = "yr",xlab = "Mean longitude")
savePlot("mean_fishing_location 2.png",type = "png")
plot(tapply(dat$yrqtr,dat$yrqtr,mean),tapply(dat$lat,dat$yrqtr,mean),xlab = "yr",ylab = "Mean latitude")
plot(tapply(dat$lon,dat$yrqtr,mean),tapply(dat$yrqtr,dat$yrqtr,mean),ylab = "yr",xlab = "Mean longitude")
savePlot("mean_fishing_location 1.png",type = "png")

write.csv(table(round(dat$hbf,0),dat$regB,useNA = "always"),file = "hbf by region.csv")
write.csv(table(round(dat$hbf,0),floor(dat$yrqtr/5)*5,dat$regB,useNA = "always"),file = "hbf by region by 5 years.csv")

dev.new(20,20);par(mfrow = c(3,3), mar = c(4,4,2,1)+.1)
for (y in seq(1975,2015,5)) {
  a <- dat[floor(dat$yrqtr/5)*5 == y & dat$lon < 125 & dat$lat < 25,]
  a <- tapply(a$hbf,list(a$lon,a$lat),mean,na.rm = T)
  image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,main = y,zlim = c(6,24),col = heat.colors(30),xlab = "Lon",ylab = "Lat",ylim = c(-45,40))
  contour(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,add = T,levels = seq(0,26,2))
  map("world",add = TRUE, fill = TRUE)
}
savePlot("mean_HBF.png",type = "png")
dev.new(20,20);par(mfrow = c(2,2))
for (y in c(1978,1988,1998,2008)) {
  a <- dat[dat$yrqtr>y & dat$yrqtr < y+10  & dat$lon < 125,]
  a <- tapply(a$hbf,list(a$lon,a$lat),mean)
  image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,main = y,zlim = c(6,24),col = heat.colors(30),xlab = "Lon",ylab = "Lat",xlim = c(-80,20),ylim = c(-45,40))
  contour(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,add = T,levels = seq(0,26,1),col = "dark blue")
  map("world",add = T, fill = TRUE) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
}
savePlot("mean_HBF_10yr.png",type = "png")

#write.csv(table(dat$ncrew,dat$reg),file = "crew by region.csv")
#write.csv(table(dat$ncrew,floor(dat$yrqtr/10)*10),file = "crew by decade.csv")
#write.csv(table(dat$ncrew,dat$fishingcat,useNA = "ifany"),file = "crew by fishingcat.csv")
write.csv(table(dat$lat5,dat$lon5),file = "ops by lat-long.csv")
write.csv(table(dat$lat5,dat$lon5,5*floor(dat$yrqtr/5)),file = "ops by lat-long-5yr.csv")

# data exploration
library(rpart)
a <- dat[dat$regB%in% c(2,5),]
dim(a)
a$betcpue <- a$bet/a$hooks
a$albcpue <- a$alb/a$hooks
a$yftcpue <- a$yft/a$hooks
a$bftcpue <- a$bft/a$hooks
a$swocpue <- a$swo/a$hooks
a$sfacpue <- a$sfa/a$hooks
a$mlscpue <- a$mls/a$hooks
a$blmcpue <- a$blm/a$hooks
a$bumcpue <- a$bum/a$hooks
simplemod <- rpart(a$betcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$albcpue + a$sfacpue + a$mlscpue + a$blmcpue + a$bumcpue)
dev.new(width = 11,height = 7)
plot(simplemod)
text(simplemod)


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

clustdir <- paste0(brdir,"clustering/")
dir.create(clustdir)
setwd(clustdir)

load(file = paste0(bralysis_dir, "BRdat.RData"))

flag <- "BR"
br_splist <- c("alb","bet","bft","blm","bum","mls","oth","sfa","sha","skj","swo","yft")

use_splist <- c("alb","bet","bum","mls","sfa","swo","yft")
allabs <- c("op_yr","op_mon","hooks",use_splist, "Total","dmy","hbf","moon","lat","lon","lat5","lon5","yrqtr","latlong","vessid","tripidmon","regB","regB1")
dat <- data.frame(dat)

for (r in c(2:3)) {
  dev.new(15,12); par(mfrow = c(4,3), mar = c(3,2,2,1), oma = c(0,0,2,0))
  a <- dat[dat$regB == r,]
  for (sp in br_splist) plot(sort(unique(a$yrqtr)),tapply(a[,sp], a$yrqtr, mean), main = sp)
  title(paste("Region", r ), outer = TRUE)
  savePlot(filename = paste("freq",flag,"Region", r, sep = "_"), type = "png")
}

nclB <- c(4,4,4)
cvn <- c("yrqtr","latlong","hooks","hbf","vessid","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")

regtype <- "regB"
for (r in 2:3) {
  fnh <- paste(flag,regtype,r,sep = "_")
  dataset <- clust_PCA_run(r = r,ddd = dat,allsp = use_splist,allabs = allabs,regtype = regtype,ncl = nclB[r],plotPCA = F,clustid = "tripidmon",allclust = F,flag = flag,fnhead = fnh,covarnames = cvn)
  save(dataset,file = paste0(fnh,".RData"))
}

#################### Run standardization models for each species ##########################
#***********************************************
#  RUN MULTISPECIES STANDARDIZATION PROCEFURES #
#***********************************************


##################################################
# Korea only, clusters, HBF
#
# R2 - 4 clusters. 1=yft+bet, 2=bet+alb, 3=bet, 4=bet+yft+swo. Use 1,2,3,4
# R3 - 3 or 4 clusters. 1=alb+bet, 2=bet, 3=yft+bet+alb, 4=alb+bft+sfa. Use 1,2,3

projdir <- "~/ICCAT/2018_Bigeye/"

brdir <- paste0(projdir, "BR/")
datadir <- paste0(brdir, "data/")
bralysis_dir <- paste0(brdir, "analyses/")
brfigs <- paste0(brdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
dir.create(bralysis_dir)
dir.create(brfigs)
setwd(bralysis_dir)

resdir <- paste0(bralysis_dir,"std_cl_BRonly_hbf/")
dir.create(resdir)
setwd(resdir)

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
library("readxl")

library(devtools)

library("cpue.rfmo")

splist <- c("alb", "bet", "yft")
stdlabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks", splist, "hcltrp", "Total","lat","lon","lat5","lon5", "reg", "flag")

#clkeepCN_B <- list("bet" = list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepJP_B <- list("bet" = list(c(1,2,4),c(1,2,3,4),c(1,2,3)))
clkeepKR_B <- list("bet" = list(c(0),c(1,2,3,4),c(1,2,3)))
clkeepBR_B <- list("bet" = list(c(0),c(1,2,3,4),c(1,2,3)))
clkeepTW_B <- list("bet" = list(c(4),c(2,3),c(0)))
clkeepUS_B <- list("bet" = list(c(2,3),c(1,3),c(0)))
clk_B <- list(JP = clkeepJP_B,KR = clkeepKR_B,BR = clkeepBR_B,TW = clkeepTW_B,US = clkeepUS_B)

runpars <- list()
runpars[["bet"]] <- list(regtype = "regB", regtype2 = "B", clk = clk_B, doregs = 2, addcl = TRUE, dohbf = TRUE, cltype = "hcltrp")

runreg = 1; runsp = "bet"

keepd = TRUE; maxyr = 2018; maxqtrs = 200; minqtrs_byreg = c(5,5,5);
for (runsp in c("bet")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  jdat <- data.frame()
  for (flag in c("BR")) {
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
    glmdat <- select_data_JointIO(jdat,runreg = runreg,clk = clk,minqtrs = minqtrs,runsp = runsp,mt = "deltabin",vars = vars,maxqtrs = maxqtrs,minvess = 50,minll = 50,minyrqtr = 50,addcl = addcl,cltype = cltype,addpca = NA,samp = NA,strsmp = NA)
    if (nrow(glmdat)>60000) glmdat <- samp_strat_data(glmdat,60)
    a <- jdat[jdat$vessid != "BR1",]

    wtt.all   <- mk_wts(glmdat,wttype = "area")
    fmla.oplogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = T,nhbf = 3)
    fmla.oplogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = F,nhbf = 3)
    fmla.boatlogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = T,nhbf = 3)
    fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = F,nhbf = 3)
    mn <- with(glmdat,0.1* mean(get(runsp)/hooks))

    modlab = "lognC_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if (lu(glmdat$clust) > 1)
    { model <- glm(fmla.oplogn,data = glmdat,weights = wtt.all,family = "gaussian", y  =  keepd, model = keepd);gc() } else
    { model <- glm(fmla.oplogn_ncl,data = glmdat,weights = wtt.all,family = "gaussian", y = keepd, model = keepd);gc() }
    summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

    modlab = "lognC_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if (lu(glmdat$clust) > 1)
    { model <- glm(fmla.boatlogn,data = glmdat,weights = wtt.all,family = "gaussian", y = keepd, model = keepd);gc() } else
    { model <- glm(fmla.boatlogn_ncl,data = glmdat,weights = wtt.all,family = "gaussian", y = keepd, model = keepd);gc() }
    summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

    # delta lognormal
    modlab = "dellog_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg);
    do_deltalog(dat = glmdat,dohbf = dohbf,addboat = F,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

    modlab = "dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat = glmdat,dohbf = dohbf,addboat = T,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)
  }
}
