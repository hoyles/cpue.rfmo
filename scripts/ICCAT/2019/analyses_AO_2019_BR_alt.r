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

options(device = "windows")

# ===================================================================================
# Please keep the data format consistent between years and for the ICCAT + IOTC analyses.
load(paste0(datadir, "BRdat.RData"))
# check the data
str(dat)
names(dat)

br_splist <- c("yft", "alb","bet", "swo", "sai", "whm", "bum", "bsh",
               "spx", "bth", "sma", "ocs", "fal", "ccs")

# Fix the problem with lat5 & lon5 not being in the middle of the cell
a <- sum(prepdat$lat5 - floor(prepdat$lat5))
if(a==0) {
  prepdat$lat5 <- prepdat$lat5 + 2.5
  prepdat$lon5 <- prepdat$lon5 + 2.5
}
prepdat$latlong <- paste(prepdat$lat5, prepdat$lon5)

# Prepare and clean the data
prepdat2 <- setup_AO_regions(prepdat, regB = TRUE, regB1 = TRUE, regY = TRUE, regY1 = TRUE, regY2 = TRUE)

table(prepdat2$regB, prepdat2$regY)

head(prepdat2)
prepdat2 <- as.data.frame(prepdat2)
dat <- dataclean_BR(prepdat2, yearlim = 2018, splist = br_splist)

save(prepdat,dat,file = "BRdat2.RData")


# ===================================================================================
# Plot and explore the data
# ===================================================================================
# These plots are for checking the data. Not especially important.
load(file = "BRdat2.RData")
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
dat$floats <- dat$hooks / dat$hbf
hist(dat$floats, nclass=200)   # ask if the sets with 1200 floats are reasonable
savePlot("floats histogram.png",type="png")

# "yft", "alb","bet", "swo", "sai", "whm", "bum", "bsh",
# "spx", "bth", "sma", "ocs", "fal", "ccs"
table(dat$alb)
table(dat$bet)
table(dat$whm)
table(dat$bum)
table(dat$bsh)
table(dat$sai)
table(dat$swo)
table(dat$spx)
table(dat$yft)
table(dat$spx)
table(prepdat$floats,useNA = "always")  # 6408 with NA! All in 1973-75
table(is.na(prepdat$floats),prepdat$op_yr,useNA="always")  # 6408 with NA!
table(round(prepdat$hbf,0),prepdat$op_yr,useNA="always")  # Looks like 1971-76 have few sets with full data and may not be usable
a <- table(prepdat$op_yr,round(prepdat$hbf,0),useNA="always")
write.csv(a,"table hbf by year.csv")

# a <- dat
# a$lon5[a$lon5 > 180] <- a$lon5[a$lon5 > 180] - 360
a <- aggregate(dat$hooks,list(dat$lat5,dat$lon5),sum,na.rm=T)

dev.new(width = 9,height = 10)
symbols(x=a[,2],y=a[,1],circles=.0005*sqrt(a[,3]),inches=F,bg=2,fg=2,xlab="Longitude",ylab="Latitude")
map("world",add=T,interior=F,fill=T)
savePlot(filename = "map_hooks.png",type = "png")

a <- log(table(dat$lon,dat$lat))
dev.new(width = 9,height = 11)
image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a)
map("worldHires",add = TRUE, fill = TRUE) # Diagonal stripe?!
savePlot("Setmap_logscale.png",type = "png")

dev.new(12,12);par(mfrow = c(5,5), mar = c(3,2,1,1))
for (y in 1993:2017) with(dat[dat$op_yr == y,],plot(lon, lat, main = y))

a <- tapply(dat$regB,list(dat$lon,dat$lat),mean)
dev.new(width = 10,height = 12)
image(as.numeric(dimnames(a)[[1]]) + .5,as.numeric(dimnames(a)[[2]]) + .5,a,col = 1:4)
map("worldHires",add=T) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
savePlot("regbet.png",type="png")

a <- tapply(dat$regY1,list(dat$lon,dat$lat),mean)
dev.new(width = 10,height = 12)
image(as.numeric(dimnames(a)[[1]]) + .5,as.numeric(dimnames(a)[[2]]) + .5,a,col = 1:4)
map("worldHires",add=T) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
savePlot("regY1.png",type="png")

# Mean fishing location  by yearqtr
dev.new(noRStudioGD = TRUE,width = 15,height = 10);par(mfrow = c(1,2))
ax <- tapply(dat$yrqtr,dat$yrqtr,mean); ay = tapply(dat$lat5,dat$yrqtr,mean)
plot(ax,ay,xlab = "yr",ylab = "Mean latitude",type = "n")
a <- 4 * (.125+dat$yrqtr-floor(dat$yrqtr))
a <- tapply(a,dat$yrqtr,mean)
text(ax,ay,a,cex = 0.7)
ax = tapply(dat$lon5,dat$yrqtr,mean);ay = tapply(dat$yrqtr,dat$yrqtr,mean)
plot(ax,ay,ylab = "yr",xlab = "Mean longitude",type = "n")
text(ax,ay,a,cex = 0.7)
savePlot("mean_fishing_location1.png",type = "png")

# Mean fishing location by year
dev.new(noRStudioGD = TRUE,width = 15,height = 10);par(mfrow = c(1,2))
plot(tapply(dat$op_yr,dat$op_yr,mean),tapply(dat$lat5,dat$op_yr,mean),xlab = "yr",ylab = "Mean latitude")
plot(tapply(dat$lon5,dat$op_yr,mean),tapply(dat$op_yr,dat$op_yr,mean),ylab = "yr",xlab = "Mean longitude")
savePlot("mean_fishing_location2.png",type = "png")

write.csv(table(round(dat$hbf,0),dat$regB,useNA = "always"),file = "hbf by region.csv")
write.csv(table(round(dat$hbf,0),floor(dat$yrqtr/5)*5,dat$regB,useNA = "always"),file = "hbf by region by 5 years.csv")

dev.new(width=15,height=20);par(mfrow = c(3,3), mar = c(4,4,2,1)+.1)
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
a <- dat[dat$regY1%in% c(2,3),]
dim(a)
a$betcpue <- a$bet/a$hooks
a$albcpue <- a$alb/a$hooks
a$yftcpue <- a$yft/a$hooks
a$swocpue <- a$swo/a$hooks
a$saicpue <- a$sai/a$hooks
a$whmcpue <- a$whm/a$hooks
a$bshcpue <- a$bsh/a$hooks
a$bumcpue <- a$bum/a$hooks
simplemod <- rpart(a$yftcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$albcpue + a$saicpue + a$whmcpue + a$bshcpue + a$bumcpue + a$betcpue)
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
br_splist <- c("yft", "alb","bet", "swo", "sai", "whm", "bum", "bsh","spx", "bth", "sma", "ocs", "fal", "ccs")

use_splist <- c("yft", "alb","bet", "swo", "sai", "whm", "bum", "bsh","sma")
allabs <- c("op_yr","op_mon","hooks",use_splist, "Total","dmy","hbf","moon","lat","lon","lat5","lon5","yrqtr","latlong","vessid","tripidmon","regY","regY1")
dat <- data.frame(dat)
allabs %in% names(dat)

for (r in c(2:3)) {
  dev.new(width = 15, height = 12);
  par(mfrow = c(4,4), mar = c(3,2,2,1), oma = c(0,0,2,0))
  a <- dat[dat$regB == r,]
  for (sp in br_splist) plot(sort(unique(a$yrqtr)),tapply(a[,sp], a$yrqtr, mean), main = sp)
  title(paste("Region", r ), outer = TRUE)
  savePlot(filename = paste("freq",flag,"Region", r, sep = "_"), type = "png")
}

nclB <- c(4,5,4)
cvn <- c("yrqtr","latlong","hooks","hbf","vessid","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")

cvn %in% names(dat)

regtype <- "regY1"
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
# Brazil only, clusters, HBF
#
# R2 - 4 clusters. 1=yft+bet, 2=bet+alb, 3=bet, 4=bet+yft+swo. Use 1,2,3,4
# R3 - 3 or 4 clusters. 1=alb+bet, 2=bet, 3=yft+bet+alb, 4=alb+bft+sfa. Use 1,2,3

projdir <- "~/ICCAT/2019_YFT/"

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

#clkeepCN_Y <- list("yft" = list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepJP_Y <- list("yft" = list(c(1,2,4),c(1,2,3,4),c(1,2,3)))
clkeepKR_Y <- list("yft" = list(c(0),c(1,2,3,4),c(1,2,3)))
clkeepBR_Y <- list("yft" = list(c(0),c(1,2,3,4,5),c(1,2,3,4)))
clkeepTW_Y <- list("yft" = list(c(4),c(2,3),c(0)))
clkeepUS_Y <- list("yft" = list(c(2,3),c(1,3),c(0)))
clk_Y <- list(JP = clkeepJP_Y,KR = clkeepKR_Y,BR = clkeepBR_Y,TW = clkeepTW_Y,US = clkeepUS_Y)

runpars <- list()
runpars[["yft"]] <- list(regtype = "regY1", regtype2 = "Y", clk = clk_Y, doregs = 2:3, addcl = TRUE, dohbf = TRUE, cltype = "hcltrp")

runreg = 1; runsp = "yft"

keepd = TRUE; maxyr = 2018; maxqtrs = 200; minqtrs_byreg = c(5,5,5);
for (runsp in c("yft")) {
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
