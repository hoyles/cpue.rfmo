projdir <- "~/ICCAT/2019_YFT/"
jpdir <- paste0(projdir, "JP/")
datadir1 <- paste0(jpdir, "data/")
jalysis_dir <- paste0(jpdir, "analyses/")
jpfigs <- paste0(jpdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")

dir.create(jpdir)
dir.create(datadir1)
dir.create(jalysis_dir)
dir.create(jpfigs)

setwd(jalysis_dir)
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
#library("dtplyr")
#library("tm")

#install.packages("devtools")
library(devtools)
# This new library replaces the 'support functions.r' file.
#install_github("hoyles/cpue.rfmo")

library("cpue.rfmo")

#source(paste0(Rdir,"support_functions.r"))
#xsd # stop!

# ===================================================================================
# Please keep the data format consistent between years and for the ICCAT + IOTC analyses.
nms <- c("op_yr","op_mon","op_day","lat","latcode","lon","loncode","callsign",
      "hbf","hooks","bft","sbt","alb","bet","yft","swo","mls","bum","blm","trip_st","sas","sha","prefecture","vesselname","logbookid")
wdths <- c(4,2,2,2,1,3,1,6,3,6,3,3,3,3,3,3,3,3,3,8,3,4,3,30,9)
cc <- "iiiiiiiciiiiiiiiiiiiiicci"
posses <- cumsum(c(1,wdths))
posses
cc <- "iiiiiiiciiiiiiiiiiiiiicci"
cbind(nms,wdths,unlist(strsplit(cc,"")))

# the following two lines can be used to check the data format.
# chk <- readLines(paste0(datadir1,"/JPNLL_20170524.dat"))
# chk[10240:10242]

# Check the first 20 rows
a <- data.frame(read_fwf(file = paste0(datadir1,"/JPNLL_AO_20190402.dat"),fwf_widths(wdths),col_types = cc,n_max = 20));gc()
cbind(names(a), nms)
names(a) <- nms
a

# If good, load the whole file
dat1 <- data.frame(read_fwf(file = paste0(datadir1,"/JPNLL_AO_20190402.dat"),fwf_widths(wdths),col_types = cc))
problems(dat1) # Check any problems. Some of those reported are not important.
names(dat1) <- nms
table(dat1$trip_st == 0,dat1$op_yr) # Check for the timing of missign trip_start variables
table(is.na(dat1$trip_st), dat1$op_yr) # Check for the timing of missign trip_start variables
table(dat1$op_yr)

# Prepare the data
rawdat <- dat1
pd1 <- dataprep_JP(rawdat, region = "AO") # No changes between IO and AO functions
pd2 <- setup_AO_regions(pd1, regY = T, regY1 = T, regY2 = T) #
str(pd1)
str(pd2)

# Clean the data
str(rawdat)
clndat <- dataclean_JPIO(rawdat)
prepdat1 <- dataprep_JP(clndat, region = "AO")
prepdat <- setup_AO_regions(prepdat1, regY = T, regY1 = T, regY2 = T) # Later will also need YFT regions, and possibly alternative BET regions
str(prepdat)
save(pd1, pd2, prepdat, file = "prepdat.RData")
#load(file = "prepdat.RData")


#dat <- make_clid(prepdat)
dat <- make_lbidmon(prepdat)
save(dat,file = "JPdat.RData")
#load(file = "JPdat.RData")


# ===================================================================================
# Plot and explore the data
# ===================================================================================
# These plots are for checking the data. Not especially important.
table(pd1$op_yr)
table(pd2$op_yr)
table(dat$op_yr)
table(clndat$op_yr)
table(prepdat1$op_yr)
table(prepdat$op_yr)
table(is.na(dat$clid))
table(dat$regY2, dat$op_yr)

xfun <- function(x) sum(x > 0)
a <- table(dat$vessid,dat$op_yr)
apply(a,2,xfun)

vnm <- (dat$vesselname)
library("tm")
tm_map(vnm, function(x) content_transformer(iconv(enc2utf8, sub = "byte")))

a <- table(dat$vesselname,dat$op_yr)
apply(a,2,xfun)


# Plot grid squares with sets by region, for each regional structure
a <- unique(paste(dat$lat,dat$lon))
a0 <- dat[match(a,paste(dat$lat,dat$lon)),c("lat","lon","regY","regY1","regY2")]
for (fld in c("regY","regY1","regY2")) {
  dev.new(width = 10,height = 10, noRStudioGD = FALSE)
  reg <- with(a0,get(fld))
  plot(a0$lon,a0$lat,type = "n",xlab = "Longitude",ylab = "Latitude",main = fld, xlim = c(-100, 50))
  text(a0$lon,a0$lat,labels = reg,cex = 0.6,col = reg + 1)
  map(add = T, fill = TRUE)
  savePlot(paste0("mapf_",fld),type = "png")
}

# Plot effort proportions by yr & region, indicating proportions of strata with > 5000 hooks, i.e. at least 2 sets.
regYord <- c(1,2,3)
dev.new(height = 12,width = 12); par(mfrow = c(2,2),mar = c(3,2,2,1))
for (r in regYord) {
  llv <- pd2[pd2$regY1==r,]
  yq <- seq(1958.125,2017.875,0.25)
  llv$yrqtr <- factor(llv$yrqtr,levels = yq)
  a <- aggregate(hooks ~ lat5 + lon5 + yrqtr,sum,data = llv)
  b <- a[a$hooks > 5000,]
  b <- tapply(b$hooks,b$yrqtr,sum)
  a <- tapply(a$hooks,a$yrqtr,sum)
  yqa <- a[match(yq,names(a))]
  yqb <- b[match(yq,names(b))]
  yqb[is.na(yqb)] <- 0
  ab <- yqb/yqa
  plot(names(ab),ab,ylim = c(0,1),main = paste("Region",r))
}
savePlot(filename = "Clean strata hooks",type = "png")

# Sets per day and per month
dev.new(width = 15,height = 9)
hist(prepdat$dmy,breaks = "days",freq = T,xlab = "Date",main = "Sets per day")
savePlot(filename = "sets_per_day.png",type = "png")
hist(prepdat$dmy,breaks = "months",freq = T,xlab = "Date",main = "Sets per month")
savePlot(filename = "sets_per_month.png",type = "png")
table(prepdat$dmy)

# Map of hook distribution, all time
a <- aggregate(dat$hooks,list(dat$lat5,dat$lon5),sum,na.rm = T)
dev.new(width = 11,height = 11)
symbols(x = a[,2],y = a[,1],circles = .0002*sqrt(a[,3]),inches = F,bg = 2,fg = 2,xlab = "Longitude",ylab = "Latitude",ylim = c(-50,60), xlim = c(-95, 20))
map(add = T,interior = F,fill = T)
savePlot(filename = "map_hooks.png",type = "png")

# Histogram of hooks per set
dev.new(width = 11,height = 11)
hist(dat$hooks, nclass = 60,xlab = "Hooks per set")   # ask if very large # hooks is okay
savePlot("Hook histogram.png",type = "png")

# Check catch distribtions for outliers. Probably no need to remove.
str(dat)
str(dat1)
table(prepdat$alb)
table(prepdat$bet)
table(prepdat$yft)
table(prepdat$sbt)
table(prepdat$bft)
#table(prepdat$ott)
table(prepdat$swo)
table(prepdat$mls)
table(prepdat$bum)
table(prepdat$blm)
#table(prepdat$otb)
#table(prepdat$skj)
#table(prepdat$sha)   # ask majority of sets (=719211) with 0 sha. Also one set with 663
#table(prepdat$oth)   # ask sets with 3059! But most (=636641) have 0.
table(prepdat$hbf,useNA = "always")  # 6408 with NA! All in 1973-75

table(dat$hbf,dat$op_yr,useNA = "always")  #
table(dat$op_yr,is.na(dat$hbf))  #

# Store some results (aggregated to avoid data concerns) for later reporting.
dat <- dat[is.na(dat$hbf) == FALSE | dat$op_yr < 1976,]
a <- table(dat$op_yr,round(dat$hbf,0),useNA = "always")
write.csv(a,"table hbf by year.csv")

# Set density map by 5 degree cell
table(clndat$loncode) # all good
a <- log(table(dat$lon5,dat$lat5))
dev.new(width = 13,height = 10)
image(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,xlab = "Longitude",ylab = "Latitude")
map("world",add = T, interior = F,fill = F)
savePlot("Setmap_logscale.png",type = "png")

# Set density map by 1 degree cell
a <- with(dat[!is.na(dat$lat) & dat$yrqtr,],log(table(lon,lat)))
dev.new(width = 10,height = 10)
image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,xlab = "Longitude",ylab = "Latitude",ylim = c(-55,70),xlim = c(-100,30))
map("world",add = T, interior = F,fill = T)
savePlot("Setmap_logscale_1deg.png",type = "png")

a <- with(dat[!is.na(dat$lat) & dat$yrqtr,],tapply(regY1,list(lon,lat),mean))
dev.new(width = 10,height = 10)
image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,col = 2:6,xlab = "Longitude",ylab = "Latitude")
map("world",add = T, interior = F,fill = T)
savePlot("regyft.png",type = "png")

# Mean fishing location  by yearqtr
dev.new(width = 15,height = 10);
plot_mean_fishing_location(dat=dat)
savePlot("mean_fishing_location1.png",type = "png")

write.csv(table(round(dat$hbf,0),dat$regY1,useNA = "always"),file = "hbf by region.csv")
write.csv(table(round(dat$hbf,0),floor(dat$yrqtr/5)*5,dat$regY1,useNA = "always"),file = "hbf by region by 5 years.csv")

# Plot hbf. Change spatial selection criteria for AO.
dev.new(20,14);par(mfrow = c(3,3),mar = c(2,2,2,2))
for (y in seq(1975,2015,5)) {
  a <- dat[floor(dat$yrqtr/5)*5==y & dat$lon5 < 125 & dat$lat5 < 55,]
  a <- tapply(a$hbf,list(a$lon5,a$lat5),mean,na.rm = T)
  image(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,main = y,zlim = c(6,24),col = heat.colors(30),xlab = "Lon",ylab = "Lat",ylim = c(-50,50))
  contour(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,add = T,levels = seq(0,26,2))
  map("world",add = T, interior = F,fill = T)
  }
savePlot("mean_HBF.png",type = "png")
qqs <- c(0.125,0.375,0.625,0.875)
for (qq in 1:4) {
  dev.new(20,14);par(mfrow = c(3,3),mar = c(2,2,2,2),oma = c(0,0,1,0))
  for (y in seq(1975,2015,5)) {
    a <- dat[dat$yrqtr %in% (qqs[qq]+y:(y+4)) & dat$lon5 < 125 & dat$lat5 < 55,]
    a <- tapply(a$hbf,list(a$lon5,a$lat5),mean,na.rm = T)
    image(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,main = y,zlim = c(6,24),col = heat.colors(30),xlab = "Lon",ylab = "Lat",xlim = c(-100,20),ylim = c(-55,55))
    contour(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,add = T,levels = seq(0,26,2))
    map("world",add = T, interior = F,fill = T)
    }
  title(paste("Quarter",qq),outer = T,line = 0)
  savePlot(paste0("mean_HBF_q",qq,".png"),type = "png")
  }

#write.csv(table(dat$ncrew,dat$reg),file = "crew by region.csv")
#write.csv(table(dat$ncrew,floor(dat$yrqtr/10)*10),file = "crew by decade.csv")
#write.csv(table(dat$ncrew,dat$fishingcat,useNA = "ifany"),file = "crew by fishingcat.csv")
write.csv(table(dat$lat5,dat$lon5),file = "ops by lat-long.csv")
write.csv(table(dat$lat5,dat$lon5,5*floor(dat$yrqtr/5)),file = "ops by lat-long-5yr.csv")

# data exploration
#install.packages("rpart")
library("rpart")
a <- dat[dat$regY1 %in% c(2),]
dim(a)
a$betcpue <- a$bet/a$hooks
a$albcpue <- a$alb/a$hooks
a$yftcpue <- a$yft/a$hooks
a$sbtcpue <- a$sbt/a$hooks
a$swocpue <- a$swo/a$hooks
a$bftcpue <- a$bft/a$hooks
a$mlscpue <- a$mls/a$hooks
a$blmcpue <- a$blm/a$hooks
a$bumcpue <- a$bum/a$hooks
#simplemod <- rpart(a$betcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$albcpue + a$othcpue + a$mlscpue + a$blmcpue + a$bumcpue)
simplemod <- rpart(a$betcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$albcpue + a$yftcpue + a$mlscpue + a$blmcpue + a$bumcpue)
dev.new(width = 11,height = 7)
plot(simplemod)
text(simplemod)
savePlot("Rpart bet cpue",type = "png")
simplemod <- rpart(a$yftcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$albcpue + a$betcpue + a$mlscpue + a$blmcpue + a$bumcpue)
dev.new(width = 11,height = 7)
plot(simplemod)
text(simplemod)
savePlot("Rpart yft cpue",type = "png")

library("randomForest") # These take a long time and use a lot of memory, but are useful.
simplefor <- randomForest(a$betcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$bftcpue + a$albcpue + a$yftcpue + a$mlscpue + a$blmcpue + a$bumcpue)
print(simplefor)
dev.new(width = 11,height = 7)
plot(importance)
text(varImpPlot,main = NULL)
savePlot("Rforest bet cpue",type = "png")
simplefor <- randomForest(a$yftcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$albcpue + a$betcpue + a$mlscpue + a$blmcpue + a$bumcpue)
print(simplefor)
dev.new(width = 11,height = 7)
plot(importance)
text(varImpPlot,main = NULL)
savePlot("Rforest yft cpue",type = "png")



# ===================================================================================
# Start the analysis proper
# ===================================================================================
#Clustering

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
library("boot")
library("beanplot")
library("cpue.rfmo")

projdir <- "~/ICCAT/2019_YFT/"
jpdir <- paste0(projdir, "JP/")
datadir1 <- paste0(jpdir, "data/")
jalysis_dir <- paste0(jpdir, "analyses/")
Rdir <- paste0(projdir, "Rfiles/")
clusdir <- paste0(jpdir, "clustering/")
clusdir_xsoi <- paste0(jpdir, "clustering_xsoi/")
dir.create(clusdir)
setwd(clusdir)
load(file = paste0(jalysis_dir,"JPdat.RData"))
str(dat)

# Remove redundant datasets, if present. Don't worry if not.
rm(dat2,prepdat,prepdat1,pd1,pd2,clndat,dat5214,rawdat,dataset,llv,dat9415b,dat9415hd,a5,lnk,a2,a0,a)

gc()
jp_splist <- c("alb","bet","yft","swo","mls","bum","blm","bft","sbt","sas","sha")
use_splist <- c("alb","bet","yft","swo","mls","bum","bft","sbt","sas")
allabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tripid","tripidmon","lbid_mon","moon",use_splist,"Total","dmy","lat","lon","lat5","lon5","regY","regY1","regY2")
dat <- data.frame(dat)
str(dat[,allabs])
flag = "JP"

for (r in c(1:3)) {
  dev.new(width=15,height=12); par(mfrow = c(4,3), mar = c(3,2,2,1), oma = c(0,0,2,0))
  a <- dat[dat$regY1 == r,]
  for (sp in jp_splist) plot(sort(unique(a$yrqtr)),tapply(a[,sp], a$yrqtr, mean), main = sp)
  title(paste("Region", r ), outer = TRUE)
  savePlot(filename = paste("freq",flag,"Region", r, sep = "_"), type = "png")
}

nclY1 = c(4,4,4) # Number of bigeye clusters. Will need to be adjusted for each fleet.
nclY2 = c(4,4,4,4,4,4) # Number of bigeye clusters. Will need to be adjusted for each fleet.
cvn <- c("yrqtr","latlong","hooks","hbf","vessid","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")
r = 2


regtype = "regY1"
for (r in 1:length(nclY1)) {
  fnh <- paste(flag,regtype,r,sep = "_")
  dataset <- clust_PCA_run(r = r,ddd = dat,allsp = use_splist,allabs = allabs,regtype = regtype,ncl = nclY1[r],plotPCA = F,clustid = "lbid_mon",allclust = F,flag = flag,fnhead = fnh,covarnames = cvn)
  save(dataset,file = paste0(fnh,".RData"))
}

# Five year cluster plots

clus5y_dir <- paste0(clusdir, "5yr/")
dir.create(clus5y_dir)
dir.create(paste0(clus5y_dir, "maps/"))
setwd(clus5y_dir)

regtype = "regY1"
ll5 <- FALSE
for (r in 1:length(nclY1)) {
  fnh <- paste(flag,regtype,r,sep = "_")
  ncl <- nclY1[r]
  load(file = paste0(clusdir,fnh,".RData"))
  yr_rng <- range(floor(dataset$yrqtr)) + c(0,1)
  latrng <- range(dataset$lat)
  lonrng <- range(dataset$lon)
  for(yq in seq(yr_rng[1], yr_rng[2], 5)) {
    ti = paste(fnh, "hcltrip", yq, sep = "_")
    dat5y <- filter(dataset, yrqtr > yq & yrqtr < (yq + 5))
    boxplots_spCL_comp(dat = dat5y, cl = "hcltrp", ti = ti, outL = F, nsp = length(use_splist), regtype = regtype, r = r, use_splist)
    beanplots_spCL_comp(dat = dat5y, cl = "hcltrp", ti = ti, outL = F, nsp = length(use_splist), regtype = regtype, r = r, use_splist)
    boxplots_CL(dat = dat5y, cl = "hcltrp", ti = ti, outL = F, dohbf = TRUE, lat5 = ll5, regtype = regtype, r = r)
    boxplots_CL_bean(dat = dat5y, cl = "hcltrp", ti = ti, outL = F, dohbf = TRUE, lat5 = ll5, regtype = regtype, r = r)
    map_clusters(ddd = dat5y, cl = "hcltrp", ti = ti, lat5 = ll5, regtype = regtype, r = r, ncl = ncl, xl = lonrng, yl = latrng, savedir = "./maps/")
    graphics.off()
  }
}

regtype = "regY2"
for (r in 1:length(nclY2)) {
  fnh <- paste(flag,regtype,r,sep = "_")
  dataset <- clust_PCA_run(r = r,ddd = dat,allsp = use_splist,allabs = allabs,regtype = regtype,ncl = nclY2[r],plotPCA = F,clustid = "lbid_mon",allclust = F,flag = flag,fnhead = fnh,covarnames = cvn)
  save(dataset,file = paste0(fnh,".RData"))
}


# ========= as an exercise, run clustering without the species of interest ===========
clusdir_xsoi <- paste0(jpdir, "clustering_xsoi/")
dir.create(clusdir_xsoi)
setwd(clusdir_xsoi)
setwd(clusdir)
load(file = paste0(jalysis_dir,"JPdat.RData"))

gc()
jp_splist <- c("alb","bet","yft","swo","mls","bum","blm","bft","sbt","sas","sha")
use_splist <- c("alb","yft","swo","mls","bum","bft","sbt","sas")
allabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tripid","tripidmon","lbid_mon","moon",use_splist,"Total","dmy","lat","lon","lat5","lon5","regY","regY1","yft")
dat <- data.frame(dat)
str(dat[,allabs])
flag = "JP"

nclY1 = c(4,4,4) # Number of bigeye clusters. Will need to be adjusted for each fleet.
cvn <- c("yrqtr","latlong","hooks","hbf","vessid","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon","regY","regY1","yft")
r = 3


regtype = "regY1"
for (r in 1:length(nclY1)) {
  fnh <- paste(flag,regtype,r,sep = "_")
  dataset <- clust_PCA_run(r = r,ddd = dat,allsp = use_splist,allabs = allabs,regtype = regtype,ncl = nclY1[r],plotPCA = F,clustid = "lbid_mon",allclust = F,flag = flag,fnhead = fnh,covarnames = cvn)
  save(dataset,file = paste0(fnh,".RData"))
}

getwd()


##################################################
# Japan only, no clusters, HBF
#
# R1 - 4 clusters, 1=bet, 2=alb+bet, 3=bft, 4=yft+bet+mls+bft+alb. Use 1,2,4.
# R2 - 4 or 2 clusters. 1=bet+yft+swo, 2=bet, 3=yft+bet, 4=alb+bet+yft. Use 1,2,3,4.
# R3 - 4 or 3 clusters. 1=bet, 2=alb+bet, 3=bet+yft, 4=alb+mls+bum+sas. Use 1,2,3.

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
library("cpue.rfmo")

projdir <- "~/ICCAT/2019_YFT/"
jpdir <- paste0(projdir, "JP/")
datadir1 <- paste0(jpdir, "data/")
jalysis_dir <- paste0(jpdir, "analyses/")
Rdir <- paste0(projdir, "Rfiles/")

resdir <- paste0(jalysis_dir,"std_cl_JPonly_nohbf/")
dir.create(resdir)
setwd(resdir)

#clkeepCN_Y <- list("yft" = list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepJP_Y <- list("yft" = list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepBR_Y <- list("yft" = list(c(1,2,4),c(1,2,3,4),c(1,2,3)))
clkeepKR_Y <- list("yft" = list(c(0),c(1,2,3,4),c(1,2,3)))
clkeepTW_Y <- list("yft" = list(c(4),c(2,3),c(0)))
clkeepUS_Y <- list("yft" = list(c(2,3),c(1,3),c(0)))
clk_Y <- list(JP = clkeepJP_Y,KR = clkeepKR_Y,TW = clkeepTW_Y,US = clkeepUS_Y)

flag <- "JP"
runpars <- list()
runpars[["yft"]] <- list(regtype = "regY1", regtype2 = "Y", clk = clk_Y, doregs = 1:3, addcl = TRUE, dohbf = FALSE, cltype = "hcltrp")
use_splist <- c("alb","bet","yft")
stdlabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","moon",use_splist,"Total","lat","lon","lat5","lon5","hcltrp","reg","flag")

runreg = 1; runsp = "yft"
keepd = TRUE; maxyr = 2018; maxqtrs = 200; minqtrs_byreg = c(5,5,5);
for (runsp in c("yft")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  jdat <- data.frame()
  for (flag in c("JP")) {
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
      glmdat <- select_data_JointIO(jdat,runreg = runreg,clk = clk,minqtrs = minqtrs,runsp = runsp,mt = "deltabin",vars = vars, maxqtrs = maxqtrs,
                                    minvess = 50, minll = 50, minyrqtr = 50, addcl = addcl, cltype = cltype, addpca = NA, samp = NA, strsmp = NA)
      if (nrow(glmdat) > 60000) glmdat <- samp_strat_data(glmdat,60)
      glmdat5279 <- select_data_JointIO(jdat,runreg = runreg,clk = clk,minqtrs = minqtrs,runsp = runsp,mt = "deltabin",vars = vars,maxqtrs = maxqtrs,
                                        minvess = 50,minll = 50,minyrqtr = 50,addcl = addcl,cltype = cltype,addpca = NA,samp = NA,strsmp = NA,
                                        yrlims = c(1952,1980))
      if (nrow(glmdat5279) > 60000) glmdat5279 <- samp_strat_data(glmdat5279,60)
      a <- jdat[jdat$vessid != "JP1",]
      glmdat79nd <- select_data_JointIO(a,runreg = runreg,clk = clk,minqtrs = minqtrs,runsp = runsp,mt = "deltabin",vars = vars,maxqtrs = maxqtrs,
                                        minvess = 50,minll = 50,minyrqtr = 50,addcl = addcl,cltype = cltype,addpca = NA,samp = NA,strsmp = NA,
                                        yrlims = c(1979,maxyr))
      if (nrow(glmdat79nd) > 60000) glmdat79nd <- samp_strat_data(glmdat79nd,60)
      wtt.all   <- mk_wts(glmdat,wttype = "area")
      wtt.5279   <- mk_wts(glmdat5279,wttype = "area")
      wtt.79nd   <- mk_wts(glmdat79nd,wttype = "area")
      fmla.oplogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = T,nhbf = 3)
      fmla.oplogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = F,nhbf = 3)
      fmla.boatlogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = T,nhbf = 3)
      fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = F,nhbf = 3)
      mn <- with(glmdat,0.1* mean(get(runsp)/hooks))

      modlab = "lognC_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
      if (lu(glmdat$clust) > 1)
      { model <- glm(fmla.oplogn,data = glmdat,weights = wtt.all,family = "gaussian");gc() } else
      { model <- glm(fmla.oplogn_ncl,data = glmdat,weights = wtt.all,family = "gaussian");gc() }
      summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

      modlab = "lognC_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
      if (lu(glmdat$clust) > 1)
      { model <- glm(fmla.boatlogn,data = glmdat,weights = wtt.all,family = "gaussian");gc() } else
      { model <- glm(fmla.boatlogn_ncl,data = glmdat,weights = wtt.all,family = "gaussian");gc() }
      summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

      modlab = "lognC_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
      mn <- with(glmdat5279,0.1* mean(get(runsp)/hooks))
      if (lu(glmdat5279$clust) > 1)
      { model <- glm(fmla.oplogn,data = glmdat5279,weights = wtt.5279,family = "gaussian");gc() } else
      { model <- glm(fmla.oplogn_ncl,data = glmdat5279,weights = wtt.5279,family = "gaussian");gc() }
      summarize_and_store(mod = model,dat = glmdat5279,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

      modlab = "lognC_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
      mn <- with(glmdat79nd,0.1* mean(get(runsp)/hooks))
      if (lu(glmdat79nd$clust) > 1)
      { model <- glm(fmla.boatlogn,    data = glmdat79nd,weights = wtt.79nd,family = "gaussian");gc() } else
      { model <- glm(fmla.boatlogn_ncl,data = glmdat79nd,weights = wtt.79nd,family = "gaussian");gc() }
      summarize_and_store(mod = model,dat = glmdat79nd,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

      # delta lognormal
      modlab = "dellog_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg);
      do_deltalog(dat = glmdat,dohbf = dohbf,addboat = F,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

      modlab = "dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
      do_deltalog(dat = glmdat,dohbf = dohbf,addboat = T,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

      modlab = "dellog_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
      do_deltalog(dat = glmdat5279,dohbf = dohbf,addboat = F,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

      modlab = "dellog_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
      do_deltalog(dat = glmdat79nd,dohbf = dohbf,addboat = T,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

      graphics.off()
    }
}

#####-----------------------------------

resdir <- paste0(jalysis_dir,"std_cl_JPonly_hbf/")
dir.create(resdir)
setwd(resdir)

flag <- "JP"
runpars <- list()
runpars[["yft"]] <- list(regtype = "regY1", regtype2 = "Y", clk = clk_Y, doregs = 1:3, addcl = TRUE, dohbf = TRUE, cltype = "hcltrp")
use_splist <- c("alb","bet","yft")
stdlabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","moon",use_splist,"Total","lat","lon","lat5","lon5","hcltrp","reg","flag")

runreg = 1; runsp = "yft"
keepd = TRUE; maxyr = 2018; maxqtrs = 200; minqtrs_byreg = c(5,5,5);
for (runsp in c("yft")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  jdat <- data.frame()
  for (flag in c("JP")) {
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
    glmdat <- select_data_JointIO(jdat,runreg = runreg,clk = clk,minqtrs = minqtrs,runsp = runsp,mt = "deltabin",vars = vars, maxqtrs = maxqtrs, minvess = 50, minll = 50, minyrqtr = 50, addcl = addcl, cltype = cltype, addpca = NA, samp = NA, strsmp = NA)
    if (nrow(glmdat) > 60000) glmdat <- samp_strat_data(glmdat,60)
    glmdat5279 <- select_data_JointIO(jdat,runreg = runreg,clk = clk,minqtrs = minqtrs,runsp = runsp,mt = "deltabin",vars = vars,maxqtrs = maxqtrs, minvess = 50,minll = 50,minyrqtr = 50,addcl = addcl,cltype = cltype,addpca = NA,samp = NA,strsmp = NA,yrlims = c(1952,1980))
    if (nrow(glmdat5279) > 60000) glmdat5279 <- samp_strat_data(glmdat5279,60)
    a <- jdat[jdat$vessid != "JP1",]
    glmdat79nd <- select_data_JointIO(a,runreg = runreg,clk = clk,minqtrs = minqtrs,runsp = runsp,mt = "deltabin",vars = vars,maxqtrs = maxqtrs, minvess = 50,minll = 50,minyrqtr = 50,addcl = addcl,cltype = cltype,addpca = NA,samp = NA,strsmp = NA,yrlims = c(1979,maxyr))
    if (nrow(glmdat79nd) > 60000) glmdat79nd <- samp_strat_data(glmdat79nd,60)
    wtt.all   <- mk_wts(glmdat,wttype = "area")
    wtt.5279   <- mk_wts(glmdat5279,wttype = "area")
    wtt.79nd   <- mk_wts(glmdat79nd,wttype = "area")
    fmla.oplogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = T,nhbf = 3)
    fmla.oplogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = F,nhbf = 3)
    fmla.boatlogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = T,nhbf = 3)
    fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = F,nhbf = 3)
    mn <- with(glmdat,0.1* mean(get(runsp)/hooks))

    modlab = "lognC_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if (lu(glmdat$clust) > 1)
    { model <- glm(fmla.oplogn,data = glmdat,weights = wtt.all,family = "gaussian");gc() } else
    { model <- glm(fmla.oplogn_ncl,data = glmdat,weights = wtt.all,family = "gaussian");gc() }
    summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

    modlab = "lognC_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if (lu(glmdat$clust) > 1)
    { model <- glm(fmla.boatlogn,data = glmdat,weights = wtt.all,family = "gaussian");gc() } else
    { model <- glm(fmla.boatlogn_ncl,data = glmdat,weights = wtt.all,family = "gaussian");gc() }
    summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

    modlab = "lognC_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    mn <- with(glmdat5279,0.1* mean(get(runsp)/hooks))
    if (lu(glmdat5279$clust) > 1)
    { model <- glm(fmla.oplogn,data = glmdat5279,weights = wtt.5279,family = "gaussian");gc() } else
    { model <- glm(fmla.oplogn_ncl,data = glmdat5279,weights = wtt.5279,family = "gaussian");gc() }
    summarize_and_store(mod = model,dat = glmdat5279,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

    modlab = "lognC_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    mn <- with(glmdat79nd,0.1* mean(get(runsp)/hooks))
    if (lu(glmdat79nd$clust) > 1)
    { model <- glm(fmla.boatlogn,    data = glmdat79nd,weights = wtt.79nd,family = "gaussian");gc() } else
    { model <- glm(fmla.boatlogn_ncl,data = glmdat79nd,weights = wtt.79nd,family = "gaussian");gc() }
    summarize_and_store(mod = model,dat = glmdat79nd,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

#    delta lognormal
    modlab = "dellog_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg);
    do_deltalog(dat = glmdat,dohbf = dohbf,addboat = F,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

    modlab = "dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat = glmdat,dohbf = dohbf,addboat = T,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

    modlab = "dellog_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat = glmdat5279,dohbf = dohbf,addboat = F,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

    modlab = "dellog_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat = glmdat79nd,dohbf = dohbf,addboat = T,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

    graphics.off()
  }
}


##############################
# clustring_xsoi

resdir <- paste0(jalysis_dir,"std_cl_JPonly_xsoi/")
dir.create(resdir)
setwd(resdir)

flag <- "JP"
runpars <- list()
runpars[["yft"]] <- list(regtype = "regY1", regtype2 = "Y", clk = clk_Y, doregs = 1:3, addcl = TRUE, dohbf = TRUE, cltype = "hcltrp")
use_splist <- c("alb","bet","yft")
stdlabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","moon",use_splist,"Total","lat","lon","lat5","lon5","hcltrp","reg","flag")

runreg = 1; runsp = "yft"
keepd = TRUE; maxyr = 2018; maxqtrs = 200; minqtrs_byreg = c(5,5,5);
for (runsp in c("yft")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  jdat <- data.frame()
  for (flag in c("JP")) {
    for (r in runpars[[runsp]]$doregs) {
      load(paste0(projdir,flag,"/clustering_xsoi/",paste(flag,regtype,r,sep = "_"),".RData"))
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
    glmdat <- select_data_JointIO(jdat,runreg = runreg,clk = clk,minqtrs = minqtrs,runsp = runsp,mt = "deltabin",vars = vars, maxqtrs = maxqtrs, minvess = 50, minll = 50, minyrqtr = 50, addcl = addcl, cltype = cltype, addpca = NA, samp = NA, strsmp = NA)
    if (nrow(glmdat) > 60000) glmdat <- samp_strat_data(glmdat,60)
    glmdat5279 <- select_data_JointIO(jdat,runreg = runreg,clk = clk,minqtrs = minqtrs,runsp = runsp,mt = "deltabin",vars = vars,maxqtrs = maxqtrs, minvess = 50,minll = 50,minyrqtr = 50,addcl = addcl,cltype = cltype,addpca = NA,samp = NA,strsmp = NA,yrlims = c(1952,1980))
    if (nrow(glmdat5279) > 60000) glmdat5279 <- samp_strat_data(glmdat5279,60)
    a <- jdat[jdat$vessid != "JP1",]
    glmdat79nd <- select_data_JointIO(a,runreg = runreg,clk = clk,minqtrs = minqtrs,runsp = runsp,mt = "deltabin",vars = vars,maxqtrs = maxqtrs, minvess = 50,minll = 50,minyrqtr = 50,addcl = addcl,cltype = cltype,addpca = NA,samp = NA,strsmp = NA,yrlims = c(1979,maxyr))
    if (nrow(glmdat79nd) > 60000) glmdat79nd <- samp_strat_data(glmdat79nd,60)
    wtt.all   <- mk_wts(glmdat,wttype = "area")
    wtt.5279   <- mk_wts(glmdat5279,wttype = "area")
    wtt.79nd   <- mk_wts(glmdat79nd,wttype = "area")
    fmla.oplogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = T,nhbf = 3)
    fmla.oplogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = F,nhbf = 3)
    fmla.boatlogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = T,nhbf = 3)
    fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = F,nhbf = 3)
    mn <- with(glmdat,0.1* mean(get(runsp)/hooks))

    modlab = "lognC_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if (lu(glmdat$clust) > 1)
    { model <- glm(fmla.oplogn,data = glmdat,weights = wtt.all,family = "gaussian");gc() } else
    { model <- glm(fmla.oplogn_ncl,data = glmdat,weights = wtt.all,family = "gaussian");gc() }
    summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

    modlab = "lognC_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if (lu(glmdat$clust) > 1)
    { model <- glm(fmla.boatlogn,data = glmdat,weights = wtt.all,family = "gaussian");gc() } else
    { model <- glm(fmla.boatlogn_ncl,data = glmdat,weights = wtt.all,family = "gaussian");gc() }
    summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

    modlab = "lognC_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    mn <- with(glmdat5279,0.1* mean(get(runsp)/hooks))
    if (lu(glmdat5279$clust) > 1)
    { model <- glm(fmla.oplogn,data = glmdat5279,weights = wtt.5279,family = "gaussian");gc() } else
    { model <- glm(fmla.oplogn_ncl,data = glmdat5279,weights = wtt.5279,family = "gaussian");gc() }
    summarize_and_store(mod = model,dat = glmdat5279,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

    modlab = "lognC_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    mn <- with(glmdat79nd,0.1* mean(get(runsp)/hooks))
    if (lu(glmdat79nd$clust) > 1)
    { model <- glm(fmla.boatlogn,    data = glmdat79nd,weights = wtt.79nd,family = "gaussian");gc() } else
    { model <- glm(fmla.boatlogn_ncl,data = glmdat79nd,weights = wtt.79nd,family = "gaussian");gc() }
    summarize_and_store(mod = model,dat = glmdat79nd,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

    #    delta lognormal
    modlab = "dellog_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg);
    do_deltalog(dat = glmdat,dohbf = dohbf,addboat = F,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

    modlab = "dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat = glmdat,dohbf = dohbf,addboat = T,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

    modlab = "dellog_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat = glmdat5279,dohbf = dohbf,addboat = F,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

    modlab = "dellog_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat = glmdat79nd,dohbf = dohbf,addboat = T,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

    graphics.off()
  }
}

# Check early data
a <- dat[dat$yrqtr >1959 & dat$regY1==2,]
by <- table(a$yft==0, a$op_yr)
bb <- table(a$bet==0, a$op_yr)
ba <- table(a$alb==0, a$op_yr)
yrs <- as.numeric(colnames(bb))
windows()
plot(1:10, 1:10, xlim = range(yrs), ylim = c(0, 1), xlab = "Year", ylab = "Proportion zero", main = "Proportion zero by set")
lim <- par("usr")
rect(lim[1], lim[3], lim[2], lim[4], col = "grey")
lines(as.numeric(colnames(bb)), bb[2,]/ apply(bb,2,sum), col = "red", lwd=2)
lines(as.numeric(colnames(by)), by[2,]/ apply(by,2,sum), col = "yellow", lwd=2)
legend("topright",legend = c("Bigeye", "Yellowfin"), col = c("red", "yellow"), lty = 1, lwd=2)
savePlot("propzero_by_set.png", type = "png")

va <- aggregate(cbind(yft, bet, alb) ~ op_yr + logbookid, data=a, FUN=sum)
vn <- aggregate(cbind(yft, bet) ~ op_yr + logbookid, data=a, FUN=length)
hist(vn$bet)
by <- with(va[vn$bet > 20,], table(yft==0, op_yr))
ba <- with(va[vn$bet > 20,], table(alb==0, op_yr))
bb <- with(va[vn$bet > 20,], table(bet==0, op_yr))
windows()
yrs <- as.numeric(colnames(bb))
plot(1:10, 1:10, xlim = range(yrs), ylim = c(0, .5), xlab = "Year", ylab = "Proportion zero", main = "Zero catches by vessel-year (vessels with 20+ sets)")
lim <- par("usr")
rect(lim[1], lim[3], lim[2], lim[4], col = "grey")
lines(as.numeric(colnames(bb)), bb[2,]/ apply(bb,2,sum), col = "red", lwd=2)
lines(as.numeric(colnames(by)), by[2,]/ apply(by,2,sum), col = "yellow", lwd=2)
legend("topright",legend = c("Bigeye", "Yellowfin"), col = c("red", "yellow"), lty = 1, lwd=2)
savePlot("propzero_by_year.png", type = "png")
