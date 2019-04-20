projdir <- "~/ICCAT/2019_YFT/"
# install.packages("tm")
# install.packages("devtools")
# install.packages("readr")
# install.packages("dtplyr")

#projdir <- "C:/blackdrive/ICCAT/2018/BET/2018_CPUE/"
USdir <- paste0(projdir, "US/")
datadir1 <- paste0(USdir, "data/")
USalysis_dir <- paste0(USdir, "analyses/")
USfigs <- paste0(USdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
setwd(USalysis_dir)

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
#library("tm")
library("devtools")
# This new library replaces the 'support functions.r' file.
#This doesn't work , have to manually install from zip file
#install_github("hoyles/cpue.rfmo")
library("cpue.rfmo")

#source(paste0(Rdir,"support_functions.r"))
#xsd # stop!

# ===================================================================================
# Please keep the data format consistent between years and for the ICCAT + IOTC analyses.
#nms <- c("op_yr","op_mon","op_day","lat","latcode","lon","loncode","callsign",
#      "hbf","hooks","sbt","alb","bet","yft","swo","mls","bum","blm","trip_st","sas","shk","prefecture","vesselname","logbookid")
#wdths <- c(4,2,2,2,1,3,1,6,3,6,3,3,3,3,3,3,3,3,8,3,4,3,30,9)
#cc <- "iiiiiiiciiiiiiiiiiiiicci"
#posses <- cumsum(c(1,wdths))
#cc <- "iiiiiiiciiiiiiiiiiiiicci"
#cbind(nms,wdths,unlist(strsplit(cc,"")))

# the following two lines can be used to check the data format.
# chk <- readLines(paste0(datadir1,"/JPNLL_20170524.dat"))
# chk[10240:10242]

# Check the first 20 rows
#a <- read_fwf(file=paste0(datadir1,"/JPNLL_20170524.dat"),fwf_widths(wdths),col_types=cc,n_max=20);gc()
#names(a) <- nms


# If good, load the whole file
#dat5216 <- read_fwf(file = paste0(datadir1,"/JPNLL_20170524.dat"),fwf_widths(wdths),col_types = cc)
dat1 <- read.csv(paste0(datadir1,"USLLV1.2simonV3.2019.csv"))
#dat1 <- read.csv("C:/blackdrive/NEWPLL/BETCPUE/USLLV1.2simonV2.csv")
head(dat1)
a <- names(dat1)
nms <- c("bsh", "sma", "por", "lon", "lat", "moon", "yrqtr2", "latlong2", "op_yr", "op_mon", "op_day", "latcode", "loncode", "callsign", "hbf", "bft", "alb", "bet", "yft", "swo", "mls", "bum","blm","trip_st", "sas", "shk", "prefecture", "vesselname", "logbookid", "qtr","SST","hooks","Total")
cbind(a, nms)
a == nms
names(dat1) <- nms

splist <- c("bft", "alb", "bet", "yft", "swo", "mls", "bum", "bsh", "sma", "por")
nms2 <- c("trip_st", "op_yr", "op_mon", "op_day", "lat", "lon", "callsign", "hbf", splist, "qtr","SST","hooks","moon")
rawdat <- dat1[,nms2]
head(rawdat)

table(dat1$op_yr)

# Prepare the data
#pd1 <- dataprep_JPIO(rawdat) # No changes between IO and AO functions
rawdat$lat5 = rawdat$lat
rawdat$lon5 = rawdat$lon

pd2 <- setup_AO_regions(rawdat, regB = TRUE, regB1 = TRUE, regY = TRUE, regY1 = TRUE, regY2 = TRUE) # Later will also need YFT regions,
#and possibly alternative BET regions
head(pd2)
table(pd2$regB)
str(pd2)


# Clean the data
clndat <- dataclean_USAO(pd2, splist = splist)
head(clndat)
prepdat <- clndat

dat = dataprep_US(prepdat, splist = splist)
head(dat)
save(dat,file = "USdat.RData")

# ===================================================================================
# Plot and explore the data
# ===================================================================================
# These plots are for checking the data. Not especially important.
# table(dat$op_yr)
# table(is.na(dat$clid))
# table(dat$clid==0)
# a <- table(dat$clid)
# hist(a,nclass = max(a))
# windows(height = 10,width = 10);par(mfrow = c(3,2))
# max(a[a<100000])
# a1 <- hist(a,nclass = max(a),main = "cluster_id frequency",xlab = "Sets per cluster_id")
# plot(a1$breaks,c(a1$counts,0) * a1$breaks,xlim = c(0,max(a)),ylim = c(0,2e5),main = "set frequency by cluster_id",
#   xlab = "Sets per cluster_id",ylab = "Number of sets")
#        head(dat)

xfun <- function(x) sum(x > 0)
dat$vessid = dat$callsign
a <- table(dat$vessid,dat$op_yr)
apply(a,2,xfun)
apply(a,2,lu)

# Plot grid squares with sets by region, for each regional structure
a <- unique(paste(dat$lat,dat$lon))

a0 <- dat[match(a,paste(dat$lat,dat$lon)),c("lat","lon","regB", "regB1", "regY", "regY1", "regY2")]
for (fld in c("regB", "regB1", "regY", "regY1", "regY2")) {
  windows(width = 15,height = 10)
  reg <- with(a0,get(fld))
  plot(a0$lon,a0$lat,type = "n",xlab = "Longitude",ylab = "Latitude",main = fld)
  text(a0$lon,a0$lat,labels = reg, cex = 0.8,col = reg + 1)
  map(add = T)
  savePlot(paste0("map_",fld),type = "png")
}


# Map of hook distribution, all time
a <- aggregate(dat$hooks,list(dat$lat5,dat$lon5),sum,na.rm = T)
windows(width = 11,height = 9)
symbols(x = a[,2],y = a[,1],circles = .0002 * sqrt(a[,3]),inches = F,bg = 2,fg = 2,xlab = "Longitude",ylab = "Latitude",
ylim = c(-50,60),xlim = c(-105,10))
map(add = T,interior = F,fill = T)
savePlot(filename = "map_hooks.png",type = "png")

# Histogram of hooks per set
table(dat$hooks[dat$hooks > 1000])
hist(dat$hooks[dat$hooks < 8000], nclass = 100,xlab = "Hooks per set")   # ask if very large # hooks is okay
savePlot("Hook histogram.png",type = "png")

# Check catch distribtions for outliers. Probably no need to remove.

table(prepdat$alb)
table(prepdat$bet)
table(prepdat$yft)
table(prepdat$bft)
table(prepdat$swo)
table(prepdat$mls)
table(prepdat$bum)
table(prepdat$bsh)
table(prepdat$sma)
table(prepdat$por)

table(prepdat$hbf,useNA = "always")  # 6408 with NA! All in 1973-75

table(dat$hbf,dat$op_yr,useNA = "always")  #
table(dat$op_yr,is.na(dat$hbf))  #

a <- table(dat$op_yr,round(dat$hbf,0),useNA = "always")
write.csv(a,"table hbf by year.csv")

# Set density map by 5 degree cell
a <- log(table(dat$lon5,dat$lat5))
windows(width = 13,height = 10)
image(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,xlab = "Longitude",ylab = "Latitude",
      col  =  rev(heat.colors(12) ) )
contour(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,xlab = "Longitude",ylab = "Latitude", add  =  TRUE)
map("world",add = T, interior = T,fill = T)
savePlot("Setmap_logscale.png",type = "png")

# Set density map by 1 degree cell
# a <- with(dat[!is.na(dat$lat) & dat$yrqtr,],log(table(dat$lon,dat$lat)))
# windows(width = 15,height = 10)
# image(as.numeric(dimnames(a)[[1]])+.5,as.numeric(dimnames(a)[[2]])+.5,a,
# xlab = "Longitude",ylab = "Latitude",ylim = c(-50,50),xlim = c(-105,10),col  =  rev(heat.colors(12) ))
# map("world",add = T, interior = F,fill = T)
# savePlot("Setmap_logscale_1deg.png",type = "png")

# Mean fishing location  by yearqtr
windows(width = 15,height = 10);par(mfrow = c(1,2))
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
windows(width = 15,height = 10);par(mfrow = c(1,2))
plot(tapply(dat$op_yr,dat$op_yr,mean),tapply(dat$lat5,dat$op_yr,mean),xlab = "yr",ylab = "Mean latitude")
plot(tapply(dat$lon5,dat$op_yr,mean),tapply(dat$op_yr,dat$op_yr,mean),ylab = "yr",xlab = "Mean longitude")
savePlot("mean_fishing_location2.png",type = "png")

#write.csv(table(round(dat$hbf,0),dat$regY,useNA = "always"),file = "hbf by region.csv")
#write.csv(table(round(dat$hbf,0),floor(dat$yrqtr/5) * 5,dat$regY,useNA = "always"),file = "hbf by region by 5 years.csv")

# Plot hbf. Change spatial selection criteria for AO.

windows(20,14);par(mfrow = c(3,3),mar = c(2,2,2,2))
for (y in seq(1985,2015,5)) {
 # a <- dat[floor(dat$yrqtr/5) * 5 == y & dat$lon5 < 0 & dat$lat5 > 0,]
  a <- dat[ dat$op_yr %in% y:(y+4),]
  a <- tapply(a$hbf,list(a$lon5,a$lat5),mean,na.rm = T)

  image( as.numeric(dimnames(a)[[1]]), as.numeric(dimnames(a)[[2]]) , a ,
  main = y,zlim = c(0,7),col = rev(heat.colors(30)) , xlab = "Lon",ylab = "Lat", ylim = c(0,50),xlim = c(-105,0))

  contour(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,add = T,
  levels = seq(0,7,2))
  map("world",add = T, interior = F,fill = T) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
  }
savePlot("mean_HBF.png",type = "png")

qqs <- c(0.125,0.375,0.625,0.875)
for (qq in 1:4) {
  windows(20,14);par(mfrow = c(3,3),mar = c(2,2,2,2),oma = c(0,0,1,0))
  for (y in seq(1985,2015,5)) {

    a <- dat[dat$yrqtr %in% (qqs[qq]+y:(y+4)) & dat$lon5 < 0 & dat$lat5 > 0,]
    a <- tapply(a$hbf,list(a$lon5,a$lat5),mean,na.rm = T)
    image(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,main = y,zlim = c(0,10),
     col = rev(heat.colors(30)),xlab = "Lon",ylab = "Lat",ylim = c(0,50),xlim = c(-105,0))
    contour(as.numeric(dimnames(a)[[1]]),as.numeric(dimnames(a)[[2]]),a,add = T,levels = seq(0,10,1),col = "blue")
    map("world",add = T, interior = F,fill = T) # delete data outside IO? But maybe it's just the EW code that's wrong - change to 1?
    }
  title(paste("Quarter",qq),outer = T,line = 0)
 savePlot(paste0("mean_HBF_q",qq,".png"),type = "png")
  }

#write.csv(table(dat$ncrew,dat$reg),file = "crew by region.csv")
#write.csv(table(dat$ncrew,floor(dat$yrqtr/10) * 10),file = "crew by decade.csv")
#write.csv(table(dat$ncrew,dat$fishingcat,useNA = "ifany"),file = "crew by fishingcat.csv")
write.csv(table(dat$lat5,dat$lon5),file = "ops by lat-long.csv")
write.csv(table(dat$lat5,dat$lon5,5 * floor(dat$yrqtr/5)),file = "ops by lat-long-5yr.csv")

# data exploration
#install.packages("rpart")
library("rpart")
a <- dat
head(dat)
dim(a)
a$betcpue <- a$bet/a$hooks
a$albcpue <- a$alb/a$hooks
a$yftcpue <- a$yft/a$hooks
a$bftcpue <- a$bft/a$hooks
a$swocpue <- a$swo/a$hooks
a$smacpue <- a$sma/a$hooks
a$porcpue <- a$por/a$hooks
a$mlscpue <- a$mls/a$hooks
a$bshcpue <- a$bsh/a$hooks
a$bumcpue <- a$bum/a$hooks

simplemod <- rpart(a$betcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$albcpue + a$yftcpue + a$mlscpue + a$bshcpue + a$bumcpue + a$smacpue)
windows(width = 11,height = 7)
plot(simplemod)
text(simplemod)
savePlot("Rpart bet cpue",type = "png")

# ===================================================================================
# Start the analysis proper
# ===================================================================================
#################################################John ended here#################################################



a <- dat[dat$regY1 == 2,]  # smaller component, with 33000 rows. Still takes 5 minutes or more.
a$betcpue <- a$bet/a$hooks
a$albcpue <- a$alb/a$hooks
a$yftcpue <- a$yft/a$hooks
a$bftcpue <- a$bft/a$hooks
a$swocpue <- a$swo/a$hooks
a$smacpue <- a$sma/a$hooks
a$porcpue <- a$por/a$hooks
a$mlscpue <- a$mls/a$hooks
a$bshcpue <- a$bsh/a$hooks
a$bumcpue <- a$bum/a$hooks
dim(a)
head(a)
#install.packages("randomForest")
library("randomForest") # These take a long time and use a lot of memory, but are useful.
simplefor <- randomForest(betcpue ~ lon + lat + yrqtr + swocpue + albcpue + yftcpue + mlscpue + bumcpue + bshcpue + smacpue + porcpue, data = a)
print(simplefor)
windows(width = 11,height = 7)
plot(simplefor)
varImpPlot(simplefor)
savePlot("Rforest bet cpue",type = "png")
partialPlot(simplefor, pred.data = a, x.var = "swocpue")
savePlot("Rforest bet cpue partial",type = "png")



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
library("lubridate")

library("cpue.rfmo")

projdir <- "~/ICCAT/2019_YFT/"
USdir <- paste0(projdir, "US/")
datadir1 <- paste0(USdir, "data/")
USalysis_dir <- paste0(USdir, "analyses/")
USfigs <- paste0(USdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
clusdir <- paste0(USdir, "clustering/")

dir.create(clusdir)
setwd(clusdir)

load(file = paste0(USalysis_dir,"USdat.RData") )

str(dat)

# Remove redundant datasets, if present. Don't worry if not.
rm(dat2,prepdat,prepdat1,pd1,pd2,clndat,dat5214,rawdat,dataset,llv,dat9415b,dat9415hd,a5,lnk,a2,a0,a)
a <- dmy(dat$trip_st)
dat$tripid <- paste(dat$vessid,year(a),week(a), sep = "_")
dat$tripidmon <- paste(dat$vessid,year(a),month(a), sep = "_")

gc()
us_splist <- c("alb","bft","bet","yft","swo","mls","bum","bsh","sma","por")
use_splist <- c("alb","bft","bet","yft","swo","mls","bum","bsh","sma")
allabs <- c("vessid","yrqtr","latlong","op_yr","hbf","hooks","tripidmon",use_splist,"Total","lat","lon","lat5","lon5","regY","regY1", "regY2")
dat <- data.frame(dat)

nclY1 = c(4,4,0) # Number of clusters.
nclY2 = c(4,4,0,0,0,4) #

flag = "US"
cvn <- c("yrqtr","latlong","hooks","hbf","vessid","Total","lat","lon","lat5","lon5","op_yr","tripidmon")
r = 1
names(dat)

for (r in c(1:2)) {
  windows(15,12); par(mfrow = c(5,3), mar = c(3,2,2,1), oma = c(0,0,2,0))
  a <- dat[dat$regY1 == r,]
  for (sp in us_splist) plot(sort(unique(a$yrqtr)),tapply(a[,sp], a$yrqtr, mean), main = sp)
  title(paste("Region", r ), outer = TRUE)
  savePlot(filename = paste("freq",flag,"Region", r, sep = "_"), type = "png")
}

regtype = "regY1"
for (r in 2:1) {
  fnh <- paste(flag,regtype,r,sep = "_")
  dataset <- clust_PCA_run(r = r,ddd = dat,allsp = use_splist,allabs = allabs,regtype = regtype,ncl = nclY1[r],plotPCA = F,clustid = "tripidmon",allclust = F, ll5 = TRUE, flag = flag, fnhead = fnh,covarnames = cvn)
  save(dataset,file = paste0(fnh,".RData"))
}

regtype = "regY2"
for (r in c(1,2,6)) {
  fnh <- paste(flag,regtype,r,sep = "_")
  dataset <- clust_PCA_run(r = r,ddd = dat,allsp = use_splist,allabs = allabs,regtype = regtype,ncl = nclY2[r],plotPCA = F,clustid = "tripidmon",allclust = F, ll5 = TRUE, flag = flag, fnhead = fnh,covarnames = cvn)
  save(dataset,file = paste0(fnh,".RData"))
}

##################################################
# US only, clusters, no HBF
#
# R1 - 3 or 4 clusters. 1=swo mostly, 2=bet+yft+swo+alb, 3=bsh+swo+yft (N), 4=yft+swo, S + W). Use 2 and 3.
# R2 - 4 or 5 clusters. 1=swo+yft+bet, 2=swo (NW), 3=swo+bum+yft+bsh+bet+alb, 4=yft+swo (NW). Use 1 and 3.

resdir <- paste0(USalysis_dir,"std_cl_USonly_nohbf/")
dir.create(resdir)
setwd(resdir)

#clkeepCN_Y <- list("yft" = list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepJP_Y <- list("yft" = list(c(1,2,4),c(1,2,3,4),c(1,2,3)))
clkeepKR_Y <- list("yft" = list(c(0),c(1,2,3,4),c(1,2,3)))
clkeepTW_Y <- list("yft" = list(c(4),c(2,3),c(0)))
clkeepUS_Y <- list("yft" = list(c(1,2,3,4),c(1,2,3,4),c(0)))
clk_Y1 <- list(JP = clkeepJP_Y,KR = clkeepKR_Y,TW = clkeepTW_Y,US = clkeepUS_Y)

runpars <- list()
runpars[["yft"]] <- list(regtype = "regY1", regtype2 = "Y1", clk = clk_Y1, doregs = 1:2, addcl = TRUE, dohbf = FALSE, cltype = "hcltrp")

short_splist <- c("alb","bet","yft")
stdlabs <- c("vessid","yrqtr","latlong","op_yr","hbf","hooks",short_splist,"lat","lon","lat5","lon5", "reg", "hcltrp", "flag")

keepd = TRUE; maxyr = 2018; maxqtrs = 200; minqtrs_byreg = c(5,5,5);
for (runsp in c("yft")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  jdat <- data.frame()
  for (flag in c("US")) {
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
      glmdat <- select_data_JointIO(jdat,runreg = runreg,clk = clk,minqtrs = minqtrs,runsp = runsp,mt = "deltabin",vars = vars, maxqtrs = maxqtrs, minvess = 50, minll = 50, minyrqtr = 50, addcl = addcl, cltype = cltype, addpca = NA, samp = NA, strsmp = NA, minhbf = 1)
      if (nrow(glmdat) > 60000) glmdat <- samp_strat_data(glmdat,60)
      wtt.all   <- mk_wts(glmdat,wttype = "area")
      fmla.oplogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = T,nhbf = 3)
      fmla.oplogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = F,nhbf = 3)
      fmla.boatlogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = T,nhbf = 3)
      fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = F,nhbf = 3)
      mn <- with(glmdat,0.1 * mean(get(runsp)/hooks))

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

      # delta lognormal
      modlab = "dellog_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg);
      do_deltalog(dat = glmdat,dohbf = dohbf,addboat = F,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

      modlab = "dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
      do_deltalog(dat = glmdat,dohbf = dohbf,addboat = T,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

      graphics.off()
    }
}

#######################--------------------------------

resdir <- paste0(USalysis_dir,"std_cl_USonly_hbf/")
dir.create(resdir)
setwd(resdir)

#clkeepCN_Y <- list("yft" = list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepJP_Y <- list("yft" = list(c(1,2,4),c(1,2,3,4),c(1,2,3)))
clkeepKR_Y <- list("yft" = list(c(0),c(1,2,3,4),c(1,2,3)))
clkeepTW_Y <- list("yft" = list(c(4),c(2,3),c(0)))
clkeepUS_Y <- list("yft" = list(c(1,2,3,4),c(1,2,3,4),c(0)))
clk_Y1 <- list(JP = clkeepJP_Y,KR = clkeepKR_Y,TW = clkeepTW_Y,US = clkeepUS_Y)

runpars <- list()
runpars[["yft"]] <- list(regtype = "regY1", regtype2 = "Y1", clk = clk_Y1, doregs = 1:2, addcl = TRUE, dohbf = TRUE, cltype = "hcltrp")

stdlabs <- c("vessid","yrqtr","latlong","op_yr","hbf","hooks",short_splist,"lat","lon","lat5","lon5", "reg", "hcltrp", "flag")

keepd = TRUE; maxyr = 2018; maxqtrs = 200; minqtrs_byreg = c(5,5,5);
for (runsp in c("yft")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  jdat <- data.frame()
  for (flag in c("US")) {
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
    glmdat <- select_data_JointIO(jdat,runreg = runreg,clk = clk,minqtrs = minqtrs,runsp = runsp,mt = "deltabin",vars = vars, maxqtrs = maxqtrs, minvess = 50, minll = 50, minyrqtr = 50, addcl = addcl, cltype = cltype, addpca = NA, samp = NA, strsmp = NA, minhbf = 1)
    if (nrow(glmdat) > 60000) glmdat <- samp_strat_data(glmdat,60)
    wtt.all   <- mk_wts(glmdat,wttype = "area")
    fmla.oplogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = T,nhbf = 3)
    fmla.oplogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = F,nhbf = 3)
    fmla.boatlogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = T,nhbf = 3)
    fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = F,nhbf = 3)
    mn <- with(glmdat,0.1 * mean(get(runsp)/hooks))

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

    # delta lognormal
    modlab = "dellog_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg);
    do_deltalog(dat = glmdat,dohbf = dohbf,addboat = F,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

    modlab = "dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat = glmdat,dohbf = dohbf,addboat = T,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd)

    graphics.off()
  }
}

