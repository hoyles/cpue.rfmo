# Set up directories
projdir <- "~/IOTC/2019_CPUE_tropical/"
natdir <- paste0(projdir, "KR/")
datadir <- paste0(natdir, "data/")
analysis_dir <- paste0(natdir, "analyses/")
figdir <- paste0(natdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")

dir.create(figdir)
dir.create(analysis_dir)
setwd(analysis_dir)

### install.packages("../../../../../influ_0.8.zip", repos = NULL, type = "win.binary")
library("influ",quietly = TRUE) # downloaded here (https://github.com/trophia/influ/releases/) after installing 'proto'

packages=c('tidyverse', 'openxlsx','knitr','date','splines','maps','mapdata','maptools','lunar','lubridate','mgcv','randomForest','nFactors','data.table','cluster','fastcluster,'boot','beanplot','influ','rgdal','RColorBrewer','scales','tm','proto')
sapply(packages,function(x) {if (!x %in% installed.packages()) install.packages(x,repos = 'https://pbil.univ-lyon1.fr/CRAN/')})
invisible(lapply(packages, library, character.only=TRUE, quietly = TRUE, warn.conflicts = FALSE))

# The command 'install_github("hoyles/cpue.rfmo", auth_token = 'xxxxxxxxxxxxxxxxx')' should now install cpue.rfmo succcessfully.
# You'll need to generate your own github personal access token. See https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line. You also need to set the scope of the token to have full control of private repositories. Do this on the page where you generate the token.

### Library developed by Simon
### Built from the github sudo R CMD build
#install.packages("../../../../../cpue.rfmo_0.1.0.zip",repos = NULL,type = "win.binary")
library(cpue.rfmo)


##################

# Load data. This section will only need to be changed if the data format changes.
rawdat <- read.csv(paste0(datadir,"IO_KOR_LL_OP_data_20190428.csv"), header = TRUE, stringsAsFactors = FALSE)
str(rawdat) #test

# Set up standard names, the same as for other fleets
nms <- c("op_yr","op_mon","VESSEL_CD","VESSEL_NAME","DATE","Lat01","NS","Long01","EW","hooks","floats",
       "alb","bet","blm","bum","mls","oth","sbt","sfa","sha","skj","swo","yft","Total")
cbind(names(rawdat),nms)
names(rawdat) <- nms
head(rawdat)
str(rawdat)
#storedat <- rawdat
#rawdat <- storedat

# Prepare and check the data
splist <- c("alb","bet","blm","bum","mls","oth","sbt","sfa","sha","skj","swo","yft")
prepdat <- dataprep_KR(rawdat, splist)
head(prepdat)
prepdat2 <- setup_IO_regions(prepdat,  regY=T, regY1=T, regY2=T, regY3=T, regB=T, regB2=T, regB3=T, regB4=T, regA=F, regA1=F, regA2=F, regA3=F, regA4=F, regA5=F)
head(prepdat2)
dat <- dataclean_KR(prepdat2, yearlim = 2019, splist = splist)
save(prepdat,dat,file=paste0(analysis_dir, "KRdat.RData"))

load(file=paste0(analysis_dir, "KRdat.RData"))
str(dat)
summary(dat)
table(dat$vessid,dat$op_yr)
table(dat$op_yr,dat$vessid)
table(dat$op_yr,is.na(dat$vessid))
table(dat$op_yr,(dat$hooks > 0))

# ===================================================================================
# Plot and explore the data
# ===================================================================================
# Plot time distribution of sets
windows(width=15,height=9)
hist(prepdat$dmy,breaks="days",freq=T,xlab="Date",main="Sets per day")
savePlot(file="sets_per_day.png",type="png")
table(prepdat$dmy)


# Check a few specific instances where there have been problems in the past. Consider checking for similar problems in new data.
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

# Look for outliers. Individual sets with high catch are not a problem.
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

# Map the effort
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

# Make maps to check regional patterns
a <- unique(paste(dat$lat,dat$lon))
regnames <- names(dat)[grep("reg", names(dat))]
a0 <- dat[match(a,paste(dat$lat,dat$lon)),c("lat","lon",regnames)]
windows(width=15,height=10)
for(fld in regnames) {
  reg <- with(a0,get(fld))
  plot(a0$lon,a0$lat,type="n",xlab="Longitude",ylab="Latitude",main=fld)
  text(a0$lon,a0$lat,labels=reg,cex=0.6,col=reg+1)
  maps::map(database = "world", add=T, fill = F)
  savePlot(paste0("map_",fld),type="png")
}

# Mean fishing location, yrqtr scale
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

# Mean fishing location, year scale
windows(width=15,height=10);par(mfrow=c(1,2))
plot(tapply(dat$op_yr,dat$op_yr,mean),tapply(dat$lat5,dat$op_yr,mean),xlab="yr",ylab="Mean latitude")
plot(tapply(dat$lon5,dat$op_yr,mean),tapply(dat$op_yr,dat$op_yr,mean),ylab="yr",xlab="Mean longitude")
savePlot("mean_fishing_location2.png",type="png")

# Store summaries of hbf by region and through time
write.csv(table(round(dat$hbf,0),dat$regY,useNA="always"),file="hbf by region.csv")
write.csv(table(round(dat$hbf,0),floor(dat$yrqtr/5)*5,dat$regY,useNA="always"),file="hbf by region by 5 years.csv")

# Map mean hbf by 5yr period
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

write.csv(table(dat$lat5,dat$lon5),file="ops by lat-long.csv")
write.csv(table(dat$lat5,dat$lon5,5*floor(dat$yrqtr/5)),file="ops by lat-long-5yr.csv")

# Exploratory regression trees. These are not particularly useful.
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
simplemod <- rpart(a$yftcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$albcpue + a$sfacpue + a$mlscpue + a$blmcpue + a$bumcpue)
windows(width=11,height=7)
plot(simplemod)
text(simplemod)
simplemod <- rpart(a$albcpue ~ a$lon + a$lat + a$yrqtr + a$swocpue + a$betcpue + a$yftcpue + a$sfacpue + a$mlscpue + a$blmcpue + a$bumcpue)
windows(width=11,height=7)
plot(simplemod)
text(simplemod)


# ===================================================================================
# Start the analysis proper
# ===================================================================================
#Clustering
#----- Clustering ----

packages=c('tidyverse', 'openxlsx','knitr','date','splines','maps','mapdata','maptools','lunar','lubridate','mgcv','randomForest','nFactors','data.table','cluster','fastcluster','boot','beanplot','influ','rgdal','RColorBrewer','scales','tm','proto','influ')
invisible(lapply(packages, library, character.only=TRUE, quietly = TRUE, warn.conflicts = FALSE))

library(cpue.rfmo)

projdir <- "~/IOTC/2019_CPUE_tropical/"
natdir <- paste0(projdir, "KR/")
datadir <- paste0(natdir, "data/")
analysis_dir <- paste0(natdir, "analyses/")
figdir <- paste0(natdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
clustdir <- paste0(natdir,"clustering/")

dir.create(clustdir)

setwd(clustdir)
load(file=paste0(analysis_dir, "KRdat.RData"))

# Set up input variables for clustering and standardization
dat <- data.frame(dat)
kr_splist <-  c("alb","bet","blm","bum","mls","oth","sbt","sfa","sha","skj","swo","yft")

# Plot the mean catch per year of each species by region, to use when deciding which species to cluster
# plot_spfreqyq(indat = dat, reg_struc = "regY2", splist = kr_splist, flag = "KR", mfr = c(4,3))
plot_spfreqyq(indat = dat, reg_struc = "regY2", splist = kr_splist, flag = "KR", mfr = c(4,3))
plot_spfreqyq(indat = dat, reg_struc = "regB3", splist = kr_splist, flag = "KR", mfr = c(4,3))
graphics.off()

# Put chosen species here
cl_splist <- c("alb","bet","blm","bum","mls","oth","sbt","swo","yft")

regnames <- names(dat)[grep("reg", names(dat))]
# Variables to use
allabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tripidmon","moon",cl_splist,"Total","dmy","lat","lon","lat5","lon5",regnames)
str(dat[,allabs])

# Determine the number of clusters. Come back and edit this.
reglist <- list()
# reglist$regA4 <- list(allreg = 1:4, ncl = c(5,4,4,4))
# reglist$regA5 <- list(allreg = 1,   ncl = 4)
reglist$regB2 <- list(allreg = 1:4, ncl = c(4,6,4,4))
reglist$regB3 <- list(allreg = c(1,5), ncl = c(4,0,0,0,4))
reglist$regB4 <- list(allreg = 1, ncl = c(4))
reglist$regY <-  list(allreg = c(1:6), ncl = c(5,4,5,4,4,4))
reglist$regY2 <- list(allreg = c(2,7), ncl = c(0,4,0,0,0,0,4))
reglist$regY3 <- list(allreg = c(1), ncl = c(4))
flag="KR"

# Covariates to pass to next stage
cvn <- c("yrqtr","latlong","hooks","hbf","vessid","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")
r=4

# Do the clustering and save the results for later
dorg <- c("regY", "regY2", "regY3", "regB2", "regB3", "regB4")
dorg <- c("regB4")
for (rg in dorg) {
  run_clustercode_byreg(indat=dat, reg_struc = rg, allsp=cl_splist, allabs=allabs, flag=flag, cvnames = cvn, rgl=reglist)
}

# ========================================================
# Standardizations, Korea only
# ========================================================

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
library("survival")

library("cpue.rfmo")

projdir <- "~/IOTC/2019_CPUE_tropical/"
natdir <- paste0(projdir, "KR/")
datadir <- paste0(natdir, "data/")
analysis_dir <- paste0(natdir, "analyses/")
figdir <- paste0(natdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
clustdir <- paste0(natdir,"clustering/")

# Define the clusters to be used. Will need to set this up after checking the cluster allocations
# clkeepKR_A4 <- list("alb"=list(c(1,2,3,4), c(1,2,3,4), c(1:4), c(1:4)))
# clk_A4 <- list(KR=clkeepKR_A4)
#
# #clkeepKR_A5 <- list("alb"=list(c(2,3,5)))
# clkeepKR_A5 <- list("alb"=list(c(1:4)))
# clk_A5 <- list(KR=clkeepKR_A5)

clkeepKR_B2 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4,5,6),c(1,2,3,4),c(1,3,4)))
clkeepKR_B3 <- list("bet"=list(c(1,2,3,4),c(0),c(0),c(0),c(1,2,3,4)))
clkeepKR_B4 <- list("bet"=list(c(1,2,3,4)))
clkeepKR_Y <- list("yft"=list(c(1,2,3,4,5),c(1,2,3,4),c(1,2,3,4),c(2,4),c(1,2,3,4),c(1,2,3,4)))
clkeepKR_Y2 <- list("yft"=list(c(0),c(1,2,3,4),c(0),c(0),c(0),c(0),c(1,2,3,4)))
clkeepKR_Y3 <- list("yft"=list(c(1,2,3,4)))

clk_Y <- list(KR=clkeepKR_Y)
clk_Y2 <- list(KR=clkeepKR_Y2)
clk_Y3 <- list(KR=clkeepKR_Y3)
clk_B2 <- list(KR=clkeepKR_B2)
clk_B3 <- list(KR=clkeepKR_B3)
clk_B4 <- list(KR=clkeepKR_B4)

std_splist <- c("alb","bet","yft")
stdlabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","moon",std_splist,"Total","lat","lon","lat5","lon5","hcltrp","reg","flag")

## ---------------------------------------------
# Run various standardization scenarios. Only one here now, and for bigeye instead of YFT and ALB.
# There are some new ones from the Madrid meeting which I will set up later.
# I can't test the code without data, so apologies if some of the changes in Madrid have broken this code.
# We can fix it in Keelung.
## ---------------------------------------------

# With clusters, and hbf
std_dir <- paste0(natdir,"std/")
setwd(std_dir)

# The runpars define the approach to be used in this run
regA4_minss <- list(minq_byreg = c(3,2,5,5), minvess=c(60,40,100,100), minll=c(30,20,50,50), minyrqtr = c(30,20,50,50), minyqll = c(3,3,5,5))
regA5_minss <- list(minq_byreg = c(5), minvess=c(100), minll=c(50), minyrqtr = c(50), minyqll = c(5))
regB3_minss <- list(minq_byreg = c(5,5,5,3,5), minvess=c(100,100,100,60,100), minll=c(50,50,50,30,50), minyrqtr = c(50,50,50,30,50), minyqll = c(5,5,5,3,5))
regY2_minss <- list(minq_byreg = c(2,5,5,2,5,2,5), minvess=c(40,100,100,40,100,40,100), minll=c(20,50,50,20,50,20,50), minyrqtr = c(20,50,50,20,50,20,50), minyqll = c(3,5,5,3,5,3,5))

runpars <- list()
runpars[["regA4"]] <- list(runsp = "alb", regtype2 = "A4", clk = clk_A4, doregs = 1:4, addcl = TRUE, dohbf = FALSE, dohook = TRUE, cltype = "hcltrp", minss = regA4_minss)
runpars[["regB3"]] <- list(runsp = "bet", regtype2 = "B3", clk = clk_B3, doregs = 1:5, addcl = TRUE, dohbf = FALSE, dohook = TRUE, cltype = "hcltrp", minss = regB3_minss)
runpars[["regY2"]] <- list(runsp = "yft", regtype2 = "Y2", clk = clk_Y2, doregs = c(2:5,7), addcl = TRUE, dohbf = FALSE, dohook = TRUE, cltype = "hcltrp", minss = regY2_minss)
runpars[["regA5"]] <- list(runsp = "alb", regtype2 = "A5", clk = clk_A5, doregs = 1,   addcl = TRUE, dohbf = TRUE, dohook = TRUE, cltype = "hcltrp", minss = regA5_minss)

regstr <- "regY2"; runreg <- 2; keepd <- TRUE; doflags <- "KR" # Values used for testing
maxyr <- 2019
for (regstr in c("regY2")) {
  rp <- runpars[[regstr]]
  runsp <- rp$runsp
  addcl <- rp$addcl
  dohbf <- rp$dohbf
  jdat <- data.frame()
  for (flag in doflags) {
    for (r in rp$doregs) {
      load(paste0(projdir,flag,"/clustering/",paste(flag,regstr,r,sep = "_"),".RData"))
      dataset$flag <- flag
      dataset$qtr <- revtrunc(defactor(dataset$yrqtr))
      jdat <- rbind(jdat,dataset[,stdlabs])
      rm(dataset)
    }
  }
  jdat <- jdat[jdat$yrqtr < maxyr,]
  jdat$vessidx <- jdat$vessid
  jdat$vessid <- paste0(jdat$flag,jdat$vessid)
  jdat$vessid <- as.factor(jdat$vessid)

  vars <- c("vessid","hooks","yrqtr","latlong","hbf")
  for (runreg in rp$doregs) {
    glmdat <- select_data_IO2(jdat,runreg = runreg,runpars = rp, mt = "deltabin",vars = vars)
    if (nrow(glmdat) > 60000) glmdat <- samp_strat_data(glmdat,60)
    wtt.all   <- mk_wts(glmdat,wttype = "area")
    fmla.oplogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = T,nhbf = 3, dohook = rp$dohook)
    fmla.oplogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = F,addcl = F,nhbf = 3, dohook = rp$dohook)
    fmla.boatlogn <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = T,nhbf = 3, dohook = rp$dohook)
    fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype = "logn",dohbf = dohbf,addboat = T,addcl = F,nhbf = 3, dohook = rp$dohook)
    mn <- with(glmdat,0.1 *  mean(get(runsp)/hooks))

    modlab = "lognC_novess_allyrs"; fname <- paste0("Joint_",regstr,"_R",runreg)
    if (lu(glmdat$clust) > 1)
    { model <- glm(fmla.oplogn,data = glmdat,weights = wtt.all,family = "gaussian", model = keepd);gc() } else
    { model <- glm(fmla.oplogn_ncl,data = glmdat,weights = wtt.all,family = "gaussian", model = keepd);gc() }
    summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

    modlab = "lognC_boat_allyrs"; fname <- paste0("Joint_",regstr,"_R",runreg)
    if (lu(glmdat$clust) > 1)
    { model <- glm(fmla.boatlogn,data = glmdat,weights = wtt.all,family = "gaussian", model = keepd);gc() } else
    { model <- glm(fmla.boatlogn_ncl,data = glmdat,weights = wtt.all,family = "gaussian", model = keepd);gc() }
    summarize_and_store(mod = model,dat = glmdat,fname,modlab,dohbf = dohbf, keepd = keepd);rm(model)

    # delta lognormal
    modlab = "dellog_novess_allyrs"; fname <- paste0("Joint_", regstr,"_R",runreg);
    do_deltalog(dat = glmdat,dohbf = dohbf,addboat = F,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd, dohook = rp$dohook)

    modlab = "dellog_boat_allyrs"; fname <- paste0("Joint_",regstr,"_R",runreg)
    do_deltalog(dat = glmdat,dohbf = dohbf,addboat = T,addcl = addcl,nhbf = 3,runsp = runsp,fname = fname,modlab = modlab, keepd = keepd, dohook = rp$dohook)

    graphics.off()
  }
}

