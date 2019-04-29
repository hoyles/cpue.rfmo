### --- DATA PREPARATION ----

### --- Paths ----
projdir <- "~/IOTC/2018_CPUE/"
sydir <- paste0(projdir,"SY/")
datadir1 <- paste0(sydir,"data/catcheffort/")
syalysis_dir <- paste0(sydir,"analyses/")
syfigs <- paste0(sydir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
clusdir <- paste0(sydir, "clustering/")
setwd(syalysis_dir)

### --- Libraries ----
library("date",quietly = TRUE)
library("splines",quietly = TRUE)
library("maps",quietly = TRUE)
library("mapdata",quietly = TRUE)
library("maptools",quietly = TRUE)
library("lunar",quietly = TRUE)
library("lubridate",quietly = TRUE)
library("readr",quietly = TRUE)
library("plyr",quietly = TRUE)
library("dplyr",quietly = TRUE)
library("mgcv",quietly = TRUE)
library("randomForest",quietly = TRUE)
library("nFactors",quietly = TRUE)
library("data.table",quietly = TRUE)
library("cluster",quietly = TRUE)
library("boot",quietly = TRUE)
library("beanplot",quietly = TRUE)

### install.packages("../../../../../influ_0.8.zip", repos = NULL, type = "win.binary")
library("influ",quietly = TRUE) # downloaded here (https://github.com/trophia/influ/releases/) after installing 'proto'
library(lubridate)   # dates management
library(magrittr)    # for the %>% symbol used in setup_IO_regions()

### Added by manu
library(rgdal)  #readOGR for reading shapefile
library(RColorBrewer) #color palettes with brewer.pal()
library(scales)       #color gradient with alpha()

### Library developed by Simon
### Built from the github sudo R CMD build
#install.packages("../../../../../cpue.rfmo_0.1.0.zip",repos = NULL,type = "win.binary")
library(cpue.rfmo)

### Sources functions updated through the GitHub (instead of loading the library as it needs to be built)
# source("../../../../../R/dataclean_functions.R")
# source("../../../../../R/dataprep_functions.R")

### Read the data set ----
fulldataset <- read.csv(paste0(datadir1,"LL_CE_N0.csv"),sep = ",", header = TRUE)
names(fulldataset[,19:40])  # 22 species reported

### Add TripHistoryID that is missing: Generate a new one from vesselid and year-month
fulldataset$TripID <- paste(fulldataset$VessHistoryID, fulldataset$LogYear,fulldataset$LogMonth,sep="-")

### Filter the data set ----

### Select years and remove some inconsistent vessels
seldat1 <- fulldataset[fulldataset$LogYear %in% 2000:2016 & !(fulldataset$VesselName %in% c('ADMIRAL DE RUITER','CARINA','BOUZON')),]

# Remove species with very few catch: BAR (Sphyraena spp), OCS (Carcharhinus longimanus), POR (Lamna nasus), SKJ (Katsuwonus pelamis), SSP (Tetrapturus angustirostris), THR (Alopias spp)
seldat2 <- seldat1[, c("LogYear","LogMonth","LogDay","LatDec","LonDec", "HooksR_Final","ALB","BET","BLM","BSH","BUM","BXQ","MAK","MLS","MZZ","OIL","RSK","SBF","SFA","SKH","SWO","YFT","TripID","VessHistoryID","LogID")]

### Define the standard names
nms <- c("op_yr","op_mon","op_day","lat","lon","hooks","alb","bet","blm","bsh","bum","bxq","mak","mls","mzz","oil","rsk","sbf","sfa","skh","swo","yft","trip_st","vessid","logbookid")
names(seldat2) <- nms
head(seldat2,1)

### Clean the data ----
clndat <- dataclean_SY(seldat2, rmssp = FALSE, splist = c("alb","bet","blm","bsh","bum","bxq","mak","mls","mzz","oil","rsk","sbf","sfa","skh","swo","yft"))

### Prepare the data ----
### Add some fields to the data set: lunar illumination (moon), 5° long and lat, total catch (Total), and tuna catch (Total2), year of fishing trip, unique trip identifier
prepdat <- dataprep_SY(clndat, region = "IO", splist = c("alb","bet","blm","bsh","bum","bxq","mak","mls","mzz","oil","rsk","sbf","sfa","skh","swo","yft"))

### Add assessment areas ----
dat <- setup_IO_regions(prepdat, regY=TRUE, regY1=FALSE, regY2=TRUE,regB=TRUE,regB1=TRUE, regB2=TRUE, regB3=TRUE, regA=FALSE, regA1=FALSE, regA2=FALSE, regA3=FALSE, regA4=FALSE, regA5=FALSE)

### Save the data ----
save(dat,file=paste(datadir1,"SYdatx.RData",sep=""))

### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### DATA CLUSTERING ----

projdir <- "~/IOTC/2018_CPUE/"
sydir <- paste0(projdir, "SY/")
datadir1 <- paste0(sydir, "data/catcheffort/")
syalysis_dir <- paste0(sydir, "analyses/")
syfigs <- paste0(sydir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
clusdir <- paste0(sydir, "clustering/")

# Libraries ----
library("date",quietly = TRUE)
library("splines",quietly = TRUE)
library("maps",quietly = TRUE)
library("mapdata",quietly = TRUE)
library("maptools",quietly = TRUE)
library("data.table",quietly = TRUE)
library("lunar",quietly = TRUE)
library("lubridate",quietly = TRUE)
library("readr",quietly = TRUE)
library("plyr",quietly = TRUE)
library("dplyr",quietly = TRUE)
library("dtplyr",quietly = TRUE,warn.conflicts = FALSE)
library("tm",quietly = TRUE)
library(cpue.rfmo, warn.conflicts = FALSE)

### Libraries for clustering
library("randomForest",quietly=TRUE)
library("influ",quietly=TRUE)
library("nFactors",quietly=TRUE)
library("cluster",quietly=TRUE)
library("boot",quietly=TRUE)
library("beanplot",quietly=TRUE)

### Load the data ----
load(file=paste0(datadir1,"SYdat.RData"))
load(file=paste0(datadir1,"SYdatx.RData"))
str(dat)

### Set the working directory ----
setwd(clusdir)

# Set up input variables for clustering and standardization
# Generate a group of billfish (mls = striped marlin, bum = blue marlin, blm = black marlin, bxq = marlins nei)
dat$bll <- apply(dat[,c('mls','bum','blm','bxq')],1,sum,na.rm=T)

### Set up input variables for clustering and standardization
summary(dat)
sy_splist <-  c("alb","bet","yft","swo","mls","bum","blm","sbf","skh","bxq","sfa","sbf","rsk","oil","mzz","bsh","bll")

### Plot the mean catch per year of each species by region, to use when deciding which species to cluster
plot_spfreqyq(indat = dat, reg_struc = "regY", splist = sy_splist, flag = "SY", mfr = c(5,4))
plot_spfreqyq(indat = dat, reg_struc = "regY2", splist = sy_splist, flag = "SY", mfr = c(4,3))

# Put chosen species here
cl_splist <- c("alb","bet","yft","swo","oil","mzz","bll")

# Variables to use
dat$hbf <- 0 # Not used for anything, but needed to make the standardization code work.
allabs <- c("op_yr","op_mon","vessid","yrqtr","latlong","hooks","hbf","tripidmon","moon",cl_splist,"Total","dmy","lat","lon","lat5","lon5","regY","regY2","regB","regB1","regB2")
str(dat[,allabs])
#allabs[!(allabs %in% names(dat))]

### Determine the number of clusters. Come back and edit this
reglist <- list()
reglist$regY <-  list(allreg = c(1,2,3,4,5), ncl = c(3,4,5,4,5))
reglist$regY2 <- list(allreg = c(2,7), ncl = c(3,4,5,4,5,5,4))
flag="SY"

### Covariates to pass to next stage
#cvn <- c("yrqtr","latlong","hooks",      "vessid","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")
cvn <- c("yrqtr","latlong","hooks","hbf","vessid","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")
### r parameter?
r <- 4

# Do the clustering and save the results for later (we also need to decide on the ALB regional structures below)

### Read the cluster functions new version
#source("../../../../../R/cluster_functions.R")

# RegionY
run_clustercode_byreg(indat=dat, reg_struc = "regY", allsp=cl_splist, allabs=allabs, flag=flag, cvnames = cvn, rgl = reglist, dohbf=F)

# Region Y2
run_clustercode_byreg(indat=dat, reg_struc = "regY2", allsp=cl_splist, allabs=allabs, flag=flag, cvnames = cvn, rgl = reglist, dohbf=F)

### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### CPUE STANDARDISATION ----

### --- Paths ----
projdir <- "~/IOTC/2018_CPUE/"
sydir <- paste0(projdir,"SY/")
datadir1 <- paste0(sydir,"data/catcheffort/")
syalysis_dir <- paste0(sydir,"analyses/")
syfigs <- paste0(sydir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
clusdir <- paste0(sydir, "clustering/")
setwd(syalysis_dir)

### Libraries ----
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

# Define the clusters to be used. Will need to set this up after checking the cluster allocations
clkeepSY_Y <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(2),c(1,2,3,4),c(0)))
clk_Y <- list(SY=clkeepSY_Y)

clkeepSY_Y2 <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(2),c(1,2,3,4),c(0),c(1,2,3,4)))
clk_Y2 <- list(SY=clkeepSY_Y2)

clkeepSY_B2 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2),c(1,2,4)))
clk_B2 <- list(SY=clkeepSY_B2)

clkeepSY_B3 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2),c(1,2,4),c(1,2,3,4)))
clk_B3 <- list(SY=clkeepSY_B3)

use_splist <- c("alb","bet","yft")
stdlabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","moon",use_splist,"Total","lat","lon","lat5","lon5","hcltrp","reg","flag")

## ---------------------------------------------
# Run various standardization scenarios. Only one here now, and for bigeye instead of YFT and ALB.
# There are some new ones from the Madrid meeting which I will set up later.
# I can't test the code without data, so apologies if some of the changes in Madrid have broken this code.
# We can fix it in Keelung.
## ---------------------------------------------

# With clusters, and hbf
std_dir <- paste0(sydir,"std/")
setwd(std_dir)

# The runpars define the approach to be used in this run
regA4_minss <- list(minq_byreg = c(3,2,5,5), minvess=c(60,40,100,100), minll=c(30,20,50,50), minyrqtr = c(30,20,50,50), minyqll = c(3,3,5,5))
regA5_minss <- list(minq_byreg = c(5), minvess=c(100), minll=c(50), minyrqtr = c(50), minyqll = c(5))
regB3_minss <- list(minq_byreg = c(5,5,5,3,5), minvess=c(100,100,100,60,100), minll=c(50,50,50,30,50), minyrqtr = c(50,50,50,30,50), minyqll = c(5,5,5,3,5))
regY2_minss <- list(minq_byreg = c(2,5,5,2,5,2,5), minvess=c(40,100,100,40,100,40,100), minll=c(20,50,50,20,50,20,50), minyrqtr = c(20,50,50,20,50,20,50), minyqll = c(3,5,5,3,5,3,5))

runpars <- list()
runpars[["regA4"]] <- list(runsp = "alb", regtype2 = "A4", clk = clk_A4, doregs = 1:4, addcl = TRUE, dohbf = FALSE, dohook = TRUE, cltype = "hcltrp", minss = regA4_minss)
runpars[["regB3"]] <- list(runsp = "bet", regtype2 = "B3", clk = clk_B3, doregs = 1:5, addcl = TRUE, dohbf = FALSE, dohook = TRUE, cltype = "hcltrp", minss = regB3_minss)
runpars[["regY2"]] <- list(runsp = "yft", regtype2 = "Y2", clk = clk_Y2, doregs = c(2:5,7), addcl = TRUE, dohbf = FALSE, dohook = TRUE, cltype = "hcltrp", minss = regB3_minss)
runpars[["regA5"]] <- list(runsp = "alb", regtype2 = "A5", clk = clk_A5, doregs = 1,   addcl = TRUE, dohbf = TRUE, dohook = TRUE, cltype = "hcltrp", minss = regA5_minss)

regstr <- "regA4"; runreg <- 2; keepd <- TRUE; doflags <- "SY"
maxyr <- 2018
for (regstr in c("regA4")) {
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

