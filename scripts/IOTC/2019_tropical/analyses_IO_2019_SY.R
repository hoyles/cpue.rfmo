### --- DATA PREPARATION ----

### --- Paths ----
projdir <- "~/IOTC/2019_CPUE_tropical/"
natdir <- paste0(projdir, "SY/")

datadir <- paste0(natdir, "data/")
analysis_dir <- paste0(natdir, "analyses/")
figdir <- paste0(natdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
clustdir <- paste0(natdir, "clustering/")
dir.create(clustdir)

dir.create(figdir)
dir.create(analysis_dir)
setwd(analysis_dir)

### install.packages("../../../../../influ_0.8.zip", repos = NULL, type = "win.binary")
library("influ",quietly = TRUE) # downloaded here (https://github.com/trophia/influ/releases/) after installing 'proto'

packages=c('tidyverse', 'openxlsx','knitr','date','splines','maps','mapdata','maptools','lunar','lubridate','mgcv','randomForest','nFactors','data.table','cluster','boot','beanplot','influ','rgdal','RColorBrewer','scales','tm','proto')
sapply(packages,function(x) {if (!x %in% installed.packages()) install.packages(x,repos = 'https://pbil.univ-lyon1.fr/CRAN/')})
invisible(lapply(packages, require, character.only=TRUE, quietly = TRUE, warn.conflicts = FALSE))

# The command 'install_github("hoyles/cpue.rfmo", auth_token = 'xxxxxxxxxxxxxxxxx')' should now install cpue.rfmo succcessfully.
# You'll need to generate your own github personal access token. See https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line. You also need to set the scope of the token to have full control of private repositories. Do this on the page where you generate the token.

### Library developed by Simon
### Built from the github sudo R CMD build
#install.packages("../../../../../cpue.rfmo_0.1.0.zip",repos = NULL,type = "win.binary")
library(cpue.rfmo)


### Read the data set ----
catch_effort_dataset_raw <- read.csv("../data/LL_CE_N0.csv",header=TRUE,sep=';')
catch_effort_dataset_raw <- catch_effort_dataset_raw[catch_effort_dataset_raw$LogYear %in% 2000:2017,]

### Remove fishing operation with mistake in numbers of yellowfin caught (for now)
catch_effort_dataset_raw <- catch_effort_dataset_raw[!is.na(catch_effort_dataset_raw$LogID) & catch_effort_dataset_raw$LogID != 1079520,]

### Extract species names
species_names <- names(catch_effort_dataset_raw)[19:45]

### Replace NA values by 0 in the catch
catch_effort_dataset_raw[,species_names][is.na(catch_effort_dataset_raw[,species_names])] <- 0

### Select the fields of interest
catch_effort_dataset_raw1 <- catch_effort_dataset_raw[, c("LogYear","LogMonth","LogDay","LatDec","LonDec", "HooksR_Final",species_names,"TripHistoryID","VessHistoryID","LogID","HooksBetweenFloats")]

### Define the standard names
nms <- c("op_yr","op_mon","op_day","lat","lon","hooks",tolower(species_names),"trip_st","vessid","logbookid","hbf")
names(catch_effort_dataset_raw1) <- nms

### Melt the catch data by for each operation and species
catch_raw_melted <- melt(catch_effort_dataset_raw1[,c('logbookid','op_yr',tolower(species_names))],id.vars = c('logbookid','op_yr'), variable.name = 'species')


### Prepare the data ----
number_caught_min <- 7000

species_to_remove <- names(apply(catch_effort_dataset_raw1[,tolower(species_names)],2,sum)[(apply(catch_effort_dataset_raw1[,tolower(species_names)],2,sum)<=number_caught_min)])

catch_effort_dataset1 <- catch_effort_dataset_raw1[,!(names(catch_effort_dataset_raw1) %in% species_to_remove)]

### Remove vessels less than number_operations_min of fishing operations throughout the time period
number_operations_min <- 50

noperations_by_vessel <- ddply(catch_effort_dataset1,'vessid',summarize,nlogbookids=length(logbookid))
vessels_to_remove <- noperations_by_vessel[noperations_by_vessel$nlogbookids<50,'vessid']

catch_effort_dataset2 <- catch_effort_dataset1[!(catch_effort_dataset1$vessid %in% vessels_to_remove),]

# Replace anormalous values of HBF by NAs, i.e. HBF > hbf_max
hbf_max <- 31
catch_effort_dataset2[!is.na(catch_effort_dataset2$hbf) & catch_effort_dataset2$hbf> hbf_max,'hbf'] <- NA

### Add some fields to the data set: lunar illumination (moon), 5° long and lat, total catch (Total), and tuna catch (Total2), year of fishing trip, unique trip identifier
prepdat <- dataprep_SY(catch_effort_dataset2, region = "IO", splist = c("alb","bet","blm","bsh","bum","bxq","mak","mls","mzz","oil","rsk","sbf","sfa","skh","swo","yft"))

### Add assessment areas ----
dat <- setup_IO_regions(prepdat, regY=TRUE, regY1=FALSE, regY2=TRUE, regY3=TRUE, regB=TRUE, regB1=TRUE, regB2=TRUE, regB3=TRUE, regB4=TRUE, regA=FALSE, regA1=FALSE, regA2=FALSE, regA3=FALSE, regA4=FALSE, regA5=FALSE)

### Save the data
save(dat,file="../data/SYdat.RData")

### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### DATA CLUSTERING ----

projdir <- "~/IOTC/2019_CPUE_tropical/"
natdir <- paste0(projdir, "SY/")
datadir1 <- paste0(natdir, "data/catcheffort/")
analysis_dir <- paste0(natdir, "analyses/")
figdir <- paste0(natdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
clustdir <- paste0(natdir, "clustering/")

dir.create(clustdir)

packages=c('tidyverse', 'openxlsx','knitr','date','splines','maps','mapdata','maptools','lunar','lubridate','mgcv','randomForest','nFactors','data.table','cluster','boot','beanplot','influ','rgdal','RColorBrewer','scales','tm','proto')
sapply(packages,function(x) {if (!x %in% installed.packages()) install.packages(x,repos = 'https://pbil.univ-lyon1.fr/CRAN/')})
invisible(lapply(packages, require, character.only=TRUE, quietly = TRUE, warn.conflicts = FALSE))

library("cpue.rfmo")

### Load the data ----
load(file=paste0(datadir,"SYdat.RData"))
str(dat)

### Set the working directory ----
setwd(clustdir)

# Set up input variables for clustering and standardization
# Generate a group of billfish (mls = striped marlin, bum = blue marlin, blm = black marlin, bxq = marlins nei)
dat$bll <- apply(dat[,c('mls','bum','blm','bxq')],1,sum,na.rm=T)

### Set up input variables for clustering and standardization
summary(dat)
sy_splist <-  c("alb","bet","yft","swo","mls","bum","blm","sbf","skh","bxq","sfa","sbf","rsk","oil","mzz","bsh","bll")

### Plot the mean catch per year of each species by region, to use when deciding which species to cluster
plot_spfreqyq(indat = dat, reg_struc = "regY", splist = sy_splist, flag = "SY", mfr = c(5,4))
plot_spfreqyq(indat = dat, reg_struc = "regY2", splist = sy_splist, flag = "SY", mfr = c(5,4))

# Put chosen species here
cl_splist <- c("alb","bet","yft","swo","oil","mzz","bll")

# Variables to use
dat$hbf <- 0 # Not used for anything, but needed to make the standardization code work.
regnames <- names(dat)[grep("reg", names(dat))]
allabs <- c("op_yr","op_mon","vessid","yrqtr","latlong","hooks","hbf","tripidmon","moon",cl_splist,"Total","lat","lon","lat5","lon5",regnames)
str(dat[,allabs])
#allabs[!(allabs %in% names(dat))]

### Determine the number of clusters. Come back and edit this
reglist <- list()
reglist$regY <-  list(allreg = c(2:5), ncl = c(3,4,4,4,4))
reglist$regY2 <- list(allreg = c(2,7), ncl = c(3,4,5,4,3,5,4))
reglist$regY3 <- list(allreg = c(1), ncl = c(4))
reglist$regB2 <- list(allreg = c(1:4), ncl = c(4,4,5,4))
reglist$regB3 <- list(allreg = c(1,5), ncl = c(4,4,5,4,4))
reglist$regB4 <- list(allreg = c(1), ncl = c(5))
flag="SY"

### Covariates to pass to next stage
cvn <- c("yrqtr","latlong","hooks","hbf","vessid","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")
### r parameter?
r <- 4

# Do the clustering and save the results for later (we also need to decide on the ALB regional structures below)

### Read the cluster functions new version
#source("../../../../../R/cluster_functions.R")

dorg <- c("regY", "regY2", "regY3", "regB2", "regB3", "regB4")
for (rg in dorg) {
  run_clustercode_byreg(indat=dat, reg_struc = rg, allsp=cl_splist, allabs=allabs, flag=flag, cvnames = cvn, rgl=reglist)
}

### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### CPUE STANDARDISATION ----

### --- Paths ----
projdir <- "~/IOTC/2019_CPUE_tropical/"
natdir <- paste0(projdir,"SY/")
datadir1 <- paste0(natdir,"data/catcheffort/")
analysis_dir <- paste0(natdir,"analyses/")
figdir <- paste0(natdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
clustdir <- paste0(natdir, "clustering/")
setwd(analysis_dir)

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

