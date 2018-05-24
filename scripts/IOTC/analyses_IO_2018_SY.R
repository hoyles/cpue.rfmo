projdir <- "~/IOTC/2017_CPUE/"
sydir <- paste0(projdir, "SY/")
datadir1 <- paste0(sydir, "data/catch_effort/")
syalysis_dir <- paste0(sydir, "analyses/")
syfigs <- paste0(sydir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
setwd(syalysis_dir)
# Install any missing packages
#install.packages("date")
#install.packages("maps")
#install.packages("mapdata")
#install.packages("maptools")
#install.packages("data.table")
#install.packages("lunar")
#install.packages("dtplyr")
#install.packages("tm")
#install.packages("devtools")
#devtools::install_github("hadley/readr")

# Load packages
library("date")
library(splines)
library("maps")
library("mapdata")
library("maptools")
library("data.table")
library("lunar")
library(lubridate)
library(readr)
library(plyr)
library(dplyr)
library(dtplyr)
library(tm)
library(devtools)

# The new library 'cpue.rfmo' replaces the 'support functions.r' file.
# The command 'install_github("hoyles/cpue.rfmo")' should install cpue.rfmo, but currently doesn't work,
# possibly due to a bug in devtools with private github repositories, or something else, I'm still looking.
# The current workaround is either:
# a) download cpue.rfmo from github and compile it into a package, following the instructions here:
# http://kbroman.org/pkg_primer/pages/build.html. This is the best approach; or
# b) download cpue.rfmo from github, and install from the binary package (cpue.rfmo_0.1.0.zip) in the top dir.
# Check first that cpue.rfmo has been recompiled to match the latest source code, which may not be the case.
# c) ask me to email you a copy of the binary package, then install it.

library(cpue.rfmo) # This will produce warnings (usually 19) but they can be ignored.

##################

# Load data.


# ===================================================================================
# Start the analysis proper
# ===================================================================================
#Clustering

library("date")
library("lubridate")
library("maps")
library("mapdata")
library("lunar")
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

projdir <- "~/IOTC/2017_CPUE/"
sydir <- paste0(projdir, "SY/")
datadir1 <- paste0(sydir, "data/")
syalysis_dir <- paste0(sydir, "analyses/")
Rdir <- paste0(projdir, "Rfiles/")
clusdir <- paste0(sydir, "clustering/")
setwd(clusdir)

load(file=paste0(syalysis_dir,"SYdat.RData"))
source(paste0(Rdir,"support_functions.r"))
str(dat)

rm(dat2,prepdat,prepdat1,pd1,pd2,clndat,dat5214,rawdat,dataset,llv,dat9415b,dat9415hd,a5,lnk,a2,a0,a)

# Set up input variables for clustering and standardization
dat <- data.frame(dat)
sy_splist <-  c("alb","bet","yft","swo","mls","bum","blm","sbt","sas","shk")

# Plot the mean catch per year of each species by region, to use when deciding which species to cluster
plot_spfreqyq(indat = dat, reg_struc = "regY2", splist = sy_splist, flag = "SY", mfr = c(4,3))
plot_spfreqyq(indat = dat, reg_struc = "regA4", splist = sy_splist, flag = "SY", mfr = c(4,3))

# Put chosen species here
use_splist <- c("alb","bet","yft","swo","mls","bum","blm","sbt","sas")
# Variables to use
allabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tripid","tripidmon","lbid_mon","moon",use_splist,"Total","dmy","lat","lon","lat5","lon5","regY","regY1","regY2","regB","regB1","regB2","regA","regA1","regA2","regA3")
str(dat[,allabs])

# Determine the number of clusters. Come back and edit this.
reglist <- list()
reglist$regA4 <- list(allreg = 1:4, ncl = c(4,4,3,5))
reglist$regA5 <- list(allreg = 1,   ncl = 5)
reglist$regB2 <- list(allreg = 1:4, ncl = c(5,5,4,4))
reglist$regB3 <- list(allreg = 1:5, ncl = c(5,5,4,4,5))
reglist$regY <-  list(allreg = 1:6, ncl = c(4,5,4,3,5,1))
reglist$regY2 <- list(allreg = 1:7, ncl = c(4,5,4,3,5,5,5))
flag="SY"

# Covariates to pass to next stage
cvn <- c("yrqtr","latlong","hooks","hbf","vessid","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")
r=4

# Do the clustering and save the results for later (we also need to decide on the ALB regional structures below)
run_clustercode_byreg(indat=dat, reg_struc = "regA4", allsp=use_sp, allabs=allabs, flag=flag, cvnames = cvn)
run_clustercode_byreg(indat=dat, reg_struc = "regA5", allsp=use_sp, allabs=allabs, flag=flag, cvnames = cvn)
run_clustercode_byreg(indat=dat, reg_struc = "regB3", allsp=use_sp, allabs=allabs, flag=flag, cvnames = cvn)
run_clustercode_byreg(indat=dat, reg_struc = "regY2", allsp=use_sp, allabs=allabs, flag=flag, cvnames = cvn)


# ========================================================
# Standardizations, Seychelles only
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

projdir <- "~/IOTC/2017_CPUE/"
sydir <- paste0(projdir, "SY/")
datadir <- paste0(sydir, "data/")
syalysis_dir <- paste0(sydir, "analyses/")
syfigs <- paste0(sydir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
clustdir <- paste0(sydir,"clustering/")

# Define the clusters to be used. Will need to set this up after checking the cluster allocations
clkeepJP_A5 <- list("alb"=list(c(2,4)))
clkeepKR_A5 <- list("alb"=list(c(5)))
clkeepTW_A5 <- list("alb"=list(c(1,2,4)))

clkeepJP_Y <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(1,2,5),c(1,2,3,4),c(0)))
clkeepKR_Y <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(2,3,5),c(1,2,3,4),c(0)))
clkeepTW_Y <- list("yft"=list(c(0),c(1,2,3,4,5),c(1,2,3),c(1,2),c(1,2,3,4,5)),c(0))
clkeepSY_Y <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(2),c(1,2,3,4),c(0)))
clk_Y <- list(JP=clkeepJP_Y,KR=clkeepKR_Y,TW=clkeepTW_Y,SY=clkeepSY_Y)

clkeepJP_B2 <- list("bet"=list(c(1,2,3,4,5),c(1,2,3,4,5),c(1,2,3,4),c(1,2,3,4)))
clkeepKR_B2 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepTW_B2 <- list("bet"=list(c(1,2,3,4,5),c(1,2,3,4,5),c(2,3),c(1,2,3,4)))
clkeepSY_B2 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2),c(1,2,4)))
clk_B2 <- list(JP=clkeepJP_B2, KR=clkeepKR_B2, TW=clkeepTW_B2, SY=clkeepSY_B2)

clkeepJP_Y2 <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(1,2,5),c(1,2,3,4),c(0),c(1,2,3,4)))
clkeepKR_Y2 <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(2,3,5),c(1,2,3,4),c(0),c(1,2,3,4)))
clkeepTW_Y2 <- list("yft"=list(c(0),c(1,2,3,4,5),c(1,2,3),c(1,2),c(1,2,3,4,5),c(0),c(1,2,3,4,5)))
clkeepSY_Y2 <- list("yft"=list(c(0),c(1,2,3,4),c(1,2,3),c(2),c(1,2,3,4),c(0),c(1,2,3,4)))
clk_Y2 <- list(JP=clkeepJP_Y2,KR=clkeepKR_Y2,TW=clkeepTW_Y2,SY=clkeepSY_Y2)

clkeepJP_B3 <- list("bet"=list(c(1,2,3,4,5),c(1,2,3,4,5),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4,5)))
clkeepKR_B3 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepTW_B3 <- list("bet"=list(c(1,2,3,4,5),c(1,2,3,4,5),c(2,3),c(1,2,3,4),c(1,2,3,4,5)))
clkeepSY_B3 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2),c(1,2,4),c(1,2,3,4)))
clk_B3 <- list(JP=clkeepJP_B3,KR=clkeepKR_B3,TW=clkeepTW_B3,SY=clkeepSY_B3)

minqtrs_Y  <- c(1,8,2,2,5,1)
minqtrs_Y2  <- c(1,7,2,2,5,1,7)
minqtrs_B2 <- c(8,8,2,2)
minqtrs_B3 <- c(7,8,2,2,7)

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

