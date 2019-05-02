# Joint standardization
########################################################
# Remove TW before 2005
# Joint standardization

projdir <- "~/IOTC/2019_CPUE_tropical/"
Rdir <- paste0(projdir, "Rfiles/")

jpdir <- paste0(projdir, "JP/")
krdir <- paste0(projdir, "KR/")
sydir <- paste0(projdir, "SY/")
twdir <- paste0(projdir, "TW/")
jointdir <- paste0(projdir, "joint/")

jntalysis_dir <- paste0(jointdir, "analyses/")
dir.create(jntalysis_dir)

### install.packages("../../../../../influ_0.8.zip", repos = NULL, type = "win.binary")
library("influ",quietly = TRUE) # downloaded here (https://github.com/trophia/influ/releases/) after installing 'proto'

packages=c('tidyverse', 'openxlsx','knitr','date','splines','maps','mapdata','maptools','lunar','lubridate','mgcv','randomForest','nFactors','data.table','cluster','fastcluster','boot','beanplot','influ','rgdal','RColorBrewer','scales','tm','proto')
sapply(packages,function(x) {if (!x %in% installed.packages()) install.packages(x,repos = 'https://pbil.univ-lyon1.fr/CRAN/')})
invisible(lapply(packages, library, character.only=TRUE, quietly = TRUE, warn.conflicts = FALSE))

# The command 'install_github("hoyles/cpue.rfmo", auth_token = 'xxxxxxxxxxxxxxxxx')' should now install cpue.rfmo succcessfully.
# You'll need to generate your own github personal access token. See https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line. You also need to set the scope of the token to have full control of private repositories. Do this on the page where you generate the token.

### Library developed by Simon
### Built from the github sudo R CMD build
#install.packages("../../../../../cpue.rfmo_0.1.0.zip",repos = NULL,type = "win.binary")
library(cpue.rfmo)

clkeepKR_B2 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4,5,6),c(1,2,3,4),c(1,3,4)))
clkeepKR_B3 <- list("bet"=list(c(1,2,3,4),c(0),c(0),c(0),c(1,2,3,4)))
clkeepKR_B4 <- list("bet"=list(c(1,2,3,4)))
clkeepKR_Y <- list("yft"=list(c(1,2,3,4,5),c(1,2,3,4),c(1,2,3,4),c(2,4),c(1,2,3,4),c(1,2,3,4)))
clkeepKR_Y2 <- list("yft"=list(c(0),c(1,2,3,4),c(0),c(0),c(0),c(0),c(1,2,3,4)))
clkeepKR_Y3 <- list("yft"=list(c(1,2,3,4)))

clkeepSY_B2 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4,5),c(1,2,3)))
clkeepSY_B3 <- list("bet"=list(c(1,2,3,4),c(0),c(0),c(0),c(1,2,3,4)))
clkeepSY_B4 <- list("bet"=list(c(1,2,3,4,5)))
clkeepSY_Y  <- list("yft"=list(c(1,2,3,4),c(1,2,3,4),c(1,2,4),c(1,4),c(1,2,3),c(0)))
clkeepSY_Y2 <- list("yft"=list(c(0),c(1,2,3,4),c(0),c(0),c(0),c(0),c(1,2,3,4)))
clkeepSY_Y3 <- list("yft"=list(c(1,2,3)))

clkeepJP_B2 <- list("bet"=list(c(1,2,3,4), c(1,2,3,4), c(1,2,3,4), c(1,2,3,4)))
clkeepJP_B3 <- list("bet"=list(c(1,2,3,4), c(0), c(0), c(0), c(1,2,3,4)))
clkeepJP_B4 <- list("bet"=list(c(1,2,3,4,5)))
clkeepJP_Y  <- list("yft"=list(c(1,2,3,4), c(1,2,3,4), c(1,3,4), c(3,4), c(1,2,3,4),c(1,2,3,4)))
clkeepJP_Y2 <- list("yft"=list(c(0),c(1,2,3,4,5),c(03),c(0),c(0),c(0),c(1,2,3,4)))
clkeepJP_Y3 <- list("yft"=list(c(1,2,3,5)))

clkeepTW_B2 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(2),c(2)))
clkeepTW_B3 <- list("bet"=list(c(1,2,3,4),c(0),c(0),c(0),c(1,2,3,4)))
clkeepTW_B4 <- list("bet"=list(c(2,3,4)))
clkeepTW_Y <- list("yft"=list(c(1,2,3,4,5), c(1,2,3,4), c(2), c(3,4), c(1,2,3,4,5), c(1,2,3,4,5)))
clkeepTW_Y2 <- list("yft"=list(c(0), c(1,2,3,4),c(0), c(0), c(0), c(0), c(1,2,3,4)))
clkeepTW_Y3 <- list("yft"=list(c(1,2,3,4,5)))

clk_Y  <- list(JP=clkeepJP_Y, KR=clkeepKR_Y, TW=clkeepTW_Y, SY=clkeepSY_Y)
clk_Y2 <- list(JP=clkeepJP_Y2,KR=clkeepKR_Y2,TW=clkeepTW_Y2,SY=clkeepSY_Y2)
clk_Y3 <- list(JP=clkeepJP_Y3,KR=clkeepKR_Y3,TW=clkeepTW_Y3,SY=clkeepSY_Y3)
clk_B2 <- list(JP=clkeepJP_B2,KR=clkeepKR_B2,TW=clkeepTW_B2,SY=clkeepSY_B2)
clk_B3 <- list(JP=clkeepJP_B3,KR=clkeepKR_B3,TW=clkeepTW_B3,SY=clkeepSY_B3)
clk_B4 <- list(JP=clkeepJP_B4,KR=clkeepKR_B4,TW=clkeepTW_B4,SY=clkeepSY_B4)

std_splist <- c("alb","bet","yft")
stdlabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks",std_splist,"lat","lon","lat5","lon5","reg","hcltrp","flag")

# The runpars define the approach to be used in this run
regB2_minss <- list(minq_byreg = c(5,5,5,3), minvess=c(100,100,100,60), minll=c(50,50,50,30), minyrqtr = c(50,50,50,30), minyqll = c(5,5,5,3))
regB3_minss <- list(minq_byreg = c(5,5,5,3,5), minvess=c(100,100,100,60,100), minll=c(50,50,50,30,50), minyrqtr = c(50,50,50,30,50), minyqll = c(5,5,5,3,5))
regY_minss <-  list(minq_byreg = c(2,5,5,2,5,2),   minvess=c(40,100,100,40,100,40),     minll=c(20,50,50,20,50,20),    minyrqtr = c(20,50,50,20,50,20),    minyqll = c(3,5,5,3,5,3))
regY2_minss <- list(minq_byreg = c(2,5,5,2,5,2,5), minvess=c(40,100,100,40,100,40,100), minll=c(20,50,50,20,50,20,50), minyrqtr = c(20,50,50,20,50,20,50), minyqll = c(3,5,5,3,5,3,5))

## ---------------------------------------------
# Run various standardization scenarios.
## ---------------------------------------------

######## tropical_and_temperate, with clusters, with hbf, with hooks ################
runpars <- list()
runpars[["regY"]] <-list(runsp = "yft", regtype2 = "Y", clk = clk_Y, doregs = 2:6,
                         addcl = TRUE, dohbf = TRUE, dohook = TRUE,
                         do_lognC = TRUE, do_deltalog = TRUE,
                         do_early = TRUE, do_late = TRUE, do_vessallyr = FALSE,
                         cltype = "hcltrp", minss = regY_minss, strsmp = 10)
runpars[["regY2"]] <-list(runsp = "yft", regtype2 = "Y2", clk = clk_Y2, doregs = c(2,7),
                         addcl = TRUE, dohbf = TRUE, dohook = TRUE,
                         do_lognC = TRUE, do_deltalog = TRUE,
                         do_early = TRUE, do_late = TRUE, do_vessallyr = FALSE,
                         cltype = "hcltrp", minss = regY2_minss, strsmp = 10)
runpars[["regB2"]] <-list(runsp = "bet", regtype2 = "B2", clk = clk_B2, doregs = 1:4,
                         addcl = TRUE, dohbf = TRUE, dohook = TRUE,
                         do_lognC = TRUE, do_deltalog = TRUE,
                         do_early = TRUE, do_late = TRUE, do_vessallyr = FALSE,
                         cltype = "hcltrp", minss = regB2_minss, strsmp = 10)
runpars[["regB3"]] <-list(runsp = "bet", regtype2 = "B3", clk = clk_B3, doregs = c(1,5),
                         addcl = TRUE, dohbf = TRUE, dohook = TRUE,
                         do_lognC = TRUE, do_deltalog = TRUE,
                         do_early = TRUE, do_late = TRUE, do_vessallyr = FALSE,
                         cltype = "hcltrp", minss = regB3_minss, strsmp = 10)


regstr <- "regY"; runreg <- 2; keepd <- TRUE; doflags <- "TW"
maxyr <- 2019

resdir <- paste0(jntalysis_dir,"trop_temp_cl1_hb1_hk1_TW2005/")
dir.create(resdir)
setwd(resdir)

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regY", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regY2", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regB2", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regB3", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

######---------------------------------------------------------------------------------
######## tropical, no clusters, with hbf, with hooks, all TW post-2005 ################
resdir <- paste0(jntalysis_dir,"trop_cl0_hb1_hk1_TW2005/")
dir.create(resdir)
setwd(resdir)

runpars <- list()
runpars[["regY"]] <-list(runsp = "yft", regtype2 = "Y", clk = clk_Y, doregs = c(2,5),
                         addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                         do_lognC = TRUE, do_deltalog = TRUE,
                         do_early = TRUE, do_late = TRUE, do_vessallyr = FALSE,
                         cltype = "hcltrp", minss = regY_minss, strsmp = 15)
runpars[["regY2"]] <-list(runsp = "yft", regtype2 = "Y2", clk = clk_Y2, doregs = c(2,7),
                          addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                          do_lognC = TRUE, do_deltalog = TRUE,
                          do_early = TRUE, do_late = TRUE, do_vessallyr = FALSE,
                          cltype = "hcltrp", minss = regY2_minss, strsmp = 15)
runpars[["regB2"]] <-list(runsp = "bet", regtype2 = "B2", clk = clk_B2, doregs = c(1,2),
                          addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                          do_lognC = TRUE, do_deltalog = TRUE,
                          do_early = TRUE, do_late = TRUE, do_vessallyr = FALSE,
                          cltype = "hcltrp", minss = regB2_minss, strsmp = 15)
runpars[["regB3"]] <-list(runsp = "bet", regtype2 = "B3", clk = clk_B3, doregs = c(1,5),
                          addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                          do_lognC = TRUE, do_deltalog = TRUE,
                          do_early = TRUE, do_late = TRUE, do_vessallyr = FALSE,
                          cltype = "hcltrp", minss = regB3_minss, strsmp = 15)

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regY", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regY2", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regB2", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regB3", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

######## temperate, no clusters, with hbf, with hooks, all TW post-2005 ################
resdir <- paste0(jntalysis_dir,"temp_cl0_hb1_hk1_TW2005/")
dir.create(resdir)
setwd(resdir)

runpars <- list()
runpars[["regY"]] <-list(runsp = "yft", regtype2 = "Y", clk = clk_Y, doregs = c(3,4),
                         addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                         do_lognC = TRUE, do_deltalog = TRUE,
                         do_early = TRUE, do_late = TRUE, do_vessallyr = FALSE,
                         cltype = "hcltrp", minss = regY_minss, strsmp = 15)
runpars[["regB2"]] <-list(runsp = "bet", regtype2 = "B2", clk = clk_B2, doregs = c(3,4),
                          addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                          do_lognC = TRUE, do_deltalog = TRUE,
                          do_early = TRUE, do_late = TRUE, do_vessallyr = FALSE,
                          cltype = "hcltrp", minss = regB2_minss, strsmp = 15)

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regY", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regB2", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

######## temperate, with clusters, no hbf, with hooks, all TW post-2005 ################
resdir <- paste0(jntalysis_dir,"temp_cl1_hb0_hk1_TW2005/")
dir.create(resdir)
setwd(resdir)

runpars <- list()
runpars[["regY"]] <-list(runsp = "yft", regtype2 = "Y", clk = clk_Y, doregs = c(3,4),
                         addcl = TRUE, dohbf = FALSE, dohook = TRUE,
                         do_lognC = TRUE, do_deltalog = TRUE,
                         do_early = TRUE, do_late = TRUE, do_vessallyr = FALSE,
                         cltype = "hcltrp", minss = regY_minss, strsmp = 15)
runpars[["regB2"]] <-list(runsp = "bet", regtype2 = "B2", clk = clk_B2, doregs = c(3,4),
                          addcl = TRUE, dohbf = FALSE, dohook = TRUE,
                          do_lognC = TRUE, do_deltalog = TRUE,
                          do_early = TRUE, do_late = TRUE, do_vessallyr = FALSE,
                          cltype = "hcltrp", minss = regB2_minss, strsmp = 15)

run_standardization(runpars, doflags = c("JP","KR","TW","SY"), regstr = "regY", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW","SY"), regstr = "regB2", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

######---------------------------------------------------------------------------------
######## tropical, no clusters, with hbf, with hooks, TW 2005-2016 ################
resdir <- paste0(jntalysis_dir,"trop_cl0_hb1_hk1_TW0516/")
dir.create(resdir)
setwd(resdir)

dat_lims <- c("a$yrqtr > 2005 | a$flag != 'TW'","a$yrqtr < 2017 | a$flag != 'TW'")

runpars <- list()
runpars[["regY"]] <-list(runsp = "yft", regtype2 = "Y", clk = clk_Y, doregs = c(2,5),
                         addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                         do_lognC = TRUE,do_deltalog=TRUE,do_early=TRUE,do_late=TRUE,do_vessallyr=FALSE,
                         dat_lims = dat_lims, cltype = "hcltrp", minss = regY_minss, strsmp = 15)
runpars[["regY2"]] <-list(runsp = "yft", regtype2 = "Y2", clk = clk_Y2, doregs = c(2,7),
                          addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                          do_lognC = TRUE,do_deltalog=TRUE,do_early=TRUE,do_late=TRUE,do_vessallyr=FALSE,
                          dat_lims = dat_lims, cltype = "hcltrp", minss = regY2_minss, strsmp = 15)
runpars[["regB2"]] <-list(runsp = "bet", regtype2 = "B2", clk = clk_B2, doregs = c(1,2),
                          addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                          do_lognC = TRUE,do_deltalog=TRUE,do_early=TRUE,do_late=TRUE,do_vessallyr=FALSE,
                          dat_lims = dat_lims, cltype = "hcltrp", minss = regB2_minss, strsmp = 15)
runpars[["regB3"]] <-list(runsp = "bet", regtype2 = "B3", clk = clk_B3, doregs = c(1,5),
                          addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                          do_lognC = TRUE,do_deltalog=TRUE,do_early=TRUE,do_late=TRUE,do_vessallyr=FALSE,
                          dat_lims = dat_lims, cltype = "hcltrp", minss = regB3_minss, strsmp = 15)

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regY", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW","SY"), regstr = "regY2", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW","SY"), regstr = "regB2", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW","SY"), regstr = "regB3", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

######## temperate, no clusters, with hbf, with hooks, TW 2005-2016 ################
resdir <- paste0(jntalysis_dir,"temp_cl0_hb1_hk1_TW0516/")
dir.create(resdir)
setwd(resdir)

dat_lims <- data.frame(flag = "TW", yrqtr = c("> 2005", "< 2017"), reg = NA)

runpars <- list()
runpars[["regY"]] <-list(runsp = "yft", regtype2 = "Y", clk = clk_Y, doregs = c(3,4),
                         addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                         do_lognC = TRUE,do_deltalog=TRUE,do_early=TRUE,do_late=TRUE,do_vessallyr=FALSE,
                         dat_lims = dat_lims, cltype = "hcltrp", minss = regY_minss, strsmp = 15)
runpars[["regB2"]] <-list(runsp = "bet", regtype2 = "B2", clk = clk_B2, doregs = c(3,4),
                          addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                          do_lognC = TRUE,do_deltalog=TRUE,do_early=TRUE,do_late=TRUE,do_vessallyr=FALSE,
                          dat_lims = dat_lims, cltype = "hcltrp", minss = regB2_minss, strsmp = 15)

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regY", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regB2", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

######## temperate, with clusters, no hbf, with hooks, TW 2005-2016 ################
resdir <- paste0(jntalysis_dir,"temp_cl1_hb0_hk1_TW0516/")
dir.create(resdir)
setwd(resdir)

dat_lims <- data.frame(flag = "TW", yrqtr = c("> 2005", "< 2017"), reg = NA)

runpars <- list()
runpars[["regY"]] <-list(runsp = "yft", regtype2 = "Y", clk = clk_Y, doregs = c(3,4),
                         addcl = TRUE, dohbf = FALSE, dohook = TRUE,
                         do_lognC = TRUE,do_deltalog=TRUE,do_early=TRUE,do_late=TRUE,do_vessallyr=FALSE,
                         dat_lims = dat_lims, cltype = "hcltrp", minss = regY_minss, strsmp = 15)
runpars[["regB2"]] <-list(runsp = "bet", regtype2 = "B2", clk = clk_B2, doregs = c(3,4),
                          addcl = TRUE, dohbf = FALSE, dohook = TRUE,
                          do_lognC = TRUE,do_deltalog=TRUE,do_early=TRUE,do_late=TRUE,do_vessallyr=FALSE,
                          dat_lims = dat_lims, cltype = "hcltrp", minss = regB2_minss, strsmp = 15)

run_standardization(runpars, doflags = c("JP","KR","TW","SY"), regstr = "regY", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW","SY"), regstr = "regB2", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

######---------------------------------------------------------------------------------
######## tropical, no clusters, with hbf, with hooks, TW2005, discards ################
resdir <- paste0(jntalysis_dir,"trop_cl0_hb1_hk1_TW2005_discard/")
dir.create(resdir)
setwd(resdir)

disc_file <- list()
disc_file$yft <- readxl::read_xlsx("../../../docs/TW_discards_byregion.xlsx", sheet="yft")
disc_file$bet <- readxl::read_xlsx("../../../docs/TW_discards_byregion.xlsx", sheet="bet")
a <- rbind(disc_file$bet[disc_file$bet$reg %in% c(1,5),],disc_file$yft[disc_file$yft$reg %in% c(1,5),])
a <- aggregate(cbind(retain, discard) ~ flag + year + regstr, data = a, FUN = sum)
a$rate <- a$discard / (a$discard + a$retain)
a[a$regstr=="regB3",]$regstr <- "regB2"
a$reg[a$regstr=="regB2"] <- 1
a[a$regstr=="regY2",]$regstr <- "regY"
a$reg[a$regstr=="regY"] <- 2
discards <- rbind(disc_file$yft, disc_file$bet, a)

runpars <- list()
runpars[["regY"]] <-list(runsp = "yft", regtype2 = "Y", clk = clk_Y, doregs = c(2,5),
                         addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                         do_lognC = TRUE,do_deltalog=TRUE,do_early=TRUE,do_late=TRUE,do_vessallyr=FALSE,
                         dat_lims = NA, cltype = "hcltrp", minss = regY_minss, strsmp = 15, discards = discards)
runpars[["regY2"]] <-list(runsp = "yft", regtype2 = "Y2", clk = clk_Y2, doregs = c(2,7),
                          addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                          do_lognC = TRUE,do_deltalog=TRUE,do_early=TRUE,do_late=TRUE,do_vessallyr=FALSE,
                          dat_lims = NA, cltype = "hcltrp", minss = regY2_minss, strsmp = 15, discards = discards)
runpars[["regB2"]] <-list(runsp = "bet", regtype2 = "B2", clk = clk_B2, doregs = c(1,2),
                          addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                          do_lognC = TRUE,do_deltalog=TRUE,do_early=TRUE,do_late=TRUE,do_vessallyr=FALSE,
                          dat_lims = NA, cltype = "hcltrp", minss = regB2_minss, strsmp = 15, discards = discards)
runpars[["regB3"]] <-list(runsp = "bet", regtype2 = "B3", clk = clk_B3, doregs = c(1,5),
                          addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                          do_lognC = TRUE,do_deltalog=TRUE,do_early=TRUE,do_late=TRUE,do_vessallyr=FALSE,
                          dat_lims = NA, cltype = "hcltrp", minss = regB3_minss, strsmp = 15, discards = discards)

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regY", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regY2", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regB2", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regB3", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

######## temperate, no clusters, with hbf, with hooks, TW2005, discards ################
resdir <- paste0(jntalysis_dir,"temp_cl0_hb1_hk1_TW2005_discard/")
dir.create(resdir)
setwd(resdir)

runpars <- list()
runpars[["regY"]] <-list(runsp = "yft", regtype2 = "Y", clk = clk_Y, doregs = c(3,4),
                         addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                         do_lognC = TRUE,do_deltalog=TRUE,do_early=TRUE,do_late=TRUE,do_vessallyr=FALSE,
                         dat_lims = NA, cltype = "hcltrp", minss = regY_minss, strsmp = 15, discards = discards)
runpars[["regB2"]] <-list(runsp = "bet", regtype2 = "B2", clk = clk_B2, doregs = c(3,4),
                          addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                          do_lognC = TRUE,do_deltalog=TRUE,do_early=TRUE,do_late=TRUE,do_vessallyr=FALSE,
                          dat_lims = NA, cltype = "hcltrp", minss = regB2_minss, strsmp = 15, discards = discards)

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regY", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regB2", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))


######## temperate, with clusters, no hbf, with hooks, TW2005, discards ################
resdir <- paste0(jntalysis_dir,"temp_cl1_hb0_hk1_TW2005_discard/")
dir.create(resdir)
setwd(resdir)

runpars <- list()
runpars[["regY"]] <-list(runsp = "yft", regtype2 = "Y", clk = clk_Y, doregs = c(3,4),
                         addcl = TRUE, dohbf = FALSE, dohook = TRUE,
                         do_lognC = TRUE,do_deltalog=TRUE,do_early=TRUE,do_late=TRUE,do_vessallyr=FALSE,
                         dat_lims = NA, cltype = "hcltrp", minss = regY_minss, strsmp = 15, discards = discards)
runpars[["regB2"]] <-list(runsp = "bet", regtype2 = "B2", clk = clk_B2, doregs = c(3,4),
                          addcl = TRUE, dohbf = FALSE, dohook = TRUE,
                          do_lognC = TRUE,do_deltalog=TRUE,do_early=TRUE,do_late=TRUE,do_vessallyr=FALSE,
                          dat_lims = NA, cltype = "hcltrp", minss = regB2_minss, strsmp = 15, discards = discards)

run_standardization(runpars, doflags = c("JP","KR","TW","SY"), regstr = "regY", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))

run_standardization(runpars, doflags = c("JP","KR","TW","SY"), regstr = "regB2", maxyr = 2019, stdlabs = stdlabs, projdir = projdir, twlimit=2005 , jplimit = list(reg=2, yr=3005))


##################################################################################################
###### Get medians and make newdats ############

flaglist <- list(jp="JP",kr="KR",tw="TW",sy="SY",all=c("JP","KR","TW","SY"))
reglist <- list("regB2" = c(1,2,3,4),"regY" = c(2,3,4,5),"regY2" = c(2,7),"regB3" = c(1,5))

datmeds <- list()
for(regtype in c("regB2","regY","regY2","regB3")) {
  print(regtype)
  datmeds[[regtype]] <- regtype
  runsp <- switch(regtype,regB2="bet",regY="yft",regY2="yft",regB3="bet")
  clk <- switch(regtype,regB2=clk_B2,regY=clk_Y,regY2=clk_Y2,regB3=clk_B3)
  jdat <- data.frame()
  for(flag in c("JP", "KR", "TW", "SY")) {
    for(r in runpars[[runsp]]$doregs) {
      load(paste0(projdir,flag,"/clustering/",paste(flag,regtype,r,sep="_"),".RData"))
      dataset$flag <- flag
      dataset$qtr <- revtrunc(defactor(dataset$yrqtr))
      jdat <- rbind(jdat,dataset[,allabs])
      rm(dataset)
    }
  }
  jdat <- jdat[jdat$yrqtr < maxyr,]
  jdat$vessidx <- jdat$vessid
  jdat$vessid <- paste0(jdat$flag,jdat$vessid)
  jdat$vessid <- as.factor(jdat$vessid)
  jdat$lat5 <- as.factor(jdat$lat5)
  jdat$op_yr <- as.factor(jdat$op_yr)
  jdat$qtr <- as.factor(jdat$qtr)
  jdat <- jdat[jdat$yrqtr > 2005 | jdat$flag != "TW",]

  for(fl in  1:5) {
    flagl <- flaglist[[fl]]
    datmeds[[regtype]][[fl]] <- flagl
    print(c(fl, flagl))
    for(runreg in reglist[[regtype]]) {
      dd <- jdat[jdat$flag %in% flagl,]
      minqtrs <- minqtrs_byreg[runreg]
      glmdat <- select_data_JointIO(dd,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,minvess=30,minll=30,minyrqtr=30,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA)
      if(dim(glmdat)[1] > 0) {
        glmdat_meds <- get_base_newdat(glmdat)
        if((length(flagl) > 1 | flagl == "JP")[1]) {
          glmdat5279 <- select_data_JointIO(dd,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars, minvess=30,minll=30,minyrqtr=30,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1952,1980))
          glmdat5279_meds <- get_base_newdat(glmdat5279)
        } else glmdat5279_meds <- ""
        a <- dd[dd$vessid != "JP1",]
        glmdat79nd <- select_data_JointIO(a,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars, minvess=30,minll=30,minyrqtr=30,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1979,maxyr))
        glmdat79nd_meds <- get_base_newdat(glmdat79nd)
        datmeds[[regtype]][[fl]][[runreg]] <- list(flag=flagl, glmdat_meds, glmdat5279_meds, glmdat79nd_meds)
      } else datmeds[[regtype]][[fl]][[runreg]] <- list(flag=flagl, 0,0,0)
    }
  }
}
save(datmeds, file = paste0(projdir, "data_medians.RData"))
