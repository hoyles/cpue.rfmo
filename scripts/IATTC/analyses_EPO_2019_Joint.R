# Joint standardization
########################################################
# Remove TW before 2005
# Joint standardization

projdir <- "~/IATTC/2019_CPUE/"

cndir <- paste0(projdir, "CN/")
krdir <- paste0(projdir, "KR/")
twdir <- paste0(projdir, "TW/")
jpdir <- paste0(projdir, "JP/")
jointdir <- paste0(projdir, "joint/")
dir.create(jointdir)

jntalysis_dir <- paste0(jointdir, "analyses/")
dir.create(jntalysis_dir)

#install.packages("survival")
#install.packages("stringr")
library(stringr)
library("date")
library(splines)
library("maps")
library("mapdata")
library("maptools")
library("lunar")
library("mgcv")
library(randomForest)
library(influ)
library("nFactors")
library(plyr)
library(dplyr)
library(data.table)
library(cluster)
library(beanplot)
library(survival)

library(cpue.rfmo)

#clkeepCN_B <- list("bet" = list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepJP_Bepo <- list("bet" = list(c(1,2,3),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepKR_Bepo <- list("bet" = list(c(1,2,3,4),c(1,2,3),c(1,2,3,4),c(1,2,3,4)))
clkeepTW_Bepo <- list("bet" = list(c(1,2,3),c(1,2),c(1,2),c(1,2)))
clk_Bepo <- list(JP = clkeepJP_Bepo,KR = clkeepKR_Bepo,TW = clkeepTW_Bepo)


short_splist <- c("alb","bet","yft")
stdlabs <- c("vessid","yrqtr","latlong","op_yr","hbf","hooks",short_splist,"lat","lon","lat5","lon5", "reg", "hcltrp", "flag", "moon")

options(error = recover)

## ---------------------------------------------
# Run various standardization scenarios.
## ---------------------------------------------

# The runpars define the approach to be used in this run
regBepo_minss <- list(minq_byreg = c(5,2,5,2), minvess=c(60,30,60,30), minll=c(60,30,60,30), minyrqtr = c(60,30,60,30), minyqll = c(5,3,5,3))

runpars <- list()
runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 1:4, addcl = TRUE, dohbf = FALSE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepo_minss, strsmp = 30)

regstr <- "regBepo"; runreg <- 2; keepd <- TRUE; doflags <- "TW"
maxyr <- 2018

# with clusters, hooks, no hbf
resdir <- paste0(jntalysis_dir,"cl1_hb0_hk1/")
dir.create(resdir)
setwd(resdir)

run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0 , jplimit = list(reg=0, yr=21000))

# with clusters, hooks, hbf
resdir <- paste0(jntalysis_dir,"cl1_hb1_hk1/")
dir.create(resdir)
setwd(resdir)

runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 1:4, addcl = TRUE, dohbf = TRUE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepo_minss, strsmp = 30)
run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=0, yr=21000))

# no clusters, with hooks, hbf
resdir <- paste0(jntalysis_dir,"cl0_hb1_hk1/")
dir.create(resdir)
setwd(resdir)

runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 1:4, addcl = FALSE, dohbf = TRUE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepo_minss, strsmp = 30)
run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=0, yr=21000))

# no clusters, with hooks, no hbf
resdir <- paste0(jntalysis_dir,"cl0_hb0_hk1/")
dir.create(resdir)
setwd(resdir)

runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 1:4, addcl = FALSE, dohbf = FALSE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepo_minss, strsmp = 30)
run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=0, yr=21000))

