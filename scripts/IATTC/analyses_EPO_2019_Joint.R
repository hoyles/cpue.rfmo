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

clkeepJP_BepoY <- list("yft" = list(c(1,2,3),c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepKR_BepoY <- list("yft" = list(c(1,2,3,4),c(1,2,3),c(1,2,3,4),c(1,2,3,4)))
clkeepTW_BepoY <- list("yft" = list(c(1,2,3),c(1,2),c(1,2),c(1,2)))
clk_BepoY <- list(JP = clkeepJP_BepoY, KR = clkeepKR_BepoY, TW = clkeepTW_BepoY)


short_splist <- c("alb","bet","yft")
stdlabs <- c("vessid","yrqtr","latlong","op_yr","hbf","hooks",short_splist,"lat","lon","lat5","lon5", "reg", "hcltrp", "flag", "moon")

options(error = recover)

## ---------------------------------------------
# Run various standardization scenarios.
## ---------------------------------------------

# The runpars define the approach to be used in this run
runpars <- list()
regBepo_minss <- list(minq_byreg = c(5,2,5,2), minvess=c(60,30,60,30), minll=c(60,30,60,30), minyrqtr = c(60,30,60,30), minyqll = c(5,3,5,3))

# with clusters, hooks, no hbf
resdir <- paste0(jntalysis_dir,"cl1_hb0_hk1/")
dir.create(resdir);setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 4:4, ylall = c(1979, 2018), addcl = TRUE, dohbf = FALSE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepo_minss, strsmp = 20)
run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0 , jplimit = list(reg=0, yr=21000), krlimit = list(reg=1, yr=c(1990, 21000)))

# with clusters, hooks, hbf
resdir <- paste0(jntalysis_dir,"cl1_hb1_hk1/")
dir.create(resdir); setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 1:4, ylall = c(1979, 2018), addcl = TRUE, dohbf = TRUE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepo_minss, strsmp = 20)
run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=0, yr=21000), krlimit = list(reg=1, yr=c(1990, 21000)))

# no clusters, with hooks, hbf
resdir <- paste0(jntalysis_dir,"cl0_hb1_hk1/")
dir.create(resdir);setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 1:4, ylall = c(1979, 2018), addcl = FALSE, dohbf = TRUE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepo_minss, strsmp = 20)
run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=0, yr=21000), krlimit = list(reg=1, yr=c(1990, 21000)))

# no clusters, with hooks, no hbf
resdir <- paste0(jntalysis_dir,"cl0_hb0_hk1/")
dir.create(resdir); setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 1:4, ylall = c(1979, 2018), addcl = FALSE, dohbf = FALSE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepo_minss, strsmp = 20)
run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=0, yr=21000), krlimit = list(reg=1, yr=c(1990, 21000)))

# Run Taiwan ###############
twdir <- paste0(projdir, "TW/")
twalysis_dir <- paste0(twdir, "analyses/"); dir.create(twalysis_dir)
runpars <- list()
regBepoTW_minss <- list(minq_byreg = c(2,2,2,2), minvess=c(10,10,10,10), minll=c(10,10,10,10), minyrqtr = c(10,10,10,10), minyqll = c(2,2,2,2))

resdir <- paste0(twalysis_dir,"cl1_hb0_hk1/") # with clusters, hooks, no hbf
dir.create(resdir); setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 1:4, ylall = c(1979, 2018), addcl = TRUE, dohbf = FALSE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepoTW_minss, strsmp = 30)
run_standardization(runpars, doflags = c("TW"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0 , jplimit = list(reg=0, yr=21000))

resdir <- paste0(twalysis_dir,"cl1_hb1_hk1/") # with clusters, hooks, hbf
dir.create(resdir); setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 1:4, ylall = c(1979, 2018), addcl = TRUE, dohbf = TRUE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepoTW_minss, strsmp = 30)
run_standardization(runpars, doflags = c("TW"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=0, yr=21000))

resdir <- paste0(twalysis_dir,"cl0_hb1_hk1/") # no clusters, with hooks, hbf
dir.create(resdir); setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 1:4, ylall = c(1979, 2018), addcl = FALSE, dohbf = TRUE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepoTW_minss, strsmp = 30)
run_standardization(runpars, doflags = c("TW"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=0, yr=21000))

resdir <- paste0(twalysis_dir,"cl0_hb0_hk1/") # no clusters, with hooks, no hbf
dir.create(resdir); setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 1:4, ylall = c(1979, 2018), addcl = FALSE, dohbf = FALSE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepoTW_minss, strsmp = 30)
run_standardization(runpars, doflags = c("TW"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=0, yr=21000))

# Run Korea ##############
krdir <- paste0(projdir, "KR/")
kralysis_dir <- paste0(krdir, "analyses/"); dir.create(kralysis_dir)
runpars <- list()
regBepoKR_minss <- list(minq_byreg = c(2,2,2,2), minvess=c(30,20,20,10), minll=c(30,20,20,10), minyrqtr = c(30,20,20,10), minyqll = c(3,3,3,3))

resdir <- paste0(kralysis_dir,"cl1_hb0_hk1/") # with clusters, hooks, no hbf
dir.create(resdir); setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 1:4, ylall = c(1979, 2018), addcl = TRUE, dohbf = FALSE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepoKR_minss, strsmp = 20)
run_standardization(runpars, doflags = c("KR"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0 , jplimit = list(reg=0, yr=21000))

resdir <- paste0(kralysis_dir,"cl1_hb1_hk1/") # with clusters, hooks, hbf
dir.create(resdir); setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 1:4, ylall = c(1979, 2018), addcl = TRUE, dohbf = TRUE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepoKR_minss, strsmp = 20)
run_standardization(runpars, doflags = c("KR"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=0, yr=21000))

resdir <- paste0(kralysis_dir,"cl0_hb1_hk1/") # no clusters, with hooks, hbf
dir.create(resdir); setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 1:4, ylall = c(1979, 2018), addcl = FALSE, dohbf = TRUE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepoKR_minss, strsmp = 20)
run_standardization(runpars, doflags = c("KR"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=0, yr=21000))

resdir <- paste0(kralysis_dir,"cl0_hb0_hk1/") # no clusters, with hooks, no hbf
dir.create(resdir); setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 1:4, ylall = c(1979, 2018), addcl = FALSE, dohbf = FALSE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepoKR_minss, strsmp = 20)
run_standardization(runpars, doflags = c("KR"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=0, yr=21000))


# Run Japan ###################
jpdir <- paste0(projdir, "JP/")
jpalysis_dir <- paste0(jpdir, "analyses/"); dir.create(jpalysis_dir)
runpars <- list()
regBepo_minss <- list(minq_byreg = c(5,2,5,2), minvess=c(60,30,60,30), minll=c(60,30,60,30), minyrqtr = c(60,30,60,30), minyqll = c(5,3,5,3))
regBepoJP_minss <- list(minq_byreg = c(5,2,5,2), minvess=c(60,30,30,10), minll=c(60,30,30,10), minyrqtr = c(60,50,50,30), minyqll = c(5,3,3,3))

resdir <- paste0(jpalysis_dir,"cl1_hb0_hk1/") # with clusters, hooks, no hbf
dir.create(resdir); setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 1:4, ylall = c(1979, 2018), addcl = TRUE, dohbf = FALSE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepoJP_minss, strsmp = 20)
run_standardization(runpars, doflags = c("JP"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0 , jplimit = list(reg=0, yr=21000))

resdir <- paste0(jpalysis_dir,"cl1_hb1_hk1/") # with clusters, hooks, hbf
dir.create(resdir); setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 1:4, ylall = c(1979, 2018), addcl = TRUE, dohbf = TRUE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepoJP_minss, strsmp = 20)
run_standardization(runpars, doflags = c("JP"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=0, yr=21000))

resdir <- paste0(jpalysis_dir,"cl0_hb1_hk1/") # no clusters, with hooks, hbf
dir.create(resdir); setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 1:4, ylall = c(1979, 2018), addcl = FALSE, dohbf = TRUE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepoJP_minss, strsmp = 20)
run_standardization(runpars, doflags = c("JP"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=0, yr=21000))

resdir <- paste0(jpalysis_dir,"cl0_hb0_hk1/") # no clusters, with hooks, no hbf
dir.create(resdir); setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "bet", regtype2 = "Bepo", clk = clk_Bepo, doregs = 1:4, ylall = c(1979, 2018), addcl = FALSE, dohbf = FALSE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepoJP_minss, strsmp = 20)
run_standardization(runpars, doflags = c("JP"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=0, yr=21000))


# Run YFT ##################

## ---------------------------------------------
# Run various standardization scenarios.
## ---------------------------------------------

# The runpars define the approach to be used in this run
runpars <- list()
regBepo_minss <- list(minq_byreg = c(5,2,5,2), minvess=c(60,30,60,30), minll=c(60,30,60,30), minyrqtr = c(60,30,60,30), minyqll = c(5,3,5,3))

# with clusters, hooks, no hbf
resdir <- paste0(jntalysis_dir,"y_cl1_hb0_hk1/")
dir.create(resdir);setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "yft", regtype2 = "Bepo", clk = clk_BepoY, doregs = 1:4, ylall = c(1979, 2018), addcl = TRUE, dohbf = FALSE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepo_minss, strsmp = 20)
run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0 , jplimit = list(reg=0, yr=21000))

# # no clusters, with hooks, hbf
# resdir <- paste0(jntalysis_dir,"y_cl0_hb1_hk1/")
# dir.create(resdir);setwd(resdir)
# runpars[["regBepo"]] <-list(runsp = "yft", regtype2 = "Bepo", clk = clk_BepoY, doregs = 1:4, ylall = c(1979, 2018), addcl = FALSE, dohbf = TRUE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepo_minss, strsmp = 20)
# run_standardization(runpars, doflags = c("JP","KR","TW"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=0, yr=21000))

# Run Taiwan ###############
twdir <- paste0(projdir, "TW/")
twalysis_dir <- paste0(twdir, "analyses/"); dir.create(twalysis_dir)
runpars <- list()
regBepoTW_minss <- list(minq_byreg = c(2,2,2,2), minvess=c(10,10,10,10), minll=c(10,10,10,10), minyrqtr = c(10,10,10,10), minyqll = c(2,2,2,2))

resdir <- paste0(twalysis_dir,"y_cl1_hb0_hk1/") # with clusters, hooks, no hbf
dir.create(resdir); setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "yft", regtype2 = "Bepo", clk = clk_BepoY, doregs = 1:4, ylall = c(1979, 2018), addcl = TRUE, dohbf = FALSE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepoTW_minss, strsmp = 30)
run_standardization(runpars, doflags = c("TW"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0 , jplimit = list(reg=0, yr=21000))

# resdir <- paste0(twalysis_dir,"y_cl0_hb1_hk1/") # no clusters, with hooks, hbf
# dir.create(resdir); setwd(resdir)
# runpars[["regBepo"]] <-list(runsp = "yft", regtype2 = "Bepo", clk = clk_BepoY, doregs = 1:4, ylall = c(1979, 2018), addcl = FALSE, dohbf = TRUE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepoTW_minss, strsmp = 30)
# run_standardization(runpars, doflags = c("TW"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=0, yr=21000))

# Run Korea ##############
krdir <- paste0(projdir, "KR/")
kralysis_dir <- paste0(krdir, "analyses/"); dir.create(kralysis_dir)
runpars <- list()
regBepoKR_minss <- list(minq_byreg = c(2,2,2,2), minvess=c(30,20,20,10), minll=c(30,20,20,10), minyrqtr = c(30,20,20,10), minyqll = c(3,3,3,3))

resdir <- paste0(kralysis_dir,"y_cl1_hb0_hk1/") # with clusters, hooks, no hbf
dir.create(resdir); setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "yft", regtype2 = "Bepo", clk = clk_BepoY, doregs = 1:4, ylall = c(1979, 2018), addcl = TRUE, dohbf = FALSE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepoKR_minss, strsmp = 20)
run_standardization(runpars, doflags = c("KR"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0 , jplimit = list(reg=0, yr=21000))

# resdir <- paste0(kralysis_dir,"y_cl0_hb1_hk1/") # no clusters, with hooks, hbf
# dir.create(resdir); setwd(resdir)
# runpars[["regBepo"]] <-list(runsp = "yft", regtype2 = "Bepo", clk = clk_BepoY, doregs = 1:4, ylall = c(1979, 2018), addcl = FALSE, dohbf = TRUE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepoKR_minss, strsmp = 20)
# run_standardization(runpars, doflags = c("KR"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=0, yr=21000))


# Run Japan ###################
jpdir <- paste0(projdir, "JP/")
jpalysis_dir <- paste0(jpdir, "analyses/"); dir.create(jpalysis_dir)
runpars <- list()
regBepo_minss <- list(minq_byreg = c(5,2,5,2), minvess=c(60,30,60,30), minll=c(60,30,60,30), minyrqtr = c(60,30,60,30), minyqll = c(5,3,5,3))
regBepoJP_minss <- list(minq_byreg = c(5,2,5,2), minvess=c(60,30,30,10), minll=c(60,30,30,10), minyrqtr = c(60,50,50,30), minyqll = c(5,3,3,3))

resdir <- paste0(jpalysis_dir,"y_cl1_hb0_hk1/") # with clusters, hooks, no hbf
dir.create(resdir); setwd(resdir)
runpars[["regBepo"]] <-list(runsp = "yft", regtype2 = "Bepo", clk = clk_BepoY, doregs = 1:4, ylall = c(1979, 2018), addcl = TRUE, dohbf = FALSE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepoJP_minss, strsmp = 20)
run_standardization(runpars, doflags = c("JP"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0 , jplimit = list(reg=0, yr=21000))

# resdir <- paste0(jpalysis_dir,"y_cl0_hb1_hk1/") # no clusters, with hooks, hbf
# dir.create(resdir); setwd(resdir)
# runpars[["regBepo"]] <-list(runsp = "yft", regtype2 = "Bepo", clk = clk_BepoY, doregs = 1:4, ylall = c(1979, 2018), addcl = FALSE, dohbf = TRUE, dohook = TRUE, rm_hbfna = TRUE, cltype = "hcltrp", minss = regBepoJP_minss, strsmp = 20)
# run_standardization(runpars, doflags = c("JP"), regstr = "regBepo", maxyr = 2018, do_early = FALSE, stdlabs = stdlabs, projdir = projdir, twlimit=0, jplimit = list(reg=0, yr=21000))

