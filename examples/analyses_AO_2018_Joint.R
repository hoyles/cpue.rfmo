# Joint standardization
########################################################
# Remove TW before 2005
# Joint standardization

projdir <- "~/ICCAT/2018_CPUE/"
Rdir <- paste0(projdir, "Rfiles/")

cndir <- paste0(projdir, "CN/")
krdir <- paste0(projdir, "KR/")
twdir <- paste0(projdir, "TW/")
jpdir <- paste0(projdir, "JP/")
usdir <- paste0(projdir, "US/")
jointdir <- paste0(projdir, "joint/")

jntalysis_dir <- paste0(jointdir, "analyses/")

library("stringr")
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

source(paste0(Rdir, "support_functions.r"))

allabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","alb","bet","yft","hcltrp",
             "Total","lat","lon","lat5","lon5","reg","flag","qtr")
allabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","alb","bet","yft","hcltrp",
            "lat","lon","lat5","lon5","reg","flag","qtr")

clkeepCN_B <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepJP_B <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepKR_B <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepTW_B <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clkeepUS_B <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(1,2,3,4)))
clk_B <- list(CN=clkeepCN_B,JP=clkeepJP_B,KR=clkeepKR_B,TW=clkeepTW_B,US=clkeepUS_B)

#---------------------------------------
### Run main effects cl nohbf

resdir <- paste0(jntalysis_dir,"std_cl_nohbf/")
dir.create(resdir)
setwd(resdir)

flaglist <- list(cn = "CN", jp = "JP", kr = "KR", tw = "TW", us = "US",all=c("CN","JP","KR","TW","US"))

runpars <- list()
runpars[["bet"]] <- list(regtype = "regB", regtype2 = "B", clk = clk_B, doregs = 1:3, addcl = TRUE, dohbf = FALSE, cltype = "hcltrp", minq_byreg = c(5,5,5))

clk <- clk_B   # for testing
vars <- c("vessid","hooks","yrqtr","latlong")

maxyr = 2018; keepd = TRUE; maxqtrs=200; addbranch<-F;addother=F;addalb=F
for(runsp in c("bet")) {
  regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  jdat <- data.frame()
  for(flag in flaglist$all) {
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
  jdat <- jdat[jdat$yrqtr > 2005 | jdat$flag != "TW",]

  vars <- c("vessid","hooks","yrqtr","latlong","hbf")
  for(runreg in runpars[[runsp]]$doregs) {
    minqtrs <- runpars[[runsp]]$minq_byreg[runreg]
    glmdat <- select_data_JointIO(jdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,maxqtrs=maxqtrs, minvess=100,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA)
    if(nrow(glmdat)>60000) glmdat <- samp_strat_data(glmdat,40)
    glmdat5279 <- select_data_JointIO(jdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,maxqtrs=maxqtrs, minvess=50,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1952,1980))
    if(nrow(glmdat5279)>60000) glmdat5279 <- samp_strat_data(glmdat5279,40)
    a <- jdat[jdat$vessid != "JP1",]
    glmdat79nd <- select_data_JointIO(a,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,maxqtrs=maxqtrs, minvess=50,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1979,maxyr))
    if(nrow(glmdat79nd)>60000) glmdat79nd <- samp_strat_data(glmdat79nd,40)
    wtt.all   <- mk_wts(glmdat,wttype="area")
    wtt.5279   <- mk_wts(glmdat5279,wttype="area")
    wtt.79nd   <- mk_wts(glmdat79nd,wttype="area")
    fmla.oplogn <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=F,addcl=T,nhbf=3)
    fmla.oplogn_ncl <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=F,addcl=F,nhbf=3)
    fmla.boatlogn <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=T,addcl=T,nhbf=3)
    fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=T,addcl=F,nhbf=3)
    mn <- with(glmdat,0.1* mean(get(runsp)/hooks))

    modlab="lognC_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if(lu(glmdat$clust) > 1)
    { model <- glm(fmla.oplogn,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() } else
    { model <- glm(fmla.oplogn_ncl,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if(lu(glmdat$clust) > 1)
    { model <- glm(fmla.boatlogn,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() } else
    { model <- glm(fmla.boatlogn_ncl,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    mn <- with(glmdat5279,0.1* mean(get(runsp)/hooks))
    if(lu(glmdat5279$clust) > 1)
    { model <- glm(fmla.oplogn,data=glmdat5279,weights=wtt.5279,family="gaussian", model = keepd);gc() } else
    { model <- glm(fmla.oplogn_ncl,data=glmdat5279,weights=wtt.5279,family="gaussian", model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat5279,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    mn <- with(glmdat79nd,0.1* mean(get(runsp)/hooks))
    if(lu(glmdat79nd$clust) > 1)
    { model <- glm(fmla.boatlogn,    data=glmdat79nd,weights=wtt.79nd,family="gaussian");gc() } else
    { model <- glm(fmla.boatlogn_ncl,data=glmdat79nd,weights=wtt.79nd,family="gaussian");gc() }
    summarize_and_store(mod=model,dat=glmdat79nd,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    # delta lognormal
    modlab="dellog_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg);
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat5279,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat79nd,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    graphics.off()
  }
}

#---------------------------------------
### run main effects nocl hbf

resdir <- paste0(jntalysis_dir,"std_nocl_hbf/")
dir.create(resdir)
setwd(resdir)

runpars <- list()
runpars[["bet"]] <- list(regtype = "regB", regtype2 = "B", clk = clk_B, doregs = 1:3, addcl = FALSE, dohbf = TRUE, cltype = "hcltrp", minq_byreg = c(5,5,5))

vars <- c("vessid","hooks","yrqtr","latlong")

maxyr = 2018; keepd = TRUE; maxqtrs=200
for(runsp in c("bet")) {
    regtype <- runpars[[runsp]]$regtype
  clk <- runpars[[runsp]]$clk
  addcl <- runpars[[runsp]]$addcl
  dohbf <- runpars[[runsp]]$dohbf
  cltype <- runpars[[runsp]]$cltype
  jdat <- data.frame()
  for(flag in flaglist$all) {
    for(r in runpars[[runsp]]$doregs) {
      load(paste0(projdir,flag,"/clustering/",paste(flag,regtype,r,sep="_"),".RData"))
      dataset$qtr <- revtrunc(defactor(dataset$yrqtr))
      dataset$flag <- flag
      jdat <- rbind(jdat,dataset[,allabs])
      rm(dataset)
    }
  }
  jdat <- jdat[jdat$yrqtr < maxyr,]
  jdat$vessidx <- jdat$vessid
  jdat$vessid <- paste0(jdat$flag,jdat$vessid)
  jdat$vessid <- as.factor(jdat$vessid)
  jdat <- jdat[jdat$yrqtr > 2005 | jdat$flag != "TW",]

  vars <- c("vessid","hooks","yrqtr","latlong","hbf")
  for(runreg in runpars[[runsp]]$doregs) {
    minqtrs <- runpars[[runsp]]$minq_byreg[runreg]
    glmdat <- select_data_JointIO(jdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,maxqtrs=maxqtrs, minvess=100,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA)
    if(nrow(glmdat)>60000) glmdat <- samp_strat_data(glmdat,40)
    glmdat5279 <- select_data_JointIO(jdat,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,maxqtrs=maxqtrs, minvess=50,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1952,1980))
    if(nrow(glmdat5279)>60000) glmdat5279 <- samp_strat_data(glmdat5279,40)
    a <- jdat[jdat$vessid != "JP1",]
    glmdat79nd <- select_data_JointIO(a,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,maxqtrs=maxqtrs, minvess=50,minll=50,minyrqtr=50,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1979,maxyr))
    if(nrow(glmdat79nd)>60000) glmdat79nd <- samp_strat_data(glmdat79nd,40)
    wtt.all   <- mk_wts(glmdat,wttype="area")
    wtt.5279   <- mk_wts(glmdat5279,wttype="area")
    wtt.79nd   <- mk_wts(glmdat79nd,wttype="area")
    fmla.oplogn <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=F,addcl=T,nhbf=3)
    fmla.oplogn_ncl <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=F,addcl=F,nhbf=3)
    fmla.boatlogn <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=T,addcl=T,nhbf=3)
    fmla.boatlogn_ncl <- make_formula_IO(runsp,modtype="logn",dohbf=dohbf,addboat=T,addcl=F,nhbf=3)
    mn <- with(glmdat,0.1* mean(get(runsp)/hooks))

    modlab="lognC_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if(lu(glmdat$clust) > 1)
    { model <- glm(fmla.oplogn,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() } else
    { model <- glm(fmla.oplogn_ncl,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    if(lu(glmdat$clust) > 1)
    { model <- glm(fmla.boatlogn,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() } else
    { model <- glm(fmla.boatlogn_ncl,data=glmdat,weights=wtt.all,family="gaussian", model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    mn <- with(glmdat5279,0.1* mean(get(runsp)/hooks))
    if(lu(glmdat5279$clust) > 1)
    { model <- glm(fmla.oplogn,data=glmdat5279,weights=wtt.5279,family="gaussian", model = keepd);gc() } else
    { model <- glm(fmla.oplogn_ncl,data=glmdat5279,weights=wtt.5279,family="gaussian", model = keepd);gc() }
    summarize_and_store(mod=model,dat=glmdat5279,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    modlab="lognC_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    mn <- with(glmdat79nd,0.1* mean(get(runsp)/hooks))
    if(lu(glmdat79nd$clust) > 1)
    { model <- glm(fmla.boatlogn,    data=glmdat79nd,weights=wtt.79nd,family="gaussian");gc() } else
    { model <- glm(fmla.boatlogn_ncl,data=glmdat79nd,weights=wtt.79nd,family="gaussian");gc() }
    summarize_and_store(mod=model,dat=glmdat79nd,fname,modlab,dohbf=dohbf, keepd = keepd);rm(model)

    # delta lognormal
    modlab="dellog_novess_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg);
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_boat_allyrs"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_novess_5279"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat5279,dohbf=dohbf,addboat=F,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    modlab="dellog_vessid_79nd"; fname <- paste0("Joint_",regtype,"_R",runreg)
    do_deltalog(dat=glmdat79nd,dohbf=dohbf,addboat=T,addcl=addcl,nhbf=3,runsp=runsp,fname=fname,modlab=modlab, keepd = keepd)

    graphics.off()
  }
}


#################################################
###### Get medians and make newdats ############
flaglist <- list(cn = "CN", jp = "JP", kr = "KR", tw = "TW", us = "US",all=c("CN","JP","KR","TW","US"))
reglist <- list("regB" = c(1,2,3))

datmeds <- list()
for(regtype in c("regB")) {
  print(regtype)
  datmeds[[regtype]] <- regtype
  runsp <- switch(regtype,regB="bet")
  clk <- switch(regtype,regB=clk_B)
  jdat <- data.frame()
  for(flag in flaglist$all) {
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
        minqtrs <- runpars[[runsp]]$minq_byreg[runreg]
        glmdat <- select_data_JointIO(dd,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,maxqtrs=maxqtrs, minvess=30,minll=30,minyrqtr=30,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA)
        if(dim(glmdat)[1] > 0) {
          glmdat_meds <- get_base_newdat(glmdat)
          if((length(flagl) > 1 | flagl == "JP")[1]) {
            glmdat5279 <- select_data_JointIO(dd,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,maxqtrs=maxqtrs, minvess=30,minll=30,minyrqtr=30,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA, yrlims=c(1952,1980))
            glmdat5279_meds <- get_base_newdat(glmdat5279)
          } else glmdat5279_meds <- ""
          a <- dd[dd$vessid != "JP1",]
          glmdat79nd <- select_data_JointIO(a,runreg=runreg,clk=clk,minqtrs=minqtrs,runsp=runsp,mt="deltabin",vars=vars,maxqtrs=maxqtrs, minvess=30,minll=30,minyrqtr=30,addcl=addcl,cltype=cltype,addpca=NA,samp=NA,strsmp=NA,yrlims=c(1979,maxyr))
          glmdat79nd_meds <- get_base_newdat(glmdat79nd)
          datmeds[[regtype]][[fl]][[runreg]] <- list(flag=flagl, glmdat_meds, glmdat5279_meds, glmdat79nd_meds)
        } else datmeds[[regtype]][[fl]][[runreg]] <- list(flag=flagl, 0,0,0)
      }
    }
}
save(datmeds, file = paste0(projdir, "data_medians.RData"))
