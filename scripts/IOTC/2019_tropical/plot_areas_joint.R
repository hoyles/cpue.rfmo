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
figdir <- paste0(jointdir, "figures/")

dir.create(figdir)
setwd(figdir)

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

clkeepTW_B2 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(2),c(4)))
clkeepTW_B3 <- list("bet"=list(c(1,2,3,4),c(0),c(0),c(0),c(1,2,3,4)))
clkeepTW_B4 <- list("bet"=list(c(2,3,4)))
clkeepTW_Y <- list("yft"=list(c(1,2,3,4,5), c(1,2,3,4), c(2), c(3,4), c(1,2,3,4,5), c(1,2,3,4,5)))
clkeepTW_Y2 <- list("yft"=list(c(0), c(1,2,3,4),c(0), c(0), c(0), c(0), c(1,2,3,4)))
clkeepTW_Y3 <- list("yft"=list(c(1,2,3,4,5)))

clkeepTW95_B2 <- list("bet"=list(c(1,2,3,4),c(1,2,3,4),c(2),c(2,4)))
clkeepTW95_B3 <- list("bet"=list(c(1,2,3,4),c(0),c(0),c(0),c(1,2,3,4)))
clkeepTW95_B4 <- list("bet"=list(c(2,3,4)))
clkeepTW95_Y  <- list("yft"=list(c(1,2,3,4,5), c(1,2,3,4), c(1,2), c(4), c(1,2,3,4,5), c(1,2,3,4,5)))
clkeepTW95_Y2 <- list("yft"=list(c(0), c(1,2,3,4),c(0), c(0), c(0), c(0), c(1,2,3,4)))
clkeepTW95_Y3 <- list("yft"=list(c(1,2,3,4,5)))

clk_Y  <- list(JP=clkeepJP_Y, KR=clkeepKR_Y, TW=clkeepTW_Y, SY=clkeepSY_Y)
clk_Y2 <- list(JP=clkeepJP_Y2,KR=clkeepKR_Y2,TW=clkeepTW_Y2,SY=clkeepSY_Y2)
clk_Y3 <- list(JP=clkeepJP_Y3,KR=clkeepKR_Y3,TW=clkeepTW_Y3,SY=clkeepSY_Y3)
clk_B2 <- list(JP=clkeepJP_B2,KR=clkeepKR_B2,TW=clkeepTW_B2,SY=clkeepSY_B2)
clk_B3 <- list(JP=clkeepJP_B3,KR=clkeepKR_B3,TW=clkeepTW_B3,SY=clkeepSY_B3)
clk_B4 <- list(JP=clkeepJP_B4,KR=clkeepKR_B4,TW=clkeepTW_B4,SY=clkeepSY_B4)

clk95_Y  <- list(JP=clkeepJP_Y, KR=clkeepKR_Y, TW=clkeepTW95_Y, SY=clkeepSY_Y)
clk95_Y2 <- list(JP=clkeepJP_Y2,KR=clkeepKR_Y2,TW=clkeepTW95_Y2,SY=clkeepSY_Y2)
clk95_Y3 <- list(JP=clkeepJP_Y3,KR=clkeepKR_Y3,TW=clkeepTW95_Y3,SY=clkeepSY_Y3)
clk95_B2 <- list(JP=clkeepJP_B2,KR=clkeepKR_B2,TW=clkeepTW95_B2,SY=clkeepSY_B2)
clk95_B3 <- list(JP=clkeepJP_B3,KR=clkeepKR_B3,TW=clkeepTW95_B3,SY=clkeepSY_B3)
clk95_B4 <- list(JP=clkeepJP_B4,KR=clkeepKR_B4,TW=clkeepTW95_B4,SY=clkeepSY_B4)

stdlabs <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","lat","lon","lat5","lon5","reg","hcltrp","flag")
maxyr <- 2019

runpars <- list()
runpars[["regY"]] <-list(runsp = "yft", regtype2 = "Y", clk = clk_Y, doregs = c(1,2,3,4,5,6),
                         addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                         do_lognC = TRUE, do_deltalog = TRUE,
                         do_early = TRUE, do_late = TRUE, do_vessallyr = FALSE,
                         cltype = "hcltrp", strsmp = 15)
runpars[["regB2"]] <-list(runsp = "bet", regtype2 = "B2", clk = clk_B2, doregs = c(1,2,3,4),
                          addcl = FALSE, dohbf = TRUE, dohook = TRUE,
                          do_lognC = TRUE, do_deltalog = TRUE,
                          do_early = TRUE, do_late = TRUE, do_vessallyr = FALSE,
                          cltype = "hcltrp", strsmp = 15)

twlimit <- 2005
jplimit = list(reg=2, yr=3005)
regstr <- "regY"
rp <- runpars[[regstr]]
jdat <- data.frame()
for (flag in c("JP", "KR", "TW", "SY")) {
  for (r in rp$doregs) {
    datfl <- paste0(projdir,flag,"/clustering/",paste(flag,regstr,r,sep = "_"),".RData")
    if(file.exists(datfl)) {
      load(datfl)
      dataset$flag <- flag
      dataset$qtr <- revtrunc(defactor(dataset$yrqtr))
      jdat <- rbind(jdat,dataset[,stdlabs])
      rm(dataset)
    }
  }
}
jdat <- jdat[jdat$yrqtr < maxyr,]
jdat$vessidx <- jdat$vessid
jdat$vessid <- paste0(jdat$flag,jdat$vessid)
jdat$vessid <- as.factor(jdat$vessid)
jdat <- jdat[jdat$yrqtr > twlimit | jdat$flag != "TW",]

vars <- c("vessid","hooks","yrqtr","latlong","hbf")

# remove data as specified in runpars, with jplimit, krlimit, and the more general dat_lims
jdat2 <- jdat[jdat$yrqtr < jplimit$yr | !jdat$reg %in% jplimit$reg | jdat$flag != "JP",]
if(!is.na(krlimit)) {
  jdat2 <- jdat2[(jdat2$yrqtr > krlimit$yr[1] & jdat2$yrqtr < krlimit$yr[2]) |
                   !jdat2$reg %in% krlimit$reg | jdat2$flag != "KR",]
}

str(jdat2)

a <-  aggregate(cbind(hooks) ~ lon + lat + eval(5*floor((op_yr+5)/5)-5),data=jdat2[!is.na(jdat2$lon),],FUN=sum)
ayr <-  aggregate(cbind(hooks) ~ lon + lat + op_yr,data=jdat2[!is.na(jdat2$lon),],FUN=sum)
a5 <- aggregate(cbind(hooks) ~ lon5 + lat5 + eval(5*floor((op_yr)/5)),data=jdat2,FUN=sum)
a5yr <- aggregate(cbind(hooks) ~ lon5 + lat5 + op_yr,data=jdat2,FUN=sum)
names(a)[3] <- names(ayr)[3] <- names(a5yr)[3] <- names(a5)[3] <- "decade"
names(a5)[1:2] <- c("lon","lat")

# maxa <- max(a$hooks)
# bk = seq(0, maxa, length.out = 10)
windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
# for(d in seq(1955,2015,5)) plot_catchmap(indat=a,vbl=a$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="Effort", brk = bk, brk2 = bk)
for(d in seq(1955,2015,5)) plot_catchmap(indat=a,vbl=a$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="Effort")
savePlot("Effort by 5yr period", type = "png")

windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap2(indat=a,vbl=a$hooks,dcd=d,latlim=c(-40,17),lonlim=c(20,120),ti="Effort")
savePlot("Effort by 5yr period", type = "png")

maxa <- max(a5$hooks)
bk = seq(0, maxa, length.out = 10)
windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap(indat=a5,vbl=a5$hooks,dcd=d,latlim=c(-40,17),lonlim=c(20,120),ti="Effort", delta=5, brk = bk, brk2 = bk)
savePlot("Effort5 by 5yr perioda", type = "png")

windows(width=20,height=20);par(mfrow=c(5,3),mar=c(2,2,2,2))
for(d in seq(1955,2015,5)) plot_catchmap2(indat=a5,vbl=a5$hooks,dcd=d,latlim=c(-40,17),lonlim=c(20,120),ti="Effort", delta=5)
savePlot("Effort5 by 5yr periodb", type = "png")

maxa <- max(a5yr$hooks)
bk = seq(0, maxa, length.out = 10)
windows(width=20,height=20);par(mfrow=c(4,3),mar=c(2,2,2,2))
for(d in seq(2005,2016,1)) plot_catchmap(indat=a5yr,vbl=a5yr$hooks,dcd=d,latlim=c(-40,17),lonlim=c(20,120),ti="Effort", delta=5, brk = bk, brk2 = bk)
savePlot("Effort5 by 1yr perioda", type = "png")

windows(width=20,height=20);par(mfrow=c(4,3),mar=c(2,2,2,2))
for(d in seq(2005,2016,1)) plot_catchmap2(indat=a5yr,vbl=a5yr$hooks,dcd=d,latlim=c(-40,17),lonlim=c(20,120),ti="Effort", delta=5)
savePlot("Effort5 by 1yr periodb", type = "png")

yrsum <- aggregate(hooks~ op_yr, data=jdat2, sum)
windows()
plot(yrsum$op_yr, yrsum$hooks, xlab = "Year", ylab = "Hooks in the CPUE")
savePlot("Hooks per year", type = "png")

windows(width=20,height=20);par(mfrow=c(4,3),mar=c(2,2,2,2))
for(d in seq(2005,2016,1)) { plot_catchmap2(indat=ayr,vbl=ayr$hooks,dcd=d,latlim=c(-45,20),lonlim=c(20,120),ti="Effort") }
savePlot("Effort by 1yr periodb",type="png")


