########################################################################
## Description: Yellowfin tuna CPUE Standardization - Longline Brazil
##
## Maintainer: ICCAT / Brazilian SCC for Tunas and Tunas like
## Author: Rodrigo Sant'Ana
## Created: ter abr 16 12:44:43 2019 (+0200)
##
## URL:
## Doc URL:
##
## Database info:
## LOA - Boat size (m)
## HBF - Number of hooks per basket
## TYPE - (1) Pelagic LL; (2) Demersal LL
## MOON - Moon illumination (%)
## BAT - Bathymetry (m)
## SST - Sea Surface Temperature (C)
## MLD - Mixture Layer Depth
## TTD - Top Thermocline Depth (m)
## CH - Clorophyll (mg)
## REGB e REGB1 - Areas for bigeye tuna
## TOTAL2 - The sum of YFT, ALB and BET
##
### Commentary:
##
### Code:
########################################################################

########################################################################
######@> Loading R packages for the exploratory analyses...
library(dplyr)
library(ggplot2)
library(lme4)
library(doBy)
library(gridExtra)
library(MuMIn)
library(lsmeans)
library(sjstats)
##library(lmerTest)
##library(pbkrtest)
library(corrplot)
library(cluster)
library(reshape2)
library(grImport)
library(date)
library(splines)
library(maps)
library(mapdata)
library(maptools)
library(data.table)
library(lunar)
library(lubridate)
library(readr)
library(plyr)
library(dplyr)
library(dtplyr)
library(tm)
library(readxl)
library(rpart)
library(randomForest)
library(mgcv)
library(influ)
library(nFactors)
library(cluster)
library(splines)
library(boot)
library(beanplot)
library(lubridate)

########################################################################
######@> Installing cpue.rfmo package...

#####@> Installing package builded in my pc...
## install.packages("/home/rodrigo/Github/cpue.rfmo_0.1.0.tar.gz",
##                  repo = NULL)

######@> Loading cpue.rfmo package...
library(cpue.rfmo)

######@> Package Description...
packageDescription("cpue.rfmo")

########################################################################
######@> Setup R...

######@> Customization for ggplot2 theme...
seta <- grid::arrow(length = grid::unit(0.2, "cm"), type = "open")
my_theme <- function (base_size = 12, base_family = "Arial") {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(axis.ticks = element_blank(),
              axis.line = element_line(arrow = seta),
              legend.background = element_blank(),
              legend.key = element_blank(),
              panel.background = element_blank(),
              panel.border = element_blank(),
              ## strip.background = element_blank(),
              plot.background = element_blank(),
              plot.title = element_text(hjust = 1),
              complete = TRUE)
}

######@> Windrose...
norte <- pictureGrob(readPicture("../Padronizacao_CPUE_BET/NorthArrow_02.ps.xml"))

########################################################################
######@> Adapting some cpue.rfmo functions...

######@> Dataprep Brazil...
dataprep_BR <- function(dat, splist) {
    dat <- mutate(dat, op_yr = year) %>%
        mutate(dmy = parse_date(str_c(parse_character(year),"/",
                                      parse_character(month),"/",
                                      parse_character(day)),
                                "%Y/%m/%d")) %>%
        mutate(op_mon = dat$month) %>%
        mutate(op_day = dat$day) %>%
        mutate(hbf = parse_integer(hpb)) %>%
        mutate(hooks = dat$effort) %>%
        mutate(floats=hooks/hbf)
    dat$moon <- dat$IL
    dat$lon <- dat$lng
    dat$lon5 <- dat$lng5
    dat$qtr <- ceiling(as.numeric(dat$op_mon)/3)
    dat$yrqtr <- dat$op_yr + floor((dat$op_mon - 1)/3)/4 + 0.125
    dat$latlong <- paste(dat$lat5, dat$lon5, sep = "_")
    dat$vessid <- as.factor(as.numeric(dat$boat))
    dat$tripidmon <- paste(dat$vessid, dat$op_yr, dat$op_mon)
    dat$Total <- apply(dat[,splist], 1, sum, na.rm = TRUE)
    dat$Total2 <- apply(dat[, c("bet.t", "yft.t", "alb.t")], 1, sum, na.rm = TRUE)
    return(dat)
}

######@> Modifying dataclean function for Brazil...
dataclean_BR <- function (dat, yearlim = 2018, splist) {
    for (sp in splist) {
        dat[, sp] <- as.numeric(dat[, sp])
        if (sum(is.na(dat[, sp])) > 0)
            dat[is.na(dat[, sp]), sp] <- 0
    }
    data <- dat[dat$type == 1, ]
    dat <- dat[!is.na(dat$hooks), ]
    dat <- dat[dat$hooks < 5000, ]
    dat <- dat[dat$hooks >= 500, ]
    dat <- dat[is.na(dat$hbf) == FALSE, ]
    dat <- dat[dat$op_yr > 1976, ]
    dat <- dat[dat$yrqtr < yearlim, ]
    dat <- dat[dat$hbf >= 5, ]
    return(dat)
}

########################################################################
######@> Loading datasets...

######@> Rawdata from Brazil...
load("../BNDA/LL/Espelho_BNDA_LL_limpo.RData")
rawdata <- dt10; rm(dt10)

######@> Brazilian Longline logbook dataset...
load("../BNDA/LL/Espelho_dataprep_BRA2.RData")
prepdat <- dataprep_BRA2; rm(dataprep_BRA2)

######@> Base map...
mm <- map_data("world")

########################################################################
######@> Preparing the structure of folders to receive the outputs...

projdir <- "ICCAT/2019_Yellowfin/"
brdir <- paste0(projdir, "BR/")
datadir <- paste0(brdir, "data/")
bralysis_dir <- paste0(brdir, "analyses/")
brfigs <- paste0(brdir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
## dir.create(bralysis_dir, recursive = TRUE)
## dir.create(brfigs)
setwd(bralysis_dir)

########################################################################
######@> Cleaning dataset...

######@> Correcting the number of floats...
prepdat$floats <- with(prepdat, hooks / hbf)

######@> Correcting the lat5 and lon5...
prepdat$lat5 <- 5 * floor(prepdat$lat/5) + 2.5
prepdat$lon5 <- 5 * floor(prepdat$lon/5) + 2.5
prepdat$latlong <- paste(prepdat$lat5, prepdat$lon5, sep = "_")

######@> Species list...
splist <- c("yft", "alb", "bet", "swo", "sai", "whm", "bum", "bsh",
            "spx", "bth", "sma", "ocs", "fal", "ccs")

######@> Setup Regions...
prepdat2 <- setup_AO_regions(dat = prepdat, regB = TRUE, regB1 = TRUE,
                             regY = TRUE, regY1 = TRUE)

#####@> Visualizing spatial distribution of sets...
p00 <- ggplot(data = prepdat2, aes(x = lon5, y = lat5)) +
    geom_tile(aes(fill = factor(regB)), colour = "black") +
    geom_polygon(data = mm, aes(x = long, y = lat, group = group)) +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    ## scale_fill_manual(name = "Areas",
    ##                   values = c("red", "green", "blue")) +
    coord_fixed(xlim = c(-55, 0), ylim = c(-50, 20)) +
    xlab(expression(paste("Longitude ", "(", degree, ")"))) +
    ylab(expression(paste("Latitude ", "(", degree, ")"))) +
    my_theme()

p01 <- ggplot(data = prepdat2, aes(x = lon5, y = lat5)) +
    geom_tile(aes(fill = factor(regB1)), colour = "black") +
    geom_polygon(data = mm, aes(x = long, y = lat, group = group)) +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    ## scale_fill_manual(name = "Areas",
    ##                   values = c("red", "green", "blue")) +
    coord_fixed(xlim = c(-55, 0), ylim = c(-50, 20)) +
    xlab(expression(paste("Longitude ", "(", degree, ")"))) +
    ylab(expression(paste("Latitude ", "(", degree, ")"))) +
    my_theme()

p02 <- ggplot(data = prepdat2, aes(x = lon5, y = lat5)) +
    geom_tile(aes(fill = factor(regY)), colour = "black") +
    geom_polygon(data = mm, aes(x = long, y = lat, group = group)) +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    ## scale_fill_manual(name = "Areas",
    ##                   values = c("red", "green", "blue")) +
    coord_fixed(xlim = c(-55, 0), ylim = c(-50, 20)) +
    xlab(expression(paste("Longitude ", "(", degree, ")"))) +
    ylab(expression(paste("Latitude ", "(", degree, ")"))) +
    my_theme()

p03 <- ggplot(data = prepdat2, aes(x = lon5, y = lat5)) +
    geom_tile(aes(fill = factor(regY1)), colour = "black") +
    geom_polygon(data = mm, aes(x = long, y = lat, group = group)) +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    ## scale_fill_manual(name = "Areas",
    ##                   values = c("red", "green", "blue")) +
    coord_fixed(xlim = c(-55, 0), ylim = c(-50, 20)) +
    xlab(expression(paste("Longitude ", "(", degree, ")"))) +
    ylab(expression(paste("Latitude ", "(", degree, ")"))) +
    my_theme()

######@> Cleaning data set...
dat <- dataclean_BR(dat = prepdat2, splist = splist)

######@> Export final data set...
save(prepdat, dat, file = "BRdat.RData")

########################################################################
######@> Exploring dataset...

######@> Looking for some data...

#####@> Some exploratory tables - Vessels per year | Sets without
#####@> Vessels | Years without hooks...
str(dat)
summary(dat)
table(dat$vessid, dat$op_yr)
table(dat$op_yr, dat$vessid)
table(dat$op_yr, is.na(dat$vessid))
table(dat$op_yr, (dat$hooks > 0))

#####@> number of vessels per year...
xfun <- function(x) sum(x > 0)
a <- table(dat$vessid, dat$op_yr)
apply(a, 2, xfun)
apply(a, 2, lu)

#####@> Sets per day...
dev.new(width = 15, height = 9)
hist(dat$dmy, breaks = "days", freq = T, xlab = "Date",
     main = "Sets per day")
savePlot(filename = "sets_per_day.png", type = "png")
dev.off()

#####@> Plot grid squares with sets by region, for each regional
#####@> structure...
a <- unique(paste(dat$lat, dat$lon))
a0 <- dat[match(a, paste(dat$lat, dat$lon)),
          c("lat","lon","regB", "regB1", "regY", "regY1")]

for (fld in c("regB", "regB1", "regY", "regY1")) {
    dev.new(widath = 10, height = 10)
    reg <- with(a0, get(fld))
    plot(a0$lon, a0$lat, type = "n", xlab = "Longitude",
         ylab = "Latitude", main = fld)
    text(a0$lon, a0$lat, labels = reg, cex = 0.8, col = reg + 1)
    map(add = T)
    savePlot(paste0("map_", fld), type = "png")
    dev.off()
}

#####@> Map of hook distribution, all time...
a <- aggregate(dat$hooks, list(dat$lat5, dat$lon5), sum, na.rm = T)
dev.new(width = 11,height = 9)
symbols(x = a[,2], y = a[,1], circles = .0002 * sqrt(a[,3]),
        inches = F, bg = 2, fg = 2, xlab = "Longitude", ylab = "Latitude",
        ylim = c(-50, 20), xlim = c(-60, -10))
map(add = T, interior = F, fill = T)
savePlot(filename = "map_hooks.png", type = "png")
dev.off()

#####@> Histogram of hooks per set
table(dat$hooks[dat$hooks > 2000])
hist(dat$hooks, main = "", breaks = seq(0, 5000, 100),
     include.lowest = TRUE, right = FALSE, ylim = c(0, 15000),
     xlab = "Hooks per set", xlim = c(0, 5000))
savePlot("Hook_histogram.png", type = "png")
dev.off()

#####@> Check catch distribtions for outliers. Probably no need to remove.
table(dat$yft)
table(dat$alb)
table(dat$bet)
table(dat$swo)
table(dat$sai)
table(dat$whm)
table(dat$bum)
table(dat$bsh)
table(dat$spx)
table(dat$bth)
table(dat$sma)
table(dat$ocs)
table(dat$fal)
table(dat$ccs)

table(dat$hbf, useNA = "always")
table(dat$hbf, dat$op_yr, useNA = "always")
table(dat$op_yr, is.na(dat$hbf))

a <- table(dat$op_yr, round(dat$hbf, 0), useNA = "always")
write.csv(a, "table_hbf_by_year.csv")

#####@> Set density map by 5 degree cell
a <- log(table(dat$lon5, dat$lat5))
dev.new(width = 13, height = 10)
image(as.numeric(dimnames(a)[[1]]), as.numeric(dimnames(a)[[2]]), a,
      xlab = "Longitude", ylab = "Latitude",
      col  =  rev(heat.colors(12)))
contour(as.numeric(dimnames(a)[[1]]), as.numeric(dimnames(a)[[2]]), a,
        xlab = "Longitude", ylab = "Latitude", add  =  TRUE)
map("world", add = T, interior = T, fill = T)
savePlot("setmap_logscale.png", type = "png")
dev.off()

#####@> Mean fishing location  by yearqtr
dev.new(width = 15, height = 10)
par(mfrow = c(1, 2))
ax <- tapply(dat$yrqtr, dat$yrqtr, mean)
ay <- tapply(dat$lat5, dat$yrqtr, mean)
plot(ax, ay, xlab = "yr", ylab = "Mean latitude", type = "n")
a <- 4 * (.125+dat$yrqtr-floor(dat$yrqtr))
a <- tapply(a, dat$yrqtr, mean)
text(ax, ay, a, cex = 0.7)
ax <- tapply(dat$lon5, dat$yrqtr, mean)
ay <- tapply(dat$yrqtr, dat$yrqtr, mean)
plot(ax, ay, ylab = "yr", xlab = "Mean longitude", type = "n")
text(ax, ay, a, cex = 0.7)
savePlot("mean_fishing_location1.png",type = "png")
dev.off()

#####@> Mean fishing location by year
dev.new(width = 15, height = 10)
par(mfrow = c(1, 2))
plot(tapply(dat$op_yr, dat$op_yr, mean), tapply(dat$lat5, dat$op_yr, mean),
     xlab = "yr", ylab = "Mean latitude")
plot(tapply(dat$lon5, dat$op_yr,mean), tapply(dat$op_yr, dat$op_yr,mean),
     ylab = "yr",xlab = "Mean longitude")
savePlot("mean_fishing_location2.png", type = "png")
dev.off()

#####@> Plot hbf... Change spatial selection criteria for AO.
dev.new(20, 14)
par(mfrow = c(3, 3), mar = c(2, 2, 2, 2))
for (y in seq(1980, 2015, 5)) {
    a <- dat[dat$op_yr %in% y:(y+4),]
    ## max.hbf <- max(a$hbf, na.rm = TRUE)
    a <- tapply(a$hbf, list(a$lon5, a$lat5), mean, na.rm = T)
    image(as.numeric(dimnames(a)[[1]]), as.numeric(dimnames(a)[[2]]) ,
          a, main = y, zlim = c(0, 30), col = rev(heat.colors(30)),
          xlab = "Lon", ylab = "Lat", ylim = c(-50, 20),
          xlim = c(-60, -20))
    contour(as.numeric(dimnames(a)[[1]]), as.numeric(dimnames(a)[[2]]),
            a, add = T, levels = seq(0, 7, 1))
    map("world", add = T, interior = F, fill = T)
}
savePlot("mean_HBF.png", type = "png")
dev.off()

#####@> Plot hbf per quarter...
qqs <- c(0.125, 0.375, 0.625, 0.875)
for (qq in 1:4) {
    dev.new(20, 14)
    par(mfrow = c(3, 3), mar = c(2, 2, 2, 2), oma = c(0, 0, 1, 0))
    for (y in seq(1980, 2015,5)) {
        a <- dat[dat$yrqtr %in% (qqs[qq]+y:(y+4)), ]
        a <- tapply(a$hbf, list(a$lon5, a$lat5), mean, na.rm = T)
        image(as.numeric(dimnames(a)[[1]]),
              as.numeric(dimnames(a)[[2]]),
              a, main = y, zlim = c(0, 30),
              col = rev(heat.colors(30)), xlab = "Lon", ylab = "Lat",
              ylim = c(20, -50), xlim = c(-60, -20))
        contour(as.numeric(dimnames(a)[[1]]),
                as.numeric(dimnames(a)[[2]]), a, add = T,
                levels = seq(0, 15, 1), col = "blue")
        map("world", add = T, interior = F, fill = T)
    }
    title(paste("Quarter",qq), outer = T, line = 0)
    savePlot(paste0("mean_HBF_q",qq,".png"), type = "png")
    dev.off()
}

write.csv(table(dat$lat5, dat$lon5), file = "ops_by_lat-long.csv")
write.csv(table(dat$lat5, dat$lon5, 5 * floor(dat$yrqtr/5)),
          file = "ops_by_lat-long-5yr.csv")

#####@> Exploration Regressions tree...

####@> preparing data...
a <- dat
head(dat)
dim(a)

###@> nominal cpue...
a$yftcpue <- a$yft/a$hooks
a$albcpue <- a$alb/a$hooks
a$betcpue <- a$bet/a$hooks
a$swocpue <- a$swo/a$hooks
a$saicpue <- a$sai/a$hooks
a$whmcpue <- a$whm/a$hooks
a$bumcpue <- a$bum/a$hooks
a$bshcpue <- a$bsh/a$hooks
a$spxcpue <- a$spx/a$hooks
a$bthcpue <- a$bth/a$hooks
a$smacpue <- a$sma/a$hooks
a$ocscpue <- a$ocs/a$hooks
a$falcpue <- a$fal/a$hooks
a$ccscpue <- a$ccs/a$hooks

###@> simple model 01...
simplemod01 <- rpart(a$yftcpue ~ a$lon + a$lat + a$yrqtr + a$albcpue +
                         a$betcpue + a$swocpue + a$saicpue + a$whmcpue +
                         a$bumcpue + a$bshcpue + a$spxcpue + a$bthcpue +
                         a$smacpue + a$ocscpue + a$falcpue + a$ccscpue)

dev.new(width = 11, height = 7)
plot(simplemod01)
text(simplemod01)
savePlot("Rpart_yft_cpue_full", type = "png")
dev.off()

simplemod02 <- rpart(a$yftcpue ~ a$lon + a$lat + a$yrqtr + a$albcpue +
                         a$betcpue + a$whmcpue)

dev.new(width = 11, height = 7)
plot(simplemod02)
text(simplemod02)
savePlot("Rpart_yft_cpue_subset", type = "png")
dev.off()

#####@> Exploration with Random Forest...

####@> These take a long time and use a lot of memory, but are useful...
system.time(
    simplefor <- randomForest(yftcpue ~ lon + lat + yrqtr + hbf + albcpue +
                                  betcpue + bshcpue + whmcpue, data = a)
)

print(simplefor)

dev.new(width = 11, height = 7)
plot(simplefor)

varImpPlot(simplefor)
savePlot("Rforest_yft_cpue", type = "png")
dev.off()

partialPlot(simplefor, pred.data = a, x.var = "hbf")
savePlot("Rforest_yft_cpue_partial", type = "png")
dev.off()

########################################################################
######@> Cluster Analyses...

######@> Change the directory...
clustdir <- "../clustering/"
## dir.create(clustdir)
setwd(clustdir)

######@> Preparing the data...
gc(reset = TRUE)
br_splist <- c("yft", "alb", "bet", "swo", "sai", "whm", "bum", "bsh",
               "spx", "bth", "sma", "ocs", "fal", "ccs")
use_splist <- c("yft", "alb", "bet", "swo", "sai", "bum", "bsh", "whm",
                "sma")
allabs <- c("vessid", "yrqtr", "latlong", "op_yr", "hbf", "hooks",
            "tripidmon", use_splist, "Total", "lat", "lon", "lat5",
            "lon5", "regB", "regB1")
dat <- data.frame(dat)

#####@> Number of yellowfin clusters. Will need to be adjusted for each
#####@> fleet...
nclB <- c(0, 5, 4)
flag <- "BR"
cvn <- c("yrqtr", "latlong", "hooks", "hbf", "vessid", "Total", "lat",
         "lon", "lat5", "lon5", "op_yr", "tripidmon", "regB", "regB1",
         "regY", "regY1")
allabs <- c("vessid", "yrqtr", "latlong", "op_yr", "hbf", "hooks",
            "tripidmon", use_splist, "Total", "lat", "lon", "lat5",
            "lon5", "regB", "regB1", "regY", "regY1")

#####@> Looping for the analysis of the species - regB region...
for (r in unique(dat$regB)) {
    dev.new(15, 12)
    par(mfrow = c(5, 3), mar = c(3, 2, 2, 1), oma = c(0, 0, 2, 0))
    a <- dat[dat$regB == r, ]
    for (sp in br_splist) {
        plot(sort(unique(a$yrqtr)), tapply(a[, sp], a$yrqtr, mean), main = sp)
        title(paste("Region", r ), outer = TRUE)
        savePlot(filename = paste("freq", flag, "regB_Region", r, sep = "_"),
                 type = "png")
    }
    dev.off()
}

#####@> Looping for the analysis of the species - regY1 region...
for (r in unique(dat$regY1)) {
    dev.new(15, 12)
    par(mfrow = c(5, 3), mar = c(3, 2, 2, 1), oma = c(0, 0, 2, 0))
    a <- dat[dat$regY1 == r, ]
    for (sp in br_splist) {
        plot(sort(unique(a$yrqtr)), tapply(a[, sp], a$yrqtr, mean), main = sp)
        title(paste("Region", r ), outer = TRUE)
        savePlot(filename = paste("freq", flag, "regY1_Region", r, sep = "_"),
                 type = "png")
    }
    dev.off()
}


#####@> Cluster analyses for regB region...
regtype <- "regB"
for (r in unique(dat$regB)) {
    fnh <- paste(flag, regtype, r, sep = "_")
    dataset <- clust_PCA_run(r = r, ddd = dat, allsp = use_splist,
                             allabs = allabs, regtype = regtype,
                             ncl = nclB[r], plotPCA = FALSE,
                             clustid = "tripidmon",
                             allclust = FALSE, ll5 = TRUE, flag = flag,
                             fnhead = fnh, covarnames = cvn)
    save(dataset, file = paste0(fnh, ".RData"))
}

#####@> Cluster analyses for regY1 region...
regtype <- "regY1"
for (r in unique(dat$regY1)) {
    fnh <- paste(flag, regtype, r, sep = "_")
    dataset <- clust_PCA_run(r = r, ddd = dat, allsp = use_splist,
                             allabs = allabs, regtype = regtype,
                             ncl = nclB[r], plotPCA = FALSE,
                             clustid = "tripidmon",
                             allclust = FALSE, ll5 = TRUE, flag = flag,
                             fnhead = fnh, covarnames = cvn)
    save(dataset, file = paste0(fnh, ".RData"))
}

#####@> Cluster analyses for both regions integrated...
dat2 <- dat
dat2$reg <- 1
nclB <- 4
cvn <- c("yrqtr", "latlong", "hooks", "hbf", "vessid", "Total", "lat",
         "lon", "lat5", "lon5", "op_yr", "tripidmon", "regB", "regB1",
         "regY", "regY1", "reg")
allabs <- c("vessid", "yrqtr", "latlong", "op_yr", "hbf", "hooks",
            "tripidmon", use_splist, "Total", "lat", "lon", "lat5",
            "lon5", "regB", "regB1", "regY", "regY1", "reg")

regtype <- "reg"
for (r in unique(dat2$reg)) {
    fnh <- paste(flag, regtype, "All_Regions_Integrated", r, sep = "_")
    dataset <- clust_PCA_run(r = r, ddd = dat2, allsp = use_splist,
                             allabs = allabs, regtype = regtype,
                             ncl = nclB[r], plotPCA = FALSE,
                             clustid = "tripidmon",
                             allclust = FALSE, ll5 = TRUE, flag = flag,
                             fnhead = fnh, covarnames = cvn)
    save(dataset, file = paste0(fnh, "All_Regions_Integrated", ".RData"))
}

########################################################################
######@> Standardization CPUE...

######@> Perceptions about cluster analysis...
## Brazil only, clusters, HBF
## R2 - 5 clusters. G01 = yft (sai, whm, sma, bum), G02 = bet (yft, bum,
## bsh, whm, sma), G03 = bsh (whm, sma), G04 = alb (bum, whm, sma, G05 =
## swo (sai, bum, whm, sma)). Use 1, 2, 3, 4, 5
## R3 - 3 clusters. G01 = bsh (sma, swo), G02 = swo (bsh, whm), G03 = yft
## (alb, bet, swo, bum, sma). Use 1, 2, 3

######@> Defining the new folder for the next analyses...
resdir <- "../analyses/std_cl_BRonly_hbf"
## dir.create(resdir)
setwd(resdir)

####@> defining the projdir...
projdir <- "/mnt/Dados/Dropbox/Works/CPGs/SCC-CPG_Atuns/R-Work/Padronizacao_CPUE_YFT/ICCAT/2019_Yellowfin/"

######@> Defining the species list...
splist <- c("alb", "bet", "yft", "swo", "bsh", "whm", "sai", "sma",
            "bum")

######@> Variables to standardization...
stdlabs <- c("vessid", "yrqtr", "latlong", "op_yr", "op_mon", "hbf",
             "hooks", splist, "hcltrp", "Total", "lat", "lon", "lat5",
             "lon5", "regB", "regY1", "flag")

######@> Defining the clusters for the standardization...

#####@> Joint standardization...
clkeepJP_Y <- list("yft" = list(c(1, 2, 4), c(1, 2, 3, 4), c(1, 2, 3)))
clkeepKR_Y <- list("yft" = list(c(0), c(1, 2, 3, 4), c(1, 2, 3)))
clkeepBR_Y <- list("yft" = list(c(1, 2, 3, 4), c(1, 2, 3, 4, 5), c(1, 2, 3, 4)))
clkeepTW_Y <- list("yft" = list(c(4), c(2, 3), c(0)))
clkeepUS_Y <- list("yft" = list(c(2, 3), c(1, 3), c(0)))
clk_Y <- list(JP = clkeepJP_Y, KR = clkeepKR_Y, BR = clkeepBR_Y,
              TW = clkeepTW_Y, US = clkeepUS_Y)

######@> Brazil only...
## clk_Y <- list(BR = clkeepBR_Y)

######@> Defining the parameters...
runpars <- list()
runpars[["yft"]] <- list(regtype = "regY1", regtype2 = "Y1", clk = clk_Y,
                         doregs = 2:3, addcl = TRUE, dohbf = TRUE,
                         cltype = "hcltrp")
runreg = 1; runsp = "yft"
keepd = TRUE; maxyr = 2017; maxqtrs = 200; minqtrs_byreg = c(5, 5, 5)

######@> Running standardization in steps...

#####@> Region Y1 02...

####@> Loading datasets from cluster folder...
load("../../clustering/BR_regY1_2.RData")
jdat02 <- dataset; rm(dataset)

####@> Defining data for glm...
glmdat <- select_data_JointIO(jdat02,
                              runreg = 2,
                              clk = clk_Y,
                              minqtrs = 4,
                              runsp = "yft",
                              mt = "deltabin",
                              vars = c("vessid", "hooks", "yrqtr",
                                       "latlong", "hbf"),
                              oneflag = "BR",
                              maxqtrs = 500,
                              minvess = 10,
                              minll = 10,
                              minyrqtr = 50,
                              yrlims = c(1990, 2017),
                              addcl = TRUE,
                              cltype = "hcltrp",
                              addpca = NA,
                              samp = NA,
                              strsmp = NA)

#####@> LognC Models...

####@> Defining the constant...
mn <- with(glmdat, 0.1 * mean(get(runsp)/hooks))

####@> Defining the weights per area...
wtt.all <- mk_wts(glmdat, wttype = "area")

####@> Running the model for no vessels included...

###@> Defining the formula...
form01 <- make_formula_IO(runsp, modtype = "logn", dohbf = TRUE, addboat = FALSE,
                          addcl = TRUE, nhbf = 3)

###@> Defining the labs for the models outputs...
modlab <- "lognC_novess_allyrs"
fname <- "BR_regY1_R02"

###@> Running the first model...
model <- glm(form01, data = glmdat, weights = wtt.all, family = "gaussian")

###@> Summary the results...
summarize_and_store(mod = model, dat = glmdat, fname, modlab,
                    dohbf = TRUE, keepd = TRUE)

####@> Running the model for vessels included...

###@> Defining the formula...
form02 <- make_formula_IO(runsp, modtype = "logn", dohbf = TRUE, addboat = TRUE,
                          addcl = TRUE, nhbf = 3)

###@> Defining the labs for the models outputs...
modlab <- "lognC_boat_allyrs"
fname <- "BR_regY1_R02"

###@> Running the first model...
model <- glm(form02, data = glmdat, weights = wtt.all, family = "gaussian")

###@> Summary the results...
summarize_and_store(mod = model, dat = glmdat, fname, modlab,
                    dohbf = TRUE, keepd = TRUE)

#####@> Delta-LogNormal Models...

####@> Running the model for no vessels included...

###@> Defining the labs for the models outputs...
modlab <- "dellog_novess_allyrs"
fname <- "BR_regY1_R02"

###@> Running the model...
do_deltalog(dat = glmdat, dohbf = TRUE, addboat = FALSE, addcl = TRUE,
            nhbf = 3, runsp = "yft", fname = fname, modlab = modlab,
            keepd = TRUE)

####@> Running the model for vessels included...

###@> Defining the labs for the models outputs...
modlab <- "dellog_boat_allyrs"
fname <- "BR_regY1_R02"

###@> Running the model...
do_deltalog(dat = glmdat, dohbf = TRUE, addboat = TRUE, addcl = TRUE,
            nhbf = 3, runsp = "yft", fname = fname, modlab = modlab,
            keepd = TRUE)

#####@> Region Y1 03...

####@> Loading datasets from cluster folder...
load("../../clustering/BR_regY1_3.RData")
jdat03 <- dataset; rm(dataset)

####@> Defining data for glm...
glmdat <- select_data_JointIO(jdat03,
                              runreg = 3,
                              clk = clk_Y,
                              minqtrs = 4,
                              runsp = "yft",
                              mt = "deltabin",
                              vars = c("vessid", "hooks", "yrqtr",
                                       "latlong", "hbf"),
                              oneflag = "BR",
                              maxqtrs = 500,
                              minvess = 10,
                              minll = 10,
                              minyrqtr = 50,
                              addcl = TRUE,
                              yrlims = c(1990, 2017),
                              cltype = "hcltrp",
                              addpca = NA,
                              samp = NA,
                              strsmp = NA)

#####@> LognC Models...

####@> Defining the constant...
mn <- with(glmdat, 0.1 * mean(get(runsp)/hooks))

####@> Defining the weights per area...
wtt.all <- mk_wts(glmdat, wttype = "area")

####@> Running the model for no vessels included...

###@> Defining the formula...
form01 <- make_formula_IO(runsp, modtype = "logn", dohbf = TRUE, addboat = FALSE,
                          addcl = TRUE, nhbf = 3)

###@> Defining the labs for the models outputs...
modlab <- "lognC_novess_allyrs"
fname <- "BR_regY1_R03"

###@> Running the first model...
model <- glm(form01, data = glmdat, weights = wtt.all, family = "gaussian")

###@> Summary the results...
summarize_and_store(mod = model, dat = glmdat, fname, modlab,
                    dohbf = TRUE, keepd = TRUE)

####@> Running the model for vessels included...

###@> Defining the formula...
form02 <- make_formula_IO(runsp, modtype = "logn", dohbf = TRUE, addboat = TRUE,
                          addcl = TRUE, nhbf = 3)

###@> Defining the labs for the models outputs...
modlab <- "lognC_boat_allyrs"
fname <- "BR_regY1_R03"

###@> Running the first model...
model <- glm(form02, data = glmdat, weights = wtt.all, family = "gaussian")

###@> Summary the results...
summarize_and_store(mod = model, dat = glmdat, fname, modlab,
                    dohbf = TRUE, keepd = TRUE)

#####@> Delta-LogNormal Models...

####@> Running the model for no vessels included...

###@> Defining the labs for the models outputs...
modlab <- "dellog_novess_allyrs"
fname <- "BR_regY1_R03"

###@> Running the model...
do_deltalog(dat = glmdat, dohbf = TRUE, addboat = FALSE, addcl = TRUE,
            nhbf = 3, runsp = "yft", fname = fname, modlab = modlab,
            keepd = TRUE)

####@> Running the model for vessels included...

###@> Defining the labs for the models outputs...
modlab <- "dellog_boat_allyrs"
fname <- "BR_regY1_R03"

###@> Running the model...
do_deltalog(dat = glmdat, dohbf = TRUE, addboat = TRUE, addcl = TRUE,
            nhbf = 3, runsp = "yft", fname = fname, modlab = modlab,
            keepd = TRUE)

#####@> Regions Integrated in One...

####@> Loading datasets from cluster folder...
load("../../clustering/BR_reg_All_Regions_Integrated_1All_Regions_Integrated.RData")
jdatAll <- dataset; rm(dataset)

####@> Defining data for glm...
glmdat <- select_data_JointIO(jdatAll,
                              runreg = 1,
                              clk = clk_Y,
                              minqtrs = 2,
                              runsp = "yft",
                              mt = "deltabin",
                              vars = c("vessid", "hooks", "yrqtr",
                                       "latlong", "hbf"),
                              oneflag = "BR",
                              maxqtrs = 500,
                              minvess = 5,
                              minll = 3,
                              minyrqtr = 100,
                              addcl = TRUE,
                              yrlims = c(1990, 2017),
                              cltype = "hcltrp",
                              addpca = NA,
                              samp = NA,
                              strsmp = NA)

#####@> LognC Models...

####@> Defining the constant...
mn <- with(glmdat, 0.1 * mean(get(runsp)/hooks))

####@> Defining the weights per area...
wtt.all <- mk_wts(glmdat, wttype = "area")

####@> Running the model for no vessels included...

###@> Defining the formula...
form01 <- make_formula_IO(runsp, modtype = "logn", dohbf = TRUE, addboat = FALSE,
                          addcl = TRUE, nhbf = 3)

###@> Defining the labs for the models outputs...
modlab <- "lognC_novess_allyrs"
fname <- "BR_Reg_All_Integrated"

###@> Running the first model...
model <- glm(form01, data = glmdat, weights = wtt.all, family = "gaussian")

###@> Summary the results...
summarize_and_store(mod = model, dat = glmdat, fname, modlab,
                    dohbf = TRUE, keepd = TRUE)

####@> Running the model for vessels included...

###@> Defining the formula...
form02 <- make_formula_IO(runsp, modtype = "logn", dohbf = TRUE, addboat = TRUE,
                          addcl = TRUE, nhbf = 3)

###@> Defining the labs for the models outputs...
modlab <- "lognC_boat_allyrs"
fname <- "BR_Reg_All_Integrated"

###@> Running the first model...
model <- glm(form02, data = glmdat, weights = wtt.all, family = "gaussian")

###@> Summary the results...
summarize_and_store(mod = model, dat = glmdat, fname, modlab,
                    dohbf = TRUE, keepd = TRUE)

#####@> Delta-LogNormal Models...

####@> Running the model for no vessels included...

###@> Defining the labs for the models outputs...
modlab <- "dellog_novess_allyrs"
fname <- "BR_Reg_All_Integrated"

###@> Running the model...
do_deltalog(dat = glmdat, dohbf = TRUE, addboat = FALSE, addcl = TRUE,
            nhbf = 3, runsp = "yft", fname = fname, modlab = modlab,
            keepd = TRUE)

####@> Running the model for vessels included...

###@> Defining the labs for the models outputs...
modlab <- "dellog_boat_allyrs"
fname <- "BR_Reg_All_Integrated"

###@> Running the model...
do_deltalog(dat = glmdat, dohbf = TRUE, addboat = TRUE, addcl = TRUE,
            nhbf = 3, runsp = "yft", fname = fname, modlab = modlab,
            keepd = TRUE)

########################################################################
##
##                 Creative Commons License 4.0
##                       (CC BY-NC-SA 4.0)
##
##  This is a humam-readable summary of (and not a substitute for) the
##  license (https://creativecommons.org/licenses/by-nc-nd/4.0/legalcode)
##
##  You are free to:
##
##  Share - copy and redistribute the material in any medium or format.
##
##  The licensor cannot revoke these freedoms as long as you follow the
##  license terms.
##
##  Under the following terms:
##
##  Attribution - You must give appropriate credit, provide a link to
##  license, and indicate if changes were made. You may do so in any
##  reasonable manner, but not in any way that suggests the licensor
##  endorses you or your use.
##
##  NonCommercial - You may not use the material for commercial
##  purposes.
##
##  ShareAlike - If you remix, transform, or build upon the material,
##  you must distributive your contributions under the same license
##  as the  original.
##
##  No additional restrictions — You may not apply legal terms or
##  technological measures that legally restrict others from doing
##  anything the license permits.
##
########################################################################
