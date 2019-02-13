projdir <- "~/IATTC/2019_CPUE/"
jpdir <- paste0(projdir, "JP/")
datadir <- paste0(jpdir, "data/")
lendat_dir <- paste0(datadir, "Size_20181220/")
jlength_dir <- paste0(jpdir, "length/")
dir.create(jlength_dir)

setwd(jlength_dir)

library("date")
library(splines)
library("maps")
library("mapdata")
library("maptools")
library("data.table")
library("lunar")
#install.packages("devtools")
#devtools::install_github("hadley/readr")
library(lubridate)
library(readr)
library(plyr)
library(dplyr)
library(dtplyr)
library(tm)

source("~/GitHub_Rstd/cpue.rfmo/scripts/Utility/support_functions.r")
source("~/GitHub_Rstd/cpue.rfmo/scripts/Utility/Length_support_functions.R")

library(cpue.rfmo)
xsd # stop!

# a <- list.files(pattern="model")
# f=a[1]
# for (f in a) {
#     load(f)
#     x <- ls()[3]
#     summ <- summary(get(x))
#     save(summ,file=paste0("summ",x,".RData") )
#     rm(summ,x)
#     ls()
#     }


######################################################
# Another option for the data - it depends what the exact format is
##################
nms <- c("species","year","quarter","month","day","resolution","latitude","latitude_code","longitude","longitude_code","fleet", "vessel_type", "unit", "place", "sex", "size_class", "number")
wdths <- c(2,4,1,2,2,1,2,1,3,1,2,1,1,2,1,3,4)
cc <- c("iiiiiiiiiiiiiiiii")
posses <- cumsum(c(1,wdths))
cbind(nms,wdths,unlist(strsplit(cc,"")))

load(paste0(lendat_dir, ".RData"))
sz <- prep_len_data(comp.d)

# Code
# species: 1: bluefin tuna, 2: southern bluefin tuna, 3: albacore, 4: bigeye tuna, 5: yellowfin tuna, 6: swordfish
# 7: striped marlin, 8: blue marlin, 9: black marlin, 10: sailfish, 11: shortbill spearfish, 12: skipjack
# resolution: resolution in degrees of latitude x longitude (1; 10  x 20, 5 x 10, 3; 5 x 5, 4; 1 x 1)
# latitude_code: 1=N, 2=S
# longitude_code: 1=E, 2=W
# fleet (gear): 1: longline, 2:longline (night setting)
# vessel_type (type of vessel): 1: commercial vessel, >2: training and research vessel
# unit:  1: kg, 2:  1 cm, 3: 1 kg, 4: 2 kg, 5: 5 kg, 6: 1 cm, 7: 2 cm, 8: 5 cm
# place: 1: On board (by fisherman), 2-8 and 10-12: port sampling (2: Kagoshima, 3: Katsuura, 4: Yaizu, 5: Shimizu, 6:Tokyo, 7: Shiogama, 8:Kesennuma, 10: Sakai-minato, 11: Kamaishi, 12: Misakai), 9: SBT monitor ship, 13: On board (by Observer)
# sex: 1: female, 2: male, 3: unknown (or 0 is unknown?)
# size_class: size class
# number: Number of specimen


# a <- read_fwf(file=paste0(datadir,"/Sizeall.txt"),fwf_widths(wdths),col_types=cc,n_max=20);gc()
# names(a) <- nms
# a[,1:10]
# a[,11:17]
# rawlen <- read_fwf(file=paste0(datadir,"/Sizeall.txt"),fwf_widths(wdths),col_types=cc)
# problems(rawlen)
# names(rawlen) <- nms
# rawlen[4705:4720,]
# getwd()
# dir.create("./test")
# setwd("./test")
# save(rawlen, file = "rawlen.RData")
# load(file = "rawlen.RData")
#
# sz <- prep_len_data(rawlen)
# save(sz, file = "sz.RData")
# load(file = "sz.RData")
#
# a <- with(sz, tapply(number, list(species, place, sex, unit, vessel_type, resolution, year), sum, na.rm = TRUE))
# save(a, file = "nums_by_class.RData")
# a[5,1,2,,,4,40]

tapply(sz$number,sz$year,sum)
table(is.na(sz$year), sz$year)
#table(sz$place, sz$year, useNA = "always")
table(sz$sex, sz$year, useNA = "always")
table(sz$unit, sz$year, useNA = "always")
table(sz$vessel_type, sz$year, useNA = "always")
table(sz$fleet, sz$year, useNA = "always")
table(is.na(sz$day), sz$year)
a <- sz
a$dd <- is.na(a$month)
# All levels except 1x1 always use monthly
# 1x1 uses monthly until 1985, then switches 100% to daily
tabfun(a=a, ppp = "dd")
tabfun2(a=a, pp1 = "dd", pp2="unit")
tabfun2(a=a, pp1 = "dd", pp2="resolution")
tabplot(a=a[a$resolution==4,], pp1 = "dd", key=list(leg2=c("day","month")), legloc = "right") #
tabplot(a=a, pp1 = "dd", key=list(leg2=c("day","month")), legloc = "right")

# Report on the data structures
#kplace=data.frame(k=c(sort(unique(sz$place)),99),
#                  legs=c("Vessel fisher", "PS Yaizu", "PS Shimizu", "PS Tokyo", "SBT monitor ship", "Vessel Observer","Not recorded"))
ksex=data.frame(k=c(sort(unique(sz$sex))), legs=c("Unknown","Female", "Male", "Not recorded"))
kunit=data.frame(k=c(sort(unique(sz$unit))), legs=c("NA","1 kg", "1 cm", "2 cm", "5 cm"))
kvesselc=data.frame(k=c(sort(unique(sz$vessel_type))), legs=c("Commercial","Trn + Rsrch"))
kresolution=data.frame(k=c(sort(unique(sz$resolution))), legs=c("10  x 20", "5 x 10", "5 x 5", "1 x 1"))
# kspecies=data.frame(k=c(sort(unique(sz$species)),99),
#                     legs=c("1: bluefin tuna","2: southern bluefin tuna","3: albacore","4: bigeye tuna","5: yellowfin tuna","swordfish", "7: striped marlin","8: blue marlin","9: black marlin","10: sailfish","11: shortbill spearfish","12: skipjack","NA"))

kspecies=data.frame(k=c(sort(unique(sz$species))),legs=c("4: bigeye tuna","5: yellowfin tuna"))

keys <- list(sex=ksex, unit=kunit, vessel_type=kvesselc, resolution=kresolution, species=kspecies)

y_only <- sz[sz$species == 5,]
b_only <- sz[sz$species == 4,]

#tabfun(a=sz, ppp = "place")
tabfun(a=sz, ppp = "sex")
tabfun(a=sz[sz$species == 5,], ppp = "sex")
tabfun(a=sz, ppp = "vessel_type")
tabfun(a=sz, ppp = "fleet")
tabfun(a=sz, ppp = "resolution")
tabfun(a=sz, ppp = "unit")
tabfun(a=sz, ppp = "species")
tabfun(a=b_only, ppp = "place")


a=sz[sz$species %in% c(4,5) & sz$year %in% 1970:2005 & sz$size_class %in% 100:120 & sz$unit == 7,]
tapply(a$number,list(a$year,a$size_class), sum)


tabfun2(a=sz, pp1 = "place", pp2="vessel_type")
tabfun2(a=sz, pp1 = "sex", pp2="vessel_type")
tabfun2(a=sz, pp1 = "sex", pp2="vessel_type")
tabfun2(a=sz, pp1 = "unit", pp2="vessel_type")
tabfun2(a=sz, pp1 = "unit", pp2="resolution")
tabfun2(a=sz, pp1 = "resolution", pp2="vessel_type")

tabfun3(a=sz, pp1="year", pp2 = "resolution", pp3="vessel_type")


tabplot(a=sz, pp1 = "unit", key=list(leg2=c("1 kg", "1 cm", "2 cm", "5 cm")), legloc = "right")
savePlot("Size units by year.png", type = "png")
#tabplot(a=sz, pp1 = "place", key=list(leg2=c("On board fisherman", "PS Yaizu", "PS Shimizu"#, "PS Tokyo", "SBT monitor ship", "On board Observer","Not recorded")),
#        legloc = "bottomleft")
#savePlot("Size sampling location by year.png", type = "png")
tabplot(a=sz, pp1 = "sex", key=list(leg2=c("Unknown","Female", "Male", "Not recorded")),legloc = "bottomleft")
savePlot("Size sex by year.png", type = "png")
tabplot(a=sz, pp1 = "vessel_type", key=list(leg2=c("Commercial","Training and research")),legloc = "bottomleft")
savePlot("Size vessel class by year.png", type = "png")
tabplot(a=sz, pp1 = "resolution", key=list(leg2=c("10  x 20", "5 x 10", "5 x 5", "1 x 1")),legloc = "bottomright")
savePlot("Size spatial resolution by year.png", type = "png")
tabplot(a=sz, pp1 = "resolution", key=list(leg2=c("10  x 20", "5 x 10", "5 x 5", "1 x 1")),legloc = "bottomright")
savePlot("Size spatial resolution by year.png", type = "png")

a <- sz[! sz$species %in% c(1,2,6,7,8, 9, 10, 11, 12),]
tabplot(a=a, pp1 = "species", key=list(leg2=c("bigeye tuna","yellowfin tuna")),legloc = "top")
savePlot("Size species by year.png", type = "png")
tabplot_nums(a=a, pp1 = "species", key=list(leg2=c("albacore tuna","bigeye tuna","yellowfin tuna")),legloc = "top")
savePlot("Size species nums by year.png", type = "png")


spname <- c("All","","Albacore","Bigeye","Yellowfin")
windows(); par(mfrow = c(2,2), mar = c(4, 4, 3, 1)+.1)
for(sp in c(1, 5, 4, 3)) {
  if(sp==1) a <- sz else a <- sz[sz$species == sp,]
  tabplot(a, pp1 = "unit", key=list(leg2=c("1 kg", "1 cm", "2 cm", "5 cm")), legloc="right", main = spname[sp],neww=F, doleg=(sp==1))
}
savePlot("Size units grid by year.png", type = "png")

windows(); par(mfrow = c(2,2), mar = c(4, 4, 3, 1) + .1)
for(sp in c(1, 5, 4, 3)) {
  kkx = keys[["unit"]]
  if(sp==1) a <- sz else a <- sz[sz$species == sp,]
  tabplot2(a, pp1 = "unit", legloc="right", main = spname[sp],neww=F, doleg=(sp==1), kk=kkx)
}
savePlot("Size units grid by year.png", type = "png")

windows(); par(mfrow = c(2,2), mar = c(4, 4, 3, 1) + .1)
for(sp in c(5,4)) {
  kkx = keys[["unit"]]
  if(sp==1) a <- sz else a <- sz[sz$species == sp,]
  tabplot2(a, pp1 = "dd", legloc="right", main = spname[sp],neww=F, doleg=(sp==1), kk=kkx)
}
savePlot("Size units grid by year.png", type = "png")

tabplot(a=a, pp1 = "dd", key=list(leg2=c("day","month")), legloc = "right")


windows(); par(mfrow = c(2,2), mar = c(4, 4, 3, 1)+.1)
for(sp in c(1, 5, 4, 3)) {
  kkx = keys[["place"]]
  if(sp==1) a <- sz else a <- sz[sz$species == sp,]
  tabplot2(a, pp1 = "place", legloc="left", main = spname[sp],neww=F, doleg=(sp==5), kk=kkx)
}
savePlot("Size samploc grid by year.png", type = "png")

windows(); par(mfrow = c(2,2), mar = c(4, 4, 3, 1)+.1)
for(sp in c(1, 5, 4, 3)) {
  kkx = keys[["sex"]]
  if(sp==1) a <- sz else a <- sz[sz$species == sp,]
  tabplot2(a, pp1 = "sex", legloc="left", main = spname[sp],neww=F, doleg=(sp==5), kk=kkx)
}
savePlot("Size sex grid by year.png", type = "png")

windows(); par(mfrow = c(2,2), mar = c(4, 4, 3, 1)+.1)
for(sp in c(1, 5, 4, 3)) {
  kkx = keys[["resolution"]]
  if(sp==1) a <- sz else a <- sz[sz$species == sp,]
  tabplot2(a, pp1 = "resolution", legloc="right", main = spname[sp],neww=F, doleg=(sp==3), kk=kkx)
}
savePlot("Size resolution grid by year.png", type = "png")

windows(); par(mfrow = c(2,2), mar = c(4, 4, 3, 1)+.1)
for(sp in c(1, 5, 4, 3)) {
  kkx = keys[["vessel_type"]]
  if(sp==1) a <- sz else a <- sz[sz$species == sp,]
  tabplot2(a, pp1 = "vessel_type", legloc="right", main = spname[sp],neww=F, doleg=(sp==3), kk=kkx)
}
savePlot("Size vessel_type grid by year.png", type = "png")

# Look at bigeye size data through time
bet <- sz[sz$species==4 & sz$unit %in% c(6,7),]
dim(sz); dim(bet)
tapply(bet$number,bet$year,sum)

bet1 <- sz[sz$species==4 & sz$unit %in% c(7),]
a <- aggregate(bet1$number, by=list(bet1$size_class), sum)
windows(width=20, height=10); par(mfrow=c(3,1), mar=c(4,2,2,1))
plot(a[,1], a$x, type = "h", xlim=c(50, 100))
plot(a[,1], a$x, type = "h", xlim=c(100, 150))
plot(a[,1], a$x, type = "h", xlim=c(150, 200))
savePlot("plot bet 2cm", type="png")

bet1 <- sz[sz$species==4 & sz$unit %in% c(6),]
a <- aggregate(bet1$number, by=list(bet1$size_class), sum)
windows(width=20, height=10); par(mfrow=c(3,1), mar=c(4,2,2,1))
plot(a[,1], a$x, type = "h", xlim=c(50, 100))
plot(a[,1], a$x, type = "h", xlim=c(100, 150))
plot(a[,1], a$x, type = "h", xlim=c(150, 200))
savePlot("plot bet 1cm", type="png")

# Look at yft size data through time
yft <- sz[sz$species==5 & sz$unit %in% c(6,7),]
dim(sz); dim(yft)
tapply(yft$number,yft$year,sum)

yft1 <- sz[sz$species==5 & sz$unit %in% c(7),]
a <- aggregate(yft1$number, by=list(yft1$size_class), sum)
windows(width=20, height=10); par(mfrow=c(3,1), mar=c(4,2,2,1))
plot(a[,1], a$x, type = "h", xlim=c(50, 100))
plot(a[,1], a$x, type = "h", xlim=c(100, 150))
plot(a[,1], a$x, type = "h", xlim=c(150, 200))
savePlot("plot yft 2cm", type="png")

yft1 <- sz[sz$species==5 & sz$unit %in% c(6),]
a <- aggregate(yft1$number, by=list(yft1$size_class), sum)
windows(width=20, height=10); par(mfrow=c(3,1), mar=c(4,2,2,1))
plot(a[,1], a$x, type = "h", xlim=c(50, 100))
plot(a[,1], a$x, type = "h", xlim=c(100, 150))
plot(a[,1], a$x, type = "h", xlim=c(150, 200))
savePlot("plot yft 1cm", type="png")

for(dec in seq(1975, 2015, 5)) {
  b <- sz[sz$species %in% c(5) & sz$unit %in% c(7) & sz$year %in% dec:(dec+4),]
  if(dim(b)[1] > 0) {
  a <- aggregate(b$number, by=list(b$size_class), sum)
  windows(width=20, height=10); par(mfrow=c(3,1), mar=c(4,2,2,1))
  plot(a[,1], a$x, type = "h", xlim=c(50, 100), main = dec)
  plot(a[,1], a$x, type = "h", xlim=c(100, 150))
  plot(a[,1], a$x, type = "h", xlim=c(150, 200))
  savePlot(paste0("plot yft 1cm dec",dec), type="png")
}}

kk <- c("","SBT","","BET","YFT", "SWO","MLS","BUM","BLM","SAL","SBS")
xl <- rbind(c(0,0),c(140,185),c(0,0), c(85, 155), c(85, 155))
for(sp in c(4,5)) {
  windows(width=12, height=12); par(mfcol=c(6,1), mar=c(2,2,2,1), oma=c(0,0,1,0))
  for(dec in seq(1950, 2015, 5)) {
    b <- sz[sz$species == sp & sz$unit %in% c(7) & sz$year %in% dec:(dec+4),]
    if(dim(b)[1] > 0) {
      a <- aggregate(b$number, by=list(b$size_class), sum)
      plot(a[,1], a$x, type = "h", xlim=xl[sp,], main = dec)
    }
  }
  title(main = kk[sp], outer = TRUE, line=0)
  savePlot(file=paste0("plotlf_5yr_",kk[sp],".png"), type="png")
}

# for(sp in c(6,7,8,9,10,11)) {
#   windows(width=12, height=12); par(mfcol=c(6,1), mar=c(2,2,2,1), oma=c(0,0,1,0))
#   for(dec in seq(1950, 2015, 5)) {
#     b <- sz[sz$species == sp & sz$unit %in% c(8) & sz$year %in% dec:(dec+4),]
#     if(dim(b)[1] > 0) {
#       a <- aggregate(b$number, by=list(b$size_class), sum)
#       plot(a[,1], a$x, type = "h", xlim = c(90, 200), main = dec)
#     }
#   }
#   title(main = kk[sp], outer = TRUE, line=0)
#   #  savePlot(file=paste0("plotlf_5yr_",kk[sp],".png"), type="png")
# }

windows(width=12, height=12); par(mfcol=c(6,2), mar=c(2,2,2,1), oma=c(0,0,1,0))
for(dec in seq(1950, 2015, 5)) {
  b <- sz[sz$unit %in% c(8) & sz$year %in% dec:(dec+4),]
  if(dim(b)[1] > 0) {
    a <- aggregate(b$number, by=list(b$size_class), sum)
    plot(a[,1], a$x, type = "h", xlim = c(90, 200), main = dec)
  }
}
#title(main = kk[sp], outer = TRUE, line=0)
#  savePlot(file=paste0("plotlf_5yr_",kk[sp],".png"), type="png")

b <- sz[sz$species %in% c(6:11)  & sz$unit %in% c(8) & sz$size_class %in% 100:130,]
tapply(b$number, list(b$year,b$size_class), sum)

b <- sz[sz$species %in% c(4,5),]
tapply(b$number, list(b$unit,b$year), sum)

b <- sz[sz$species == 5 & sz$unit %in% c(7),]
tapply(b$number, b$year, sum)
evens <- seq(50, 188, 2)

b <- sz[sz$species == 5 & sz$unit %in% c(7) & sz$size_class %in% evens & sz$year %in% 1970:1990,]
tapply(b$number, list(b$year, b$size_class), sum)

tapply(sz$number,list(sz$species,sz$unit),sum)

loc <- yft$unit==7 & yft$size_class %in% seq(2,300,2)
table(loc, yft$year) # Identify where the off bins are
# loc      1952  1953  1954  1955  1956  1957  1958  1959  1960  1961  1962  1963  1964
# FALSE     0     0     0     0     0     0     0     0     0     0     0     0     0
# TRUE    362   920  1716  3571  2955  1123  2298  2381  3308  1943  2645   993  2057
#
# loc      1965  1966  1967  1968  1969  1970  1971  1972  1973  1974  1975  1976  1977
# FALSE    30    12     1    18    11 13582 15109 15081 10292 10681 12867 14627 13674
# TRUE   9511 10947  6677 10639 12730     9    16     9    11    12     3    11     7
#
# loc      1978  1979  1980  1981  1982  1983  1984  1985  1986  1987  1988  1989  1990
# FALSE 10870 12145  9380 10289 11077 12298 13390 11313  8133  7090  6183  6534  7684
# TRUE      3     3     1     0     1     1     0     0     3     2     0     0     0
#
# loc      1991  1992  1993  1994  1995  1996  1997  1998  1999  2000  2001  2002  2003
# FALSE  3619  1349  2691  3285  4778  2794  5425  8639  3864  5939  4110  1120   859
# TRUE      0     0     0     0     0     0     0     0  2645   692   341     0     0
#
# loc      2004  2005  2006  2007  2008  2009  2010  2011  2012  2013  2014  2015  2016
# FALSE   996   938  2178  1357  1376   252   184   214   147    24    78     1   163
# TRUE      0    95   583     0     0     0     0     0     0     0     0     0     0

# Create new variable cls2 with the even number units adjusted up.
bet$cls2 <- bet$size_class
loc <- bet$unit==7 & bet$size_class %in% seq(2,300,2)
table(loc, bet$year) # Identify where the off bins are
bet$cls2[loc] <- bet$size_class[loc] - 1

bet1 <- bet[bet$unit %in% c(7),]
a <- aggregate(bet1$number, by=list(bet1$cls2), sum)
windows(width=20, height=10); par(mfrow=c(3,1), mar=c(4,2,2,1))
plot(a[,1], a$x, type = "h", xlim=c(50, 100))
plot(a[,1], a$x, type = "h", xlim=c(100, 150))
plot(a[,1], a$x, type = "h", xlim=c(150, 200))
savePlot("plot bet 2cm shifted", type="png")
table(bet1$cls2)

table(sz$species, sz$unit)
#         3      6      7      8
# 1       5      2      0      0
# 2  161981 683959  19795   4761
# 3    7452 170598    586    742
# 4  120532 163449 265454   1020
# 5  108399 105498 264477   1940
# 6    5363  18910    325  19878
# 7    9701  10527     35  29876
# 8    7835   6453     67  17229
# 9    5741   3730     74  28551
# 10    845   1096      0   9424
# 11     43    522      0   2408
# 12      4  14600      0      0

# Where are the 1x1 bins wrt lengths etc
a <- sz[sz$species==4,] # bet
table(a$unit, a$resolution)
#        1      2      4
# 3  11419  65175  43938  # 1kg, have a lot of 10x20 and 5x10, first 1x1 in 1986
# 6     31     21 163397  # 1cm, almost all 1x1
# 7    128  13922 251404  # 2cm, mostly 1x1, others mostly in 65, 66
# 8      0      0   1020  # 5cm, all 1x1

a <- sz[sz$species==5,] # yft
table(a$unit, a$resolution)
#        1      2      4
# 3   8274  66621  33504
# 6     45     12 105441
# 7    475  46492 217510
# 8      0      0   1940

a <- sz[sz$species %in% c(4,5),] # yft
# Check the class 7 (2cm) that have resolution 1 or 2 (20x10 or 5x10)
with(a[a$unit==7 & a$resolution %in% c(1,2),],table(year))
# year
# 1952  1953  1954  1955  1956  1957  1958  1959  1960  1961  1962  1963  1964  1965  1966
# 362   920  1716  3571  2955  1123  2298  2381  3308  1943  2645   993  2057 16741 17570
# 1967  1968  1970
# 95   317    22

# resolutions for class 3 (1kg)
with(a[a$unit==3,],table(resolution, year))
# year
# resolution  1953  1956  1957  1958  1959  1960  1961  1962  1963  1964  1965  1966  1967  1968
# 1     0     0     0     0     0     0     0     0     0     0  4037  2560  2418  3328
# 2   184  1021  3283  2584  3177  2615  1817  2375  2073  3599  9385  9453  4621  6843
# 4     0     0     0     0     0     0     0     0     0     0     0     0     0     0
# year
# resolution  1969  1970  1971  1972  1973  1974  1975  1976  1977  1978  1979  1980  1981  1982
# 1  1038  1014  1544   363   352   606   463     0   168     0     0     0   178    76
# 2  3848  2966  4194   851   661  1200  1536   524   558  2259  2693   816   479  1804
# 4     0     0     0     0     0     0     0    47     0     0     0     0     0     0
# year
# resolution  1983  1984  1985  1986  1987  1988  1989  1990  1991  1992  1993  1994  1995  1996
# 1   454   438    92   259    77   124   104     0     0     0     0     0     0     0
# 2  8620 15439 23976  4629   169   529   792    87    68     0    68     0     0     0
# 4     0     0     0 18887 11616 13320  6182  7846  5593  5719  2175   817   723   294
# year
# resolution  1997  1998  1999  2000  2001  2003  2004  2005  2006  2009  2010  2011  2012  2013
# 1     0     0     0     0     0     0     0     0     0     0     0     0     0     0
# 2     0     0     0     0     0     0     0     0     0     0     0     0     0     0
# 4    11  3851     1    48    90     5    20    76     3     2     5    52    53     1
# year
# resolution  2014  2016
# 1     0     0
# 2     0     0
# 4     2     3

##############################################
# Standardize and plot year and spatial effects
table(sz$longitude, sz$longitude_code, useNA="always")
table(sz$latitude, sz$latitude_code, useNA="always")
tapply(sz$number, list(sz$lat1, sz$lon1), sum)
plot(tapply(sz$number, list(sz$lon1), sum))

table(sz$lon1)
table(sz$resolution)
bet1 <- sz[sz$species==4 & sz$resolution== 4,] # resolution 4 is 1x1
yft1 <- sz[sz$species==5 & sz$resolution== 4,] # resolution 4 is 1x1
str(bet1)
table(bet1$year)

a <- with(bet1, tapply(number, list(lon1,lat1), sum))
lons <- as.numeric(rownames(a))
lats <- as.numeric(colnames(a))
windows()
image(lons,lats,log(a), xlim=c(180, 290), ylim=c(-40, 40))
contour(lons,lats,log(a),add=T)
map("world2",add=T, fill=T)

for (sp in 4:5) {
  windows(height=14,width = 14); par(mfrow=c(3,3),mar = c(2,2,2,1), oma=c(0,0,2,0))
  dat <- sz[sz$species==sp & sz$resolution== 4,]
  for(dec in seq(1975, 2015, 5)) {
    b <- dat[dat$year %in% dec:(dec+4),]
    if(dim(b)[1] > 0) {
      b$lonf <- factor(b$lon1,levels=seq(min(b$lon1),max(b$lon1), 1))
      b$latf <- factor(b$lat1,levels=seq(min(b$lat1),max(b$lat1), 1))
      a <- with(b, tapply(number, list(lonf,latf), sum))
      lons <- as.numeric(rownames(a))
      lats <- as.numeric(colnames(a))
      image(lons,lats,log(a), xlim=c(180, 290), ylim=c(-40, 40), main=dec, xlab = "", ylab = "")
      # mtext(side = 1, text = "Longitude", line = 2, cex = .7)
      # mtext(side = 2, text = "Latitude", line = 2, cex = .7)
      contour(lons,lats,log(a),add=T)
#      map("world2",add=T, fill=T)
      map_EPO()
    }
  }
  title(kk[sp],outer=T)
  savePlot(paste("map", kk[sp], "1x1 samples by 5yr"), type="png")
}

# resolution (resolution), vessel_type(), unit
for (sp in 4:5) {
  windows(height=12,width = 12); par(mfrow=c(3,3),mar = c(4,4,2,1), oma=c(0,0,2,0))
  dat <- sz[sz$species==sp & sz$resolution== 2,]
  for(dec in seq(1975, 2015, 5)) {
    b <- dat[dat$year %in% dec:(dec+4),]
    if(dim(b)[1] > 0) {
      b$lonf <- factor(b$lon510,levels=seq(min(b$lon510),max(b$lon510), 10))
      b$latf <- factor(b$lat510,levels=seq(min(b$lat510),max(b$lat510), 5))
      a <- with(b, tapply(number, list(lonf,latf), sum))
      lons <- as.numeric(rownames(a))
      lats <- as.numeric(colnames(a))
      image(lons,lats,log(a), xlim=c(180, 290), ylim=c(-40,40), main=dec, xlab = "", ylab = "Latitude")
      mtext(side = 1, text = "Longitude", line = 2.5, cex = .7)
      contour(lons,lats,log(a),add=T)
      map_EPO()
    }
  }
  title(kk[sp],outer=T)
  savePlot(paste("map", kk[sp], "5x10 samples by 5yr"), type="png")
}

for (sp in 4:5) {
  windows(height=14,width = 12); par(mfrow=c(3,2),mar = c(4,4,2,1), oma=c(0,0,2,0))
  dat <- sz[sz$species==sp & sz$resolution== 1,]
  for(dec in seq(1975, 2015, 5)) {
    b <- dat[dat$year %in% dec:(dec+4),]
    if(dim(b)[1] > 0) {
      b$lonf <- factor(b$lon2010,levels=seq(min(b$lon2010),max(b$lon2010), 20))
      b$latf <- factor(b$lat2010,levels=seq(min(b$lat2010),max(b$lat2010), 10))
      a <- with(b, tapply(number, list(lonf,latf), sum))
      lons <- as.numeric(rownames(a))
      lats <- as.numeric(colnames(a))
      if(dim(a)[1] > 1) {
      image(lons,lats,log(a), xlim=c(180, 290), ylim=c(-40,40), main=dec, xlab = "", ylab = "Latitude")
      mtext(side = 1, text = "Longitude", line = 2.5, cex = .7)
      contour(lons,lats,log(a),add=T)
      map_EPO()
      }}
  }
  title(kk[sp],outer=T)
  savePlot(paste("map", kk[sp], "10x20 samples by 5yr"), type="png")
}

for (sp in 4:5) {
  windows(height=14,width = 14); par(mfrow=c(4,3),mar = c(2,2,2,1), oma=c(0,0,2,0))
  dat <- sz[sz$species==sp & sz$resolution== 4,]
  for(dec in seq(1990, 2001, 1)) {
    b <- dat[dat$year %in% dec:(dec+0),]
    if(dim(b)[1] > 0) {
      b$lonf <- factor(b$lon1,levels=seq(min(b$lon1),max(b$lon1), 1))
      b$latf <- factor(b$lat1,levels=seq(min(b$lat1),max(b$lat1), 1))
      a <- with(b, tapply(number, list(lonf,latf), sum))
      lons <- as.numeric(rownames(a))
      lats <- as.numeric(colnames(a))
      image(lons,lats,log(a), xlim=c(180, 290), ylim=c(-40,40), main=dec, xlab = "", ylab = "")
      # mtext(side = 1, text = "Longitude", line = 2, cex = .7)
      # mtext(side = 2, text = "Latitude", line = 2, cex = .7)
      contour(lons,lats,log(a),add=T)
      map_EPO()
    }
  }
  title(kk[sp],outer=T)
  savePlot(paste("map", kk[sp], "1x1 samples by 1yr"), type="png")
}


tabfun(a=sz[sz$species == 4 & sz$unit %in% c(6,7,8),], ppp = "resolution")
tabfun(a=sz[sz$species == 4 & sz$unit %in% c(3,4),], ppp = "resolution")
tabfun(a=sz[sz$species == 5 & sz$unit %in% c(6,7,8),], ppp = "resolution")
tabfun(a=sz[sz$species == 5 & sz$unit %in% c(3,4),], ppp = "resolution")

bet1 <- sz[sz$species==4 & sz$resolution== 4 & sz$vessel_type==1,] # resolution 4 is 1x1
bet1$num <- bet1$number
yft1 <- sz[sz$species==5 & sz$resolution== 4 & sz$vessel_type==1,] # resolution 4 is 1x1
yft1$num <- yft1$number

plot_resp_image_resp(dat=bet1,alat="lat1", alon="lon1", minlat=-10,maxlat=10,minlong=210,maxlong=250,minfrq=100,zl2=seq(40,800,by=20),ti="",fn="BET1_model",rng=c(50,200),respvar="cls2",sz_name="BET1_yq_sz_plot",mapname="BET1_map_comm_sz_plot",yq_mode=F,llmode=F, lenplot=T,newwindow=T, sp = "BET")

plot_resp_image_resp(dat=yft1,alat="lat1", alon="lon1", minlat=-10,maxlat=10,minlong=210,maxlong=250,minfrq=100,zl2=seq(40,800,by=20),ti="",fn="YFT1_model",rng=c(30,180),respvar="cls2",sz_name="YFT1_yq_comm_sz_plot",mapname="YFT1_map_comm_sz_plot",yq_mode=F,llmode=F,lenplot=T,newwindow=T, sp = "YFT")

bet1len <- sz[sz$species==4 & sz$resolution== 4 & sz$unit %in% c(6,7,8),] # resolution 4 is 1x1
yft1len <- sz[sz$species==5 & sz$resolution== 4 & sz$unit %in% c(6,7,8),] # resolution 4 is 1x1

plot_resp_image_resp(dat=bet1len,alat="lat1", alon="lon1", minlat=-41,maxlat=20,minlong=20,maxlong=130,
                     minfrq=100,zl2=seq(40,800,by=5),
                     ti="",fn="BET1len_model",rng=c(90,150),respvar="cls2",sz_name="BET1len_sz_plot",mapname="BET1len_map_plot",yq_mode=F,llmode=F,
                     lenplot=T,newwindow=T, sp = "BET")
plot_resp_image_resp(dat=yft1len,alat="lat1", alon="lon1", minlat=-41,maxlat=25,minlong=20,maxlong=130,
                     minfrq=100,zl2=seq(40,800,by=5),
                     ti="",fn="YFT1len_model",rng=c(70,150),respvar="cls2",sz_name="YFT1len_sz_plot",mapname="YFT1len_map_plot",yq_mode=F,llmode=F,
                     lenplot=T,newwindow=T, sp = "YFT")

bet1x <- sz[sz$species==4 & sz$resolution== 4 & sz$lat > -15 & sz$lat < 10 & sz$ year < 1990,] # resolution 4 is 1x1
yft1x <- sz[sz$species==5 & sz$resolution== 4 & sz$lat > -15 & sz$lat < 10 & sz$ year < 1990,] # resolution 4 is 1x1

plot_resp_image_resp(dat=bet1x,alat="lat1", alon="lon1", minlat=-41,maxlat=20,minlong=20,maxlong=130,
                     minfrq=100,zl2=seq(40,800,by=5),
                     ti="",fn="BET1x_model",rng=c(90,150),respvar="cls2",sz_name="BET1x_sz_plot",mapname="BET1x_map_plot",yq_mode=F,llmode=F,
                     lenplot=T,newwindow=T, sp = "BET")
plot_resp_image_resp(dat=yft1x,alat="lat1", alon="lon1", minlat=-41,maxlat=25,minlong=20,maxlong=130,
                     minfrq=100,zl2=seq(40,800,by=5),
                     ti="",fn="YFT1x_model",rng=c(70,150),respvar="cls2",sz_name="YFT1x_sz_plot",mapname="YFT1x_map_plot",yq_mode=F,llmode=F,
                     lenplot=T,newwindow=T, sp = "YFT")

sz2 <- sz[sz$resolution==2,]
tabfun(a=sz2,ppp="vessel_type") # only commercial
tabfun(a=sz2,ppp="unit") # weight (1956 - 1989),  1cm (1965-71), 2cm (1952 - 71)

bet2kg <- sz2[sz2$species==4 & sz2$unit == 3,] # resolution 2
yft2kg <- sz2[sz2$species==5 & sz2$unit == 3,] # resolution 2
tabfun(a=yft2kg,ppp="unit") # weight (1956 - 1989),  1cm (1965-71), 2cm (1952 - 71)
tapply(bet2kg$cls2,bet2kg$year, mean, na.rm=TRUE)

plot_resp_image_resp(dat=bet2kg,alat="lat510", alon="lon510", minlat=-41,maxlat=20,minlong=20,maxlong=130,
                     minfrq=80,zl2=seq(0,800,by=2),
                     ti="",fn="BET2kg_model",rng=c(10, 100),respvar="cls2",sz_name="BET2kg_sz_plot",mapname="BET2kg_map_plot",yq_mode=F,llmode=F,
                     lenplot=T,newwindow=T, sp = "BET", ylab = "Weight (kg)")
plot_resp_image_resp(dat=yft2kg,alat="lat510", alon="lon510", minlat=-41,maxlat=25,minlong=20,maxlong=130,
                     minfrq=80,zl2=seq(0,800,by=2),
                     ti="",fn="YFT2kg_model",rng=c(10,70),respvar="cls2",sz_name="YFT2kg_sz_plot",mapname="YFT2kg_map_plot",yq_mode=F,llmode=F,
                     lenplot=T,newwindow=T, sp = "YFT", ylab = "Weight (kg)")

bet2cm <- sz2[sz2$species==4 & sz2$unit %in% c(6,7),] # resolution 2
yft2cm <- sz2[sz2$species==5 & sz2$unit %in% c(6,7),] # resolution 2
tabfun(a=yft2cm,ppp="unit") # weight (1956 - 1989),  1cm (1965-71), 2cm (1952 - 71)
tapply(yft2cm$cls2,yft2cm$year, mean, na.rm=TRUE)

plot_resp_image_resp(dat=yft2cm,alat="lat510", alon="lon510", minlat=-41,maxlat=25,minlong=20,maxlong=130,
                     minfrq=80,zl2=seq(0,800,by=2),
                     ti="",fn="YFT2cm_model",rng=c(70,150),respvar="cls2",sz_name="YFT2cm_sz_plot",mapname="YFT2cm_map_plot",yq_mode=F,llmode=F,
                     lenplot=T,newwindow=T, sp = "YFT", ylab = "Length (cm)")


wb <- read.csv("~/../Google Drive/My papers/WCPFC_SC_papers/13th Regular Session/2017UpdatedIndices.csv")
wy <- read.csv("~/../Google Drive/My papers/WCPFC_SC_papers/13th Regular Session/YFT_DLN-ind_NOVESHES_LONG_MFCL-frmtd_2017-regions.csv")
wbo <- read.csv("~/../OneDrive/Consulting/IOTC/2017_CPUE/JP/lengths/bet.csv")
wyo <- read.csv("~/../OneDrive/Consulting/IOTC/2017_CPUE/JP/lengths/yft.csv")
a <- wyo$R5
wyo$R5 <- wyo$R6
wyo$R6 <- a

str(wb)
str(wy)
par()
windows(height = 14, width = 14); par(mfrow = c(3,3), mar = c(4,4,3,.5)+.1)
sp = "BET"
for (i in 1:9) {
  reg <- paste0("R",i)
  pts <- with(wb, get(reg))
  plot(wb$yrqtr, pts, main = paste(sp, reg), xlab = "Year-quarter", ylab = "Index", xlim = c(1962, 1995))
  abline(v=1976.875, col = 2, lty = 3)
  abline(v=1977.875, col = 2, lty = 3)
}
savePlot("WCPO bet", type = "png")
windows(height = 14, width = 14); par(mfrow = c(3,3), mar = c(4,4,3,.5)+.1)
sp = "YFT"
for (i in 1:9) {
  reg <- paste0("R",i)
  pts <- with(wy, get(reg))
  plot(wy$yrqtr, pts, main = paste(sp, reg), xlab = "Year-quarter", ylab = "Index", xlim = c(1962, 1995))
  abline(v=1976.875, col = 2, lty = 3)
  abline(v=1977.875, col = 2, lty = 3)
}
savePlot("WCPO yft", type = "png")

windows(height = 14, width = 14); par(mfrow = c(3,2), mar = c(4,4,3,.5)+.1)
sp = "BET"
for (i in 1:6) {
  reg <- paste0("R",i)
  pts <- with(wbo, get(reg))
  plot(wbo$Yrqtr, pts, main = paste(sp, reg), xlab = "Year-quarter", ylab = "Index", xlim = c(1952, 2011))
  abline(v=1976.875, col = 2, lty = 3)
  abline(v=1977.875, col = 2, lty = 3)
}
savePlot("WCPO 2011 bet", type = "png")
windows(height = 14, width = 14); par(mfrow = c(3,2), mar = c(4,4,3,.5)+.1)
sp = "YFT"
for (i in 1:6) {
  reg <- paste0("R",i)
  pts <- with(wyo, get(reg))
  plot(wyo$Yrqtr, pts, main = paste(sp, reg), xlab = "Year-quarter", ylab = "Index", xlim = c(1952, 2011))
  abline(v=1976.875, col = 2, lty = 3)
  abline(v=1977.875, col = 2, lty = 3)
}
savePlot("WCPO 2011 yft", type = "png")



iob1 <- read.csv("~/../OneDrive/Consulting_done/IOTC/CPUE_LL_2016/2016_secondmeet/joint/std_xTW/outputs/Joint_regB2_R1_dellog_boat_allyrs.csv")
iob2 <- read.csv("~/../OneDrive/Consulting_done/IOTC/CPUE_LL_2016/2016_secondmeet/joint/std_xTW/outputs/Joint_regB2_R2_dellog_boat_allyrs.csv")
iob3 <- read.csv("~/../OneDrive/Consulting_done/IOTC/CPUE_LL_2016/2016_secondmeet/joint/std_xTW/outputs/Joint_regB2_R3_dellog_boat_allyrs.csv")
iob4 <- read.csv("~/../OneDrive/Consulting_done/IOTC/CPUE_LL_2016/2016_secondmeet/joint/std_xTW/outputs/Joint_regB2_R4_dellog_boat_allyrs.csv")
iob1nc <- read.csv("~/../OneDrive/Consulting_done/IOTC/CPUE_LL_2016/2016_secondmeet/joint/std_nocl_xTW_nohbf/outputs/Joint_regB2_R1_dellog_boat_allyrs.csv")
iob2nc <- read.csv("~/../OneDrive/Consulting_done/IOTC/CPUE_LL_2016/2016_secondmeet/joint/std_nocl_xTW_nohbf/outputs/Joint_regB2_R2_dellog_boat_allyrs.csv")
iob3nc <- read.csv("~/../OneDrive/Consulting_done/IOTC/CPUE_LL_2016/2016_secondmeet/joint/std_nocl_xTW_nohbf/outputs/Joint_regB2_R3_dellog_boat_allyrs.csv")
iob4nc <- read.csv("~/../OneDrive/Consulting_done/IOTC/CPUE_LL_2016/2016_secondmeet/joint/std_nocl_xTW_nohbf/outputs/Joint_regB2_R4_dellog_boat_allyrs.csv")
ioy2 <- read.csv("~/../OneDrive/Consulting_done/IOTC/CPUE_LL_2016/2016_secondmeet/joint/std_xTW/outputs/Joint_regY_R2_dellog_boat_allyrs.csv")
ioy3 <- read.csv("~/../OneDrive/Consulting_done/IOTC/CPUE_LL_2016/2016_secondmeet/joint/std_xTW/outputs/Joint_regY_R3_dellog_boat_allyrs.csv")
ioy4 <- read.csv("~/../OneDrive/Consulting_done/IOTC/CPUE_LL_2016/2016_secondmeet/joint/std_xTW/outputs/Joint_regY_R4_dellog_boat_allyrs.csv")
ioy5 <- read.csv("~/../OneDrive/Consulting_done/IOTC/CPUE_LL_2016/2016_secondmeet/joint/std_xTW/outputs/Joint_regY_R5_dellog_boat_allyrs.csv")
ioy2nc <- read.csv("~/../OneDrive/Consulting_done/IOTC/CPUE_LL_2016/2016_secondmeet/joint/std_nocl_xTW_nohbf/outputs/Joint_regY_R2_dellog_boat_allyrs.csv")
ioy3nc <- read.csv("~/../OneDrive/Consulting_done/IOTC/CPUE_LL_2016/2016_secondmeet/joint/std_nocl_xTW_nohbf/outputs/Joint_regY_R3_dellog_boat_allyrs.csv")
ioy4nc <- read.csv("~/../OneDrive/Consulting_done/IOTC/CPUE_LL_2016/2016_secondmeet/joint/std_nocl_xTW_nohbf/outputs/Joint_regY_R4_dellog_boat_allyrs.csv")
ioy5nc <- read.csv("~/../OneDrive/Consulting_done/IOTC/CPUE_LL_2016/2016_secondmeet/joint/std_nocl_xTW_nohbf/outputs/Joint_regY_R5_dellog_boat_allyrs.csv")

windows(height = 14, width = 14); par(mfrow = c(2,2), mar = c(4,4,3,.5)+.1)
sp = "BET"
for (i in 1:4) {
  reg <- paste0("R",i)
  pts <- get(paste0("iob",i))
  pts <- pts[pts$yq > 1962 & pts$yq < 1995,]
  plot(pts$yq, pts$pr, main = paste(sp, reg, "clust"), xlab = "Year-quarter", ylab = "Index", xlim = c(1962, 1995))
  abline(v=1976.875, col = 2, lty = 3)
  abline(v=1977.875, col = 2, lty = 3)
}
savePlot("IO BET CPUE clust", type = "png")
windows(height = 14, width = 14); par(mfrow = c(2,2), mar = c(4,4,3,.5)+.1)
sp = "BET"
for (i in 1:4) {
  reg <- paste0("R",i)
  pts <- get(paste0("iob",i,"nc"))
  pts <- pts[pts$yq > 1962 & pts$yq < 1995,]
  plot(pts$yq, pts$pr, main = paste(sp, reg, "nocl"), xlab = "Year-quarter", ylab = "Index", xlim = c(1962, 1995))
  abline(v=1976.875, col = 2, lty = 3)
  abline(v=1977.875, col = 2, lty = 3)
}
savePlot("IO BET CPUE nocl", type = "png")
windows(height = 14, width = 14); par(mfrow = c(2,2), mar = c(4,4,3,.5)+.1)
sp = "YFT"
for (i in c(2,5,3,4)) {
  reg <- paste0("R",i)
  pts <- get(paste0("ioy",i))
  pts <- pts[pts$yq > 1962 & pts$yq < 1995,]
  plot(pts$yq, pts$pr, main = paste(sp, reg, "clust"), xlab = "Year-quarter", ylab = "Index", xlim = c(1962, 1995))
  abline(v=1976.875, col = 2, lty = 3)
  abline(v=1977.875, col = 2, lty = 3)
}
savePlot("IO YFT CPUE clust", type = "png")
windows(height = 14, width = 14); par(mfrow = c(2,2), mar = c(4,4,3,.5)+.1)
sp = "YFT"
for (i in c(2,5,3,4)) {
  reg <- paste0("R",i)
  pts <- get(paste0("ioy",i,"nc"))
  pts <- pts[pts$yq > 1962 & pts$yq < 1995,]
  plot(pts$yq, pts$pr, main = paste(sp, reg, "nocl"), xlab = "Year-quarter", ylab = "Index", xlim = c(1962, 1995))
  abline(v=1976.875, col = 2, lty = 3)
  abline(v=1977.875, col = 2, lty = 3)
}
savePlot("IO YFT CPUE nocl", type = "png")

windows(height = 14, width = 14); par(mfrow = c(2,2), mar = c(4,4,3,.5)+.1)
sp = "BET"
for (i in 1:4) {
  reg <- paste0("R",i)
  pts <- get(paste0("iob",i))
  pts <- pts[pts$yq > 1962 & pts$yq < 1995,]
  pts2 <- get(paste0("iob",i,"nc"))
  pts2 <- pts2[pts2$yq > 1962 & pts2$yq < 1995,]
  plot(pts$yq, pts$pr, main = paste(sp, reg), xlab = "Year-quarter", ylab = "Index",
       xlim = c(1962, 1995), ylim = c(0, max(c(pts$pr, pts2$pr), na.rm = TRUE)))
  points(pts2$yq, pts2$pr, col = 4, pch = 3, cex = .9)
  abline(v=1976.875, col = 2, lty = 3)
  abline(v=1977.875, col = 2, lty = 3)
}
legend("topright", legend = c("clusters","no clusters"), pch = c(1,3), col = c(1,4))
savePlot("IO BET CPUE both", type = "png")
windows(height = 14, width = 14); par(mfrow = c(2,2), mar = c(4,4,3,.5)+.1)
sp = "YFT"
for (i in c(2,5,3,4)) {
  reg <- paste0("R",i)
  pts <- get(paste0("ioy",i))
  pts <- pts[pts$yq > 1962 & pts$yq < 1995,]
  pts2 <- get(paste0("ioy",i,"nc"))
  pts2 <- pts2[pts2$yq > 1962 & pts2$yq < 1995,]
  plot(pts$yq, pts$pr, main = paste(sp, reg), xlab = "Year-quarter", ylab = "Index",
       xlim = c(1962, 1995), ylim = c(0, max(c(pts$pr, pts2$pr), na.rm = TRUE)))
  points(pts2$yq, pts2$pr, col = 4, pch = 3, cex = .9)
  abline(v=1976.875, col = 2, lty = 3)
  abline(v=1977.875, col = 2, lty = 3)
}
legend("topright", legend = c("clusters","no clusters"), pch = c(1,3), col = c(1,4))
savePlot("IO YFT CPUE both", type = "png")

