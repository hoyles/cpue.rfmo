# C.M. Minte-Vera IATTC cminte@iattc.org
# Version Jan.20.2019 6:12 AM
#clean
rm(list=ls())
# CHANGE TO YOUR WORKING DIRECTORY HERE:
WorkDir<-"~/IATTC/2019_CPUE/TW/analyses/"

setwd(WorkDir)
#install.packages("tidyverse")
require(tidyverse)
require(stringi)
#install.packages("htmlwidgets")
require(htmlwidgets)
#require(cpue.rfmo)  #not needed yet
require(lunar)



######### FUNCTIONS
dataprep_TWN<- function(dat, splist) {
  #modified from cpue.rfmo from Simon Hoyle excluded lines: ##
  dat<-mutate(dat,op_yr=Year) %>% mutate(dmy=parse_date(str_c(parse_character(Year),"/",
    parse_character(op_mon),"/",parse_character(op_day)), "%Y/%m/%d")) %>%
    mutate(hbf=parse_integer(hbf)) %>% mutate(floats=hooks*hbf)

  #head(cbind(dat$op_yr,dat$op_mon,dat$op_day,dat$dmy))
  dat$moon <- lunar.illumination(dat$dmy)
  dat$lat <- dat$Lat01
  dat$lon <- dat$Long01
  dat$lat5 <- dat$Lat05
  dat$lon5 <- dat$Long05
  dat$yrqtr <- dat$op_yr + floor((dat$op_mon - 1)/3)/4 + 0.125
  dat$latlong <- paste(dat$lat5, dat$lon5, sep = "_")
  dat$vessid <- as.factor(as.numeric(dat$VESSEL_CD))
  dat$tripidmon <- paste(dat$vessid, dat$op_yr, dat$op_mon)
  dat$Totalx <- apply(dat[,splist], 1, sum, na.rm = TRUE)
  dat$Total2 <- apply(dat[, c("bet", "yft", "alb")], 1, sum, na.rm = TRUE)
  return(dat)
}




################DATA IMPORT ####################################
# import data
DataDir<- paste0(WorkDir, "../data/")
# CPUE operational level data
DataFile1<-"Logbook_PAC.csv"
MyData1<-read_csv(paste0(DataDir,DataFile1))
head(MyData1)
#A tibble: 585,632 x 34
head(MyData1[,15:(15+19)])
head(MyData1[,25:34])
#CTNO - is the unique identifier for the vessel? How are those numbers assigned? What they mean?
# What is the unit for "Hook"?
# what does "TUN" stands for?
# what species are in "OTH"?
# What does "SKX" stands for?
# how were the weight variables computed? Or are they raw data measure by fishers?
# are some species reported in weight and others in numbers?
summary(MyData1)
unique(MyData1$NHBF)
barplot(table(MyData1$NHBF))
type.convert(MyData1)
#clean characters of numeric data by specifying as integers
#A tibble: 585,632 x 34
#CTNO  Year Month   Day  Lon5  Lat5  Lon1  Lat1 Hooks  NHBF ALB_N BET_N YFT_N TUN_N SWO_N
#<int> <int> <int> <int> <dbl> <dbl> <dbl> <dbl> <int> <int> <int> <int> <int> <int> <int>
#MLS_N <int>, BUM_N <int>, BLM_N <int>,
#   BIL_N <int>, SKJ_N <int>, SKX_N <int>, OTH_N <int>, ALB_W <int>, BET_W <int>, YFT_W <int>,
#   TUN_W <int>, SWO_W <int>, MLS_W <int>, BUM_W <int>, BLM_W <int>, BIL_W <int>, SKJ_W <int>,
#   SKX_W <int>, OTH_W <int>
table(MyData1$Year)

OpData<-MyData1
table(OpData$Year)
#CallSign = VESSEL_CD
# Set up standard names, the same as for other fleets (based on cpue.rfmo but not the equal)
MyNames <- c("VESSEL_CD","Year","op_mon","op_day","Long05","Lat05","Long01", "Lat01","hooks",
         "hbf","alb","bet","yft", "tun", "swo","mls", "bum","blm","bil","skj","skx","oth",
         "ALB_W", "BET_W", "YFT_W","TUN_W", "SWO_W", "MLS_W", "BUM_W", "BLM_W",
         "BIL_W", "SKJ_W","SKX_W", "OTH_W")
names(OpData)<-MyNames
names(MyData1)
names(OpData)
splist<-c("alb","bet","yft","blm","bum","mls","skx","oth","skj","swo")
#dim(OpData)
barplot(table(OpData$hbf))
# first load functions
dim(OpData)
#[1] 585632     34

OpData<-dataprep_TWN(OpData,splist = splist)
dim(OpData)
#[1] 585632     47

head(OpData)
plot(table(OpData$op_yr))
table(is.na(OpData$op_yr))
#I will not exclude any data, will leave that to the analyst
#OpData<-OpData[OpData$op_yr>1974,]

##############Clean the data
# ensure that the number of fish is less then or equal the number of hooks
hist(OpData$hooks); summary(OpData$hooks)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##KOR:
##240    2350    2560    2620    2860    8814
#TWN:
#170    1540    2700    2509    3100    7568
hist(OpData$Totalx); summary(OpData$Totalx)
##KOR:
##Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
##0.00    17.00    29.00    38.21    48.00 2137.0
#TWN:
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.00   26.00   44.00   60.77   75.00 1211.00
#are there outliers?
table(OpData$Totalx>OpData$hooks)
#FALSE
#585632
table(is.na(OpData$Totalx))
#FALSE
#585632
which(OpData$Total>OpData$hooks)
#integer(0)
table(is.na(OpData$op_yr))
#FALSE
#585632
table(OpData$op_yr<1975)
#92815 cases before 1975
#FALSE   TRUE
#492817  92815
hist(OpData$hbf)
table(OpData$hbf) #nothing removed here yet - should I?
#3     4     5     6     7     8     9    10    11    12    13    14    15    16    17
#1   468   149   122   560    82  8266 34245 26957 16507  5409  7563 28957 57346 97649
#18    19    20    21    22    23    24    25    26    27    28    29    30    38   127
#18451  7704  2226   987   953   986   341  5237   583   868    83   413    28     5     1
#maybe is shallow water, they use less HBF, DO NOT REMOVE
OpData<-OpData[OpData$hbf>2,] #Nothing was removed here:
dim(OpData)
#[1] 585632     47
###Vessels:
unique(OpData$VESSEL_CD)
#[1]    NA 50650 50657 50728 50854 50874 60024 60187 60188 60253 60263 60451 60471 60735 60778
#[16] 60779 60936 60950 60975 60993 [...]
length(unique(OpData$VESSEL_CD))
#[1] 250
#249 Unique Vessel IDs + NA
# There are a lot of NAs in the number of hooks
table(is.na(OpData$hooks))
#FALSE   TRUE
#323147 262485
# another aspect to consider is exclude hooks(x) : x >= 1,000 and x < 5,500
table(OpData$hooks<=1000)
#FALSE   TRUE
#323131     16
table(OpData$hooks>5500)
#FALSE   TRUE
#323144      3

table(is.na(OpData$hooks),MyData1$Year)
table(is.na(OpData$hooks),OpData$op_yr)

# 1964  1965  1966  1967  1968  1969  1970  1971  1972  1973  1974  1975  1976  1977
# FALSE     0     0     0     0     0     0     0     0     0     0     0     0     0     0
# TRUE    592  1784  7192 11229 10298  7738 11001 10233 10022 11877 10849  5654  4551  6865
#
# 1978  1979  1980  1981  1982  1983  1984  1985  1986  1987  1988  1989  1990  1991
# FALSE     0     0     0     0     0     0     0     0     0     0     0     0     0     0
# TRUE   5617 15172 18717 18428 13847  7938  8060  5746  5125  6751  4993  2851  1617  2482
#
# 1992  1993  1994  1995  1996  1997  1998  1999  2000  2001  2002  2003  2004  2005
# FALSE     0     0     0  2657  4058  3216  2062  3506  2727 10629 17227 17370 31001 27722
# TRUE   3047  6285  5406  1473  2249  1837  1257  2933  3651  5240   683    76   830    64
#
# 2006  2007  2008  2009  2010  2011  2012  2013  2014  2015  2016  2017
# FALSE 24792 19241 15512 16245 18282 17132 17164 14553 12370 12175 16124 17382
# TRUE      0     0     0     0   225     0     0     0     0     0     0     0
#

table(is.na(MyData1$Hooks),MyData1$Year)
hist(MyData1$Hooks)

dim(OpData)
save(OpData,"OpData",file="TWNdat.RData")

##### clean and load
rm(list=ls())
load(file="TWNdat.RData")
#check
dim(OpData)
#[1] 585632     48
