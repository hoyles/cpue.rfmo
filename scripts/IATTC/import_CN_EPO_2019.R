# CHINESE data IMPORT
# Minte-Vera IATTC cminte@iattc.org
# Version Fev.6.2019 1339

require(tidyverse)
require(stringi)
require(htmlwidgets)
require(lunar)

projdir <- "C:\\Users\\cminte\\Documents\\Carolina2019\\Meetings\\LONGLINE Workshop\\"
cndir <- paste0(projdir, "CN/")
datadir1 <- paste0(cndir, "data/")
cnalysis_dir <- paste0(cndir, "analyses/")
cnfigs <- paste0(cndir, "figures/")
Rdir <- paste0(projdir, "Rfiles/")
dir.create(cndir)
dir.create(datadir1)
dir.create(cnalysis_dir)
dir.create(cnfigs)

setwd(cnfigs)


################DATA IMPORT ####################################
# import data
# CPUE operational level data file sent by Jiangfeng Zhu <jfzhu@shou.edu.cn>
# on Sun 1/20/2019 12:36 AM
# 'Dear Carolina,
# We have finished the logbook data of CHN longline in the IATTC water. China implemented logbook program in 2009, so data started since 2010. It was actually raw data and was compiled by Zhe Geng.
# Attached are two files, one for the set-by-set catch and effort by species, the other is just the set site distribution.  Please explore the data and let me know if there is any problem. I am not sure the data is high quality because they were recorded by fishermen on-board.
# In the logbook data, we have no size data. The size data was only available in our observer data, which will be provided later. These data sets have much sparse spatial distribution as they were from around 5% effort coverage. So I think for abundance index, only logbook data was suitable.
# See you soon.
#
# Jiangfeng
# ________________________________________
# 朱江峰
# 上海海洋大学 科学技术处
# Jiangfeng Zhu, PhD
# Deputy Director
# Division of Science & Technology
# Shanghai Ocean University
# 999 Hucheng Huan Rd, Shanghai 201306
# P.R. CHINA
# Email: jfzhu@shou.edu.cn '

######READ DATA and do basic explorations
DataFile1<-"IATTC_logbook_2010-2018.csv"
#MyData1<-read.csv(paste0(datadir1,DataFile1)) # this funtion is from {utils}
MyData1<-read_csv(paste0(datadir1,DataFile1)) # this function is  from {readr}
#Warning: 1784 parsing failures.
spec(MyData1)
# cols(
#   Date = col_character(),
#   Vessel_ID = col_character(),
#   Active = col_integer(),
#   Lat = col_integer(),
#   LatH = col_character(),
#   Lon = col_integer(),
#   LonH = col_character(),
#   SetStart = col_time(format = ""),
#   Hooks = col_integer(),
#   HBF = col_integer(),
#   Alb_n = col_integer(),
#   Alb_kg = col_integer(),
#   Bet_n = col_integer(),
#   Bet_kg = col_integer(),
#   Skj_n = col_integer(),
#   Skj_kg = col_integer(),
#   Yft_n = col_integer(),
#   Yft_kg = col_integer(),
#   Blm_n = col_integer(),
#   Blm_kg = col_integer(),
#   Bum_n = col_integer(),
#   Bum_kg = col_integer(),
#   Mls_n = col_integer(),
#   Mls_kg = col_integer(),
#   Swo_n = col_integer(),
#   Swo_kg = col_integer(),
#   Bsh_n = col_integer(),
#   Bsh_kg = col_integer(),
#   Spn_n = col_integer(),
#   Spn_kg = col_integer(),
#   Mak_n = col_integer(),
#   Mak_kg = col_integer(),
#   Ocs_n = col_integer(),
#   Ocs_kg = col_integer(),
#   Por_n = col_integer(),
#   Por_kg = col_integer(),
#   Fal_n = col_integer(),
#   Fal_kg = col_integer(),
#   Thr_n = col_integer(),
#   Thr_kg = col_integer()
# )


table(is.na(MyData1$Lat))
#FALSE  TRUE
#74424   899
dim(MyData1)
#[1] 75323    40
#histograms of minutes
hist(as.numeric(stri_sub(MyData1$Lat,from=-2,to=-1)))
# ~uniform from 0 to 60 minutes
quantile(as.numeric(stri_sub(MyData1$Lat,from=-2,to=-1)),probs=seq(0,1,0.05),na.rm=T)
#0%   5%  10%  15%  20%  25%  30%  35%  40%  45%  50%  55%  60%  65%  70%  75%  80%  85%  90%  95% 100%
#0    1  5    8   10   13   17   20   23   26   29   32   35   38   41   45   48   50   53   56   95
# range of latitudes (1 degree precison)
hist(as.numeric(stri_sub(MyData1$Lon,from=-2,to=-1)))
# ~uniform from 0 to 60 minutes
quantile(as.numeric(stri_sub(MyData1$Lon,from=-2,to=-1)),probs=seq(0,1,0.05), na.rm=T)
#0%   5%  10%  15%  20%  25%  30%  35%  40%  45%  50%  55%  60%  65%  70%  75%  80%  85%  90%  95% 100%
#0    1    5    7   10   13   16   20   22   26   29   31   35   38   40   44   47   50   53   56   97
#Years
table(as.numeric(format(parse_date(MyData1$Date),'%Y')))
#2010 2011 2012 2013 2014 2015 2016 2017
#6  330  336 1311 2739 3719 3576 4664
summary(MyData1)
unique(MyData1$HBF)
barplot(table(MyData1$HBF))
names(MyData1)
# [1] "Date"      "Vessel_ID" "Active"    "Lat"       "LatH"      "Lon"       "LonH"      "SetStart"  "Hooks"
# [10] "HBF"       "Alb_n"     "Alb_kg"    "Bet_n"     "Bet_kg"    "Skj_n"     "Skj_kg"    "Yft_n"     "Yft_kg"
# [19] "Blm_n"     "Blm_kg"    "Bum_n"     "Bum_kg"    "Mls_n"     "Mls_kg"    "Swo_n"     "Swo_kg"    "Bsh_n"
# [28] "Bsh_kg"    "Spn_n"     "Spn_kg"    "Mak_n"     "Mak_kg"    "Ocs_n"     "Ocs_kg"    "Por_n"     "Por_kg"
# [37] "Fal_n"     "Fal_kg"    "Thr_n"     "Thr_kg"
unique(MyData1$Active)
#[1] 1
#names(OpData)
splist<-c("alb","bet","blm","bum","mls","osh","oth","sfa","skj","swo","yft")
splist2<-c("alb","bet", "skj","yft","blm","bum", "mls","swo","bsh","spn",
           "mak","ocs","por","fal","thr")
dim(MyData1)

OpData<-dataprep_CN(MyData1,splist = splist2)
dim(OpData)
#75323    61
###Save
save(OpData,"OpData",file=paste0(datadir1,"CNdat.RData"))

##### clean and load to check
rm(OpData)
load(file=paste0(datadir1,"CNdat.RData"))
#check
dim(OpData)


######### FUNCTION##################  <><  <><  <><   ###############

dataprep_CN<- function(dat, splist) {

  #modified from cpue.rfmo from Simon Hoyle
  # Original variable names
  # [1] "Date"      "Vessel_ID" "Active"    "Lat"       "LatH"      "Lon"       "LonH"      "SetStart"  "Hooks"
  # [10] "HBF"       "Alb_n"     "Alb_kg"    "Bet_n"     "Bet_kg"    "Skj_n"     "Skj_kg"    "Yft_n"     "Yft_kg"
  # [19] "Blm_n"     "Blm_kg"    "Bum_n"     "Bum_kg"    "Mls_n"     "Mls_kg"    "Swo_n"     "Swo_kg"    "Bsh_n"
  # [28] "Bsh_kg"    "Spn_n"     "Spn_kg"    "Mak_n"     "Mak_kg"    "Ocs_n"     "Ocs_kg"    "Por_n"     "Por_kg"
  # [37] "Fal_n"     "Fal_kg"    "Thr_n"     "Thr_kg"
  # Questions for China:
  #Vessel_ID - is the unique identifier for the vessel? How are those numbers assigned? What they mean?
  # Active?
  # What is the unit for "Hook"?
  # what does the species codes are?
  # how were the weight variables computed? Or are they raw data measure by fishers?
  # are some species reported in weight and others in numbers by the fishers?

  #
  dat<-type.convert(dat)

  nms <- c("DATE","VESSEL_CD","Active","Lat01","NS","Long01","EW","SetStart","hooks","hbf",
           "alb","alb_w","bet","bet_w", "skj","skj_w","yft","yft_w","blm","blm_w",
           "bum","bum_w", "mls","mls_w","swo","swo_w",
           #this names are different than for the rest of the fleets
           "bsh", "bsh_w","spn","spn_w","mak","mak_w","ocs","ocs_w","por","por_w","fal","fal_w","thr","thr_w")
  names(dat)<-nms
  dat<- mutate(dat, dmy=parse_date(DATE,'%Y/%m/%d')) %>%
    mutate(op_yr=as.numeric(format(dmy,'%Y'))) %>%
    mutate(op_mon=as.numeric(format(dmy,'%m'))) %>%
    mutate(floats=hooks*hbf)


  dat$moon <- lunar.illumination(dat$dmy)

  #quantile(MyData1$Lat01,na.rm=T)
  #0%  25%  50%  75% 100%
  #0  902 1400 1839 4737 the coordinates seem to exact to the nearest minute
  dat$lat <- dat$Lat01
  dat$lon <- dat$Long01
  dat$lonx <- dat$Long01
  #only EPO data, it seems (that is only W longitude)
  dat$lonx[dat$EW == "W" | dat$EW == "w"] <- (dat$lon[dat$EW == "W" | dat$EW == "w" ] ) * -1

  # only positive values
  dat$lat1 <-  floor(dat$lat/100) + 0.5
  dat$lon1x <-  floor(dat$lonx/100) + 0.5
  dat$lon1 <-  floor(dat$lon/100) + 0.5 # from 0 to 360

  dat$lat5 <- 5 * floor(dat$lat1/5) + 2.5
  dat$lon5x <- 5 * floor(dat$lon1x/5) + 2.5 # from -360 to 0
  dat$lon5 <- 5 * floor(dat$lon1/5) + 2.5 # from 0 to 360

  #only EPO data, it seems (that is only W longitude, set as negative iin lonx west, will go from  -360 to 0
  dat$lon1x[dat$EW == "W" | dat$EW == "w"] <- (dat$lon1x[dat$EW == "W" | dat$EW == "w" ] ) * -1
  dat$lon5x[dat$EW == "W" | dat$EW == "w"] <- (dat$lon5x[dat$EW == "W" | dat$EW == "w" ] ) * -1

  #negative if south
  #unique(MyData1$LatH)
  #[1] "S" "N" "s" "n"
  dat$lat[dat$NS == "S" | dat$NS == "s"] <- (dat$lat[dat$NS == "S" | dat$NS == "s" ] ) * -1
  dat$lat1[dat$NS == "S" | dat$NS == "s"] <- (dat$lat1[dat$NS == "S" | dat$NS == "s" ] ) * -1
  dat$lat5[dat$NS == "S" | dat$NS == "s"] <- (dat$lat5[dat$NS == "S" | dat$NS == "s" ] ) * -1


  dat$yrqtr <- dat$op_yr + floor((dat$op_mon - 1)/3)/4 + 0.125
  dat$latlong <- paste(dat$lat5, dat$lon5, sep = "_")
  dat$vessid <- as.factor(as.numeric(dat$VESSEL_CD))
  dat$tripidmon <- paste(dat$vessid, dat$op_yr, dat$op_mon)
  dat$Totalx <- apply(dat[,splist], 1, sum, na.rm = TRUE)
  dat$Total2 <- apply(dat[, c("bet", "yft", "alb")], 1, sum, na.rm = TRUE)
  dat$nsp<- apply(dat[,splist]>0, 1, sum, na.rm = TRUE)

  return(dat)
}

