# Data cleaning
dataclean_TW <- function(dat1,rmssp=F) {
 # hist(dat1$hbf,nclass=400)
  dat1 <- dat1[!is.na(dat1$hooks),] #
#  hist(dat1$hooks,nclass=250)
#  dat1 <- dat1[dat1$hooks<10000,] # clean up outliers
  dat1 <- dat1[dat1$hooks<4500,] # clean up outliers
#  dat1 <- dat1[dat1$hooks<5000,] # clean up outliers
  dat1 <- dat1[dat1$hooks>200,]
#  dat1 <- dat1[dat1$yft<750,]
#  dat1 <- dat1[dat1$bet<250,]
#  dat1 <- dat1[dat1$alb<250,]
#  dat1 <- dat1[is.na(dat1$hbf)==F,]
#  dat1 <- dat1[dat1$op_yr > 1976,]
#  dat1 <- dat1[dat1$yrqtr < 2017,]
#  dat1 <- dat1[dat1$EW==1,]
#  dat1 <- dat1[dat1$hooks >= 1000,]
  dat1[dat1$hbf %in% c(35,155,20000),"hbf"] <- 15
  dat1[dat1$hbf %in% c(26,30),"hbf"] <- 20
  dat1[dat1$hbf %in% c(25),"hbf"] <- 24
  lenzero <- function(x) sum(x > 0)
  if(rmssp) {
    ssp <- apply(dat1[,c("alb","bet","yft","ott","swo","mls","bum","blm","otb","skj","sha","oth","sbt")],1,lenzero)
    dat1 <- dat1[ssp > 1,]
  }
#  dat1$hbf <- as.factor(as.character(dat1$hbf))
  return(dat1)
  }

dataclean_TW_std <- function(dat1, doHBF=F) {
  dat1 <- dat1[!is.na(dat1$hooks),]
  dat1 <- dat1[dat1$hooks<10000,]
  dat1 <- dat1[dat1$hooks>1000,]
  dat1 <- dat1[dat1$yft + dat1$bet + dat1$alb > 0,]
  if(doHBF) dat1 <- dat1[dat1$hbf <= 25,]
  lenzero <- function(x) sum(x > 0)
  ssp <- apply(dat1[,c("alb","bet","yft","ott","swo","mls","bum","blm","otb","skj","sha","oth","sbt")],1,lenzero)
  dat1 <- dat1[ssp > 1,]
  remvec <- c("G")
  dat1 <- dat1[-grep(remvec,dat1$rem),]
  return(dat1)
  }

dataclean_KR <- function(dat, yearlim = 2016) {
 # hist(dat$hbf,nclass=400)
  if(sum(is.na(dat$alb))>0) dat[is.na(dat$alb),]$alb <- 0
  if(sum(is.na(dat$bet))>0) dat[is.na(dat$bet),]$bet <- 0
  if(sum(is.na(dat$blm))>0) dat[is.na(dat$blm),]$blm <- 0
  if(sum(is.na(dat$bum))>0) dat[is.na(dat$bum),]$bum <- 0
  if(sum(is.na(dat$mls))>0) dat[is.na(dat$mls),]$mls <- 0
  if(sum(is.na(dat$oth))>0) dat[is.na(dat$oth),]$oth <- 0
#  if(sum(is.na(dat$pbf))>0) dat[is.na(dat$pbf),]$pbf <- 0
  if(sum(is.na(dat$sbt))>0) dat[is.na(dat$sbt),]$sbt <- 0
  if(sum(is.na(dat$sfa))>0) dat[is.na(dat$sfa),]$sfa <- 0
  if(sum(is.na(dat$sha))>0) dat[is.na(dat$sha),]$sha <- 0
  if(sum(is.na(dat$skj))>0) dat[is.na(dat$skj),]$skj <- 0
  if(sum(is.na(dat$swo))>0) dat[is.na(dat$swo),]$swo <- 0
  if(sum(is.na(dat$yft))>0) dat[is.na(dat$yft),]$yft <- 0
  dat <- dat[!is.na(dat$hooks),] # Clean up 294 NAs
  dat <- dat[dat$hooks<5000,] # clean up outliers
 # dat <- dat[dat$hooks>200,]
  dat <- dat[dat$hooks >= 1000,]
  # dat <- dat[dat$yft<750,]
#  dat <- dat[dat$bet<250,]
#  dat <- dat[dat$alb<250,]
  dat <- dat[is.na(dat$hbf)==F,]
  dat <- dat[dat$op_yr > 1976,]
  dat <- dat[dat$yrqtr < yearlim,]
  dat <- dat[dat$EW==1,]
  dat <- dat[dat$hbf >= 5,]
  return(dat)
  }

dataclean_JPIO <- function(dat,checktg=F,allHBF=F) {
  dat$op_yr <- as.numeric(dat$op_yr)
  dat$op_mon <- as.numeric(dat$op_mon)
  dat$op_day <- as.numeric(dat$op_day)
  dat <- dat[dat$op_day < 32,]
  dat <- dat[!is.na(dat$op_day),]
  dmy <- as.Date(paste(dat$op_yr,dat$op_mon,dat$op_day,sep="-"))
  dat <- dat[!is.na(dmy),]
  dat$lat <- as.numeric(dat$lat)
  dat <- dat[!is.na(dat$lat),]
  dat <- dat[!is.na(dat$lon),]
  dat$latcode <- as.numeric(dat$latcode)
  dat$lon <- as.numeric(dat$lon)
  dat$loncode <- as.numeric(dat$loncode)
  dat <- dat[dat$loncode %in% c(1,2),]
  dat$hbf <- as.numeric(dat$hbf)
#  dat$tonnage <- as.numeric(dat$tonnage)
  dat$hooks <- as.numeric(dat$hooks)
  dat$alb <- as.numeric(dat$alb)
  dat$bet <- as.numeric(dat$bet)
  dat$yft <- as.numeric(dat$yft)
  dat$swo <- as.numeric(dat$swo)
  dat$sbt <- as.numeric(dat$sbt)
  dat$blm <- as.numeric(dat$blm)
  dat$bum <- as.numeric(dat$bum)
  dat$mls <- as.numeric(dat$mls)
  if(sum(is.na(dat$alb))>0) dat[is.na(dat$alb),]$alb <- 0
  if(sum(is.na(dat$bet))>0) dat[is.na(dat$bet),]$bet <- 0
  if(sum(is.na(dat$yft))>0) dat[is.na(dat$yft),]$yft <- 0
  if(sum(is.na(dat$swo))>0) dat[is.na(dat$swo),]$swo <- 0
  if(sum(is.na(dat$sbt))>0) dat[is.na(dat$sbt),]$sbt <- 0
  if(sum(is.na(dat$blm))>0) dat[is.na(dat$blm),]$blm <- 0
  if(sum(is.na(dat$bum))>0) dat[is.na(dat$bum),]$bum <- 0
  if(sum(is.na(dat$mls))>0) dat[is.na(dat$mls),]$mls <- 0
  dat <- dat[!is.na(dat$hooks),]
  dat <- dat[dat$hooks<5000,] # clean up outliers
  dat <- dat[dat$hooks>200,]
#  dat <- dat[dat$yft<250,]
#  dat <- dat[dat$bet<250,]
#  dat <- dat[dat$alb<250,]
#  dat <- dat[dat$tonnage<30000 | is.na(dat$tonnage),]
#  dat[dat$fishingcat =="0",]
#  dat <- dat[dat$fishingcat !=".",]
#  dat <- dat[dat$fishingcat !="0",]
#  dat <- dat[dat$hbf != "  .",]
#  dat <- dat[is.na(dat$hbf)==FALSE | dat$op_yr < 1976,]
#  dat <- dat[dat$hbf < 26 | is.na(dat$hbf)==TRUE,]
  dat <- dat[(!is.na(dat$hbf) & dat$hbf < 26) | (dat$op_yr < 1976 & is.na(dat$hbf)),]
#  if(allHBF==F) {
#    dat[dat$hbf>22,]$hbf <- 22     # pool hbf > 22 into 22
#    dat <- dat[dat$hbf > 4,]   # remove swordfish targeting in R1 and R2
#    }
#  dat$ncrew <- as.numeric(dat$ncrew)
  if(checktg) dat <- dat[dat$target == 3 | is.na(dat$target),] # tuna target  (remove to avoid a change in 1994 - but recent trend is more important)
  return(dat)
  }

dataclean <- function(dat,checktg=F,allHBF=F) {
  dat$op_yr <- as.numeric(dat$op_yr)
  dat$op_mon <- as.numeric(dat$op_mon)
  dat$op_day <- as.numeric(dat$op_day)
  dat <- dat[dat$op_day < 32,]
  dat <- dat[!is.na(dat$op_day),]
  dat$lat <- as.numeric(dat$lat)
  dat$latcode <- as.numeric(dat$latcode)
  dat$lon <- as.numeric(dat$lon)
  dat$loncode <- as.numeric(dat$loncode)
  dat <- dat[dat$loncode %in% c(1,2),]
  dat$hbf <- as.numeric(dat$hbf)
#  dat$tonnage <- as.numeric(dat$tonnage)
  dat$hooks <- as.numeric(dat$hooks)
  dat$alb <- as.numeric(dat$alb)
  dat$bet <- as.numeric(dat$bet)
  dat$yft <- as.numeric(dat$yft)
  dat$swo <- as.numeric(dat$swo)
  dat$sbt <- as.numeric(dat$sbt)
  dat$blm <- as.numeric(dat$blm)
  dat$bum <- as.numeric(dat$bum)
  dat$mls <- as.numeric(dat$mls)
  if(sum(is.na(dat$alb))>0) dat[is.na(dat$alb),]$alb <- 0
  if(sum(is.na(dat$bet))>0) dat[is.na(dat$bet),]$bet <- 0
  if(sum(is.na(dat$yft))>0) dat[is.na(dat$yft),]$yft <- 0
  if(sum(is.na(dat$swo))>0) dat[is.na(dat$swo),]$swo <- 0
  if(sum(is.na(dat$sbt))>0) dat[is.na(dat$sbt),]$sbt <- 0
  if(sum(is.na(dat$blm))>0) dat[is.na(dat$blm),]$blm <- 0
  if(sum(is.na(dat$bum))>0) dat[is.na(dat$bum),]$bum <- 0
  if(sum(is.na(dat$mls))>0) dat[is.na(dat$mls),]$mls <- 0
  dat <- dat[!is.na(dat$hooks),]
  dat <- dat[dat$hooks<10000,] # clean up outliers
  dat <- dat[dat$hooks>200,]
  dat <- dat[dat$yft<250,]
  dat <- dat[dat$bet<250,]
  dat <- dat[dat$alb<250,]
  dat <- dat[dat$tonnage<30000 | is.na(dat$tonnage),]
#  dat[dat$fishingcat =="0",]
#  dat <- dat[dat$fishingcat !=".",]
  dat <- dat[dat$fishingcat !="0",]
#  dat <- dat[dat$hbf != "  .",]
  dat <- dat[is.na(dat$hbf)==F | dat$op_yr < 1976,]
  dat <- dat[dat$hbf < 26 | is.na(dat$hbf)==T,]
#  if(allHBF==F) {
#    dat[dat$hbf>22,]$hbf <- 22     # pool hbf > 22 into 22
#    dat <- dat[dat$hbf > 4,]   # remove swordfish targeting in R1 and R2
#    }
#  dat$ncrew <- as.numeric(dat$ncrew)
  if(checktg) dat <- dat[dat$target == 3 | is.na(dat$target),] # tuna target  (remove to avoid a change in 1994 - but recent trend is more important)
  return(dat)
  }

# Data preparation
dataprep_TW <- function(dat,alldat=F) {
  dat$dmy <- ymd(paste(dat$op_yr,dat$op_mon,dat$op_day,sep="-"))
  makedmy <- function(yy,mm,dd) {
    a= paste(yy,mm,dd,sep="-")
    a1 <- gsub(" ","",a)
    a2 <- ymd(a1)
    return(a2)
  }
  dat$embark_dmy <- makedmy(dat$embark_yr,dat$embark_mn,dat$embark_dd)
  dat$debark_dmy <- makedmy(dat$debark_yr,dat$debark_mn,dat$debark_dd)
  dat$op_start_dmy <- makedmy(dat$op_start_yr,dat$op_start_mn,dat$op_start_dd)
  dat$op_end_dmy <- makedmy(dat$op_end_yr,dat$op_end_mn,dat$op_end_dd)

  #  hist(dat$op_start_dmy,breaks="months")
  dat$lat <- dat$op_lat
  dat$lon <- dat$op_lon

  dat$tonnage <- as.factor(substring(dat$callsign,1,1))
  a <- levels(dat$tonnage)
  levs <- cbind(c("0","1","2","3","4","5","6","7","8"),c("< 5","5-10","10-20","20-50","50-100","100-200","200-500","500-1000",">= 1000"))
  levels(dat$tonnage) <- levs[match(a,levs[,1]),2]
  #  table(dat$tonnage,substring(dat$callsign,1,1))
  #  table(dat$NS,useNA="always")
  #  table(dat$EW,useNA="always")

  dat$lat[is.na(dat$NS)==F & dat$NS%in% c(3,7)] <- (dat$lat[is.na(dat$NS)==F & dat$NS%in% c(3,7)]+1) * -1
  dat$lon[is.na(dat$EW)==F & dat$EW==2] <- 360 - (dat$lon[is.na(dat$EW)==F & dat$EW==2] + 1)
  dat <- dat[is.na(dat$lon) | dat$lon >= 10,]
  dat <- dat[is.na(dat$lon) | dat$lon < 130,]
  dat <- dat[is.na(dat$lat) | dat$lat < 29,]

  la <- as.integer(substring(dat$op_area,1,2))
  lo <- as.integer(substring(dat$op_area,3,4))
  dat$lat5 <- 5*(la - 73)/2+2.5
  dat$lat5[la%%2 == 0] <- -(5 * (la[la%%2 == 0] - 74))/2-2.5   #

  dat$lon5 <- -(5*(lo - 1)/2+2.5)
  dat$lon5[lo%%2 == 0] <- 5*(lo[lo%%2 == 0] - 2)/2 + 2.5
  # Indian ocean regions for YFT and BET?

  dat$bt1 <- !(is.na(dat$bait1) | dat$bait1==0)
  dat$bt2 <- !(is.na(dat$bait2) | dat$bait2==0)
  dat$bt3 <- !(is.na(dat$bait3) | dat$bait3==0)
  dat$bt4 <- !(is.na(dat$bait4) | dat$bait4==0)
  dat$bt5 <- !(is.na(dat$bait5) | dat$bait5==0)

  dat$vessid <- as.factor(as.numeric(as.factor(dat$callsign)))
  dat$tripid <- as.factor(paste(dat$vessid,dat$op_start_dmy))
  dat$tripidmon <- as.factor(paste(dat$vessid,dat$op_yr,dat$op_mon))
  dat$moon <- lunar.illumination(dat$dmy)

  dat$yrqtr <- dat$op_yr + floor((dat$op_mon-1)/3)/4 + 0.125
  dat$latlong <- paste(dat$lat5,dat$lon5,sep="_")
  dat$sbt <- dat$sbf + dat$pbf
  dat$sbt_w <- dat$sbf_w + dat$pbf_w
  dat$Total <- with(dat,alb + bet + yft + pbf + sbf + ott + swo + mls + bum + blm + otb + skj + sha + oth)
  dat$Total2 <- apply(dat[,c("bet","yft","alb")],1,sum)
  noms <- c("vessid","callsign","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tonnage","tripid","tripidmon","moon",
            "alb","bet","yft","ott","swo","mls","bum","blm","otb","skj","sha","oth","sbt","Total","Total2",
            "alb_w","bet_w","yft_w","ott_w","swo_w","mls_w","bum_w","blm_w","otb_w","skj_w","sha_w","oth_w","sbt_w",
            "sst","bt1","bt2","bt3","bt4","bt5","hookdp","target","rem","dmy","embark_dmy","debark_dmy","op_start_dmy","op_end_dmy",
            "lat","lon","lat5","lon5","oil","foc")
  dat <- dat[,noms]
  return(dat)
}

# dataprep_TW <- function(dat,alldat=F) {
#   dat$dmy <- ymd(paste(dat$op_yr,dat$op_mon,dat$op_day,sep="-"))
#   makedmy <- function(yy,mm,dd) {
#     a= paste(yy,mm,dd,sep="-")
#     a1 <- gsub(" ","",a)
#     a2 <- ymd(a1)
#     return(a2)
#   }
#   dat$embark_dmy <- makedmy(dat$embark_yr,dat$embark_mn,dat$embark_dd)
#   dat$debark_dmy <- makedmy(dat$debark_yr,dat$debark_mn,dat$debark_dd)
#   dat$op_start_dmy <- makedmy(dat$op_start_yr,dat$op_start_mn,dat$op_start_dd)
#   dat$op_end_dmy <- makedmy(dat$op_end_yr,dat$op_end_mn,dat$op_end_dd)
#
#   #  hist(dat$op_start_dmy,breaks="months")
#   dat$lat <- dat$op_lat
#   dat$lon <- dat$op_lon
#
#   dat$tonnage <- as.factor(substring(dat$callsign,1,1))
#   a <- levels(dat$tonnage)
#   levs <- cbind(c("0","1","2","3","4","5","6","7","8"),c("< 5","5-10","10-20","20-50","50-100","100-200","200-500","500-1000",">= 1000"))
#   levels(dat$tonnage) <- levs[match(a,levs[,1]),2]
#   #  table(dat$tonnage,substring(dat$callsign,1,1))
#   #  table(dat$NS,useNA="always")
#   #  table(dat$EW,useNA="always")
#
#   dat$lat[is.na(dat$NS)==F & dat$NS%in% c(3,7)] <- (dat$lat[is.na(dat$NS)==F & dat$NS%in% c(3,7)]+1) * -1
#   dat$lon[is.na(dat$EW)==F & dat$EW==2] <- 360 - (dat$lon[is.na(dat$EW)==F & dat$EW==2] + 1)
#   dat <- dat[is.na(dat$lon) | dat$lon >= 10,]
#   dat <- dat[is.na(dat$lon) | dat$lon < 130,]
#   dat <- dat[is.na(dat$lat) | dat$lat < 29,]
#
#   la <- as.integer(substring(dat$op_area,1,2))
#   lo <- as.integer(substring(dat$op_area,3,4))
#   dat$lat5 <- 5*(la - 73)/2+2.5
#   dat$lat5[la%%2 == 0] <- -(5 * (la[la%%2 == 0] - 74))/2-2.5   #
#
#   dat$lon5 <- -(5*(lo - 1)/2+2.5)
#   dat$lon5[lo%%2 == 0] <- 5*(lo[lo%%2 == 0] - 2)/2 + 2.5
#   # Indian ocean regions for YFT and BET?
#   #regY   1 is N of 10N and W of 80,
#   dat$regY <- 0
#   dat <- mutate(dat,regY = replace(regY,which(lat5 >=  10 & lon5 < 80 & !is.na(lat5)),1)) %>%
#     mutate(regY = replace(regY,which(lat5 <  10 & lat5 >=  -10 & lon5 >= 40 & lon5 < 75 & !is.na(lat5)),2))  %>%
#     mutate(regY = replace(regY,which(lat5 <  -10 & lat5 >=  -15 & lon5 >= 60 & lon5 < 75 & !is.na(lat5)),2)) %>%
#     mutate(regY = replace(regY,which(lat5 <  -10 & lat5 >= -30 & lon5 >= 20 & lon5 < 60 & !is.na(lat5)),3))  %>%
#     mutate(regY = replace(regY,which(lat5 <  -30 & lat5 >= -40 & lon5 >= 20 & lon5 < 40 & !is.na(lat5)),3))  %>%
#     mutate(regY = replace(regY,which(lat5 <  -15 & lat5 >= -40 & lon5 >= 60 & lon5 < 120 & !is.na(lat5)),4)) %>%
#     mutate(regY = replace(regY,which(lat5 <  -30 & lat5 >= -40 & lon5 >= 40 & lon5 < 60 & !is.na(lat5)),4))  %>%
#     mutate(regY = replace(regY,which(lat5 < 10 & lat5 >= -15 & lon5 >= 75 & lon5 < 100 & !is.na(lat5)),5))   %>%
#     mutate(regY = replace(regY,which(lat5 < -5 & lat5 >= -15 & lon5 >= 100 & lon5 < 110 & !is.na(lat5)),5))  %>%
#     mutate(regY = replace(regY,which(lat5 < -10 & lat5 >= -15 & lon5 >= 110 & lon5 < 130 & !is.na(lat5)),5)) %>%
#     mutate(regY = replace(regY,which(lat5 < 30 & lat5 >= 10 & lon5 >= 80 & lon5 < 100 & !is.na(lat5)),6))
#
#   dat$regY1 <- 0
#   dat <- mutate(dat,regY1 = replace(regY1,which(regY %in% c(1,2)),1)) %>%
#     mutate(regY1 = replace(regY1,which(regY %in% c(3)),2)) %>%
#     mutate(regY1 = replace(regY1,which(regY %in% c(4)),3)) %>%
#     mutate(regY1 = replace(regY1,which(regY %in% c(5,6)),4))
#
#   #regB   North of 15S and west of 80 is R1, or north of  20 and west of 45; north of 15S and east of 80 is R2; north of 35S is R3
#   dat$regB <- 0
#   dat <- mutate(dat,regB = replace(regB,which(lat5 <  10 & lat5 >=  -15 & lon5 >= 20 & lon5 < 80 & !is.na(lat5)),1)) %>%
#     mutate(regB = replace(regB,which(lat5 <  10 & lat5 >=  -20 & lon5 >= 20 & lon5 < 46 & !is.na(lat5)),1)) %>%
#     mutate(regB = replace(regB,which(lat5 < 10 & lat5 >= -15 & lon5 >= 80 & lon5 < 100 & !is.na(lat5)),2)) %>%
#     mutate(regB = replace(regB,which(lat5 < -3 & lat5 >= -15 & lon5 >= 100 & lon5 < 110 & !is.na(lat5)),2)) %>%
#     mutate(regB = replace(regB,which(lat5 < -7 & lat5 >= -15 & lon5 >= 110 & lon5 < 130 & !is.na(lat5)),2)) %>%
#     mutate(regB = replace(regB,which(lat5 <  -20 & lat5 >= -35 & lon5 >= 20 & lon5 < 120 & !is.na(lat5)),3)) %>%
#     mutate(regB = replace(regB,which(lat5 <  -15 & lat5 >= -35 & lon5 >= 46 & lon5 < 120 & !is.na(lat5)),3))
#
#   dat$regB1 <- 0
#   dat <- mutate(dat,regB1 = replace(regB,which(regB %in% 1:3),1))
#
#   dat$regB2 <- 0
#   dat <- mutate(dat,regB2 = replace(regB2,which(lat5 <  10 & lat5 >=  -15 & lon5 >= 20 & lon5 < 80 & !is.na(lat5)),1)) %>%
#     mutate(regB2 = replace(regB2,which(lat5 <  10 & lat5 >=  -20 & lon5 >= 20 & lon5 < 46 & !is.na(lat5)),1)) %>%
#     mutate(regB2 = replace(regB2,which(lat5 < 10 & lat5 >= -15 & lon5 >= 80 & lon5 < 100 & !is.na(lat5)),2)) %>%
#     mutate(regB2 = replace(regB2,which(lat5 < -3 & lat5 >= -15 & lon5 >= 100 & lon5 < 110 & !is.na(lat5)),2)) %>%
#     mutate(regB2 = replace(regB2,which(lat5 < -7 & lat5 >= -15 & lon5 >= 110 & lon5 < 130 & !is.na(lat5)),2)) %>%
#     mutate(regB2 = replace(regB2,which(lat5 <  -20 & lat5 >= -35 & lon5 >= 20 & lon5 < 75 & !is.na(lat5)),3)) %>%
#     mutate(regB2 = replace(regB2,which(lat5 <  -15 & lat5 >= -35 & lon5 >= 46 & lon5 < 75 & !is.na(lat5)),3)) %>%
#     mutate(regB2 = replace(regB2,which(lat5 <  -15 & lat5 >= -35 & lon5 >= 75 & lon5 < 120 & !is.na(lat5)),4))
#
#   #regA
#   dat$regA <- 0
#   dat <- mutate(dat,regA = replace(regA,which(dat$lat5 < -10 & dat$lat5 >=  -20 & !is.na(dat$lat5)),1)) %>%
#     mutate(regA = replace(regA,which(dat$lat5 < -20 & dat$lat5 > -40 & !is.na(dat$lat5)),2))
#
#   dat$regA1 <- 0
#   dat <- mutate(dat,regA1 = replace(regA1,which(dat$lat5 < -10 & dat$lat5 >=  -25 & !is.na(dat$lat5)),1)) %>%
#     mutate(regA1 = replace(regA1,which(dat$lat5 < -25 & dat$lat5 > -40 & !is.na(dat$lat5)),2))
#
#   dat$regA2 <- 0
#   dat <- mutate(dat,regA2 = replace(regA2,which(dat$lat5 < -10 & dat$lat5 >=  -20 & dat$lon5 < 75 & !is.na(dat$lat5)),1)) %>%
#     mutate(regA2 = replace(regA2,which(dat$lat5 < -10 & dat$lat5 >=  -20 & dat$lon5 >= 75 & !is.na(dat$lat5)),2)) %>%
#     mutate(regA2 = replace(regA2,which(dat$lat5 < -20 & dat$lat5 > -40 & dat$lon5 < 75 & !is.na(dat$lat5)),3)) %>%
#     mutate(regA2 = replace(regA2,which(dat$lat5 < -20 & dat$lat5 > -40 & dat$lon5 >= 75 & !is.na(dat$lat5)),4))
#
#   dat$regA3 <- 0
#   dat <- mutate(dat,regA3 = replace(regA3,which(dat$lat5 <  -10 & dat$lat5 >=  -25 & dat$lon5 < 75 & !is.na(dat$lat5)),1)) %>%
#     mutate(regA3 = replace(regA3,which(dat$lat5 <  -10 & dat$lat5 >=  -25 & dat$lon5 >= 75 & !is.na(dat$lat5)),2)) %>%
#     mutate(regA3 = replace(regA3,which(dat$lat5 < -25 & dat$lon5 < 75 & !is.na(dat$lat5)),3)) %>%
#     mutate(regA3 = replace(regA3,which(dat$lat5 < -25 & dat$lon5 >= 75 & !is.na(dat$lat5)),4))
#
#   dat$regA4 <- 0
#   dat <- mutate(dat,regA4 = replace(regA4,which(dat$lat5 <  -10 & dat$lat5 >=  -25 & dat$lon5 < 75 & !is.na(dat$lat5)),1)) %>%
#     mutate(regA4 = replace(regA4,which(dat$lat5 <  -10 & dat$lat5 >=  -25 & dat$lon5 >= 75 & !is.na(dat$lat5)),2)) %>%
#     mutate(regA4 = replace(regA4,which(dat$lat5 < -25 & dat$lat5 > -40 & dat$lon5 < 75 & !is.na(dat$lat5)),3)) %>%
#     mutate(regA4 = replace(regA4,which(dat$lat5 < -25 & dat$lat5 > -40 & dat$lon5 >= 75 & !is.na(dat$lat5)),4))
#
#   dat$regA5 <- 0
#   dat <- mutate(dat,regA5 = replace(regA5,which(dat$lat5 <  -15 & dat$lat5 >=  -45 & dat$lon5 > 55 & dat$lon5 < 100 & !is.na(dat$lat5)),1))
#
#   dat$bt1 <- !(is.na(dat$bait1) | dat$bait1==0)
#   dat$bt2 <- !(is.na(dat$bait2) | dat$bait2==0)
#   dat$bt3 <- !(is.na(dat$bait3) | dat$bait3==0)
#   dat$bt4 <- !(is.na(dat$bait4) | dat$bait4==0)
#   dat$bt5 <- !(is.na(dat$bait5) | dat$bait5==0)
#
#   dat$vessid <- as.factor(as.numeric(as.factor(dat$callsign)))
#   dat$tripid <- as.factor(paste(dat$vessid,dat$op_start_dmy))
#   dat$tripidmon <- as.factor(paste(dat$vessid,dat$op_yr,dat$op_mon))
#   dat$moon <- lunar.illumination(dat$dmy)
#
#   dat$yrqtr <- dat$op_yr + floor((dat$op_mon-1)/3)/4 + 0.125
#   dat$latlong <- paste(dat$lat5,dat$lon5,sep="_")
#   dat$sbt <- dat$sbf + dat$pbf
#   dat$sbt_w <- dat$sbf_w + dat$pbf_w
#   dat$Total <- with(dat,alb + bet + yft + pbf + sbf + ott + swo + mls + bum + blm + otb + skj + sha + oth)
#   dat$Total2 <- apply(dat[,c("bet","yft","alb")],1,sum)
#   noms <- c("vessid","callsign","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tonnage","tripid","tripidmon","moon",
#             "alb","bet","yft","ott","swo","mls","bum","blm","otb","skj","sha","oth","sbt","Total","Total2",
#             "alb_w","bet_w","yft_w","ott_w","swo_w","mls_w","bum_w","blm_w","otb_w","skj_w","sha_w","oth_w","sbt_w",
#             "sst","bt1","bt2","bt3","bt4","bt5","hookdp","target","rem","dmy","embark_dmy","debark_dmy","op_start_dmy","op_end_dmy",
#             "lat","lon","lat5","lon5","regY","regB","regY1","regB1","regB2","regA","regA1","regA2","regA3","regA4","regA5","oil","foc")
#   dat <- dat[,noms]
#   return(dat)
# }

dataprep_KR <- function(dat,alldat=F) {
  dat$dmy <- as.Date(dat$DATE)
  dat$hbf <- round(dat$hooks / dat$floats,0)
  dat$moon <- lunar.illumination(dat$dmy)

  dat$lat <- dat$Lat01
  dat$lon <- dat$Long01
  dat$lat[dat$NS==2] <- (dat$Lat01[dat$NS==2]+1) * -1
  dat$lon[dat$EW==2] <- 360 - (dat$Long01[dat$EW==2] + 1)
  dat <- dat[dat$lon >= 0,]
  dat <- dat[dat$lat < 29,]

  dat$lat5 <- 5 * floor(dat$lat/5) + 2.5
  dat$lon5 <- 5 * floor(dat$lon/5) + 2.5

  # Indian ocean regions for YFT and BET?
  #regY   1 is N of 10N and W of 80,

  dat$yrqtr <- dat$op_yr + floor((dat$op_mon-1)/3)/4 + 0.125
  dat$latlong <- paste(dat$lat5,dat$lon5,sep="_")
  dat$vessid <- as.factor(as.numeric(dat$VESSEL_NAME))
  dat$vessid <- as.factor(as.numeric(as.factor(dat$VESSEL_NAME)))
  #dat$vessid <- as.factor(as.numeric(dat$VESSEL_CD))
  dat$tripidmon <- paste(dat$vessid,dat$op_yr,dat$op_mon)
  dat$Totalx <- with(dat,alb + bet + yft + sbt + sfa + mls + bum + blm + swo + sha + skj + oth)
  dat$Total2 <- apply(dat[,c("bet","yft","alb")],1,sum)
  return(dat)
  }

setup_IO_regions <- function(dat, regY=F, regY1=F, regY2=F, regB=F, regB1=F, regB2=F, regB3 = F, regA=F, regA1=F, regA2=F, regA3=F, regA4=F, regA5=F) {
  if(regY) {
    dat$regY <- 0
    dat <- mutate(dat,regY = replace(regY,which(lat5 >=  10 & lon5 < 80 & !is.na(lat5)),1)) %>%
    mutate(regY = replace(regY,which(lat5 <  10 & lat5 >=  -10 & lon5 >= 40 & lon5 < 75 & !is.na(lat5)),2))  %>%
    mutate(regY = replace(regY,which(lat5 <  -10 & lat5 >=  -15 & lon5 >= 60 & lon5 < 75 & !is.na(lat5)),2)) %>%
    mutate(regY = replace(regY,which(lat5 <  -10 & lat5 >= -30 & lon5 >= 20 & lon5 < 60 & !is.na(lat5)),3))  %>%
    mutate(regY = replace(regY,which(lat5 <  -30 & lat5 >= -40 & lon5 >= 20 & lon5 < 40 & !is.na(lat5)),3))  %>%
    mutate(regY = replace(regY,which(lat5 <  -15 & lat5 >= -40 & lon5 >= 60 & lon5 < 120 & !is.na(lat5)),4)) %>%
    mutate(regY = replace(regY,which(lat5 <  -30 & lat5 >= -40 & lon5 >= 40 & lon5 < 60 & !is.na(lat5)),4))  %>%
    mutate(regY = replace(regY,which(lat5 < 10 & lat5 >= -15 & lon5 >= 75 & lon5 < 100 & !is.na(lat5)),5))   %>%
    mutate(regY = replace(regY,which(lat5 < -5 & lat5 >= -15 & lon5 >= 100 & lon5 < 110 & !is.na(lat5)),5))  %>%
    mutate(regY = replace(regY,which(lat5 < -10 & lat5 >= -15 & lon5 >= 110 & lon5 < 130 & !is.na(lat5)),5)) %>%
    mutate(regY = replace(regY,which(lat5 < 30 & lat5 >= 10 & lon5 >= 80 & lon5 < 100 & !is.na(lat5)),6))
  }

  if(regY1) {
    dat$regY1 <- 0
    dat <- mutate(dat,regY1 = replace(regY1,which(regY %in% c(1,2)),1)) %>%
    mutate(regY1 = replace(regY1,which(regY %in% c(3)),2)) %>%
    mutate(regY1 = replace(regY1,which(regY %in% c(4)),3)) %>%
    mutate(regY1 = replace(regY1,which(regY %in% c(5,6)),4))
  }

  if(regY2) {
    dat$regY2 <- 0
    dat <- mutate(dat,regY2 = replace(regY2,which(lat5 >=  10 & lon5 < 80 & !is.na(lat5)),1)) %>%
      mutate(regY2 = replace(regY2,which(lat5 <  10 & lat5 >=    0 & lon5 >= 40 & lon5 < 75 & !is.na(lat5)),7))  %>%
      mutate(regY2 = replace(regY2,which(lat5 <   0 & lat5 >=  -10 & lon5 >= 40 & lon5 < 75 & !is.na(lat5)),2))  %>%
      mutate(regY2 = replace(regY2,which(lat5 <  -10 & lat5 >=  -15 & lon5 >= 60 & lon5 < 75 & !is.na(lat5)),2)) %>%
      mutate(regY2 = replace(regY2,which(lat5 <  -10 & lat5 >= -30 & lon5 >= 20 & lon5 < 60 & !is.na(lat5)),3))  %>%
      mutate(regY2 = replace(regY2,which(lat5 <  -30 & lat5 >= -40 & lon5 >= 20 & lon5 < 40 & !is.na(lat5)),3))  %>%
      mutate(regY2 = replace(regY2,which(lat5 <  -15 & lat5 >= -40 & lon5 >= 60 & lon5 < 120 & !is.na(lat5)),4)) %>%
      mutate(regY2 = replace(regY2,which(lat5 <  -30 & lat5 >= -40 & lon5 >= 40 & lon5 < 60 & !is.na(lat5)),4))  %>%
      mutate(regY2 = replace(regY2,which(lat5 < 10 & lat5 >= -15 & lon5 >= 75 & lon5 < 100 & !is.na(lat5)),5))   %>%
      mutate(regY2 = replace(regY2,which(lat5 < -5 & lat5 >= -15 & lon5 >= 100 & lon5 < 110 & !is.na(lat5)),5))  %>%
      mutate(regY2 = replace(regY2,which(lat5 < -10 & lat5 >= -15 & lon5 >= 110 & lon5 < 130 & !is.na(lat5)),5)) %>%
      mutate(regY2 = replace(regY2,which(lat5 < 30 & lat5 >= 10 & lon5 >= 80 & lon5 < 100 & !is.na(lat5)),6))
  }

  #regB   North of 15S and west of 80 is R1, or north of  20 and west of 45; north of 15S and east of 80 is R2; north of 35S is R3
  if(regB) {
    dat$regB <- 0
    dat <- mutate(dat,regB = replace(regB,which(lat5 <  10 & lat5 >=  -15 & lon5 >= 20 & lon5 < 80 & !is.na(lat5)),1)) %>%
    mutate(regB = replace(regB,which(lat5 <  10 & lat5 >=  -20 & lon5 >= 20 & lon5 < 46 & !is.na(lat5)),1)) %>%
    mutate(regB = replace(regB,which(lat5 < 10 & lat5 >= -15 & lon5 >= 80 & lon5 < 100 & !is.na(lat5)),2)) %>%
    mutate(regB = replace(regB,which(lat5 < -3 & lat5 >= -15 & lon5 >= 100 & lon5 < 110 & !is.na(lat5)),2)) %>%
    mutate(regB = replace(regB,which(lat5 < -7 & lat5 >= -15 & lon5 >= 110 & lon5 < 130 & !is.na(lat5)),2)) %>%
    mutate(regB = replace(regB,which(lat5 <  -20 & lat5 >= -35 & lon5 >= 20 & lon5 < 120 & !is.na(lat5)),3)) %>%
    mutate(regB = replace(regB,which(lat5 <  -15 & lat5 >= -35 & lon5 >= 46 & lon5 < 120 & !is.na(lat5)),3))
  }

  if(regB1) {
    dat$regB1 <- 0
    dat <- mutate(dat,regB1 = replace(regB1,which(regB1 %in% 1:3),1))  # Doesn't look right
  }

  if(regB2) {
    dat$regB2 <- 0
    dat <- mutate(dat,regB2 = replace(regB2,which(lat5 <  10 & lat5 >=  -15 & lon5 >= 20 & lon5 < 80 & !is.na(lat5)),1)) %>%
      mutate(regB2 = replace(regB2,which(lat5 <  10 & lat5 >=  -20 & lon5 >= 20 & lon5 < 46 & !is.na(lat5)),1)) %>%
      mutate(regB2 = replace(regB2,which(lat5 < 10 & lat5 >= -15 & lon5 >= 80 & lon5 < 100 & !is.na(lat5)),2)) %>%
      mutate(regB2 = replace(regB2,which(lat5 < -3 & lat5 >= -15 & lon5 >= 100 & lon5 < 110 & !is.na(lat5)),2)) %>%
      mutate(regB2 = replace(regB2,which(lat5 < -7 & lat5 >= -15 & lon5 >= 110 & lon5 < 130 & !is.na(lat5)),2)) %>%
      mutate(regB2 = replace(regB2,which(lat5 <  -20 & lat5 >= -35 & lon5 >= 20 & lon5 < 75 & !is.na(lat5)),3)) %>%
      mutate(regB2 = replace(regB2,which(lat5 <  -15 & lat5 >= -35 & lon5 >= 46 & lon5 < 75 & !is.na(lat5)),3)) %>%
      mutate(regB2 = replace(regB2,which(lat5 <  -15 & lat5 >= -35 & lon5 >= 75 & lon5 < 120 & !is.na(lat5)),4))
  }

  if(regB3) {
    dat$regB3 <- 0
    dat <- mutate(dat,regB3 = replace(regB3,which(lat5 <  10 & lat5 >=  0 & lon5 >= 20 & lon5 < 80 & !is.na(lat5)),5)) %>%
      mutate(regB3 = replace(regB3,which(lat5 <  0  & lat5 >= -15 & lon5 >= 20 & lon5 < 80 & !is.na(lat5)),1)) %>%
      mutate(regB3 = replace(regB3,which(lat5 <  0 & lat5 >=  -20 & lon5 >= 20 & lon5 < 46 & !is.na(lat5)),1)) %>%
      mutate(regB3 = replace(regB3,which(lat5 < 10 & lat5 >= -15 & lon5 >= 80 & lon5 < 100 & !is.na(lat5)),2)) %>%
      mutate(regB3 = replace(regB3,which(lat5 < -3 & lat5 >= -15 & lon5 >= 100 & lon5 < 110 & !is.na(lat5)),2)) %>%
      mutate(regB3 = replace(regB3,which(lat5 < -7 & lat5 >= -15 & lon5 >= 110 & lon5 < 130 & !is.na(lat5)),2)) %>%
      mutate(regB3 = replace(regB3,which(lat5 <  -20 & lat5 >= -35 & lon5 >= 20 & lon5 < 75 & !is.na(lat5)),3)) %>%
      mutate(regB3 = replace(regB3,which(lat5 <  -15 & lat5 >= -35 & lon5 >= 46 & lon5 < 75 & !is.na(lat5)),3)) %>%
      mutate(regB3 = replace(regB3,which(lat5 <  -15 & lat5 >= -35 & lon5 >= 75 & lon5 < 120 & !is.na(lat5)),4))
  }

  #regA
  if(regA) {
    dat$regA <- 0
    dat <- mutate(dat,regA = replace(regA,which(dat$lat5 < -10 & dat$lat5 >=  -20 & !is.na(dat$lat5)),1)) %>%
    mutate(regA = replace(regA,which(dat$lat5 < -20 & dat$lat5 > -40 & !is.na(dat$lat5)),2))
  }

  if(regA1) {
    dat$regA1 <- 0
    dat <- mutate(dat,regA1 = replace(regA1,which(dat$lat5 < -10 & dat$lat5 >=  -25 & !is.na(dat$lat5)),1)) %>%
    mutate(regA1 = replace(regA1,which(dat$lat5 < -25 & dat$lat5 > -40 & !is.na(dat$lat5)),2))
  }

  if(regA2) {
    dat$regA2 <- 0
    dat <- mutate(dat,regA2 = replace(regA2,which(dat$lat5 < -10 & dat$lat5 >=  -20 & dat$lon5 < 75 & !is.na(dat$lat5)),1)) %>%
    mutate(regA2 = replace(regA2,which(dat$lat5 < -10 & dat$lat5 >=  -20 & dat$lon5 >= 75 & !is.na(dat$lat5)),2)) %>%
    mutate(regA2 = replace(regA2,which(dat$lat5 < -20 & dat$lat5 > -40 & dat$lon5 < 75 & !is.na(dat$lat5)),3)) %>%
    mutate(regA2 = replace(regA2,which(dat$lat5 < -20 & dat$lat5 > -40 & dat$lon5 >= 75 & !is.na(dat$lat5)),4))
  }

  if(regA3) {
    dat$regA3 <- 0
    dat <- mutate(dat,regA3 = replace(regA3,which(dat$lat5 <  -10 & dat$lat5 >=  -25 & dat$lon5 < 75 & !is.na(dat$lat5)),1)) %>%
    mutate(regA3 = replace(regA3,which(dat$lat5 <  -10 & dat$lat5 >=  -25 & dat$lon5 >= 75 & !is.na(dat$lat5)),2)) %>%
    mutate(regA3 = replace(regA3,which(dat$lat5 < -25 & dat$lon5 < 75 & !is.na(dat$lat5)),3)) %>%
    mutate(regA3 = replace(regA3,which(dat$lat5 < -25 & dat$lon5 >= 75 & !is.na(dat$lat5)),4))
  }

  if(regA4) {
    dat$regA4 <- 0
    dat <- mutate(dat,regA4 = replace(regA4,which(dat$lat5 <  -10 & dat$lat5 >=  -25 & dat$lon5 < 75 & !is.na(dat$lat5)),1)) %>%
    mutate(regA4 = replace(regA4,which(dat$lat5 <  -10 & dat$lat5 >=  -25 & dat$lon5 >= 75 & !is.na(dat$lat5)),2)) %>%
    mutate(regA4 = replace(regA4,which(dat$lat5 < -25 & dat$lat5 > -40 & dat$lon5 < 75 & !is.na(dat$lat5)),3)) %>%
    mutate(regA4 = replace(regA4,which(dat$lat5 < -25 & dat$lat5 > -40 & dat$lon5 >= 75 & !is.na(dat$lat5)),4))
  }

  if(regA5) {
    dat$regA5 <- 0
    dat <- mutate(dat,regA5 = replace(regA5,which(dat$lat5 <  -15 & dat$lat5 >=  -45 & dat$lon5 > 55 & dat$lon5 < 100 & !is.na(dat$lat5)),1))
  }
  return(dat)
  }

dataprep_KRSBT <- function(dat,alldat=F) {
  dat$dmy <- as.Date(dat$DATE)
  dat$hbf <- round(dat$hooks / dat$floats,0)
  dat$lat <- dat$Lat01
  dat$lon <- dat$Long01
  dat$lat[dat$NS==2] <- (dat$Lat01[dat$NS==2]+1) * -1
  dat$lon[dat$EW==2] <- 360 - (dat$Long01[dat$EW==2] + 1)
  dat <- dat[dat$lon >= 0,]
  dat <- dat[dat$lat < 29,]

  dat$lat5 <- 5 * floor(dat$lat/5) + 2.5
  dat$lon5 <- 5 * floor(dat$lon/5) + 2.5

  #regSBT   preliminary boudnaries - update later
  dat$regSBT <- 0
  dat[dat$lat <  -30 & dat$lon < 60,]$regSBT <- 1
  dat[dat$lat <  -30 & dat$lon >= 60,]$regSBT <- 2

  dat$yrqtr <- dat$op_yr + floor((dat$op_mon-1)/3)/4 + 0.125
  dat$latlong <- paste(dat$lat5,dat$lon5,sep="_")
  dat$vessid <- as.factor(as.numeric(dat$VESSEL_NAME_rev))
  dat$tripidmon <- paste(dat$vessid,dat$op_yr,dat$op_mon)
  return(dat)
  }

dataprep_JPIO <- function(dat,alldat=T) {
  dat <- dat[order(dat$op_yr,dat$op_mon,dat$op_day),]
  dat$dmy <- ymd(paste(dat$op_yr,dat$op_mon,dat$op_day,sep="-"))
  dat$moon <- lunar.illumination(dat$dmy)

  dat$lat_raw <- dat$lat
  dat$lon_raw <- dat$lon
  dat$lat[dat$latcode==2] <- (dat$lat_raw[dat$latcode==2]+1) * -1
  dat$lon[dat$loncode==2] <- 360 - (dat$lon_raw[dat$loncode==2] + 1)
  dat <- dat[dat$lon >= 0,]

  dat$lat5 <- 5 * floor(dat$lat/5) + 2.5
  dat$lon5 <- 5 * floor(dat$lon/5) + 2.5


  dat$callsign[is.na(dat$callsign)] <- ".     "
  dat$callsign[dat$callsign == "      "] <- ".     "
  dat$vessid <- as.numeric(as.factor(paste(dat$callsign)))
  if (alldat==F) dat <- dat[dat$vessid != 1,]
  dat$vessid <- as.numeric(as.factor(dat$vessid))
  dat$tripidmon <- as.factor(paste(dat$vessid,dat$op_yr,dat$op_mon))

  dat$yrqtr <- dat$op_yr + floor((dat$op_mon-1)/3)/4 + 0.125
  dat$latlong <- paste(dat$lat5,dat$lon5,sep="_")
  dat$Total <- with(dat,alb + bet + yft + sbt + swo + mls + bum + blm)
  dat$Total2 <- apply(dat[,c("bet","yft","alb")],1,sum)

  dat$trip_yr <- as.numeric(substr(as.character(dat$trip_st),1,4))
#  dat <- dat[dat$trip_yr > 1945 | is.na(dat$trip_yr),]

  dat$tripid <- paste(dat$vessid,dat$trip_st,sep="_")
  dat$tripid[dat$vessid == 1] <- NA
  dat$tripid[dat$trip_st == 0] <- NA

  dat$hbf[dat$op_yr < 1976 & is.na(dat$hbf)] <- 5
#  a <- table(dat$tripid)
#  dat$sets_per_trip <- NA
#  dat$sets_per_trip <- a[match(dat$tripid,names(a))]
#  noms <- c("vessid","yrqtr","latlong","op_yr","op_mon","hbf","hooks","tripid","tripidmon","trip_yr","moon",
#  "alb","bet","yft","swo","mls","bum","blm","sbt","Total","Total2","dmy","lat","lon","lat5","lon5",
#  "regY","regY1","regB","regB1","regA","regA1","regA2","regA3")
#  dat <- dat[,noms]

  return(dat)
  }

make_clid <- function(dat) {
  dat$latlong2 <- paste(2*floor(dat$lat/2),2*floor(dat$lon/2),sep="_")
  dat$latlong1 <- paste(dat$lat,dat$lon,sep="_")
  #1
  dat$clid <- NA
  dat$aggid <- paste(dat$op_yr,dat$dmy,dat$latlong1)
  a <- table(dat$aggid)
  usea <- a[a > 14 & a < 1000]
  dat$clid[dat$aggid %in% names(usea)] <- dat$aggid[dat$aggid %in% names(usea)]
  #2
  dat$aggid <- paste(dat$op_yr,week(dat$dmy),dat$latlong1)
  dat$aggid[!is.na(dat$clid)] <- 0
  a <- table(dat$aggid)
  usea <- a[a > 10 & a < 1000]
  dat$clid[dat$aggid %in% names(usea)] <- dat$aggid[dat$aggid %in% names(usea)]
  #3
  dat$aggid <- paste(dat$op_yr,week(dat$dmy),dat$latlong2)
  dat$aggid[!is.na(dat$clid)] <- 0
  a <- table(dat$aggid)
  usea <- a[a > 10 & a < 1000]
  dat$clid[dat$aggid %in% names(usea)] <- dat$aggid[dat$aggid %in% names(usea)]
  #4
  dat$aggid <- paste(dat$op_yr,week(dat$dmy),dat$latlong)
  dat$aggid[!is.na(dat$clid)] <- 0
  a <- table(dat$aggid)
  usea <- a[a > 10 & a < 1000]
  dat$clid[dat$aggid %in% names(usea)] <- dat$aggid[dat$aggid %in% names(usea)]
  #5
  dat$aggid <- paste(dat$op_yr,dat$op_mon,dat$latlong)
  dat$aggid[!is.na(dat$clid)] <- 0 # where clid is already set, change aggid to 0
  a <- table(dat$aggid)
  usea <- a[a<1000]
  dat$clid[dat$aggid %in% names(usea)] <- dat$aggid[dat$aggid %in% names(usea)]
  dat$aggid <- NULL

  dat$jnt_clid <- dat$clid
  dat$jnt_clid[dat$vessid>1] <- dat$tripidmon[dat$vessid>1]
  return(dat)
}

make_lbidmon<- function(dat){
  dat$lbid_mon <- paste(dat$logbookid,dat$op_mon)
  return(dat)
  }


 #   dat$regY <- 0
#  dat[dat$lat5 >=  10 & dat$lon5 < 80 & !is.na(dat$lat5),]$regY <- 1
#  dat[dat$lat5 <  10 & dat$lat5 >=  -10 & dat$lon5 >= 40 & dat$lon5 < 75 & !is.na(dat$lat5),]$regY <- 2
#  dat[dat$lat5 <  -10 & dat$lat5 >=  -15 & dat$lon5 >= 60 & dat$lon5 < 75 & !is.na(dat$lat5),]$regY <- 2
#  dat[dat$lat5 <  -10 & dat$lat5 >= -30 & dat$lon5 >= 20 & dat$lon5 < 60 & !is.na(dat$lat5),]$regY <- 3
#  dat[dat$lat5 <  -30 & dat$lat5 >= -40 & dat$lon5 >= 20 & dat$lon5 < 40 & !is.na(dat$lat5),]$regY <- 3
#  dat[dat$lat5 <  -15 & dat$lat5 >= -40 & dat$lon5 >= 60 & dat$lon5 < 120 & !is.na(dat$lat5),]$regY <- 4
#  dat[dat$lat5 <  -30 & dat$lat5 >= -40 & dat$lon5 >= 40 & dat$lon5 < 60 & !is.na(dat$lat5),]$regY <- 4
#  dat[dat$lat5 < 10 & dat$lat5 >= -15 & dat$lon5 >= 75 & dat$lon5 < 100 & !is.na(dat$lat5),]$regY <- 5
#  dat[dat$lat5 < -5 & dat$lat5 >= -15 & dat$lon5 >= 100 & dat$lon5 < 110 & !is.na(dat$lat5),]$regY <- 5
#  dat[dat$lat5 < -10 & dat$lat5 >= -15 & dat$lon5 >= 110 & dat$lon5 < 130 & !is.na(dat$lat5),]$regY <- 5
#  dat[dat$lat5 < 30 & dat$lat5 >= 10 & dat$lon5 >= 80 & dat$lon5 < 100 & !is.na(dat$lat5),]$regY <- 6
#
#  dat$regY1 <- 0
#  dat$regY1[dat$regY %in% c(1,2)] <- 1
#  dat$regY1[dat$regY %in% c(3)] <- 2
#  dat$regY1[dat$regY %in% c(4)] <- 3
#  dat$regY1[dat$regY %in% c(5,6)] <- 4
#
#  #regB   North of 15S and west of 80 is R1, or north of  20 and west of 45; north of 15S and east of 80 is R2; north of 35S is R3
#  dat$regB <- 0
#  dat[dat$lat5 <  10 & dat$lat5 >=  -15 & dat$lon5 >= 20 & dat$lon5 < 80 & !is.na(dat$lat5),]$regB <- 1
#  dat[dat$lat5 <  10 & dat$lat5 >=  -20 & dat$lon5 >= 20 & dat$lon5 < 46 & !is.na(dat$lat5),]$regB <- 1
#  dat[dat$lat5 < 10 & dat$lat5 >= -15 & dat$lon5 >= 80 & dat$lon5 < 100 & !is.na(dat$lat5),]$regB <- 2
#  dat[dat$lat5 < -3 & dat$lat5 >= -15 & dat$lon5 >= 100 & dat$lon5 < 110 & !is.na(dat$lat5),]$regB <- 2
#  dat[dat$lat5 < -7 & dat$lat5 >= -15 & dat$lon5 >= 110 & dat$lon5 < 130 & !is.na(dat$lat5),]$regB <- 2
#  dat[dat$lat5 <  -20 & dat$lat5 >= -35 & dat$lon5 >= 20 & dat$lon5 < 120 & !is.na(dat$lat5),]$regB <- 3
#  dat[dat$lat5 <  -15 & dat$lat5 >= -35 & dat$lon5 >= 46 & dat$lon5 < 120 & !is.na(dat$lat5),]$regB <- 3
#
#  dat$regB1 <- 0
#  dat$regB1[dat$regB %in% 1:3] <- 1
#
#  #regA
#  dat$regA <- 0
#  dat[dat$lat5 <  25 & dat$lat5 >=  -20 & !is.na(dat$lat5),]$regA <- 1
#  dat[dat$lat5 < -20 & !is.na(dat$lat5),]$regA <- 2
#
#  dat$regA1 <- 0
#  dat[dat$lat5 <  25 & dat$lat5 >=  -25 & !is.na(dat$lat5),]$regA1 <- 1
#  dat[dat$lat5 < -25 & !is.na(dat$lat5),]$regA1 <- 2
#
#  dat$regA2 <- 0
#  dat[dat$lat5 <  25 & dat$lat5 >=  -20 & dat$lon5 < 75 & !is.na(dat$lat5),]$regA2 <- 1
#  dat[dat$lat5 <  25 & dat$lat5 >=  -20 & dat$lon5 >= 75 & !is.na(dat$lat5),]$regA2 <- 2
#  dat[dat$lat5 < -20 & dat$lon5 < 75 & !is.na(dat$lat5),]$regA2 <- 3
#  dat[dat$lat5 < -20 & dat$lon5 >= 75 & !is.na(dat$lat5),]$regA2 <- 4
#
#  dat$regA3 <- 0
#  dat <- mutate(dat,regA3 = replace(regA3,which(dat$lat5 <  -10 & dat$lat5 >=  -25 & dat$lon5 < 75 & !is.na(dat$lat5)),1)) %>%
#  mutate(regA3 = replace(regA3,which(dat$lat5 <  -10 & dat$lat5 >=  -25 & dat$lon5 >= 75 & !is.na(dat$lat5)),2)) %>%
#  mutate(regA3 = replace(regA3,which(dat$lat5 < -25 & dat$lon5 < 75 & !is.na(dat$lat5)),3)) %>%
#  mutate(regA3 = replace(regA3,which(dat$lat5 < -25 & dat$lon5 >= 75 & !is.na(dat$lat5)),4))


dataprep <- function(dat,alldat=F) {
  dat$lat_raw <- dat$lat
  dat$lon_raw <- dat$lon
  dat$lat[dat$latcode==2] <- (dat$lat_raw[dat$latcode==2]+1) * -1
  dat$lon[dat$loncode==2] <- 360 - (dat$lon_raw[dat$loncode==2] + 1)
  dat <- dat[dat$lon >= 0,]

  dat$lat5 <- 5 * floor(dat$lat/5)
  dat$lon5 <- 5 * floor(dat$lon/5)

  dat$reg <- 0
  dat[dat$lat <  40 & dat$lat >=  20 & dat$lon >= 110 & dat$lon < 170,]$reg <- 1
  dat[dat$lat <  40 & dat$lat >=  20 & dat$lon >= 170 & dat$lon < 210,]$reg <- 2
  dat[dat$lat <  20 & dat$lat >= -10 & dat$lon >= 110 & dat$lon < 170,]$reg <- 3
  dat[dat$lat <  20 & dat$lat >= -10 & dat$lon >= 170 & dat$lon < 210,]$reg <- 4
  dat[dat$lat < -10 & dat$lat >= -40 & dat$lon >= 140 & dat$lon < 170,]$reg <- 5
  dat[dat$lat < -10 & dat$lat >= -40 & dat$lon >= 170 & dat$lon < 210,]$reg <- 6
  dat[dat$lat <  40 & dat$lat >=  20 & dat$lon >= 210,]$reg <- 7
  dat[dat$lat <  20 & dat$lat >= -40 & dat$lon >= 210,]$reg <- 8

  dat$subreg <- 0
  dat[dat$lat <  40 & dat$lat >=  20 & dat$lon >= 110 & dat$lon < 170,]$subreg <- 1
  dat[dat$lat <  40 & dat$lat >=  20 & dat$lon >= 170 & dat$lon < 210,]$subreg <- 2
  dat[dat$lat <  20 & dat$lat >=   0 & dat$lon >= 110 & dat$lon < 150,]$subreg <- 3.1
  dat[dat$lat <  20 & dat$lat >=   0 & dat$lon >= 150 & dat$lon < 170,]$subreg <- 3.2
  dat[dat$lat <   0 & dat$lat >= -10 & dat$lon >= 110 & dat$lon < 150,]$subreg <- 3.3
  dat[dat$lat <   0 & dat$lat >= -10 & dat$lon >= 150 & dat$lon < 170,]$subreg <- 3.4
  dat[dat$lat <  20 & dat$lat >= -10 & dat$lon >= 170 & dat$lon < 180,]$subreg <- 4.1
  dat[dat$lat <  20 & dat$lat >= -10 & dat$lon >= 180 & dat$lon < 210,]$subreg <- 4.2
  dat[dat$lat < -10 & dat$lat >= -40 & dat$lon >= 140 & dat$lon < 170,]$subreg <- 5
  dat[dat$lat < -10 & dat$lat >= -40 & dat$lon >= 170 & dat$lon < 210,]$subreg <- 6
  dat[dat$lat <  40 & dat$lat >=  20 & dat$lon >= 210,]$subreg <- 7
  dat[dat$lat <  20 & dat$lat >= -40 & dat$lon >= 210,]$subreg <- 8


  dat$callsign[dat$callsign == "      "] <- ".     "
  dat$vessid <- as.numeric(as.factor(paste(dat$callsign)))
  if (alldat==F) dat <- dat[dat$vessid != 1,]
  dat$vessid <- as.numeric(as.factor(dat$vessid))

  dat$yrqtr <- dat$op_yr + floor((dat$op_mon)/3)/4 + 0.125
  dat$latlong <- paste(dat$lat5,dat$lon5,sep=".")
  dat <- dat[dat$yrqtr < 2012,]
  #dat <- dat[dat$reg %in% 1:6,]

  dat$newfishingcat <- NA
  dat <- dat[dat$fishingcat<=3,]
  dat <- dat[dat$op_yr < 1967 | dat$op_yr > 1970 | dat$fishingcat < 3,]
  dat <- dat[dat$op_yr <= 1957 | dat$fishingcat != ".",]
  dat[dat$op_yr <=1957 & dat$reg %in% c(1),]$newfishingcat <- 1
  dat[dat$op_yr <=1957 & dat$reg %in% c(0,2:8),]$newfishingcat <- 2
  dat[dat$op_yr >1957 & dat$op_yr<=1993 & dat$fishingcat==1,]$newfishingcat <- 1
  dat[dat$op_yr >1993 & dat$fishingcat==3,]$newfishingcat <- 1
  dat[dat$op_yr >1957 & dat$op_yr<=1966 & dat$fishingcat %in% c(2,3),]$newfishingcat <- 2
  dat[dat$op_yr >1966 & dat$op_yr<=1970 & dat$fishingcat %in% c(2),]$newfishingcat <- 2
  dat[dat$op_yr >=1971 & dat$op_yr<=1993 & dat$fishingcat %in% c(2,3),]$newfishingcat <- 2
  dat[dat$op_yr >1993 & dat$fishingcat %in% c(1,2),]$newfishingcat <- 2

  dat <- dat[dat$yrqtr > 1945,]

  dat$trip_yr <- as.numeric(substr(as.character(dat$trip_st),1,4))
  dat <- dat[dat$trip_yr > 1945 | is.na(dat$trip_yr),]

  dat$tripid <- paste(dat$vessid,dat$trip_st,sep="_")
  dat$tripid[dat$vessid == 1] <- NA
  dat$tripid[dat$trip_st == "       0"] <- NA

  a <- table(dat$tripid)
  dat$sets_per_trip <- NA
  dat$sets_per_trip <- a[match(dat$tripid,names(a))]
  return(dat)
  }


get.coefs <- function(model,nyrs) {
  nyrs <- length(unlist(model$xlevels[1]))
  coefs <- exp(c(0,summary(model)$coefficients[2:nyrs,1]))
  coefs <- coefs/mean(coefs)
  return(coefs)
  }

get.bin.coefs2 <- function(model,nyrs,dat) {
  nyrs <- length(unique(dat$yrqtr))
  mn <- logit(mean(dat[,3]!=0))
  a <- c(0,summary(model)$coefficients[2:nyrs,1])
  a <- a - mean(a) + mn
  coefs <- inv.logit(a)
#  coefs <- coefs/mean(coefs)
  return(coefs)
  }

get.bin.coefs <- function(model,nyrs,dat) {
  nyrs <- length(unlist(model$xlevels[1]))
  mn <- logit(mean(model$y))
  a <- c(0,summary(model)$coefficients[2:nyrs,1])
  a <- a - mean(a) + mn
  coefs <- inv.logit(a)
  return(coefs)
  }

get.coefs.summ <- function(modelsumm,nyrs) {
  coefs <- exp(c(0,modelsumm$coefficients[2:nyrs,1]))
  coefs <- coefs/mean(coefs)
  return(coefs)
  }

get.summ.coefs <- function(model,nyrs) {
  coefs <- exp(c(0,model$coefficients[2:nyrs,1]))
  coefs <- coefs/mean(coefs)
  }

get.bin.coefs.summ <- function(modelsumm,nyrs,dat) {
  mn <- logit(mean(modelsumm$deviance.resid > -0.001))
  a <- c(0,modelsumm$coefficients[2:nyrs,1])
  a <- a - mean(a) + mn
  coefs <- inv.logit(a)
  return(coefs)
  }


#get.coefs <- function(model,op_yr) {
#  a <- grep("yrqtr",names(model$coefficients))
#  estyrs <- as.numeric(gsub("as.factor(yrqtr)","",names(model$coefficients)[a],fixed=T))
#  coefs <- exp(c(0,summary(model)$coefficients[2:nyrs,1]))
#  coefs <- coefs/mean(coefs)
#  }

plot.slope.ratio <- function(coefs1,coefs2,yr,titl) {
  # base goes first, boat goes second
  windows(height=14,width=12)
  par(mfrow=c(2,1),mar=c(4,4,3,1))
  plot(yr,coefs1/coefs2,xlim=c(1976,2010),ylim=c(0,2),xlab="Year",ylab="Ratio of coefficients")
  title(main=titl,cex.main=1.2)
  lin <-lm(coefs1/coefs2 ~ yr)
  logl <-lm(log(coefs1/coefs2) ~ yr)
  lines(yr,exp(logl$coefficients[1] + logl$coefficients[2]*yr),lty=3)
  tt <- paste(prettyNum(100*logl$coefficients[2],digits=2,format="f"),
  "% \261 ",prettyNum(100*summary(logl)$coefficients[2,2],digits=2,format="f"),", p = ",
  prettyNum(summary(logl)$coefficients[2,4],
  digits=2,format="f"),sep="")
  text(min(yr)+5, 1.9, tt,font=2,col="red",cex=1.1)
  par(mar=c(5,4,1,1))
  plot(yr,coefs1,type="l",ylab="Relative abundance estimate",xlab="Year",ylim=c(0,2.5))
  lines(yr,coefs2,col="red")
  }

plot.slope.ratio2 <- function(coefs1,coefs2,yr1,yr2,titl) {
  # base goes first, boat goes second
  windows(height=14,width=12)
  par(mfrow=c(2,1),mar=c(4,4,3,1))
  yy <- c(yr1,yr2)
  yr <- seq(min(yy),max(yy),0.25)
  coefs1 <- coefs1[match(yr,yr1)]
  coefs2 <- coefs2[match(yr,yr2)]
  plot(yr,coefs1/coefs2,ylim=c(0,2),xlab="Year",ylab="Ratio of coefficients")
  title(main=titl,cex.main=1.2)
  lin <-lm(coefs1/coefs2 ~ yr)
  logl <-lm(log(coefs1/coefs2) ~ yr)
  lines(yr,exp(logl$coefficients[1] + logl$coefficients[2]*yr),lty=3)
  tt <- paste(prettyNum(100*logl$coefficients[2],digits=2,format="f"),
  "% \261 ",prettyNum(100*summary(logl)$coefficients[2,2],digits=2,format="f"),", p = ",
  prettyNum(summary(logl)$coefficients[2,4],digits=2,format="f"),sep="")
  text(min(yr)+10, 1.9, tt,font=2,col="red",cex=1.1)
  par(mar=c(5,4,1,1))
  plot(yr,coefs1,type="l",ylab="Relative abundance estimate",xlab="Year",ylim=c(0,3.5))
  lines(yr,coefs2,col="red")
  }

plot.agg.slope.ratio <- function(coefs1,coefs2,aggyr,opyr,titl,lab1="coefs1",lab2="coefs2",fname=NULL,addrate=T) {
  # agg goes first, op goes second
  windows(height=16,width=14)
  par(mfrow=c(2,1),mar=c(4,4,3,1))
  myr <- aggyr[match(opyr,aggyr)]
  coefs1 <- coefs1[match(opyr,aggyr)]
  plot(myr,coefs1/coefs2,ylim=c(0,2),xlab="Year",ylab="Ratio of coefficients",cex=0.8)
  title(main=titl,cex.main=1.2)
  legend("bottomright",legend=paste(lab1,"/",lab2),col=1,lty=3)
  lin <-lm(coefs1/coefs2 ~ myr)
  logl <-lm(log(coefs1/coefs2) ~ myr)
  lines(myr,exp(logl$coefficients[1] + logl$coefficients[2]*myr),lty=3)
  if(addrate) {
    tt <- paste(prettyNum(100*logl$coefficients[2],digits=2,format="f"),
    "% \261 ",prettyNum(100*summary(logl)$coefficients[2,2],digits=2,format="f"),", p = ",
    prettyNum(summary(logl)$coefficients[2,4],digits=2,format="f"),sep="")
    text(min(as.numeric(as.character(myr)))+15, 1.9, tt,font=2,col="red",cex=1.1)
    }
  par(mar=c(5,4,1,1))
  plot(myr,coefs1,type="p",ylab="Relative abundance estimate",xlab="Year",ylim=c(0,4),cex=0.7)
  lines(myr,coefs2,col="red")
  legend("topright",legend=c(lab1,lab2),col=1:2,lty=c(0,1),pch=c(1,-1))
  if(is.null(fname)==F) savePlot(paste(fname,lab1,lab2),type="png")
  }

plot.agg.slope.ratio_2000 <- function(coefs1,coefs2,aggyr,opyr,titl,lab1="coefs1",lab2="coefs2") {
  # agg goes first, op goes second
  windows(height=14,width=12)
  par(mfrow=c(2,1),mar=c(4,4,3,1))
  myr <- aggyr[match(opyr,aggyr)]
  coefs1 <- coefs1[match(opyr,aggyr)]
  plot(myr,coefs1/coefs2,xlim=c(1976,2010),ylim=c(0,2),xlab="Year",ylab="Ratio of coefficients")
  title(main=titl,cex.main=1.2)
  legend("bottomright",legend=paste(lab1,"/",lab2),col=1,lty=3)

  lin <-lm(coefs1/coefs2 ~ myr)
  logl <-lm(log(coefs1/coefs2) ~ myr)
  logl2000 <-lm(log(coefs1[myr<=2000]/coefs2[myr<=2000]) ~ myr[myr<=2000])
  lines(myr[myr<=2000],exp(logl2000$coefficients[1] + logl2000$coefficients[2]*myr[myr<=2000]),lty=3)
  tt <- paste(prettyNum(100*logl2000$coefficients[2],digits=2,format="f"),
  "% \261 ",prettyNum(100*summary(logl2000)$coefficients[2,2],digits=2,format="f"),", p = ",
  prettyNum(summary(logl2000)$coefficients[2,4],
  digits=2,format="f"),sep="")
  text(min(myr)+5, 1.9, tt,font=2,col="red",cex=1.1)
  par(mar=c(5,4,1,1))
  plot(myr,coefs1,type="l",ylab="Relative abundance estimate",xlab="Year",ylim=c(0,2.5))
  lines(myr,coefs2,col="red")
  legend("topleft",legend=c(lab1,lab2),col=1:2,lty=c(1,1))
  }

plot.res <- function(summ1,summ2,name1,name2) {
  nyrs <- length(grep("yrqtr",rownames(summ1$coefficients)))+1
  coefs1 <- get.summ.coefs(summ1,nyrs)
  coefs2 <- get.summ.coefs(summ2,nyrs)
  yrs <- rownames(summ1$coefficients)[grep("yrqtr",rownames(summ1$coefficients))]
  yrs <- c(1980.25,as.numeric(substring(yrs,17)))
  windows(height=13,width=11)
  par(mfrow=c(2,1),mar=c(5,4,1,2))
  plot.slope.ratio(coefs1,coefs2,yrs,"")
  plot(yrs,coefs1,type="l",ylab="Relative abundance estimate",xlab="Year",ylim=c(0,2.5))
  lines(yrs,coefs2,col="red")
  legend("topleft",legend=c(name1, name2),lty=c(1,1),col=c("black","red"))
}

make_formula <- function(runsp,modtype,addboat,splitboat=F,addmain=F,addbranch=F,addother=F,addalb=F) {
  fmla <- "~ as.factor(yrqtr) + as.factor(latlong) + poly(hbf,6)"
  modhead <- switch(modtype,logn=paste("log(",runsp,"/hooks+0.01)"),deltabin=paste(runsp,"!= 0"),deltapos=paste("log((",runsp,")/hooks)"),qp=runsp,propn=runsp)
  fmla <- paste(modhead,fmla)
  if(modtype %in% c("deltabin","qp")) fmla <- paste(fmla,"+ ns(hooks,6)")
  if(addboat & !splitboat) fmla <- paste(fmla,"+ as.factor(vessid)")
  if(addboat & splitboat) fmla <- paste(fmla,"+ as.factor(splitvess)")
  if(addmain) fmla <- paste(fmla,"+ as.factor(mainline) + as.factor(mainline):ns(hbf,6)")
  if(addbranch) fmla <- paste(fmla,"+ as.factor(branchline)")
  if(addother) fmla <- paste(fmla,"+ as.factor(other)")
  if(addalb) fmla <- paste(fmla,"+ as.factor(alb_cat)")
  return(fmla)
  }

make_formula_IO <- function(runsp,modtype,addboat,dohbf=T,splitboat=F,addcl=F,addpca=NA,nhbf=6) {
  fmla <- "~ yrqtr + latlong"
  if(dohbf) fmla <- paste0(fmla," + ns(hbf,",nhbf,")")
  modhead <- switch(modtype,
                    logn=paste0("log(",runsp,"/hooks+mn)"),
                    deltabin=paste0(runsp," != 0"),
                    deltapos=paste0("log((",runsp,")/hooks)"),
                    qp=runsp,
                    propn=runsp,
                    negbin=runsp,
                    weibull=paste0("Surv(",runsp,")"))
  fmla <- paste(modhead,fmla)
  #  if(modtype %in% c("deltabin","qp")) fmla <- paste(fmla,"+ ns(hooks,10)")
  fmla <- paste(fmla,"+ ns(hooks,10)")
  if(addboat & !splitboat) fmla <- paste(fmla,"+ vessid")
  if(addboat & splitboat) fmla <- paste(fmla,"+ splitvess")
  if(addcl) fmla <- paste(fmla,"+ clust")
  if(!is.na(addpca)) fmla <- paste(fmla,"+",addpca)
#  fmla <- as.formula(fmla)
  return(fmla)
}

make_formula_IOx <- function(runsp,modtype,addboat,dohbf=T,splitboat=F,addcl=F,addpca=NA,nhbf=6, lat5xqtr=F, lat5xyr=F) {
  fmla <- "~ yrqtr + latlong"
  if (lat5xqtr) fmla <- paste(fmla, "+ lat5:qtr")
  if (lat5xyr) fmla <- paste(fmla, "+ lat5:op_yr")
  if (dohbf) fmla <- paste0(fmla," + ns(hbf,",nhbf,")")
  modhead <- switch(modtype,
                    logn=paste0("log(",runsp,"/hooks+mn)"),
                    deltabin=paste0(runsp," != 0"),
                    deltapos=paste0("log((",runsp,")/hooks)"),
                    qp=runsp,
                    propn=runsp,
                    negbin=runsp,
                    weibull=paste0("Surv(",runsp,")"))
  fmla <- paste(modhead,fmla)
  #  if(modtype %in% c("deltabin","qp")) fmla <- paste(fmla,"+ ns(hooks,10)")
  fmla <- paste(fmla,"+ ns(hooks,10)")
  if(addboat & !splitboat) fmla <- paste(fmla,"+ vessid")
  if(addboat & splitboat) fmla <- paste(fmla,"+ splitvess")
  if(addcl) fmla <- paste(fmla,"+ clust")
  if(!is.na(addpca)) fmla <- paste(fmla,"+",addpca)
  return(fmla)
}

aggregate_data <- function(dat,sp) {
  tab1 <- aggregate(dat[,3], list(dat$yrqtr, dat$latlong, dat$hbf), sum)
  tab2 <- aggregate(dat$hooks, list(dat$yrqtr, dat$latlong, dat$hbf), sum)
  taball <- cbind(tab1[,1:4],tab2[,4])
  names(taball) <-  c("yrqtr", "latlong","hbf",sp,"hooks")
  taball$yrqtr <- as.numeric(as.character(taball$yrqtr))
  taball$latlong <- as.character(taball$latlong)
  taball$hbf <- as.numeric(as.character(taball$hbf))
  taball[,4] <- as.numeric(as.character(taball[,4]))
  taball$hooks <- as.numeric(as.character(taball$hooks))
  return(taball)
  }

mk_wts <- function(dat,wttype,catch=NULL,sp=NULL) {
  if(wttype=="equal") wts <- NULL
  if(wttype=="propn") wts <- catch
  if(wttype=="area") {
    a <- tapply(dat$latlong,list(dat$latlong,dat$yrqtr),length)
    i <- match(dat$latlong,rownames(a))
    j <- match(dat$yrqtr,colnames(a))
    n <- mapply("[", list(a), i, j)
    wts <- 1/n
    }
  if(wttype=="catch") {
    if(is.null(catch)) catch <- tapply(dat[,sp],list(dat$latlong),sum)
    a <- tapply(dat$latlong,list(dat$latlong,dat$yrqtr),length)
    i <- match(dat$latlong,rownames(a))
    j <- match(dat$yrqtr,colnames(a))
    n <- mapply("[", list(a), i, j)
    cwts <- mapply("[", list(catch), i)/sum(catch)
    wts <- cwts/n
    }
  return(wts)
  }

mk_wts_integer <- function(dat,wttype,catch=NULL) {
  if(wttype=="equal") wts <- NULL
  if(wttype=="area") {
    a <- tapply(dat$latlong,list(dat$latlong,dat$yrqtr),length)
    i <- match(dat$latlong,rownames(a))
    j <- match(dat$yrqtr,colnames(a))
    n <- mapply("[", list(a), i, j)
    wts <- 1/n
    wts <- floor(10 * max(n) * wts)
    }
  if(wttype=="catch") {
    if(is.null(catch)) catch <- tapply(dat$bet,list(dat$latlong),sum)
    a <- tapply(dat$latlong,list(dat$latlong,dat$yrqtr),length)
    i <- match(dat$latlong,rownames(a))
    j <- match(dat$yrqtr,colnames(a))
    n <- mapply("[", list(a), i, j)
    cwts <- mapply("[", list(catch), i)/sum(catch)
    wts <- cwts/n
    }
  return(wts)
  }

# p(bet !=0) ~ yrqtr + latlong + poly(hbf,6) + ns(hooks,6) + vessel id
# log(bet) ~ yrqtr + latlong + poly(hbf,6) + vessel id

# select the dataset
select_data <- function(indat,runreg,runsp,mt,minqtrs=2,maxqtrs=500,addmain=F,addother=F,addalb=F,fc="both",bait="no23",llstrat=5,doboth=F,addcl=F) {
  gdat <- indat[indat$reg==runreg,]
  if(runreg==9) gdat <- indat[indat$reg==3 & indat$lat >= -5 & indat$lat <10,]
  if(llstrat!=5) gdat$latlong <- paste(llstrat*floor(gdat$lat/llstrat),llstrat*floor(gdat$lon/llstrat),sep="_")
  if(mt=="deltapos") gdat <- gdat[gdat[,runsp] > 0,]
  if(bait=="no23") gdat <- gdat[(gdat$bait!=2 & gdat$bait !=3) | is.na(gdat$bait),]
  a <- table(gdat$vessid,gdat$yrqtr)
  a <- apply(a>0,1,sum)
  table(a)
  a <- a[a >= minqtrs & a <= maxqtrs]
  gdat <- gdat[gdat$vessid %in% names(a),]
  a <- table(gdat$yrqtr);a
  a <- a[a>=100]
  gdat <- gdat[gdat$yrqtr %in% names(a),]
  a <- table(gdat$vessid);a
  a <- a[a>=100]
  gdat <- gdat[gdat$vessid %in% names(a),]
  if(sum(is.na(gdat$hbf)) > 0) gdat[is.na(gdat$hbf),]$hbf <- 5
  gdat <- gdat[gdat$hbf >= 5,]
  fcnum <- switch(fc,"both"=0,"OS"=1,"DW"=2)
  if (fc!="both") gdat <- gdat[gdat$newfishingcat==fcnum,]
  if(addmain) {
    gdat <- gdat[gdat$target==3,c("vessid","hooks","yft", "bet", "alb", "hbf", "yrqtr","latlong","mainline","branchline")]
    gdat <- gdat[gdat$mainline %in% c(1,2),]
    gdat <- gdat[gdat$branchline %in% c(1,2),]
    } else {
    gdat <- gdat[,c("vessid","hooks","yft", "bet", "alb", "hbf", "yrqtr","latlong")]
    }
  if(addother) {
      a <- (gdat[,switch(runsp,"bet"="yft","yft"="bet")]+1)/gdat$hooks
      divs <- c(0,0.1, 0.5, 0.9,1)
      b <- quantile(a,divs)
      gdat$other <- findInterval(a,b)
      }
  if(addalb) {
      a <- (gdat[,"alb"]+1)/gdat$hooks
      divs <- c(0,0.1, 0.5, 0.9,1)
      b <- quantile(a,divs)
      gdat$alb_cat <- findInterval(a,b)
      }
  a <- grep(switch(runsp,"bet"="yft","yft"="bet"),names(gdat))
  if(!doboth) gdat <- gdat[,-a]
  a <- grep("alb",names(gdat),fixed=T)[1]
  gdat <- gdat[,-a]
  return(gdat)
  }

make_strat <- function(dat) {
  a <- tapply(dat$latlong,list(dat$latlong,dat$yrqtr),length)
  i <- match(dat$latlong,rownames(a))
  j <- match(dat$yrqtr,colnames(a))
  n <- mapply("[", list(a), i, j)
  return(n) # n
  }

samp_data <- function(dat,n,nsamp) {
  p <- nsamp / n
  r <- runif(length(n))
  d2 <- dat[r<p,]
  return(d2)
  }

samp_strat_data <- function(dd,nsamp) {
  a <- tapply(dd$latlong,list(dd$latlong,dd$yrqtr),length)
  i <- match(dd$latlong,rownames(a))
  j <- match(dd$yrqtr,colnames(a))
  n <- mapply("[", list(a), i, j)
  p <- nsamp / n
  r <- runif(length(n))
  d2 <- dd[r<p,]
  return(d2)
  }

samp_data2 <- function(dat,p) {
  nn <- dim(dat)[1]
  nsamp <- floor(nn * p)
  r <- runif(nn)
  d1 <- dat[order(r),]
  d2 <- d1[1:nsamp,]
  return(d2)
  }

select_data_JPIO <- function(indat,runreg,runsp,mt,minqtrs=2,maxqtrs=500,minvess=100,minll=100,minyrqtr=100,
                 llstrat=5,addcl=NA,addpca=NA,samp=NA,strsmp=30) {
  gdat <- indat[indat$reg==runreg,]
  if(sum(is.na(gdat$hbf)) > 0) gdat[is.na(gdat$hbf),]$hbf <- 5
  gdat <- gdat[gdat$hbf >= 5,]
  if(llstrat!=5) gdat$latlong <- paste(llstrat*floor(gdat$lat/llstrat),llstrat*floor(gdat$lon/llstrat),sep="_")
  if(mt=="deltapos") gdat <- gdat[gdat[,runsp] > 0,]
  a <- table(gdat$vessid,gdat$yrqtr)
  a <- apply(a>0,1,sum)
  table(a)
  a <- a[a >= minqtrs & a <= maxqtrs]
  gdat <- gdat[gdat$vessid %in% names(a),]
  a <- table(gdat$yrqtr);a
  a <- a[a>=minyrqtr]
  gdat <- gdat[gdat$yrqtr %in% names(a),]
  a <- table(gdat$latlong);a
  a <- a[a>=minll]
  gdat <- gdat[gdat$latlong %in% names(a),]
  a <- table(gdat$vessid);a
  a <- a[a>=minvess]
  gdat <- gdat[gdat$vessid %in% names(a),]
  vars <- c("vessid","hooks", "hbf", "yrqtr","latlong", "moon")
  if(!is.na(addcl)) vars <- c(vars,addcl)
  if(!is.na(addpca)) vars <- c(vars,addpca)
  vars <- c(vars,runsp)
  gdat <- gdat[,vars]
  names(gdat)[names(gdat)==addcl] <- "clust"
  if(!is.na(samp)) gdat <- samp_data2(gdat,samp)
  if(!is.na(strsmp)) gdat <- samp_strat_data(gdat,strsmp)
  gdat$vessid <- as.factor(gdat$vessid)
  gdat$latlong <- as.factor(gdat$latlong)
  gdat$yrqtr <- as.factor(gdat$yrqtr)
  gdat$clust <- as.factor(gdat$clust)
  return(gdat)
  }

select_data_TWIO <- function(indat,runreg,clk=NA,runsp,mt,minqtrs=2,maxqtrs=500,minvess=100,minll=100,minyrqtr=100,
                 llstrat=5,addcl=NA,addpca=NA,samp=NA,strsmp=30) {
  gdat <- indat[indat$reg==runreg,]
  if(sum(is.na(gdat$hbf)) > 0) gdat[is.na(gdat$hbf),]$hbf <- 5
  gdat <- gdat[gdat$hbf >= 5,]
  if(!is.na(clk)) {
    clk <- clkeep[[runsp]][[runreg]]
    gdat <- gdat[gdat$clust %in% clk[[runsp]][[runreg]],]
    }
  if(llstrat!=5) gdat$latlong <- paste(llstrat*floor(gdat$lat/llstrat),llstrat*floor(gdat$lon/llstrat),sep="_")
  if(mt=="deltapos") gdat <- gdat[gdat[,runsp] > 0,]
  a <- table(gdat$vessid,gdat$yrqtr)
  a <- apply(a>0,1,sum)
  table(a)
  a <- a[a >= minqtrs & a <= maxqtrs]
  gdat <- gdat[gdat$vessid %in% names(a),]
  a <- table(gdat$yrqtr);a
  a <- a[a>=minyrqtr]
  gdat <- gdat[gdat$yrqtr %in% names(a),]
  a <- table(gdat$latlong);a
  a <- a[a>=minll]
  gdat <- gdat[gdat$latlong %in% names(a),]
  a <- table(gdat$vessid);a
  a <- a[a>=minvess]
  gdat <- gdat[gdat$vessid %in% names(a),]
  vars <- c("vessid","hooks","yft", "bet", "yrqtr","latlong", "moon","bt1","bt2","bt3","bt4","bt5")
  if(!is.na(addcl)) vars <- c(vars,addcl)
  if(!is.na(addpca)) vars <- c(vars,addpca)
  gdat <- gdat[,vars]
  a <- grep(switch(runsp,"bet"="yft","yft"="bet"),names(gdat))
  gdat <- gdat[,-a]
  if(!is.na(samp)) gdat <- samp_data2(gdat,samp)
  gdat$vessid <- as.factor(gdat$vessid)
  gdat$latlong <- as.factor(gdat$latlong)
  gdat$yrqtr <- as.factor(gdat$yrqtr)
  return(gdat)
  }

select_data_JointIO <- function(indat,runreg,clk=NA,runsp,mt,vars,minqtrs=2,maxqtrs=500,minvess=100,minll=100,minyrqtr=100,
                 llstrat=5,addcl=F,cltype=NA,addpca=NA,samp=NA,strsmp=30,yrlims=NA,oneflag=NA) {
  gdat <- indat[indat$reg==runreg,]
  if(!is.na(yrlims[1])) gdat <- gdat[gdat$yrqtr > yrlims[1] & gdat$yrqtr < yrlims[2],]
  if(addcl) {
    names(gdat)[names(gdat)==cltype] <- "clust"
    a <- data.frame()
    if(!is.na(clk[1])) {
      if(!is.na(oneflag))
        {
        gdat <- rbind(a,gdat[gdat$clust %in% clk[[oneflag]][[runsp]][[runreg]],])
        }
      else {
        a <- rbind(a,gdat[gdat$clust %in% clk$JP[[runsp]][[runreg]] & gdat$flag=="JP",])
        a <- rbind(a,gdat[gdat$clust %in% clk$KR[[runsp]][[runreg]] & gdat$flag=="KR",])
        a <- rbind(a,gdat[gdat$clust %in% clk$SY[[runsp]][[runreg]] & gdat$flag=="SY",])
        gdat <- rbind(a,gdat[gdat$clust %in% clk$TW[[runsp]][[runreg]] & gdat$flag=="TW",])
        }
      }
    gdat$clust <- as.factor(paste0(gdat$flag,gdat$clust))
    vars <- c(vars,"clust")
  }
  if(dim(gdat)[1] > 0) {
    if(sum(is.na(gdat$hbf)) > 0) gdat[is.na(gdat$hbf),]$hbf <- 5
    if(sum(gdat$hbf ==0) > 0) gdat[gdat$hbf == 0,]$hbf <- 5
    gdat <- gdat[gdat$hbf >= 5,]
    if(llstrat!=5) gdat$latlong <- paste(llstrat*floor(gdat$lat/llstrat),llstrat*floor(gdat$lon/llstrat),sep="_")
    if(mt=="deltapos") gdat <- gdat[gdat[,runsp] > 0,]

    a <- table(gdat$vessid,gdat$yrqtr)
    a <- apply(a>0,1,sum)
    a <- a[a >= minqtrs & a <= maxqtrs]      # Vessel fishes in at least 'minqtrs' quarters
    gdat <- gdat[gdat$vessid %in% names(a),]
    a <- table(gdat$yrqtr)
    a <- a[a>=minyrqtr]                      # At least 'minyrqtr' sets in the yrqtr
    gdat <- gdat[gdat$yrqtr %in% names(a),]
    a <- table(gdat$latlong)
    a <- a[a>=minll]                         # At least 'minll' sets in the cell
    gdat <- gdat[gdat$latlong %in% names(a),]
    a <- table(gdat$vessid)
    a <- a[a >= minvess]                       # At least 'minvess' sets by the vessel
    gdat <- gdat[gdat$vessid %in% names(a),]

    if(!is.na(addpca)) vars <- c(vars,addpca)
    vars <- c(vars,runsp)
    gdat <- gdat[,vars]
    if(!is.na(samp)) gdat <- samp_data2(gdat,samp)
    if(!is.na(strsmp)) gdat <- samp_strat_data(gdat,strsmp)
    gdat$vessid <- as.factor(gdat$vessid)
    gdat$latlong <- as.factor(gdat$latlong)
    gdat$yrqtr <- as.factor(gdat$yrqtr)
    if(addcl!=F) gdat$clust <- as.factor(gdat$clust)
  }
  return(gdat)
  }

select_data_IO <- function(indat,runreg,clk=NA,runsp,mt,vars,minqtrs=2,maxqtrs=500,minvess=100,minll=100,minyrqtr=100,
                 llstrat=5,addcl=F,cltype=NA,addpca=NA,samp=NA,strsmp=30) {
  gdat <- indat[indat$reg==runreg,]
  if(addcl) names(gdat)[names(gdat)==cltype] <- "clust"
  if(sum(is.na(gdat$hbf)) > 0) gdat[is.na(gdat$hbf),]$hbf <- 5
  gdat <- gdat[gdat$hbf >= 5,]
  if(!is.na(clk[1])) gdat <- gdat[gdat$clust %in% clk,]
  if(llstrat!=5) gdat$latlong <- paste(llstrat*floor(gdat$lat/llstrat),llstrat*floor(gdat$lon/llstrat),sep="_")
  if(mt=="deltapos") gdat <- gdat[gdat[,runsp] > 0,]
  a <- table(gdat$vessid,gdat$yrqtr)
  a <- apply(a>0,1,sum)
  a <- a[a >= minqtrs & a <= maxqtrs]
  gdat <- gdat[gdat$vessid %in% names(a),]
  a <- table(gdat$yrqtr);a
  a <- a[a>=minyrqtr]
  gdat <- gdat[gdat$yrqtr %in% names(a),]
  a <- table(gdat$latlong);a
  a <- a[a>=minll]
  gdat <- gdat[gdat$latlong %in% names(a),]
  a <- table(gdat$vessid);a
  a <- a[a>=minvess]
  gdat <- gdat[gdat$vessid %in% names(a),]
  vars <- c(vars,"clust")
  if(!is.na(addpca)) vars <- c(vars,addpca)
  vars <- c(vars,runsp)
  gdat <- gdat[,vars]
  if(!is.na(samp)) gdat <- samp_data2(gdat,samp)
  if(!is.na(strsmp)) gdat <- samp_strat_data(gdat,strsmp)
  gdat$vessid <- as.factor(gdat$vessid)
  gdat$latlong <- as.factor(gdat$latlong)
  gdat$yrqtr <- as.factor(gdat$yrqtr)
  gdat$clust <- as.factor(gdat$clust)
  return(gdat)
  }


select_data_TW <- function(indat,runreg,runsp,mt,minqtrs=2,maxqtrs=500,addmain=F,addother=F,addalb=F,fc="both",bait="no23",llstrat=5,doboth=F) {
  gdat <- indat[indat$reg==runreg,]
  if(llstrat!=5) gdat$latlong <- paste(llstrat*floor(gdat$lat/llstrat),llstrat*floor(gdat$lon/llstrat),sep="_")
  if(mt=="deltapos") gdat <- gdat[gdat[,runsp] > 0,]
  if(bait=="no23") gdat <- gdat[(gdat$bait!=2 & gdat$bait !=3) | is.na(gdat$bait),]
  a <- table(gdat$vessid,gdat$yrqtr)
  a <- apply(a>0,1,sum)
  table(a)
  a <- a[a >= minqtrs & a <= maxqtrs]
  gdat <- gdat[gdat$vessid %in% names(a),]
  a <- table(gdat$yrqtr);a
  a <- a[a>=100]
  gdat <- gdat[gdat$yrqtr %in% names(a),]
  a <- table(gdat$vessid);a
  a <- a[a>=100]
  gdat <- gdat[gdat$vessid %in% names(a),]
  if(sum(is.na(gdat$hbf)) > 0) gdat[is.na(gdat$hbf),]$hbf <- 5
  gdat <- gdat[gdat$hbf >= 5,]
  fcnum <- switch(fc,"both"=0,"OS"=1,"DW"=2)
  if (fc!="both") gdat <- gdat[gdat$newfishingcat==fcnum,]
  if(addmain) {
    gdat <- gdat[gdat$target==3,c("vessid","hooks","yft", "bet", "alb", "hbf", "yrqtr","latlong","mainline","branchline")]
    gdat <- gdat[gdat$mainline %in% c(1,2),]
    gdat <- gdat[gdat$branchline %in% c(1,2),]
    } else {
    gdat <- gdat[,c("vessid","hooks","yft", "bet", "alb", "hbf", "yrqtr","latlong")]
    }
  if(addother) {
      a <- (gdat[,switch(runsp,"bet"="yft","yft"="bet")]+1)/gdat$hooks
      divs <- c(0,0.1, 0.5, 0.9,1)
      b <- quantile(a,divs)
      gdat$other <- findInterval(a,b)
      }
  if(addalb) {
      a <- (gdat[,"alb"]+1)/gdat$hooks
      divs <- c(0,0.1, 0.5, 0.9,1)
      b <- quantile(a,divs)
      gdat$alb_cat <- findInterval(a,b)
      }
  a <- grep(switch(runsp,"bet"="yft","yft"="bet"),names(gdat))
  if(!doboth) gdat <- gdat[,-a]
  a <- grep("alb",names(gdat),fixed=T)[1]
  gdat <- gdat[,-a]
  return(gdat)
  }

qqDist <- function (x, standardise = F, add.median = F, ...)
{
    n <- length(x)
    seq.length <- min(1000, n)
    if (standardise) {
        SEQ <- seq(1, 2 * n + 1, length = seq.length)/2
        U <- qnorm(qbeta(0.975, SEQ, rev(SEQ)))
        L <- qnorm(qbeta(0.025, SEQ, rev(SEQ)))
        if (add.median)
            M <- qnorm(qbeta(0.5, SEQ, rev(SEQ)))
    }
    else {
        SD <- sqrt(var(x) * (n + 1)/n)
        SEQ <- seq(1, 2 * n + 1, length = seq.length)/2
        U <- mean(x) + SD * qt(qbeta(0.975, SEQ, rev(SEQ)), n -
            1)
        L <- mean(x) + SD * qt(qbeta(0.025, SEQ, rev(SEQ)), n -
            1)
        if (add.median)
            M <- mean(x) + SD * qt(qbeta(0.5, SEQ, rev(SEQ)),
                n - 1)
    }
    X <- qnorm((SEQ - 0.25)/(n + 0.5))
    qqnorm(x, main = "", ...)
    lines(X, U, type = "l",col=2)
    lines(X, L, type = "l",col=2)
    if (add.median)
        lines(X, M, type = "l",col=2)
    invisible()
}

plotdiags <- function(res,ti="") {
  hist(res,nclass=200,freq=F,xlab="Residuals",main=ti)
  lines((-30:30)/10,dnorm((-30:30)/10,sd=sd(res)),col=2)
  sdres <- res/sd(res)
  qqDist(sdres,add.median=T)
  }

splitvessels <- function(indat,period) {
  vess <- unique(indat$vessid)
  indat$oldvess <- indat$vessid
  indat$vessid <- ""
  minyr <- maxyr <- vess
  for (i in 1:length(vess)) {
    a <- grep(vess[i],indat$oldvess)
    minyr <- min(indat[a,]$yrqtr)
    indat[a,]$vessid <- paste(indat[a,]$oldvess,floor((indat[a,]$yrqtr - minyr)/period))
    }
  return(indat)
  }

mt <- "deltabin"
mt <- "deltapos"

plot_effects <- function(model,indat,addmain=F,addbranch=F,addalb=F,addother=F,ti="") {
  cf <- model$coefficients
  pred <- predict(model,data=indat,type="terms",se.fit=T)
  fishlab <- switch(runsp,yft="Yellowfin",bet="Bigeye"); methlab <- switch(mt,deltabin="Delta-binomial",deltapos="Delta-positive",logl="Lognormal(+0.01)",propn="Proportion Bigeye")
  nfigs <- 6 + addmain + addbranch + addalb + addother
  mf <- switch(nfigs-5,c(2,3),c(3,3),c(3,3),c(3,3),c(3,4))
  hw <- c(14,19)
  windows(height=hw[1],width=hw[2])
  par(mfrow=mf,mar=c(5,4,2,1),oma=c(0,0,3,0))
  pr <- pred$fit ; prse <- pred$se.fit
  termlist <- dimnames(pred$fit)[[2]]
  llpos <- grep("latlong",termlist)
  hbfpos  <- grep("hbf",termlist)
  mainpos <- grep("mainline",termlist)
  vesspos <- grep("vessid",termlist)
  branchpos <- grep("branchline",termlist)
  if(length(grep("hooks",termlist))>0) db <- T else db <- F

  index <- sort(unique(indat$yrqtr))
  b <- match(index,indat$yrqtr)
  out <- exp(pr[b,1])
  se1 <- exp(pr[b,1] - 1.96 * prse[b,1])
  se2 <- exp(pr[b,1] + 1.96 * prse[b,1])
  plot(as.numeric(as.character(index)),out,ylim=c(0,3),xlab="Year",ylab="Effect size",main="Year effects")
  segments(as.numeric(as.character(index)), se1,  as.numeric(as.character(index)), se2, lty=1, col="slate grey")
  points(as.numeric(as.character(index)), out, pch=16)

  index <- sort(unique(indat$latlong))
  b <- match(index,indat$latlong)
  out <- exp(pr[b,llpos])
  se1 <- exp(pr[b,llpos] - 1.96 * prse[b,llpos])
  se2 <- exp(pr[b,llpos] + 1.96 * prse[b,llpos])
  plot(as.numeric(as.character(index)),out,ylim=c(0,3),xlab="Latlong",ylab="Effect size",main="Spatial effects")
  segments(as.numeric(as.character(index)), se1,  as.numeric(as.character(index)), se2, lty=1, col="slate grey")
  points(as.numeric(as.character(index)), out, pch=16)
#  plot(indat$latlong,pr[,2],ylim=c(-.75,.75),xlab="Latitude",ylab="Effect size")

  ##plot coefficients
  library(maps)
  ll <- as.numeric(indat$latlong)
  index <- sort(unique(ll))
  lats <- trunc(ll)
  lons <- 1000*abs((ll - trunc(ll)))
  alat <- round(lats[match(index, ll)]*2)/2 + 2.5
  alon <- round(lons[match(index, ll)]*2)/2 + 2.5
# lat <- trunc((dat$newlat[match(index, dat$latlong)] + 50)/5) * 5 + 2.5 - 50
# long <- trunc((dat$newlong[match(index, dat$latlong)])/5) * 5 + 2.5
  coefs <- exp(pr[match(index, ll),llpos])
  coefs2 <- tapply(coefs, list(alon, alat), mean)
  image(sort(as.numeric(unique(alon))), sort(unique(alat)), coefs2, zlim=c(0.5, 2.5), ylab="Lat", xlab="Long")
  contour(sort(unique(alon)), sort(unique(alat)), coefs2, levels= c(0,1,2,2.5), add=TRUE,col=4)
  map(add=TRUE)

  index <- sort(unique(indat$vessid))
  b <- match(index,indat$vessid)
  out <- exp(pr[b,vesspos])
  se1 <- exp(pr[b,vesspos] - 1.96 * prse[b,vesspos])
  se2 <- exp(pr[b,vesspos] + 1.96 * prse[b,vesspos])
  plot(as.factor(index),out,type="n",ylim=c(0,3),xlab="Vessel ID",ylab="Effect size",main="")
  segments(match(index,index), se1,  match(index,index), se2, lty=1, col="slate grey")
  points(as.factor(index), out, pch=16)

  vts <- tapply(pr[,vesspos],list(indat$yrqtr,indat$vessid),mean)
  y<-exp(as.vector(vts))
  x<-rep(as.numeric(dimnames(vts)[[1]]),dim(vts)[[2]])
  plot(x,y,type="p",ylim=c(0,3),xlab="Vessel ID",ylab="Effect size",main="Vessel effects",pch=16,cex=0.9)
  mn <- tapply(exp(pr[,vesspos]),indat$yrqtr,mean)
  lines(as.numeric(dimnames(vts)[[1]]), mn, col=2,lwd=2)
#  plot(as.factor(indat$vessid),pr[,vesspos],ylim=c(-.75,.75),xlab="Vessel",ylab="Effect size")

  rsp <- pr[,hbfpos[1]]
  rsp.se <- prse[,hbfpos[1]]
  if(addmain) {
    a <- indat$mainline==1
    rsp2 <- pr[,hbfpos[1]] + pr[,hbfpos[2]] + pr[,mainpos[1]]
    rsp2.se <- exp(prse[,hbfpos[1]] + prse[,hbfpos[2]] + prse[,mainpos[1]])
    b <- match(sort(unique(indat[a,]$hbf)),indat[a,]$hbf)
    plot(indat[a,]$hbf[b],exp(rsp[a][b]),ylim=c(0,10),xlab="Nylon mainline HBF",ylab="Effect size",main=paste("Nylon HBF"))
    lines(indat[a,]$hbf[b],exp(rsp[a][b]+1.96*rsp.se[a][b]),lty=2)
    lines(indat[a,]$hbf[b],exp(rsp[a][b]-1.96*rsp.se[a][b]),lty=2)
    b <- match(sort(unique(indat[a==F,]$hbf)),indat[a==F,]$hbf)
    plot(indat[a==F,]$hbf[b],exp(rsp2[a==F][b]),ylim=c(0,3),xlab="Other mainline HBF",ylab="Effect size",main = "Other HBF")
    lines(indat[a==F,]$hbf[b],exp(rsp2[a==F][b] + 1.96*rsp2.se[a==F][b]),lty=2)
    lines(indat[a==F,]$hbf[b],exp(rsp2[a==F][b] - 1.96*rsp2.se[a==F][b]),lty=2)
    } else {
    b <- match(sort(unique(indat$hbf)),indat$hbf)
    plot(indat$hbf[b],exp(pr[,hbfpos][b]),ylim=c(0,3),xlab="HBF",ylab="Effect size",main="HBF effects")
    lines(indat$hbf[b],exp(rsp[b]+2*rsp.se[b]),lty=2)
    lines(indat$hbf[b],exp(rsp[b]-2*rsp.se[b]),lty=2)
    }
}

glm.coefs <- function(model, dat) {
  nms <- names(dat)
  bval <- data.frame(t(rep(0, length(nms))))
  names(bval) <- nms
  facs <- sapply(dat[1,],is.factor)
  for (i in 1:length(names(dat))) {
    if (facs[i])  bval[,i] <- Mode(dat[,i])
    if (!facs[i]) bval[,i] <- median(dat[,i])
  }
  bval <- bval[-grep("yrqtr",nms)]
  yrqtr <- data.frame(yrqtr = sort(unique(dat$yrqtr)))
  newdat <- expand.grid.df(as.data.frame(bval),yrqtr)
  res <- predict(model, newdata=newdat, type = "response", se.fit = TRUE)
  res.te <- predict(model, newdata=newdat, type = "terms", se.fit = TRUE)
  newdat$fitx <- res$fit
  newdat$fitx.se <- res.te$se.fit[,"yrqtr"]
  if (model$family$family == "gaussian") {
    newdat$fit <- exp(newdat$fitx)
    mout <- mean(newdat$fit)
    newdat$hi <- exp(newdat$fitx + 1.96 * newdat$fitx.se) / mout
    newdat$lo <- exp(newdat$fitx - 1.96 * newdat$fitx.se) / mout
    newdat$fit <- newdat$fit / mout
  } else {
    newdat$fit <- newdat$fitx
    newdat$hi <- inv.logit(logit(newdat$fit) + 1.96 * newdat$fitx.se)
    newdat$lo <- inv.logit(logit(newdat$fit) - 1.96 * newdat$fitx.se)
  }
  return(newdat)
}

get_base_newdat <- function(dat) {
  nms <- names(dat)
  bval <- data.frame(t(rep(0, length(nms))))
  names(bval) <- nms
  facs <- sapply(dat[1,],is.factor)
  for (i in 1:length(names(dat))) {
    if (facs[i])  bval[,i] <- Mode(dat[,i])
    if (!facs[i]) bval[,i] <- median(dat[,i])
  }
  bval <- bval[-grep("yrqtr",nms)]
  yrqtr <- data.frame(yrqtr = sort(unique(dat$yrqtr)))
  newdat <- expand.grid.df(as.data.frame(bval),yrqtr)
  return(newdat)
}

glm.coefs2 <- function(model, dat, parm="yrqtr") {
  nms <- names(dat)
  bval <- data.frame(t(rep(0, length(nms))))
  names(bval) <- nms
  facs <- sapply(dat[1,],is.factor)
  for (i in 1:length(names(dat))) {
    if (facs[i])  bval[,i] <- Mode(dat[,i])
    if (!facs[i]) bval[,i] <- median(dat[,i])
  }
  bval <- bval[-grep(parm, nms)]
  pm <- data.frame(pm = sort(unique(dat[,parm])))
  newdat <- expand.grid.df(as.data.frame(bval),pm)
  names(newdat)[grep("pm", names(newdat))] <- parm
  res <- predict(model, newdata=newdat, type = "response", se.fit = TRUE)
  res.te <- predict(model, newdata=newdat, type = "terms", se.fit = TRUE)
  newdat$fitx <- res$fit
  pos <- grep(parm, dimnames(res.te$fit)[[2]])
  newdat$fitx.se <- res.te$se.fit[,pos]
  if (model$family$family == "gaussian") {
    newdat$fit <- exp(newdat$fitx)
    mout <- mean(newdat$fit)
    newdat$hi <- exp(newdat$fitx + 1.96 * newdat$fitx.se) / mout
    newdat$lo <- exp(newdat$fitx - 1.96 * newdat$fitx.se) / mout
    newdat$fit <- newdat$fit / mout
  } else {
    newdat$fit <- newdat$fitx
    newdat$hi <- inv.logit(logit(newdat$fit) + 1.96 * newdat$fitx.se)
    newdat$lo <- inv.logit(logit(newdat$fit) - 1.96 * newdat$fitx.se)
  }
  return(newdat)
}

expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))

plot_effects_IO <- function(model,indat,dovess=T,addcl=F,addpca=F,ti="",dohbf=T) {
  cf <- model$coefficients
  fam <- model$family$family
  pred <- predict(model,data=indat,type="terms",se.fit=T)
  fishlab <- switch(runsp,yft="Yellowfin",bet="Bigeye"); methlab <- switch(mt,deltabin="Delta-binomial",deltapos="Delta-positive",logl="Lognormal(+0.01)",propn="Proportion Bigeye")
  pr <- pred$fit ; prse <- pred$se.fit
  termlist <- dimnames(pred$fit)[[2]]
  docl = "clust" %in% termlist
  llpos <- grep("latlong",termlist)
  hbfpos  <- grep("hbf",termlist)
  vesspos <- grep("vessid",termlist)
  branchpos <- grep("branchline",termlist)
  if(length(grep("hooks",termlist))>0) db <- T else db <- F

  nfigs = 3 + 2 * dovess + docl + addpca + dohbf
  mf <- switch(nfigs-2,c(2,2), c(2,2), c(2,3),c(2,3),c(3,3),c(3,3),c(3,3),c(3,4))
  hw <- c(14,19)
  windows(height=hw[1],width=hw[2])
  par(mfrow=mf,mar=c(5,4,2,1),oma=c(0,0,3,0))

    coefs <- glm.coefs(model, indat)
  if(fam=="gaussian") lims <- c(0, 3) else lims <- c(0, 1)
  with(coefs, plot(as.numeric(as.character(yrqtr)), fit, ylim = lims, xlab="Year", ylab="Effect size", main="Year effects"))
  with(coefs, segments(as.numeric(as.character(yrqtr)), lo,  as.numeric(as.character(yrqtr)), hi, lty=1, col="slate grey"))
  with(coefs, points(as.numeric(as.character(yrqtr)), fit, pch=16))

  coefs <- glm.coefs2(model, indat, parm = "latlong")
  with(coefs, plot(as.numeric(latlong), fit, ylim = lims, xlab="Latlong", ylab="Effect size", main="Spatial effects"))
  with(coefs, segments(as.numeric(latlong), lo,  as.numeric(latlong), hi, lty=1, col="slate grey"))
  with(coefs, points(as.numeric(latlong), fit, pch=16))

  if(fam=="gaussian") trans <- function(x) exp(x)
  if(fam!="gaussian") trans <- function(x) inv.logit(x)

    ##plot coefficients
  library(maps)
  ll <- as.character(indat$latlong)
  # browser()
  index <- sort(unique(ll))
  index2 <- unlist(strsplit(index,"\\_"))
  pos1 <- 2*(1:length(index)) -1
  pos2 <- pos1 + 1
  lats <- as.numeric(index2[pos1])
  lons <- as.numeric(index2[pos2])
  alat <- round(lats[match(ll,index)]*2)/2
  alon <- round(lons[match(ll,index)]*2)/2
  coefs <- exp(pr[,llpos])
  coefs2 <- tapply(coefs, list(alon, alat), mean, na.rm = TRUE)
  coefs2 <- coefs2/mean(coefs2, na.rm = TRUE)
  image(sort(as.numeric(unique(alon))), sort(unique(alat)), coefs2, zlim=c(0, 4), ylab="Lat", xlab="Long")
  if(length(coefs2) > 5) contour(sort(unique(alon)), sort(unique(alat)), coefs2, levels= c(0,1,2,2.5), add=TRUE,col=4)
  map(add = TRUE, fill = TRUE)


    if(dovess) {
    coefs <- glm.coefs2(model, indat, parm = "vessid")
    if(fam=="gaussian") lims <- c(0, 3) else lims <- c(0, 1)
    with(coefs, plot(as.numeric(vessid), fit, ylim = lims, xlab="Vessel ID", ylab="Effect size", main=""))
    with(coefs, segments(as.numeric(vessid), lo,  as.numeric(vessid), hi, lty=1, col="slate grey"))
    with(coefs, points(as.numeric(vessid), fit, pch=16))

    vts <- tapply(pr[,vesspos],list(indat$yrqtr,indat$vessid),mean)
    y<-trans(as.vector(vts))
    x<-rep(as.numeric(dimnames(vts)[[1]]),dim(vts)[[2]])
    plot(x,y,type="p",ylim=lims,xlab="Vessel ID",ylab="Effect size",main="Vessel effects",pch=16,cex=0.9)
    mn <- tapply(trans(pr[,vesspos]),indat$yrqtr,mean)
    lines(as.numeric(dimnames(vts)[[1]]), mn, col=2,lwd=2)
    #  plot(as.factor(indat$vessid),pr[,vesspos],ylim=c(-.75,.75),xlab="Vessel",ylab="Effect size")
  }

  # if(dohbf) {
  #   rsp <- pr[,hbfpos[1]]
  #   rsp.se <- prse[,hbfpos[1]]
  #   b <- match(sort(unique(indat$hbf)),indat$hbf)
  #   plot(indat$hbf[b],trans(pr[,hbfpos][b]),ylim=lims,xlab="HBF",ylab="Effect size",main="HBF effects")
  #   lines(indat$hbf[b],trans(rsp[b]+2*rsp.se[b]),lty=2)
  #   lines(indat$hbf[b],trans(rsp[b]-2*rsp.se[b]),lty=2)
  # }

  if(dohbf) {
    coefs <- glm.coefs2(model, indat, parm = "hbf")
    if(fam=="gaussian") lims <- c(0, 3) else lims <- c(0, 1)
    with(coefs, plot(hbf, fit, ylim = lims, type="l", xlab="HBF", ylab="Effect size", main="HBF effects"))
    with(coefs, lines(hbf,hi,lty=2))
    with(coefs, lines(hbf,lo,lty=2))
  }

  if(docl) {
    coefs <- glm.coefs2(model, indat, parm = "clust")
    if(fam=="gaussian") lims <- c(0, 3) else lims <- c(0, 1)
    with(coefs, plot(clust, fit, ylim = lims, xlab="Cluster", ylab="Effect size", main="Cluster effects"))
    with(coefs, segments(as.numeric(clust), lo,  as.numeric(clust), hi, lty=1, col="slate grey"))
  }

  if(addpca) {
    ind <- indat[order(indat$pca),]
    out <- trans(pr[b,1])
    se1 <- trans(pr[b,1] - 1.96 * prse[b,1])
    se2 <- trans(pr[b,1] + 1.96 * prse[b,1])
    mout <- mean(out)
    out <- out/mout;se1 <- se1/mout;se2 <- se2/mout;
    plot(as.numeric(as.character(index)),out,ylim=lims,xlab="Cluster category",ylab="Effect size",main="Cluster effects")
    segments(as.numeric(as.character(index)), se1,  as.numeric(as.character(index)), se2, lty=1, col="slate grey")
    points(as.numeric(as.character(index)), out, pch=16)
  }
  title(main=ti,cex.main=1.5,outer=T)
}


plot_effects_IO_old <- function(model,indat,dovess=T,addcl=F,addpca=F,ti="",dohbf=T) {
  cf <- model$coefficients
  pred <- predict(model,data=indat,type="terms",se.fit=T)
  fishlab <- switch(runsp,yft="Yellowfin",bet="Bigeye"); methlab <- switch(mt,deltabin="Delta-binomial",deltapos="Delta-positive",logl="Lognormal(+0.01)",propn="Proportion Bigeye")
  if(addcl != F) nfigs <- 6 + addpca
  if(addcl == F) nfigs <- 5 + addpca
  mf <- switch(nfigs-4,c(2,3),c(2,3),c(3,3),c(3,3),c(3,3),c(3,4))
  hw <- c(14,19)
  windows(height=hw[1],width=hw[2])
  par(mfrow=mf,mar=c(5,4,2,1),oma=c(0,0,3,0))
  pr <- pred$fit ; prse <- pred$se.fit
  termlist <- dimnames(pred$fit)[[2]]
  llpos <- grep("latlong",termlist)
  hbfpos  <- grep("hbf",termlist)
  vesspos <- grep("vessid",termlist)
  branchpos <- grep("branchline",termlist)
  if(length(grep("hooks",termlist))>0) db <- T else db <- F

 # browser()
  index <- sort(unique(indat$yrqtr))
  b <- match(index,indat$yrqtr)
  out <- exp(pr[b,1])
  se1 <- exp(pr[b,1] - 1.96 * prse[b,1])
  se2 <- exp(pr[b,1] + 1.96 * prse[b,1])
  mout <- median(out)
  out <- out/mout;se1 <- se1/mout;se2 <- se2/mout;
  plot(as.numeric(as.character(index)),out,ylim=c(0,4),xlab="Year",ylab="Effect size",main="Year effects")
  segments(as.numeric(as.character(index)), se1,  as.numeric(as.character(index)), se2, lty=1, col="slate grey")
  points(as.numeric(as.character(index)), out, pch=16)

  index <- sort(unique(indat$latlong))
  b <- match(index,indat$latlong)
  out <- exp(pr[b,llpos])
  se1 <- exp(pr[b,llpos] - 1.96 * prse[b,llpos])
  se2 <- exp(pr[b,llpos] + 1.96 * prse[b,llpos])
  mout <- median(out)
  out <- out/mout;se1 <- se1/mout;se2 <- se2/mout;
  plot(as.numeric(index),out,ylim=c(0,4),xlab="Latlong",ylab="Effect size",main="Spatial effects")
  segments(as.numeric(index), se1,  as.numeric(index), se2, lty=1, col="slate grey")
  points(as.numeric(index), out, pch=16)
  #  plot(indat$latlong,pr[,2],ylim=c(-.75,.75),xlab="Latitude",ylab="Effect size")

  ##plot coefficients
  library(maps)
  ll <- as.character(indat$latlong)
 # browser()
  index <- sort(unique(ll))
  index2 <- unlist(strsplit(index,"\\_"))
  pos1 <- 2*(1:length(index)) -1
  pos2 <- pos1 + 1
  lats <- as.numeric(index2[pos1])
  lons <- as.numeric(index2[pos2])
  alat <- round(lats[match(ll,index)]*2)/2
  alon <- round(lons[match(ll,index)]*2)/2
  coefs <- exp(pr[,llpos])
  coefs2 <- tapply(coefs, list(alon, alat), mean, na.rm = TRUE)
  coefs2 <- coefs2/mean(coefs2, na.rm = TRUE)
  image(sort(as.numeric(unique(alon))), sort(unique(alat)), coefs2, zlim=c(0, 4), ylab="Lat", xlab="Long")
  if(length(coefs2) > 5) contour(sort(unique(alon)), sort(unique(alat)), coefs2, levels= c(0,1,2,2.5), add=TRUE,col=4)
  map(add = TRUE, fill = TRUE)

  if(dovess) {
    index <- sort(unique(indat$vessid))
    b <- match(index,indat$vessid)
    out <- exp(pr[b,vesspos])
    se1 <- exp(pr[b,vesspos] - 1.96 * prse[b,vesspos])
    se2 <- exp(pr[b,vesspos] + 1.96 * prse[b,vesspos])
    mout <- median(out)
    out <- out/mout;se1 <- se1/mout;se2 <- se2/mout;
    ind <- as.factor(index)
    plot(match(ind,ind),out,type="n",ylim=c(0,4),xlab="Vessel ID",ylab="Effect size",main="")
    segments(match(ind,ind), se1,  match(ind,ind), se2, lty=1, col="slate grey")
    points(match(ind,ind), out, pch=16)

    vts <- tapply(pr[,vesspos],list(indat$yrqtr,indat$vessid),mean)
    y<-exp(as.vector(vts))
    x<-rep(as.numeric(dimnames(vts)[[1]]),dim(vts)[[2]])
    plot(x,y,type="p",ylim=c(0,3),xlab="Vessel ID",ylab="Effect size",main="Vessel effects",pch=16,cex=0.9)
    mn <- tapply(exp(pr[,vesspos]),indat$yrqtr,mean)
    lines(as.numeric(dimnames(vts)[[1]]), mn, col=2,lwd=2)
    #  plot(as.factor(indat$vessid),pr[,vesspos],ylim=c(-.75,.75),xlab="Vessel",ylab="Effect size")
  }

  if(dohbf) {
    rsp <- pr[,hbfpos[1]]
    rsp.se <- prse[,hbfpos[1]]
    b <- match(sort(unique(indat$hbf)),indat$hbf)
    plot(indat$hbf[b],exp(pr[,hbfpos][b]),ylim=c(0,3),xlab="HBF",ylab="Effect size",main="HBF effects")
    lines(indat$hbf[b],exp(rsp[b]+2*rsp.se[b]),lty=2)
    lines(indat$hbf[b],exp(rsp[b]-2*rsp.se[b]),lty=2)
  }

  if(addcl != F) {
    index <- sort(unique(indat$cl))
    b <- match(index,indat$cl)
    out <- exp(pr[b,1])
    se1 <- exp(pr[b,1] - 1.96 * prse[b,1])
    se2 <- exp(pr[b,1] + 1.96 * prse[b,1])
    mout <- mean(out)
    out <- out/mout;se1 <- se1/mout;se2 <- se2/mout;
    plot(index,out,ylim=c(0,3),xlab="Cluster category",ylab="Effect size",main="Cluster effects")
    segments(as.numeric(index), se1, as.numeric(index), se2, lty=1, col="slate grey")
    points(index, out, pch=16)
  }

  if(addpca) {
    ind <- indat[order(indat$pca),]
    out <- exp(pr[b,1])
    se1 <- exp(pr[b,1] - 1.96 * prse[b,1])
    se2 <- exp(pr[b,1] + 1.96 * prse[b,1])
    mout <- mean(out)
    out <- out/mout;se1 <- se1/mout;se2 <- se2/mout;
    plot(as.numeric(as.character(index)),out,ylim=c(0,3),xlab="Cluster category",ylab="Effect size",main="Cluster effects")
    segments(as.numeric(as.character(index)), se1,  as.numeric(as.character(index)), se2, lty=1, col="slate grey")
    points(as.numeric(as.character(index)), out, pch=16)
  }
  title(main=ti,cex.main=1.5,outer=T)
}

plot.pacific <- function(plot_title="",lims=c(100,260,-45,45)) {
  plot(1,1, yaxt="n", xaxt="n", type="n", xlim=c(lims[1]+10,lims[2]-10), ylim=c(lims[3]+5,lims[4]-5), ylab="", xlab="", bg="lightblue")
  polygon(c(lims[1],lims[2],lims[2],lims[1]), c(lims[3],lims[3],lims[4],lims[4]), col="lightblue")
#  polygon(eez[,1], eez[,2], lwd=1, col="white")
#  lines(eez[,1], eez[,2], lwd=1, col="slate grey")
  map('world2Hires',  yaxt="n", xaxt="n", add=T, resolution=1)
  map('world2Hires',  region = c("USA","Hawaii","Mexico","Japan","China","South Korea","North Korea","Philippines","Vietnam","Laos","Taiwan","Fiji", "Vanuatu", "Malaysia", "Australia", "New Zealand", "Indonesia", "New Caledonia", "Papua New Guinea", "Solomon Islands"), fill=T, add=T, yaxt="n", xaxt="n", col="black", density=50)
  box(lwd=3)
  lines(c(210, 210, 230, 230), c(45, -2.5, -2.5, -45), lwd=2, lty=2)
  lines(c(170, 170), c(-35, 40), lwd=2, lty=1)
  lines(c(120, 210), c(20, 20), lwd=2, lty=1)
  lines(c(120, 230), c(-10, -10), lwd=2, lty=1)
  lines(c(120, 230), c(-35, -35), lwd=2, lty=1)
  axis(1, at=seq(lims[1],lims[2],by=10), labels=F)
  axis(2, at=seq(lims[3],lims[4],by=5), labels=F)
  latseq <- seq(lims[3]+10,lims[4]-10,by=10) ;latseq2 <- as.character(latseq)
  lonseq <- seq(lims[1]+20,lims[2]-20,by=20) ;lonseq2 <- as.character(lonseq)
  latseq2[latseq < 0] <- paste(abs(latseq[latseq < 0]),"S",sep="")
  latseq2[latseq > 0] <- paste(latseq[latseq > 0],"N",sep="")
  lonseq2[lonseq < 180] <- paste(lonseq2[lonseq < 180],"E",sep="")
  lonseq2[lonseq > 180] <- paste(360-lonseq[lonseq > 180],"W",sep="")
  axis(2, at=latseq, labels=latseq2, cex.axis=0.75)
  axis(1, at=lonseq, labels=lonseq2, cex.axis=0.75)
  mtext(side=3, line=0.5, plot_title)
}

plot.pacific <- function(plot_title="",lims=c(100,260,-45,45)) {
  plot(1,1, yaxt="n", xaxt="n", type="n", xlim=c(lims[1]+10,lims[2]-10), ylim=c(lims[3]+5,lims[4]-5), ylab="", xlab="", bg="lightblue")
  polygon(c(lims[1],lims[2],lims[2],lims[1]), c(lims[3],lims[3],lims[4],lims[4]), col="lightblue")
#  polygon(eez[,1], eez[,2], lwd=1, col="white")
#  lines(eez[,1], eez[,2], lwd=1, col="slate grey")
  map('world2Hires',  yaxt="n", xaxt="n", add=T, resolution=1)
  map('world2Hires',  region = c("USA","Hawaii","Mexico","Japan","China","South Korea","North Korea","Philippines","Vietnam","Laos","Taiwan","Fiji", "Vanuatu", "Malaysia", "Australia", "New Zealand", "Indonesia", "New Caledonia", "Papua New Guinea", "Solomon Islands"), fill=T, add=T, yaxt="n", xaxt="n", col="black", density=50)
  box(lwd=3)
  lines(c(210, 210, 230, 230), c(45, -2.5, -2.5, -45), lwd=2, lty=2)
  lines(c(170, 170), c(-35, 40), lwd=2, lty=1)
  lines(c(120, 210), c(20, 20), lwd=2, lty=1)
  lines(c(120, 230), c(-10, -10), lwd=2, lty=1)
  lines(c(120, 230), c(-35, -35), lwd=2, lty=1)
  axis(1, at=seq(lims[1],lims[2],by=10), labels=F)
  axis(2, at=seq(lims[3],lims[4],by=5), labels=F)
  latseq <- seq(lims[3]+10,lims[4]-10,by=10) ;latseq2 <- as.character(latseq)
  lonseq <- seq(lims[1]+20,lims[2]-20,by=20) ;lonseq2 <- as.character(lonseq)
  latseq2[latseq < 0] <- paste(abs(latseq[latseq < 0]),"S",sep="")
  latseq2[latseq > 0] <- paste(latseq[latseq > 0],"N",sep="")
  lonseq2[lonseq < 180] <- paste(lonseq2[lonseq < 180],"E",sep="")
  lonseq2[lonseq > 180] <- paste(360-lonseq[lonseq > 180],"W",sep="")
  axis(2, at=latseq, labels=latseq2, cex.axis=0.75)
  axis(1, at=lonseq, labels=lonseq2, cex.axis=0.75)
  mtext(side=3, line=0.5, plot_title)
}

plot_IO <- function (plot_title = "", uselims = c(20, 130, -50, 25), sp = "YFT", newm=T, lwdm=3, axes = T, tcol="red")
{
    lims <- uselims
    if(newm){
      plot(1, 1, yaxt = "n", xaxt = "n", type = "n", xlim = c(lims[1], lims[2]), ylim = c(lims[3], lims[4]),
        ylab = "", xlab = "", bg = "lightblue")
      polygon(c(lims[1]-5, lims[2]+5, lims[2]+5, lims[1]-5), c(lims[3]-5, lims[3]-5, lims[4]+5, lims[4]+5), col = "lightblue")
    }
    if (sp == "ALB") {
        lines(c(34.5, 44.2), c(-20, -20), lwd = lwdm, col = "slate grey",lty = 1)
        lines(c(49, 120), c(-20, -20), lwd = lwdm, col = "slate grey",lty = 1)
        xoffset <- 5
        yoffset <- 2.5
        text(115 + xoffset, -11 - yoffset, "N", col = tcol,  cex = 1.5)
        text(115 + xoffset, -40 - yoffset, "S", col = tcol,  cex = 1.5)
    }
    if (sp %in% c("YFT")) {
      lines(c(20, 120), c(-40, -40), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(20, 20), c(-40, -35), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(40, 40), c(-40, -30), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(40, 40), c(-10, 10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(60, 60), c(-30, -10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(75, 75), c(-15, 10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(100, 100), c(-5, 10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(110, 110), c(-10, -5), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(120, 120), c(-40, -15), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(130, 130), c(-15, -10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(40, 60), c(-30, -30), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(60, 130), c(-15, -15), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(40, 60), c(-10, -10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(110, 130), c(-10, -10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(100, 110), c(-5, -5), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(40, 100), c(10, 10), lwd = lwdm, col = "slate grey",lty = 1)
      text(67.5, 15, "R1", col = tcol,  cex = 1.5)
      text(57.5, -2.5, "R2", col = tcol,  cex = 1.5)
      text(52.5, -27.5, "R3", col = tcol,  cex = 1.5)
      text(82.5, -27.5, "R4", col = tcol,  cex = 1.5)
      text(85, -2.5, "R5", col = tcol,  cex = 1.5)
      text(90, 15, "R6", col = tcol,  cex = 1.5)
    }
    if (sp %in% c("YFT2")) {
      lines(c(20, 120), c(-40, -40), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(20, 20), c(-40, -35), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(40, 40), c(-40, -30), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(40, 40), c(-10, 10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(60, 60), c(-30, -10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(75, 75), c(-15, 10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(100, 100), c(-5, 10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(110, 110), c(-10, -5), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(120, 120), c(-40, -15), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(130, 130), c(-15, -10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(40, 60), c(-30, -30), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(60, 130), c(-15, -15), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(40, 60), c(-10, -10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(110, 130), c(-10, -10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(100, 110), c(-5, -5), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(40, 75), c(0, 0), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(40, 100), c(10, 10), lwd = lwdm, col = "slate grey",lty = 1)
      text(62.5, 17.5, "R1", col = tcol,  cex = 1.5)
      text(57.5, 2.5, "R2N", col = tcol,  cex = 1.5)
      text(57.5, -7.5, "R2S", col = tcol,  cex = 1.5)
      text(52.5, -27.5, "R3", col = tcol,  cex = 1.5)
      text(82.5, -27.5, "R4", col = tcol,  cex = 1.5)
      text(85, -2.5, "R5", col = tcol,  cex = 1.5)
      text(90, 17.5, "R6", col = tcol,  cex = 1.5)
    }
    if (sp %in% c("BET")) {
      lines(c(20, 120), c(-35, -35), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(80, 80), c(-15, 10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(100, 100), c(-5, 10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(110, 110), c(-10, -5), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(120, 120), c(-35, -15), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(130, 130), c(-15, -10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(47, 130), c(-15, -15), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(35, 45), c(-20, -20), lwd = lwdm, col = "slate grey",lty = 1)
      #      lines(c(45, 45), c(-20, -15), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(110, 130), c(-10, -10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(100, 110), c(-5, -5), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(40, 100), c(10, 10), lwd = lwdm, col = "slate grey",lty = 1)
      text(65, 0, "R1", col = tcol,  cex = 1.5)
      text(90, 0, "R2", col = tcol,  cex = 1.5)
      text(70, -30, "R3", col = tcol,  cex = 1.5)
    }
    if (sp %in% c("BET3")) {
      lines(c(20, 120), c(-35, -35), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(80, 80), c(-15, 10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(100, 100), c(-5, 10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(110, 110), c(-10, -5), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(120, 120), c(-35, -15), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(130, 130), c(-15, -10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(47, 130), c(-15, -15), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(35, 45), c(-20, -20), lwd = lwdm, col = "slate grey",lty = 1)
      #      lines(c(45, 45), c(-20, -15), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(110, 130), c(-10, -10), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(100, 110), c(-5, -5), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(40, 80), c(0, 0), lwd = lwdm, col = "slate grey",lty = 1)
      lines(c(40, 100), c(10, 10), lwd = lwdm, col = "slate grey",lty = 1)
      text(57.5, 2.5, "R1N", col = tcol,  cex = 1.5)
      text(67.5, -2.5, "R1S", col = tcol,  cex = 1.5)
      text(87.5, -2.5, "R2", col = tcol,  cex = 1.5)
      text(72.5, -27.5, "R3", col = tcol,  cex = 1.5)
    }
    if (sp %in% c("BETcore","YFTcore")) {
        lines(c(20, 140), c(-35, -35), lwd = lwdm, col = "slate grey",lty = 1)
        lines(c(20, 125.5), c(-15, -15), lwd = lwdm, col = "slate grey",lty = 1)
        lines(c(20, 100), c(10, 10), lwd = lwdm, col = "slate grey",lty = 1)
        lines(c(80, 80), c(10, -15), lwd = lwdm, col = "slate grey",lty = 1)
        xoffset <- 5
        yoffset <- 2.5
        text(115 + xoffset, -11 - yoffset, "N", col = tcol,  cex = 1.5)
        text(115 + xoffset, -40 - yoffset, "S", col = tcol,  cex = 1.5)
    }
    map("world", yaxt = "n", xaxt = "n", add = T, resolution = 1, interior=F,fill=T)
    if(axes) {
      box(lwd = 3)
      axis(1, at = seq(lims[1], lims[2], by = 10), labels = F)
      axis(2, at = seq(lims[3], lims[4], by = 5), labels = F)
      latseq <- seq(lims[3] + 10, lims[4] - 10, by = 10)
      latseq2 <- as.character(latseq)
      lonseq <- seq(lims[1] + 20, lims[2] - 10, by = 20)
      lonseq2 <- as.character(lonseq)
      latseq2[latseq < 0] <- paste(abs(latseq[latseq < 0]), "S",
          sep = "")
      latseq2[latseq > 0] <- paste(latseq[latseq > 0], "N", sep = "")
      lonseq2[lonseq < 180] <- paste(lonseq2[lonseq < 180], "E",
          sep = "")
      lonseq2[lonseq > 180] <- paste(360 - lonseq[lonseq > 180],
          "W", sep = "")
      axis(2, at = latseq, labels = latseq2, cex.axis = 0.75)
      axis(1, at = lonseq, labels = lonseq2, cex.axis = 0.75)
    }
    mtext(side = 3, line = 0.5, plot_title, font = 2, cex = 1.1)
}

storesumm <- function(mod,fname) {
  summry <- summary(mod)
  save(summry,file=paste(fname,".RData"))
  a <- capture.output(summry);
  cat(a,file=paste(fname,".txt"),sep="\n",append=F)
  }

combine_delta_xl <- function(fnamedelta,fnamepos) {
    xl_delta <- read.csv(paste(fnamedelta,".csv",sep=""))
    xl_pos <- read.csv(paste(fnamepos,".csv",sep=""))
    pos <- match(xl_pos[,2],xl_delta[,2])
    coefs.boat <- xl_delta[pos,5] * xl_pos[,5]
    coefs.base <- xl_delta[pos,3] * xl_pos[,3]
    yrpos <- xl_delta[pos,2]
    fishlab <- switch(runsp,yft="Yellowfin",bet="Bigeye");
    plot.slope.ratio(coefs.base,coefs.boat,yrpos,titl=paste("Region",runreg,fishlab,"Delta lognormal combined"))
#    par(mar=c(5,4,1,1))
#    plot(yrpos,coefs.base,type="l",ylab="Relative abundance estimate",xlab="Year",ylim=c(0,2.5))
#    lines(yrpos,coefs.boat,col="red")
    fname2 <- gsub("deltabin","deltacomb",fnamedelta)
    savePlot(paste(fname2,".png",sep=""),type="png")
    write.csv(cbind(yrpos,coefs.base,coefs.boat),file=paste(fname2,".csv",sep=""))
    graphics.off()
    }

combine_delta_xl_indices <- function(fnamedelta,fnamepos) {
    xl_delta <- read.csv(paste(fnamedelta,".csv",sep=""))
    xl_pos <- read.csv(paste(fnamedelta,".csv",sep=""))
    coefs.boat <- xl_delta[,3] * xl_pos[,3]
    fishlab <- switch(runsp,yft="Yellowfin",bet="Bigeye");
    yrpos <- xl_delta[,2]
    windows();par(mar=c(5,4,1,1))
    plot(yrpos,coefs.boat,type="l",ylab="Relative abundance estimate",xlab="Year",ylim=c(0,2.5))
    fname2 <- gsub("deltabin","deltacomb",fnamedelta)
    savePlot(paste(fname2,".png",sep=""),type="png")
    write.csv(cbind(yrpos,coefs.boat),file=paste(fname2,".csv",sep=""))
    graphics.off()
    }

combine_delta <- function(fnamedelta,fnamepos) {
    load(paste("model.",fnamedelta,".base.RData",sep=""))
    load(paste("model.",fnamedelta,".boat.RData",sep=""))
    yrbin <- as.numeric(model.base$xlevels[[1]])
    a <- length(yrbin)
    coefsdeltabin.base <- get.coefs(model.base,a)
    coefsdeltabin.boat <- get.coefs(model.boat,a)
    rm(model.base,model.boat); gc()
    load(paste("model.",fnamepos,".base.RData",sep=""))
    load(paste("model.",fnamepos,".boat.RData",sep=""))
    yrpos <- as.numeric(model.base$xlevels[[1]])
    a <- length(yrpos)
    coefsdeltapos.base <- get.coefs(model.base,a)
    coefsdeltapos.boat <- get.coefs(model.boat,a)
    rm(model.base,model.boat); gc()
    a <- match(names(coefsdeltapos.base),names(coefsdeltabin.base))
    coefs.base <- coefsdeltabin.base[a] * coefsdeltapos.base
    coefs.boat <- coefsdeltabin.boat[a] * coefsdeltapos.boat
    fishlab <- switch(runsp,yft="Yellowfin",bet="Bigeye");
    plot.slope.ratio(coefs.base,coefs.boat,yrpos,titl=paste("Region",runreg,fishlab,"Delta lognormal combined"))
    par(mar=c(5,4,1,1))
    plot(yrpos,coefs.base,type="l",ylab="Relative abundance estimate",xlab="Year",ylim=c(0,2.5))
    lines(yrpos,coefs.boat,col="red")
    fname2 <- paste(fname," deltacomb",sep="")
    savePlot(paste(fname2,".png",sep=""),type="png")
    write.csv(cbind(yr,coefs.base,coefs.boat),file=paste(fname,".csv",sep=""))
    graphics.off()
    }

plot_catchmap <- function(indat=a,vbl,dcd,latlim=c(-40,20),lonlim=c(20,130),brk=seq(0,1,.05),brk2=seq(0,1,.1),ti="") {
  plot(1:5,1:5,ylim=latlim,xlim=lonlim,type="n",xlab="Longitude",ylab="Latitude")
  indat <- cbind(indat,vbl)
  indat <- indat[indat$lon <= 140,]
  a1 <- with(indat[indat$decade==dcd,],tapply(vbl,list(lon,lat),sum))
  image(as.numeric(rownames(a1)),as.numeric(colnames(a1)),a1,add=T,col=heat.colors(length(brk)-1),breaks=brk)
  contour(as.numeric(rownames(a1)),as.numeric(colnames(a1)),a1,add=T,levels=brk2)
  map(database="world",add=T, interior=F,fill=T)
  title(paste(dcd,ti))
  }

plot_catchmap2 <- function(indat=a,vbl,dcd,latlim=c(-40,20),lonlim=c(20,130),ti="") {
  plot(1:5,1:5,ylim=latlim,xlim=lonlim,type="n",xlab="Longitude",ylab="Latitude")
  indat <- cbind(indat,vbl)
  indat <- indat[indat$lon <= 140,]
  indat$lo <- factor(indat$lon,levels=seq(min(indat$lo,na.rm=T),max(indat$lo,na.rm=T),1))
  indat$la <- factor(indat$lat,levels=seq(min(indat$la,na.rm=T),max(indat$la,na.rm=T),1))
  a1 <- with(indat[indat$decade==dcd,],tapply(vbl,list(lo,la),sum))
  image(as.numeric(rownames(a1)),as.numeric(colnames(a1)),a1,add=T)
  contour(as.numeric(rownames(a1)),as.numeric(colnames(a1)),a1,add=T)
  map(database="world",add=T, interior=F,fill=T)
  title(paste(dcd,ti))
  }

plot_cpuemap <- function(indat=a,vb1,vb2,dcd,latlim=c(-40,40),lonlim=c(120,210),brk=seq(0,1,.05),brk2=seq(0,1,.1),ti="") {
  plot(1:5,1:5,ylim=latlim,xlim=lonlim,type="n",xlab="Longitude",ylab="Latitude")
  indat <- cbind(indat,vb1,vb2)
  indat <- indat[indat$lon <= 210,]
  a1 <- with(indat[indat$decade==dcd,],tapply(vb1,list(lon,lat),sum)/tapply(vb2,list(lon,lat),sum))
  image(as.numeric(rownames(a1)),as.numeric(colnames(a1)),a1,add=T,col=heat.colors(length(brk)-1),breaks=brk)
  contour(as.numeric(rownames(a1)),as.numeric(colnames(a1)),a1,add=T,levels=brk2)
  map(database="world",add=T, interior=F,fill=T)
  title(paste(dcd,ti))
  }

plot_cpuemap2 <- function(indat=a,vb1,vb2,dcd,latlim=c(-40,40),lonlim=c(120,210),ti="") {
  plot(1:5,1:5,ylim=latlim,xlim=lonlim,type="n",xlab="Longitude",ylab="Latitude")
  indat <- cbind(indat,vb1,vb2)
  indat <- indat[indat$lon <= 210,]
  indat$lo <- factor(indat$lon,levels=seq(min(indat$lo,na.rm=T),max(indat$lo,na.rm=T),1))
  indat$la <- factor(indat$lat,levels=seq(min(indat$la,na.rm=T),max(indat$la,na.rm=T),1))
  a1 <- with(indat[indat$decade==dcd,],tapply(vb1,list(lo,la),sum)/tapply(vb2,list(lo,la),sum))
  image(as.numeric(rownames(a1)),as.numeric(colnames(a1)),a1,add=T)
  contour(as.numeric(rownames(a1)),as.numeric(colnames(a1)),a1,add=T)
  map(database="world",add=T, interior=F,fill=T)
  title(paste(dcd,ti))
  }

boxplots_spCL <- function(dat=dataset,cl="kmeans",ti="",outL=T,nsp=13,regtype=regtype,r=r) {
  if(nsp==8) windows(30,20);par(mfrow=c(2,4),mar=c(3,3,3,3))
  if(nsp==12) windows(30,20);par(mfrow=c(3,4),mar=c(3,3,3,3))
  if(nsp==13) windows(30,20);par(mfrow=c(3,5),mar=c(3,3,3,3))
  wd=table(dat[,cl])
  for(sp in allsp) {
    boxplot(dat[,sp]/dat$hooks ~ dat[,cl],width=wd,main=sp,outline=outL,pars = list(boxwex = 1))
    }
  title(paste(ti,cl),outer=T,line=-1,cex.main=1.5)
  savePlot(paste0(ti,"_boxplots_spCL",cl,".png"),type="png")
  }

boxplots_spCL_comp <- function(dat=dataset,cl="kmeans",ti="",outL=T,nsp=13,regtype=regtype,r=r) {
  if(nsp==8) { windows(30,20);par(mfrow=c(2,4),mar=c(3,3,3,3)) }
  if(nsp==12){ windows(30,20);par(mfrow=c(3,4),mar=c(3,3,3,3)) }
  if(nsp==13){ windows(30,20);par(mfrow=c(3,5),mar=c(3,3,3,3)) }
  wd=table(dat[,cl])
  for(sp in allsp) {
    boxplot(dat[,sp]/dat$Total ~ dat[,cl],width=wd,main=sp,outline=outL,pars = list(boxwex = 1),ylim=c(0,1))
    }
  title(paste(gsub("_"," ",ti),cl),outer=T,line=-1,cex.main=1.5)
  savePlot(paste0(ti,"_boxplots_spCLcomp",cl,".png"),type="png")
  }

beanplots_spCL_comp <- function(dat=dataset,cl="kmeans",ti="",outL=T,nsp=13,regtype=regtype,r=r) {
  if(nsp==8) { windows(30,20);par(mfrow=c(2,4),mar=c(3,3,3,3)) }
  if(nsp==12){ windows(30,20);par(mfrow=c(3,4),mar=c(3,3,3,3)) }
  if(nsp==13){ windows(30,20);par(mfrow=c(3,5),mar=c(3,3,3,3)) }
  wd=table(dat[,cl])
  for(sp in allsp) {
    beanplot(dat[,sp]/dat$Total ~ dat[,cl],bw=.01,what=c(1,1,1,0),ylim=c(0,1),log="",main=sp)
    #boxplot(dat[,sp]/dat$Total ~ dat[,cl],width=wd,main=sp,outline=outL,pars = list(boxwex = 1),ylim=c(0,1))
    }
  title(paste(gsub("_"," ",ti),cl),outer=T,line=-1,cex.main=1.5)
  nm <- paste0(ti,"_beanplots_spCLcomp",cl)
  savePlot(paste0(nm,".png"),type="png")
  }

boxplots_CL <- function(dat=dataset,cl="kmeans",ti="",outL=T,dohbf=T,lat5=F,regtype=regtype,r=r) {
  windows(30,20);par(mfrow=c(2,3),mar=c(3,3,3,3),oma=c(0,0,2,0))
  wd=table(dat[,cl])
  boxplot(dat$op_yr ~ dat[,cl],width=wd,main="Year",outline=outL,pars = list(boxwex = 1))
    if(lat5) {
    boxplot(dat$lat5 ~ dat[,cl],width=wd,main="Lat",outline=outL,pars = list(boxwex = 1))
    boxplot(dat$lon5 ~ dat[,cl],width=wd,main="Lon",outline=outL,pars = list(boxwex = 1))
    } else {
    boxplot(dat$lat ~ dat[,cl],width=wd,main="Lat",outline=outL,pars = list(boxwex = 1))
    boxplot(dat$lon ~ dat[,cl],width=wd,main="Lon",outline=outL,pars = list(boxwex = 1))
    }
  boxplot(dat$hooks ~ dat[,cl],width=wd,main="Hooks",outline=outL,pars = list(boxwex = 1))
  if(dohbf) boxplot(dat$hbf ~ dat[,cl],width=wd,main="HBF",outline=outL,pars = list(boxwex = 1))
  boxplot(dat$op_mon ~ dat[,cl],width=wd,main="Months",outline=outL,pars = list(boxwex = 1))
  title(paste(gsub("_"," ",ti),cl),outer=T,line=1,cex.main=1.5)
  savePlot(paste0(ti,"_boxplots_CL",cl,".png"),type="png")
  }

boxplots_CL_bean <- function(dat=dataset,cl="kmeans",ti="",outL=T,dohbf=T,lat5=F,regtype=regtype,r=r) {
  windows(30,20);par(mfrow=c(2,3),mar=c(3,3,3,3),oma=c(0,0,2,0))
  wd=table(dat[,cl])
  beanplot(dat$op_yr ~ dat[,cl],bw=.5,what=c(1,1,1,0),ylim=c(min(dat$op_yr,na.rm=T)-.5,max(dat$op_yr,na.rm=T)+.5),log="",main="Year")
    if(lat5) {
    beanplot(dat$lat5 ~ dat[,cl],bw=1.25,what=c(1,1,1,0),ylim=c(min(dat$lat5,na.rm=T)-2.5,max(dat$lat5,na.rm=T)+2.5),log="",main="Lat")
    beanplot(dat$lon5 ~ dat[,cl],bw=1.25,what=c(1,1,1,0),ylim=c(min(dat$lon5,na.rm=T)-2.5,max(dat$lon5,na.rm=T)+2.5),log="",main="Lon")
    } else {
    beanplot(dat$lat ~ dat[,cl],bw=.5,what=c(1,1,1,0),ylim=c(min(dat$lat,na.rm=T)-.5,max(dat$lat,na.rm=T)+.5),log="",main="Lat")
    beanplot(dat$lon ~ dat[,cl],bw=.5,what=c(1,1,1,0),ylim=c(min(dat$lon,na.rm=T)-.5,max(dat$lon,na.rm=T)+.5),log="",main="Lon")
    }
  beanplot(dat$hooks ~ dat[,cl],bw=50,what=c(1,1,1,0),ylim=c(min(dat$hooks,na.rm=T)-.5,max(dat$hooks,na.rm=T)+.5),log="",main="Hooks")
  if(dohbf) beanplot(dat$hbf ~ dat[,cl],bw=.5,what=c(1,1,1,0),ylim=c(min(dat$hbf,na.rm=T)-.5,max(dat$hbf,na.rm=T)+.5),log="",main="HBF")
  beanplot(dat$op_mon ~ dat[,cl],bw=.5,what=c(1,1,1,0),log="",main="Months",ylim=c(.5,12.5))
  title(paste(gsub("_"," ",ti),cl),outer=T,line=1,cex.main=1.5)
  savePlot(paste0(ti,"_boxplots_CL",cl,"_bean.png"),type="png")
  }

boxplots_PCA <- function(dat,nPCA=3,ti="",dohbf=T,lat5=F,regtype="regY",r=r) {
  for (ppc in paste0("PC",1:nPCA)) {
    pp <- with(dat,get(ppc))
    windows(width=30,height=20);par(mfrow=c(2,3))
    boxplot(pp ~ yrqtr,data=dat,main="YQ")
    if(lat5) {
      boxplot(pp ~ (floor(lat5)),data=dat,main="LAT")
      boxplot(pp ~ (floor(lon5)),data=dat,main="LON")
      } else {
      boxplot(pp ~ (floor(lat)),data=dat,main="LAT")
      boxplot(pp ~ (floor(lon)),data=dat,main="LON")
      }
    if(dohbf) boxplot(pp ~ (floor(hbf)),data=dat,main="HBF")
    boxplot(pp ~ eval(100*floor(hooks/100)),data=dat,main="Hooks")
    boxplot(pp ~ vessid,data=dat,main="Vessel")
    title(paste(gsub("_"," ",ti),ppc),outer=T,line=-1,cex.main=1.5)
    savePlot(paste0(ti,"_boxplots_",ppc,".png"),type="png")
    }
  }

boxplots_TPCA <- function(dat,nPCA=6,ti="",dohbf=T,lat5=F,regtype="regY",r=r) {
  for (ppc in paste0("TPC",1:nPCA)) {
    pp <- with(dat,get(ppc))
    windows(20,14);par(mfrow=c(2,4),oma=c(0,0,2,0))
    boxplot(pp ~ yrqtr,data=dat,main="YQ")
    if(lat5) {
    boxplot(pp ~ (floor(lat5)),data=dat,main="LAT")
    boxplot(pp ~ (floor(lon5)),data=dat,main="LON")
    } else {
    boxplot(pp ~ (floor(lat)),data=dat,main="LAT")
    boxplot(pp ~ (floor(lon)),data=dat,main="LON")
    }
    if(dohbf) boxplot(pp ~ (floor(hbf)),data=dat,main="HBF")
    boxplot(pp ~ eval(100*floor(hooks/100)),data=dat,main="Hooks")
    boxplot(pp ~ vessid,data=dat,main="Vessel")
    title(paste(gsub("_"," ",ti),ppc),outer=T,line=1,cex.main=1.5)
    savePlot(paste0(ti,"_boxplots_",ppc,".png"),type="png")
    }
  }

boxplots_spPCA <- function(dat=dataset,nPCA=6,ti="",outL=T,nsp=13,regtype="regY",r=r) {
  for (ppc in paste0("PC",1:nPCA)) {
    pp <- with(dat,get(ppc))
    notdone<- T
    bk <- quantile(pp, seq(0,1,length.out=11),include.lowest = TRUE)
    if(length(bk)==length(unique(bk))) ppq <- cut(pp,breaks = bk) else ppq <- cut(pp,breaks = 11)
    if(nsp==8){ windows(30,20);par(mfrow=c(2,4),mar=c(3,3,3,3),oma=c(0,0,2,0))}
    if(nsp==12){ windows(30,20);par(mfrow=c(3,4),mar=c(3,3,3,3),oma=c(0,0,2,0))}
    if(nsp==13){ windows(30,20);par(mfrow=c(3,5),mar=c(3,3,3,3),oma=c(0,0,2,0))}
    for(sp in allsp) {
      boxplot(dat[,sp]/dat$Total ~ ppq,main=sp,outline=outL,ylim=c(0,1))
      }
    title(paste(gsub("_"," ",ti),ppc),outer=T,line=1,cex.main=1.5)
    savePlot(paste0(ti,"_boxplots_spPCA",ppc,".png"),type="png")
    }
  }

boxplots_spTPCA <- function(dat=dataset,nPCA=6,ti="",outL=T,nsp=13,regtype="regY",r=r) {
  for (ppc in paste0("TPC",1:nPCA)) {
    pp <- with(dat,get(ppc))
    ppq <- cut(pp,breaks = quantile(pp, seq(0,1,length.out=11),include.lowest = TRUE))
    if(nsp==8){ windows(30,20);par(mfrow=c(2,4),mar=c(3,3,3,3),oma=c(0,0,2,0))}
    if(nsp==12){ windows(30,20);par(mfrow=c(3,4),mar=c(3,3,3,3),oma=c(0,0,2,0))}
    if(nsp==13){ windows(30,20);par(mfrow=c(3,5),mar=c(3,3,3,3),oma=c(0,0,2,0))}
    for(sp in allsp) {
      boxplot(dat[,sp]/dat$Total ~ ppq,main=sp,outline=outL,ylim=c(0,1))
      }
    title(paste(gsub("_"," ",ti),ppc),outer=T,line=1,cex.main=1.5)
    savePlot(paste0(ti,"_boxplots_spTPCA",ppc,".png"),type="png")
    }
  }

mapPCA <- function(ddd,nPCA=6,ti="",lat5=F,regtype="regY",r=r) {
  windows(20,14);par(mfrow=c(2,2))
  for (ppc in paste0("PC",1:nPCA)) {
    pp <- with(ddd,get(ppc))
    if(lat5) {
      pcm <- with(ddd,tapply(pp,list(floor(lon5),floor(lat5)),mean))
      } else {
      pcm <- with(ddd,tapply(pp,list(floor(lon),floor(lat)),mean))
      }
    lonn <- as.numeric(dimnames(pcm)[[1]])+0.5
    latn <- as.numeric(dimnames(pcm)[[2]])+0.5
    image(lonn,latn,pcm)
    contour(lonn,latn,pcm,add=T)
    map(database="world2Hires",add=T)
    title(ppc,line=-2,cex.main=1.5)
    }
  title(paste(gsub("_"," ",ti)),outer=T,line=-1,cex.main=1.5)
  savePlot(paste0(ti,"_map_PCs",".png"),type="png")
  }
mapTPCA <- function(ddd,nPCA=6,ti="",lat5=F,regtype="regY",r=r) {
  windows(20,14);par(mfrow=c(2,2))
  for (ppc in paste0("TPC",1:nPCA)) {
    pp <- with(ddd,get(ppc))
    if(lat5) {
      pcm <- with(ddd,tapply(pp,list(floor(lon5),floor(lat5)),mean))
      } else {
      pcm <- with(ddd,tapply(pp,list(floor(lon),floor(lat)),mean))
      }
    lonn <- as.numeric(dimnames(pcm)[[1]])+0.5
    latn <- as.numeric(dimnames(pcm)[[2]])+0.5
    image(lonn,latn,pcm)
    contour(lonn,latn,pcm,add=T)
    map(database="world2Hires",add=T)
    title(ppc,line=-2,cex.main=1.5)
    }
  title(paste(regtype,r,ti),outer=T,line=-1,cex.main=1.5)
  savePlot(paste0(ti,"_map_TPCs",".png"),type="png")
  }

map_clusters <- function(ddd,cl="hclustcl",ti="",lat5=F,regtype="regY",ncl,r=r) {
  if(ncl <= 4) { windows(20,14);par(mfrow=c(2,2),mar=c(3,3,3,3),oma=c(0,0,2,0)) }
  if(ncl %in% c(5,6)) { windows(20,20);par(mfrow=c(3,2),mar=c(3,3,3,3),oma=c(0,0,2,0))}
  for (clx in 1:ncl) {
    if(lat5) {
      add5=2.5
      lo <- floor(ddd$lon5)
      la <- floor(ddd$lat5)
      } else {
      add5=.5
      lo <- floor(ddd$lon)
      la <- floor(ddd$lat)
      }
    lo <- factor(lo,levels=seq(min(lo,na.rm=T),max(lo,na.rm=T),2*add5))
    la <- factor(la,levels=seq(min(la,na.rm=T),max(la,na.rm=T),2*add5))
    clm <- tapply(ddd[,cl]==clx,list(lo,la),mean)
    lonn <- as.numeric(levels(lo))
    latn <- as.numeric(levels(la))
    ylm <- range(latn)
    ylm[1] <- max(ylm[1],-50)
    image(lonn,latn,clm,ylim=ylm)
    contour(lonn,latn,clm,add=T)
    map(database="world2",add=T,fill=T)
    title(paste(cl,clx),line=.5,cex.main=1.5)
    }
  title(paste(gsub("_"," ",ti),"cluster map"),outer=T,line=-2,cex.main=1.5)
  savePlot(paste0(ti,"_mapclust_",cl,".png"),type="png")
  }

plot_km_deviance <- function(dat,allsp,r,ti,regtype="regY") {
  a <- scale(dat[,allsp])
  wss <- (nrow(a)-1)*sum(apply(a[,allsp],2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(a[,allsp],centers=i,iter.max = 40)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares",main=paste("Region",r))
  savePlot(paste0(ti,"_plot_km_deviance",".png"),type="png")
  }

plot_km_deviance_trip <- function(ddd,allsp,r,ti,regtype="regY") {
  ddd$TRIP_NUM <- ddd$tripidmon
  indat <- aggregate_by_trip(ddd,allsp)
  a <- indat[,allsp]
#  a <- scale(indat[,allsp])
  wss <- (nrow(a)-1)*sum(apply(a[,allsp],2,var))
  for (i in 2:15) wss[i] <- sum(kmeans(a[,allsp],centers=i,iter.max = 40)$withinss)
  plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares",main=paste("Region",r))
  savePlot(paste0(ti,"_plot_km_deviance_trip",".png"),type="png")
  }

aggregate_by_trip <- function(dat=newdat,flds) {
  dt1 <- data.table(dat)
  setkey(dt1,TRIP_NUM)
  df2 <- dt1[, lapply(.SD, mean, na.rm=T), by=list(TRIP_NUM),.SDcols=flds]
  df2 <- data.frame(df2)
  df3 <- cbind(TRIP_NUM=df2[,1],df2[,flds]/apply(df2[,flds],1,sum))
  return(df3)
  }

make_clusters <- function(setdat=dat1,spp=allsp,ncl=5,titx="",setclust=T,tripid="tripid",fname="",regtype="regY") {
  setdat$TRIP_NUM <- as.vector(setdat[,tripid])
  spec_dat <- setdat[,allsp] #extract catch composition
  spec_dat$sum <- apply(spec_dat, 1,sum)
  nspec <- length(allsp)
  dat2 <- setdat[spec_dat$sum > 0,]
  mmult_dat <- spec_dat[spec_dat$sum>0,]
  clus_dat <- mmult_dat[,1:nspec]/mmult_dat$sum  # raw proportions
  FT = kmeans(clus_dat,ncl)$cluster

  aset <- na.omit(setdat[,spp])
  aset <- scale(aset[,spp])     # rescaled data

  indat <- aggregate_by_trip(dat2,spp)
  atrp <- na.omit(indat);
  #NbClust(atrp[,spp],method="ward.D");flush.console()
  atrp <- scale(atrp[,spp])
  dtrp <- dist(atrp, method = "euclidean");
  fittrp <- hclust(dtrp, method="ward.D")
  plot(fittrp, labels = FALSE, hang=-1,  main = paste(titx,"trip")) # display dendogram  #looks like 3 (or 4)
  grptrp <- cutree(fittrp, k=ncl) # cut tree into ncl clusters
  print(table(grptrp))
  rect.hclust(fittrp, k=ncl, border="red")
  savePlot(paste0(fname,"_hclusters_trip.png"),type="png")
  if(setclust) {
    dset <- dist(aset, method = "euclidean") # distance matrix
    fitset <- hclust(dset, method="ward.D")
    windows()
    plot(fitset, labels = FALSE, hang=-1,  main = paste(titx,"set")) # display dendogram  #looks like 3 (or 4)
    grpset <- cutree(fitset, k=ncl) # cut tree into ncl clusters
    print(table(grpset))
    rect.hclust(fitset, k=ncl, border="red")
    savePlot(paste0(fname,"_hclusters_trip.png"),type="png")
    }
  claratrp <- clara(atrp,ncl)              #clustering based upon the percent of spp in total catch of tuna
  claraset <-  clara(aset,ncl)             #clustering based upon the percent of spp in total catch of tuna
  kmtrp <- kmeans(atrp,centers=ncl,iter.max = 60)
  kmset <-  kmeans(aset,centers=ncl,iter.max = 60)
  setdat$kcltrp <- kmtrp$cluster[match(setdat$TRIP_NUM,indat$TRIP_NUM)]
  setdat$clrtrp <- claratrp$clustering[match(setdat$TRIP_NUM,indat$TRIP_NUM)]
  setdat$hcltrp <- grptrp[match(setdat$TRIP_NUM,indat$TRIP_NUM)]
  setdat$kclset <- kmset$cluster
  setdat$FT <- FT
  setdat$clrset <- claraset$clustering
  if(setclust) setdat$hclset <- grpset else setdat$hclset <- NA
  return(list(d=dtrp,fit=fittrp,clarax=claratrp,setdat=setdat))
  }

clust_PCA_run <- function(r,ddd=dat,allsp,allabs=allabs,regtype="regY",ncl,plotPCA=T,clustid="tripidmon",allclust=F,flag="JP",cllist=NA,fnhead="",
                   covarnames=c("yrqtr","latlong","hooks","hbf","vessid","callsign","Total","lat","lon","lat5","lon5","moon","op_yr","op_mon")){
  datr <- ddd[with(ddd,get(regtype))==r,allabs]
  datr$reg <- datr[,regtype]
  spec_dat <- datr[,allsp] #extract catch composition
  spec_dat$sum <- apply(spec_dat, 1,sum)
  datr <- datr[spec_dat$sum > 0,]
  if(sum(datr$sbt)==0) datr$sbt[1] <- 1
  if(sum(datr$sfa)==0) datr$sfa[1] <- 1
  if(sum(datr$blm)==0) datr$blm[1] <- 1

  pcaset <- PCA_by_set(datr,Screeplotname=paste0(fnhead," ","screeplot set"),allsp)
  pcatrp <- PCA_by_trip(datr,Screeplotname=paste0(fnhead," ","screeplot trip"),allsp)
  #################### Clustering
  datr <- data.frame(datr)
  cldat <- make_clusters(setdat=data.frame(datr),spp=allsp,ncl=ncl,titx=paste0(flag," ",regtype,r," "),setclust=F,tripid=clustid,fname=fnhead,regtype=regtype)
  plot_km_deviance_trip(ddd=datr,allsp,r=r,ti=fnhead,regtype=regtype)

  # outputs
  datprop <- cldat$setdat
  datprop[,allsp] <- cldat$setdat[,allsp] / apply(cldat$setdat[,allsp],1,sum)
  a1 <- aggregate(formula=as.formula(paste0("cbind(",paste(allsp,collapse=","),") ~ kcltrp")),data=datprop,FUN=mean); names(a1)[1] <- "cluster";a1$ctype<- "kcltrp"
  a2 <- aggregate(formula=as.formula(paste0("cbind(",paste(allsp,collapse=","),") ~ clrtrp")),data=datprop,FUN=mean); names(a2)[1] <- "cluster";a2$ctype<- "clrtrp"
  a3 <- aggregate(formula=as.formula(paste0("cbind(",paste(allsp,collapse=","),") ~ hcltrp")),data=datprop,FUN=mean); names(a3)[1] <- "cluster";a3$ctype<- "hcltrp"
  a4 <- aggregate(formula=as.formula(paste0("cbind(",paste(allsp,collapse=","),") ~ kclset")),data=datprop,FUN=mean); names(a4)[1] <- "cluster";a4$ctype<- "kclset"
  a5 <- aggregate(formula=as.formula(paste0("cbind(",paste(allsp,collapse=","),") ~ FT")),data=datprop,FUN=mean); names(a5)[1] <- "cluster";a5$ctype<- "FT"
  a6 <- aggregate(formula=as.formula(paste0("cbind(",paste(allsp,collapse=","),") ~ clrset")),data=datprop,FUN=mean); names(a6)[1] <- "cluster";a6$ctype<- "clrset"
  a <- rbind(a1,a2,a3,a4,a5,a6)
  write.csv(a,file=paste(fnhead,"cluster proportions",clustid,".csv",sep="_"))
  ll5 <- F;   dbh <- T

  ###### Create datasets ######################################################
#  covarnames <- c("yrqtr","latlong","hooks","hbf","vessid","callsign","Total","lat","lon","lat5","lon5","reg","moon","op_yr","op_mon")
  covarnames <- c(covarnames,"reg")
  dataset <- cbind(datr[,covarnames],datr[,allsp],pcaset$pcs[,1:6],pcaset$pcsBin[,1:6],pcatrp$pcs[,1:6],pcatrp$pcsBin[,1:6],cldat$setdat[,c("FT","kcltrp","clrtrp","hcltrp","kclset","clrset","hclset")])
  if(!is.na(cllist[1])) dataset <- cbind(datr[,covarnames],datr[,allsp],cldat$setdat[,cllist])
  ti=paste(fnhead,clustid,sep="_")
  # Examine factors associated with each cluster or PCA
  boxplots_spCL_comp(dat=dataset,cl="hcltrp",ti=ti,outL=F,nsp=length(allsp),regtype=regtype,r=r)
  beanplots_spCL_comp(dat=dataset,cl="hcltrp",ti=ti,outL=F,nsp=length(allsp),regtype=regtype,r=r)
  boxplots_CL(dat=dataset,cl="hcltrp",ti=ti,outL=F,dohbf=dbh,lat5=ll5,regtype=regtype,r=r)
  boxplots_CL_bean(dat=dataset,cl="hcltrp",ti=ti,outL=F,dohbf=dbh,lat5=ll5,regtype=regtype,r=r)
  map_clusters(ddd=dataset,cl="hcltrp",ti=ti,lat5=ll5,regtype=regtype,r=r,ncl=ncl)
  if(allclust) {
    boxplots_spCL_comp(dat=dataset,cl="kcltrp",ti=ti,outL=F,nsp=length(allsp),regtype=regtype,r=r)
    boxplots_spCL_comp(dat=dataset,cl="clrtrp",ti=ti,outL=F,nsp=length(allsp),regtype=regtype,r=r)
    boxplots_spCL_comp(dat=dataset,cl="kclset",ti=ti,outL=F,nsp=length(allsp),regtype=regtype,r=r)
    boxplots_spCL_comp(dat=dataset,cl="clrset",ti=ti,outL=F,nsp=length(allsp),regtype=regtype,r=r)
    boxplots_CL(dat=dataset,cl="kcltrp",ti=ti,outL=F,dohbf=dbh,lat5=ll5,regtype=regtype,r=r)
    boxplots_CL(dat=dataset,cl="clrtrp",ti=ti,outL=F,dohbf=dbh,lat5=ll5,regtype=regtype,r=r)
    boxplots_CL(dat=dataset,cl="FT",ti=ti,outL=F,dohbf=dbh,lat5=ll5,regtype=regtype,r=r)
    boxplots_CL(dat=dataset,cl="kclset",ti=ti,outL=F,dohbf=dbh,lat5=ll5,regtype=regtype,r=r)
    boxplots_CL(dat=dataset,cl="clrset",ti=ti,outL=F,dohbf=dbh,lat5=ll5,regtype=regtype,r=r)
  }
  graphics.off()

  if(plotPCA) {
    boxplots_PCA(dat=dataset,nPCA=4,ti=ti,dohbf=dbh,lat5=ll5,regtype=regtype,r=r)
    boxplots_spPCA(dat=dataset,nPCA=4,ti=ti,nsp=8,regtype=regtype,r=r)
    mapPCA(ddd=dataset,nPCA=4,ti=ti,lat5=ll5,regtype=regtype,r=r)
    boxplots_TPCA(dat=dataset,nPCA=4,ti=ti,dohbf=dbh,lat5=ll5)
    boxplots_spTPCA(dat=dataset,nPCA=4,ti=ti,nsp=8)
    mapTPCA(ddd=dataset,nPCA=4,ti=ti,lat5=ll5)
    graphics.off()
    }
  return(invisible(dataset))
  }

# PCA by set
PCA_by_set <- function(datr,Screeplotname="screeplot set",allsp) {
  spec_dat <- datr[,allsp] #extract catch composition
  spec_dat$sum <- apply(spec_dat, 1,sum)
  nspec <- length(allsp)
  dat2 <- datr[spec_dat$sum > 0,]
  mmult_dat <- spec_dat[spec_dat$sum>0,]
  pca_dat <- (sqrt(sqrt(mmult_dat[,1:nspec]/mmult_dat$sum))) ## prepare species composition for PCA
  apply(spec_dat,2,sum)
  pca<-prcomp(pca_dat, scale=T) #run PCA
  pr_pca <- predict(pca,pca_dat) # Predict PCA Loadings
  pcs <- data.frame(pr_pca[,1:nspec])
  eig = summary(pca)$sdev^2 # get Eigenvalue
  OCtest = nScree(eig) # run Optimal Coordinates test
  windows()
  plotnScree(OCtest);par(col="black")
  savePlot(Screeplotname,type="png")
  nPC = min(OCtest$Components$nkaiser,OCtest$Components$noc) # retain number of PCs in combination with Eigenvalue > 1
  pcsBin = ifelse(pcs>0.5,1,0)  ### GLM-DPC variable as binary PCs
  dimnames(pcsBin)[[2]] <- paste0("B",names(pcs))
  return(PCAset=list(pcs=pcs,pcsBin=pcsBin))
}

# PCA by trip
PCA_by_trip <- function(datr,Screeplotname="screeplot set",allsp) {
  spec_dat <- datr[,allsp] #extract catch composition
  spec_dat$sum <- apply(spec_dat, 1,sum)
  nspec <- length(allsp)
  dat2 <- datr[spec_dat$sum > 0,]
  mmult_dat <- spec_dat[spec_dat$sum>0,]
  dat2$TRIP_NUM <- dat2$tripidmon
  tpagg <- aggregate_by_trip(dat2,allsp)
  pca_tpdat <- (sqrt(sqrt(tpagg[,allsp]))) ## prepare species composition for PCA
  pca <- prcomp(pca_tpdat, scale=T) #run PCA
  pr_pca <- predict(pca,pca_tpdat) # Predict PCA Loadings
  pcs <- data.frame(pr_pca[,1:nspec])
  names(pcs) <- paste0("T",names(pcs))
  setpcs <- pcs[match(dat2$TRIP_NUM,tpagg$TRIP_NUM),]
  eigtp = summary(pca)$sdev^2 # get Eigenvalue
  OCtesttp = nScree(eigtp) # run Optimal Coordinates test
  windows()
  plotnScree(OCtesttp);par(col="black")
  savePlot(Screeplotname,type="png")
  nPC = min(OCtesttp$Components$nkaiser,OCtesttp$Components$noc) # retain number of PCs in combination with Eigenvalue > 1
  pcsBin = ifelse(pcs>0.5,1,0)  ### GLM-DPC variable as binary PCs
  dimnames(pcsBin)[[2]] <- paste0("B",names(pcs))
  setpcsBin <- pcsBin[match(dat2$TRIP_NUM,tpagg$TRIP_NUM),]
  return(PCAtrip=list(pcs=setpcs,pcsBin=setpcsBin))
  }


Mode <- function(x) {
# from http://stackoverflow.com/questions/2547402/standard-library-function-in-r-for-finding-the-mode
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

make_newdat <- function(model,datx) {
  vessidx <- Mode(datx$vessid)
  latlongx <- Mode(datx$latlong)
  newdat <- expand.grid(yrqtr=sort(unique(datx$yrqtr)),vessid=vessidx,latlong=latlongx,hooks=median(datx$hooks),hbf=median(datx$hbf))
  predresp <- predict(model,newdata=newdat,type="response",se.fit=T)
  predterms <- predict(model,newdata=newdat,type="terms",se.fit=T)
  return(list(newdat=newdat,predresp=predresp,predterms=predterms))
  }

make_newdat2 <- function(model,datx) {
  vessidx <- Mode(datx$vessid)
  latlongx <- Mode(datx$latlong)
  newdat <- expand.grid(yrqtr=sort(unique(datx$yrqtr)),vessid=vessidx,latlong=latlongx,hooks=median(datx$hooks),hbf=median(datx$hbf),moon=0.5)
  predresp <- predict(model,newdata=newdat,type="response",se.fit=T)
  predterms <- predict(model,newdata=newdat,type="terms",se.fit=T)
  return(list(newdat=newdat,predresp=predresp,predterms=predterms))
  }

make_newdat3 <- function(model,datx) {
  vessidx <- Mode(datx$vessid)
  latlongx <- Mode(datx$latlong)
  if("hbf" %in% names(datx) & "clust" %in% names(datx)) newdat <- expand.grid(yrqtr=sort(unique(datx$yrqtr)),vessid=vessidx,latlong=latlongx,hooks=median(datx$hooks),hbf=median(datx$hbf),clust=Mode(datx$clust))
  if("hbf" %in% names(datx) & !"clust" %in% names(datx)) newdat <- expand.grid(yrqtr=sort(unique(datx$yrqtr)),vessid=vessidx,latlong=latlongx,hooks=median(datx$hooks),hbf=median(datx$hbf))
  if(!"hbf" %in% names(datx) & "clust" %in% names(datx)) newdat <- expand.grid(yrqtr=sort(unique(datx$yrqtr)),vessid=vessidx,latlong=latlongx,hooks=median(datx$hooks),clust=Mode(datx$clust))
  if(!"hbf" %in% names(datx) & !"clust" %in% names(datx)) newdat <- expand.grid(yrqtr=sort(unique(datx$yrqtr)),vessid=vessidx,latlong=latlongx,hooks=median(datx$hooks))
  predresp <- predict(model,newdata=newdat,type="response",se.fit=T)
  predterms <- predict(model,newdata=newdat,type="terms",se.fit=T)
  return(list(newdat=newdat,predresp=predresp,predterms=predterms))
  }

make_newdat_TW <- function(model,datx) {
  vessidx <- Mode(datx$vessid)
  latlongx <- Mode(datx$latlong)
  newdat <- expand.grid(yrqtr=sort(unique(datx$yrqtr)),vessid=vessidx,latlong=latlongx,hooks=median(datx$hooks),
    bt1=sort(unique(datx$bt1))[1],bt2=sort(unique(datx$bt2))[1],bt3=sort(unique(datx$bt3))[1],
    bt4=sort(unique(datx$bt4))[1],bt5=sort(unique(datx$bt5))[1],moon=0.5)
  predresp <- predict(model,newdata=newdat,type="response",se.fit=T)
  predterms <- predict(model,newdata=newdat,type="terms",se.fit=T)
  return(list(newdat=newdat,predresp=predresp,predterms=predterms))
  }

make_newdat_JP <- function(model,datx) {
  vessidx <- Mode(datx$vessid)
  latlongx <- Mode(datx$latlong)
  newdat <- expand.grid(yrqtr=sort(unique(datx$yrqtr)),vessid=vessidx,latlong=latlongx,hooks=median(datx$hooks),
            moon=0.5,hbf=median(datx$hbf,na.rm=T))
  predresp <- predict(model,newdata=newdat,type="response",se.fit=T)
  predterms <- predict(model,newdata=newdat,type="terms",se.fit=T)
  return(list(newdat=newdat,predresp=predresp,predterms=predterms))
  }

ls.objects <- function (pos = 1, pattern, order.by, decreasing = FALSE, head = FALSE, n = 5)
{
    napply <- function(names, fn) sapply(names, function(x) fn(get(x,
        pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x) as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.dim)
    names(out) <- c("Type", "Size", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing = decreasing),
            ]
    if (head)
        out <- head(out, n)
    out
}

lsos <- function(...,n=10) {
  ls.objects(...,order.by="Size",decreasing=TRUE,head=TRUE,n=n)
  }

limit_vessels <- function(gdat,minqtrs,maxqtrs=10000) {
  a <- table(gdat$vessid,gdat$yrqtr)
  a <- apply(a>0,1,sum)
  table(a)
  a <- a[a >= minqtrs & a <= maxqtrs]
  gdat <- gdat[gdat$vessid %in% names(a),]
  a <- table(gdat$yrqtr);a
  a <- a[a>=100]
  gdat <- gdat[gdat$yrqtr %in% names(a),]
  a <- table(gdat$vessid);a
  a <- a[a>=100]
  gdat <- gdat[gdat$vessid %in% names(a),]
  return(gdat)
  }

  lu <- function(x) {
  length(unique(x))
  }

dimu <- function(x) { length(unique(x)) }

countzero <- function(a) {
  z <- sum(a==0)
  tot <- length(a)
  pz <- z/tot ; return(pz)
  }

summarize_and_store <- function(mod,dat,fname,modlab,dohbf=dohbf,docl=docl,keepd = TRUE) {
  coefs <- get.coefs(mod)
  windows(); par(mfrow=c(2,1),mar=c(4,4,3,1))  # plot diagnostics
  plotdiags(mod$residuals,ti=paste(fname,modlab)); savePlot(paste(fname,modlab,"diags",sep="_"),type="png")
  if(is.null(mod$xlevels$vessid)) dovess <- FALSE else dovess <- TRUE
  plot_effects_IO(mod,indat=dat,dovess=dovess,addcl=addcl,dohbf=dohbf); savePlot(paste0(fname,"_",modlab,"_effects.png"),type="png")
  nd <- make_newdat3(model=mod,datx=dat); save(nd,file=paste0(fname,"_",modlab,"_predictions.RData"))
  summ <- summary(mod); save(summ,file=paste0(fname,"_",modlab,"_summary.RData"))
  if (!keepd) mod$data <- NULL
  save(mod,file=paste0(fname,"_",modlab,"_model.RData"))
  gc();graphics.off()
}

defactor <- function(x) as.numeric(as.character(x))

revtrunc <- function(x) { x - floor(x) }

plot_effects_IOx <- function(model,indat,dovess=T,addcl=F,addpca=F,ti="",dohbf=T) {
  cf <- model$coefficients
  pred <- predict(model,data=indat,type="terms",se.fit=T)
  fishlab <- switch(runsp,yft="Yellowfin",bet="Bigeye"); methlab <- switch(mt,deltabin="Delta-binomial",deltapos="Delta-positive",logl="Lognormal(+0.01)",propn="Proportion Bigeye")
  if(addcl != F) nfigs <- 6 + addpca
  if(addcl == F) nfigs <- 5 + addpca
  mf <- switch(nfigs-4,c(2,3),c(2,3),c(3,3),c(3,3),c(3,3),c(3,4))
  hw <- c(14,19)
  windows(height=hw[1],width=hw[2])
  par(mfrow=mf,mar=c(5,4,2,1),oma=c(0,0,3,0))
  pr <- pred$fit ; prse <- pred$se.fit
  termlist <- dimnames(pred$fit)[[2]]
  llpos <- grep("latlong",termlist)
  hbfpos  <- grep("hbf",termlist)
  vesspos <- grep("vessid",termlist)
  branchpos <- grep("branchline",termlist)
  if(length(grep("hooks",termlist))>0) db <- T else db <- F

  #  browser()
  index <- sort(unique(indat$yrqtr))
  b <- match(index,indat$yrqtr)
  out <- exp(pr[b,1])
  se1 <- exp(pr[b,1] - 1.96 * prse[b,1])
  se2 <- exp(pr[b,1] + 1.96 * prse[b,1])
  mout <- mean(out)
  out <- out/mout;se1 <- se1/mout;se2 <- se2/mout;
  plot(as.numeric(as.character(index)),out,ylim=c(0,3),xlab="Year",ylab="Effect size",main="Year effects")
  segments(as.numeric(as.character(index)), se1,  as.numeric(as.character(index)), se2, lty=1, col="slate grey")
  points(as.numeric(as.character(index)), out, pch=16)

  index <- sort(unique(indat$latlong))
  b <- match(index,indat$latlong)
  out <- exp(pr[b,llpos])
  se1 <- exp(pr[b,llpos] - 1.96 * prse[b,llpos])
  se2 <- exp(pr[b,llpos] + 1.96 * prse[b,llpos])
  mout <- mean(out)
  out <- out/mout;se1 <- se1/mout;se2 <- se2/mout;
  plot(as.numeric(index),out,ylim=c(0,3),xlab="Latlong",ylab="Effect size",main="Spatial effects")
  segments(as.numeric(index), se1,  as.numeric(index), se2, lty=1, col="slate grey")
  points(as.numeric(index), out, pch=16)
  #  plot(indat$latlong,pr[,2],ylim=c(-.75,.75),xlab="Latitude",ylab="Effect size")

  ##plot coefficients
  library(maps)
  ll <- as.character(indat$latlong)
  index <- sort(unique(ll))
  index2 <- unlist(strsplit(index,"\\_"))
  pos1 <- 2*(1:length(index)) -1
  pos2 <- pos1 + 1
  lats <- as.numeric(index2[pos1])
  lons <- as.numeric(index2[pos2])
  alat <- round(lats[match(ll,index)]*2)/2 + 2.5
  alon <- round(lons[match(ll,index)]*2)/2 + 2.5
  # lat <- trunc((dat$newlat[match(index, dat$latlong)] + 50)/5) * 5 + 2.5 - 50
  # long <- trunc((dat$newlong[match(index, dat$latlong)])/5) * 5 + 2.5
  coefs <- exp(pr[match(ll,index),llpos])
  coefs2 <- tapply(coefs, list(alon, alat), mean)
  coefs2 <- coefs2/mean(coefs2)
  image(sort(as.numeric(unique(alon))), sort(unique(alat)), coefs2, zlim=c(0.5, 2.5), ylab="Lat", xlab="Long")
  if(length(coefs2) > 5) contour(sort(unique(alon)), sort(unique(alat)), coefs2, levels= c(0,1,2,2.5), add=TRUE,col=4)
  map(add=TRUE)

  if(dovess) {
    index <- sort(unique(indat$vessid))
    b <- match(index,indat$vessid)
    out <- exp(pr[b,vesspos])
    se1 <- exp(pr[b,vesspos] - 1.96 * prse[b,vesspos])
    se2 <- exp(pr[b,vesspos] + 1.96 * prse[b,vesspos])
    mout <- mean(out)
    out <- out/mout;se1 <- se1/mout;se2 <- se2/mout;
    ind <- as.factor(index)
    plot(match(ind,ind),out,type="n",ylim=c(0,3),xlab="Vessel ID",ylab="Effect size",main="")
    segments(match(ind,ind), se1,  match(ind,ind), se2, lty=1, col="slate grey")
    points(match(ind,ind), out, pch=16)

    vts <- tapply(pr[,vesspos],list(indat$yrqtr,indat$vessid),mean)
    y<-exp(as.vector(vts))
    x<-rep(as.numeric(dimnames(vts)[[1]]),dim(vts)[[2]])
    plot(x,y,type="p",ylim=c(0,3),xlab="Vessel ID",ylab="Effect size",main="Vessel effects",pch=16,cex=0.9)
    mn <- tapply(exp(pr[,vesspos]),indat$yrqtr,mean)
    lines(as.numeric(dimnames(vts)[[1]]), mn, col=2,lwd=2)
    #  plot(as.factor(indat$vessid),pr[,vesspos],ylim=c(-.75,.75),xlab="Vessel",ylab="Effect size")
  }

  if(dohbf) {
    rsp <- pr[,hbfpos[1]]
    rsp.se <- prse[,hbfpos[1]]
    b <- match(sort(unique(indat$hbf)),indat$hbf)
    plot(indat$hbf[b],exp(pr[,hbfpos][b]),ylim=c(0,3),xlab="HBF",ylab="Effect size",main="HBF effects")
    lines(indat$hbf[b],exp(rsp[b]+2*rsp.se[b]),lty=2)
    lines(indat$hbf[b],exp(rsp[b]-2*rsp.se[b]),lty=2)
  }

  if(addcl != F) {
    index <- sort(unique(indat$cl))
    b <- match(index,indat$cl)
    out <- exp(pr[b,1])
    se1 <- exp(pr[b,1] - 1.96 * prse[b,1])
    se2 <- exp(pr[b,1] + 1.96 * prse[b,1])
    mout <- mean(out)
    out <- out/mout;se1 <- se1/mout;se2 <- se2/mout;
    plot(index,out,ylim=c(0,3),xlab="Cluster category",ylab="Effect size",main="Cluster effects")
    segments(as.numeric(index), se1, as.numeric(index), se2, lty=1, col="slate grey")
    points(index, out, pch=16)
  }

  if(addpca) {
    ind <- indat[order(indat$pca),]
    out <- exp(pr[b,1])
    se1 <- exp(pr[b,1] - 1.96 * prse[b,1])
    se2 <- exp(pr[b,1] + 1.96 * prse[b,1])
    mout <- mean(out)
    out <- out/mout;se1 <- se1/mout;se2 <- se2/mout;
    plot(as.numeric(as.character(index)),out,ylim=c(0,3),xlab="Cluster category",ylab="Effect size",main="Cluster effects")
    segments(as.numeric(as.character(index)), se1,  as.numeric(as.character(index)), se2, lty=1, col="slate grey")
    points(as.numeric(as.character(index)), out, pch=16)
  }
  title(main=ti,cex.main=1.5,outer=T)
}

get.coefsx <- function(nd, mn, bcorr=F) {
  nd$areas <- cell_areas$garea[match(nd$latlong, cell_areas$latlong)]
  nd$density <- exp(nd$fit + bcorr * (nd$sd^2 / 2)) - mn
  nd$fish <- nd$density * nd$areas
  index <- tapply(nd$fish, nd$yrqtr, sum)
  coefs <- index/mean(index)
  return(coefs)
}

get.bincoefsx <- function(nd) {
  nd$areas <- cell_areas$garea[match(nd$latlong, cell_areas$latlong)]
  nd$prob <- inv.logit(nd$fit)
  nd$fish <- nd$prob * nd$areas
  coefs <- tapply(nd$fish, nd$yrqtr, sum) / tapply(nd$areas, nd$yrqtr, sum)
  return(coefs)
}

get_medians <- function(dat) {

}

summarize_and_store_logCx <- function(mod1,mod2,dat,fname,modlab,addcl=addcl,dohbf=dohbf, keepd = TRUE, mn, bcorr=FALSE) {
  # mod1=model1, mod2=model2, dat, fname, modlab, addcl, dohbf, keepd, mn, bcorr
  # mod1=model1;mod2=model2;fname;modlab=modlab;dohbf=dohbf;addcl=addcl; keepd = keepd;bcorr=F
  adat <- length(unique(dat$yrqtr))
  summ1 <- summary(mod1)
  summ2 <- summary(mod2)
  save(summ1, summ2, file=paste0(fname,"_",modlab,"_summary.RData"))

  vessidx <- Mode(dat$vessid)
  latlongx <- Mode(dat$latlong)
  ndtxt <- "expand.grid("
  if (isTRUE(grep("clust", mod1$formula) >= 1)) ndtxt <- paste0(ndtxt, "clust=Mode(dat$clust),")
  if (dohbf) ndtxt <- paste0(ndtxt, "hbf=median(dat$hbf),")
  if (isTRUE(grep("lat5", mod1$formula) >= 1)) ndtxt <- paste0(ndtxt, "latlong=sort(unique(dat$latlong)),")
  ndtxt <- paste0(ndtxt, "yrqtr=sort(unique(dat$yrqtr)),hooks=median(dat$hooks),vessid=vessidx)")
  newdat <- eval(parse(text=ndtxt))
  yqloc <- match(newdat$yrqtr, dat$yrqtr)
  newdat$op_yr <- dat[yqloc,]$op_yr
  newdat$qtr <- dat[yqloc,]$qtr
  newdat$lat5 <- dat[match(newdat$latlong, dat$latlong),]$lat5

  pred1resp  <- predict(mod1,newdata=newdat,type="response",se.fit=T)
  pred1terms <- predict(mod1,newdata=newdat,type="terms",se.fit=T)
  nd1 <- cbind(newdat,fit = pred1resp$fit, sd = pred1resp$se.fit)

  pred2resp <- predict(mod2,newdata=newdat,type="response",se.fit=T)
  nd2 <- cbind(newdat,fit = pred2resp$fit, sd = pred2resp$se.fit)
  pred2terms <- predict(mod2,newdata=newdat,type="terms",se.fit=T)
  ndx <- nd1
  loc <- is.na(ndx$fit)
  ndx[loc,]$fit <- nd2[loc,]$fit
  ndx[loc,]$sd <- nd2[loc,]$sd
  res = list(nd1=nd1, nd2=nd2, pred1terms=pred1terms, pred2terms=pred2terms)
  save(res,file=paste0(fname,"_",modlab,"_predictions.RData"))

  coefsx <- get.coefsx(ndx, mn, bcorr = bcorr)
  coefs2 <- get.coefsx(nd2, mn, bcorr = bcorr)
  windows(width = 12, height = 14); par(mfrow = c(2,1), mar = c(4.2, 5, 2, 2))
  yqx <- as.numeric(names(coefsx))
  yq2 <- as.numeric(names(coefs2))
  yqall <- seq(min(yqx), max(yqx), 0.25)
  plot(yqall, coefsx[match(yqall, yqx)], type = "l", ylim = c(0,4), xlab = "Year - quarter", ylab = "Index")
  lines(yqall, coefs2[match(yqall, yq2)], col = 2)
  legend("topright", legend = c("Area x time", "Area + time"), lty = 1, col = 1:2)
  plot(yqall, coefsx[match(yqall, yqx)] / coefs2[match(yqall, yqx)], type = "p", ylim = c(0,3), xlab = "Year - quarter", ylab = "Ratio of models, area x time / area + time")
  savePlot(paste(fname,modlab,"compare_types",sep="_"),type="png")

  windows(); par(mfrow=c(2,1),mar=c(4,4,3,1))  # plot diagnostics
  plotdiags(mod1$residuals,ti=paste(fname,modlab)); savePlot(paste(fname,modlab,"diags1",sep="_"),type="png")
  windows(); par(mfrow=c(2,1),mar=c(4,4,3,1))  # plot diagnostics
  plotdiags(mod2$residuals,ti=paste(fname,modlab)); savePlot(paste(fname,modlab,"diags2",sep="_"),type="png")
  if(is.null(mod1$xlevels$vessid)) dovess <- FALSE else dovess <- TRUE

  if(is.null(mod1$xlevels$vessid)) dovess <- FALSE else dovess <- TRUE
  plot_effects_IO(mod1,indat=dat,dovess=dovess,addcl=addcl,dohbf=dohbf); savePlot(paste0(fname,"_",modlab,"_logCx_effects.png"),type="png")
  plot_effects_IO(mod2,indat=dat,dovess=dovess,addcl=addcl,dohbf=dohbf); savePlot(paste0(fname,"_",modlab,"_logC_effects.png"),type="png")

  #  plot_effects_IOx(mod1,indat=dat,dovess,dohbf=dohbf); savePlot(paste0(fname,"_",modlab,"_effects.png"),type="png")
  if (!keepd) mod1$data <- mod2$data <- NULL
  save(mod1, mod2,file=paste0(fname,"_",modlab,"_model.RData"))
  gc();graphics.off()
}

make_newdat_x <- function(mod,dat) {
  vessidx <- Mode(dat$vessid)
  latlongx <- Mode(dat$latlong)
  ndtxt <- "expand.grid("
  if (isTRUE(grep("clust", mod$formula) >= 1)) ndtxt <- paste0(ndtxt, "clust=Mode(dat$clust),")
  if (dohbf) ndtxt <- paste0(ndtxt, "hbf=median(dat$hbf),")
  if (isTRUE(grep("lat5", mod$formula) >= 1)) ndtxt <- paste0(ndtxt, "latlong=sort(unique(dat$latlong)),")
  ndtxt <- paste0(ndtxt, "yrqtr=sort(unique(dat$yrqtr)),hooks=median(dat$hooks),vessid=vessidx)")
  newdat <- eval(parse(text=ndtxt))
  yqloc <- match(newdat$yrqtr, dat$yrqtr)
  newdat$op_yr <- dat[yqloc,]$op_yr
  newdat$qtr <- dat[yqloc,]$qtr
  newdat$lat5 <- dat[match(newdat$latlong, dat$latlong),]$lat5
  return(newdat)
}

su <- function(x) sort(unique(x))

summarize_and_store_dlx <- function(modb1,modb2,modp1,modp2,dat,datpos,fname,modlab,dohbf=dohbf,addcl=addcl, keepd = TRUE,bcorr) {
  abin <- length(unique(dat$yrqtr))
  apos <- length(unique(datpos$yrqtr))
  summodb1 <- summary(modb1)
  summodp1 <- summary(modp1)
  summodb2 <- summary(modb2)
  summodp2 <- summary(modp2)
  save(summodb1,summodp1,summodb2,summodp2,file=paste(fname,"_",modlab," summary_deltax.RData",sep=""))

  # make data bin
  newdat <- make_newdat_x(modb1, dat)

  # predict responses bin
  predrespb1 <- predict.glm(modb1,newdata=newdat,type="response",se.fit=T)
  predrespb2 <- predict.glm(modb2,newdata=newdat,type="response",se.fit=T)
  predtermsb1 <- predict.glm(modb1,newdata=newdat,type="terms",se.fit=T)
  predtermsb2 <- predict.glm(modb2,newdata=newdat,type="terms",se.fit=T)
  ndb1 <- cbind(newdat,fit = predrespb1$fit, sd = predrespb1$se.fit, sdt = predtermsb1$se.fit)
  ndb2 <- cbind(newdat,fit = predrespb2$fit, sd = predrespb2$se.fit, sdt = predtermsb2$se.fit)

  # Substitute when missing values in interaction run
  loc <- is.na(ndb1$fit)
  ndb1[loc,]$fit <- ndb2[loc,]$fit
  ndb1[loc,]$sd  <- ndb2[loc,]$sd
  ndb1[loc,]$sdt <- ndb2[loc,]$sdt
  # Make indices
  mnb <- logit(mean(modb1$y))
  a1 <- ndb1$fit - mean(ndb1$fit) + mnb
  a2 <- ndb2$fit - mean(ndb2$fit) + mnb
  ndb1$fit2 <- inv.logit(a1)
  ndb2$fit2 <- inv.logit(a2)
  ndbin1 = list(newdat=ndb1, predresp=predrespb1, predterms=predtermsb1)
  ndbin2 = list(newdat=ndb2, predresp=predrespb2, predterms=predtermsb2)
  save(ndbin1, ndbin2, file=paste0(fname,"_bin_",modlab,"_predictions.RData"))

  # make data pos
  newdat <- make_newdat_x(modp1, datpos)

  # predict responses pos
  predrespp1 <- predict.glm(modp1,newdata=newdat,type="response",se.fit=T)
  predrespp2 <- predict.glm(modp2,newdata=newdat,type="response",se.fit=T)
  predtermsp1 <- predict.glm(modp1,newdata=newdat,type="terms",se.fit=T)
  predtermsp2 <- predict.glm(modp2,newdata=newdat,type="terms",se.fit=T)
  ndp1 <- cbind(newdat,fit = predrespp1$fit, sd = predrespp1$se.fit, sdt = predtermsp1$se.fit)
  ndp2 <- cbind(newdat,fit = predrespp2$fit, sd = predrespp2$se.fit, sdt = predtermsp2$se.fit)

  # Substitute pos when missing values in interaction run
  loc <- is.na(ndp1$fit)
  ndp1[loc,]$fit <- ndp2[loc,]$fit
  ndp1[loc,]$sd  <- ndp2[loc,]$sd
  ndp1[loc,]$sdt <- ndp2[loc,]$sdt
  # Make indices
  ndp1$fit2 <- exp(ndp1$fit)
  ndp2$fit2 <- exp(ndp2$fit)
  ndpos1 = list(newdat=ndp1, predresp=predrespp1, predterms=predtermsp1)
  ndpos2 = list(newdat=ndp2, predresp=predrespp2, predterms=predtermsp2)
  save(ndpos1, ndpos2, file=paste0(fname,"_pos_",modlab,"_predictions.RData"))

  coefs.bin1 <- get.bincoefsx(ndb1)
  coefs.bin2 <- get.bincoefsx(ndb2)
  coefs.pos1 <- get.coefsx(ndp1, mn, bcorr = bcorr)
  coefs.pos2 <- get.coefsx(ndp2, mn, bcorr = bcorr)

  a <- na.omit(match(su(ndb1$yrqtr),su(ndp1$yrqtr)))  # make indices
  coefs1 <- coefs.bin1[a] * coefs.pos1
  coefs2 <- coefs.bin2[a] * coefs.pos2

  windows(width = 12, height = 14); par(mfrow = c(2,1), mar = c(4.2, 5, 2, 2))
  yq1 <- as.numeric(names(coefs1))
  yq2 <- as.numeric(names(coefs2))
  yqall <- seq(min(yq1), max(yq1), 0.25)
  plot(yqall, coefs1[match(yqall, yq1)], type = "l", ylim = c(0,4), xlab = "Year - quarter", ylab = "Index")
  lines(yqall, coefs2[match(yqall, yq2)], col = 2)
  legend("topright", legend = c("Area x time", "Area + time"), lty = 1, col = 1:2)
  plot(yqall, coefs1[match(yqall, yq1)] / coefs2[match(yqall, yq1)], type = "p", ylim = c(0,3), xlab = "Year - quarter", ylab = "Ratio of models, area x time / area + time")
  savePlot(paste(fname,modlab,"compare_types",sep="_"),type="png")

  windows(); par(mfrow=c(2,1),mar=c(4,4,3,1))  # plot diagnostics
  plotdiags(modp1$residuals,ti=paste(fname,modlab, "area intx"));savePlot(paste(fname,modlab,"diags_intx.png",sep="_"),type="png")
  plotdiags(modp2$residuals,ti=paste(fname,modlab));savePlot(paste(fname,modlab,"diags.png",sep="_"),type="png")

  if(is.null(modb1$xlevels$vessid)) dovess <- FALSE else dovess <- TRUE
  plot_effects_IO(modb1,indat=dat,dovess=dovess,addcl=addcl,dohbf=dohbf); savePlot(paste0(fname,"_",modlab,"_binx_effects.png"),type="png")
  plot_effects_IO(modp1,indat=datpos,dovess=dovess,addcl=addcl,dohbf=dohbf); savePlot(paste0(fname,"_",modlab,"_posx_effects.png"),type="png")
  plot_effects_IO(modb2,indat=dat,dovess=dovess,addcl=addcl,dohbf=dohbf); savePlot(paste0(fname,"_",modlab,"_bin_effects.png"),type="png")
  plot_effects_IO(modp2,indat=datpos,dovess=dovess,addcl=addcl,dohbf=dohbf); savePlot(paste0(fname,"_",modlab,"_pos_effects.png"),type="png")

  if (!keepd) { modb1$data <- NULL;modb2$data <- NULL; modp1$data <- NULL; modp2$data <- NULL }
  save(modb1,modp1,modb2,modp2,file=paste(fname,"_",modlab," moddelta.RData",sep=""))
  gc();graphics.off()
}

summarize_and_store_dl <- function(modb,modp,dat,datpos,fname,modlab,dohbf=dohbf,addcl=addcl, keepd = TRUE) {
  abin <- length(unique(dat$yrqtr))
  apos <- length(unique(datpos$yrqtr))
  summodb <- summary(modb)
  summodp <- summary(modp)
  save(summodb,summodp,file=paste(fname,"_",modlab," summary_delta.RData",sep=""))

  vessidx <- Mode(dat$vessid)
  latlongx <- Mode(dat$latlong)
  if("clust" %in% names(dat)) newdat <- expand.grid(yrqtr=sort(unique(dat$yrqtr)),latlong=latlongx,hooks=median(dat$hooks),vessid=vessidx,clust=Mode(dat$clust))
  if(!"clust" %in% names(dat)) newdat <- expand.grid(yrqtr=sort(unique(dat$yrqtr)),latlong=latlongx,hooks=median(dat$hooks),vessid=vessidx)
  if( dohbf & "clust" %in% names(dat)) newdat <- expand.grid(yrqtr=sort(unique(dat$yrqtr)),latlong=latlongx,hooks=median(dat$hooks),vessid=vessidx,clust=Mode(dat$clust),hbf=median(dat$hbf))
  if(dohbf & !"clust" %in% names(dat)) newdat <- expand.grid(yrqtr=sort(unique(dat$yrqtr)),latlong=latlongx,hooks=median(dat$hooks),vessid=vessidx,hbf=median(dat$hbf))
  #    assign("fmla.bin", as.formula(fmla.bin), envir = .GlobalEnv)

  predresp <- predict.glm(modb,newdata=newdat,type="response",se.fit=T)
  predterms <- predict.glm(modb,newdata=newdat,type="terms",se.fit=T)
  mn <- logit(mean(modb$y))
  a <- predresp$fit - mean(predresp$fit) + mn
  predresp$fit2 <- inv.logit(a)
  ndbin = list(newdat=newdat,predresp=predresp,predterms=predterms)
  save(ndbin,file=paste0(fname,"_bin_",modlab,"_predictions.RData"))

  vessidx <- Mode(datpos$vessid)
  latlongx <- Mode(datpos$latlong)
  if(!"clust" %in% names(dat)) newdat <- expand.grid(yrqtr=sort(unique(datpos$yrqtr)),latlong=latlongx,hooks=median(datpos$hooks),vessid=vessidx)
  if(dohbf & "clust" %in% names(dat)) newdat <- expand.grid(yrqtr=sort(unique(datpos$yrqtr)),latlong=latlongx,hooks=median(datpos$hooks),vessid=vessidx,clust=Mode(datpos$clust),hbf=median(datpos$hbf))
  if(dohbf & !"clust" %in% names(dat)) newdat <- expand.grid(yrqtr=sort(unique(datpos$yrqtr)),latlong=latlongx,hooks=median(datpos$hooks),vessid=vessidx,hbf=median(datpos$hbf))
  predresp <- predict(modp,newdata=newdat,type="response",se.fit=T)
  predterms <- predict(modp,newdata=newdat,type="terms",se.fit=T)
  ndpos = list(newdat=newdat,predresp=predresp,predterms=predterms)
  save(ndpos,file=paste0(fname,"_pos_",modlab,"_predictions.RData"))

  pbin <- ndbin$predresp$fit2
  ppos <- ndpos$predresp$fit
  a <- na.omit(match(ndbin$newdat$yrqtr,ndpos$newdat$yrqtr))  # make indices
  pcoefs <- pbin[a] * exp(ppos)
  pcoefs <- pcoefs/mean(pcoefs)

  coefs.bin <- get.bin.coefs(modb,abin,dat);
  coefs.pos <- get.coefs(modp,apos);
  windows(); par(mfrow=c(2,1),mar=c(4,4,3,1))  # plot diagnostics
  plotdiags(modp$residuals,ti=paste(fname,modlab)); savePlot(paste(fname,modlab,"diags.png",sep="_"),type="png")
  if(is.null(modb$xlevels$vessid)) dovess <- FALSE else dovess <- TRUE
  plot_effects_IO(modb,indat=dat,dovess=dovess,addcl=addcl,dohbf=dohbf); savePlot(paste0(fname,"_",modlab,"_bin_effects.png"),type="png")
  plot_effects_IO(modp,indat=datpos,dovess=dovess,addcl=addcl,dohbf=dohbf); savePlot(paste0(fname,"_",modlab,"_pos_effects.png"),type="png")

  mn <- with(dat,0.1* mean(get(runsp)/hooks))
  opyr <- sort(unique(datpos$yrqtr))
  a <- match(names(coefs.pos),names(coefs.bin))             # make indices
  coefs <- coefs.bin[a] * coefs.pos
  save(coefs.bin,coefs.pos,coefs,pcoefs,file=paste(fname,"_",modlab,"_indices.RData",sep=""))
  if (!keepd) { modb$data <- NULL; modp$data <- NULL }
  save(modb,modp,file=paste(fname,"_",modlab," moddelta.RData",sep=""))
  gc();graphics.off()
}

#    model,indat,dovess=T,addcl=F,addpca=F,ti="",dohbf=T

do_lognCx <- function(dat,dohbf=F,addboat=F,addcl=addcl,nhbf=3,runsp,fname,modlab,keepd=TRUE, lat5xqtr = T, lat5xyr = T, bcorr=FALSE){
  mn <- with(dat,0.1* mean(get(runsp)/hooks))
  fmla1 <-     make_formula_IOx(runsp,modtype="logn",dohbf=dohbf,addboat=addboat,addcl=T,nhbf=nhbf, lat5xqtr = lat5xqtr, lat5xyr = lat5xyr)
  fmla1.ncl <- make_formula_IOx(runsp,modtype="logn",dohbf=dohbf,addboat=addboat,addcl=F,nhbf=nhbf, lat5xqtr = lat5xqtr, lat5xyr = lat5xyr)
  fmla2 <-     make_formula_IOx(runsp,modtype="logn",dohbf=dohbf,addboat=addboat,addcl=T,nhbf=nhbf, lat5xqtr = F, lat5xyr = F)
  fmla2.ncl <- make_formula_IOx(runsp,modtype="logn",dohbf=dohbf,addboat=addboat,addcl=F,nhbf=nhbf, lat5xqtr = F, lat5xyr = F)
  dat$.wtt <- mk_wts(dat,wttype="area")
  if(lu(dat$clust) > 1) {
    model1 <- glm(as.formula(fmla1),data=dat,family="gaussian", weights = .wtt, model = keepd)
    model2 <- glm(as.formula(fmla2),data=dat,family="gaussian", weights = .wtt, model = keepd)
  } else {
    model1 <- glm(as.formula(fmla1.ncl),data=dat,family="gaussian", weights = .wtt, model = keepd)
    model2 <- glm(as.formula(fmla2.ncl),data=dat,family="gaussian", weights = .wtt, model = keepd) }
  summarize_and_store_logCx(mod1=model1, mod2=model2, dat, fname, modlab, addcl, dohbf, keepd, mn, bcorr)
}

do_deltalogx <- function(dat,dohbf=F,addboat=F,addcl=addcl,nhbf=3,runsp,fname,modlab,keepd=TRUE, lat5xqtr = T, lat5xyr = T, bcorr=FALSE){
  datpos <- dat[dat[,runsp] >0,]
  fmla1.bin <- make_formula_IOx(runsp,modtype="deltabin",dohbf=dohbf,addboat=addboat,addcl=T,nhbf=nhbf, lat5xqtr = lat5xqtr, lat5xyr = lat5xyr)
  fmla1.pos <- make_formula_IOx(runsp,modtype="deltapos",dohbf=dohbf,addboat=addboat,addcl=T,nhbf=nhbf, lat5xqtr = lat5xqtr, lat5xyr = lat5xyr)
  fmla1.bin_ncl <- make_formula_IOx(runsp,modtype="deltabin",dohbf=dohbf,addboat=addboat,addcl=F,nhbf=nhbf, lat5xqtr = lat5xqtr, lat5xyr = lat5xyr)
  fmla1.pos_ncl <- make_formula_IOx(runsp,modtype="deltapos",dohbf=dohbf,addboat=addboat,addcl=F,nhbf=nhbf, lat5xqtr = lat5xqtr, lat5xyr = lat5xyr)
  fmla2.bin <- make_formula_IOx(runsp,modtype="deltabin",dohbf=dohbf,addboat=addboat,addcl=T,nhbf=nhbf, lat5xqtr = F, lat5xyr = F)
  fmla2.pos <- make_formula_IOx(runsp,modtype="deltapos",dohbf=dohbf,addboat=addboat,addcl=T,nhbf=nhbf, lat5xqtr = F, lat5xyr = F)
  fmla2.bin_ncl <- make_formula_IOx(runsp,modtype="deltabin",dohbf=dohbf,addboat=addboat,addcl=F,nhbf=nhbf, lat5xqtr = F, lat5xyr = F)
  fmla2.pos_ncl <- make_formula_IOx(runsp,modtype="deltapos",dohbf=dohbf,addboat=addboat,addcl=F,nhbf=nhbf, lat5xqtr = F, lat5xyr = F)
  datpos$.wtt <- mk_wts(datpos,wttype="area")
  if(lu(dat$clust) > 1) {
    modelbin1 <- glm(as.formula(fmla1.bin),    data=dat,family="binomial", model = keepd)
    modelbin2 <- glm(as.formula(fmla2.bin),    data=dat,family="binomial", model = keepd)
  } else {
    modelbin1 <- glm(as.formula(fmla1.bin_ncl),data=dat,family="binomial", model = keepd)
    modelbin2 <- glm(as.formula(fmla2.bin_ncl),data=dat,family="binomial", model = keepd)
  }
  if(lu(datpos$clust) > 1) {
    modelpos1 <- glm(as.formula(fmla1.pos),    family="gaussian",data=datpos,weights=.wtt, model = keepd)
    modelpos2 <- glm(as.formula(fmla2.pos),    family="gaussian",data=datpos,weights=.wtt, model = keepd)
  } else {
    modelpos1 <- glm(as.formula(fmla1.pos_ncl),family="gaussian",data=datpos,weights=.wtt, model = keepd)
    modelpos2 <- glm(as.formula(fmla2.pos_ncl),family="gaussian",data=datpos,weights=.wtt, model = keepd)
  }
  summarize_and_store_dlx(modb1=modelbin1,modb2=modelbin2,modp1=modelpos1,modp2=modelpos2,dat=dat,datpos=datpos,fname,modlab=modlab,dohbf=dohbf,addcl=addcl, keepd = keepd, bcorr)
}

do_deltalog <- function(dat,dohbf=F,addboat=F,addcl=addcl,nhbf=3,runsp,fname,modlab,keepd=TRUE){
  datpos <- dat[dat[,runsp] >0,]
  if(lu(dat$clust) > 1) {
    fmla.bin <- make_formula_IO(runsp,modtype="deltabin",dohbf=dohbf,addboat=addboat,addcl=T,nhbf=nhbf)
    fmla.pos <- make_formula_IO(runsp,modtype="deltapos",dohbf=dohbf,addboat=addboat,addcl=T,nhbf=nhbf)
    addcl <- TRUE
  } else {
    fmla.bin <- make_formula_IO(runsp,modtype="deltabin",dohbf=dohbf,addboat=addboat,addcl=F,nhbf=nhbf)
    fmla.pos <- make_formula_IO(runsp,modtype="deltapos",dohbf=dohbf,addboat=addboat,addcl=F,nhbf=nhbf)
    addcl <- FALSE
  }
  datpos$.wtt <- mk_wts(datpos,wttype="area")
  modelbin <- glm(as.formula(fmla.bin),    data=dat,family="binomial", model = keepd)
  modelpos <- glm(as.formula(fmla.pos),    family="gaussian",data=datpos,weights=.wtt, model = keepd)
  summarize_and_store_dl(modb=modelbin,modp=modelpos,dat=dat,datpos=datpos,
                         fname,modlab=modlab,dohbf=dohbf,addcl=addcl, keepd = keepd)
}


# load_jointdat <- function(flags=c("JP","KR","TW"),regs,regtype="regB2",bdir=paste0(basedir,"../"),allabs=allabs,endyr,noTW=T) {
#   jdat <- data.frame()
#   for(flag in flags) {
#     for(r in regs) {
#       load(paste0(bdir,flag,"/clustering/",paste(flag,regtype,r,sep="_"),".RData"))
#       dataset$flag <- flag
#       jdat <- rbind(jdat,dataset[,allabs])
#       rm(dataset)
#       }
#     }
#   jdat <- jdat[jdat$yrqtr < endyr,]
#   jdat$vessidx <- jdat$vessid
#   jdat$vessid <- paste0(jdat$flag,jdat$vessid)
#   jdat$vessid <- as.factor(jdat$vessid)
#   if(noTW) jdat <- jdat[jdat$yrqtr > 2005 | jdat$flag != "TW",]
#   return(jdat)
# }

