# January 25-29, 2019

# Data cleaning for JPN C&E LL data

# load op-level data from Satoh-san
load("~/IATTC/2019_CPUE/JP/Japan/Operational Level Data/20190126/CE/CE_20190126.RData")

save("comp.d",file="~/IATTC/2019_CPUE/JP/data/comp_d.RData")

load("C:\\Users\\clennert\\Documents\\R\\Carolina\\Longline_Feb 2019 wrkshp\\Japan\\conf\\data\\20190126\\comp.d.RData")

# save comp.d to separate workspace, then open that workspace (this way you will have only the data in the workspace)

# coerce "." to NA in numeric fields
comp.d$latitude<-as.numeric(as.character(comp.d$latitude))
comp.d$vessel_type<-as.numeric(as.character(comp.d$vessel_type))
comp.d$gear<-as.numeric(as.character(comp.d$gear))
comp.d$MAIN<-as.numeric(as.character(comp.d$MAIN))
comp.d$Branch<-as.numeric(as.character(comp.d$Branch))
comp.d$NHBF<-as.numeric(as.character(comp.d$NHBF))
comp.d$LBranch<-as.numeric(as.character(comp.d$LBranch))
comp.d$Lfloat<-as.numeric(as.character(comp.d$Lfloat))
comp.d$HOOK<-as.numeric(as.character(comp.d$HOOK))
comp.d$PTS95<-as.numeric(as.character(comp.d$PTS95))
comp.d$PTS85<-as.numeric(as.character(comp.d$PTS85))
comp.d$PTS75<-as.numeric(as.character(comp.d$PTS75))
comp.d$PTS65<-as.numeric(as.character(comp.d$PTS65))
comp.d$PTS50<-as.numeric(as.character(comp.d$PTS50))

# remove records with missing data for latitude, longitude and hooks
comp.d<-comp.d[!is.na(comp.d$latitude) & !is.na(comp.d$HOOK) & !is.na(comp.d$longitude),]

# Trim outliers in HOOK, following what Simon Hoyle does (SC7-SA-IP-01, Appendix 1)
comp.d<-comp.d[comp.d$HOOK>200 & comp.d$HOOK<10000,]

# Trim outliers for NHBF, following what Simon Hoyle does (SC7-SA-IP-01, Appendix 1)
comp.d<-comp.d[(comp.d$NHBF<26 & !is.na(comp.d$NHBF)) | is.na(comp.d$NHBF),]

# remove data for training vessels
comp.d<-comp.d[comp.d$vessel_type==1 | comp.d$vessel_type==4 |is.na(comp.d$vessel_type),]

# remove some target that were for shark or SWO (per SC7-SA-IP-01 Appendix 1)
comp.d<-comp.d[(comp.d$gear==3 & !is.na(comp.d$gear)) | is.na(comp.d$gear),]

# remove callsigns that are not valid; skip this part if you want to keep data prior to late 1970s
comp.d$CAL<-as.character(comp.d$CAL)
tmp.flg<-rep(T,length(comp.d$CAL))
tmp.flg[comp.d$CAL=="XXX   " | comp.d$CAL=="--    " | comp.d$CAL=="      "]<-F
comp.d<-comp.d[tmp.flg,]
rm(tmp.flg)

# reformat latitude and longitude ( I think longc=2 is east and longc=1 is west)
# Satoh-san says longitudes of 180 are not correct, so deleting any
comp.d<-comp.d[comp.d$longitude!=180,]
lat.1dgc<-comp.d$latitude
lon.1dgc<-comp.d$longitude
lat.1dgc[comp.d$latc==1]<-lat.1dgc[comp.d$latc==1]+0.5
lat.1dgc[comp.d$latc==2]<-((lat.1dgc[comp.d$latc==2]+1)*(-1))+0.5
lon.1dgc[comp.d$longc==2]<-(lon.1dgc[comp.d$longc==2]*(-1))-0.5
lon.1dgc[comp.d$longc==1]<-lon.1dgc[comp.d$longc==1]-360+0.5
comp.d$lat.1dgc<-lat.1dgc
comp.d$lon.1dgc<-lon.1dgc
rm(lat.1dgc)
rm(lon.1dgc)

save.image("C:\\Users\\clennert\\Documents\\R\\Carolina\\Longline_Feb 2019 wrkshp\\Japan\\conf\\data\\20190126\\jpn_raw.RData")

