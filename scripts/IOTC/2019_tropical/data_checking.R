# Script to check causes of CPUE trends in R4

projdir <- "~/IOTC/2019_CPUE_ALB/"
Rdir <- paste0(projdir, "Rfiles/")

jpdir <- paste0(projdir, "JP/")
krdir <- paste0(projdir, "KR/")
sydir <- paste0(projdir, "SY/")
twdir <- paste0(projdir, "TW/")
jointdir <- paste0(projdir, "joint/")

setwd(jointdir)

load(paste0(jpdir,"clustering/","JP_regA4_4.RData"))
JP <- dataset
load(paste0(twdir,"clustering/","TW_regA4_4.RData"))
TW <- dataset
load(paste0(krdir,"clustering/","KR_regA4_4.RData"))
KR <- dataset

windows(15, 12); par(mfrow = c(2,2))
for(dd1 in c("JP", "TW", "KR")){
  dd <- get(dd1)
  a <- with(dd, tapply(alb, list(hcltrp, op_yr), mean, na.rm = TRUE))
  plot(1952:2017, 1952:2017, type = "n", ylim = c(0, max(a, na.rm = TRUE)), ylab = "ALB", main = dd1)
  dimx <- dim(a)[1]
  for (i in 1:dimx) points(as.numeric(colnames(a)), a[i,], col = i, pch = i)
}
legend("topleft", legend = c("cl1", "cl2", "cl3", "cl4", "cl5"), pch = 1:5, col = 1:5)
savePlot("CPUE by cluster and fleet.png", type="png")

jp_splist <- c("alb","bet","yft","swo","mls","bum","blm","bft","sbt","sas","shk")

windows(15, 12); par(mfrow = c(3,5), mar =c(3,2,2,.5))
for(dd1 in c("JP", "TW", "KR")){
  dd <- get(dd1)
  for (cl in sort(unique(dd$hcltrp))) {
    a2 <- dd[dd$hcltrp==cl & dd$op_yr > 1978,]
    a <- aggregate(cbind(alb,bet,sbt,swo,yft) ~ op_yr, a2, mean, na.rm = TRUE)
    a[,-1] <- a[,-1] / apply(a[,-1], 1, sum)
    barplot(as.matrix(t(a)[-1,]), names.arg=a[,1], main = paste(dd1, cl), col = rainbow(5))
  }
}
plot(1:3, 1:3, xlb="", ylab = "", type = "n")
legend("topleft", legend = c("alb","bet","sbt","swo","yft"), col = rainbow(6), fill=rainbow(5))
savePlot("Spcomp by cluster and fleet.png", type="png")

windows(15, 12); par(mfrow = c(3,5), mar =c(3,2,2,.5))
for(dd1 in c("JP", "TW", "KR")){
  dd <- get(dd1)
  for (cl in sort(unique(dd$hcltrp))) {
    a2 <- dd[dd$hcltrp==cl & dd$op_yr > 1978,]
    a <- aggregate(cbind(alb,bet,sbt,swo,yft) ~ op_yr, a2, mean, na.rm = TRUE)
    barplot(as.matrix(t(a)[-1,]), names.arg=a[,1], main = paste(dd1, cl), col = rainbow(5))
  }
}
plot(1:3, 1:3, xlb="", ylab = "", type = "n")
legend("topleft", legend = c("alb","bet","sbt","swo","yft"), col = rainbow(6), fill=rainbow(5))
savePlot("CPUE by species cluster fleet.png", type="png")

windows(15, 12); par(mfrow = c(3,5), mar =c(3,2,2,.5))
for(dd1 in c("JP", "TW", "KR")){
  dd <- get(dd1)
  for (cl in sort(unique(dd$hcltrp))) {
    a2 <- dd[dd$hcltrp==cl & dd$op_yr > 1978 & dd$lat5 < -35,]
    a <- aggregate(cbind(alb,bet,sbt,swo,yft) ~ op_yr, a2, mean, na.rm = TRUE)
    barplot(as.matrix(t(a)[-1,]), names.arg=a[,1], main = paste(dd1, cl), col = rainbow(5))
  }
}
plot(1:3, 1:3, xlb="", ylab = "", type = "n")
legend("topleft", legend = c("alb","bet","sbt","swo","yft"), col = rainbow(6), fill=rainbow(5))
savePlot("CPUE by species cluster fleet south.png", type="png")

is.even <- function(x) x %% 2 == 0
is.odd <- function(x) x %% 2 != 0
is.five <- function(x) x %% 5 == 0

windows(15, 12); par(mfrow = c(4,4), mar =c(3,2,2,.5))
dd <- get("JP")
for(dd1 in c(-27.5, -32.5, -37.5)){
  for (cl in sort(unique(dd$hcltrp))) {
    a2 <- dd[dd$hcltrp==cl & dd$op_yr > 1978 & dd$lat5 == dd1,]
    a <- aggregate(cbind(alb,bet,sbt,swo,yft) ~ op_yr, a2, mean, na.rm = TRUE)
    nm <- as.character(a[,1])
    nm[is.five(a[,1])==FALSE] <- ""
    barplot(as.matrix(t(a)[-1,]), names.arg=nm, main = paste(dd1, "JP", cl), col = rainbow(5), las = 3)
  }
}
plot(1:3, 1:3, xlb="", ylab = "", type = "n")
legend("topleft", legend = c("alb","bet","sbt","swo","yft"), col = rainbow(6), fill=rainbow(5))
savePlot("CPUE by species cluster latitude JP.png", type="png")

windows(15, 12); par(mfrow = c(4,5), mar =c(3,2,2,.5))
dd <- get("TW")
for(dd1 in c(-27.5, -32.5, -37.5)){
  for (cl in sort(unique(dd$hcltrp))) {
    a2 <- dd[dd$hcltrp==cl & dd$op_yr > 1978 & dd$lat5 == dd1,]
    a <- aggregate(cbind(alb,bet,sbt,swo+mls+blm+bum,ot2,yft) ~ op_yr, a2, mean, na.rm = TRUE)
    nm <- as.character(a[,1])
    nm[is.five(a[,1])==FALSE] <- ""
    barplot(as.matrix(t(a)[-1,]), names.arg=nm, main = paste(dd1, "TW", cl), col = rainbow(8), las = 3)
  }
}
plot(1:3, 1:3, xlb="", ylab = "", type = "n")
legend("topleft", legend = c("alb","bet","sbt","billf","ot2","yft"), col = rainbow(7), fill=rainbow(8))
savePlot("CPUE by species cluster latitude TW.png", type="png")

