library(readxl)

projdir <- "~/ICCAT/2018_Bigeye/"
covdir <- paste0(projdir, "coverage/")

setwd(covdir)

cov <- list()

## Load Japan
a <- as.data.frame(read_excel(paste0(covdir, "JPNLL_AO_coverage.xlsx"), sheet = "All Atl", range = "A9:G68", col_names = FALSE))
names(a) <- c("yr", "lb_hooks", "lb_betn",  "x1","x2", "tot_hooks","tot_betn")
a[,c("x1","x2")] <- NULL
a$tot_hooks <- a$tot_hooks * 1000
a$tot_betn <- a$tot_betn * 1000
a$cov_hooks <- a$lb_hooks/a$tot_hooks
a$cov_betn <- a$lb_betn/a$tot_betn

cov$JP$full <- a

lb <- as.data.frame(read_excel(paste0(covdir, "JPNLL_AO_coverage.xlsx"), sheet = "By area", range = "A4:D179", col_names = FALSE))
names(lb) <- c("yr", "area", "lb_hooks", "lb_betn")

tot <- as.data.frame(read_excel(paste0(covdir, "JPNLL_AO_coverage.xlsx"), sheet = "By area", range = "G3:J181", col_names = FALSE))
names(tot) <- c("area", "yr", "tot_hooks", "tot_betn")
tot$tot_hooks <- tot$tot_hooks * 1000

lb$cov_hooks <- lb$lb_hooks / tot$tot_hooks[match(paste(lb$area, lb$yr), paste(tot$area, tot$yr))]
lb$cov_betn <- lb$lb_betn / tot$tot_betn[match(paste(lb$area, lb$yr), paste(tot$area, tot$yr))]
cov$JP$area <- lb


## Load TW
a <- as.data.frame(read_excel(paste0(covdir, "Coverage rate for Taiwanese Atlantic Fleet.xlsx"), sheet = "coverage rate by year", range = "A1:C38", col_names = FALSE))
names(a) <- c("yr", "cov_betn", "cov_hooks")
cov$TW$full <- a
a <- as.data.frame(read_excel(paste0(covdir, "Coverage rate for Taiwanese Atlantic Fleet.xlsx"), sheet = "coverage rate by year area", range = "A1:D112", col_names = FALSE))
names(a) <- c("yr", "area", "cov_betn", "cov_hooks")
cov$TW$area <- a

## Load KR
a <- as.data.frame(read_excel(paste0(covdir, "KR BET coverage.xlsx"), range = "A4:D41", col_names = FALSE))
names(a) <- c("yr", "lb_betn", "tot_betn", "cov_betn")
a$cov_betn <- a$cov_betn / 100
cov$KR$full <- a

# Load US
a <- read.csv(paste0(covdir,"USLLcoverage.csv"))
a$cov_betn <- a$sumLB / a$TASK1
# usco <- a[,4]
# usco[is.na(usco)] <- 0
# a[,4] <- usco
names(a)[c(1)] <- c("yr")
cov$US$full <- a

save(cov, file = "coverage.RData")

# Plot full
windows(width=10, height = 7)
with(cov$JP$full, plot(yr, cov_betn, type = "l", lwd=2, ylim = c(0, 1.5), xlab = "Year", ylab = "Coverage"))
with(cov$TW$full, lines(yr, cov_betn, type = "l", lwd=2, lty = 2, col = 2))
with(cov$TW$full[cov$TW$full$yr >= 2005,], lines(yr, cov_betn, type = "l", lwd=2, lty = 1, col = 2))
with(cov$KR$full, lines(yr, cov_betn, type = "l", lwd=2, col = 3, lty = 1))
#with(cov$US$full, lines(yr, cov_betn, type = "l", lwd=2, col = 4, lty = 2))
with(cov$US$full[cov$US$full$yr > 1994,], lines(yr, cov_betn, type = "l", lwd=2, col = 4, lty = 1))
legend("topleft", legend = c("JP","TW","KR", "US"), lty = c(1,1,1,1), lwd = 2, col = 1:4, horiz = TRUE)
abline(h = 1, lty = 2)
savePlot("Coverage", type = "png")

# Plot by area
windows(10,10); par(mfrow = c(2,2), mar = c(3,3,3,1))
for (r in 1:3) {
  with(cov$JP$area[cov$JP$area$area==r,], plot(yr, cov_betn, type = "l", ylim = c(0, 1.5), main = paste("Region", r)))
  with(cov$TW$area[cov$TW$area$area==r,], lines(yr, cov_betn, type = "l", col = 2))
  if (r == 2) {
    with(cov$KR$full, lines(yr, cov_betn, type = "l", col = 3))
    legend("topleft", legend = c("JP","TW","KR", "US"), lty = 1, col = 1:4, horiz = TRUE)
  }
}
savePlot("Coverage by region", type = "png")



