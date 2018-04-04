#' Diagnostic plots for Gaussian glm.
#'
#' The function produces 2 Gaussian diagnostic plots: a frequency histogram and a QQ plot.
#' @param res Fitted model for diagnostics.
#' @param ti Titles for the plots.
#'
plotdiags <- function(res, ti = "") {
  hist(res, nclass = 200, freq = F, xlab = "Residuals", main = ti)
  lines((-30:30)/10, dnorm((-30:30)/10, sd = sd(res)), col = 2)
  sdres <- res/sd(res)
  qqDist(sdres, add.median = T)
}

#' QQ plot for Gaussian glm.
#'
#' The function produces a QQ plot for a GLM.
#' @param x Residuals or standardized residuals.
#' @param standardize Standardize the residuals if TRUE.
#' @param add.median Add median line if TRUE.
#'
qqDist <- function(x, standardise = F, add.median = F, ...) {
  n <- length(x)
  seq.length <- min(1000, n)
  if (standardise) {
    SEQ <- seq(1, 2 * n + 1, length = seq.length)/2
    U <- qnorm(qbeta(0.975, SEQ, rev(SEQ)))
    L <- qnorm(qbeta(0.025, SEQ, rev(SEQ)))
    if (add.median)
      M <- qnorm(qbeta(0.5, SEQ, rev(SEQ)))
  } else {
    SD <- sqrt(var(x) * (n + 1)/n)
    SEQ <- seq(1, 2 * n + 1, length = seq.length)/2
    U <- mean(x) + SD * qt(qbeta(0.975, SEQ, rev(SEQ)), n - 1)
    L <- mean(x) + SD * qt(qbeta(0.025, SEQ, rev(SEQ)), n - 1)
    if (add.median)
      M <- mean(x) + SD * qt(qbeta(0.5, SEQ, rev(SEQ)), n - 1)
  }
  X <- qnorm((SEQ - 0.25)/(n + 0.5))
  qqnorm(x, main = "", ...)
  lines(X, U, type = "l", col = 2)
  lines(X, L, type = "l", col = 2)
  if (add.median)
    lines(X, M, type = "l", col = 2)
  invisible()
}

