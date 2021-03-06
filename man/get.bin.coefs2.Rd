% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_coef_functions.R
\name{get.bin.coefs2}
\alias{get.bin.coefs2}
\title{Old version of get.bin.coefs.}
\usage{
get.bin.coefs2(model, nyrs = NA, dat)
}
\arguments{
\item{model}{The model summary object from a binomial glm.}

\item{nyrs}{The number of yrqtr time variables to extract.}

\item{dat}{Data file used to produce glm.}
}
\value{
Yrqtr coefficients as proportions, with the same mean as the observed proportions.
}
\description{
The function extracts yrqtr coefficients from a binomial glm summary.
}
