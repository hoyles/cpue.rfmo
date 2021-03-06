% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_coef_functions.R
\name{get.bin.coefs.summ}
\alias{get.bin.coefs.summ}
\title{Extract yrqtr coefficients from a binomial glm summary object.}
\usage{
get.bin.coefs.summ(modelsumm, nyrs, dat = NA)
}
\arguments{
\item{modelsumm}{The model summary object from a binomial glm.}

\item{nyrs}{The number of yrqtr time variables to extract.}

\item{dat}{Deprecated, not used.}
}
\value{
Yrqtr coefficients as proportions, with the same mean as the observed proportions.
}
\description{
The function extracts yrqtr coefficients from a binomial glm summary object.
}
