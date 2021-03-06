% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/samp_strat_wts_functions.R
\name{samp_strat_data}
\alias{samp_strat_data}
\title{Randomly sample dataset stratified by grid and yearqtr.}
\usage{
samp_strat_data(dd, nsamp)
}
\arguments{
\item{dd}{Input dataset.}

\item{nsamp}{The number of samples needed.}
}
\value{
Modified dataset.
}
\description{
The function reduces the size of the dataset per stratum to approximately nsamp.
}
