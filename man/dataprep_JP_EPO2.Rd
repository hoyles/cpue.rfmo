% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataprep_functions.R
\name{dataprep_JP_EPO2}
\alias{dataprep_JP_EPO2}
\title{Prepare Japanese longline data for the eastern and western Pacific, IATTC 2019.}
\usage{
dataprep_JP_EPO2(
  dat,
  splist = c("alb", "bet", "yft", "swo", "mls", "bum", "blm", "sas", "sha", "sfa", "ssp")
)
}
\arguments{
\item{dat}{Input dataset}

\item{splist}{List of species codes}
}
\value{
Modified dataset.
}
\description{
The function prepares Japanese longline data for EPO (mainly) but also some WCPO analyses. Data are already partly preprared.
}
