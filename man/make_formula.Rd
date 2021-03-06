% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formula_functions.R
\name{make_formula}
\alias{make_formula}
\title{Create formula object to use in the glm.}
\usage{
make_formula(
  runsp,
  modtype,
  addboat,
  splitboat = F,
  addmain = F,
  addbranch = F,
  addother = F,
  addalb = F
)
}
\arguments{
\item{runsp}{Species of interest.}

\item{modtype}{logn (lognormal constant), deltabin(binomial), deltapos (lognormal positive), or qp (quasi-Poisson).}

\item{addboat}{Include vess if TRUE.}

\item{splitboat}{Include splitvess if TRUE.}

\item{addmain}{Include mainline if TRUE.}

\item{addbranch}{Include branchline if TRUE.}

\item{addother}{Include other if TRUE.}

\item{addalb}{include alb_cat if TRUE.}
}
\value{
Modified dataset.
}
\description{
This function creates a formula object for the glm, based on the input parameters. Developed for Japanese data in the WCPO.
}
