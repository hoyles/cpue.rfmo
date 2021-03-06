% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_store_functions.R
\name{summarize_and_store_dl}
\alias{summarize_and_store_dl}
\title{Summarize and store GLM results for delta lognormal.}
\usage{
summarize_and_store_dl(
  modb,
  modp,
  dat,
  datpos,
  fname,
  modlab,
  dohbf = dohbf,
  addcl = addcl,
  keepd = TRUE
)
}
\arguments{
\item{modb}{The model result file from the binomial glm.}

\item{modp}{The model result file from the lognormal positive glm.}

\item{dat}{The dataset that was input to the binomial glm.}

\item{datpos}{The dataset that was input to the lognormal positive glm.}

\item{fname}{File name used for saving outputs.}

\item{modlab}{Model label used in plots and for output filenames.}

\item{dohbf}{Include hbf if TRUE.}

\item{addcl}{Use clusters if TRUE.}

\item{keepd}{Remove data from model object unless TRUE.}
}
\description{
The function summarizes and stores the reuslts of both components of a delta lognormal standardization
}
