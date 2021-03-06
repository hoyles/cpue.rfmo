% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_store_functions.R
\name{summarize_and_store}
\alias{summarize_and_store}
\title{Summarize and store GLM results.}
\usage{
summarize_and_store(
  mod,
  dat,
  fname,
  modlab,
  dohbf = dohbf,
  addcl = addcl,
  docl = docl,
  keepd = TRUE
)
}
\arguments{
\item{mod}{The model result file from the glm.}

\item{dat}{The dataset that was input to the glm.}

\item{fname}{File name used for saving outputs.}

\item{modlab}{Model label used in plots and for output filenames.}

\item{dohbf}{Include hbf if TRUE.}

\item{addcl}{Use clusters if TRUE.}

\item{docl}{Use clusters if TRUE. Deprecated.}

\item{keepd}{Remove data from model object unless TRUE.}
}
\description{
The function summarizes and stores the reuslts of a glm.
}
