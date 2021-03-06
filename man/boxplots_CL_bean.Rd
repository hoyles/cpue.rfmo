% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster_plot_functions.R
\name{boxplots_CL_bean}
\alias{boxplots_CL_bean}
\title{Beanplots of set characteristics by cluster.}
\usage{
boxplots_CL_bean(
  dat,
  cl = "kmeans",
  ti = "",
  outL = T,
  dohbf = T,
  lat5 = F,
  regtype = regtype,
  r = r,
  saveplot = TRUE,
  savedir = "."
)
}
\arguments{
\item{dat}{Input dataset}

\item{cl}{Cluster type to select from dataset.}

\item{ti}{Plot main title.}

\item{outL}{Deprecated, not used.}

\item{dohbf}{Boxplot HBF by cluster.}

\item{lat5}{Use 'lat' variable if FALSE, otherwise 'lat5'.}

\item{regtype}{Deprecated, not used.}

\item{r}{Deprecated, not used.}

\item{saveplot}{Should be set to FALSE when working in RMarkdown.}

\item{savedir}{The directory to save the plot.}
}
\description{
This function plots beanplots of set characteristics by cluster.
}
