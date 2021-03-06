% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster_functions.R
\name{run_clustercode_byreg}
\alias{run_clustercode_byreg}
\title{Run clustering code across all regions and save the results.}
\usage{
run_clustercode_byreg(
  indat,
  reg_struc,
  allsp = parent.frame()$allsp,
  allabs = parent.frame()$allabs,
  ncl = "lst",
  plotPCA = F,
  clustid = "tripidmon",
  allclust = F,
  flag = parent.frame()$flag,
  dohbf = TRUE,
  cvnames = parent.frame()$cvn,
  rgl,
  saveplot = TRUE,
  basedir = getwd()
)
}
\arguments{
\item{indat}{The input dataset.}

\item{reg_struc}{The name of the regional structure to be used.}

\item{allsp}{Vector of variable names to include in the species composition clustering.}

\item{allabs}{Vector of variable names to pass to the clust_PCA_run function.}

\item{ncl}{Vector of number of clusters to identify by region. Default of 'lst' uses the ncl values specified in reg_struc.}

\item{plotPCA}{Generate PCA plots if TRUE.}

\item{clustid}{Name of variable to aggregate across for trip-level clustering.}

\item{allclust}{Generate plots for all clustering methods if TRUE.}

\item{flag}{Fleet code to use in figure titles.}

\item{dohbf}{Include HBF in cluster plots if TRUE.}

\item{cvnames}{Names of covariates to return with dataset.}

\item{rgl}{A list specifying, for each regional structure, the regions to run and how many clusters to select in each region.}

\item{saveplot}{Should be set to FALSE when working in RMarkdown.}

\item{basedir}{The folder in which to save the dataset.}
}
\value{
Nothing is returned but each dataset is saved.
}
\description{
Run clustering code across all regions and save the results.
}
