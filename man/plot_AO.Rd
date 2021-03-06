% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_functions.R
\name{plot_AO}
\alias{plot_AO}
\title{Map of the Atlantic Ocean.}
\usage{
plot_AO(
  plot_title = "",
  uselims = c(-100, 30, -50, 60),
  sp = "BET",
  newm = T,
  lwdm = 3,
  axes = T,
  tcol = "red"
)
}
\arguments{
\item{plot_title}{Plot title.}

\item{uselims}{Latitudes and Longitudes for the edges of the map.}

\item{sp}{Region type code for the region boundaries.}

\item{newm}{If TRUE, create a new plot, otherwise add boundaries etc to existing plot.}

\item{lwdm}{Line width for boundaries.}

\item{axes}{If TRUE, create x and y axes.}

\item{tcol}{Text colour.}
}
\description{
Function to make a map of the Atlantic Ocean, with regional boundaries.
}
