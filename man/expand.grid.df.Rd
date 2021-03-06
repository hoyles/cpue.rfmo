% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{expand.grid.df}
\alias{expand.grid.df}
\title{Expand a grid across a data frame.}
\usage{
expand.grid.df(...)
}
\arguments{
\item{...}{list of data frames (first varies fastest)}
}
\value{
Expanded grid.
}
\description{
Creates new data frame containing all combination of rows from data.frames in ...
Copied from reshape package
https://www.rdocumentation.org/packages/reshape/versions/0.8.7/topics/expand.grid.df
}
