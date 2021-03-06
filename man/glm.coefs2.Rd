% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_coef_functions.R
\name{glm.coefs2}
\alias{glm.coefs2}
\title{Predict CPUE from a lognormal positive glm object.}
\usage{
glm.coefs2(model, dat, parm = "yrqtr")
}
\arguments{
\item{model}{The fitted model object.}

\item{dat}{Dataset used in the glm.}

\item{parm}{Variable to predict for, defaults to 'yrqtr'.}
}
\value{
Predicted catch rates by yrqtr with CIs.
}
\description{
The function predicts CPUE lognormal positive glm object.
}
