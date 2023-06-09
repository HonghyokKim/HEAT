% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createlag.R
\name{createlag}
\alias{createlag}
\title{createlag}
\usage{
createlag(data, vari, maxlag, cutoff = NULL, piecewise = FALSE)
}
\arguments{
\item{data}{dataset to be used.}

\item{vari}{the name of temperature variable to be used in the dataset (e.g., t_mean; not "t_mean").}

\item{maxlag}{the maximum lag period}

\item{cutoff}{the value to distinguish low temperatures and high temperatures, like optimal temperature. Default is NULL. Only to be used if piecewise=T}

\item{piecewise}{create piecewise variables for low temperatures and high temperatures. Default is FALSE}
}
\description{
This function create lag variables
}
\examples{
createlag(data=data,vari=t_mean,maxlag=5,cutoff=quantile(data$t_mean,0.70),piecewise=TRUE)
}
