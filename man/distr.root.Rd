% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distr.root.R
\name{distr.root}
\alias{distr.root}
\title{Calculate the objective function values}
\usage{
distr.root(d, y, c, my_env)
}
\arguments{
\item{d}{A numeric: distance or range of data}

\item{y}{A numeric: stratum width}

\item{c}{A numeric: stratum cost}

\item{my_env}{My environment my_env contains the constants and outputs
from various calculations carried out by other key functions}
}
\value{
\code{} returns the value of the objective function
}
\description{
This function is called within other important functions in the package
to calculate the objective function values at systematic incremental
progressions of stratum width and range of the data
}
\author{
Karuna Reddy <karuna.reddy@usp.ac.fj>\cr
MGM Khan <khan_mg@usp.ac.fj>
}
