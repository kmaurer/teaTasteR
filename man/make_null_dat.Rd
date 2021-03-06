% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_lineup_data.R
\name{make_null_dat}
\alias{make_null_dat}
\title{Function to permute y column in dataframe}
\usage{
make_null_dat(dat, xname = "x", yname = "y", bootstrap = FALSE,
  jitter = 0)
}
\arguments{
\item{dat}{dataframe}

\item{xname}{column name from dat for x-dimension in plots (defaults "x")}

\item{yname}{column name from dat for y-dimension in plots (defaults "y")}

\item{bootstrap}{logical indicator of if bootstrapping should be used following permutation}

\item{jitter}{amount of jitter to add to point locations - units of sds}
}
\value{
dataframe with columns x, permy and type
}
\description{
Function to permute y column in dataframe
}
\examples{
dat=data.frame(x=rnorm(5))
dat$y=dat$x+rnorm(5)
null_dat <- make_null_dat(dat)
null_dat <- make_null_dat(dat, bootstrap=TRUE)
null_dat <- make_null_dat(dat, bootstrap=TRUE, jitter=1)
}
