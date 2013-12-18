\name{todVonMises}
\alias{todVonMises}
\title{VonMises dist Admission Times}
\usage{
  todVonMises(hr, kappa)
}
\arguments{
  \item{hr}{hour (possibly fractional) of the daily peak}

  \item{kappa}{concentration parameter 0=uniform}
}
\value{
  generator
}
\description{
  Generate daily admission times from a von Mises
  distribution
}
\details{
  Returns a generator that creates times sampled from a von
  Mises distribution with given peak hour and spread kappa
}
\author{
  Barry S Rowlingson
}

