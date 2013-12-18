\name{dailySeasonalPoisson}
\alias{dailySeasonalPoisson}
\title{Seasonal Poisson Rate generator}
\usage{
  dailySeasonalPoisson(phi, scale, offset)
}
\arguments{
  \item{phi}{temporal offset}

  \item{scale}{range of the mean rate}

  \item{offset}{minimum rate}
}
\value{
  Generator function with sinusoidal varying rate
}
\description{
  Seasonal Poisson generator
}
\author{
  Barry S Rowlingson
}

