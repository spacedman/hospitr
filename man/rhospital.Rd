\name{rhospital}
\alias{rhospital}
\title{Random Hospital Generator}
\usage{
  rhospital(name, nMed, nSurg, medicalSize = c(10, 30),
    surgicalSize = c(10, 30))
}
\arguments{
  \item{name}{Hospital Name}

  \item{nMed}{Number of medical wards}

  \item{nSurg}{Number of surgical wards}

  \item{medicalSize}{vector of length 2 of min/max medical
  ward size}

  \item{surgicalSize}{vector of length 2 of min/max
  surgical ward size}
}
\value{
  A hospital object
}
\description{
  Make a random hospital
}
\author{
  Barry S Rowlingson
}

