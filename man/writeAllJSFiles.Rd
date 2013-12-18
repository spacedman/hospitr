\name{writeAllJSFiles}
\alias{writeAllJSFiles}
\title{Write All Web Output Files}
\usage{
  writeAllJSFiles(emerg, daysAhead = 45, daysBefore = 7,
    site = "./Web/Cactus/Site/")
}
\arguments{
  \item{emerg}{data frame of admission (Date) and Section
  (Medical/Surgical)}

  \item{daysAhead}{number of days to predict ahead}

  \item{daysBefore}{number of data points to include
  before}

  \item{site}{path to site root}
}
\value{
  nothing much
}
\description{
  write all files for web (not just JS!)
}
\details{
  this function writes the JS and other stuff for the web
  site

  Process:

  load("./Data/25-7-2013/emerg.RData")
  emerg=subset(emerg,AEOBS=="") edata =
  data.frame(admission=emerg$Adm.Date,
  Section=emerg$med.surg..based.on.specialty.)
  writeAllJSFiles(edata)
}
\author{
  Barry S Rowlingson
}

