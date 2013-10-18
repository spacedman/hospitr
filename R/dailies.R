##' Independent Identical Poisson rate generator
##'
##' @title Constant Poisson Rate generator
##' @param rate mean number of cases per day
##' @return a poisson rate generator function
##' @author Barry S Rowlingson
dailyPoisson <- function(rate){
  force(rate)
  foo = function(dates,...){
    n = length(dates)
    nAdmissions = rpois(n,rate)
    return(nAdmissions)
  }
  return(foo)
}

##' Seasonal Poisson generator
##'
##' @title Seasonal Poisson Rate generator
##' @param phi temporal offset
##' @param scale range of the mean rate
##' @param offset minimum rate 
##' @return Generator function with sinusoidal varying rate
##' @author Barry S Rowlingson
dailySeasonalPoisson <- function(phi, scale, offset){
  require(lubridate)
  force(phi)
  force(scale)
  foo = function(dates,...){
    f = pi*(yday(dates)-phi)/365
    sc = offset + scale * sin(f)^2
    return(rpois(length(sc),sc))
  }
  return(foo)
}

##' Uniform Time-of-Day generator
##'
##' @title Uniform time-of-day generator
##' @return Generator function returning fraction of a day
##' @author Barry S Rowlingson
todUniform <- function(){
  foo=function(admissions){
      ncases=nCases(admissions)
      return(ddays(runif(ncases)))
  }
  class(foo)=c("uniform","daily","function")
  return(foo)
}

print.uniform <- function(x,...){
    cat("Uniform daily time generator\n")
}


##' Generate daily admission times from a von Mises distribution
##'
##' Returns a generator that creates times sampled from a
##' von Mises distribution with given peak hour and spread kappa
##' @title 
##' @param hr hour (possibly fractional) of the daily peak
##' @param kappa concentration parameter 0=uniform
##' @return 
##' @author Barry S Rowlingson
todVonMises <- function(hr,kappa){
    force(hr)
    force(kappa)
    mu = (2*pi)*(hr/24)
    foo = function(admissions){
        ncases=nCases(admissions)
        rvm = ddays(as.numeric(rvonmises(ncases,circular(mu),kappa))/(2*pi))
        return(rvm)
    }
    class(foo)=c("vonmises","daily","function")
    return(foo)
}

print.vonmises <- function(x,...){
    e = environment(x)
    cat("Daily admission times via a Von Mises distribution\n")
    cat("Peak time (hour of day) = ",with(e,hr),"\n")
    cat("Concentration = ",with(e,kappa),"\n")
    invisible(0)
}

##' uniform time-of-day between given hours
##'
##' @title uniform time-of-day between hours
##' @param hlo early hour
##' @param hhi late hour - must be greater than hlo
##' @return a time-of-day generator.
##' @author Barry S Rowlingson
todBetween <- function(hlo,hhi){
    hlo=hlo/24
    hhi=hhi/24
    foo=function(admissions){
        ncases=nCases(admissions)
        rt = ddays(runif(ncases,hlo,hhi))
        return(rt)
    }
    class(foo)=c("between","daily","function")
    return(foo)
}
