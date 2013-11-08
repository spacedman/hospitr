##' Generate some sample cases
##'
##' Sample case generation
##' @title Generate some initial cases
##' @param n number of cases
##' @param day start date
##' @param dayStay stay duration
##' @return dataframe with admission and discharge columns
##' @author Barry S Rowlingson
initCases <- function(n,day=as.Date("2010-01-01"),dayStay=2){
  meanStay = dayStay * 24 * 60 * 60
  
  admission = as.POSIXct(day)+runif(n,0,24*60*60)
  discharge = as.POSIXct(admission + rexp(length(admission),1/meanStay))
  cases = data.frame(admission=admission, discharge=discharge)
  return(cases)
}

##' how many cases
##'
##' return number of cases in a set of patient records
##' @title Number of Cases
##' @param cases case record data frame
##' @return number of cases
##' @author Barry S Rowlingson
nCases <- function(cases){
  return(nrow(cases))
}

rAdmissions <- function(dayAdmissions,
                        conditions=conditions,
                        todFunction,
                        stayFunction
                        ){
    newPatients = mdply(dayAdmissions,function(day,nCases){
        rDayAdmissions(nCases,day,conditions,todFunction,stayFunction)
    }
        )
    newPatients[order(newPatients$admission),-c(1,2)]
}

rDayAdmissions <- function(n,day=as.Date("2010-01-01"),
                        conditions=conditions,
                        todFunction,
                        stayFunction
                        ){
    nConditions = nrow(conditions)
    s = sample(nConditions,n,prob=conditions$F,replace=TRUE)
    cases=data.frame(admission=as.POSIXct(day),condition=conditions[s,"Name"])
    tod = todFunction(cases)
    stay = stayFunction(cases)
    cases$admission = cases$admission + tod
    cases$discharge = cases$admission + stay
    return(cases)
}

rTod <- function(admissions,todFunction){
    return(todFunction(admissions))
}

rStays <- function(admissions,stayFunction){
    return(stayFunction(admissions))
}
    
occupancyAt <- function(cases,datetime){
    return(cases$admission<=datetime & cases$discharge >= datetime)
}


rElectivePoisson <- function(lambda){
    force(lambda)
    rF = function(dates){
        return(rpois(length(dates),lambda))
    }
    class(rF) <- c("function","electivePoisson")
    return(rF)
}

rElectiveExpPoisson <- function(lambda, decay){
    force(lambda)
    force(decay)
    rF = function(dates){
        delta = dates-dates[1]
        lambdaT = lambda * exp(-as.numeric(delta)/decay)
        return(rpois(length(dates),lambdaT))
    }
    class(rF)=c("function","electiveDecaying")
    return(rF)
}

rZeroProduct <- function(pzero, generator){
    force(pzero)
    force(generator)
    rF = function(dates){
        d = generator(dates)
        u = runif(length(dates))
        d[u<=pzero]=0
        return(d)
    }
    class(rF)=c("function","electiveZeroes")
    return(rF)
}

dailyRates <- function(s7){
    ## s7 is Sun/M/T/W/T/F/Sat
    force(s7)
    F = function(dates){
        return(s7[wday(dates)])
    }
    return(F)
}

dailyComposition <- function(daily, generator){
    force(daily)
    force(generator)
    F = function(dates){
        return(round(daily(dates)*generator(dates)))
    }
    return(F)
}

        
