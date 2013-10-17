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


