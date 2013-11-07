##' Constant stay-time generator
##'
##' @title Constant stay-time generator
##' @param meanStay mean stay time in days
##' @return a stay-time generator
##' @author Barry S Rowlingson
stayIIDExponential <- function(meanStay){
  force(meanStay)
  foo = function(cases,...){
    s = 24*60*60*rexp(nCases(cases),rate=1/meanStay)
    return(s)
  }
  return(foo)
}

##' Stay-time function generator for conditions
##'
##' @title Return a stay-time function generator
##' @param conditions data frame of condition info
##' @return stay-time function generator
##' @author Barry S Rowlingson
stayCondition <- function(conditions){

    rownames(conditions)=conditions$Name
    
    foo=function(cases){
        s = conditions$Stay[cases$condition]
        return(24*60*60*rexp(length(s),1/s))
    }
    return(foo)
}
