##' mean absolute percentage errors
##'
##' compute the mape
##' @title mape
##' @param forecast forecasted value
##' @param actual actual value
##' @return the MAPE
##' @author Barry Rowlingson
mape <- function(forecast, actual){
    100 * mean(abs(actual-forecast)/actual)
}


##' root mean square prediction error
rmse <- function(forecast, actual){
    sqrt(mean((forecast-actual)^2))
}


compareAccuracy <- function(f1, f2, actual, measure=rmse){
    a1 = measure(f1, actual)
    a2 = measure(f2, actual)
    c(a1, a2)
}


compareModelHospital <- function(modelForecast, hospital, measure=rmse){
    
}
