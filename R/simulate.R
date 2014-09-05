simAdmissions <- function(start,ndays,dailyRate,todAdmission,stayTime){
  require(plyr)
  start = as.POSIXct(start)
  days = seq(start,by="1 day",length=ndays)
  newPatients = ldply(days,function(day){
    newCasesOn(day,dailyRate,todAdmission,stayTime)
  }
    )
  newPatients[order(newPatients$admission),]
  
}

newCasesOn <- function(day,dailyRate,todAdmission,stayTime){
  day = as.POSIXct(day)
  nNew = dailyRate(day)
  day = rep(day,nNew)
  newCases = data.frame(
    admission = day + todAdmission(day)
    )
  newCases$discharge = newCases$admission + stayTime(newCases)
  return(newCases)
}


fakeAdmissionsEtc <- function(dates,
                     Med=list(mean=30, phi=0, amp=10),
                     Sur=list(mean=20, phi=pi, amp=12),
                     wdayF, noise, id=1){
    fake = data.frame(id=id:(id+length(dates)-1))
    fake$date = dates
    y = yday(dates)
    t = 2*pi*(y/365)
    fake$MedActual = spois(t, Med$mean, Med$phi, Med$amp)
    fake$SurgActual = spois(t, Sur$mean, Sur$phi, Sur$amp)
    fake$MedPredict = fake$MedActual * runif(length(y),0.92,1.07)
    fake$SurgPredict = fake$SurgActual * runif(length(y),0.92,1.07)
    fake$MedTCI = zippois(length(y),.2,8)
    fake$SurgTCI = zippois(length(y),.4,4)
    fake
}

spois <- function(t, mean, phi, amp){
    amp = log(amp)
    z = exp(amp * sin(t-phi))

    y = rpois(length(z),z+mean)
    y
}

zippois <- function(n, p, mu){
    y = rep(0,n)
    z = runif(n) < p
    y[z]=rpois(sum(z),mu)
    y
}
    
##' Simulate dynamic model
##'
##' Simulate a dynamic intercept model with sin/cos periodicity and weekday effects.
##' @title Simulate dynamic model
##' @param b0 mean parameter
##' @param phi AR correlation parameter
##' @param w AR noise parameter
##' @param bsin periodic parameter 1
##' @param bcos periodic parameter 2
##' @param alpha length-6 vector of weekday effects (Monday-Sat)
##' @param days days for prediction on (consecutive)
##' @return simulation
##' @author Barry Rowlingson
##' @example
##'  days = as.Date("2012-01-01") + 1:(365*2)
##'  s = simdynam(2.4, .2, .2, .2,.02,c(1,2,3,3,2,1)/5,days)
##'  plot(s)
simdynam <- function(b0, phi, w, bsin, bcos, alpha, days){
    ndays = length(days)
    W = rnorm(ndays,0,w)
    B = rep(NA,ndays)
    B[1] = W[1]
    for(i in 2:ndays){
        B[i] = B[i-1]*phi + W[i]
    }
    B = B + b0
    wday = wday(days)
    yday = yday(days)
    p = 2*pi*yday/365
    alpha=c(0,alpha)
    logmu = B + bsin * sin(p) + bcos*cos(p) + alpha[wday]
    cases = exp(logmu)
    d = data.frame(date=days, cases=cases)
    d$wday = factor(wday(d$date, label=TRUE),ordered=FALSE)
    d$yday = yday(d$date)
    d
}
