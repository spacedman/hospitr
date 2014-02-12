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
    
