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
