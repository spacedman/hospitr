soccupancy <- function(cases, time){
  sum(cases$admission<time & cases$discharge>time)
}

occupancy=Vectorize(soccupancy,"time")


maxDaily <- function(cases, days, tail=FALSE){
    if(missing(days)){
        days = as.POSIXct(as.Date(seq(min(cases$admission),max(cases$admission),by="1 day")))
    }
    daysF = as.character(as.Date(days))
    lateDays=c()
    if(tail){
        lateDays = cases$discharge[cases$discharge > max(cases$admission)]
    }
    tPoints = c(days, cases$admission, lateDays)
    occ = occupancy(cases,tPoints)
    dayIn = factor(days,levels=days)
    maxOcc = tapply(occ,as.Date(tPoints),FUN=max)
    maxOcc[is.na(maxOcc)]=NA
    data.frame(day=as.Date(rownames(maxOcc)),maxOccupancy=maxOcc)
}

plotDayMaxes <- function(cases,maxes,tlim){
    if(missing(tlim)){
        tlim=range(maxes$day)
    }
    plot(as.POSIXct(maxes$day),maxes$maxOccupancy,type="s",xlim=as.POSIXct(tlim))
    points(cases$admission,occupancy(cases,cases$admission),col=as.Date(cases$admission),pch=19)
    lines(as.POSIXct(maxes$day),maxes$maxOccupancy,type="s",xlim=as.POSIXct(tlim))

}
