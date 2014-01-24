###  some naive predictor generators

##' constant predictions
##'
##' predict the same number always.
##' @title constant prediction
##' @param c the prediction value
##' @return a predictor function, a function of a date vector
##' @author Barry S Rowlingson
constant <- function(c){
    force(c)
    f = function(dates){
        return(rep(c,length(dates)))
    }
    f
}

meanValue <- function(adms){
    m = mean(adms)
    constant(m)
}

weekDayAverage <- function(dates,adms, average=mean){
    wdays = wday(dates)
    wdayM = tapply(adms, wdays, average)
    wdayMeans = rep(NA,7)
    wdayMeans[as.numeric(names(wdayM))]=wdayM
    f = function(dates){
        wdays = wday(dates)
        p = wdayMeans[wdays]
        p
    }
    f
}

yearDayAverage <- function(dates,adms, average=mean){
    ydays = yday(dates)
    ydayM = tapply(adms, ydays, average)
    ydayMeans = rep(NA, 366)
    ydayMeans[as.numeric(names(ydayM))] = ydayM
    f = function(dates){
        ydayMeans[yday(dates)]
    }
    f
}

weekYearModel <- function(dates, adms){
    data = data.frame(
        adms = adms,
        yearday = factor(yday(dates)),
        weekday = factor(wday(dates))
        )
    model = glm(adms ~ yearday + weekday -1 , data=data, family="poisson")
    f = function(dates){
        newData = data.frame(
            yearday = factor(yday(dates)),
            weekday = factor(wday(dates))
            )
        predict(model,newdata=newData, type="response")
    }
    f
}

sincosModel <- function(dates,adms){
    t = 2*pi*yday(dates)/366
    d = data.frame(
        adms = adms,
        cosT = cos(t),
        sinT = sin(t),
        wday = factor(wday(dates))
        )
    model = glm(adms ~ cosT + sinT + wday, data=d,family="poisson")
    f = function(dates){
        t = 2*pi*yday(dates)/366
        newData = data.frame(
            cosT=cos(t),
            sinT=sin(t),
            wday = factor(wday(dates))
            )
        predict(model,newdata=newData, type="response")
    }
    f
}

ABSerror <- function(predictor, dates, actual){
    estimated = predictor(dates)
    estimated - actual
}

RMSerror <- function(predictor, dates, actual){
    estimated = predictor(dates)
    return( sqrt(mean((actual-estimated)^2)) )
}

    
