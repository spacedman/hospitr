dailyAdmissionsSectionCounts <- function(epatients,conditions){
    epatients$Section = conditions[epatients$condition,"Section"]
    dailyAdmissionCounts(epatients)
}

dailyAdmissionCounts <- function(epatients){
    dates = as.character(seq(as.Date(min(epatients$admission)),as.Date(max(epatients$admission)),by="1 day"))
    admitDate = factor(as.character(as.Date(epatients$admission)),levels=dates)
    times = as.data.frame(table(Date=admitDate,Section=epatients$Section))
    times$Date = as.Date(as.character(times$Date))
    names(times)[3]="Count"
    return(times)
    
}

linearFit <- function(epatients,ndays,escale=3,crits=c(0.5,0.95,0.99)){
    npatients = nrow(epatients)
    epatients$weight = exp(as.numeric(-(max(epatients$Date)-epatients$Date)/escale))
    epatients$wday = wday_factor(epatients$Date)

    prediction=data.frame(Date = max(epatients$Date) + (0:ndays))
    prediction$wday=wday_factor(prediction$Date)
    
    predictSection = function(data,SectionName,crits){
        data = data[data$Section==SectionName,]
        m = glm(Count~Date+wday, weight=data$weight, data = data, family=poisson)
        pM = predict(m,newdata=prediction,type="response")

        predictionData = ldply(crits, function(crit){
            se=qnorm((crit+1)/2)
            d = glm_predict_interval(m, newdata=prediction, se=se)
            d$crit=crit
            d$Count[1]=data[nrow(data),"Count"]
            d$low[1]=d$Count[1]
            d$high[1]=d$Count[1]
            d
        })
        predictionData$Section=SectionName
        predictionData$Date=prediction$Date
        return(predictionData)
        
    }

    surgical = predictSection(epatients, "Surgical", crits = crits)
    medical = predictSection(epatients, "Medical", crits = crits)

    rbind(surgical,medical)
    
}

predictAdmissions <- function(m, ndays, crit=c(.5, .95, .99)){
    ses = qnorm((crit+1)/2)
    
    pM = predict(m,newdata=prediction, type="response", se.fit=TRUE)
}

plotPredictions <- function(ecount, efit, pre=28){
    lim = max(ecount$Date) - pre
    ecount = subset(ecount, Date >= lim)
    ggplot(data=efit,aes(x=Date))+
        geom_ribbon(aes(ymin=low,ymax=high),data=subset(efit,crit==0.99), fill="gray75", alpha=0.5) +
        geom_ribbon(aes(ymin=low,ymax=high),data=subset(efit,crit==0.95), fill="gray50", alpha=0.5) +
        geom_ribbon(aes(ymin=low,ymax=high),data=subset(efit,crit==0.5), fill="gray30", alpha=0.5) +
        geom_line(aes(y=Count),colour="black") +
        geom_line(data=ecount,aes(x=Date,y=Count))+
         facet_wrap(~Section,ncol=1,scales="free_y")

}



rChartPredictions <- function(ecount, predictions, pre=28){
    require(rCharts)
    ecount$Date=as.character(ecount$Date)
    m1 <- mPlot(x = "Date", y = "Count", type = "Line", data = ecount)
    m1 
                                        #    r1 <- rPlot(Count ~ Date | Section, data = ecount, type = "line")
#    r1
}
