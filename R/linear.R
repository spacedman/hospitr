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

linearFit <- function(epatients,ndays,escale=14){
    npatients = nrow(epatients)
    epatients$weight = exp(as.numeric(-(max(epatients$Date)-epatients$Date)/escale))
    epatients$wday = wday(epatients$Date, label=TRUE)

    prediction=data.frame(Date = max(epatients$Date) + (1:ndays))
    prediction$wday=wday(prediction$Date, label=TRUE)
    
    predictSection = function(data,SectionName){
        data = data[data$Section==SectionName,]
        m = glm(Count~Date+wday, weight=data$weight, data = data, family=poisson)
        pM = predict(m,newdata=prediction, type="response", se.fit=TRUE)
        p = prediction
        p$Count = pM$fit
        p$se = pM$se
        p$Section = SectionName
        p$wday = NULL
        p
    }

    surgical = predictSection(epatients, "Surgical")
    medical = predictSection(epatients, "Medical")

    rbind(surgical,medical)
    
}

plotPredictions <- function(epatients, predictions, pre=28){
    lim = max(epatients$Date) - pre
    epatients = subset(epatients, Date > lim)
    epatients$se = 0
    all = rbind(epatients, predictions)
    ggplot(data=all, aes(x = Date, y = Count, group = Section)) +
        geom_ribbon(aes(x = Date, ymin = pmax(Count - 2.60 * se, 0), ymax=Count+2.60*se ), fill="gray75", alpha=0.5) + 
        geom_ribbon(aes(x=Date, ymin=pmax(Count-1.96*se, 0), ymax=Count+1.96*se ), fill="gray50", alpha=0.5) + 
        geom_ribbon(aes(x=Date, ymin=pmax(Count-0.68*se, 0), ymax=Count+0.68*se ), fill="gray30", alpha=0.5) + 
        geom_line(aes(y=Count)) +
#        geom_hline(yintercept=20, colour="red") + 
        facet_wrap(~Section, ncol=1, scales="free_y")
}
