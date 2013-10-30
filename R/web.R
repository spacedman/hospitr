makeFlotSeries <- function(efit){

    crits=sort(unique(efit$crit))
    for(section in c("Medical","Surgical")){
        
    }

}

writeAllJSFiles <- function(emerg, daysAhead=45, daysBefore=7, site="./Web/Cactus/Site/"){
    data=file.path(site,"static/data/")
    templates = file.path(site,"templates")
    tp = function(f){file.path(templates,f)}
    fp=function(f){file.path(data, f)}
    ecount = dailyAdmissionCounts(emerg)
    efit = linearFit(ecount, daysAhead, escale=5)
    m=makeSeries(efit, "Medical")
    s=makeSeries(efit, "Surgical")
    writeSeries(s, "surgical", fp("forecastSurgical.js"))
    writeSeries(m, "medical", fp("forecastMedical.js"))
    writeData(ecount, daysBefore, fp("admissionData.js"))
    writeSectionTable(efit,"Medical",tp("mtab.html"))
    writeSectionTable(efit,"Surgical",tp("stab.html"))
    writePredictionXL(efit,fp("prediction.xlsx"))
}

writeData <- function(ecount, ndays, filename){
    s = getDataXYSection(ecount, "Surgical", ndays)
    m = getDataXYSection(ecount, "Medical", ndays)
    sm = paste0("admissions = { surgical: ",s," , medical: ",m,"}\n")
    cat(sm,file=filename)
    invisible(sm)
}

sectionTable <- function(efit, section){
    efits = subset(efit,Section==section)
    efits = subset(efits, Date!=min(Date))
    parts = split(efits,efits$crit)
    date = parts[[1]]$Date
    count = parts[[1]]$Count
    table = do.call(cbind,llply(parts,function(x){x[,c("low","high")]}))
    table = cbind(Date=date, Forecast=count, table)
    names(table)=c("Date","Forecast","Lower 50%","Upper 50%","Lower 95%","Upper 95%","Lower 99%","Upper 99%")
    return(table)
}

writeSectionTable <- function(efit,section,fp){
    require(hwriter)
    d=options()$digits
    options(digits=2)
    t = sectionTable(efit,section)
    tm = as.matrix(t)
    tm[,-1] = as.character(round(as.numeric(tm[,-1])))
    tm[,1]=format(t$Date,"%b&nbsp;%d")
    tdclass=rep(c("date","forecast","low c50","high c50","low c95","high c95","low c99","high c99"),rep(nrow(tm),8))
    tableText = hwrite(tm,row.names=FALSE,table.id=paste0(section,"table"), table.class="neat sectiontable", class=tdclass)
    cat(tableText, file=fp)
    options(digits=d)
}

getDataXYSection <- function(ecount, section, ndays){
    data = subset(ecount, Section==section)
    lastDay = max(data$Date)
    data = data[data$Date > (lastDay - ndays),]
    data = data[order(data$Date),] # sorted
    xy = toXY(toJSDateNum(data$Date),data$Count)
    xy
}

makeSeries <- function(efit,section){
    efit=subset(efit,Section==section)
    efit = efit[order(efit$Date),]
    crits = sort(unique(efit$crit))
    efit$dJS = toJSDateNum(efit$Date)
    eseries = efit[crit==crits[1],c("dJS","Count")]
    series = list(est = toXY(eseries$dJS,eseries$Count))
    critseries = llply(crits,function(crit){
        ecrit = efit[efit$crit==crit,]
        list(lows = toXY(ecrit$dJS,ecrit$low),
             highs = toXY(ecrit$dJS,ecrit$high),
             crit=crit
             )
    })
    attr(critseries,"crits")=crits
    series$envelopes = critseries
    series
}

writeSeries <- function(es,name,filepath){
    m = makeSeriesJS(es,name)
    cat(m,file=filepath)
}

makeSeriesJS <- function(es,name){
    est=paste0("est: ",es$est,",\n")
    envs = laply(es$envelopes,makeEnvJS)
    envJS = paste0("envs: [",paste(envs,collapse=","),"]\n")

    paste0(name," = {",est,envJS,"}\n")
}

makeEnvJS <- function(env){
    paste0("{ low: ",env$lows,",\n high: ",env$highs,", \n crit: ",env$crit," }\n")
}

toXY <- function(x,y){
paste0("[ [",paste(x,y,sep=",",collapse="], ["),"] ]")
}
