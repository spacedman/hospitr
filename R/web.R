makeFlotSeries <- function(efit){

    crits=sort(unique(efit$crit))
    for(section in c("Medical","Surgical")){
        
    }

}
##' write all files for web (not just JS!)
##'
##' this function writes the JS and other stuff for the web site
##'
##' Process:
##' 
##' load("./Data/25-7-2013/emerg.RData")
##' emerg=subset(emerg,AEOBS=="")
##' edata = data.frame(admission=emerg$Adm.Date, Section=emerg$med.surg..based.on.specialty.)
##' writeAllJSFiles(edata)
##' 
##' @title Write All Web Output Files
##' @param emerg data frame of admission (Date) and Section (Medical/Surgical)
##' @param daysAhead number of days to predict ahead
##' @param daysBefore number of data points to include before
##' @param site path to site root
##' @return nothing much
##' @author Barry S Rowlingson
writeAllJSFiles <- function(emerg, daysAhead=45, daysBefore=7, site="./Web/Cactus/Site/", electives){
    data=file.path(site,"static/data/")
    templates = file.path(site,"templates")
    tp = function(f){file.path(templates,f)}
    fp=function(f){file.path(data, f)}
    ecount = dailyAdmissionCounts(emerg)
    efit = linearFit(ecount, daysAhead, escale=5)
    ## now we have the dates, get the electives
    m=makeSeries(efit, "Medical")
    s=makeSeries(efit, "Surgical")
    writeSeries(s, "surgical", fp("forecastSurgical.js"))
    writeSeries(m, "medical", fp("forecastMedical.js"))
    writeData(ecount, daysBefore, fp("admissionData.js"))
    writeSectionTable(efit,"Medical",tp("mtab.html"),electives[["Medical"]])
    writeSectionTable(efit,"Surgical",tp("stab.html"),electives[["Surgical"]])
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

writeSectionTable <- function(efit,section,fp, gelective){
    require(hwriter)
    file.remove(fp)
#    O = function(text,file=fp,append=TRUE,...){
#        cat(text,file=fp,append=TRUE,...)
#    }
    d=options()$digits
    options(digits=2)
    t = sectionTable(efit,section)
    nelect = gelective(t$Date)
    tm = as.matrix(t)
    tm[,-1] = as.character(round(as.numeric(tm[,-1])))
    tm=cbind(tm,nelect)
    dow=substr(format(t$Date,"%a"),1,1)
    tm[,1]=paste(dow,"&nbsp;",format(t$Date,"%b&nbsp;%d"),sep="")
    colclasses = c("date","forecast","low c50","high c50","low c95","high c95","low c99","high c99","sched")
    tdclass=matrix(rep(colclasses,rep(nrow(tm),ncol(tm))),ncol=ncol(tm))
    heads=c("Date","Forecast","Lower 50%","Upper 50%","Lower 95%","Upper 95%","Lower 99%","Upper 99%","Sched.")
    weekend = ifelse(is.weekend(t$Date),"weekend","weekday")
    sat = ifelse(wday(t$Date)==7,"saturday","")
    sun = ifelse(wday(t$Date)==1,"sunday","")
    trclass=paste(weekend,sat,sun,sep=" ")
    writeHTML(tm, id=paste0(section,"table"),
              mainclass="neat sectiontable",
              heads = heads,
              trclass=trclass,
              tdclass=tdclass,
              fp=fp)
    options(digits=d)
    return(invisible(0))
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
    eseries = efit[efit$crit==crits[1],c("dJS","Count")]
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

is.weekend <- function(d){
    wday(d) %% 7 <2
}


writeHTML <- function(table, id="mytable", mainclass="mainclass", heads, trclass="myrow", tdclass, fp=""){
    O = function(text,file=fp,append=TRUE,...){
        cat(text,file=fp,append=TRUE,...)
    }
    O(paste0('<table id="',id,'" class="',mainclass,'">\n'))
    O("<tr>")
    for(i in 1:ncol(table)){
        O(paste0("<th>",heads[i],"</th>\n"))
    }
    O("</tr>\n")
    trclass=rep(trclass,length=nrow(table))
    for(i in 1:nrow(table)){
        O(paste0('<tr class="',trclass[i],'">\n'))
        for(j in 1:ncol(table)){
            O(paste0('<td class="',tdclass[i,j],'">'))
            O(table[i,j])
            O('</td>\n')
        }
        O('</tr>\n')
    }
    O("</table>\n")
}
