makeFlotSeries <- function(efit){

    crits=sort(unique(efit$crit))
    for(section in c("Medical","Surgical")){
        
    }

}

writeAllJSFiles <- function(emerg,data="./Web/Cactus/Site/static/data/"){
    fp=function(f){file.path(data,f)}
    ecount = dailyAdmissionCounts(emerg)
    efit = linearFit(ecount,45,escale=5)
    m=makeSeries(efit,"Medical")
    s=makeSeries(efit,"Surgical")
    writeSeries(s,"surgical",fp("forecastSurgical.js"))
    writeSeries(m,"medical",fp("forecastMedical.js"))
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
