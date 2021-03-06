##' plot coloured distributions as function of x
##'
##' coloured distribution plots
##' @title distribution visualisation plot
##' @param x x coordinates
##' @param ydist function of x returning the distribution...
##' @param ymax  maximum y
##' @return nothing
##' @author Barry S Rowlingson

distplot <- function(x,ydist,ymax,ylab="count",xlab="Date",colScheme=cschemeBlack, cex=1){
    plot(range(x),c(0,ymax),type="n",ylab=ylab,xlab=xlab)
    xy=expand.grid(seq_along(x),0:ymax)
    xyp = expand.grid(x,0:ymax)
    points(xyp[,1],xy[,2],pch=19,cex=cex,col=colScheme(ydist(xy[,1],xy[,2])))
}

envplot <- function(x,ydist,ymax,ylab="count",xlab="Date", col=c(0,0,0), cex=1, add=FALSE){
    if(!add){
        plot(range(x),c(0,ymax),type="n",ylab=ylab,xlab=xlab)
    }
    xy=expand.grid(seq_along(x),0:ymax)
    xyp = expand.grid(x,0:ymax)
    q = attr(ydist,"q")
    d = ydist(xy[,1],xy[,2])
    alpha = 1-abs(1-2*d)
    points(xyp[,1],xy[,2],pch=19,cex=cex,col=rgb(col[1],col[2],col[3],alpha=alpha))
}

c2r <- function(col){
    t(col2rgb(col))/255
}

cschemeBlack <- function(p){
    rgb(0,0,0,alpha=p)
}

cscheme1 <- function(p){
    p = 1-p
    cname = c2r(as.character(cut(p,breaks = c(-0.1,.5,.95,1.1),labels=c("#30FF30","orange","red"))))
    return(rgb(cname[,1],cname[,2],cname[,3],(1-p)^(0.75)))
}
    

envareas <- function(x,ydist,ymax,ylab="count",xlab="Date", col=c(0,0,0), cex=1, p=c(.99,.95,.5),add=FALSE){
    if(!add){
        plot(range(x),c(0,ymax),type="n",ylab=ylab,xlab=xlab)   
    }
    q = attr(ydist,"q")
    xs = seq_along(x)
    opac = 0.2/ (length(p)/3)

    for(pval in p){
        low = (1-pval)/2
        high=low + pval
        ylow = q(xs,low)
        yhigh = q(rev(xs),high)
        g = grey(pval^2)
        polygon(c(x,rev(x)),c(ylow,yhigh), col=rgba(c(0,0,0),opac), border=FALSE)
    }
            
}

qlines <- function(x,ydist,p,colours,...){
    for(i in seq_along(p)){
        pv = p[i]
        lines(x,attr(y,"q")(seq_along(x),pv), col=colours[i], ...)
    }
}

qpoints <- function(x,ydist,p,colours,...){
    for(i in seq_along(p)){
        pv = p[i]
        points(x,attr(ydist,"q")(seq_along(x),pv), col=colours[i], ...)
    }
}

pmap <- function(x, ydist, p, ...){
    xyp = expand.grid(x=seq_along(x),y=p)
    xyd = expand.grid(x=x,p)
    q = attr(ydist,"q")(xyp[,1],xyp[,2])
    d = data.frame(x=xyd$x,y=1-xyp$y,q=q)
    return(d)
}

exceedenceimage <- function(d){
    levelplot(q~x+y,data=d)
}

exceedencecontours <- function(d){
    ggplot(data=d,aes(x=x,y=y,z=q,colour=..level..))+geom_contour(binwidth=1)+ scale_colour_gradient(low = "red", high = "green")+        xlab("Date") +
                ylab("Probability")

}

exceedplot <- function(x, ydist, q,...){
    xq = expand.grid(x=seq_along(x),q=q)
    xyd = expand.grid(x=x,q=q)
    p = 1-attr(ydist,"p")(xq$x,xq$q)
    data = data.frame(x=xyd$x,y=p,N=factor(xq$q))
    ggplot(data,aes(x=x,y=y,colour=N))+geom_line() + ylim(0,1)  +
        ggtitle("Probability of exceeding N admissions") +
            xlab("Date") +
                ylab("Probability")
}

dailydensityplot <- function(x,ydist,date,nmax=40){
    if(!is.numeric(date)){
        date = which(x==date)[1]
    }
    ds = x[date]
    mytitle=paste("Exceedence probabilities for ",ds)
    d = data.frame(x=0:nmax, y = attr(ydist,"d")(date,0:nmax))

    ggplot(d,aes(x=x,y=y))+geom_line() + xlab("Date") + ylab("d") + ggtitle(mytitle)
}

dailyexceedenceplot <- function(x,ydist,date,nmax=40){
    if(!is.numeric(date)){
        date = which(x==date)[1]
    }
    ds = x[date]
    mytitle=paste("Probability of getting N or more cases on ",ds)
    d = data.frame(x=0:nmax, p =1-attr(ydist,"p")(date,0:nmax))
    ggplot(d,aes(x=x,y=p))+geom_line() + xlab("N") + ylab("Probability")  +
        ggtitle(mytitle)
}

#
# demo distribution based on log-normals
#
ylnorm <- function(){
    mu = function(i){(6+(sin(i/2)*2)^2)}
    spread = function(i){0.1 + 0.06 * sqrt(i)}

    q = function(i,p){
        m = mu(i)
        s = spread(i)
        return(qlnorm(p,log(m),s))
    }

    d = function(i,x){
        m = mu(i)
        s = spread(i)
        return(dlnorm(x,log(m),s))
    }
    
    r = function(i,j){
        m = mu(i) 
        s = spread(i)
        return(1-plnorm(j,log(m),s))
    }

    p = function(i,q){
        m = mu(i)
        s = spread(i)
        return(plnorm(q,log(m),s))
    }
            
    attr(r,"d")=d
    attr(r,"p")=p
    attr(r,"q")=q
    r
}

ypois <- function(){
    r = function(i,j){
        mu = (sin(i/4)*4)^2
        return(ppois(mu,j))
    }
    r
}

ynbinom <- function(){
    r = function(i,j){
        mu = (sin((40-i)/4)*4)^2
        return(pnbinom(mu,mu=j,size=40-i))
    }
    r
}

rgba <- function(col,a){
    rgb(col[1],col[2],col[3],alpha=a)
}

addData <- function(x,ydist){
    
}


plotDaily <- function(the_day, dailies, mh, back=7){
    the_day = as.Date(the_day)

    if(!the_day %in% attr(dailies,"dates")){
        stop("No predictions from ",the_day)
    }
    iDay = which(the_day == attr(dailies,"dates"))

    days = seq(the_day - back, max(dailies[[iDay]]$medical$date), "day")   

    mh = rangeHospital(mh, days, the_day)

    par(mfrow=c(2,1))
    oneSectionPlot(dailies[[iDay]]$medical, mh[mh$section=="medical",],"Medical")
    oneSectionPlot(dailies[[iDay]]$surgical, mh[mh$section=="surgical",],"Surgical")
    par(mfrow=c(1,1))
    
}

rangeHospital <- function(mh,days, the_day){
    print(days)
    mh = mh[min(days) <= mh$Date & mh$Date <= max(days),]
    mh = mh[!(mh$measure=="actual" & mh$Date >= the_day),]
    return(mh)
}

oneSectionPlot <- function(section, mhsection, title){
    pred = melt(
        data.frame(
            date = section$date,
            mean = section$mean,
            lower = section$cis$values[[1]][1,],
            upper = section$cis$values[[1]][2,]
            ),
        id="date"
        )
    print(head(mhsection))
    g = ggplot(pred, aes(x=date,y=value, group=variable, col=variable))+geom_line()  + 
        geom_line(data=mhsection, aes(x=Date, y=count,group=measure, col=measure))
    print(g)
#    xlim=range(pred$date, mhsection$date)
#    ylim = range(pred$mean, pred$lower, pred$upper, mh$count)
#    plot(xlim,ylim,xlim=as.Date(xlim), ylim=ylim,main=title,type="n")
}

maxMinPlot <- function(allActuals){
    ## shows the max and min values of actual data
    ## allActuals = readAllActuals("./Server/Data/XLS/")
    allActuals$data = filter(allActuals$data, !(is.na(Actual.Med) & is.na(Actual.Surg)))
    maxmins = allActuals$data %>% group_by( Dates) %>% summarise(min.Med=min(Actual.Med), max.Med=max(Actual.Med), min.Surg=min(Actual.Surg), max.Surg=max(Actual.Surg))
    melted = melt(maxmins, id.vars="Dates") %.% do(cbind(.,data.frame(parts=str_split_fixed(.$variable,"\\.",2))))
    recasted = dcast(melted, Dates+parts.2~parts.1,value.var="value")
    ggplot(recasted,aes(x=Dates,ymin=min,ymax=max))+geom_errorbar() + facet_wrap(~parts.2,ncol=1)
}
