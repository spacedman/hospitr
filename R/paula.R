
fnConstructDailyCounts<-function(ds,mindates,maxdates){

    ## transform to dates
    dates <- as.Date(ds$Adm.Date)

    ## contingency table
    tab <- as.data.frame(table(dates))
    names(tab)[2] <- "adm"
    tab$dates <- as.Date(tab$dates)


    ## sequence of dates
    res <- data.frame(dates=seq(from=mindates,to=maxdates,by="1 day"))
    ## merge
    res <- merge(res,tab,by="dates",all.x=TRUE)
    res[is.na(res$adm),"adm"] <- 0

    ## day of week
    ## 1 is Sunday
    res$wday<-wday(res$dates,label=FALSE)

    return(res)
}

### 1. DATASET

fnCovMat <- function(date, count){
    res = data.frame(dates=date, adm=count)
    res$wday = wday(res$dates)
    fnCompleteDataset(res)
}

fnCompleteDataset<-function(res){
    n<- dim(res)[1]
    Y<-res$adm
    dayofweek<-res$wday
    ## change time term to fraction of year
    yd = yday(res$dates)
    cosine=cos(2*pi*yd/365)
    sine =sin(2*pi*yd/365)
    day2<-as.numeric(dayofweek==2)
    day3<-as.numeric(dayofweek==3)
    day4<-as.numeric(dayofweek==4)
    day5<-as.numeric(dayofweek==5)
    day6<-as.numeric(dayofweek==6)
    day7<-as.numeric(dayofweek==7)
    dataComplete<-data.frame(Y,dayofweek,yd,cosine,sine,day2,day3,day4,day5,day6,day7)
    return(dataComplete)
}

fnSubsetDataset<-function(res,mindate,maxdate){
    dataComplete<-fnCompleteDataset(res)
    windowmin<-as.Date(res$dates) >= as.Date(mindate)
    windowmax<-as.Date(res$dates) <= as.Date(maxdate)
    dataSubset<-dataComplete[(windowmin & windowmax),]
    return(dataSubset)
}

### 2. CREATE OBSERVED, MATRIXCOVARIATES, DYNAMICCOEFF

fnCreateObsMatrixCovariatesDynamicCoeff<-function(dataset,modelo){
    ## my: Observed counts
    ## mX: Matrix covariates. #First columns static, last columns dynamic
    ## dynamicCoeff: Binary vector that indicates which coefficients are dynamic (=1) and which static (=0)

    my<-dataset$Y

    if(modelo=="Static"){
        mX<-cbind(rep(1,nrow(dataset)),as.matrix(dataset[,c("cosine","sine","day2","day3","day4","day5","day6","day7")]))
        dynamicCoeff<-rep(0,9)
    }
    if(modelo=="DynamicInterceptCosSin"){
        mX<-cbind(rep(1,nrow(dataset)),as.matrix(dataset[,c("cosine","sine","day2","day3","day4","day5","day6","day7")]),
                  rep(1,nrow(dataset)),as.matrix(dataset[,c("cosine","sine")]))
        dynamicCoeff<-c(rep(0,9),1,1,1)
    }
    if(modelo=="DynamicIntercept"){
        mX<-cbind(rep(1,nrow(dataset)),as.matrix(dataset[,c("cosine","sine","day2","day3","day4","day5","day6","day7")]),
                  rep(1,nrow(dataset)))
        dynamicCoeff<-c(rep(0,9),1)
    }
    if(modelo=="DynamicCosSin"){
        mX<-cbind(rep(1,nrow(dataset)),as.matrix(dataset[,c("cosine","sine","day2","day3","day4","day5","day6","day7")]),
                  as.matrix(dataset[,c("cosine","sine")]))
        dynamicCoeff<-c(rep(0,9),1,1)
    }
    return(list(y=my,X=mX,dyn=dynamicCoeff))
}



### 3. DEFINE AND ESTIMATE MODEL

fnDefineModel<-function(my, mX, mQ=NULL, mT=NULL){
    if(is.null(mQ)){
        mQ<-diag(0,dim(mX)[2])
    }
    model<-regSSM(y=my, X=mX, H = NULL, Q = mQ, u = NULL, distribution = "Poisson")
    if(is.null(mT)){
        model$T[,,1]<-diag(1,dim(mX)[2])
    }else{
        model$T[,,1]<-mT
    }
    return(model)
}


fnEstimateQandT<-function(model, dynamicCoeff){
    if(sum(dynamicCoeff==1)==0){stop("There are not dynamic terms")}

    ## Estimate parameters Q and T
    logLikModel<-function(theta){
        m<-(length(theta)/2)
        model$Q[posDyn,posDyn,1]<-diag(exp(theta[1:m]),nrow=m)
        model$T[posDyn,posDyn,1]<-diag(theta[(1+m):(length(theta))],nrow=m)
        -logLik(object = model, nsim = 0, antithetics = TRUE, taylor = TRUE, theta = NULL, maxiter = 5000)
    }
    posDyn<-which(dynamicCoeff==1)
    ## Initial values variance and autorregressive
    theta0<-c(rep(-5,length(posDyn)),rep(1,length(posDyn)))
    out=optim(theta0,logLikModel,method="BFGS")
    
    ## Set matrix Q (variance), T (autoregressive)
    m<-(length(out$par)/2)
    model$Q[posDyn,posDyn,1]<-diag(exp(out$par[1:m]),nrow=m)
    model$T[posDyn,posDyn,1]<-diag(out$par[(1+m):(length(out$par))],nrow=m)
    
    return(list(model, out$par))
}


### 4. SAMPLE. Importance sampling (weights) and Independent sample
fnImportanceSample<-function(model, numsim){
    sam<-importanceSSM(model,save.model=FALSE,nsim=numsim)
    w<-sam$weights/sum(sam$weights)
    return(list(sam=sam,w=w))
}

### Independent sample. Sampling with replacement from the importance sample
fnIndependentSample<-function(sam,w,size=1000){
    sam$states<-sam$states[,,sample(length(w), size, replace = TRUE, prob = w)]
    return(sam)
}

# 5. SUMMARY RESULTS

fnCombLinearOnlystates<-function(indices,sam,w){
    if(is.null(w)){w<-rep(1/(dim(sam$states)[3]),dim(sam$states)[3])}
    terminos<-0
    for(i in 1:length(indices)){terminos<-terminos+sam$states[indices[i],,]}
    return(colSums(t(terminos)*w))
}

fnCombLinear<-function(exponential,indices,model,sam,w){
    if(is.null(w)){w<-rep(1/(dim(sam$states)[3]),dim(sam$states)[3])}
    terminos<-0
    for(i in 1:length(indices)){terminos<-terminos+model$Z[1,indices[i],]*sam$states[indices[i],,]}
    cl<-colSums(t(terminos)*w)
    if(exponential==TRUE){cl<-colSums(t(exp(terminos))*w)}
    return(cl)
}


fnFixedEffectsAndCI<-function(dynamicCoeff,sam,w){
    if(is.null(w)){w<-rep(1/(dim(sam$states)[3]),dim(sam$states)[3])}
    mFixedEffects<-NULL
    indices<-which(dynamicCoeff==0)
    for(idcoef in indices){
        coefAux<-colSums(t(sam$states[idcoef,,])*w)
        ## Last value because at the beginning is the diffuse phase
        coef<-coefAux[length(coefAux)]
        varcoef<-colSums(t(sam$states[idcoef,,]^2)*w)[length(coefAux)]-coef^2
        intvCoef<-coef + qnorm(0.975)*sqrt(varcoef)%o%c(-1,1)
        mFixedEffects<-rbind(mFixedEffects,c(idcoef,coef,intvCoef))
    }
    return(mFixedEffects)
}


fnObserved<-function(indices,idForecast,model,sam){
    terminos<-0
    for(i in 1:length(indices)){terminos<-terminos+model$Z[1,indices[i],]*sam$states[indices[i],,]}
    ## Just do it for forecast times, not for the whole series
    if(!is.null(idForecast)){
        if(is.null(dim(terminos))){
            terminos<-terminos[idForecast]
        }else{
            terminos<-terminos[idForecast,]
        }
    }
    if(is.null(dim(terminos))){
        obs<-sapply(exp(terminos), function(x){rpois(1,x)})
    }else{
        obs<-apply(exp(terminos), c(1,2), function(x){rpois(1,x)})
    }
    return(obs)
}


fnMean<-function(model,sam,w){
    if(is.null(w)){w<-rep(1/(dim(sam$states)[3]),dim(sam$states)[3])}
    meanadm<-fnCombLinear(exponential=TRUE,indices=1:(dim(model$Q)[2]),model,sam,w)
    return(meanadm)
}


fnQuantiles<-function(obs,quant,w){
    if(is.null(w)){w<-rep(1/length(length(obs)),length(obs))}
    if(is.null(dim(obs))){
        quantadm<-fnQuantiles2(obs,quant,w)
    }else{
        quantadm<-apply(obs,1, fnQuantiles2, quant=quant, w=w)
    }
    return(quantadm)
}

fnQuantiles2<-function(x,quant,w){
    orden<-order(x)
    sortx<-x[orden]
    sortw<-w[orden]
    cumsumsortw<-cumsum(sortw)/sum(sortw)
    q1<-0.5-quant/2
    q2<-0.5+quant/2
    return(c(sortx[which.min(abs(cumsumsortw-q1))],sortx[which.min(abs(cumsumsortw-q2))]))
}


fnMeanQs <- function(r, Q, T, NUMSIM, mP1inf, ma1){
    #modelHere <-fnDefineModel(r$y, r$X, mQ=Q, mT=T)
    modelHere <-fnDefineModel(r[[1]], r[[2]], mQ=Q, mT=T)
    #ini add Paula
    modelHere$P1inf<-mP1inf
    modelHere$a1<-ma1
    #end add Paula
    s1<-fnImportanceSample(modelHere, NUMSIM)
    s<-list(sam=s1[[1]],w=s1[[2]])
    meanadm<-fnMean(modelHere,s$sam,s$w)
    #obs<-fnObserved(indices=1:(dim(modelHere$Q)[2]),idForecast=which(is.na(r$y)),modelHere,s$sam)
    obs<-fnObserved(indices=1:(dim(modelHere$Q)[2]),idForecast=which(is.na(r[[1]])),modelHere,s$sam)
    lower = fnQuantiles(obs,quant=0.5,s$w)
    upper = fnQuantiles(obs,quant=0.95,s$w)
    list(mean=meanadm, ul=rbind(lower,upper), obs=obs, w=s$w)
}

fullDist <- function(obs, w, maxcount){
    ntimes = nrow(res$obs)
    d = matrix(0, ntimes, maxcount+2) # +1 for zero, +1 for over max
    for(i in 1:ntimes){
        tab = tapply(w, obs[i,], sum)
        tab = tab/sum(tab) # normalise to 1
        d[i,]=vtab(tab, maxcount)
    }
    d
}

vtab <- function(tab, maxcount){
    v = rep(0,maxcount+2)
    v[1 + as.numeric(names(tab))] = tab
    v[maxcount+2] = sum(v[-(1:(maxcount+1))],na.rm=TRUE)
    v[1:(maxcount+2)]
}


##' Extend a data set for prediction
##'
##' given a time-count dataset, return the covariate matrix with added observations after the last one
##' with NA as observed value
##' @title Generate extended covariate matrix
##' @param dataset input data set
##' @param formula of the form count~date
##' @param npredict number of extra rows to add on
##' @return a data frame of covariates for the model
##' @author Barry S Rowlingson
extendData <- function(dataset, formula, npredict){
    mm = model.frame(formula, dataset) # count ~ date
    pm = data.frame(count=rep(NA,npredict), date=max(as.Date(mm[,2])) + 1:npredict)
    return(fnCovMat(c(mm[,2], pm[,2]), c(mm[,1],pm[,1])))
    
}

##' Fit model to historical data
##'
##' Given some date-count data and a model name, return the fitted model parameters
##' @title part1 - fitting
##' @param dataset 
##' @param formula 
##' @param modelName 
##' @return the model object
##' @author Barry S Rowlingson
part1 <- function(dataset, formula,
                  modelName=c("Static","DynamicInterceptCosSin","DynamicIntercept","DynamicCosSin")
                  ){
    modelo = match.arg(modelName)

    mm = model.frame(formula, dataset) # formula is count ~ date
    dataModel = fnCovMat(mm[,2],mm[,1]) # date, count
    
    r<-fnCreateObsMatrixCovariatesDynamicCoeff(dataModel,modelo)
    my<-r[[1]]
    mX<-r[[2]]
    dynamicCoeff<-r[[3]]
    model<-fnDefineModel(my, mX)
    model<-fnEstimateQandT(model, dynamicCoeff)[[1]]
    model$modelName = modelo
    model
}

##' make predictions from dynamic model
##'
##' given a model from part1, and a dataset, extend that dataset and predict
##' for those new days by simulation
##' @title part2 - prediction
##' @param model returned from part1
##' @param dataset data frame with day and count columns
##' @param formula of the form count~date 
##' @param npredict number of days ahead to predict
##' @param nsims number of simulations
##' @return means and confidence intervals
##' @author Barry S Rowlingson
part2 <- function(model, dataset, formula, npredict, nsims){

    dataFit = extendData(dataset, formula, npredict)
    modelo = model$modelName
    r<-fnCreateObsMatrixCovariatesDynamicCoeff(dataFit,modelo)
    my<-r[[1]]
    mX<-r[[2]]
    dynamicCoeff<-r[[3]]
    model<-fnDefineModel(my, mX, mQ=model$Q, mT=model$T)
    s<-fnImportanceSample(model, nsims)
    impsample<-s[[1]]
    w<-s[[2]]
    mFixedEffects<-fnFixedEffectsAndCI(dynamicCoeff,impsample,w)
    mP1inf<-diag(dynamicCoeff)
    ma1<-rep(0,length(dynamicCoeff))
    ma1[which(dynamicCoeff==0)]<-mFixedEffects[,2]
    MeanQ = fnMeanQs(r, model$Q, model$T, nsims, mP1inf, ma1)
    return(MeanQ)    
}
    
