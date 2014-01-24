
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

fnCompleteDataset<-function(res){
    n<- dim(res)[1]
    Y<-res$adm
    dayofweek<-res$wday
    t<-1:n
    cosine=cos((2*pi/365)*t)
    sine =sin((2*pi/365)*t)
    day2<-as.numeric(dayofweek==2)
    day3<-as.numeric(dayofweek==3)
    day4<-as.numeric(dayofweek==4)
    day5<-as.numeric(dayofweek==5)
    day6<-as.numeric(dayofweek==6)
    day7<-as.numeric(dayofweek==7)
    dataComplete<-data.frame(Y,dayofweek,t,cosine,sine,day2,day3,day4,day5,day6,day7)
    return(dataComplete)
}

fnSubsetDataset<-function(res,mindate,maxdate){
    dataComplete<-fnCompleteDataset(res)
    windowmin<-which(as.Date(res$dates)== as.Date(mindate))
    windowmax<-which(as.Date(res$dates)== as.Date(maxdate))
    dataSubset<-dataComplete[(windowmin:windowmax),]
    return(dataSubset)
}

### 2. CREATE OBSERVED, MATRIXCOVARIATES, DYNAMICCOEFF

fnCreateObsMatrixCovariatesDynamicCoeff<-function(dataset,modelo){
    ## my: Observed counts
    ## mX: Matrix covariates. #First columns static, last columns dynamic
    ## dynamicCoeff: Binary vector that indicates which coefficients are dynamic (=1) and which static (=0)

    my<-dataset$Y

    if(modelo=="Static"){
        mX<-cbind(rep(1,length(dataset$t)),as.matrix(dataset[,c("cosine","sine","day2","day3","day4","day5","day6","day7")]))
        dynamicCoeff<-rep(0,9)
    }
    if(modelo=="DynamicInterceptCosSin"){
        mX<-cbind(rep(1,length(dataset$t)),as.matrix(dataset[,c("cosine","sine","day2","day3","day4","day5","day6","day7")]),
                  rep(1,length(dataset$t)),as.matrix(dataset[,c("cosine","sine")]))
        dynamicCoeff<-c(rep(0,9),1,1,1)
    }
    if(modelo=="DynamicIntercept"){
        mX<-cbind(rep(1,length(dataset$t)),as.matrix(dataset[,c("cosine","sine","day2","day3","day4","day5","day6","day7")]),
                  rep(1,length(dataset$t)))
        dynamicCoeff<-c(rep(0,9),1)
    }
    if(modelo=="DynamicCosSin"){
        mX<-cbind(rep(1,length(dataset$t)),as.matrix(dataset[,c("cosine","sine","day2","day3","day4","day5","day6","day7")]),
                  as.matrix(dataset[,c("cosine","sine")]))
        dynamicCoeff<-c(rep(0,9),1,1)
    }
    return(list(my,mX,dynamicCoeff))
}



### 3. DEFINE AND ESTIMATE MODEL

fnDefineModel<-function(my, mX, mQ=NULL, mT=NULL){
    if(is.null(mQ)){
        mQ<-diag(0,dim(mX)[2])
    }
    model<-fnRegSSM(y=my, X=mX, H = NULL, Q = mQ, u = NULL, distribution = "Poisson")
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
    return(list(sam,w))
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

#' Create a State Space Model Representation of Linear Regression Model
#'
#' Function regSSM creates a state space representation of linear regression model.
#' 
#' The linear Gaussian state space model is given by
#' 
#' \deqn{y_t = X_t \beta_t + \epsilon_t,}{y[t] = Z[t]\alpha[t] + \epsilon[t], (observation equation)}
#' 
#' \deqn{\alpha_{t+1} = T_t \alpha_t + R_t \eta_t,}{\alpha[t+1] = T[t]\alpha[t] + R[t]\eta[t], (transition equation)}
#' 
#' where \eqn{\epsilon_t ~ N(0,H_t)}{\epsilon[t] ~ N(0,H[t])}, \eqn{\eta_t ~ N(0,Q_t)}{\eta[t] ~ N(0,Q[t])} 
#' and \eqn{\alpha_1 ~ N(a_1,P_1)}{\alpha[1] ~ N(a[1],P[1])} independently of each other. In case of non-Gaussian observations,
#' the observation equation is of form \eqn{p(y_t|\theta_t) = p(y_t|Z_t\alpha_t)}{p(y[t]|\theta[t]) = p(y[t]|Z[t]\alpha[t])},
#' with \eqn{p(y_t|\theta_t)}{p(y[t]|\theta[t])} being one of the following:
#'

#'
#' @export
#'  @inheritParams SSModel
#'  @seealso \code{\link{arimaSSM}} for state space representation of ARIMA model, \code{\link{structSSM}} for structural time series model, and \code{\link{SSModel}} for custom \code{SSModel} object.
#' @param X A \eqn{n \times k}{n*k} matrix of explanatory variables, with each column containing one explanatory variable, or a list of length \eqn{p} 
#' containing \eqn{X} matrices for each series. If X is matrix, it is assumed that all \eqn{p} series use same explanatory variables.  
#' @param H A \eqn{p \times p}{p*p} covariance matrix (or \eqn{p \times p \times n}{p*p*n} array in of time-varying case) of the disturbance terms 
#'  \eqn{\epsilon_t}{epsilon[t]} of the observation equation. Default gives \eqn{p \times p}{p*p} zero matrix.
#'  Omitted in case of non-Gaussian distributions. Augment the state vector if you want to add additional noise.
#' @param Q A \eqn{r \times r}{r*r} (or \eqn{r \times r \times n}{r*r*n} array in of time-varying case) covariance matrix of the disturbance terms 
#'  \eqn{\eta_t}{\eta[t]} of the system equation.  Default is \eqn{m \times m}{m*m} zero matrix ie. ordinary time-invariant regression.
fnRegSSM <- function(y, X, H=NULL, Q=NULL, u=NULL, distribution = c("Gaussian", "Poisson", "Binomial"), 
        transform = c("none", "ldl", "augment"), tolF = .Machine$double.eps^0.5, tol0 = .Machine$double.eps^0.5) {
    
    transform <- match.arg(arg=transform, choices = c("none", "ldl", "augment"))    
    distribution <- match.arg(arg=distribution, choices = c("Gaussian", "Poisson", "Binomial"))    
    
    y<-as.ts(y)
    if(is.array(y)){
        p <- as.integer(dim(y)[2])
        n <- as.integer(dim(y)[1])  
    } else {
        p <- as.integer(1)
        n <- as.integer(length(y))  
    }     
    storage.mode(y)<-"double"
    
    if(p>1 & !identical(distribution,"Gaussian")) stop("Only univariate series are supported for non-Gaussian models.")
    #define number of states    
    
    if(!is.list(X)){       
        
        if(!is.ts(X) & (!identical(dim(X)[1],n) | is.null(dim(X)[2]))) stop("X is not an n times k matrix or list of such matrices.")
        X2<-vector("list",p)
        for(i in 1:p)
            X2[[i]]<-data.matrix(X)
        X<-X2
    } 
    xdim <- sapply(X,dim) #number of regressors including the constant
    if(!identical(sum(xdim[1,]==n),p)) stop("X is not an n times k matrix or list of such matrices.")
    
    kn <- xdim[2,] #number of regressors including the constant
    m <- as.integer(sum(kn)) # number of states
    tspy<-attributes(y)
    y<-data.matrix(y)
    for(j in 1:p){
        ymiss <- is.na(y[,j])
        if(sum(ymiss)>0 & sum(is.na(X[[j]]))>0){
            for(i in 1:kn[j]){
                xmiss<-is.na(X[[j]][, i])
                y[xmiss,j]<-NA
            }
            if(!identical(ymiss,is.na(y[,j])))
                warning("Missing values in X, corresponding elements in y set to NA.")            
        }
    }        
    attributes(y)<-tspy
    kncs<-c(0,cumsum(kn))
    Z<-array(0, dim=c(p, m, n))  
    states<-NULL
    for (j in 1:p){                 
        Z[j,(kncs[j]+1):kncs[j+1] , ] <- t(X[[j]])
        states<-c(states,paste0("beta",1:kn[j],".",j))
    }
    
    #H
    if(distribution!="Gaussian"){
        H_type<-"Omitted"
        H <- array(0, dim=c(p, p, 1))
    } else {
        if(is.null(H)){
            H <- array(0, dim=c(p, p, 1))
            H_type<-"Diagonal"
        } else {            
            H <- array(H, dim=c(p, p,  (n-1) * (max(dim(H)[3], 0,na.rm=TRUE) > 1) + 1))
            if(sum(is.na(H))>0){
                H_type<-"Untransformed"
            } else {
                if(transform=="none"){
                    if(p==1){
                        H_type<-"Diagonal"
                    } else {      
                        H_type<-"Diagonal"
                        for(i in 1:dim(H)[3]){
                            if(max(abs(H[,,i][-which(diag(p)==1)]), na.rm=TRUE)>0){
                                H_type<-"Untransformed"
                                break
                            }
                        }
                    }
                } else{
                    H_type<-NULL
                } 
            }   
        }         
    }
    T<-diag(m)
    dim(T)<-c(m,m,1)
    
    if(is.null(Q)){
        Q <- array(0, dim=c(m, m, 1))  
        r<-as.integer(m)        
    } else {   
        r<-as.integer(max(dim(Q)[1],1))        
        Q <- array(Q, dim=c(r, r,  (n-1) * (max(dim(Q)[3], 0,na.rm=TRUE) > 1) + 1))        
    } 
    
    R <- array(0, dim=c(m, r, 1))
    R[,,1]<-diag(m)[,1:r,drop=FALSE]         
    
    
    if(!identical(distribution,"Gaussian")){
        if(is.null(u))
            u<-rep(1,n)    
        u<-array(u,dim=n)    
    }
    a1<-matrix(0,m,1)
    rownames(a1)<-states
    object<-list(y=y,Z=Z,H=H,T=T,R=R,Q=Q,a1=a1,P1=matrix(0,m,m),P1inf=diag(m),u=as.double(u), p=p,n=n,m=m,k=r,distribution=distribution,H_type=H_type,tolF=tolF,tol0=tol0)
    class(object) <- c("SSModel","regSSM")
    if (transform %in% c("ldl", "augment") & sum(is.na(H))==0) 
        object <- transformSSM(object, type = transform)
    
    invisible(object)
} 
