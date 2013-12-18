rnormal_poisson <- function(n, mean=0, sd = 1){
    r = exp(rnorm(n,mean=mean,sd=sd))
    return(rpois(n,r))
}

    
    
