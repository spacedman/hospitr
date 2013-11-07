wday_factor <- function(Dates){
    require(lubridate)
    factor(wday(Dates,label=TRUE),ordered=FALSE)

}

glm_predict_interval <- function(m,newdata,se=2){
    p = predict(m,newdata,se=TRUE)
    pR = predict(m,newdata,type="response")
    p = data.frame(low=exp(p$fit-se*p$se),high=exp(p$fit+se*p$se))
    p$Count = pR
    p
        
}

toJSDateNum <- function(Dates){
    as.numeric(Dates)*1000*24*60*60
}
