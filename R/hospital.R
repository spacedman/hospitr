##' Make a hospital object
##'
##' @title Make a hospital object
##' @param name Hospital name
##' @param medicalWards Medical ward data frame - Name and Size
##' @param surgicalWards  Surgical ward data frame - Name and Size
##' @return A hospital object
##' @author Barry S Rowlingson
hospital <- function(name, medicalWards, surgicalWards){
    h = list()
    h$Name = name
    h$MedicalWards = medicalWards
    h$SurgicalWards = surgicalWards
    class(h)="hospital"
    h
}

capacity <- function(x,...){
    UseMethod("capacity")
}

capacity.hospital <- function(x,...){
    return(sum(x$MedicalWards$Beds)+sum(x$SurgicalWards$Beds))
}

##' Make a random hospital
##'
##' 
##' @title Random Hospital Generator
##' @param name Hospital Name
##' @param nMed Number of medical wards
##' @param nSurg Number of surgical wards
##' @param medicalSize vector of length 2 of min/max medical ward size
##' @param surgicalSize vector of length 2 of min/max surgical ward size
##' @return A hospital object
##' @author Barry S Rowlingson
rhospital <- function(name,nMed,nSurg,medicalSize=c(10,30),surgicalSize=c(10,30)){
    meds=data.frame(Name = paste0("Ward M",1:nMed))
    surgs=data.frame(Name = paste0("Ward S",1:nSurg))

    meds$Beds = as.integer(runif(nMed,medicalSize[1],medicalSize[2]))
    surgs$Beds = as.integer(runif(nSurg,surgicalSize[1],surgicalSize[2]))
    
    hospital(name, meds, surgs)
    
}
