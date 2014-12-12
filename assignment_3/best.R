best <- function (state,outcome){

    ## Read outcome data
    data <- read.csv("data/outcome-of-care-measures.csv", colClasses="character")
    s <- state
    o <- outcome
    ## Check that state and outcome are valid
    isostate <- which(data$State == s)
    if (length(isostate) <=0) stop("invalid state")
    
    ## Return hospital name in that state with lowest 30-day death rate
    nameVector <- data$Hospital.Name[isostate]
    
    if (outcome == "heart attack") {
        
        pV <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[isostate])
        isobest <- which(pV == min(pV,na.rm=T))
    }
    else if (outcome == "heart failure") {
        
        pV <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[isostate])
        isobest <- which(pV == min(pV,na.rm=T))
    }
    else if (outcome == "pneumonia") {
        
        pV <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[isostate])
        isobest <- which(pV == min(pV,na.rm=T))
    }else {
        stop(sprintf("invalid outcome"))
    }
    name <- nameVector[isobest]
    return(name)

}