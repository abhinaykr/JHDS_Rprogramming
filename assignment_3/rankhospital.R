# id, name, rate, rank
#stringAsfactor = false
#http://stackoverflow.com/questions/14394402/how-to-break-ties-with-order-function-in-r


rankhospital <- function (state, outcome, num = "best") {
  
  data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  s <- state
  isostate <- which(data$State == s)
  if(length(isostate)<=0) stop("invalid state")
  
##---##  

  if (outcome == "heart attack") {
    
    Rate <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[isostate])
    Hospital.Name <- data$Hospital.Name[isostate]
    temp <- data.frame(Hospital.Name,Rate, stringsAsFactors = F)
    
    sort<- temp[order(temp$Rate,temp$Hospital.Name,na.last = NA),]
    
    if(num =="worst") {
      temp<-temp[order(temp$Hospital.Name),]
      sort<- temp[order(temp$Rate,na.last = NA,decreasing = T),]
      choose <- sort$Hospital.Name[1]
    } else if (num == "best"){
      choose<-sort$Hospital.Name[1]
    }
    else {
      choose<-sort$Hospital.Name[num]
    }
    

  }
  else if (outcome == "heart failure") {
    
    Rate <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[isostate])
    Hospital.Name <- data$Hospital.Name[isostate]
    temp <- data.frame(Hospital.Name,Rate, stringsAsFactors = F)
    
    sort<- temp[order(temp$Rate,temp$Hospital.Name,na.last = NA),]
    
    if(num =="worst") {
      temp<-temp[order(temp$Hospital.Name),]
      sort<- temp[order(temp$Rate,na.last = NA,decreasing = T),]
      choose <- sort$Hospital.Name[1]
    } else if (num == "best"){
      choose<-sort$Hospital.Name[1]
    }
    else {
      choose<-sort$Hospital.Name[num]
    }
    
  }
  else if (outcome == "pneumonia") {
    
    Rate <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[isostate])
    Hospital.Name <- data$Hospital.Name[isostate]
    temp <- data.frame(Hospital.Name,Rate, stringsAsFactors = F)
    
    sort<- temp[order(temp$Rate,temp$Hospital.Name,na.last = NA),]
    
    if(num =="worst") {
      temp<-temp[order(temp$Hospital.Name),]
      sort<- temp[order(temp$Rate,na.last = NA,decreasing = T),]
      choose <- sort$Hospital.Name[1]
    } else if (num == "best"){
      choose<-sort$Hospital.Name[1]
    }
    else {
      choose<-sort$Hospital.Name[num]
    }
    
  }

  else {
    stop(sprintf("invalid outcome"))
  }

  return(choose)

}