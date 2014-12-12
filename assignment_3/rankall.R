rankall <- function (outcome, num = "best") {
options(warn=-1)
  data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  allstates<- c('AK', 'AL', 'AR', 'AZ', 'CA', 'CO', 'CT', 'DC', 'DE', 'FL', 'GA', 'HI', 'IA', 'ID', 
                'IL', 'IN', 'KS', 'KY', 'LA', 'MA', 'MD', 'ME', 'MI', 'MN', 'MO', 'MS', 'MT', 'NC', 
                'ND', 'NE', 'NH', 'NJ', 'NM', 'NV', 'NY', 'OH', 'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 
                'TN', 'TX', 'UT', 'VA', 'VT', 'WA', 'WI', 'WV', 'WY')
  h<-vector()
  s<-vector()
  r<-vector()
  
  numstate <- c("best","worst",num)
  
  
  for (state in seq_along(allstates)){
  
    isostate <<- which(data$State == allstates[state])
    
    ##

      if (outcome == "heart attack") {
        
        Rate <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[isostate])
        Hospital.Name <- data$Hospital.Name[isostate]
        temp <- data.frame(Hospital.Name,Rate, stringsAsFactors = F)
        
        sort<- temp[order(temp$Rate,temp$Hospital.Name),]
        
        if(num =="worst") {
          temp<-temp[order(temp$Hospital.Name),]
          sort<- temp[order(temp$Rate,decreasing = T),]
          choose <- sort$Hospital.Name[1]
          chrate <- sort$Rate[1]
        } else if (num == "best"){
          choose<-sort$Hospital.Name[1]
          chrate <- sort$Rate[1]
        }
        else {
          choose<-sort$Hospital.Name[num]
          chrate <- sort$Rate[num]
        }
      }
      ##
      
      else if (outcome == "heart failure") {
        
        Rate <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[isostate])
        Hospital.Name <- data$Hospital.Name[isostate]
        temp <- data.frame(Hospital.Name,Rate, stringsAsFactors = F)
        
        sort<- temp[order(temp$Rate,temp$Hospital.Name),]
        
        if(num =="worst") {
          temp<-temp[order(temp$Hospital.Name),]
          sort<- temp[order(temp$Rate,decreasing = T),]
          choose <- sort$Hospital.Name[1]
          chrate <- sort$Rate[1]
        } else if (num == "best"){
          choose<-sort$Hospital.Name[1]
          chrate <- sort$Rate[1]
        }
        else {
          choose<-sort$Hospital.Name[num]
          chrate <- sort$Rate[num]
        }
      }

      else if (outcome == "pneumonia") {
          
          Rate <- as.numeric(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[isostate])
          Hospital.Name <- data$Hospital.Name[isostate]
          temp <- data.frame(Hospital.Name,Rate, stringsAsFactors = F)
          
          sort<- temp[order(temp$Rate,temp$Hospital.Name),]
          
          if(num =="worst") {
            temp<-temp[order(temp$Hospital.Name),]
            sort<- temp[order(temp$Rate,decreasing = T),]
            choose <- sort$Hospital.Name[1]
            chrate <- sort$Rate[1]
          } else if (num == "best"){
            choose<-sort$Hospital.Name[1]
            chrate <- sort$Rate[1]
          }
          else {
            choose<-sort$Hospital.Name[num]
            chrate <- sort$Rate[num]
          }        
      
      
    }else stop("invalid outcome")

  h[state] <- choose
  s[state] <- allstates[state]
  r[state] <- chrate
  
  }
      x <- data.frame("Hospital.Name" = h,"State" = s, "Rate" = r)
      x <- x[order(x$State,x$Hospital.Name),]
      y <- data.frame("hospital" = x$Hospital.Name,"state" = x$State)

return(y)

}