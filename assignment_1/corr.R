corr <- function (directory, threshold = 0) {
  c <- complete (directory,1:332)
  num_files <- length(1:332)
  
  
  lst <- list.files("/Users/Abhinay/Documents/Coursera/Data_Science/datasciencecoursera/rprog-003/Assignment_1/specdata")
  
  t <- vector()
  
  for (i in 1:332){
    cs <-c$nobs
    if(cs[i] > threshold){
      t[i] <- i
    }else{
      t[i] <- 0
    }
  }
  
  t <- t[t!=0]
  store <- vector() 
  l <- 1
  for (i in t){
    loc<-paste(directory,lst[i],sep="/")
    temp <- read.csv(loc)
    good <- complete.cases(temp)
    temp<-temp[good,]   
    store[l]<-cor(temp$sulfate,temp$nitrate)
    temp<-l+1
    l<-temp
  }
  
#  final <- store
  return(store)
  
}