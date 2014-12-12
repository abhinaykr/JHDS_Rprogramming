complete <- function (directory, id = 1:332){
  
##setwd(directory)
  
  num_files <- length(id)
  
  lst <- list.files("/Users/Abhinay/Documents/Coursera/Data_Science/datasciencecoursera/rprog-003/Assignment_1/specdata")
  
  output <- matrix(nrow=num_files, ncol=2, dimnames = list(c(1:num_files), c("ID", "nobs")))

l <-1
  
  for(i in id) {
    loc<-paste(directory,lst[i],sep="/")
    temp <- read.csv(loc)
    good <- complete.cases(temp)    
    temp_obs <- length(temp[good,][,2])
    temp_num <- i
    output[l,] = c(temp_num,temp_obs)
    temp<-l+1
    l<-temp
    
  }

df_final <- as.data.frame(output)
  
  return(df_final)
}
