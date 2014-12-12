pollutantmean <- function (directory, pollutant, id = 1:332){

## Get Files ## Isolate the pollutant data ## Calculate Mean
  
setwd(directory)

num_files <- length(id)

lst <- list.files()

date_sum <- vector("numeric", length = length(lst))
count <- vector("numeric", length = length(lst))

for(i in id) {
  temp_all <- read.csv(lst[i])
  bad <- is.na(temp_all[pollutant])
  temp_iso <- temp_all[pollutant]
  date_sum[i]<-sum(temp_iso[!bad])
  count[i]<-length(temp_iso[!bad])
  
}

final_mean<-sum(date_sum[date_sum!=0])/sum(count[count!=0])

return(final_mean)
  
}