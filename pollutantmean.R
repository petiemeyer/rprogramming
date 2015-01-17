pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  startdir <- getwd()
  setwd(directory)
  for (i in id){
    padding <- ""
    if (i < 10) padding <- "00"
    else if (i < 100) padding <- "0"
    filename<-paste(padding,i,".csv", sep="")
    
    if (file.exists(filename)) {
      tdata <- read.csv(filename)
      if (!exists("cdata")) cdata <- tdata
      else cdata <- rbind(cdata, tdata)
    }
    else {
      errorstr = paste("error - file doesn't exist: ",filename)
      print(errorstr) 
    }
  }
  setwd(startdir)
  
  if (pollutant=="sulfate") pmean <- mean(cdata$sulfate, na.rm=TRUE)
  else if (pollutant == "nitrate") pmean <- mean(cdata$nitrate, na.rm=TRUE)
  else print("error")
  print(pmean)
}
