complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  cdata <- data.frame()
  startdir <- getwd()
  setwd(directory)
  for (i in id) {
    padding <- ""
    if (i < 10) padding <- "00"
    else if (i < 100) padding <- "0"
    filename<-paste(padding,i,".csv", sep="")
    
    if (file.exists(filename)) {
      tdata <- read.csv(filename)
      cdata <- rbind(cdata, c(i, sum(complete.cases(tdata))))
    }
    else {
      errorstr = paste("error - file doesn't exist: ",filename)
      print(errorstr) 
    }
  }
  setwd(startdir)
  
  colnames(cdata) <- c("id", "nobs")
  cdata
}
