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

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  cdata <- numeric()
  startdir <- getwd()
  setwd(directory)
  filelist <- list.files()
  for (i in filelist) {
    tdata <- read.csv(i)
    vcases <- complete.cases(tdata)
    vobs = sum(vcases)
    if ((vobs >= threshold) & (vobs > 0)) cdata <- c(cdata,cor(tdata$nitrate[vcases],tdata$sulfate[vcases]))
  }
  setwd(startdir)
  return(cdata)
}