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