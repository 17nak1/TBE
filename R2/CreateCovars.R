
create_covars <- function(startTime=1991, endTime=2016, stepsize=0.005) {
  
  
  time <- (seq(startTime,endTime+1,stepsize) - startTime)*365
  data <- read.csv("SzombathelyTempDaily1901to2015Monthly.csv", header=TRUE, colClasses=rep("numeric",3) )
  data[,"Time"] <- (data[,"Year"] + data[,"Month"] / 12 - startTime)*365
  data <- as.data.frame(data)
  temperature <- approx(data[,"Time"],data[,"Temperature"],time,method="linear",rule=2)$y
  covars <- cbind(time,temperature)
  covars <- as.data.frame(covars)
  
  
  return(covars)
}

