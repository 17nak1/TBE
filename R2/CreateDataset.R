
create_dataset <- function(startTime=1991, endTime=2016) {
  
  data <- read.csv("170705PseudoData.csv", header=TRUE, colClasses=rep("numeric",3) )
  
  t <- sort(rep(seq(1991,1997),52))
  
  
  add_data <- cbind(t,rep(seq(1,52),7),rep(NaN,52*7))
  # add_data <- cbind(rep(1997,52),seq(1,52),rep(NaN,52))
  colnames(add_data)<- c("Year","Week","Encephalitis")
  
  data <- rbind(add_data,data)
  
  data[,"Time"] <- (data[,"Year"] + data[,"Week"] / 52 - startTime)*365
  
  
  
  data <- as.data.frame(data[,c("Time","Encephalitis")])
  colnames(data) <- c("time","reports")
  
  
  
  return(data)
}