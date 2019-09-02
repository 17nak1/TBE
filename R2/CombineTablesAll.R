rm(list=ls())
mainDir <- getwd()
# mainDir <- "C:/Users/Felicia/Dropbox/R/Pomp/TBE"
setwd(mainDir)
source("SetValues.R")

runs <- c(seq(5,10),"all")
runs <- c("all")

tol <- 1e2
nmax <- 30e3

for (run in runs) {
  if (run=="all") {
    tol_save <- 2e3
  } else {
    tol_save <- 2e3
  }
  
  
  
  setwd(mainDir)
  if (run=="all") {
    subDir <- paste0("TBE_all")      
  } else {
    subDir <- paste0("TBE_run", run)      
  } 
  setwd(file.path(mainDir,subDir))
  if (exists("dataset")) rm(dataset);
  file_list=list.files();
  for (file in file_list){
    if (!exists("dataset")){
      x <- try(read.table(file, header=TRUE, sep=",",
                          colClasses=rep("numeric",length(params.noic)+length(params.ic)+1)))
      if(inherits(x, "try-error")) {
        dataset <-NULL
      }
      else {
        dataset <-x
      }
      rm(x)
    }
    else {
      x <- try(read.table(file, header=TRUE, sep=",",
                          colClasses=rep("numeric",length(params.noic)+length(params.ic)+1)))
      if(inherits(x, "try-error")) {
        temp_dataset <-NULL
      }
      else {
        temp_dataset <-x
      }
      rm(x)
      
      dataset<-rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }
  }
  ind <- which((!is.na(dataset[,"LogLik"])))
  print(length(ind))
  dataset <- dataset[ind,]
  
  ind <- which(!duplicated(dataset[,1:(length(params.ic)+length(params.noic))]))
  print("Not duplicates")
  print(length(ind))
  dataset <- dataset[ind,]
  
  dataset <- dataset[order(-dataset[,"LogLik"]),]
  
  dataset <- dataset[,c(params.noic,params.ic,
                        "LogLik")]
  
  ind <- which(dataset[,"LogLik"]<0)
  print("LogLik less than zero")
  print(length(ind))
  
  dataset <- dataset[ind,]
  
  
  k <- ceiling(max(dataset[,"LogLik"]))
  dataset <- subset(dataset,
                    subset=LogLik<0&LogLik>k-tol_save&obsprob<1&obsprob>0)
  print("Higher than minimum")
  print(nrow(dataset))
  
  if (nrow(dataset) > nmax & run=="all") {
    dataset <- dataset[1:nmax,]
  }
  if (nrow(dataset) > nmax) {
    dataset <- dataset[1:nmax,]
  }
  
  print(max(dataset[,"LogLik"]))
  if (run=="all") {
    setwd(file.path(mainDir))
    write.csv(dataset,file=paste("TBE_all.csv", sep=""),row.names=FALSE);  
    
  } else {
    setwd(file.path(mainDir,"TBE_all"))
    write.csv(dataset,file=paste("TBE_run",run,".csv", sep=""),row.names=FALSE)
    unlink(file.path(mainDir,subDir),recursive=TRUE)
  }
  setwd(mainDir)
  
  
}

# source("GenerateSetAll.R")