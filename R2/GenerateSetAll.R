rm(list=ls())
# mainDir <- "C:/Users/Felicia/Dropbox/R/Pomp/TBE/"
mainDir <- getwd()
setwd(mainDir)
source("SetValues.R")

runs <- seq(5,10)
runs <- c(4,5,6,7,9,10)
runs <- c(6)


no.profile <- 100
no.points <- 1e3
tol <- 100
span.tol <- 100

for (run in runs) {
  setwd(mainDir)
  source("DetermineRunType.R")
  
  print(param)
  setwd(mainDir)
  
  file <- paste0("TBE_all.csv")
  print(file)
  dataset <- read.table(file, header=TRUE,sep=",")
  
  k1 <- ceiling(max(dataset[,"LogLik"],na.rm=T))
  dataset <- subset(dataset,subset=LogLik>k1-tol&LogLik<0)
  print (nrow(dataset))
        
        
  if (lscale==1) {
    param.array <- exp(seq(log(param_lims[1]),log(param_lims[2]),length=no.profile))
  } else {
    param.array <- seq(param_lims[1],param_lims[2],length=no.profile)
  }
        
  param.profile <- NULL
  for (q in 2:(length(param.array))) {
    ind1 <- which(dataset[,param]>=param.array[q-1] & dataset[,param]<=param.array[q])
    if (length(ind1)>0) {
      set1 <- dataset[ind1,]
      set1 <- set1 [order ( set1[,"LogLik"],decreasing=TRUE),]
      param.profile <- rbind(param.profile,set1[1,])
    }
  }
  print (nrow(param.profile))
        
  temp <- param.profile
  temp2 <- param.profile
  for (q in 1:ceiling(no.points/nrow(temp))) {
    if (ind_mult ==1) {
      if (ind_inc == -1) {
        temp2[,param] <- temp2[,param]/(1+s)
        param.profile <- rbind(param.profile,temp2)
      } else if (ind_inc == 1) {
        temp[,param] <- temp[,param]*(1+s)
        param.profile <- rbind(param.profile,temp)
      } else {
        if (q%%2==1) {
          temp2[,param] <- temp2[,param]/(1+s)
          param.profile <- rbind(param.profile,temp2)
        } else {
          temp[,param] <- temp[,param]*(1+s)
          param.profile <- rbind(param.profile,temp)
        }
      }
    } else {
        if (ind_inc == -1) {
          temp2[,param] <- temp2[,param]-s
          param.profile <- rbind(param.profile,temp2)
        } else if (ind_inc == 1) {
          temp[,param] <- temp[,param]+s
          param.profile <- rbind(param.profile,temp)
        } else {
          if (q%%2==1) {
            temp2[,param] <- temp2[,param]-s
            param.profile <- rbind(param.profile,temp2)
          } else {
            temp[,param] <- temp[,param]+s
            param.profile <- rbind(param.profile,temp)
          }
        }
    }
  }
  rm(temp,temp2)
  param.profile <- param.profile[1:no.points,]
  if (flag_bound==1) {
    param.profile[which(param.profile[,param]>1-1e-12),param] <-1-1e-12
    param.profile[which(param.profile[,param]< 1e-12),param] <- 1e-12
  } else if (flag_bound==2) {
    param.profile[which(param.profile[,param]< 1e-12),param] <- 1e-12
  }
  param.profile[,"gamma"] <- 1/3
  param.profile[,"mu_h"] <- 1/365
  param.profile[,"alpha"] <- 1660
  
  setwd(mainDir)
  ParamSetFile <- paste0("ParamSet_run",run,".csv")
  write.csv(param.profile,file=ParamSetFile,row.names=FALSE)
          
  if (lscale==0) {
    plot(param.profile[,param],param.profile[,"LogLik"], font.main=1)
  } else {
    plot(log10(param.profile[,param]),param.profile[,"LogLik"], font.main=1)
  }

}