rm(list=ls())

setwd("~/Git/TBE/R2")

library("digest")
library("mvtnorm")
library("deSolve")
library("coda")
library("subplex")
library("nloptr")
library("pomp")

mainDir <- getwd()
# setwd(mainDir)

# args <- (commandArgs(TRUE))
# job <- as.numeric(args[1])
job <- 1

# runs <- c(1,seq(3,13))
# runs <- c(1,5)
runs <- c(3)

startTime <- 1991
endTime <- 2008


total.cores <-1# c(5e2)       # Total number of cores being used for the phase
no.points <- 10#c(1e3)         # Number of points per region
est.icstart <- c(0)         # Are there no initial conditions given? 0-Given, 1-No, 2-TrajMatch

no.cores <- total.cores
job <- job - (ceiling(job/no.cores)-1)*no.cores


dt <- 0.005 # Step size

# Parameters that need to stay fixed
params.ic.fixed <- c()
params.fixed <- c("p", "delta",
                  "mu_e","mu_ql","mu_el","mu_qn","mu_en","mu_qa","mu_ea","mu_h",
                  "beta_nh","beta_hl","beta_hn", "gamma", "Tf", "c", params.ic.fixed,"alpha")
# Parameters that are not to be transformed
params.notrans <- params.fixed

source("ModelSnippet.R")

for (run in runs) {
  if (run==1) {
    ParamSetFile <- paste0("ParamSet_TBE.csv") 
    param.prof <- NULL  
  } else {
    ParamSetFile <- paste0("ParamSet_run",run,".csv")    
    source("DetermineRunType.R")
    param.prof <- param
  }  
  
  params.fixed <- c(params.fixed,param.prof)
  params.noic <- c("p","omega","delta",
                   "mu_e","mu_ql","mu_el","mu_qn","mu_en","mu_qa","mu_ea","mu_h",
                   "beta_nh","beta_hl","beta_hn","gamma","alpha", "f_l","f_n","f_a",
                   "obsprob", "lambda_l", "lambda_n", "lambda_a","kappa","Tf","c","T_min_l")
  params.ic <- paste0(statenames,"0")
  
  test <- c(rep(0.05,length(params.ic)+10))
  names(test) <- c(params.ic,paste0("a",seq(0,4)),paste0("b",seq(0,4)))
  test <- c(test, 
            p=2000, omega=0.1, delta=0.5,
            mu_e=0.002, mu_ql=0.006, mu_el=0.006, mu_qn=0.0021, mu_en=0.0021,
            mu_qa=0.0014, mu_ea=0.0014, mu_h=1/365,
            beta_nh=0.8, beta_hl=0.9, beta_hn=0.9, gamma=0.3,
            obsprob=0.2, alpha=0.1,f_l=0.1,f_n=0.1,f_a=0.1,
            lambda_l=1,lambda_n=1,lambda_a=1,kappa=0.5,Tf=4,c=0.5,T_min_l=1)  
  
  # Generate functions
  source("CreateModel.R")
  source("CreateCovars.R")
  source("CreateDataset.R")
  
  
  # Generate covars, data and pomp object
  
  covars <- create_covars(startTime, endTime, stepsize=dt)
  data <- create_dataset(startTime, endTime)
  out <- create_pomp_model(data, covars, t0=0, dt=dt, params.notransform=params.fixed) 
  
  po <- out$model
  params.fit <- params.noic
  params.ic.fit <- params.ic
  rm(out)
  
  if (length(which(params.noic %in% params.fixed))>0) {
    params.fit <- params.noic[-which(params.noic %in% params.fixed)]
  }
  if (length(which(params.ic %in% params.fixed))>0) {
    params.ic.fit <- params.ic[-which(params.ic %in% params.fixed)]
  }
  
  # Check that the transforms work
  coef(po) <- test
  test2 <- coef(po, transform=TRUE)
  po2 <- po
  coef(po2, transform=TRUE) <- test2
  print(all.equal(coef(po), coef(po2)))
  rm(test,test2,po2)
  
  
  
  # Load start parameters
  if (est.icstart >= 1) {
    select.set <- c(params.noic)
  } else {
    select.set <- c(params.noic, params.ic)
  }
  
  fullset <- subset(read.csv(file=ParamSetFile,header=TRUE), select=select.set)
  fullset <- fullset[1:no.points,]
  
  n <- ceiling(dim(fullset)[1]/no.cores)
  
  i_start <- 1 + (job-1)*n 
  i_end <-  n + (job-1)*n  
  if (job==no.cores) {i_end=dim(fullset)[1]} 
  
  subDir <- paste0("TBE_run",run)
  # Create folder
  
  if (file.exists(subDir)){
    setwd(file.path(mainDir, subDir))
  } else {
    dir.create(file.path(mainDir, subDir))
    setwd(file.path(mainDir, subDir))
  }
  
  
  currentset<-mat.or.vec(n,dim(fullset)[2])
  currentset <- as.data.frame(currentset)
  names(currentset) <- names(fullset)
  seeds <- ceiling(runif(n, min=1, max=2^30))
  index <- 0
  i=1
  for (i in i_start:i_end) {
    
    current.params <- unlist(fullset[i,])
    index <- index + 1
    try({
      
      coef(po) <- c(current.params)
      traj.match(po,
                 start=current.params,
                 transform=TRUE,
                 est=c()) -> sets.traj
      
      # Save and compute loglikelihood
      current.params <- coef(sets.traj)
      loglik.traj <- logLik(sets.traj)
      
      for(k in 1:length(names(current.params))){
        currentset[index,names(current.params)[k]]<- current.params[k]
      }
      currentset[index,"LogLik"] <- loglik.traj
      
    })
    if (index > 0) {
      write.csv(currentset,file=paste0("TBE_job", job, ".csv"),row.names=FALSE)    
    }
  }
  
  write.csv(currentset,file=paste0("TBE_job", job, ".csv"),row.names=FALSE)    
  setwd(mainDir)
}