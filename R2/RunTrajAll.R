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
runs <- c(2)

startTime <- 1991
endTime <- 2008


total.cores <-1# c(5e2)       # Total number of cores being used for the phase
no.points <-2# c(1e3)         # Number of points per region
est.icstart <- c(0)         # Are there no initial conditions given? 0-Given, 1-No, 2-TrajMatch

no.cores <- total.cores
job <- job - (ceiling(job/no.cores)-1)*no.cores


dt <- 0.005 # Step size

# Parameters that need to stay fixed
params.ic.fixed <- c()
params.fixed <- c("p", "delta",
                  "mu_e","mu_ql","mu_el","mu_qn","mu_en","mu_qa","mu_ea","mu_h",
                  "beta_nh","beta_hl","beta_hn","alpha", "c", "Tf","gamma", params.ic.fixed)
# Parameters that are not to be transformed
params.notrans <- params.fixed

source("ModelSnippet.R")

run = 1
# for (run in runs) {
  if (run==1) {
    ParamSetFile <- paste0("pn.csv") 
    param.prof <- NULL  
  } else {
    ParamSetFile <- paste0("ParamSet_run",run,".csv")    
    source("DetermineRunType.R")
    param.prof <- param
  }  

  params.fixed <- c(params.fixed,param.prof)
  params.noic <- c("p","omega","delta",
                   "mu_e","mu_ql","mu_el","mu_qn","mu_en","mu_qa","mu_ea","mu_h",
                   "beta_nh","beta_hl","beta_hn", "lambda_l", "lambda_n", "lambda_a","alpha", "f_l","f_n","f_a","kappa","c","Tf","obsprob","T_min_l","gamma")
  
  params.ic <- paste0(statenames,"0")
 
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
 
  
  # Load start parameters
  if (est.icstart >= 1) {
    select.set <- c(params.noic)
  } else {
    select.set <- c(params.noic, params.ic)
  }
  
  fullset <- subset(read.csv(file=ParamSetFile,header=TRUE))
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
                 transform=TRUE,
                 ode_control=list(method="lsoda"),
                 est=c()) -> sets.traj#params.ic.fit,params.fit
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
