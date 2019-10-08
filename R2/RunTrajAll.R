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
runs <- c(1)

startTime <- 1991
endTime <- 2008


total.cores <-1# c(5e2)       # Total number of cores being used for the phase
no.points <-200# c(1e3)         # Number of points per region
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
    ParamSetFile <- paste0("resTBE3_200.csv")
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
                 method = c("subplex"),
                 est=c()) -> sets.traj#params.ic.fit,params.fit
      # logLik(sets.traj)
      # coef(sets.traj)
      # ss <- sets.traj@states
      # setwd("~/Git/TBE")
      # nn= 1
      # write.csv(ss[14,],"cases.csv")
      # res <- read.csv("resall1.csv")
      # tt <-  sets.traj@times
      # E <-  c()
      # 
      # for( i in 1:937) { #937
      #     E[i] <-   res[i,nn + 1]
      # }
      # 
      # 
      # plot(E,col="red")
      # points(ss[nn,])
      # aa <- rbind(ss[nn,], E)
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
# }

  
#   
#   library(deSolve)
# 
#   LotVmod <- function (Time, y, Pars) {
#     with(as.list(c(y, Pars)), {
#       dx = y[1]*(alpha - beta*y[2])
#       dy = -y[1]*(gamma - delta*y[1])
#       dz = y[3]
#       return(list(c(dx, dy,dz)))
#     })
#   }
#   
#   parms <- c(alpha = 2/3, beta = 4/3, gamma = 1, delta = 1)
#   State <- c(x = 1, y = 1, z=1)
#   times <- seq(0, 10, by = 1)
# 
#   # out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = times))
#   # plot(out[-1,3])
# 
#   out <- lsoda(c(1, 1,1), times, LotVmod, parms, rtol = 1e-4,
#                atol = my.atol, hmax = Inf)
#   
# LotVmod1 <- function (Time, State, Pars) {
#   with(as.list(c(State, Pars)), {
#     dx = x*(alpha - beta*y)
#     dy = -x* (gamma - delta*x)
#     dz = z
#     return(list(c(dx, dy,dz)))
#   })
# }
# 
# Pars <- c(alpha = 2/3, beta = 4/3, gamma = 1, delta = 1)
# State1 <- c(x = 1, y = 1,z=1)
# Time <- seq(0, 6, by = 1)
# 
# out1 <- as.data.frame(ode(func = LotVmod1, y = State1, parms = Pars, times = Time, method = "lsoda"))
# #   plot(out[-1,3],type="l")
# #   points(exp(out1[-1,3]),col="red",type= "l")
# #   
# #  
#   #############################################################################
  # parms   <- c(k1 = 0.04, k2 = 1e4, k3 = 3e7)
  # my.atol <- c(1e-6,  1e-10,  1e-6)
  # times <- c()
  # for(i in 1:12) {
  #   times[i] = .4 * 10 ^(i-1)
  # }
  #
  #
  # lsexamp <- function(t, y, p) {
  #   with(as.list(c(y,p)), {
  #   yd1 <- -k1 * y[1] + k2 * y[2]*y[3]
  #   yd3 <- k3 * y[2]^2
  #   yd2 <- -yd1-yd3
  #   list(c(yd1, yd2, yd3))
  #   })
  # }
  #
  #   out <- lsoda(c(1, 0, 0), times, lsexamp, parms, rtol = 1e-4,
  #                atol = my.atol, hmax = Inf)
  # out
#   