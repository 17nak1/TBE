rm(list=ls())
mainDir <- getwd()
mainDir <- "~/Git/TBE/R2"
setwd(mainDir)
# 
#  loc <- file.path("/global/home/hpc4068/pomp_devel")
# 
# library("digest",lib.loc=loc)
# library("mvtnorm",lib.loc=loc)
# library("deSolve",lib.loc=loc)
# library("coda",lib.loc=loc)
# library("subplex",lib.loc=loc)
# library("nloptr",lib.loc=loc)
# library("pomp",lib.loc=loc)
# library("reshape2",lib.loc=loc)
# library("magrittr",lib.loc=loc)
# library("ggplot2",lib.loc=loc)
# library("Rcpp",lib.loc=loc)
# library("bindrcpp",lib.loc=loc)
# library("rlang",lib.loc=loc)
# library("R6",lib.loc=loc)
# library("glue",lib.loc=loc)
# library("tibble",lib.loc=loc)
# library("pkgconfig",lib.loc=loc)
# library("dplyr",lib.loc=loc)
# library("plyr",lib.loc=loc)

library(pomp)
library(ggplot2)
source("ModelSnippet.R")

run <- "all"

startTime <- 1991
endTime <- 2008
dt <- 0.005

params.noic <- c("p","omega","delta",
                 "mu_e","mu_ql","mu_el","mu_qn","mu_en","mu_qa","mu_ea","mu_h",
                 "beta_nh","beta_hl","beta_hn","gamma",
                 "alpha", "f_l","f_n","f_a",
                 "obsprob", "lambda_l", "lambda_n","lambda_a","kappa",
                 "Tf","c","T_min_l")
params.ic <- paste0(statenames,"0")