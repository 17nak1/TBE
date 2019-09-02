
if (run %in% c(2)) {
  param <- paste0("gamma")
  lscale <- 0
  param_lims <- c(0,2)
  flag_bound <- 0
} else if (run %in% c(3)) {
  param <- paste0("omega")
  lscale <- 0
  param_lims <- c(0,10)
  flag_bound <- 2
} else if (run %in% c(4)) {
  param <- paste0("obsprob")
  lscale <- 0
  param_lims <- c(0,1)
  flag_bound <- 1
} else if (run %in% c(5)) {
  param <- paste0("lambda_l")
  lscale <- 0
  param_lims <- c(0,5e-1)
  flag_bound <- 1
} else if (run %in% c(6)) {
  param <- "lambda_n"
  lscale <- 0
  param_lims <- c(0,2e-3)
  flag_bound <- 1
} else if (run %in% c(7)) {
  param <- "lambda_a"
  lscale <- 0
  param_lims <- c(0,40)
  flag_bound <- 2
}  else if (run %in% c(8)) {
  param <- "alpha"
  lscale <- 0
  param_lims <- c(0,30e3)
  flag_bound <- 2
} else if (run %in% c(9)) {
  param <- "f_l"
  lscale <- 0
  param_lims <- c(0,1)
  flag_bound <- 1
} else if (run %in% c(10)) {
  param <- "f_n"
  lscale <- 0
  param_lims <- c(0,1)
  flag_bound <- 1
} else if (run %in% c(11)) {
  param <- "f_a"
  lscale <- 0
  param_lims <- c(0,1)
  flag_bound <- 1
} else if (run %in% c(12)) {
  param <- "kappa"
  lscale <- 0
  param_lims <- c(0,2)
  flag_bound <- 2
} else if (run %in% c(13)) {
  param <- "c"
  lscale <- 0
  param_lims <- c(0,1)
  flag_bound <- 1
} else if (run %in% c(14)) {
  param <- "T_min_l"
  lscale <- 0
  param_lims <- c(0,25)
  flag_bound <- 0
} 




 
if (run %in% c(0)) {
  ind_inc <- 0
  ind_mult <- 1
  s <- 0.03
} else {
  ind_inc <- 0
  ind_mult <- 1
  s <- 0.01
}
