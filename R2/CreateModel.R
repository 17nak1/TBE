# Function creating the pomp model

source("ModelSnippet.R")

create_pomp_model <- function(data, covars, t0, 
                              dt=0.005, params.notransform=NULL) {
  dt <- dt
  covars <- subset(covars, select = c("time", "temperature"))

  trans1 <- c("p","omega","delta",
              "mu_e","mu_ql","mu_el","mu_qn","mu_en","mu_qa","mu_ea","mu_h",
              "beta_nh","gamma",
              "lambda_l","lambda_n","lambda_a","alpha","kappa","Tf","T_min_l",
              paste0(statenames[1:11],"0"))
  trans2 <- c("f_l","f_n","f_a","c")
  trans1 <- trans1[which(!(trans1 %in% params.notransform))]
  trans2 <- trans2[which(!(trans2 %in% params.notransform))]
  
  

  # Create the pomp model
  TBE <- pomp(
    data = subset(data, select = c("time", "reports")),
    times = "time",
    t0 = t0,
    covar = covars,
    tcovar = "time",
    zeronames = c("cases"),
    paramnames = c("p","omega","delta",
                   "mu_e","mu_ql","mu_el","mu_qn","mu_en","mu_qa","mu_ea","mu_h",
                   "beta_nh","beta_hl","beta_hn","gamma",
                   "obsprob", 
                   "lambda_l","lambda_n","lambda_a","alpha","f_l","f_n","f_a","kappa", "Tf", "c","T_min_l",
                   paste0(statenames,"0")),
    dmeasure = dObs,
    rmeasure = rObs,
    skeleton = vectorfield(skel),
    log.trans = trans1,
    logit.trans = trans2,
    fromEstimationScale  = function(params, log.trans, logit.trans,  ...){
      params[log.trans] <- exp(params[log.trans])
      params[logit.trans] <- plogis(params[logit.trans])
      params[c("H_s0","H_i0")] <- exp(params[c("H_s0","H_i0")]) / (1 + sum(exp(params[c("H_s0","H_i0")])))
      params
    },
    toEstimationScale = function(params, log.trans, logit.trans, ...){
      params[log.trans] <- log(params[log.trans])
      params[logit.trans] <- qlogis(params[logit.trans])
      params[c("H_s0","H_i0")] <- log(params[c("H_s0","H_i0")] / (1 - sum(params[c("H_s0","H_i0")])))
      params
    },
    initializer = rInit,
    statenames=c(statenames, "cases")
  )

  output <- list(model=TBE)
  print("Model created.")
  return(output)
}