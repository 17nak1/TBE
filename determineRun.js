
let Index = require('./indices')
let determineRun = {}
determineRun.type = function(run) {
  let param, lscale, param_lims, flag_bound, ind_inc, ind_mult, s, final
  if (run === 2) {
    param = Index.gamma
    lscale = 0
    param_lims = [0,2]
    flag_bound = 0
  } else if (run === 3) {
    param = Index.omega
    lscale = 0
    param_lims = [0,10]
    flag_bound = 2
  } else if (run === 4) {
    param = Index.obsprob
    lscale = 0
    param_lims = [0,1]
    flag_bound = 1
  } else if (run === 5) {
    param = Index.lambda_l
    lscale = 0
    param_lims = [0,5e-1]
    flag_bound = 1
  } else if (run === 6) {
    param = Index.lambda_n
    lscale = 0
    param_lims = [0,2e-3]
    flag_bound = 1
  } else if (run === 7) {
    param = Index.lambda_a
    lscale = 0
    param_lims = [0,40]
    flag_bound = 2
  }  else if (run === 8) {
    param = Index.alpha
    lscale = 0
    param_lims = [0,30e3]
    flag_bound = 2
  } else if (run === 9) {
    param = Index.f_l
    lscale = 0
    param_lims = [0,1]
    flag_bound = 1
  } else if (run === 10) {
    param = Index.f_n
    lscale = 0
    param_lims = [0,1]
    flag_bound = 1
  } else if (run === 11) {
    param = Index.f_a
    lscale = 0
    param_lims = [0,1]
    flag_bound = 1
  } else if (run === 12) {
    param = Index.kappa
    lscale = 0
    param_lims = [0,2]
    flag_bound = 2
  } else if (run === 13) {
    param = Index.c
    lscale = 0
    param_lims = [0,1]
    flag_bound = 1
  } else if (run === 14) {
    param = Index.T_min_l
    lscale = 0
    param_lims = [0,25]
    flag_bound = 0
  } 
  
   
  if (run === 0) {
    ind_inc = 0
    ind_mult = 1
    s = 0.03
  } else {
    ind_inc = 0
    ind_mult = 1
    s = 0.01
  }
  final = {paramProf : param, lscale : lscale, param_lims : param_lims, flag_bound : flag_bound, ind_inc : ind_inc, ind_mult : ind_mult, s : s}
  return final
} 
module.exports = determineRun
