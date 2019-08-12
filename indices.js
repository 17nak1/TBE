
// Indices and names of all parametrs
const Index = {
  p : 0,
  omega : 1,
  delta : 2,
  mu_e : 3,
  mu_ql : 4,
  mu_el : 5,
  mu_qn : 6,
  mu_en : 7,
  mu_qa : 8,
  mu_ea : 9,
  mu_h : 10,
  beta_nh : 11,
  beta_hl : 12,
  beta_hn : 13,
  // tau : 14,
  lambda_l : 14,
  lambda_n : 15,
  lambda_a : 16,
  alpha : 17,
  f_l : 18,
  f_n : 19,
  f_a : 20,
  kappa : 21,
  c : 22,
  Tf : 23,
  obsprob : 24,
  T_min_l : 25,
  gamma : 26,
  E : 27,
  QL : 28,
  EL_s : 29,
  EL_i : 30,
  QN_s : 31,
  QN_i : 32,
  EN_s : 33,
  EN_i : 34,
  QA_s : 35,
  QA_i : 36,
  EA : 37,
  H_s : 38,
  H_i : 39,
  LogLik : 40
}
// Index.names = ['p', 'omega', 'delta', 'mu_e', 'mu_ql', 'mu_el', 'mu_qn', 'mu_en', 'mu_qa', 'mu_ea', 'mu_h', 'beta_nh', 'beta_hl',
//                'beta_hn', 'tau', 'lambda_l', 'lambda_n', 'lambda_a', 'alpha', 'f_l', 'f_n', 'f_a', 'kappa', 'c', 'Tf', 'obsprob',
//                 'T_min_l', 'gamma', 'E0', 'QL0', 'EL_s0', 'EL_i0', 'QN_s0', 'QN_i0', 'EN_s0', 'EN_i0', 'QA_s0', 'QA_i0', 'EA0', 'H_s0', 'H_i0']

// Print parameter names based on the array of indicies.
Index.param = function(array) {
  for ( let i =0; i < array.length; i ++) {
    console.log(Object.keys(Index)[array[i]])
  }
  console.log(";")
}

module.exports = Index
