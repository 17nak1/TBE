
/**
 *  @file        modeSnippet.js
 *               Makes the model and its dependencies.
 *                 
 *  @autor       Nazila Akhavan, nazila@kingsds.network
 *  @date        June 2019
 */

let mathLib = require ('./mathLib.js')
let Index = require('./indices')

let snippet = {}

// dmeasure
snippet.dObs = function (obsprob, cases, reports, give_log) {
  var lik, liktemp 
  if (!isNaN(reports)) {
    if ( (reports > 0) && (cases === 0) ) {
      lik = (give_log) ? Math.log(1e-16) : 1e-16;
    } else {
      lik = mathLib.dpois(reports, cases * obsprob, 1)//;console.log(lik, reports, cases , obsprob)
    }
  } else {
    lik = (give_log) ? 0 : 1;
  }//console.log("final",lik)
  return lik
}


// rmeasure
snippet.rObs = function (obsprob, cases) {
  var reports = mathLib.rpois(obsprob*cases);
  return reports
}

// skeleton
snippet.skeleton = function (t, N, params, interpolateTemp) {
  let temperature = interpolateTemp(t)
  let [p, omega, delta, mu_e, mu_ql, mu_el, mu_qn, mu_en, mu_qa, mu_ea, mu_h, beta_nh, beta_hl, beta_hn, lambda_l, lambda_n,
   lambda_a, alpha, f_l, f_n, f_a, kappa, c, Tf, obsprob, T_min_l, gamma] = params    
  let [E, QL, EL_s, EL_i, QN_s, QN_i, EN_s, EN_i, QA_s, QA_i, EA, H_s, H_i] = N               

  let d_el = 0;
  if (temperature >= 8.4) {
    d_el = -0.00001 * Math.pow(temperature, 2) + 0.002 * temperature - 0.019
    if (d_el < 0) {
        d_el = 0
    }
  }
                 
  let d_ln = 0;
  if (temperature >= 7.4) {
    d_ln = 0.00003 *Math. pow(temperature, 2) + 0.00073 * temperature - 0.007
    if (d_ln < 0) {
      d_ln = 0
    }
  }
                 
  let d_na = 0;
  if (temperature >= 8.7) {
    d_na = - 0.000008 * Math.pow(temperature, 2) + 0.0019 * temperature - 0.016
    if (d_na < 0) {
      d_na = 0
    }
  }
                 
  let d_pop = 0;
  if (temperature >= 4) {
    d_pop = -0.00001867 * Math.pow(temperature, 3) + 0.0008724 * Math.pow(temperature, 2) - 0.006195 * temperature + 0.01802
    if (d_pop < 0) {
      d_pop = 0
    }
  }
                 
  let a_l = 0;
  let pQL = 0;
  if (temperature >= T_min_l)  {
    pQL =  1;
    if (pQL<0) {
      pQL = 0
    }
  }
  a_l = pQL * lambda_l;
                 
  let a_n = 0;
  let pQN = 0;
  if (temperature >= 7)  {
    pQN = 1;
    if (pQN < 0) {
      pQN = 0
    }
  }
  a_n = pQN * lambda_n;          
                 
  let a_a = 0;
  let pQA = 0;
  if (temperature >= 7)  {
    pQA = 1;
    if (pQA < 0) { 
      pQA = 0
    }
  }
  a_a = pQA * lambda_a;
                 
  let lambda_hum = alpha * Math.exp(0.058 * temperature)
  let beta_n = kappa * lambda_hum * pQN
  let beta_a = lambda_hum * pQA
  let d = 1 - Math.pow((1 - c),Tf * a_n * QN_i)
  let DE, DQL, DEL_s, DEL_i, DQN_s, DQN_i, DEN_s, DEN_i, DQA_s, DQA_i, DEA, DH_s, DH_i, Dcases

  DE = p * delta * d_pop * Math.exp(-omega * delta * d_pop * EA) * EA - d_el * E - mu_e * E;
  DQL = d_el * E - a_l * QL - mu_ql * QL;
  DEL_s = (1 - d) * ((1 - beta_hl) * H_i + (1 - H_i)) * f_l * a_l * QL - d_ln * EL_s - mu_el * EL_s; 
  DEL_i = d * ((1 - beta_hl) * H_i + (1 - H_i)) * f_l * a_l * QL + beta_hl * f_l * a_l * QL * H_i - d_ln * EL_i - mu_el * EL_i; 
  DQN_s = d_ln * EL_s - a_n * QN_s - mu_qn * QN_s;
  DQN_i = d_ln * EL_i - a_n * QN_i - mu_qn * QN_i;
  DEN_s = (1 - d) * ((1 - beta_hn) * H_i + (1 - H_i)) * f_n * a_n * QN_s - d_na * EN_s - mu_en * EN_s;
  DEN_i = d *((1 - beta_hn) * H_i + (1 - H_i)) * f_n * a_n * QN_s + beta_hn * f_n * a_n * QN_s * H_i + f_n * a_n * QN_i - d_na * EN_i - mu_en * EN_i;
  DQA_s = d_na * EN_s - a_a * QA_s - mu_qa * QA_s;
  DQA_i = d_na * EN_i - a_a * QA_i - mu_qa * QA_i;
  DEA = f_a * a_a * (QA_s + QA_i) - d_pop * EA - mu_ea * EA;
  DH_s = mu_h - beta_nh * a_n * QN_i * H_s - mu_h * H_s;
  DH_i = beta_nh * a_n * QN_i * H_s - gamma * H_i - mu_h * H_i;
  Dcases = beta_n * QN_i + beta_a * QA_i;
  return [DE, DQL, DEL_s, DEL_i,DQN_s,DQN_i, DEN_s, DEN_i, DQA_s, DQA_i, DEA, DH_s, DH_i, Dcases]
}


snippet.statenames = function() {
   return [Index.E,Index.QL,Index.EL_s,Index.EL_i,Index.QN_s,Index.QN_i,Index.EN_s,Index.EN_i,Index.QA_s,Index.QA_i,Index.EA,Index.H_s,Index.H_i]
} 

snippet.rInit = function (params) {
  let E = params[Index.E],
  QL = params[Index.QL],
  EL_s = params[Index.EL_s],
  EL_i = params[Index.EL_i],
  QN_s = params[Index.QN_s],
  QN_i = params[Index.QN_i],
  EN_s = params[Index.EN_s],
  EN_i = params[Index.EN_i],
  QA_s = params[Index.QA_s],
  QA_i = params[Index.QA_i],
  EA = params[Index.EA],
  H_s = params[Index.H_s],
  H_i = params[Index.H_i],
  cases = 0
  
  return [E, QL, EL_s, EL_i, QN_s, QN_i, EN_s, EN_i, QA_s, QA_i, EA, H_s, H_i, cases]
}

module.exports = snippet