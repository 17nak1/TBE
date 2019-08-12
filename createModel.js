
/**
 *  @file        CreateModel.js
 *               Transforms parameters to and from the scale. 
 *                 
 *  @autor       Nazila Akhavan, nazila@kingsds.network
 *  @date        July 2019
 */
snippet = require("./modelSnippet.js")
mathLib = require ("./mathLib.js")
Index = require('./indices')

let createModel = {}
createModel.createPompModel = function(data, covars, t0, dt = 0.005, params_notransform = null) { 

  let statenames = snippet.statenames()
  trans1 = [Index.p,Index.omega,Index.delta, Index.mu_e,Index.mu_ql, Index.mu_el,
              Index.mu_qn,Index.mu_en,Index.mu_qa,Index.mu_ea,Index.mu_h, Index.beta_nh,
              Index.lambda_l,Index.lambda_n,Index.lambda_a,Index.alpha, Index.kappa, Index.Tf, Index.T_min_l, Index.gamma]
  for ( let i = 0; i < 11; i++) {
    trans1.push(statenames[i])
  }
  
  // Remove parameters in trans1 that exist in params_notransform.
  for ( let i = 0; i < trans1.length; i++) {
    for ( let j = 0; j < params_notransform.length; j++) {
      if ( trans1[i] === params_notransform[j]) {
        trans1.splice(i,1)
        i -= 1
      }
    }
  }

  trans2 = [Index.f_l,Index.f_n,Index.f_a,Index.c]
  // Remove parameters in trans2 that exist in params_notransform.
  for ( let i = 0; i < trans2.length; i++) {
    for ( let j = 0; j < params_notransform.length; j++) {
      if ( trans2[i] === params_notransform[j]) {
        trans2.splice(i,1)
        i -= 1
      }
    }
  }

  // Trans1 uses log and trans2 uses logit 
  return [trans1, trans2]

}  
createModel.fromEstimationScale = function(params, logTrans, logitTrans){
  for ( let i = 0; i < logTrans.length; i++) {
    params[logTrans [i]] = Math.exp(params[logTrans[i]])
  }
  for ( let i = 0; i < logitTrans.length; i++) {
    params[logitTrans [i]] = mathLib.plogis(params[logitTrans[i]])
  }      
  let sumH = Math.exp(params[Index.H_s]) + Math.exp(params[Index.H_i])
  params[Index.H_s] = Math.exp(params[Index.H_s]) / (1 + sumH)
  params[Index.H_i] = Math.exp(params[Index.H_i]) / (1 + sumH)
  return params
} 

createModel.toEstimationScale = function(params, logTrans, logitTrans){
  for ( let i = 0; i < logTrans.length; i++) {
    params[logTrans [i]] = Math.log(params[logTrans[i]])
  }
  for ( let i = 0; i < logitTrans.length; i++) {
    params[logitTrans [i]] = mathLib.qlogis(params[logitTrans[i]])
  }      
  let sumH = params[Index.H_s] + params[Index.H_i]
  params[Index.H_s] = Math.log(params[Index.H_s] / (1 - sumH))
  params[Index.H_i] = Math.log(params[Index.H_i] / (1 - sumH))
  return params
}   


  module.exports = createModel