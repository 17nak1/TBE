/**
 *  @file       runTraj.js        
 *              This function attempts to match trajectories of a model's deterministic skeleton to data.
 *              Trajectory matching is equivalent to maximum likelihood estimatedation under the assumption 
 *              that process noise is entirely absent, i.e., that all stochasticity is measurement error.
 *              Accordingly, this method uses only the skeleton and dmeasure components of a POMP model.
 *
 *  @author     Nazila Akhavan, nazila@kingsds.network
 *  @date       July 2019
 */

let snippet = require('./modelSnippet.js')
let model = require('./createModel')
let create  = require('./create.js')
let mathLib = require('./mathLib')
let Index = require('./indices')
let DetermineRun = require('./determineRun')
let fmin    = require('fmin')
let fs = require('fs')


let dt = 0.005 // Step size only use in covar
let startTime = 1991
let endTime = 2008
let estIcstart = [0] // Are there no initial conditions given? 0-Given, 1-No, 2-TrajMatch
let run = 1; 


// Parameters that is consider always fixed
let paramsIcFixed = snippet.statenames()
let paramsFixed =[Index.p, Index.delta, Index.mu_e, Index.mu_ql, Index.mu_el, Index.mu_qn, Index.mu_en, Index.mu_qa, Index.mu_ea,
          Index.mu_h, Index.beta_nh, Index.beta_hn, Index.beta_hl, Index.alpha, Index.c, Index.Tf, Index.gamma, ...paramsIcFixed]

// Parameters not to be transformed
let paramsNotrans = [].concat(paramsFixed)              
let ParamSetFile, paramProf
if (run === 1) {
  ParamSetFile = "./ParamSet_TBE1.csv" 
  paramProf = null 
} else {
  ParamSetFile = `ParamSet_run${run}.csv`    
  paramProf = DetermineRun.type(run).paramProf
}  

paramsFixed = [...paramsFixed, paramProf]

// Define all type of parameters
paramsNoic = [Index.p, Index.omega, Index.delta, Index.mu_e, Index.mu_ql, Index.mu_el, Index.mu_qn, Index.mu_en, Index.mu_qa,
              Index.mu_ea, Index.mu_h, Index.beta_nh, Index.beta_hl,Index.beta_hn, Index.lambda_l, Index.lambda_n, Index.lambda_a,
              Index.alpha, Index.f_l, Index.f_n, Index.f_a,  Index.kappa, Index.c,Index.Tf, Index.obsprob, Index.T_min_l,Index.gamma]
paramsIc  = snippet.statenames()

// paramsFit =  paramsNoic - paramfixed
let paramsFit = []
let flag = 0
for (let i = 0; i < paramsNoic.length; i++) {
  for ( let j = 0; j < paramsFixed.length; j++) {
    if ( paramsNoic[i] === paramsFixed[j]) {
      flag = 1
      break
    }
  }
  if(flag === 0) {
    paramsFit.push(paramsNoic[i])
  }
flag = 0
}

// paramsIcFit = paramsIc - paramfixed
let paramsIcFit = []
flag = 0
for (let i = 0; i < paramsIc.length; i++) {
  for ( let j = 0; j < paramsFixed.length; j++) {
    if ( paramsIc[i] === paramsFixed[j]) {
      flag = 1
      break
    }
  }
  if(flag === 0) {
    paramsIcFit.push(paramsIc[i])
  }
flag = 0
}

let index = Array(40).fill(0)
let estimatedIndex = [...paramsFit, ...paramsIcFit]
for ( let i = 0; i < estimatedIndex.length; i++) {
  index[estimatedIndex[i]] = 1
}
let place = estimatedIndex
let fullset = []
var set1 = fs.readFileSync(ParamSetFile).toString()
var lines = set1.split('\n')
for (let i = 0; i < lines.length; i++) {
  fullset.push(lines[i].split(','))
}

// Generate covars and data  
let covars = create.covars(startTime, endTime, dt)
let data = create.dataset(startTime, endTime)
let times = [0, data[0][0], data[data.length - 1][0]]

let t0 = times[0]
let dataStartTime = times[1]
let dataEndTime = times[2]


//only read the first test as an example
let params = []
for ( let i = 0; i < fullset[0].length; i++) {
  params.push(Number(fullset[1][i]))
}


traj_match (interpolTemperature, data, params, times, index, place)
function traj_match (interpolTemperature, data, params, times, index, place) {
  let deltaT = (1 / 52) * 365
  var tempIndex = 0
  var estimated = []
  var states = []
  var data1 = []
  var data2 = []
  var solution
  
  // Index of parameters that need to be transfered
  let temp = model.createPompModel(data, covars, t0 = 0, dt = 0.005, paramsNotrans)
  let logTrans = temp[0]
  let logitTrans = temp[1]
  
  // Change the parameters' scale 
  model.toEstimationScale(params, logTrans, logitTrans)
 

  // Choose those that should be estimated.
  for (let i = 0; i < index.length; i++) {
    if (index[i] === 1 ) {
      estimated.push(params[i])
    }
  }
  let DATE = new Date() 
  //* Optimizer function using Nelder Mead method
  // solution = fmin.nelderMead(logLik,estimated )
  logLik (estimated)
  console.log(new Date() - DATE)
  //* calculate log likelihood
  function logLik (estimated) {
    var likvalue = 0
    var loglik = 0
    var rho 
    var psi
    //var ar = []
    for (let i = 0; i < estimated.length; i++) {
      params[place[i]] = estimated[i]
    }

    // Return parameters' scale to original
    model.fromEstimationScale(params, logTrans, logitTrans)
    
    var simH = integrate(interpolTemperature, params, times, deltaT)
    for (let i = 0; i < simH.length; i++) {
      likvalue = snippet.dObs(params[Index.obsprob], simH[i], data[i][1], 1)//;ar.push([likvalue])
      loglik = loglik + likvalue
    }
    console.log(params, loglik)
  //   const createCsvWriter = require('csv-writer').createArrayCsvWriter;
  // const csvWriter = createCsvWriter({
  //   header: [],
  //   path: './logall.csv'
  // })   
  // csvWriter.writeRecords(ar)
  //   .then(() => {
  //   console.log('...Done')
  // })
    return [-(loglik).toFixed(6)]
  }
  // return[params, -solution.fx]
}

 //* ODE solver
function integrate (interpolTemperature, params, times, deltaT) {
  let steps = 2000 // Total number of steps in the each interval.
  let t0 = times[0]
  let dataStartTime = Number(times[1].toFixed(6))
  let dataEndTime = times[2]
  let arr = [] ,arr2 =[]
  let timetemp
  let Npre
  let dt, count
  let N = snippet.rInit(params)
  let casesPlace = N.length - 1
  let k = t0 
  let flag = 0

  dt = deltaT
  while ( flag === 0 ) {
    Npre = N
    for (let stp = 0; stp < steps; stp++) { 
      gam = stp / steps * dt
      N = mathLib.odeMethod('rkf45', snippet.skeleton, N, k + gam , 1 / steps * dt, params, interpolTemperature)
    }//console.log(k,N)
    timetemp = k
    k += dt
    
    if (k - 1e-8 > dataStartTime) {  // telorance of 1e-8 to cover the machine error.
      k = timetemp
      dt = dataStartTime - timetemp ;
      N = Npre
    }
    if (k >= dataStartTime) {  
      k = timetemp + dt
      flag = 1
      arr.push(N[casesPlace])
      arr2.push([k,...N])
    }
  }
  count = 0
  while (k < dataEndTime) {
    k2 = data[count + 1][0]
    if (k2 !== "undefined") {
      dt = k2 - data[count][0]
    } else {
      dt = deltaT
    }
    N[casesPlace] = 0
    for (let stp = 0; stp < steps; stp++) { 
      gam = stp / steps * dt
      N = mathLib.odeMethod('rkf45', snippet.skeleton, N, k + gam , 1 / steps * dt, params, interpolTemperature)
      H = N[casesPlace]
    }console.log(k,N)
    k = k2
    count++
    arr.push(N[casesPlace])
    arr2.push([k,...N])
  }
  const createCsvWriter = require('csv-writer').createArrayCsvWriter;
  const csvWriter = createCsvWriter({
    header: ['time','E0', 'QL0', 'EL_s0', 'EL_i0', 'QN_s0',
      'QN_i0', 'EN_s0', 'EN_i0', 'QA_s0', 'QA_i0','EA0', 'H_s0',  'H_i0','cases'],
    // ['p', 'omega', 'delta', 'mu_e', 'mu_ql', 'mu_el', 'mu_qn', 'mu_en', 'mu_qa', 'mu_ea', 'mu_h', 'beta_nh', 'beta_hl', 'beta_hn', 'lambda_l',
    //  'lambda_n', 'lambda_a','alpha', 'f_l', 'f_n', 'f_a', 'kappa', 'c', 'Tf', 'obsprob', 'T_min_l', 'gamma', 'E0', 'QL0', 'EL_s0', 'EL_i0', 'QN_s0',
    //   'QN_i0', 'EN_s0', 'EN_i0', 'QA_s0', 'QA_i0','EA0', 'H_s0',  'H_i0'],
    path: './resall1.csv'
  })   
  csvWriter.writeRecords(arr2)
    .then(() => {
    console.log('...Done')
  })
  return arr
}

// for ( let id = 0; id < params.length; id++) {
  
//   res.push(traj_match (interpolPopulation, interpolBirth, data, params[id], times, index))
// }



