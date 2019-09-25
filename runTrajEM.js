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

let newDate = new Date()
let snippet = require('./modelSnippet.js')
let model = require('./createModel')
let create  = require('./create.js')
let mathLib = require('./mathLib')
let Index = require('./indices')
let DetermineRun = require('./determineRun')
let fmin    = require('fmin')
let fs = require('fs')
const Module = require('./lsoda.js')
let Main = require('./subplex/subplex.js');
/************************************************************ Will be defined in ui ************************************************/
let dt = 0.005 // Step size only use in covar
let startTime = 1991
let endTime = 2008
let estIcstart = [0] // Are there no initial conditions given? 0-Given, 1-No, 2-TrajMatch
let run = 1; 


// Parameters that is consider always fixed
let paramsIcFixed = snippet.statenames()
let paramsFixed =[Index.p, Index.delta, Index.mu_e, Index.mu_ql, Index.mu_el, Index.mu_qn, Index.mu_en, Index.mu_qa, Index.mu_ea,
          Index.mu_h, Index.beta_nh, Index.beta_hl, Index.beta_hn, Index.alpha, Index.c, Index.Tf, Index.gamma]

// Parameters not to be transformed
// let paramsNotrans = [].concat(paramsFixed)  
              
let ParamSetFile, paramProf
if (run === 1) {
  ParamSetFile = "./R2/ParamSet_TBE3.csv" 
  paramProf = null 
} else {
  ParamSetFile = `ParamSet_run${run}.csv`    
  paramProf = DetermineRun.type(run).paramProf
}  

paramsFixed = [...paramsFixed, paramProf]
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


// let estimatedIndex = [...paramsFit, ...paramsIcFit]
let covars = create.covars(startTime, endTime, dt)
let data = create.dataset(startTime, endTime)
let temp = model.createPompModel(data, covars, 0, 0.005, paramsFixed)
temp = [...temp[0], ...temp[1]]
let estimatedIndex = temp.sort()
let index = Array(40).fill(0)
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
// let covars = create.covars(startTime, endTime, dt)
// let data = create.dataset(startTime, endTime)
let times = [0, data[0][0], data[data.length - 1][0]]

let t0 = times[0]
let dataStartTime = times[1]
let dataEndTime = times[2]


//only read the first test as an example
let params = []
for ( let i = 0; i < fullset[0].length; i++) {
  params.push(Number(fullset[1][i]))
}
// console.log(params)
/**************************************************************************************************************************************************/
function traj_match (data, params, times, index, place) {
  let deltaT = (1 / 52) * 365
  var tempIndex = 0
  var estimated = []
  var states = []
  var data1 = []
  var data2 = []
  var solution
  // Index of parameters that need to be transfered
  let temp = model.createPompModel(data, covars, 0, 0.005, paramsFixed)
  let logTrans = temp[0]
  let logitTrans = temp[1]
  // Change the parameters' scale 
  let toScale = model.toEstimationScale(params, logTrans, logitTrans)
 
  // Choose those that should be estimated.
  for (let i = 0; i < index.length; i++) {
    if (index[i] === 1 ) {
      estimated.push(toScale[i])
    }
  }

  // //* Optimizer function using Nelder Mead method
  Main.f = logLik
  Main.x0 = estimated;//console.log(estimated)
  Main.tol = 0.1
  Main.run()
  // console.log(estimated,logLik)
  // solution = fmin.nelderMead(logLik,estimated )
  // logLik (estimated)
  
  //* calculate log likelihood
  function logLik (n,estimated) {
    var likvalue = 0
    var loglik = 0
    var tLength = 936 
    var simHarranged = []
    
    for (let i = 0; i < n; i++) {
      params[place[i]] = estimated[i+1]
    }

    // Return parameters' scale to original
    params = model.fromEstimationScale(params, logTrans, logitTrans)
    
    var simH = integrate(params, tLength, deltaT)
    simHarranged[0] = simH[0]
    var aa = []
    for ( let i = 1; i < simH.length; i++) {
      simHarranged[i -1] = simH[i] - simH[i - 1]
      aa.push([i , simHarranged[i -1]])
    }

    for (let i = 0; i < simHarranged.length; i++) {
      likvalue = snippet.dObs(params[Index.obsprob], simHarranged[i], data[i][1], 1)
      loglik = loglik + likvalue
    }
    // console.log(params, loglik)
    return -(loglik).toFixed(6)
  }
  // return[params, -solution.fx]
}
 //* ODE solver
function integrate (params, tLength, deltaT) {
  let arr = []
  let buffer
  let N = snippet.rInit(params)  
  let inputArray = Array(40).fill('number')
  let nByte = 8
  let lengthBuffer = tLength + 1 // extra space for t0  

  lsodaTem = Module.cwrap('run_me', "number", inputArray)
  buffer = Module._malloc(lengthBuffer * nByte)
  lsodaTem(lengthBuffer, buffer, ...N, ...params, deltaT)
  for (var i = 0; i < lengthBuffer; i++) {
    arr.push(Module.getValue(buffer + i * nByte, 'double'))
  }
  return arr
}
 


/** Main program entry point */
function main() {
  traj_match (data, params, times, index, place)
  console.log((new Date - newDate)/1000)
}

/* Run main only when emscripten is ready */
Module.onRuntimeInitialized = main

/*emcc lsoda.c -o lsoda.js -s  EXPORTED_FUNCTIONS='["_run_me"]' -s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap","getValue"]' -s EXIT_RUNTIME=1
*/