/**
 *  @file       runTrajEm.js        
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
let Module = require('./lsoda.js')
let optimizer = require('./subplex/subplex.js');
/************************************************************ Will be defined in ui ************************************************/
let dt = 0.005 // Step size only use in covar
let startTime = 1991
let endTime = 2008
let estIcstart = [0] // Are there no initial conditions given? 0-Given, 1-No, 2-TrajMatch
let run = 1; 
let t0 = 0;

/* Parameters that is consider always fixed */
let paramsIcFixed = snippet.statenames()
let paramsFixed =[Index.p, Index.delta, Index.mu_e, Index.mu_ql, Index.mu_el, Index.mu_qn, Index.mu_en, Index.mu_qa, Index.mu_ea,
          Index.mu_h, Index.beta_nh, Index.beta_hl, Index.beta_hn, Index.alpha, Index.c, Index.Tf, Index.gamma]

/*paramsNotrans := (paramsFixed)  */
              
let ParamSetFile, paramProf
if (run === 1) {
  ParamSetFile = "./ParamSet_TBE3.csv"
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

// paramsFit :=  paramsNoic - paramfixed
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
let dataUpload = []
let d = fs.readFileSync("./data/170705PseudoData.csv").toString()  
var lines = d.split('\n')
for (let i = 0; i < lines.length; i++) {
  dataUpload.push(lines[i].split(','))
}

let fullset = []
var set1 = fs.readFileSync(ParamSetFile).toString()
var lines = set1.split('\n')
for (let i = 0; i < lines.length; i++) {
  fullset.push(lines[i].split(','))
}

// Generate covars and data  
let covars = create.covars(startTime, endTime, dt);
let covarTime = [], covarTemperature = [];
for (let i = 0; i < covars.length; i++) {
  covarTime.push(covars[i][0])
  covarTemperature.push(covars[i][1])
}
let times = []
let data = create.dataset(startTime, endTime, dataUpload)
for (let i = 0; i < data.length; i++) {
  times.push(data[i][0])
}

let temp = model.createPompModel(data, covars, 0, 0.005, paramsFixed)
/* Index of parameters that need to be transfered */
let logTrans = temp[0]
let logitTrans = temp[1]
temp = [...temp[0], ...temp[1]]
let estimatedIndex = temp.sort()
let index = Array(40).fill(0)
for ( let i = 0; i < estimatedIndex.length; i++) {
  index[estimatedIndex[i]] = 1
}
let place = estimatedIndex
// /**************************************************************************************************************************************************/
function traj_match (data, covarTime, covarTemperature, params, times, t0, index, place) {
  let deltaT = (1 / 52) * 365
  var estimated = []
  var flagStates = 1
  var solution
  /* Change the parameters' scale  */
  params = model.toEstimationScale(params, logTrans, logitTrans,flagStates)

  /* Choose values that should be estimated */
  for (let i = 0; i < index.length; i++) {
    if (index[i] === 1 ) {
      estimated.push(params[i])
    }
  }
  /* Optimizer function using Nelder Mead method */
  optimizer.f = logLik
  optimizer.x0 = estimated
  optimizer.tol = 0.1

  solution = optimizer.run()
  for (let i = 0; i < optimizer.x0.length; i++) {
    params[place[i]] = solution[0][i]
  }
  params = model.fromEstimationScale(params, logTrans, logitTrans,flagStates)

  /* calculate log likelihood */
  function logLik (n,estimated) {
    var likvalue = 0
    var loglik = 0
    var tLength = 936 
    var simHarranged = []
    var simH
    for (let i = 0; i < n; i++) {  // fortran array start at one => estimated[i+1] 
      params[place[i]] = estimated[i+1]
    }

    /* Return parameters' scale to original */
    var simH = integrate(params, times, t0,covarTime, covarTemperature)
    simHarranged[0] = simH[t0]
    for ( let i = 1; i < simH.length; i++) {
      simHarranged[i] = simH[i] - simH[i - 1]
    }
  
    for (let i = 0; i < simHarranged.length; i++) {
      likvalue = snippet.dObs(params[Index.obsprob], simHarranged[i], data[i][1], 1)
      loglik = loglik + likvalue
    }
    console.log("ll",params,loglik)
    return -(loglik).toFixed(6)
  }
//   console.log(params, -solution[1])
  return[...params, -solution[1]]
}
 /* ODE solver using emscripten */
// function integrate (params, tLength, deltaT) {
function integrate (params, times, t0, covarTime, covarTemperature) {
  let arr = []
  let count
  let N = snippet.rInit(params)
    
  let inputArray = Array(42).fill('number') 
  let lsoda = Module.cwrap('run_me', "number", inputArray);
  let nByte = 8

  /* Make an empty buffer to carry the results from c to JS*/
  let lengthBuffer = times.length
  let buffer = Module._malloc(lengthBuffer * nByte)

  /* Allocate memory to send covarTime and covarTemperature arrays to C */
  let covarTimeLength = covarTime.length;
  let strArr1 = covarTime;
  let strArr2 = covarTemperature;

  let ptrCovarTime = Module._malloc(covarTimeLength * nByte);
  let ptrCovarData = Module._malloc(covarTimeLength * nByte);

  for (let i = 0; i < covarTimeLength; i++) {
      Module.setValue(ptrCovarTime + (i + 1) * nByte, strArr1[i], 'double');
      Module.setValue(ptrCovarData + (i + 1) * nByte, strArr2[i], 'double');
  }

  /* Allocate memory to send times array to C; always need t0 at times[0] */
  let dataTimeLength = times.length;
  let strArr3 = [t0].concat(times);
  let ptrTimes = Module._malloc(dataTimeLength * nByte);
  for (let i = 0; i < dataTimeLength; i++) {
      Module.setValue(ptrTimes + i * nByte, strArr3[i], 'double');
  }

  lsodaException = lsoda(lengthBuffer, buffer,ptrTimes, ptrCovarTime,ptrCovarData, ...N, ...params, covarTimeLength)
  for (var i = 0; i < lengthBuffer; i++) {
    arr.push(Module.getValue(buffer+i*nByte, 'double'))
  }
  Module._free(buffer)
  return arr
}
 

/** Main program entry point */
function main() {
  let resultSet = [], result

  for(let count = 1; count <= 1; count++) {
    var params = []
    for ( let i = 0; i < fullset[0].length; i++) {
      params.push(Number(fullset[count][i]))
    }
    try{
      result = traj_match (data, covarTime, covarTemperature, params, times, t0, index, place);  
      resultSet.push(result);
    }
    catch(e) {
      resultSet.push([...Array(params.length).fill(0), NaN]);
      console.error(e);
    }
  }
  const createCsvWriter = require('csv-writer').createArrayCsvWriter;
  const csvWriter = createCsvWriter({
    header: ['p' , 'omega' , 'delta' , 'mu_e' , 'mu_ql' , 'mu_el' , 'mu_qn' , 'mu_en' , 'mu_qa' , 'mu_ea' , 'mu_h' ,
     'beta_nh' , 'beta_hl' , 'beta_hn' , 'lambda_l' , 'lambda_n' , 'lambda_a' , 'alpha' , 'f_l' , 'f_n' , 'f_a' , 'kappa' , 
     'c' , 'Tf' , 'obsprob' , 'T_min_l' , 'gamma' , 'E0' , 'QL0' , 'EL_s0' , 'EL_i0' , 'QN_s0' , 'QN_i0' , 'EN_s0' , 'EN_i0' ,
      'QA_s0' , 'QA_i0' , 'EA0' , 'H_s0' , 'H_i0' , 'LogLik'],
    path: './resTBE3_200.csv'
  })   
  csvWriter.writeRecords(resultSet)
    .then(() => {
    console.log('...Done')
  })
}    


/* Run main only when emscripten is ready */
Module.onRuntimeInitialized = main

// /*emcc lsoda.c -o lsoda.js -s  EXPORTED_FUNCTIONS='["_run_me"]' -s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap","getValue"]' -s EXIT_RUNTIME=1
// */

// // if (!runFast) {
// //     solution = optimizer.run()
// //     for (let i = 0; i < optimizer.x0.length; i++) {
// //       params[place[i]] = solution[0][i]
// //     }
// //   } else {
// //     logLik(estimated.length, [null,...estimated])
// //   }*/