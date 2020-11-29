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

let snippet = require('../LIB/modelSnippet.js')
let model = require('../LIB/createModel')
let Index = require('../LIB/indices')
let integrate = require('../lsoda/integrate.js')
let optimizer = require('../subplex/subplex.js');

let trajMatch = function (params, data, covarTime, covarTemperature, t0, times, place, index, temp) {
  let deltaT = (1 / 52) * 365
  let estimated = []
  let solution
  let logTrans = temp[0]
  let logitTrans = temp[1]

  /* Change the parameters' scale  */
  params = model.toEstimationScale(params, logTrans, logitTrans)

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
  params = model.fromEstimationScale(params, logTrans, logitTrans)

  // console.log("TrajMatch worker is starting...");

  /* calculate log likelihood */
  function logLik (n,estimated) {
    if (typeof progress === 'function') progress();

    var likvalue = 0
    var loglik = 0
    var simHarranged = []
    var simH
    for (let i = 0; i < n; i++) {  // fortran array start at one => estimated[i+1]
      params[place[i]] = estimated[i+1]
    }

    /* Return parameters' scale to original */
    params = model.fromEstimationScale(params, logTrans, logitTrans)
    simH = integrate(params, t0, times, deltaT,covarTime, covarTemperature) // simH is  cumulative
    simHarranged[0] = simH[0]
    for ( let i = 1; i < simH.length; i++) {
      simHarranged[i] = simH[i] - simH[i - 1]
    }

    for (let i = 0; i < simHarranged.length; i++) {
      likvalue = snippet.dObs(params[Index.obsprob], simHarranged[i], data[i][1], 1)
      loglik = loglik + likvalue
    }
    // console.log(params,loglik)
    return -(loglik).toFixed(6)
  }
  // console.log("final",params, -solution[1])
  return[...params, -solution[1]]
}


module.exports = trajMatch
