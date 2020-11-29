/**
 *  @file       integrate.js        
 *              ODE solver(lsoda method) using emscripten/
 *
 *  @author     Nazila Akhavan, nazila@kingsds.network
 *  @date       July 2019
 */
let snippet = require('../LIB/modelSnippet.js')

let integrate = function (params, t0, times, deltaT, covarTime, covarTemperature) {
  let lsodaException = 0
  let arr = []
  let buffer
  let N = snippet.rInit(params)  
  let inputArray = Array(40).fill('number')
  let nByte = 8
  let lengthBuffer = times.length  

  lsodaTem = Module.cwrap('run_me', "number", inputArray)
  buffer = Module._malloc(lengthBuffer * nByte)

  /* Allocate memory and end covars' columns to C */
  let covarLength = covarTime.length;
  let covarTime_p = Module._malloc(covarLength * 8);
  let covarData_p = Module._malloc(covarLength * 8);
  for (let i = 0; i < covarLength; i++) {
      Module.setValue(covarTime_p + i * 8, covarTime[i], 'double');
      Module.setValue(covarData_p + i * 8, covarTemperature[i], 'double');
  }

  let timeAdd0 = [t0].concat(times);
  let ptrTimes = Module._malloc(timeAdd0.length  * 8);
  for (let i = 0; i < timeAdd0.length; i++) {
      Module.setValue(ptrTimes + i * 8, timeAdd0[i], 'double');
  }

  lsodaException = lsodaTem(lengthBuffer,covarLength, buffer,ptrTimes, covarTime_p,covarData_p, ...N, ...params)
  if(lsodaException < 0){
    throw 'lsoda steps taken before reaching tout'
  } 
  for (var i = 0; i < lengthBuffer; i++) {
    arr.push(Module.getValue(buffer + i * nByte, 'double'))
  }
  
  Module._free(buffer)
  Module._free(covarTime_p)
  Module._free(covarData_p)
  Module._free(ptrTimes)
  return arr;
}

module.exports = integrate
 