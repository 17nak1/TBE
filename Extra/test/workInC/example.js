// /* 
//  * example
//  */
 // const Module = require('./a.out.js');
//  let g
// Module['onRuntimeInitialized'] = function() {

//   var pointer = Module.addFunction(back, 'undefined');
//   var call = Module.cwrap('call', 'number', ['pointer', 'number', 'number', 'number', 'number']);
  
//   let input = Array(24).fill('number')
//   let lsoda = Module.cwrap('lsoda', 'Array', input)

// let iwork1 = iwork2 = iwork5 = iwork6 = iwork7 = iwork8 = iwork9 = 0,
//   rwork1 = rwork5 = rwork6 = rwork7 = 0,
//   neq = 3,
//   y = [1,1,1], 
//   rtol = [],
//   atol = [],
//   t = 0,
//   tout = 0,
//   itol = 2,
//   itask = 1,
//   istate = 1,
//   iopt = 0,
//   jt = 2

// rtol[0] = 0
// atol[0] = 0
// rtol[1] = rtol[3] = 1E-4
// rtol[2] = 1E-8
// atol[1] = 1E-6
// atol[2] = 1E-10
// atol[3] = 1E-6

// let fex = function (times, y, parms) {    
//     dx = y[1]*(parms[0] - parms[1]*y[2])
//     dy = -y[1]*(parms[3] - parms[4]*y[1])
//     dz = y[3]
//     return [dx, dy,dz]    
// }

// var func_map = {
//     0: fex
// };

// let alpha = 2/3
// let parms = [alpha, beta = 4/3, gamma = 1, delta = 1]
// let State = [x = 1, y = 1, z=1]
  
// console.log(lsoda(fex, neq, y, t, tout, itol, rtol, atol, itask, istate, iopt, jt,
//         iwork1, iwork2, iwork5, iwork6, iwork7, iwork8, iwork9,
//         rwork1, rwork5, rwork6, rwork7, 0))

// };

const Module = require('./lsoda.js');
Module['onRuntimeInitialized'] = function() {
  let lsoda = Module.cwrap('run', 'number', ['number'])

};
