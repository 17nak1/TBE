
const Module = require('./a.out.js');


////example1
// Module['onRuntimeInitialized'] = function() {
//   let sqrtplus = Module.cwrap('sqrtplus', 'number', ['number','number','number','number'])
//   console.log(sqrtplus(10,1,2,3))
// };
/* The first parameter is the name of the function to be wrapped,
 the second is the return type of the function (or a JavaScript null value if there isn’t one), 
 and the third is an array of parameter types (which may be omitted if there are no parameters). 
 The types are “number” (for a JavaScript number corresponding to a C integer, float, or general pointer),
  “string” (for a JavaScript string that corresponds to a C char* that represents a string) or 
  “array” (for a JavaScript array or typed array that corresponds to a C array; for typed arrays, it must be a Uint8Array or Int8Array)*/



f =function() {
  fillArray = Module.cwrap('fillArray', null, ['number', 'number']);
  var nByte = 8
  var length = 5 
  var buffer = Module._malloc(length*nByte);

  let g = function (buffer, length, nByte, l) {
    for (var i = 0; i < length; i++) {
      console.log(Module.getValue(buffer + i * nByte, 'double'));
    }
  }
  for (var i = 0; i < length; i++) { 
    fillArray(buffer, length,i)
    g (buffer, length, nByte,i)
  }
      
}    
 
/** Main program entry point */
function main() {
  f ()
}

/* Run main only when emscripten is ready */
Module.onRuntimeInitialized = main

