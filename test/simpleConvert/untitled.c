
#include <math.h>
#include <stdio.h>


/*//example1
float sqrtplus(float x1, float x2, float x3, float x4) {
  return sqrt(x1) + x2 + x3 + x4;
}

double sqrtplus(float *x) {
  printf("%f\n", x[1]);
  return x[1];
}
*/
/*emcc hello_function.c -s EXPORTED_FUNCTIONS='["_sqrtplus"]' -s EXTRA_EXPORTED_RUNTIME_METHODS='["ccall", "cwrap"]'
*/


//example2
void fillArray(double * a, int len)
{
  for (int i = 0; i<len; i++)
  {
    a[i] = 2.0 *i;
  }
  for (int j = 0; j < len; ++j)
  {
    printf("a[%d] = %f\n", j, a[j]);
  }
}



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



/* make 2 EMPTY buffers of double(nbyte =8) array of length 5 and send it to C. They are filled and getValue return results form C */

// Module['onRuntimeInitialized'] = function() {
  fillArray = Module.cwrap('fillArray', null, ['number', 'number']);
  let nByte = 8
  let length = 5 
  let buffer = Module._malloc(length*nByte);
  let aa= []

  fillArray(buffer, length);
  
  for (let i = 0; i < length; i++)
  {
    aa.push(Module.getValue(buffer+i*nByte, 'double'))
  }
  
  init = function() {
    return aa
  }
  console.log(init())
// }    

