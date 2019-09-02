/* Copyright (c) 2007 Massachusetts Institute of Technology
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/**
 *
 *  @file      sobolDesign.js         Generation of Sobol sequences in up to 1111 dimensions, based on the algorithms described in:
 *                                     P. Bratley and B. L. Fox, Algorithm 659, ACM Trans.Math. Soft. 14 (1), 88-100 (1988),
 *                                     as modified by:
 *                                     S. Joe and F. Y. Kuo, ACM Trans. Math. Soft 29 (1), 49-57 (2003).
 *
 *  @references                       https://github.com/kingaa/pomp/blob/master/src/sobolseq.c 
 * 
 *
 *                                      
 *
 *  @author     Nazila Akhavan
 *  @date       March 2019
 */



/* sobolData.js includes table of primitive polynomials and starting direction #'s generated from Fortran code 
 *  For more information please visit https://github.com/kingaa/pomp/blob/master/src/sobolseq.c
 */
sobolseq = {}
let sobolData = require('./sobolData.js');

rightZero32 = function (n) {
  const a = 0x05f66a47 // magic number, found by brute force.
  let decode = [0,1,2,26,23,3,15,27,24,21,19,4,12,16,28,6,31,25,22,14,20,18,11,5,30,13,17,10,29,9,8,7]
  n = ~n // change to rightmost-one problem.
  n = a * (n & (-n)) // store in n to make sure mult. is 32 bits. 
  n = n & 0xffffffff
  return decode[n >>> 27]
}

/*  generate the next term x_{n+1} in the Sobol sequence, as an array
 *  x[sdim] of numbers in (0,1).  Returns 1 on success, 0 on failure (if too many #'s generated)
 *  Note: n == 2^32 - 1 ... we would need to switch to a 64-bit version to generate more terms.
 */
sobolGen = function (sd, xx) {
  let c, b, sdim 
  if(sd.n == 4294967295) {
    return 0
  }

  c = rightZero32(sd.n++)
  sdim = sd.sdim
  for(i = 0; i < sdim; ++i){
    b = sd.b[i]
    if (b >= c) {
      sd.x[i] ^= sd.m[c][i] << (b - c)
      xx[i] = (sd.x[i]) / (1 << (b + 1))
    } else {
      sd.x[i] = (sd.x[i] << (c - b)) ^ sd.m[c][i]
      sd.b[i] = c
      xx[i] = (sd.x[i]) / (1 << (c + 1))
    }  
  }
  return 1
}

/* NLopt API to Sobol sequence creation, which hides soboldata structure
   behind an opaque pointer */

nloptSobolCreate = function (sdim) {
  let s = {
    sdim : sdim,
    m    : new Array(32),
    x    : new Array(sdim),
    b    : new Array(sdim),
    n    : 0
  }
  let a, d
  if(sdim === undefined || sdim > sobolData.MAXDIM) {
    return undefined
  }
  for (j = 0; j < 32; ++j) {
    s.m[j] = new Array(sdim)
    s.m[j][0] = 1 // special-case Sobol sequence 
  }
  for (i = 1; i < sdim; ++i) {
    a = sobolData.BinaryCoef[i - 1]
    d = 0
    while (a) {
      ++d
      a >>= 1
    }
    d-- // d is now degree of poly 

    // set initial values of m from table 
    for (j = 0; j < d; ++j) {
      s.m[j][i] = sobolData.mInitial[j][i - 1]
    }
    // fill in remaining values using recurrence 
    for (j = d; j < 32; ++j) {
      a = sobolData.BinaryCoef[i - 1]
      s.m[j][i] = s.m[j - d][i]
      for (k = 0; k < d; ++k) {
        s.m[j][i] ^= ((a & 1) * s.m[j - d + k][i]) << (d - k)
        a >>= 1
      }
    }
  }
  for (i = 0; i < sdim; ++i) {
    s.x[i] = 0
    s.b[i] = 0
  }
  return s
}    

/* if we know in advance how many points (n) we want to compute, then
 * adopt the suggestion of the Joe and Kuo paper, which in turn
 * is taken from Acworth et al (1998), of skipping a number of
 * points equal to the largest power of 2 smaller than n 
 */

nloptSobolSkip = function(s,n,x){
  let k = 1
  while(k * 2 < n){
    k *= 2
  }
  while(k-- > 0) {
    sobolGen(s, x[0])
  }
}

sobolSequence = function(dim, numberOfPoints){
  let s = nloptSobolCreate(dim)
  let sp = new Array(numberOfPoints).fill(null).map(() => Array(dim))

  if (s === undefined) {
    throw 'Dimension is too high!'
    return undefined
  }
  
  nloptSobolSkip(s,numberOfPoints,sp)
  for (k = 1; k < numberOfPoints; k++) {
    sobolGen(s, sp[k])
  }
  return sp
}

sobolseq.sobolDesign = function(lowerBounds, upperBounds, numberOfPoints) {
  let dim = Object.values(lowerBounds).length
  let result = new Array(numberOfPoints).fill(null).map(() => Array(dim))
  let sobolresult = sobolSequence(dim, numberOfPoints)
  var diff
  if(Object.values(lowerBounds).length != Object.values(upperBounds).length) {
    throw 'LowerBounds and UpperBounds should have same size.'
  }
  for(i = 0; i< dim; i++) {
    if (Object.keys(lowerBounds)[i] !== Object.keys(upperBounds)[i]) {
      throw 'Names of LowerBounds and UpperBounds do not match!'
    }
  }

  for(j = 0; j < dim; ++j){
    diff = (Object.values(upperBounds)[j] - Object.values(lowerBounds)[j])
    for(i = 0; i < numberOfPoints; i++) {
      result[i][j] = Object.values(lowerBounds)[j] +  diff * sobolresult[i][j]
    }
  }
  return result
}

module.exports = sobolseq