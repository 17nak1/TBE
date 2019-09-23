
/*
*  @file         dasum.js
*                dasum takes the sum of the absolute values.
*  @inputs
*   n            - number of elements in input vector.
*
*   dx           - double precision vector with n elements
* 
*   incx         - storage spacing between elements of dx
*
*  @author       Nazila Akhavan
*  @date         Sep 2019
*  @references   Tom Rowan, Department of Computer Sciences, University of Texas at Austin
*                https://www.netlib.org/opt/
*/

let dasum =  function (n, dx, incx) {
  das = 0
  dtemp = 0
  if(n < 0) return 0
  if(incx === 1) {//20
     m = n - 6 * Math.floor(n / 6)
    if( m !== 0 ) {//40
      for(let i = 1; i <= m; i++) {
        dtemp = dtemp + Math.abs(dx[i])
      }
      if( n < 6 ) {//60
        das = dtemp
        return das
      }
    } 
    mp1 = m + 1
    for(let i = mp1; i <= n; i += 6) {
      dtemp = dtemp + Math.abs(dx[i]) + Math.abs(dx[i + 1]) + Math.abs(dx[i + 2]) + Math.abs(dx[i + 3]) + Math.abs(dx[i + 4]) + Math.abs(dx[i + 5])
    }

  }
  else{
    ix = 0
    if(incx < 0) incx = -incx
    for (i = 0; i < n; i++) {
      if(dx[ix] || dx[ix] === 0) {
        dtemp = dtemp + Math.abs(dx[ix])
        ix = ix + incx
      }
    }
  }
    das = dtemp
      return das
}

module.exports = dasum;