/*
*  @file         dcopy.js
*                Copy a vector dx in dy.
*  @inputs
*   n             -number of elements in input vector(s)
*
*   dx            -double precision vector with n elements
*
*   incx          -storage spacing between elements of dx
*
*   dy            -double precision vector with n elements
*
*   incy          -storage spacing between elements of dy
*
*  @author       Nazila Akhavan
*  @date         Sep 2019
*  @references   Tom Rowan, Department of Computer Sciences, University of Texas at Austin
*                https://www.netlib.org/opt/
*/

let dcopy = function(n,dx,incx,dy,incy) {
   let ix,iy,m,mp1
      if(n <= 0) return 0
      if(incx === 1 && incy === 1) {
        m = n - 7 * Math.floor(n / 7)
        if( m === 0 ) {
          mp1 = m + 1
          for (let i = mp1; i <= n; i += 7) {
            dy[i] = dx[i]
            dy[i + 1] = dx[i + 1]
            dy[i + 2] = dx[i + 2]
            dy[i + 3] = dx[i + 3]
            dy[i + 4] = dx[i + 4]
            dy[i + 5] = dx[i + 5]
            dy[i + 6] = dx[i + 6]
          }
          return
        }
        for(i = 1; i <= m; i++) {
          dy[i] = dx[i]
        }
        if( n  <  7 ) return dy
      } 
      ix = 1
      iy = 1
      if(incx < 0) ix = (-n+1)*incx + 1
      if(incy < 0) iy = (-n+1)*incy + 1
      for (let i = 1; i <= n; i++) {
        dy[iy] = dx[ix]
        ix = ix + incx
        iy = iy + incy
      }
      return 
 }

module.exports = dcopy;