

var fIdxArrHandler = {
  get: function (target, prop, receiver) {
    if (typeof prop === 'symbol' || isNaN(+prop))
      return target[prop]
    if (+prop <= 0)
      throw new RangeError("Fortran Arrays start at index 1")
    return target[prop - 1]
  },

  set: function (target, prop, value, receiver) {
    if (+prop <= 0)
      throw new RangeError("Fortran Arrays start at index 1")
    target[prop - 1] = value
  }
}

function FortranIndex(a) {
  if (typeof a === 'number') {
    let arr = []
    return new Proxy(arr, {
      get: function (target, prop, receiver) {
        if (typeof prop === 'symbol' || isNaN(+prop))
          return target[prop]
        if (+prop <= 0)
          throw new RangeError("Fortran Arrays start at index 1")
        if (typeof arr[prop - 1] !== 'undefined')
          return arr[prop - 1]
        else
          return a
      },

      set: function (target, prop, value, receiver) {
        if (+prop <= 0)
          throw new RangeError("Fortran Arrays start at index 1")
        target[prop - 1] = value
      }
    })
  }

  return new Proxy(a, fIdxArrHandler)
}

/**
 * Copy a vector, x, to a vector, y.
 * Uses unrolled loops for increments equal to one.
 * (original author: jack dongarra, linpack, 3/11/78)
 *
 * @param       n       {number}        Number of elements in input vector
 * @param       dx      {Object}        Input vector, Array-like object
 * @param       incx    {number}        Storage spacing between elements of dx
 * @param       dy      {Object}        Input vector with n elements, Array-like object
 * @param       incy    {number}        Storage spacing between elements of dy
 */
function dcopy(n, dx, incx, dy, incy) {
  var i, ix, iy, m, mpl, n;
  var pc=0

  dx = FortranIndex(dx)
  dy = FortranIndex(dy)
  
  jump:
  while(true) {
    debugger;
    switch(pc)
    {
      case 0:
      if (n <= 0)
        return;

      if (incx === 1 && incy === 1) {
        pc=20;
        continue jump;
      }

      ix = 1;
      iy = 1;
      if (incx < 0) ix = ((-n+1) * incx) + 1;
      if (incy < 0) iy = ((-n+1) * incy) + 1;

      l10: for (i=1; i <= n; i++) {
        dy[iy] = dx[ix];
        ix = ix + incx;
        iy = iy + incy;
        continue l10;
      }
      return;

      case 20:
      m = n%7
      if (m === 0) {
        pc=40;
        continue jump;
      }

      l30: for (i=1; i <=m; i++) {
        dy[i] = dx[i]
        continue l30;
      }
      if (n < 7) return;
      
      case 40:
      mp1 = m + 1
      l50: for (i=mp1; i<=n; i+=7) {
        dy[i] = dx[i]
        dy[i + 1] = dx[i + 1]
        dy[i + 2] = dx[i + 2]
        dy[i + 3] = dx[i + 3]
        dy[i + 4] = dx[i + 4]
        dy[i + 5] = dx[i + 5]
        dy[i + 6] = dx[i + 6]
        continue l50;
      }
      return
      break jump;
    }
  } 
}



dy = [1,2]
dcopy(2, [-1,-2], 1, dy, 1)
console.log('dy is', dy)
