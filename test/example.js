

// const Module = require('./lsoda.js');
// Module['onRuntimeInitialized'] = function() {
//   let param = [2000, 2.58466796875, 0.5, 0.02618, 0.0068 , 0.001428,  0.0034,  0.000476,  0.00136, 0.000408,  0.002739726027397, 0.9, 0.8, 0.8, 0.0351513671875, 0.0301416015625, 0.0410888671875, 65.140205078125, 0.61943359375, 0.27490234375, 0.66689453125, 0.01775810546875,  0.4, 4, 0.47744140625, 5.275390625, 0.082255859375]
  
//   console.log(param.length)
//   let state = [98.14453125, 71.38671875, 79.00390625, 52.44140625, 74.12109375, 56.73828125, 11.42578125, 34.27734375, 60.44921875, 23.53515625, 54.98046875, 0.09326171875, 0.02001953125, 0.0]
//   let deltaT = 365 / 52
//   let temperature = 2
//   let res = []
//   
//   let lsodaTem = Module.cwrap('run_me', 'number', [])
//   var nByte = 8
//   var length = 15 
//   var buffer = Module._malloc(length*nByte);
//   for ( let it = 0; it < 80; it += deltaT){
//     res.push([it,...state]) 
//     var buffer = Module._malloc(length*nByte);
//     lsodaTem(...state,buffer, length, it , temperature);
  
//     for (var i = 1; i < 1; i++) {
//       state[i - 1] = Module.getValue(buffer+i*nByte, 'double')
//       console.log("js",Module.getValue(buffer+i*nByte, 'double'));
//     }
//   }
//   const createCsvWriter = require('csv-writer').createArrayCsvWriter
//   let csvWriter = createCsvWriter({
//     header:[],
//     path:'./results.csv'
//   })
//   csvWriter.writeRecords(res)
    

// };


function interpolator(points) {
  var first, n = points.length - 1,
    interpolated,
    leftExtrapolated,
    rightExtrapolated;
  if (points.length === 0) {
    return function () {
      return 0
    }
  }
  if (points.length === 1) {
    return function () {
      return points[0][1]
    }
  }
  points = points.sort(function (a, b) {
    return a[0] - b[0]
  })
  first = points[0]
  last = points[points.length - 1]

  leftExtrapolated = function (x) {
    var a = points[0], b = points[1];
    return a[1] + (x - a[0]) * (b[1] - a[1]) / (b[0] - a[0])
  }
  interpolated = function (x, a, b) {
    console.log(a, b)
    return a[1] + (x - a[0]) * (b[1] - a[1]) / (b[0] - a[0])
  }
  rightExtrapolated = function (x) {
    var a = points[n - 1], b = points[n];
    return b[1] + (x - b[0]) * (b[1] - a[1]) / (b[0] - a[0])
  }
  return function (x) {
    var i
    if (x <= first[0]) {
      return leftExtrapolated(x)
    }
    if (x >= last[0]) {
      return last[1]
    }
    for (i = 0; i < n; i += 1) {
      if (x > points[i][0] && x <= points[i + 1][0]) {
        return interpolated(x, points[i], points[i + 1])
      }
    }
    return rightExtrapolated(x);
  }
}

  let x = interpolator([[1,3],[2,4]])
  let p=1.5
  console.log(x(p))