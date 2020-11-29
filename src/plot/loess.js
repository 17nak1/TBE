
//adapted from the LoessInterpolator in org.apache.commons.math
function loess(pairs, bandwidth) {
  var xval = pairs.map(function(pair){return pair[0]});
  var yval = pairs.map(function(pair){return pair[1]});
  var res = loessfunction(xval, yval, bandwidth);
  return xval.map(function(x,i){return [x, res[i]]});
}

function loessfunction(xval, yval, bandwidth) {
    function tricube(x) {
        var tmp = 1 - x * x * x;
        return tmp * tmp * tmp;
    }

  var res = [];

  var left = 0;
  var right = Math.floor(bandwidth * xval.length) - 1;

  for(var i in xval)
  {
    var x = xval[i];

    if (i > 0) {
      if (right < xval.length - 1 &&
        xval[right+1] - xval[i] < xval[i] - xval[left]) {
          left++;
          right++;
      }
    }

    var edge;
    if (xval[i] - xval[left] > xval[right] - xval[i])
      edge = left;
    else
      edge = right;

    var denom = Math.abs(1.0 / (xval[edge] - x));

    var sumWeights = 0;
    var sumX = 0, sumXSquared = 0, sumY = 0, sumXY = 0;

    var k = left;
    while(k <= right)
    {
      var xk = xval[k];
      var yk = yval[k];
      var dist;
      if (k < i) {
        dist = (x - xk);
      } else {
        dist = (xk - x);
      }
      var w = tricube(dist * denom);
      var xkw = xk * w;
      sumWeights += w;
      sumX += xkw;
      sumXSquared += xk * xkw;
      sumY += yk * w;
      sumXY += yk * xkw;
      k++;
    }

    var meanX = sumX / sumWeights;
    var meanY = sumY / sumWeights;
    var meanXY = sumXY / sumWeights;
    var meanXSquared = sumXSquared / sumWeights;

    var beta;
    if (meanXSquared == meanX * meanX)
      beta = 0;
    else
      beta = (meanXY - meanX * meanY) / (meanXSquared - meanX * meanX);

    var alpha = meanY - beta * meanX;

    res[i] = beta * x + alpha;
  }

  return res;
}

// //Example

// function sequance (a,b,n = b-a+1) {
//   let step = (b - a) / (n - 1);
//   let seq = [];
//   for( let i = a; i <= b ; i += step){
//     seq.push(Number(i.toFixed(10)));
//   }
//   if(seq[seq.length - 1] !== b) {
//     seq.push(b);
//   }
//   return seq;
// }
// let pairs = []
// let p1 = sequance (1,100,100)
// let p2 = sequance(1,5,100)
// p2[5] = p2[6]=p2[7]=p2[8]=p2[9]=10
// p2[15] = p2[16]=p2[17]=p2[18]=10

// for(let i=0; i<p1.length; i++) {
//   pairs.push([p1[i],p2[i]])
// }
// let bandwidth = .9
// ll = loess_pairs(pairs, bandwidth)
// console.log("ll",ll)
module.exports  = loess; 