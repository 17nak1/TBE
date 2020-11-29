/*
*  @file         loess2.js
*                Local Polynomial Regression Fitting
*                return the object 
*  @reference    https://github.com/yongjun21/loess
*/
function loess2(data1, span1) {
  var data = {
        x: data1[0],
        y: data1[1]  
  }
  var { default: Loess } = require('loess')
  var options = {span: span1, band: 0.2, degree: 1, normalize:false}
  var model = new Loess(data, options)
  
  var fit = model.predict()
  return fit
}

module.exports  = loess2; 