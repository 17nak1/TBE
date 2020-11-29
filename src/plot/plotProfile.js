/* 
* @file        plotProfile.js
*              for the given parameter(param) the function cleans the data set and return the 
*              best set of points(the bigest liklihood) with param in pram_limits.
*
*  @author     Nazila Akhavan, nazila@kingsds.network
*  @date       July 2019
*/
Index = require('../LIB/indices.js')
mathLib= require('../LIB/mathLib.js')

function plotProfile(dataset, param, param_lims) {

  let tol = 100, no_profile = 101, lscale = 0;
  let alpha_fit = 0.10, span_tol = 30;
  
  let theta = 0.2;
  let cutoff = 1.920729; // qchisq(p=0.95,df=1)/2
  
  let table_MLE = [], table_values = [];
  let rowLoglik, k1, dataSubset = [], param_array = [], param_profile = [];

    dataset.sort(mathLib.sortFunction);
    Maxloglik = dataset[0][Index.LogLik];
  
    k1 = Math.ceil(Maxloglik);
    dataSubset = [];
    for (let i = 0; i < dataset.length; i++) {
      rowLoglik = dataset[i][Index.LogLik];
      if(rowLoglik > (k1 - tol) && rowLoglik < 0) {
        dataSubset.push(dataset[i]);
      }
    }
  
    if (lscale === 1) {
      tempParam_array = sequance(Math.log(param_lims[0]), Math.log(param_lims[1]),no_profile);
      for (let i = 0; i < tempParam_array.length; i++) {
        param_array.push(Math.exp(tempParam_array[i]));
      }
    } else {
      param_array = sequance(param_lims[0],param_lims[1],no_profile);
    }
    
    for (q = 1; q < param_array.length; q++) {
      set1 = [];
      for (let i = 0; i < dataSubset.length; i++) {
        if (dataSubset[i][param] >= param_array[q-1] && dataSubset[i][param] <= param_array[q]) {
          set1.push(dataSubset[i]);
        }
      } 
      if (set1.length > 0) {
        set1.sort(mathLib.sortFunction);
        param_profile.push(set1[0]);
      }
    }
    if(param_profile.length === 0) return -1;
    param_profile.sort(mathLib.sortFunction)
    //TODO : define the best point here for the line in the plot
    mathLib.sortAscendingByKey(param_profile, param);
    let xplot = [param_profile[0][param]], yplot = [param_profile[0][Index.LogLik]];
    for (let i = 1; i < param_profile.length; i++) {
      if(param_profile[i-1][param] !== param_profile[i][param]) {
        xplot.push(Number(param_profile[i][param]));
        yplot.push(Number(param_profile[i][Index.LogLik]));
      }
    }
    return [xplot, yplot];
}
 

function sequance (a,b,n = 1) {
  let step = (b - a) / (n - 1);
  let seq = [];
  for( let i = a; i <= b ; i += step){
    seq.push(Number(i.toFixed(10)));
  }
  if(seq[seq.length - 1] !== b) {
    seq.push(b);
  }
  return seq;
}

module.exports  = plotProfile; 
