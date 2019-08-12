/**
 *  @file        TBEgenerateSobol.js
 *               Generates initial sets to start with in runTraj.js
 *               The methods are all based on using the function "sobolseq.js"  

 *  @autor       Nazila Akhavan, nazila@kingsds.network
 *  @date        July 2019
 */

let sobolSeq = require('./generate-sobol/sobolSeq.js')
let snippet = require('./modelSnippet.js')
let Index = require('./indices')
let fs = require('fs')

// generate = function(numberOfPoints, run, prevResults = null) {

// } // TODO
let numberOfPoints = 200
let run  = 12

// Set Lower/Upper bounds to use in sobolDesine
let statenames = snippet.statenames()
let ic = Array(statenames.length).fill(0)
let LowerBounds = Array(Index.names.length).fill(0)
let UpperBounds = []
if (run === 1) {
  LowerBounds[Index.p] = 2000
  LowerBounds[Index.omega] = 0.1
  LowerBounds[Index.delta] = 0.5
  LowerBounds[Index.mu_e] = 0.02618
  LowerBounds[Index.mu_ql] = 0.0068
  LowerBounds[Index.mu_el] = 0.001428
  LowerBounds[Index.mu_qn] = 0.0034
  LowerBounds[Index.mu_en] = 0.000476
  LowerBounds[Index.mu_qa] = 0.00136
  LowerBounds[Index.mu_ea] = 0.000408
  LowerBounds[Index.mu_h] = 1/365
  LowerBounds[Index.beta_nh] = 0.9 
  LowerBounds[Index.beta_hl] = 0.8
  LowerBounds[Index.beta_hn] = 0.8
  LowerBounds[Index.tau] = 0.1 
  LowerBounds[Index.lambda_l] = 0.005
  LowerBounds[Index.lambda_n] = 0.005
  LowerBounds[Index.lambda_a] = 0.005
  LowerBounds[Index.alpha] = 0.01
  LowerBounds[Index.f_l] = 0.1
  LowerBounds[Index.f_n] = 0.1
  LowerBounds[Index.f_a] = 0.1 
  LowerBounds[Index.kappa] = 0.0001
  LowerBounds[Index.c] = 0.4
  LowerBounds[Index.Tf] = 4
  LowerBounds[Index.obsprob]= 0.2
  LowerBounds[Index.T_min_l]= 5
  LowerBounds[Index.gamma] = 1/100
                     
  UpperBounds = [].concat(LowerBounds)    
  UpperBounds[Index.omega] = 10
  UpperBounds[Index.tau] = 1
  UpperBounds[Index.lambda_l] = 0.1
  UpperBounds[Index.lambda_n] = 0.1
  UpperBounds[Index.lambda_a] = 0.1
  UpperBounds[Index.alpha] = 100
  UpperBounds[Index.f_l] = 1
  UpperBounds[Index.f_n] = 1
  UpperBounds[Index.f_a] = 1 
  UpperBounds[Index.kappa] = 0.1
  UpperBounds[Index.obsprob]=0.5
  UpperBounds[Index.T_min_l]=11
  UpperBounds[Index.gamma] = 0.5
  for( let i = 0; i < ic.length - 2; i++) {
    UpperBounds[statenames[i]] = 100
  }
  UpperBounds[statenames[ic.length - 2]] = 0.5
  UpperBounds[statenames[ic.length - 1]] = 0.5 
    
  let paramRanges = sobolSeq.sobolDesign(LowerBounds, UpperBounds, numberOfPoints)
  const createCsvWriter = require('csv-writer').createArrayCsvWriter;
  const csvWriter = createCsvWriter({
    header: [...Index.names],
    path: './ParamSet_TBE.csv'
  })   
  csvWriter.writeRecords(paramRanges)
    .then(() => {
    console.log('...Done')
  })    
} else {
  // Set Lower/Upper bounds of new parater set near the ones with the best result in the previous run  
  let prevResults = []
  let data = fs.readFileSync("./data/TBE_all.csv").toString()
  let lines = data.split('\n')
  for (let i = 0; i < lines.length - 1; i++) {
    prevResults.push(lines[i].split(','))
  }
  for ( let i = 0; i < prevResults[0].length; i++) { 
    if( prevResults[0][i] !== Index.names[i]) {
      throw " Inputs order in TBE_all.csv should be : " + Index.names
    }
  }
  let bestResult = prevResults[run - 1]
  let epsilon = 0.1
  LowerBounds[Index.p] = 2000
  LowerBounds[Index.omega] = bestResult[Index.omega]*(1-epsilon)
  LowerBounds[Index.delta] = 0.5
  LowerBounds[Index.mu_e] = 0.02618
  LowerBounds[Index.mu_ql] = 0.0068
  LowerBounds[Index.mu_el] = 0.001428
  LowerBounds[Index.mu_qn] = 0.0034
  LowerBounds[Index.mu_en] = 0.000476
  LowerBounds[Index.mu_qa] = 0.00136
  LowerBounds[Index.mu_ea] = 0.000408
  LowerBounds[Index.mu_h] = 1/365
  LowerBounds[Index.beta_nh] = 0.9 
  LowerBounds[Index.beta_hl] = 0.8
  LowerBounds[Index.beta_hn] = 0.8
  LowerBounds[Index.tau] = bestResult[Index.tau]*(1-epsilon) 
  LowerBounds[Index.lambda_l] = bestResult[Index.lambda_l]*(1-epsilon)
  LowerBounds[Index.lambda_n] = bestResult[Index.lambda_n]*(1-epsilon)
  LowerBounds[Index.lambda_a] = bestResult[Index.lambda_a]*(1-epsilon)
  LowerBounds[Index.alpha] = bestResult[Index.alpha]*(1-epsilon)
  LowerBounds[Index.f_l] = bestResult[Index.f_l]*(1-epsilon)
  LowerBounds[Index.f_n] = bestResult[Index.f_n]*(1-epsilon)
  LowerBounds[Index.f_a] = bestResult[Index.f_a]*(1-epsilon) 
  LowerBounds[Index.kappa] = bestResult[Index.kappa]*(1-epsilon)
  LowerBounds[Index.c] = 0.4
  LowerBounds[Index.Tf] = 4
  LowerBounds[Index.obsprob]= bestResult[Index.obsprob]*(1-epsilon)
  LowerBounds[Index.T_min_l]= bestResult[Index.T_min_l]*(1-epsilon)
  LowerBounds[Index.gamma] = bestResult[Index.gamma]*(1-epsilon)
   
  UpperBounds = [].concat(LowerBounds)    
  UpperBounds[Index.omega] = bestResult[Index.omega]*(1+epsilon)
  UpperBounds[Index.tau] = bestResult[Index.tau]*(1+epsilon)
  UpperBounds[Index.lambda_l] = bestResult[Index.lambda_l]*(1+epsilon)
  UpperBounds[Index.lambda_n] = bestResult[Index.lambda_n]*(1+epsilon)
  UpperBounds[Index.lambda_a] = bestResult[Index.lambda_a]*(1+epsilon)
  UpperBounds[Index.alpha] = bestResult[Index.alpha]*(1+epsilon)
  UpperBounds[Index.f_l] = Math.min(bestResult[Index.f_l]*(1+epsilon),1)
  UpperBounds[Index.f_n] = Math.min(bestResult[Index.f_n]*(1+epsilon),1)
  UpperBounds[Index.f_a] = Math.min(bestResult[Index.f_a]*(1+epsilon),1)
  UpperBounds[Index.kappa] = bestResult[Index.kappa]*(1+epsilon)
  UpperBounds[Index.obsprob] =bestResult[Index.obsprob]*(1+epsilon)
  UpperBounds[Index.T_min_l] = bestResult[Index.T_min_l]*(1+epsilon)
  UpperBounds[Index.gamma] = bestResult[Index.gamma]*(1-epsilon)
  for( let i = 0; i < ic.length - 2; i++) {
    UpperBounds[statenames[i]] = 100
  }
  UpperBounds[statenames[ic.length - 2]] = 0.5
  UpperBounds[statenames[ic.length - 1]] = 0.5    
  let paramRanges = sobolSeq.sobolDesign(LowerBounds, UpperBounds, numberOfPoints)
     
  const createCsvWriter = require('csv-writer').createArrayCsvWriter;
  const csvWriter = createCsvWriter({
    header: [...Index.names],
    path: './ParamSet_run' + run +'.csv'
  })   
  csvWriter.writeRecords(paramRanges)
    .then(() => {
    console.log('...Done')
  })
}


