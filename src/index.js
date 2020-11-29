/**
 * @file         index.js
 *
 */

require('../www/styles/index.scss');
let generate = require('./generateSet/generateSets.js')
let sobolSeq = require('./generate-sobol/sobolSeq.js')
let snippet = require('./LIB/modelSnippet.js')
let model = require('./LIB/createModel')
let create  = require('./LIB/create.js')
let mathLib = require('./LIB/mathLib')
var index = require('./LIB/indices')
let DetermineRun = require('./LIB/determineRun')
let integrate = require('./lsoda/integrate.js')
let plotProfile = require('./plot/plotProfile.js')
let loess2 = require('./plot/loess2.js')

// Used to load emsdk module
const Module = window.Module = {};
const nameArray = ['p' , 'omega' , 'delta' , 'mu_e' , 'mu_ql' , 'mu_el' , 'mu_qn' , 'mu_en' , 'mu_qa' , 'mu_ea' , 'mu_h' ,
       'beta_nh' , 'beta_hl' , 'beta_hn' , 'lambda_l' , 'lambda_n' , 'lambda_a' , 'alpha' , 'f_l' , 'f_n' , 'f_a' , 'kappa' ,
       'c' , 'Tf' , 'obsprob' , 'T_min_l' , 'gamma' , 'E0' , 'QL0' , 'EL_s0' , 'EL_i0' , 'QN_s0' , 'QN_i0' , 'EN_s0' , 'EN_i0' ,
        'QA_s0' , 'QA_i0' , 'EA0' , 'H_s0' , 'H_i0' , 'LogLik'];
function start (workerFn) {
  let dataCovarUpload = [], dataCasesUpload = [];
  let covFlag = 0, caseFlag = 0;
  let data = [], covarTime = [], covarTemperature = [], times = [];
  let initalRefinPoints = [], indexSobol = [], estimateIndices = [];
  let temp , t0;
  let fixedIndices = [];
  let covars;
  let lowerBounds = [], upperBounds = [], flagBoundArray = [];
  let generateModelFlag = 0;
  let autoFlag;
  let refineIterationNumber;
  let bestResults = [];
  let simHarranged = [];
  let jobs = {};
  let param_lims = new Array(40).fill([0,100]);
  let bandwidth = 0.5;
  let lowerBoundsInit = [], upperBoundsInit = [];
  let plotIndex = [Index.omega, Index.lambda_l, Index.lambda_n, Index.lambda_a, Index.alpha, Index.f_l,
        Index.f_n, Index.f_a, Index.kappa, Index.obsprob, Index.T_min_l, Index.gamma];
  let indexPlot = ['$\\omega $', '$\\lambda_l $', '$\\lambda_n $', '$\\lambda_a $', '$\\alpha $', '$f_l $'
  , '$f_n $', '$f_a $', '$\\kappa $', 'obsprob', 'T_min_l', '$\\gamma $']   
  let timeExe = []   

  // Stub for compute.for, will be replaced with DCP API later.
  const runComputeFor = function(data, ...args) {
    // Local exec
    //  for (let i = 0; i < data.length; i++) {
    //   console.log(`Set ${i}`);
    //   eval(workerFn + `(
    //     ${JSON.stringify(data[i])},
    //     ${args.map(a => JSON.stringify(a)).join(',')}
    //   )`)
    // }
    // data = data.map(a => [a]);
    let job = dcp.compute.for(data, workerFn, args);
    job.on('ETOOMANYERRORS', (e) => console.error('Got an error:', e));
    job.work.on('uncaughtException', (e) => console.error('Got an error:', e));
    job.work.on('console', (e) => {
      if (e.level === 'debug') {
        timeExe.push(parseFloat(e.message));
        console.log("Got a timing message:", e);
      } else {
        console.log('got console:', e)
      }
    });
    job.on('accepted', () => console.log("Job accepted"));
    job.on('result', (ev) => console.log(ev));
    job.public = {
      name: 'TrajMatch: Tick-borne encephalitis',
      description: 'Estimating the tick-borne enecphalitis (TBE) infection risk for public health intervention strategies',
      link: 'https://www.ncbi.nlm.nih.gov/pubmed/31163042'
    }
    job.exec(0.00001).catch((e) => {
      console.warn('Job was cancelled', e);
      job.emit('myCustomError', e);
    });

    return job;
  }

  let req = new XMLHttpRequest()
  /* File picker for uploading dataCovarUpload.csv */
  var fileChooser = document.getElementById('tab2file1-upload')
  fileChooser.onclick = function () { this.value = '' }
  document.getElementById('tab2file1-upload').onchange = function () {
    document.getElementById('label-tab2file1').innerHTML = 'Uploaded'
    document.getElementById('label-tab2file1').style.backgroundColor = '#ffbf00'
    var file = this.files[0]
    dataCovarUpload = []
    var reader = new FileReader()
    reader.onload = function () {
      var lines = this.result.split('\n')
      for (var line = 1; line < lines.length; line++) {
        if(lines[line].length) {
          dataCovarUpload.push(lines[line].split(','))
        }
      }
    }
    reader.readAsText(file)
    covFlag=1;
    if(caseFlag ==1 ) {
      $('#cov-and-case').addClass('success');
      $('#cov-and-case').removeClass('failure');
      $('.initial-search-1, #generateModel').removeClass('disabled');
    }
  }

  function updateRefinementsGenerateBtn(modelGenerated, initialPointsUploaded) {
    if (modelGenerated && initialPointsUploaded) {
      if(!fixedIndices.includes(Index.omega))$('#buttonRunOmega').removeClass('disabled');
      if(!fixedIndices.includes(Index.lambda_l))$('#buttonRunLambda_l').removeClass('disabled');
      if(!fixedIndices.includes(Index.lambda_n))$('#buttonRunLambda_n').removeClass('disabled');
      if(!fixedIndices.includes(Index.lambda_a))$('#buttonRunLambda_a').removeClass('disabled');
      if(!fixedIndices.includes(Index.alpha))$('#buttonRunAlpha').removeClass('disabled');
      if(!fixedIndices.includes(Index.f_l))$('#buttonRunF_l').removeClass('disabled');
      if(!fixedIndices.includes(Index.f_n))$('#buttonRunF_n').removeClass('disabled');
      if(!fixedIndices.includes(Index.f_a))$('#buttonRunF_a').removeClass('disabled');
      if(!fixedIndices.includes(Index.kappa))$('#buttonRunKappa').removeClass('disabled');
      if(!fixedIndices.includes(Index.obsprob))$('#buttonRunObsprob').removeClass('disabled');
      if(!fixedIndices.includes(Index.T_min_l))$('#buttonRunTminL').removeClass('disabled');
      if(!fixedIndices.includes(Index.gamma))$('#buttonRunGamma').removeClass('disabled');
      $('#buttonRunAll').removeClass('disabled');
    }
  }

  /* File picker for uploading dataCasesUpload.csv */
  document.getElementById('tab2file2-upload').onchange = function () {
    document.getElementById('label-tab2file2').innerHTML = 'Uploaded'
    document.getElementById('label-tab2file2').style.backgroundColor = '#ffbf00'
    var file = this.files[0]
    dataCasesUpload = []
    var reader = new FileReader ()
    reader.onload = function () {
      var lines = this.result.split('\n')
      for (var line = 1; line < lines.length; line++) {
        if(lines[line].length) {
          dataCasesUpload.push(lines[line].split(','))
        }
      }
    }
    reader.readAsText(file)
    caseFlag = 1;
    if(covFlag ==1 ) {
      $('#cov-and-case').addClass('success');
      $('#cov-and-case').removeClass('failure');
      $('.initial-search-1, #generateModel').removeClass('disabled');
    }
  }
function dropHandlerCov(ev) {

  // Prevent default behavior (Prevent file from being opened)
  ev.preventDefault();
  dataCovar = [];
  if (ev.dataTransfer.items) {
    // Use DataTransferItemList interface to access the file(s)
    for (var i = 0; i < ev.dataTransfer.items.length; i++) {
      // If dropped items aren't files, reject them
      if (ev.dataTransfer.items[i].kind === 'file') {
        var file = ev.dataTransfer.items[i].getAsFile();
        var reader = new FileReader()
        reader.onload = function () {
          var lines = this.result.split('\n')
          for (var line = 1; line < lines.length; line++) {
            if(lines[line].length) {
              dataCovar.push(lines[line].split(','))
            }
          }
        }
        reader.readAsText(file)
      }
    }
  } else {
    // Use DataTransfer interface to access the file(s)

  }
  jQuery('#covariates-file').addClass('uploaded');
  covUploaded = 1;
  checkUploaded();
}
function dragOverHandlerCov(ev) {
console.log('File(s) in drop zone');

// Prevent default behavior (Prevent file from being opened)
ev.preventDefault();
}

  /* SECOND TAB: read values and pass them to sobol function */
  let generateModel = document.getElementById('generateModel')
  t0 = Number(tZero.value)

  let sobolBoundTable = document.getElementById('sobolBound')
  let rows = sobolBoundTable.querySelectorAll('tr')
  /* Read values from the "Initial Search" table and check if it is interval or fraction */
  generateModel.onclick = async function () {
    if ($('#generateModel').hasClass('disabled')) return false;

    lowerBoundsInit = [], upperBoundsInit = []
    fixedIndices = [], flagBoundArray = []
    for(i = 1; i < rows.length; i++) {
      let row = rows[i]
      let cols = row.querySelectorAll('td')
      let flagcol = cols[3].querySelector('.flag-bound')
      for(let j = 0; j < 3; j++) {
        if (flagcol.querySelectorAll('input')[j].checked)
          flagBoundArray.push(j)
      }
      let check = cols[1].querySelector('input#p-switch')
      if(check.checked) {
        range = row.querySelector('div#rng')
        lowerBoundsInit.push(Number(range.querySelector('input#from').value))
        upperBoundsInit.push(Number(range.querySelector('input#to').value))
      } else{
        paramValue = row.querySelector('div#val')
        pvalue = paramValue.querySelector('input').value
        if(pvalue.split('/').length == 2 ) {
          divisionresult = pvalue.split('/')
          pvalue = Number(divisionresult[0]) / Number(divisionresult[1])
        }
        fixedIndices.push(i-1)
        lowerBoundsInit.push(Number(pvalue))
        upperBoundsInit.push(Number(pvalue))
      }
    }
    if (autoSwitch.checked) {
      refineIterationNumber = document.getElementById("refineIteration").value;
      refineIterationNumber = Number(refineIterationNumber);
      autoFlag = 1;
    }
    if(!(dataCovarUpload.length  && dataCasesUpload.length)) {
        alert('Upload the data first!');
    } else {
      let estimatedTab = displayEstimetedTab(fixedIndices);
    
      /* Create data to use in trajMatch*/
      covars = create.covars(Number(startTime.value), Number(endTime.value), Number(dt.value), dataCovarUpload);
      data = create.dataset(Number(startTime.value), Number(endTime.value), dataCasesUpload);

      /* Create covarTime,covarTemperature, and times to use in trajMatch */
      covarTime = [], covarTemperature = [];
      for (let i = 0; i < covars.length; i++) {
        covarTime.push(covars[i][0]);
        covarTemperature.push(covars[i][1]);
      }
      times = [];
      for (let i = 0; i < data.length; i++) {
        times.push(data[i][0]);
      }
    }
    generateModelFlag = 1;
    $('.progress-item#model-ind').removeClass('failure');
    $('.progress-item#model-ind').addClass('success');
    $('#sobolButton').removeClass('disabled');
    $('#refinements-tab-').removeClass('disabled');

    updateRefinementsGenerateBtn(generateModelFlag, initalRefinPoints.length);
  }

  let runButtonSobol = document.getElementById('sobolButton');
  let downloadButtonSobol = document.getElementById('sobolButtonDownload');
  
/* Initial Search */
  runButtonSobol.onclick = function (cancelOnly=false) {
    
    if ($('#sobolButton').hasClass('disabled')) return false;
    if ($('#sobolButton').hasClass('running')) {
      jobs['sobol'].cancel();
      specialLog('#special-log-sobol', 'Cancelling job...');
      runButtonSobol.innerText = "Generate & Run";
      $('#sobolButton').removeClass('running');
      return;
    } else if (cancelOnly === 'cancelOnly') return;
    if (autoSwitch.style.checked) {

    }
    let SobolNumberOfPoints = Number(document.getElementById('sobolPoint').value);
    if(!generateModelFlag) {
        alert('Upload data and Model parameters first!');
    } else if (SobolNumberOfPoints === 0) {
      alert('Enter the number of initial points for the first search.')
    } else {
      generateModel.style.display = 'none';
      let area = document.querySelector('#special-log-sobol');
      area.value =''
      /* Index of parameters that need to be transfered */
      temp = model.createPompModel(data, covars, 0, dt.value, fixedIndices);
      estimateIndices = [...temp[0], ...temp[1]].sort();
      var index = Array(40).fill(0);
      for ( let i = 0; i < estimateIndices.length; i++) {
        index[estimateIndices[i]] = 1;
      }

      runButtonSobol.innerText = "Cancel";
      $('#sobolButton').addClass('running');
      downloadButtonSobol.style.display = '';
      let resSobol = [];

      /* Create an array of initial set of points := sobolSet
       * Use traj_match to calculate each row of sobolSet and add the results to sobolResult
       * Using DCP for each set
       */
      let sobolSet = sobolSeq.sobolDesign( lowerBoundsInit, upperBoundsInit, SobolNumberOfPoints);
      var accumulatedResults = [];
      // data:
      var job = jobs['sobol'] = runComputeFor(sobolSet, data, covarTime, covarTemperature, t0, times, estimateIndices, index, temp);
      job.on('myCustomError', (e) => {
        specialLog('#special-log-sobol', 'Failed to deploy job: ' + e.message);
      });
      
      specialLog('#special-log-sobol', 'Accessing Distributed Computer...');
      job.on('accepted', () => {
        specialLog('#special-log-sobol', 'Job accepted, working...');
      })
      let resultsRetrieved = 0;

      job.on('result', function(res) {
        // Show the results come back from the workers in consule and html.
        if(typeof res != 'undefined') {
          if(res.result) {
            resultsRetrieved++;
            if(res.result[0] !== 0) {
              accumulatedResults.push(res.result);
            }
            specialLog('#special-log-sobol', res, resultsRetrieved / sobolSet.length);
            $('#sobolButtonDownload').removeClass('disabled');

             downloadButtonSobol.onclick = function () {
              accumulatedResults.sort(mathLib.sortFunction);
               Csv(accumulatedResults, 'initial-results.csv');
             }
           }
        }
      });
      job.on('complete', async function(res) {
        await job.results.fetch();
        let iter = 1;
        res = job.results.values();
        console.log('onComplete', res, typeof res);
        if(autoFlag) {
          specialLog('#special-log-sobol', '\nInitial search complete!  Refining...');
        } else {
          specialLog('#special-log-sobol', '\nJob complete! Click download above for results.'); 
        }
        
        runButtonSobol.innerText = "Generate & Run"
        res = Array.from(res)
        res.sort(mathLib.sortFunction);
        res = res.filter((row) => row[0] !== 0 && row[row.length - 1] < 0);
        $('#buttonRunAll').removeClass('disabled');

        runButtonSobol.innerText = "Generate & Run";
        $('#sobolButton').removeClass('running');
        downloadButtonSobol.onclick = function () {
          Csv(res, 'initial-results.csv');
        }
        console.log("Got timing results:", timeExe);

        if(autoFlag) {
          Csv(res, 'initial-results.csv');
          initalRefinPoints = res;
          bestPoint = initalRefinPoints[0];
          updateRefinementsGenerateBtn(generateModelFlag, initalRefinPoints.length);
          for (let ii = 0; ii < refineIterationNumber; ii++) {              
            for(let i = 0; i < 12; i++) {   
              let bestValue = bestPoint[plotIndex[i]] ;  
              if(!fixedIndices.includes(plotIndex[i])){   
                document.getElementById('limit1'+ Object.keys(Index)[plotIndex[i]]).value = bestValue * 0.8;    
                if(plotIndex[i] === Index.f_l || plotIndex[i] === Index.f_n || plotIndex[i] === Index.f_a ||    
                plotIndex[i] === Index.c || plotIndex[i] === Index.obsprob) {   
                  document.getElementById('limit2'+ Object.keys(Index)[plotIndex[i]]).value = Math.min(bestValue * 1.2, 1);   
                } else {    
                  document.getElementById('limit2'+ Object.keys(Index)[plotIndex[i]]).value = bestValue * 1.2;    
                }   
              }   
            }
            await runAllButton.onclick();
            initalRefinPoints = BestResultsButton.onclick(iter);
            iter++
          }
          
        }
      });
    }
  }

  /* Upload initial set for refinements*/
  document.getElementById('tab4-upload').onclick = function (e) {
    this.value = '';
    if(!generateModelFlag) {
        e.preventDefault();
        alert('Upload the data and Model parameters then generate the model on the Initial Search page!');
    }
    else {
      document.getElementById('tab4-upload').onchange = function () {
      $('#buttonResults').removeClass('disabled');
      var file = this.files[0];
      initalRefinPoints = [];
      var reader = new FileReader ();
      reader.onload = function () {
        var lines = this.result.split('\n');
        // First row is just names. Skip first row, start at index 1.
        for (var line = 1; line < lines.length; line++) {
          if(lines[line].length) {
            initalRefinPoints.push(lines[line].split(','));
          }
        }
        for ( let i = 0; i <initalRefinPoints.length; i++) {
          for (let j = 0; j < initalRefinPoints[0].length; j++) {
            initalRefinPoints[i][j] = Number(initalRefinPoints[i][j]);
          }
        }
        bestResults = rmSameRow(initalRefinPoints);
        let bestPoint = bestResults[0];
        /* data in simHarranged in needed for ploting sample trajectory*/
        let simH = integrate(bestResults[0], t0, times, Number(dt.value),covarTime, covarTemperature) // simH is  cumulative
        simHarranged[0] = simH[0]
        for ( let i = 1; i < simH.length; i++) {
          simHarranged[i] = (simH[i] - simH[i - 1]) * bestPoint[Index.obsprob]
        }
        let dataSampleTraj = []
        for (let i = 0; i < data.length; i++) {
          if(!isNaN(data[i][1])) {
            dataSampleTraj.push([data[i][0]/365 + Number(startTime.value),data[i][1], simHarranged[i]])//time,data,simulation
          } 
        }
        trigerPlotTrajectory(dataSampleTraj, 'plot-sampleTraj')
        let bestPonitTable = document.getElementById('bestResult-table')
        let rows = bestPonitTable.querySelectorAll('tr')
        rows[0].querySelectorAll('td')[1].querySelector('input').value = bestPoint[Index.LogLik]
        let j = 0
        for(i = 1; i < rows.length; i++) {
          let row = rows[i]
          let cols = row.querySelectorAll('td')
          cols[1].querySelector('input').value = bestPoint[j]
          cols[3].querySelector('input').value = bestPoint[j+1]
          cols[5].querySelector('input').value = bestPoint[j+2]
          j += 3
        }
          
        /* Refreshing the plots in refinments*/ 
        for(let i = 0; i < 12; i++) {
          let bestValue = bestPoint[plotIndex[i]]
          if(!fixedIndices.includes(plotIndex[i])){
            document.getElementById('limit1'+ Object.keys(Index)[plotIndex[i]]).value = bestValue * 0.8;
            if(plotIndex[i] === Index.f_l || plotIndex[i] === Index.f_n || plotIndex[i] === Index.f_a || 
            plotIndex[i] === Index.c || plotIndex[i] === Index.obsprob) {
              document.getElementById('limit2'+ Object.keys(Index)[plotIndex[i]]).value = Math.min(bestValue * 1.2, 1);
            } else {
              document.getElementById('limit2'+ Object.keys(Index)[plotIndex[i]]).value = bestValue * 1.2;
            }
            lowerLimit = lowerBoundsInit[plotIndex[i]] 
            upperLimit = (upperBoundsInit[plotIndex[i]] < bestValue) ? ( 1.2 * bestValue): upperBoundsInit[plotIndex[i]];
            param_lims[plotIndex[i]] = [lowerLimit,upperLimit];
            trigerPlot(bestResults, plotIndex[i], 'plot'+Object.keys(Index)[plotIndex[i]], bandwidth, param_lims[plotIndex[i]],indexPlot[i])
          }
        }
        console.log('Plots have been refreshed')

        updateRefinementsGenerateBtn(generateModelFlag, initalRefinPoints.length);
      }
      reader.readAsText(file);
      document.getElementById('label-tab4').innerHTML = 'Uploaded';
      document.getElementById('label-tab4').style.backgroundColor = '#ffbf00';
      $('#ref-init-points').toggleClass('success');
      $('#ref-init-points').toggleClass('failure');
      }
    }
  }
  /*Download the csv file includes all the best results from all tabs. Also refresh plots based on this csv file*/ 
  let BestResultsButton = document.getElementById('buttonResults');
  BestResultsButton.onclick = function (iteration = 0) {
    bestResults = rmSameRow(bestResults);
    bestResults = bestResults.filter((row) => row[0] !== 0 && row[row.length - 1] < 0);
    bestPoint = bestResults[0];
    console.log('Plots have been refreshed')
    if(autoFlag) {
      Csv(bestResults, 'best-results'+ iteration + '.csv');
    } else {
      Csv(bestResults, 'best-results.csv');
    }
    for(let i = 0; i < 12; i++) {
      let bestValue = bestPoint[plotIndex[i]]
      if(!fixedIndices.includes(plotIndex[i])){
        document.getElementById('limit1'+ Object.keys(Index)[plotIndex[i]]).value = bestValue * 0.8;
        lowerLimit =  0.5 * bestValue
        if(plotIndex[i] === Index.f_l || plotIndex[i] === Index.f_n || plotIndex[i] === Index.f_a || 
        plotIndex[i] === Index.c || plotIndex[i] === Index.obsprob) {
          document.getElementById('limit2'+ Object.keys(Index)[plotIndex[i]]).value = Math.min(bestValue * 1.2, 1);
          upperLimit = (1.5 * bestValue < 1) ? ( 1.5 * bestValue): 1;
        } else {
          document.getElementById('limit2'+ Object.keys(Index)[plotIndex[i]]).value = bestValue * 1.2;
          upperLimit = 1.5 * bestValue
        }
        // lowerLimit = (lowerBoundsInit[plotIndex[i]] > bestValue) ? ( 0.8 * bestValue): lowerBoundsInit[plotIndex[i]];
        // upperLimit = (upperBoundsInit[plotIndex[i]] < bestValue) ? ( 1.2 * bestValue): upperBoundsInit[plotIndex[i]];
        param_lims[plotIndex[i]] = [lowerLimit,upperLimit];
        setTimeout(() => {
          trigerPlot(bestResults, plotIndex[i], 'plot'+Object.keys(Index)[plotIndex[i]], bandwidth, param_lims[plotIndex[i]], indexPlot[i])
        });
      }
    }
    
    /* data in simHarranged in needed for ploting sample trajectory*/
    let simH = integrate(bestResults[0], t0, times, Number(dt.value),covarTime, covarTemperature) // simH is  cumulative
    simHarranged[0] = simH[0]
    for ( let i = 1; i < simH.length; i++) {
      simHarranged[i] = (simH[i] - simH[i - 1]) * bestPoint[Index.obsprob]
    }
    let dataSampleTraj = []
    for (let i = 0; i < data.length; i++) {
      if(!isNaN(data[i][1])) {
        dataSampleTraj.push([data[i][0]/365 + Number(startTime.value),data[i][1], simHarranged[i]])//time,data,simulation
      } 
    }
    trigerPlotTrajectory(dataSampleTraj, 'plot-sampleTraj')
    let bestPonitTable = document.getElementById('bestResult-table')
    let rows = bestPonitTable.querySelectorAll('tr')
    rows[0].querySelectorAll('td')[1].querySelector('input').value = bestPoint[Index.LogLik]
    let j = 0
    for(i = 1; i < rows.length; i++) {
      let row = rows[i]
      let cols = row.querySelectorAll('td')
      cols[1].querySelector('input').value = bestPoint[j]
      cols[3].querySelector('input').value = bestPoint[j+1]
      cols[5].querySelector('input').value = bestPoint[j+2]
      j += 3
    }

    return bestResults;
  }
  let runAllButton = document.getElementById('buttonRunAll');
  let runButtonOmega = document.getElementById('buttonRunOmega');
  let downloadButtonOmega = document.getElementById('OmegaButtonDownload');
  let runButtonLambda_l = document.getElementById('buttonRunLambda_l')
  let downloadButtonLambda_l = document.getElementById('Lambda_lButtonDownload')
  let runButtonLambda_n = document.getElementById('buttonRunLambda_n')
  let downloadButtonLambda_n = document.getElementById('Lambda_nButtonDownload')
  let runButtonLambda_a = document.getElementById('buttonRunLambda_a')
  let downloadButtonLambda_a = document.getElementById('Lambda_aButtonDownload')
  let runButtonAlpha = document.getElementById('buttonRunAlpha')
  let downloadButtonAlpha = document.getElementById('AlphaButtonDownload')
  let runButtonf_l = document.getElementById('buttonRunF_l')
  let downloadButtonf_l = document.getElementById('F_lButtonDownload')
  let runButtonf_n = document.getElementById('buttonRunF_n')
  let downloadButtonf_n = document.getElementById('F_nButtonDownload')
  let runButtonf_a = document.getElementById('buttonRunF_a')
  let downloadButtonf_a = document.getElementById('F_aButtonDownload')
  let runButtonKappa = document.getElementById('buttonRunKappa')
  let downloadButtonKappa = document.getElementById('KappaButtonDownload')
  let runButtonObsprob = document.getElementById('buttonRunObsprob')
  let downloadButtonObsprob = document.getElementById('ObsprobButtonDownload')
  let runButtonT_min_l = document.getElementById('buttonRunTminL')
  let downloadButtonT_min_L = document.getElementById('TminLButtonDownload')
  let runButtonGamma = document.getElementById('buttonRunGamma')
  let downloadButtonGamma = document.getElementById('gammaButtonDownload')
  let autoSwitch = document.getElementById('auto-switch')
/* run All*/
  runAllButton.onclick = async function () {
    if ($('#buttonRunAll').hasClass('disabled')) {
      return false;
    }
    if(autoFlag ==1 ) {
      $('#ref-init-points').addClass('success');
      $('#ref-init-points').removeClass('failure');
    }  
    $('#buttonResults').removeClass('disabled');
    if ($('#buttonRunAll').hasClass('running')) {
      runAllButton.innerText = "Run All";
      $('#buttonRunAll').removeClass('running');

      if(!fixedIndices.includes(Index.omega)) setTimeout(() => runButtonOmega.onclick('cancelOnly'), 0);
      if(!fixedIndices.includes(Index.lambda_l)) setTimeout(() => runButtonLambda_l.onclick('cancelOnly'), 0);
      if(!fixedIndices.includes(Index.lambda_n)) setTimeout(() => runButtonLambda_n.onclick('cancelOnly'), 0);
      if(!fixedIndices.includes(Index.lambda_a)) setTimeout(() => runButtonLambda_a.onclick('cancelOnly'), 0);
      if(!fixedIndices.includes(Index.alpha)) setTimeout(() => runButtonAlpha.onclick('cancelOnly'), 0);
      if(!fixedIndices.includes(Index.f_l)) setTimeout(() => runButtonf_l.onclick('cancelOnly'), 0);
      if(!fixedIndices.includes(Index.f_n)) setTimeout(() => runButtonf_n.onclick('cancelOnly'), 0);
      if(!fixedIndices.includes(Index.f_a)) setTimeout(() => runButtonf_a.onclick('cancelOnly'), 0);
      if(!fixedIndices.includes(Index.kappa)) setTimeout(() => runButtonKappa.onclick('cancelOnly'), 0);
      if(!fixedIndices.includes(Index.obsprob)) setTimeout(() => runButtonObsprob.onclick('cancelOnly'), 0);
      if(!fixedIndices.includes(Index.T_min_l)) setTimeout(() => runButtonT_min_l.onclick('cancelOnly'), 0);
      if(!fixedIndices.includes(Index.gamma)) setTimeout(() => runButtonGamma.onclick('cancelOnly'), 0);
      return;
    }
    if(!(generateModelFlag && initalRefinPoints.length)) {
        alert('Upload the data first!')
    } else {  
      runAllButton.innerText = 'Cancel All';
      $('#buttonRunAll').removeClass('disabled');
      $('#buttonRunAll').addClass('running');
      BestResultsButton.style.display = '';

      let estimbuttuns = [
        runButtonOmega,
        runButtonLambda_l,
        runButtonLambda_n,
        runButtonLambda_a,
        runButtonAlpha,
        runButtonf_l,
        runButtonf_n,
        runButtonf_a,
        runButtonKappa,
        runButtonObsprob,
        runButtonT_min_l,
        runButtonGamma
      ];
      let buttons = []
      for (let i = 0; i < estimbuttuns.length; i++) {
        if(!fixedIndices.includes(plotIndex[i])) {
          buttons.push(estimbuttuns[i])
        }
      }

      // This is cheating: to avoid creating 12 keystore modals at once,
      // send a request using the protocol which will prompt for one before continuing 
      await dcp.protocol.send('addresses');
      let completePromises = buttons.map(button => {
        return new Promise(resolve => {
          setTimeout(() => {
            let job = button.onclick();
            if (!job) console.error("JOB IS NOT RETURNED");
            job.on('complete', resolve);
          }, 0);
        });
      });
      
      await Promise.all(completePromises);
      runAllButton.innerText = "Run All";
      $('#buttonRunAll').removeClass('running');
      $('#buttonRunAll').removeClass('disabled');
    }
  }
/* OMEGA*/
  runButtonOmega.onclick = function (cancelOnly=false) {
    
    if ($('#buttonRunOmega').hasClass('disabled')) {
      return false;
    }
    
    if ($('#buttonRunOmega').hasClass('running')) {
      jobs['omega'].cancel();
      specialLog('#special-log-Omega', 'Cancelling job...');
      runButtonOmega.innerText = "Generate & Run";
      $('#buttonRunOmega').removeClass('running');
      setTimeout(() => $('#buttonRunAll').removeClass('disabled'),0);
      $('#buttonRunAll').addClass('running')
      $('#buttonRunAll').removeClass('disabled');
      return;
    } else if (cancelOnly === 'cancelOnly') return;

    if(!(generateModelFlag && initalRefinPoints.length)) {
        alert('Upload the data first!')
    } else {
      let area = document.querySelector('#special-log-Omega');
      area.value =''
      if (!$('#buttonRunAll').hasClass('running')) {
        $('#buttonRunAll').addClass('disabled');
      }
      /* Index of parameters that need to be transfered */
      temp = model.createPompModel(data, covars, 0, dt.value, [...fixedIndices, Index.omega])
      estimateIndices = [...temp[0], ...temp[1]].sort()
      var index = Array(40).fill(0)
      for ( let i = 0; i < estimateIndices.length; i++) {
        index[estimateIndices[i]] = 1
      }
      runButtonOmega.innerText = 'Cancel';
      $('#buttonRunOmega').addClass('running');
      downloadButtonOmega.style.display = '';
      $('#OmegaButtonDownload').addClass('disabled');
      BestResultsButton.style.display = '';
      logScale = 0;

      lowerLimit = document.getElementById('limit1omega').value;
      upperLimit = document.getElementById('limit2omega').value;
      lowerLimit = Number(lowerLimit);
      upperLimit = Number(upperLimit);
      param_lims[Index.omega] = [lowerLimit,upperLimit];
       
      if (autoFlag) {
        NoPoints = Number(document.getElementById('refineIterationNumber').value);
      } else {
        NoPoints = Number(document.getElementById('NumberOfPoints').value);
      }
      flagBound = flagBoundArray[Index.omega];
      // flagBound = 2;
      if(fixedIndices.includes(Index.omega)) {
        flagBound = 0;
      }
      let generatedSet = generate.generateSet(initalRefinPoints, Index.omega, logScale, [lowerLimit,upperLimit], flagBound, NoPoints);
      let accumulatedResults = [];
      // data:
      var job = jobs['omega'] = runComputeFor(generatedSet, data, covarTime, covarTemperature, t0, times, estimateIndices, index, temp);
      job.on('myCustomError', (e) => {
        specialLog('#special-log-Omega', 'Failed to deploy job: ' + e.message);
      });
      specialLog('#special-log-Omega', 'Accessing Distributed Computer...');
      job.on('accepted', () => {
        specialLog('#special-log-Omega', 'Job accepted, working...');
      })
      let resultsRetrieved = 0;

      job.on('result', function(res) {
        // Show the results come back from the workers in consule and html.
        if(typeof res != 'undefined') {
          if(res.result) {
            resultsRetrieved++;
            if(res.result[0] !== 0) {
              accumulatedResults.push(res.result);
              bestResults.push(res.result);
            }
            specialLog('#special-log-Omega', res, resultsRetrieved / generatedSet.length);
            $('#OmegaButtonDownload').removeClass('disabled');

            downloadButtonOmega.onclick = function () {
              accumulatedResults.sort(mathLib.sortFunction);
              Csv(accumulatedResults, 'omega.csv');
            }
          }
        }
      });
      job.on('complete', async function(res) {
        await job.results.fetch();
        res = job.results.values();
        console.log('onComplete', res);
        runButtonOmega.innerText = "Generate & Run"
        $('#buttonRunOmega').removeClass('running');
        specialLog('#special-log-Omega', '\nJob complete! Click download for results.');
        res = Array.from(res)
        res.sort(mathLib.sortFunction);
        res = res.filter((row) => row[0] !== 0 && row[row.length - 1] < 0);
        $('#buttonRunAll').removeClass('disabled');

        downloadButtonOmega.onclick = function () {
          Csv(res,'omega.csv');
        }
      });

      return job;
    }
  }

  /* LAMBDA_L*/
  runButtonLambda_l.onclick = function (cancelOnly=false) {
    
    if ($('#runButtonLambda_l').hasClass('disabled')) {
      return false;
    }
    
    if ($('#buttonRunLambda_l').hasClass('running')) {
      jobs['lambda_l'].cancel();
      specialLog('#special-log-Lambda_l', 'Cancelling job...');
      runButtonLambda_l.innerText = "Generate & Run";
      $('#buttonRunLambda_l').removeClass('running');
      setTimeout(() => $('#buttonRunAll').removeClass('disabled'),0);
      $('#buttonRunAll').addClass('running')
      return;
    } else if (cancelOnly === 'cancelOnly') return;

    if(!(generateModelFlag && initalRefinPoints.length)) {
        alert('Upload the data first!')
    } else {
      let area = document.querySelector('#special-log-Lambda_l');
      area.value ='';
      if (!$('#buttonRunAll').hasClass('running')) {
        $('#buttonRunAll').addClass('disabled');
      }
      /* Index of parameters that need to be transfered */
      temp = model.createPompModel(data, covars, 0, dt.value, [...fixedIndices, Index.lambda_l])
      estimateIndices = [...temp[0], ...temp[1]].sort()
      var index = Array(40).fill(0)
      for ( let i = 0; i < estimateIndices.length; i++) {
        index[estimateIndices[i]] = 1
      }

      runButtonLambda_l.innerText = 'Cancel'
      downloadButtonLambda_l.style.display = ''
      BestResultsButton.style.display = '';
      $('#buttonRunLambda_l').addClass('running');
      $('#Lambda_lButtonDownload').addClass('disabled');
      logScale = 0, flagBound = 0

      lowerLimit = document.getElementById('limit1lambda_l').value
      upperLimit = document.getElementById('limit2lambda_l').value
      lowerLimit = Number(lowerLimit);
      upperLimit = Number(upperLimit);
      param_lims[Index.lambda_l] = [lowerLimit,upperLimit]
      if (autoFlag) {
        NoPoints = Number(document.getElementById('refineIterationNumber').value);
      } else {
        NoPoints = Number(document.getElementById('NumberOfPoints').value);
      }
      flagBound = flagBoundArray[Index.lambda_l]
      // flagBound = 1;
      if(fixedIndices.includes(Index.lambda_l)) {
        flagBound = 0;
      }

      let generatedSet = generate.generateSet(initalRefinPoints, Index.lambda_l, logScale, [lowerLimit,upperLimit], flagBound, NoPoints)
      let accumulatedResults = []
      // data:
      let job = jobs['lambda_l'] = runComputeFor(generatedSet, data, covarTime, covarTemperature, t0, times, estimateIndices, index, temp);
      job.on('myCustomError', (e) => {
        specialLog('#special-log-Lambda_l', 'Failed to deploy job: ' + e.message);
      });
      specialLog('#special-log-Lambda_l', 'Accessing Distributed Computer...');
      job.on('accepted', () => {
        specialLog('#special-log-Lambda_l', 'Job accepted, working...');
      })
      let resultsRetrieved = 0;

      job.on('result', function(res) {
        // Show the results come back from the workers in consule and html.
        if(typeof res != 'undefined') {
          if(res.result) {
            resultsRetrieved++;
            if(res.result[0] !== 0) {
              accumulatedResults.push(res.result);
              bestResults.push(res.result);
            }
            specialLog('#special-log-Lambda_l', res, resultsRetrieved / generatedSet.length);
            $('#Lambda_lButtonDownload').removeClass('disabled');

            downloadButtonLambda_l.onclick = function () {
              accumulatedResults.sort(mathLib.sortFunction);
              Csv(accumulatedResults,'lambda_l.csv.csv');
            }
          }
        }
      });
      job.on('complete', async function(res) {
        await job.results.fetch();
        res = job.results.values();
        console.log('onComplete', res);
        runButtonLambda_l.innerText = "Generate & Run"
        $('#buttonRunLambda_l').removeClass('running');
        res = Array.from(res)
        res.sort(mathLib.sortFunction);
        specialLog('#special-log-Lambda_l', '\nJob complete! Click download for results.');
        res = Array.from(res)
        res.sort(mathLib.sortFunction);
        res = res.filter((row) => row[0] !== 0 && row[row.length - 1] < 0);
        $('#buttonRunAll').removeClass('disabled');

        downloadButtonLambda_l.onclick = function () {
          Csv(res,'lambda_l.csv.csv');
        }
      });
      return job;
    }
  }

  /* LAMBDA_N*/
  runButtonLambda_n.onclick = function (cancelOnly=false) {
    
    if ($('#runButtonLambda_n').hasClass('disabled')) {
      return false;
    }
    
    if ($('#buttonRunLambda_n').hasClass('running')) {
      jobs['lambda_n'].cancel();
      specialLog('#special-log-Lambda_n', 'Cancelling job...');
      runButtonLambda_n.innerText = "Generate & Run";
      $('#buttonRunLambda_n').removeClass('running');
      setTimeout(() => $('#buttonRunAll').removeClass('disabled'),0);
      $('#buttonRunAll').addClass('running')
      return;
    } else if (cancelOnly === 'cancelOnly') return;

    if(!(generateModelFlag && initalRefinPoints.length)) {
        alert('Upload the data first!')
    } else {
      let area = document.querySelector('#special-log-Lambda_n');
      area.value ='';
      if (!$('#buttonRunAll').hasClass('running')) {
        $('#buttonRunAll').addClass('disabled');
      }
      /* Index of parameters that need to be transfered */
      temp = model.createPompModel(data, covars, 0, dt.value, [...fixedIndices, Index.lambda_n])
      estimateIndices = [...temp[0], ...temp[1]].sort()
      var index = Array(40).fill(0)
      for ( let i = 0; i < estimateIndices.length; i++) {
        index[estimateIndices[i]] = 1
      }

      runButtonLambda_n.innerText = 'Cancel'
      downloadButtonLambda_n.style.display = ''
      BestResultsButton.style.display = '';
      $('#buttonRunLambda_n').addClass('running');
      $('#Lambda_nButtonDownload').addClass('disabled');
      logScale = 0, flagBound = 0

      lowerLimit = document.getElementById('limit1lambda_n').value
      upperLimit = document.getElementById('limit2lambda_n').value
      lowerLimit = Number(lowerLimit);
      upperLimit = Number(upperLimit);
      param_lims[Index.lambda_n] = [lowerLimit,upperLimit]

      if (autoFlag) {
        NoPoints = Number(document.getElementById('refineIterationNumber').value);
      } else {
        NoPoints = Number(document.getElementById('NumberOfPoints').value);
      }
      flagBound = flagBoundArray[Index.lambda_n]
      // flagBound = 1;
      if(fixedIndices.includes(Index.lambda_n)) {
        flagBound = 0;
      }

      let generatedSet = generate.generateSet(initalRefinPoints, Index.lambda_n, logScale, [lowerLimit,upperLimit], flagBound, NoPoints)
      let accumulatedResults = []
      // data:
      var job = jobs['lambda_n'] = runComputeFor(generatedSet, data, covarTime, covarTemperature, t0, times, estimateIndices, index, temp);
      job.on('myCustomError', (e) => {
        specialLog('#special-log-Lambda_n', 'Failed to deploy job: ' + e.message);
      });
      specialLog('#special-log-Lambda_n', 'Accessing Distributed Computer...');
      job.on('accepted', () => {
        specialLog('#special-log-Lambda_n', 'Job accepted, working...');
      })
      let resultsRetrieved = 0;

      job.on('result', function(res) {
        // Show the results come back from the workers in consule and html.
        if(typeof res != 'undefined') {
          if(res.result) {
            resultsRetrieved++;
            if(res.result[0] !== 0) {
              accumulatedResults.push(res.result);
              bestResults.push(res.result);
            }
            specialLog('#special-log-Lambda_n', res, resultsRetrieved / generatedSet.length);
            $('#Lambda_nButtonDownload').removeClass('disabled');

            downloadButtonLambda_n.onclick = function () {
              accumulatedResults.sort(mathLib.sortFunction);
              Csv(accumulatedResults,'lambda_n.csv');
            }
          }
        }
      });
      job.on('complete', async function(res) {
        await job.results.fetch();
        res = job.results.values();
        console.log('onComplete', res);
        runButtonLambda_n.innerText = "Generate & Run"
        $('#buttonRunLambda_n').removeClass('running');
        specialLog('#special-log-Lambda_n', '\nJob complete! Click download for results.');
        res = Array.from(res)
        res.sort(mathLib.sortFunction);
        res = res.filter((row) => row[0] !== 0 && row[row.length - 1] < 0);
        $('#buttonRunAll').removeClass('disabled');
        
        downloadButtonLambda_n.onclick = function () {
          Csv(res,'lambda_n.csv');
        }
      });
      return job;
    }
  }

  /* LAMBDA_A*/
  runButtonLambda_a.onclick = function (cancelOnly=false) {
    
    if ($('#runButtonLambda_a').hasClass('disabled')) {
      return false;
    }
    
    if ($('#buttonRunLambda_a').hasClass('running')) {
      jobs['lambda_a'].cancel();
      specialLog('#special-log-Lambda_a', 'Cancelling job...');
      runButtonLambda_a.innerText = "Generate & Run";
      $('#buttonRunLambda_a').removeClass('running');
      setTimeout(() => $('#buttonRunAll').removeClass('disabled'),0);
      $('#buttonRunAll').addClass('running')
      return;
    } else if (cancelOnly === 'cancelOnly') return;

    if(!(generateModelFlag && initalRefinPoints.length)) {
        alert('Upload the data first!')
    } else {
      let area = document.querySelector('#special-log-Lambda_a');
      area.value ='';
      if (!$('#buttonRunAll').hasClass('running')) {
        $('#buttonRunAll').addClass('disabled');
      }
      /* Index of parameters that need to be transfered */
      temp = model.createPompModel(data, covars, 0, dt.value, [...fixedIndices, Index.lambda_a])
      estimateIndices = [...temp[0], ...temp[1]].sort()
      var index = Array(40).fill(0)
      for ( let i = 0; i < estimateIndices.length; i++) {
        index[estimateIndices[i]] = 1
      }

      runButtonLambda_a.innerText = 'Cancel'
      downloadButtonLambda_a.style.display = ''
      BestResultsButton.style.display = '';
      $('#buttonRunLambda_a').addClass('running');
      $('#Lambda_aButtonDownload').addClass('disabled');
      logScale = 0, flagBound = 0

      lowerLimit = document.getElementById('limit1lambda_a').value
      upperLimit = document.getElementById('limit2lambda_a').value
      lowerLimit = Number(lowerLimit);
      upperLimit = Number(upperLimit);
      param_lims[Index.lambda_a] = [lowerLimit,upperLimit]

      if (autoFlag) {
        NoPoints = Number(document.getElementById('refineIterationNumber').value);
      } else {
        NoPoints = Number(document.getElementById('NumberOfPoints').value);
      }
      flagBound = flagBoundArray[Index.lambda_a]
      // flagBound = 2;
      if(fixedIndices.includes(Index.lambda_a)) {
        flagBound = 0;
      }

      let generatedSet = generate.generateSet(initalRefinPoints, Index.lambda_a, logScale, [lowerLimit,upperLimit], flagBound, NoPoints)
      let accumulatedResults = []
      // data:
      var job = jobs['lambda_a'] = runComputeFor(generatedSet, data, covarTime, covarTemperature, t0, times, estimateIndices, index, temp);
      job.on('myCustomError', (e) => {
        specialLog('#special-log-Lambda_a', 'Failed to deploy job: ' + e.message);
      });
      specialLog('#special-log-Lambda_a', 'Accessing Distributed Computer...');
      job.on('accepted', () => {
        specialLog('#special-log-Lambda_a', 'Job accepted, working...');
      })
      let resultsRetrieved = 0;

      job.on('result', function(res) {
        // Show the results come back from the workers in consule and html.
        if(typeof res != 'undefined') {
          if(res.result) {
            resultsRetrieved++;
            if(res.result[0] !== 0) {
              accumulatedResults.push(res.result);
              bestResults.push(res.result);
            }
            specialLog('#special-log-Lambda_a', res, resultsRetrieved / generatedSet.length);
            $('#Lambda_aButtonDownload').removeClass('disabled');

            downloadButtonLambda_a.onclick = function () {
              accumulatedResults.sort(mathLib.sortFunction);
              Csv(accumulatedResults, 'lambda_a.csv');
            }
          }
        }
      });
      job.on('complete', async function(res) {
        await job.results.fetch();
        res = job.results.values();
        console.log('onComplete', res);
        runButtonLambda_a.innerText = "Generate & Run"
        $('#buttonRunLambda_a').removeClass('running');
        specialLog('#special-log-Lambda_a', '\nJob complete! Click download for results.');
        res = Array.from(res)
        res.sort(mathLib.sortFunction);
        res = res.filter((row) => row[0] !== 0 && row[row.length - 1] < 0);
        $('#buttonRunAll').removeClass('disabled');

        downloadButtonLambda_a.onclick = function () {
          Csv(res, 'lambda_a.csv');
        }
      });
      return job;
    }
  }

/* ALPHA*/
  runButtonAlpha.onclick = function (cancelOnly=false) {
    
    if ($('#runButtonAlpha').hasClass('disabled')) {
      return false;
    }
    
    if ($('#buttonRunAlpha').hasClass('running')) {
      jobs['alpha'].cancel();
      specialLog('#special-log-Alpha', 'Cancelling job...');
      runButtonAlpha.innerText = "Generate & Run";
      $('#buttonRunAlpha').removeClass('running');
      setTimeout(() => $('#buttonRunAll').removeClass('disabled'),0);
      $('#buttonRunAll').addClass('running')
      return;
    } else if (cancelOnly === 'cancelOnly') return;

    if(!(generateModelFlag && initalRefinPoints.length)) {
        alert('Upload the data first!')
    } else {
      let area = document.querySelector('#special-log-Alpha');
      area.value ='';
      if (!$('#buttonRunAll').hasClass('running')) {
        $('#buttonRunAll').addClass('disabled');
      }
      /* Index of parameters that need to be transfered */
      temp = model.createPompModel(data, covars, 0, dt.value, [...fixedIndices, Index.alpha])
      estimateIndices = [...temp[0], ...temp[1]].sort()
      var index = Array(40).fill(0)
      for ( let i = 0; i < estimateIndices.length; i++) {
        index[estimateIndices[i]] = 1
      }

      runButtonAlpha.innerText = 'Cancel'
      downloadButtonAlpha.style.display = ''
      BestResultsButton.style.display = '';
      $('#buttonRunAlpha').addClass('running');
      $('#AlphaButtonDownload').addClass('disabled');
      logScale = 0, flagBound = 0

      lowerLimit = document.getElementById('limit1alpha').value
      upperLimit = document.getElementById('limit2alpha').value
      lowerLimit = Number(lowerLimit);
      upperLimit = Number(upperLimit);
      param_lims[Index.alpha] = [lowerLimit,upperLimit]

      if (autoFlag) {
        NoPoints = Number(document.getElementById('refineIterationNumber').value);
      } else {
        NoPoints = Number(document.getElementById('NumberOfPoints').value);
      }
      flagBound = flagBoundArray[Index.alpha]
      // flagBound = 2;
      if(fixedIndices.includes(Index.alpha)) {
        flagBound = 0;
      }
      let generatedSet = generate.generateSet(initalRefinPoints, Index.alpha, logScale, [lowerLimit,upperLimit], flagBound, NoPoints)
      let accumulatedResults = []
      // data:
      var job = jobs['alpha'] = runComputeFor(generatedSet, data, covarTime, covarTemperature, t0, times, estimateIndices, index, temp);
      job.on('myCustomError', (e) => {
        specialLog('#special-log-Alpha', 'Failed to deploy job: ' + e.message);
      });
      specialLog('#special-log-Alpha', 'Accessing Distributed Computer...');
      job.on('accepted', () => {
        specialLog('#special-log-Alpha', 'Job accepted, working...');
      })
      let resultsRetrieved = 0;

      job.on('result', function(res) {
        // Show the results come back from the workers in consule and html.
        if(typeof res != 'undefined') {
          if(res.result) {
            resultsRetrieved++;
            if(res.result[0] !== 0) {
              accumulatedResults.push(res.result);
              bestResults.push(res.result);
            }
            specialLog('#special-log-Alpha', res, resultsRetrieved / generatedSet.length);
            $('#AlphaButtonDownload').removeClass('disabled');

            downloadButtonAlpha.onclick = function () {
              accumulatedResults.sort(mathLib.sortFunction);
              Csv(accumulatedResults, 'alpha.csv');
            }
          }
        }
      });
      job.on('complete', async function(res) {
        await job.results.fetch();
        res = job.results.values();
        console.log('onComplete', res);
        runButtonAlpha.innerText = "Generate & Run"
        $('#buttonRunAlpha').removeClass('running');
        specialLog('#special-log-Alpha', '\nJob complete! Click download for results.');
        res = Array.from(res)
        res.sort(mathLib.sortFunction);
        res = res.filter((row) => row[0] !== 0 && row[row.length - 1] < 0);
        $('#buttonRunAll').removeClass('disabled');

        downloadButtonAlpha.onclick = function () {
          Csv(res, 'alpha.csv');
        }
      });
      return job;
    }
  }
/* F_L*/
  runButtonf_l.onclick = function (cancelOnly=false) {
    
    if ($('#runButtonf_l').hasClass('disabled')) {
      return false;
    }
    
    if ($('#buttonRunF_l').hasClass('running')) {
      jobs['F_l'].cancel();
      specialLog('#special-log-F_l', 'Cancelling job...');
      runButtonf_l.innerText = "Generate & Run";
      $('#buttonRunF_l').removeClass('running');
      setTimeout(() => $('#buttonRunAll').removeClass('disabled'),0);
      $('#buttonRunAll').addClass('running')
      return;
    } else if (cancelOnly === 'cancelOnly') return;

    if(!(generateModelFlag && initalRefinPoints.length)) {
        alert('Upload the data first!')
    } else {
      let area = document.querySelector('#special-log-F_l');
      area.value ='';
      if (!$('#buttonRunAll').hasClass('running')) {
        $('#buttonRunAll').addClass('disabled');
      }
      /* Index of parameters that need to be transfered */
      temp = model.createPompModel(data, covars, 0, dt.value, [...fixedIndices, Index.f_l])
      estimateIndices = [...temp[0], ...temp[1]].sort()
      var index = Array(40).fill(0)
      for ( let i = 0; i < estimateIndices.length; i++) {
        index[estimateIndices[i]] = 1
      }

      runButtonf_l.innerText = 'Cancel'
      downloadButtonf_l.style.display = ''
      BestResultsButton.style.display = '';
      $('#buttonRunF_l').addClass('running');
      $('#F_lButtonDownload').addClass('disabled');
      logScale = 0, flagBound = 0

      lowerLimit = document.getElementById('limit1f_l').value
      upperLimit = document.getElementById('limit2f_l').value
      lowerLimit = Number(lowerLimit);
      upperLimit = Number(upperLimit);
      param_lims[Index.f_l] = [lowerLimit,upperLimit]

      if (autoFlag) {
        NoPoints = Number(document.getElementById('refineIterationNumber').value);
      } else {
        NoPoints = Number(document.getElementById('NumberOfPoints').value);
      }
      flagBound = flagBoundArray[Index.f_l]
      // flagBound = 1;
      if(fixedIndices.includes(Index.f_l)) {
        flagBound = 0;
      }

      let generatedSet = generate.generateSet(initalRefinPoints, Index.f_l, logScale, [lowerLimit,upperLimit], flagBound, NoPoints)
      let accumulatedResults = []
      // data:
      var job = jobs['F_l'] = runComputeFor(generatedSet, data, covarTime, covarTemperature, t0, times, estimateIndices, index, temp);
      job.on('myCustomError', (e) => {
        specialLog('#special-log-F_l', 'Failed to deploy job: ' + e.message);
      });
      specialLog('#special-log-F_l', 'Accessing Distributed Computer...');
      job.on('accepted', () => {
        specialLog('#special-log-F_l', 'Job accepted, working...');
      })
      let resultsRetrieved = 0;

      job.on('result', function(res) {
        // Show the results come back from the workers in consule and html.
        if(typeof res != 'undefined') {
          if(res.result) {
            resultsRetrieved++;
            if(res.result[0] !== 0) {
              accumulatedResults.push(res.result);
              bestResults.push(res.result);
            }
            specialLog('#special-log-F_l', res, resultsRetrieved / generatedSet.length);
            $('#F_lButtonDownload').removeClass('disabled');

            downloadButtonf_l.onclick = function () {
              accumulatedResults.sort(mathLib.sortFunction);
              Csv(accumulatedResults,'f_l.csv');
            }
          }
        }
      });
      job.on('complete', async function(res) {
        await job.results.fetch();
        res = job.results.values();
        console.log('onComplete', res);
        runButtonf_l.innerText = "Generate & Run"
        $('#buttonRunF_l').removeClass('running');
        specialLog('#special-log-F_l', '\nJob complete! Click download for results.');
        res = Array.from(res)
        res.sort(mathLib.sortFunction);
        res = res.filter((row) => row[0] !== 0 && row[row.length - 1] < 0);
        $('#buttonRunAll').removeClass('disabled');

        downloadButtonf_l.onclick = function () {
          Csv(res,'f_l.csv');
        }
      });
      return job;
    }
  }

/* F_N*/
  runButtonf_n.onclick = function (cancelOnly=false) {
    
    if ($('#runButtonf_n').hasClass('disabled')) {
      return false;
    }
    
    if ($('#buttonRunF_n').hasClass('running')) {
      jobs['F_n'].cancel();
      specialLog('#special-log-F_n', 'Cancelling job...');
      runButtonf_n.innerText = "Generate & Run";
      $('#buttonRunF_n').removeClass('running');
      setTimeout(() => $('#buttonRunAll').removeClass('disabled'),0);
      $('#buttonRunAll').addClass('running')
      return;
    } else if (cancelOnly === 'cancelOnly') return;

    if(!(generateModelFlag && initalRefinPoints.length)) {
        alert('Upload the data first!')
    } else {
      let area = document.querySelector('#special-log-F_n');
      area.value ='';
      if (!$('#buttonRunAll').hasClass('running')) {
        $('#buttonRunAll').addClass('disabled');
      }
      /* Index of parameters that need to be transfered */
      temp = model.createPompModel(data, covars, 0, dt.value, [...fixedIndices, Index.f_n])
      estimateIndices = [...temp[0], ...temp[1]].sort()
      var index = Array(40).fill(0)
      for ( let i = 0; i < estimateIndices.length; i++) {
        index[estimateIndices[i]] = 1
      }

      runButtonf_n.innerText = 'Cancel'
      downloadButtonf_n.style.display = ''
      BestResultsButton.style.display = '';
      $('#buttonRunF_n').addClass('running');
      $('#F_nButtonDownload').addClass('disabled');
      logScale = 0, flagBound = 0

      lowerLimit = document.getElementById('limit1f_n').value
      upperLimit = document.getElementById('limit2f_n').value
      lowerLimit = Number(lowerLimit);
      upperLimit = Number(upperLimit);
      param_lims[Index.f_n] = [lowerLimit,upperLimit]

      if (autoFlag) {
        NoPoints = Number(document.getElementById('refineIterationNumber').value);
      } else {
        NoPoints = Number(document.getElementById('NumberOfPoints').value);
      }
      flagBound = flagBoundArray[Index.f_n]
      // flagBound = 1;
      if(fixedIndices.includes(Index.f_n)) {
        flagBound = 0;
      }

      let generatedSet = generate.generateSet(initalRefinPoints, Index.f_n, logScale, [lowerLimit,upperLimit], flagBound, NoPoints)
      let accumulatedResults = []
      // data:
      var job = jobs['F_n'] = runComputeFor(generatedSet, data, covarTime, covarTemperature, t0, times, estimateIndices, index, temp);
      job.on('myCustomError', (e) => {
        specialLog('#special-log-F_n', 'Failed to deploy job: ' + e.message);
      });
      specialLog('#special-log-F_n', 'Accessing Distributed Computer...');
      job.on('accepted', () => {
        specialLog('#special-log-F_n', 'Job accepted, working...');
      })
      let resultsRetrieved = 0;

      job.on('result', function(res) {
        // Show the results come back from the workers in consule and html.
        if(typeof res != 'undefined') {
          if(res.result) {
            resultsRetrieved++;
            if(res.result[0] !== 0) {
              accumulatedResults.push(res.result);
              bestResults.push(res.result);
            }
            specialLog('#special-log-F_n', res, resultsRetrieved / generatedSet.length);
            $('#F_nButtonDownload').removeClass('disabled');

            downloadButtonf_n.onclick = function () {
              accumulatedResults.sort(mathLib.sortFunction);
              Csv(accumulatedResults, 'f_n.csv');
            }
          }
        }
      });
      job.on('complete', async function(res) {
        await job.results.fetch();
        res = job.results.values();
        console.log('onComplete', res);
        runButtonf_n.innerText = "Generate & Run"
        $('#buttonRunF_n').removeClass('running');
        specialLog('#special-log-F_n', '\nJob complete! Click download for results.');
        res = Array.from(res)
        res.sort(mathLib.sortFunction);
        res = res.filter((row) => row[0] !== 0 && row[row.length - 1] < 0);
        $('#buttonRunAll').removeClass('disabled');

        downloadButtonf_n.onclick = function () {
          Csv(res, 'f_n.csv');
        }
      });
      return job;
    }
  }

/* F_A*/
  runButtonf_a.onclick = function (cancelOnly=false) {
    
    if ($('#runButtonf_a').hasClass('disabled')) {
      return false;
    }
    
    if ($('#buttonRunF_a').hasClass('running')) {
      jobs['F_a'].cancel();
      specialLog('#special-log-F_a', 'Cancelling job...');
      runButtonf_a.innerText = "Generate & Run";
      $('#buttonRunF_a').removeClass('running');
      setTimeout(() => $('#buttonRunAll').removeClass('disabled'),0);
      $('#buttonRunAll').addClass('running')
      return;
    } else if (cancelOnly === 'cancelOnly') return;

    if(!(generateModelFlag && initalRefinPoints.length)) {
        alert('Upload the data first!')
    } else {
      let area = document.querySelector('#special-log-F_a');
      area.value ='';
      if (!$('#buttonRunAll').hasClass('running')) {
        $('#buttonRunAll').addClass('disabled');
      }
      /* Index of parameters that need to be transfered */
      temp = model.createPompModel(data, covars, 0, dt.value, [...fixedIndices, Index.f_a])
      estimateIndices = [...temp[0], ...temp[1]].sort()
      var index = Array(40).fill(0)
      for ( let i = 0; i < estimateIndices.length; i++) {
        index[estimateIndices[i]] = 1
      }

      runButtonf_a.innerText = 'Cancel'
      downloadButtonf_a.style.display = ''
      BestResultsButton.style.display = '';
      $('#buttonRunF_a').addClass('running');
      $('#F_aButtonDownload').addClass('disabled');
      logScale = 0, flagBound = 0

      lowerLimit = document.getElementById('limit1f_a').value
      upperLimit = document.getElementById('limit2f_a').value
      lowerLimit = Number(lowerLimit);
      upperLimit = Number(upperLimit);
      param_lims[Index.f_a] = [lowerLimit,upperLimit]

      if (autoFlag) {
        NoPoints = Number(document.getElementById('refineIterationNumber').value);
      } else {
        NoPoints = Number(document.getElementById('NumberOfPoints').value);
      }
      flagBound = flagBoundArray[Index.f_a]
      // flagBound = 1;
      if(fixedIndices.includes(Index.f_a)) {
        flagBound = 0;
      }

      let generatedSet = generate.generateSet(initalRefinPoints, Index.f_a, logScale, [lowerLimit,upperLimit], flagBound, NoPoints)
      let accumulatedResults = []
      // data:
      var job = jobs['F_a'] = runComputeFor(generatedSet, data, covarTime, covarTemperature, t0, times, estimateIndices, index, temp);
      job.on('myCustomError', (e) => {
        specialLog('#special-log-F_a', 'Failed to deploy job: ' + e.message);
      });
      specialLog('#special-log-F_a', 'Accessing Distributed Computer...');
      job.on('accepted', () => {
        specialLog('#special-log-F_a', 'Job accepted, working...');
      })
      let resultsRetrieved = 0;

      job.on('result', function(res) {
        // Show the results come back from the workers in consule and html.
        if(typeof res != 'undefined') {
          if(res.result) {
            resultsRetrieved++;
            if(res.result[0] !== 0) {
              accumulatedResults.push(res.result);
              bestResults.push(res.result);
            }
            specialLog('#special-log-F_a', res, resultsRetrieved / generatedSet.length);
            $('#F_aButtonDownload').removeClass('disabled');

            downloadButtonf_a.onclick = function () {
              accumulatedResults.sort(mathLib.sortFunction);
              Csv(accumulatedResults, 'f_a.csv');
            }
          }
        }
      });
      job.on('complete', async function(res) {
        await job.results.fetch();
        res = job.results.values();
        console.log('onComplete', res);
        runButtonf_a.innerText = "Generate & Run"
        $('#buttonRunF_a').removeClass('running');
        specialLog('#special-log-F_a', '\nJob complete! Click download for results.');
        res = Array.from(res)
        res.sort(mathLib.sortFunction);
        res = res.filter((row) => row[0] !== 0 && row[row.length - 1] < 0);
        $('#buttonRunAll').removeClass('disabled');

        downloadButtonf_a.onclick = function () {
          Csv(res, 'f_a.csv');
        }
      });
      return job;
    }
  }

/* KAPPA*/
  runButtonKappa.onclick = function (cancelOnly=false) {
    
    if ($('#runButtonKappa').hasClass('disabled')) {
      return false;
    }
    
    if ($('#buttonRunKappa').hasClass('running')) {
      jobs['kappa'].cancel();
      specialLog('#special-log-Kappa', 'Cancelling job...');
      runButtonKappa.innerText = "Generate & Run";
      $('#buttonRunKappa').removeClass('running');
      setTimeout(() => $('#buttonRunAll').removeClass('disabled'),0);
      $('#buttonRunAll').addClass('running')
      return;
    } else if (cancelOnly === 'cancelOnly') return;

    if(!(generateModelFlag && initalRefinPoints.length)) {
        alert('Upload the data first!')
    } else {
      let area = document.querySelector('#special-log-Kappa');
      area.value ='';
      if (!$('#buttonRunAll').hasClass('running')) {
        $('#buttonRunAll').addClass('disabled');
      }
      /* Index of parameters that need to be transfered */
      temp = model.createPompModel(data, covars, 0, dt.value, [...fixedIndices, Index.kappa])
      estimateIndices = [...temp[0], ...temp[1]].sort()
      var index = Array(40).fill(0)
      for ( let i = 0; i < estimateIndices.length; i++) {
        index[estimateIndices[i]] = 1
      }

      runButtonKappa.innerText = 'Cancel'
      downloadButtonKappa.style.display = ''
      BestResultsButton.style.display = '';
      $('#buttonRunKappa').addClass('running');
      $('#KappaButtonDownload').addClass('disabled');
      logScale = 0, flagBound = 0

      lowerLimit = document.getElementById('limit1kappa').value
      upperLimit = document.getElementById('limit2kappa').value
      lowerLimit = Number(lowerLimit);
      upperLimit = Number(upperLimit);
      param_lims[Index.kappa] = [lowerLimit,upperLimit]

      if (autoFlag) {
        NoPoints = Number(document.getElementById('refineIterationNumber').value);
      } else {
        NoPoints = Number(document.getElementById('NumberOfPoints').value);
      }
      flagBound = flagBoundArray[Index.kappa]
      // flagBound = 2;
      if(fixedIndices.includes(Index.kappa)) {
        flagBound = 0;
      }

      let generatedSet = generate.generateSet(initalRefinPoints, Index.kappa, logScale, [lowerLimit,upperLimit], flagBound, NoPoints)
      let accumulatedResults = []
      // data:
      var job = jobs['kappa'] = runComputeFor(generatedSet, data, covarTime, covarTemperature, t0, times, estimateIndices, index, temp);
      job.on('myCustomError', (e) => {
        specialLog('#special-log-Kappa', 'Failed to deploy job: ' + e.message);
      });
      specialLog('#special-log-Kappa', 'Accessing Distributed Computer...');
      job.on('accepted', () => {
        specialLog('#special-log-Kappa', 'Job accepted, working...');
      })
      let resultsRetrieved = 0;

      job.on('result', function(res) {
        // Show the results come back from the workers in consule and html.
        if(typeof res != 'undefined') {
          if(res.result) {
            resultsRetrieved++;
            if(res.result[0] !== 0) {
              accumulatedResults.push(res.result);
              bestResults.push(res.result);
            }
            specialLog('#special-log-Kappa', res, resultsRetrieved / generatedSet.length);
            $('#KappaButtonDownload').removeClass('disabled');

            downloadButtonKappa.onclick = function () {
              accumulatedResults.sort(mathLib.sortFunction);
              Csv(accumulatedResults, 'kappa.csv');
            }
          }
        }
      });
      job.on('complete', async function(res) {
        await job.results.fetch();
        res = job.results.values();
        console.log('onComplete', res);
        runButtonKappa.innerText = "Generate & Run"
        $('#buttonRunKappa').removeClass('running');
        specialLog('#special-log-Kappa', '\nJob complete! Click download for results.');
        res = Array.from(res)
        res.sort(mathLib.sortFunction);
        res = res.filter((row) => row[0] !== 0 && row[row.length - 1] < 0);
        $('#buttonRunAll').removeClass('disabled');

        downloadButtonKappa.onclick = function () {
          Csv(res, 'kappa.csv');
        }
      });
      return job;
    }
  }

/* OBSPROB*/
  runButtonObsprob.onclick = function (cancelOnly=false) {
    
    if ($('#runButtonObsprob').hasClass('disabled')) {
      return false;
    }
    
    if ($('#buttonRunObsprob').hasClass('running')) {
      jobs['obsprob'].cancel();
      specialLog('#special-log-Obsprob', 'Cancelling job...');
      runButtonObsprob.innerText = "Generate & Run";
      $('#buttonRunObsprob').removeClass('running');
      setTimeout(() => $('#buttonRunAll').removeClass('disabled'),0);
      $('#buttonRunAll').addClass('running')
      return;
    } else if (cancelOnly === 'cancelOnly') return;

    if(!(generateModelFlag && initalRefinPoints.length)) {
        alert('Upload the data first!')
    } else {
      let area = document.querySelector('#special-log-Obsprob');
      area.value ='';
      if (!$('#buttonRunAll').hasClass('running')) {
        $('#buttonRunAll').addClass('disabled');
      }
      /* Index of parameters that need to be transfered */
      temp = model.createPompModel(data, covars, 0, dt.value, [...fixedIndices, Index.obsprob])
      estimateIndices = [...temp[0], ...temp[1]].sort()
      var index = Array(40).fill(0)
      for ( let i = 0; i < estimateIndices.length; i++) {
        index[estimateIndices[i]] = 1
      }

      runButtonObsprob.innerText = 'Cancel'
      downloadButtonObsprob.style.display = ''
      BestResultsButton.style.display = '';
      $('#buttonRunObsprob').addClass('running');
      $('#ObsprobButtonDownload').addClass('disabled');
      logScale = 0, flagBound = 0

      lowerLimit = document.getElementById('limit1obsprob').value
      upperLimit = document.getElementById('limit2obsprob').value
      lowerLimit = Number(lowerLimit);
      upperLimit = Number(upperLimit);
      param_lims[Index.obsprob] = [lowerLimit,upperLimit]

      if (autoFlag) {
        NoPoints = Number(document.getElementById('refineIterationNumber').value);
      } else {
        NoPoints = Number(document.getElementById('NumberOfPoints').value);
      }
      flagBound = flagBoundArray[Index.obsprob]
      // flagBound = 0; 
      if(fixedIndices.includes(Index.obsprob)) {
        flagBound = 0;
      }

      let generatedSet = generate.generateSet(initalRefinPoints, Index.obsprob, logScale, [lowerLimit,upperLimit], flagBound, NoPoints)
      let accumulatedResults = []
      // data:
      var job = jobs['obsprob'] = runComputeFor(generatedSet, data, covarTime, covarTemperature, t0, times, estimateIndices, index, temp);
      job.on('myCustomError', (e) => {
        specialLog('#special-log-Obsprob', 'Failed to deploy job: ' + e.message);
      });
      specialLog('#special-log-Obsprob', 'Accessing Distributed Computer...');
      job.on('accepted', () => {
        specialLog('#special-log-Obsprob', 'Job accepted, working...');
      })
      let resultsRetrieved = 0;

      job.on('result', function(res) {
        // Show the results come back from the workers in consule and html.
        if(typeof res != 'undefined') {
          if(res.result) {
            resultsRetrieved++;
            if(res.result[0] !== 0) {
              accumulatedResults.push(res.result);
              bestResults.push(res.result);
            }
            specialLog('#special-log-Obsprob', res, resultsRetrieved / generatedSet.length);
            $('#ObsprobButtonDownload').removeClass('disabled');

            downloadButtonObsprob.onclick = function () {
              accumulatedResults.sort(mathLib.sortFunction);
              Csv(accumulatedResults, 'obsprob.csv');
            }
          }
        }
      });
      job.on('complete', async function(res) {
        await job.results.fetch();
        res = job.results.values();
        console.log('onComplete', res);
        runButtonObsprob.innerText = "Generate & Run"
        $('#buttonRunObsprob').removeClass('running');
        specialLog('#special-log-Obsprob', '\nJob complete! Click download for results.');
        res = Array.from(res)
        res.sort(mathLib.sortFunction);
        res = res.filter((row) => row[0] !== 0 && row[row.length - 1] < 0);
        $('#buttonRunAll').removeClass('disabled');

        downloadButtonObsprob.onclick = function () {
          Csv(res, 'obsprob.csv');
        }
      });
      return job;
    }
  }

/* T_MIN_L*/
  runButtonT_min_l.onclick = function (cancelOnly=false) {
    
    if ($('#runButtonT_min_l').hasClass('disabled')) {
      return false;
    }
    
    if ($('#buttonRunTminL').hasClass('running')) {
      jobs['tminl'].cancel();
      specialLog('#special-log-TminL', 'Cancelling job...');
      runButtonT_min_l.innerText = "Generate & Run";
      $('#buttonRunTminL').removeClass('running');
      setTimeout(() => $('#buttonRunAll').removeClass('disabled'),0);
      $('#buttonRunAll').addClass('running')
      return;
    } else if (cancelOnly === 'cancelOnly') return;

    if(!(generateModelFlag && initalRefinPoints.length)) {
        alert('Upload the data first!')
    } else {
      let area = document.querySelector('#special-log-TminL');
      area.value ='';
      if (!$('#buttonRunAll').hasClass('running')) {
        $('#buttonRunAll').addClass('disabled');
      }
      /* Index of parameters that need to be transfered */
      temp = model.createPompModel(data, covars, 0, dt.value, [...fixedIndices, Index.T_min_l])
      estimateIndices = [...temp[0], ...temp[1]].sort()
      var index = Array(40).fill(0)
      for ( let i = 0; i < estimateIndices.length; i++) {
        index[estimateIndices[i]] = 1
      }

      runButtonT_min_l.innerText = 'Cancel'
      downloadButtonT_min_L.style.display = ''
      BestResultsButton.style.display = '';
      $('#buttonRunTminL').addClass('running');
      $('#TminLButtonDownload').addClass('disabled');
      logScale = 0, flagBound = 0

      lowerLimit = document.getElementById('limit1T_min_l').value;
      upperLimit = document.getElementById('limit2T_min_l').value;
      lowerLimit = Number(lowerLimit);;
      upperLimit = Number(upperLimit);
      param_lims[Index.T_min_l] = [lowerLimit,upperLimit];

      if (autoFlag) {
        NoPoints = Number(document.getElementById('refineIterationNumber').value);
      } else {
        NoPoints = Number(document.getElementById('NumberOfPoints').value);
      }
      flagBound = flagBoundArray[Index.T_min_l];
      // flagBound = 1;
      if(fixedIndices.includes(Index.T_min_l)) {
        flagBound = 0;
      }

      let generatedSet = generate.generateSet(initalRefinPoints, Index.T_min_l, logScale, [lowerLimit,upperLimit], flagBound, NoPoints);
      let accumulatedResults = [];
      // data:
      var job = jobs['tminl'] = runComputeFor(generatedSet, data, covarTime, covarTemperature, t0, times, estimateIndices, index, temp);
      job.on('myCustomError', (e) => {
        specialLog('#special-log-TminL', 'Failed to deploy job: ' + e.message);
      });
      specialLog('#special-log-TminL', 'Accessing Distributed Computer...');
      job.on('accepted', () => {
        specialLog('#special-log-TminL', 'Job accepted, working...');
      })
      let resultsRetrieved = 0;

      job.on('result', function(res) {
        // Show the results come back from the workers in consule and html.
        if(typeof res != 'undefined') {
          if(res.result) {
            resultsRetrieved++;
            if(res.result[0] !== 0) {
              accumulatedResults.push(res.result);
              bestResults.push(res.result);
            }
            specialLog('#special-log-TminL', res, resultsRetrieved / generatedSet.length);
            $('#TminLButtonDownload').removeClass('disabled');

            downloadButtonT_min_L.onclick = function () {
              accumulatedResults.sort(mathLib.sortFunction);
              Csv(accumulatedResults, 'T_min_l.csv');
            }
          }
        }
      });
      job.on('complete', async function(res) {
        await job.results.fetch();
        res = job.results.values();
        console.log('onComplete', res);
        runButtonT_min_l.innerText = "Generate & Run";
        $('#buttonRunTminL').removeClass('running');
        specialLog('#special-log-TminL', '\nJob complete! Click download for results.');
        res = Array.from(res)
        res.sort(mathLib.sortFunction);
        res = res.filter((row) => row[0] !== 0 && row[row.length - 1] < 0);
        $('#buttonRunAll').removeClass('disabled');
        downloadButtonT_min_L.onclick = function () {
          Csv(res, 'T_min_l.csv');
        }
      });
      return job;
    }
  }

/* GAMMA*/
  runButtonGamma.onclick = function (cancelOnly=false) {
    
    if ($('#runButtonGamma').hasClass('disabled')) {
      return false;
    }
    
    if ($('#buttonRunGamma').hasClass('running')) {
      jobs['gamma'].cancel();
      specialLog('#special-log-Gamma', 'Cancelling job...');
      runButtonGamma.innerText = "Generate & Run";
      $('#buttonRunGamma').removeClass('running');
      setTimeout(() => $('#buttonRunAll').removeClass('disabled'),0);
      $('#buttonRunAll').addClass('running')
      return;
    } else if (cancelOnly === 'cancelOnly') return;

    if(!(generateModelFlag && initalRefinPoints.length)) {
        alert('Upload the data first!')
    } else {
      let area = document.querySelector('#special-log-Gamma');
      area.value ='';
      if (!$('#buttonRunAll').hasClass('running')) {
        $('#buttonRunAll').addClass('disabled');
      }
      /* Index of parameters that need to be transfered */
      temp = model.createPompModel(data, covars, 0, dt.value, [...fixedIndices, Index.gamma])
      estimateIndices = [...temp[0], ...temp[1]].sort()
      var index = Array(40).fill(0)
      for ( let i = 0; i < estimateIndices.length; i++) {
        index[estimateIndices[i]] = 1
      }

      runButtonGamma.innerText = 'Cancel'
      downloadButtonGamma.style.display = ''
      BestResultsButton.style.display = '';
      $('#buttonRunGamma').addClass('running');
      $('#gammaButtonDownload').addClass('disabled');
      logScale = 0, flagBound = 0

      lowerLimit = document.getElementById('limit1gamma').value
      upperLimit = document.getElementById('limit2gamma').value
      lowerLimit = Number(lowerLimit);
      upperLimit = Number(upperLimit);
      param_lims[Index.gamma] = [lowerLimit,upperLimit]

      if (autoFlag) {
        NoPoints = Number(document.getElementById('refineIterationNumber').value);
      } else {
        NoPoints = Number(document.getElementById('NumberOfPoints').value);
      }
      flagBound = flagBoundArray[Index.gamma]
      if(fixedIndices.includes(Index.gamma)) {
        flagBound = 0;
      }

      let generatedSet = generate.generateSet(initalRefinPoints, Index.gamma, logScale, [lowerLimit,upperLimit], flagBound, NoPoints)
      let accumulatedResults = []
      // data:
      var job = jobs['gamma'] = runComputeFor(generatedSet, data, covarTime, covarTemperature, t0, times, estimateIndices, index, temp);
      job.on('myCustomError', (e) => {
        specialLog('#special-log-Gamma', 'Failed to deploy job: ' + e.message);
      });
      specialLog('#special-log-Gamma', 'Accessing Distributed Computer...');
      job.on('accepted', () => {
        specialLog('#special-log-Gamma', 'Job accepted, working...');
      })
      let resultsRetrieved = 0;

      job.on('result', function(res) {
        // Show the results come back from the workers in consule and html.
        if(typeof res != 'undefined') {
          if(res.result) {
            resultsRetrieved++;
            if(res.result[0] !== 0) {
              accumulatedResults.push(res.result);
              bestResults.push(res.result);
            }
            specialLog('#special-log-Gamma', res, resultsRetrieved / generatedSet.length);
            $('#gammaButtonDownload').removeClass('disabled');

            downloadButtonGamma.onclick = function () {
              accumulatedResults.sort(mathLib.sortFunction);
              Csv(accumulatedResults, 'gamma.csv');
            }
          }
        }
      });
      job.on('complete', async function(res) {
        await job.results.fetch();
        res = job.results.values();
        console.log('onComplete', res);
        runButtonGamma.innerText = "Generate & Run"
        $('#buttonRunGamma').removeClass('running');
        specialLog('#special-log-Gamma', '\nJob complete! Click download for results.');
        res = Array.from(res);
        res.sort(mathLib.sortFunction);
        res = res.filter((row) => row[0] !== 0 && row[row.length - 1] < 0);;
        $('#buttonRunAll').removeClass('disabled');

        downloadButtonGamma.onclick = function () {
          Csv(res, 'gamma.csv');
        }
      });
      return job;
    }
  }
  
}
// Helper functions
function Csv (res, type) {
  res = [nameArray].concat(res);
  var csv = ''
  res.forEach(function (row) {
    csv += row.join(',')
    csv += '\n'
  })
  var hiddenElement = document.createElement('a')
  hiddenElement.href = URL.createObjectURL(new Blob([csv], { type: 'text/csv;charset=utf-8;' }));
  hiddenElement.setAttribute('download', type)
  hiddenElement.click()
}

// log to initial search display.
function specialLog(parentId, result, percent=false) {
  let area = document.querySelector(parentId);
  if (typeof result === 'string') {
    area.value += result + '\n';
  } else {
    area.value += `Received result for point ${result.sliceNumber}${percent? '(' + (percent * 100).toFixed(1) + '% done)' : ''}: `;
    if (result.result[result.result.length - 1] === null) {
      area.value += 'Unsuccessful search.\n';
    } else {
      area.value += JSON.stringify(result.result) + ';\n';
    }
  }
  area.scrollTop = area.scrollHeight;
}

function rmSameRow(allSets) {
  allSets.sort(mathLib.sortFunction);
  let finalSet = [allSets[0]];
  size = allSets[0].length - 1;
    for (let i = 1; i < allSets.length; i++) {
      while(allSets[i - 1][size].toFixed(8) === allSets[i][size].toFixed(8)) {
          i++;
      } 
      finalSet.push(allSets[i]);
    }
  return finalSet; 
}
function plot2D(data, PlotParamName, xTitle) {

  var layout = {
    width: 600,
    height: 600,
    autosize: false,
    margin: {'b': 100},
    xaxis: {
      title: {
        text: xTitle,// '$\\omega $',
        font: {
          family: 'Courier New, monospace',
          size: 20,
          color: '#7f7f7f'
        }
      },
    },
    scene: {
      xaxis: {
      },
      yaxis: {
        title: "F",
        rangemode: 'tozero',
      }
    },
    
    legend: {
    x: 1,
    y: 1
    }
  };

  Plotly.newPlot(PlotParamName, data, layout, { scrollZoom: true }); 
}

document.addEventListener("DOMContentLoaded", () => {
  
  document.querySelector('#special-log-sobol').value = '';
  document.querySelector('#special-log-Omega').value = '';
  document.querySelector('#special-log-Lambda_l').value = '';
  document.querySelector('#special-log-Lambda_a').value = '';
  document.querySelector('#special-log-Lambda_n').value = '';
  document.querySelector('#special-log-Alpha').value = '';
  document.querySelector('#special-log-F_l').value = '';
  document.querySelector('#special-log-F_a').value = '';
  document.querySelector('#special-log-F_n').value = '';
  document.querySelector('#special-log-Kappa').value = '';
  document.querySelector('#special-log-Obsprob').value = '';
  document.querySelector('#special-log-TminL').value = '';
  document.querySelector('#special-log-Gamma').value = '';

  $('.interact > .disabled').click(function(evt) {
    if ($(this).hasClass('disabled')) {
      evt.preventDefault();
      evt.stopPropagation();
      evt.returnValue = false;
      return false;
    }
  });

  fetch('js/worker-bundle.js')
    .then(response => response.text())
    .then((workerBundle) => {
      // self.workerfn gets set by the workerBundle (see worker-src/index.js)
      const workerFn = `(async (...args) => {
        ${workerBundle}
        // Local exec
        // console.log(await self.workerfn(...args));
        return await self.workerfn(...args);
      })`;


      Module.wasmBinary = new Uint8Array(require('./lsoda/lsoda.wasm'))

      // Will patch Module with rest of Emscripten
      require('./lsoda/lsoda.js');

      console.log("Started main, waiting for EMSDK Module to load");
      Module['onRuntimeInitialized'] = () => {
        console.log("EMSDK Module loaded.");
        start(workerFn)
      };
  });
});

function trigerPlot(bestResults,indexPlot, PlotParamName, bandwidth, param_lims, xTitle) {
  let pairs = [];
  let interpolateX =[], interpolateY = [];
  let plotData = plotProfile(bestResults, indexPlot, param_lims);
  
  if (plotData < 0) {
    return 0;
  }
  if(plotData[0][0] === plotData[0][1]) {
    return 0
  }
  var trace1 = {
    x: plotData[0],
    y: plotData[1],
    mode: 'markers',
    marker: {
      color: '#2ed573',
      size: 3,
      line: {
        color: '2ed573',
        width: 2
      }
    },
    name: 'Actual'
  }; 
     
  for(let i = 0; i < plotData[0].length; i++) {
    pairs.push([plotData[0][i],plotData[1][i]])
  }
  mathLib.sortAscendingByKey(pairs, 0)

  bandwidth = .6
  let fit = loess2(plotData, bandwidth)
  let interpolateResult = fit.fitted
  var upperLimitplot = fit.fitted.map((yhat, idx) => yhat + fit.halfwidth[idx])
  var lowerLimitplot = fit.fitted.map((yhat, idx) => yhat - fit.halfwidth[idx])
  
  var trace2 = {
    x: plotData[0],
    y: lowerLimitplot,
    mode: 'lines',
    line: {color: "#D3D3D3"},
    showlegend: false
  }; 

  var trace3 = {
    x: plotData[0],
    y: upperLimitplot,
    mode: 'lines',
    line: {color: "#D3D3D3"},
    fill: 'tonexty',
    name: ' Uncertainty'
  };

  var trace4 = {
    x: plotData[0],
    y: interpolateResult,
    mode: 'lines',
    line: {color: "#000000"},
    name:'Fitted'
  }; 
  
  var dataPlot = [trace1, trace2, trace3, trace4];
  plot2D(dataPlot, PlotParamName, xTitle)
}

function trigerPlotTrajectory(dataInput, PlotParamName) {
  let times = [];
  let data = [];
  let simHarranged = [];
  for (let i = 0; i < dataInput.length; i++) {
    times.push(dataInput[i][0]);
    data.push(dataInput[i][1]);
    simHarranged.push(dataInput[i][2]);
  }

  var trace1 = {
    x: times,
    y: data,
    mode: 'lines',
    line: {color: "#00a473"},
    name: 'data'
  }; 

  var trace2 = {
    x: times,
    y: simHarranged,
    mode: 'lines',
    line: {color: "#1c3583"},
    name: ' trajectory'
  };

  var dataPlot = [trace1, trace2];
  plot2D(dataPlot, PlotParamName)
}