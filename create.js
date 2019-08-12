/**
 *  @file        Create.js
 *               Includes functions to create covar and data sets.
 *
 *  @autor       Nazila Akhavan, nazila@kingsds.network
 *  @date        July 2019
 */
let create = {}
let mathLib = require('./mathLib.js')
let fs = require('fs')

// Read data and returns covars which is an array of 2; time and approximation of temperature.
create.covars = function(startTime=1991, endTime=2016, stepsize=0.005) {
  let time = [], x
  
  // time: Shows the number of days, start from zero and calculated by deviding [0 , endTime - startTime] with stepsize.  
  for (let i = startTime; i <= endTime + 1 + 1e-8 ; i += stepsize) {
    x = (i - startTime) * 365
    time.push(Number(x.toFixed(6)))
  }
  let d = fs.readFileSync("./data/SzombathelyTempDaily1901to2015Monthly.csv").toString()
  let data = [], data1 = []
  let lines = d.split('\n')
  for (let i = 0; i < lines.length - 1; i++) {
    data.push(lines[i].split(','))
  }
  for (let i = 1; i < data.length; i++) {
    for (let j = 0; j < data[0].length; j++) {
      data[i][j] = Number(data[i][j])
    }
  }
  // data1: Array of 2; 1.The number of days calculated  based on data[year, month] 2. Temperatute
  for ( let i = 0; i < data.length; i++) {
    data1.push([(data[i][0] + data[i][1] / 12 - startTime) * 365, data[i][2]])
  } 
  
  interpolTemperature = mathLib.interpolator(data1)
  covars = []
  // covars  Array of 2; time and approximation of temperature based on data1.
  for ( let i = 0; i < time.length; i++) {
    covars.push( [time[i], interpolTemperature(time[i])])
  }
  return(covars)
}

// Read data and returns covars which is an array of 2; time and approximation of temperature.
create.dataset = function(startTime=1991, endTime=2016) {
  let data = []
  // data :Array of [1991:1997,1:52, NaN] follows by rows in data file ["Year","Week","Encephalitis"].
  for ( let k = 1991; k <= 1997; k++) {
    for (let i = 0; i < 52; i++) {
      data.push([k,i + 1, NaN])  
    }
  }

  let d = fs.readFileSync("./data/170705PseudoData.csv").toString()  
  let lines = d.split('\n')
  for (let i = 1; i < lines.length - 1; i++) {
    data.push(lines[i].split(','))
  }
  
  for (let i = 0; i < data.length; i++) {
    for (let j = 0; j < data[0].length; j++) {
      data[i][j] = Number(data[i][j])
    }
  }

  // Calculates time column that is number of days(almost weekly) based on first two rows in ["Year","Week","Encephalitis"].
  let finalData = []
  for (let i = 0; i < data.length; i++) {
    finalData.push([(data[i][0] + data[i][1] / 52 - startTime) * 365, data[i][2]])
  }
  return(finalData)
}
module.exports = create

// let finalData  =create.dataset(1991, 2008)
// const createCsvWriter = require('csv-writer').createArrayCsvWriter
//   const csvWriter = createCsvWriter({
//     header: [],
//     path: './finalData.csv'
//   })   
//   csvWriter.writeRecords(finalData)
//     .then(() => {
//     console.log('...Done')
//   })