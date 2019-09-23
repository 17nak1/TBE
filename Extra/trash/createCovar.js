/**
  * @file     creatCovars.js
  * 
  *  time:    Array, start from zero and calculated by using startTime, endTime, stepsize.
  *  data1:   Array of 2; 1.calculated Time based on data[year, month] 
                          2. Temperatute 
  *  covars   Array of 2; time and approximation of temperature.
  */
let mathLib = require('./mathLib.js')
let fs = require('fs')

creatCovars = function(startTime=1991, endTime=2016, stepsize=0.005) {
  let time = [], x
    
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
  console.log(isNaN(Number(data[0][0])))
  for (let i = 1; i < data.length; i++) {
    for (let j = 0; j < data[0].length; j++) {
      data[i][j] = Number(data[i][j])
    }
  }

  for ( let i = 0; i < data.length; i++) {
    data1.push([(data[i][0] + data[i][1] / 12 - startTime) * 365, data[i][2]])
  } 
  
  interpolTemperature = mathLib.interpolator(data1)
  covars = []
  for ( let i = 0; i < time.length; i++) {
    covars.push( [time[i], interpolTemperature(time[i])])
  }
  const createCsvWriter = require('csv-writer').createArrayCsvWriter;
  const csvWriter = createCsvWriter({
    header: ['time', 'temperature'],
    path: './covarjs.csv'
  })   
  csvWriter.writeRecords(covars)
    .then(() => {
    console.log('...Done')
  })
  
  return(covars)
}

creatCovars(1991, 2016, .005)