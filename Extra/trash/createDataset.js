/**
  * @file         creatDataset.js
  *  data:        Array of [1977,1:52, NaN] follows by rows in data file ["Year","Week","Encephalitis"].
  *  finaldata:   Array of 2; 1.calculated Time based on data[Year, Week] 
                              2. Encephalitis 
  */

let fs = require('fs')

createDataset = function(startTime=1991, endTime=2016) {
   let d = fs.readFileSync("./data/170705PseudoData.csv").toString()
  let data = [], finalData = []
  for (let i = 0; i < 52; i++) {
    data.push([1997,i + 1, NaN])  
  }
  let lines = d.split('\n')
  for (let i = 1; i < lines.length - 1; i++) {
    data.push(lines[i].split(','))
  }
  
  for (let i = 0; i < data.length; i++) {
    for (let j = 0; j < data[0].length; j++) {
      data[i][j] = Number(data[i][j])
    }
  }

  for (let i = 0; i < data.length; i++) {
    finalData.push([(data[i][0] + data[i][1] / 52 - startTime) * 365, data[i][2]])
  }
 
  // const createCsvWriter = require('csv-writer').createArrayCsvWriter;
  // const csvWriter = createCsvWriter({
  //   header: ['Time', 'Encephalitis'],
  //   path: './diff/covarjs.csv'
  // })   
  // csvWriter.writeRecords(finalData)
  //   .then(() => {
  //   console.log('...Done')
  // })
  return(finalData)
}


// createDataset()