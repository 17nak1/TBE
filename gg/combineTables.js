

fs= require('fs')

combineTables = function (modeltype, runs) {
  let allSets = [] 

  for ( run = 0; run < runs.length; run++){
    var table = [], dataset = []
    var temp
    var data = fs.readFileSync('./' + modeltype  + runs[run] +'.csv').toString()
    var lines = data.split('\n')
    for (let i = 1; i < lines.length; i++) {
      temp = lines[i].split(',')
      if (temp[temp.length - 1] !== '"NaN"' && temp[temp.length - 1] < 0) {
        table.push(temp)
      }
    }

    table.sort(sortFunction)

    var newSet = {}
    table.forEach(function(arr){
      newSet[arr.join("|")] = arr
    })

    var dataset = Object.keys(newSet).map(function(k){
      return newSet[k]
    })

    console.log(run, dataset.length)
    allSets.push(...dataset) 
  }

  allSets.sort(sortFunction)
  let finalSet = [allSets[0]]
  let size = allSets[0].length - 1
  for (let i = 1; i < allSets.length; i++) {
    if(allSets[i - 1][0] !== allSets[i][0]) {
      finalSet.push(allSets[i])
    } else {
      if(allSets[i - 1][size] !== allSets[i][size]) {
        finalSet.push(allSets[i])
      }
    } 
  }
  
  const createCsvWriter = require('csv-writer').createArrayCsvWriter;
  const csvWriter = createCsvWriter({
    header: [],
    path: './all.csv'
  }) 
  csvWriter.writeRecords(finalSet)
    .then(() => {
    console.log('...done')
  })
}

function sortFunction(a, b) {
  if (Number(a[a.length - 1]) === Number(b[a.length - 1])) {
    return 0
  }
  else {
    return (Number(a[a.length - 1]) < Number(b[a.length - 1])) ? 1 : -1;
  }
}
// example
combineTables ('TBEres', ['2','3','4','5','6','8','9','10','11','12','13','14','15','16','17','18','19','20'])