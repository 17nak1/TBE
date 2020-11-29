/**
 *  @file      combineTables.js       This function clean, sort and combine tables in csv files.
 *                                    runs : The array of parameters that wants to combine their tables.
 *  @return                           A sorted table based on the "LogLik" column.
 *  @reference                        Aaron A. King.
 *  @author    Nazila Akhavan
 *  @date      March 2019
 */

combineTables = {}

combineTables.combine = function (runArray) {
  let allSets = [] 
  let newSet
  let finalSet 
  let size
  for ( run = 0; run < runArray.length; run++){
    var table = [], dataset = []
    var table = runArray[run]
    
    // To remove name's row if exists.
    if ( table.length) {
      if (isNaN(Number(table[0][0])) && !Array.isArray(table[0][0])) {
        table.shift()
      }
      table.sort(sortFunction)
      while(table[0][table[0].length - 1] === 0 || isNaN(table[0][table[0].length - 1])) {
        table.shift()
        if (isNaN(table[0]) && !Array.isArray(table[0])){
          break;
        }
      }
      
      newSet = {}
      table.forEach(function(arr){
        newSet[arr.join("|")] = arr
      })
      dataset = Object.keys(newSet).map(function(k){
        return newSet[k]
      })
      allSets.push(...dataset)
    } 
  }

  allSets.sort(sortFunction)
  // Make only one array in each row and solve bug if loglike added as an array
  for (let i = 0; i < allSets.length; i++) {
    if (Array.isArray(allSets[i][0])) {
      allSets[i] = [ ...allSets[i][0], allSets[i][1]]
    }
  } 

  // Delete rows with the same values. 
  finalSet = [allSets[0]]
  if (allSets.length > 0) {
    size = allSets[0].length - 1
    for (let i = 1; i < allSets.length; i++) {
      if(allSets[i - 1][0].toFixed(6) !== allSets[i][0].toFixed(6)) {
        finalSet.push(allSets[i])
      } else {
        if(allSets[i - 1][size].toFixed(6) !== allSets[i][size].toFixed(6)) {
          finalSet.push(allSets[i])
        }
      } 
    }
  }  
  return finalSet
}
// Helper function. Sort the table based on the last column.
function sortFunction(a, b) {
  if (Number(a[a.length - 1]) === Number(b[a.length - 1])) {
    return 0
  }
  else {
    return (Number(a[a.length - 1]) < Number(b[a.length - 1])) ? 1 : -1;
  }
}

module.exports = combineTables