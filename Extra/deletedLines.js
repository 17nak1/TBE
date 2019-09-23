let paramsFit = []
let flag = 0
// to see which paramsNoic is in paramfixed
for (let i = 0; i < paramsNoic.length; i++) {
  for ( let j = 0; j < paramsFixed.length; j++) {
    if ( paramsNoic[i] === paramsFixed[j]) {
      flag = 1
      break
    }
  }
  if(flag === 0) {
    paramsFit.push(paramsNoic[i])
  }
flag = 0
}
// to see which paramsIc is in paramfixed
let paramsIcFit = []
flag = 0
for (let i = 0; i < paramsIc.length; i++) {
  for ( let j = 0; j < paramsFixed.length; j++) {
    if ( paramsIc[i] === paramsFixed[j]) {
      flag = 1
      break
    }
  }
  if(flag === 0) {
    paramsIcFit.push(paramsNoic[i])
  }
flag = 0
}