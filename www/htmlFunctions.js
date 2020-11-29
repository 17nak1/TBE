


function activeRngVal(checkbox, i) {
  valDiv = document.querySelectorAll('div#val')[i];
  rngDiv = document.querySelectorAll('div#rng')[i];
  let rows = document.getElementById('sobolBound').querySelectorAll('tr');
  let row = rows[i+1];
  let cols = row.querySelectorAll('td');
  let flagcol = cols[3].querySelector('.flag-bound');

  if (checkbox.checked) {
    valDiv.style.display = "none";
    rngDiv.style.display = "";
    flagcol.querySelectorAll('input')[0].checked=false;
  } else {
    valDiv.style.display = "";
    rngDiv.style.display = "none"; 
    flagcol.querySelectorAll('input')[0].checked=true;
    flagcol.querySelectorAll('input')[1].checked=false;
    flagcol.querySelectorAll('input')[2].checked=false;
  }
}

function checkBox(idDiv,idInput) {
  let flag = document.querySelectorAll('.flag-bound')[idDiv];
  if( idDiv >= 27) {
    for (let i = 1; i < 3; i++) {
      flag.querySelectorAll('input')[i].checked = false;
    }
  } else {
    for (let i = 0; i < 3; i++) {
      flag.querySelectorAll('input')[i].checked = false;
    }
    flag.querySelectorAll('input')[idInput].checked = true;
  }
}

function activeRefine(checkbox) {
  if (!checkbox.checked) {
    document.getElementById("autoRefinement").textContent = "Manual refinements";
    document.getElementById("refinementField").style.display='none';
    document.getElementById("refinementFieldNumber").style.display='none';
    return 0;
  }else{
    document.getElementById("autoRefinement").textContent = "Automatic refinements"; 
    document.getElementById("refinementField").style.display=''; 
    document.getElementById("refinementFieldNumber").style.display='';
    return 1;
  }  
}

function displayEstimetedTab(fixedIndices) {
  let indecices = Array.from(Array(27).keys());
  for ( let i = 0; i < indecices.length; i++) {
    for ( let j = 0; j < fixedIndices.length; j++) {
      if ( indecices[i] === fixedIndices[j]) {
        indecices.splice(i,1);
        i -= 1;
      }
    }
  }
  return indecices;
}
function addFile() {
  var input=document.createElement('input');
  input.type="file";
  input.class="combine-input";
  document.getElementById("file-combine-container").append(input)
}
// function removeFile() {
//   let select = document.getElementById("file-combine-container");
// }
// let select = document.getElementById("combine-input");
// var newFileList = [];

// let removed = 0;
// let outEl = document.getElementById('file-names');
// let fileBlobs = [];
// select.onchange = function(e) {
//   let n = 0;
//   var newFileListTemp = [];

//   for(n = 0; n<this.files.length;n++) {
//     var reader = new FileReader ()
//     reader.onload = function () {
//       fileBlobs.push(this.result.split('\n'));
//     }
//     reader.readAsText(this.files[n]);
//     newFileListTemp.push(this.files[n]);
//   }
//   console.log(fileBlobs);
//   //newFileListTemp = Array.from(this.files);
//   newFileList.push(newFileListTemp);
//   let i = 0;
//   for(i = 0;i<this.files.length;i++ ) {
//     let cont = document.createElement('div');
//     cont.class="file-info-container";
//     cont.id = "file" + i;
//     let rmBtn = document.createElement('button');
//     rmBtn.innerHTML = "Remove";
//     let fn = "rmFile(" + i + ")";
//     rmBtn.setAttribute( "onClick", fn );
//     const pEl = document.createElement('p');
//     var node = document.createTextNode(this.files[i].name);
//     pEl.append(node);
//     cont.append(pEl);
//     cont.append(rmBtn);
//     outEl.append(cont);
//   }
// }

function rmFile(i) {
  let fileid = 'file' + i;
  document.getElementById(fileid).remove();
  newFileList.splice(i-removed, 1);
  fileBlobs.splice(i-removed, 1);
  removed += 1;
  select.files.length -= 1;
  console.log(fileBlobs);
}


tableCsv = [];
function combine() {

    for ( let i = 0; i <fileBlobs.length; i++) {
      fileBlobs[i].splice(0, 1);
    }
  for ( let i = 0; i <fileBlobs.length; i++) {
    for (let j = 0; j < fileBlobs[i].length; j++) {
      tableCsv.push(fileBlobs[i][j].split(','));
    }
  }

    for ( let i = 0; i <tableCsv.length; i++) {
      for (let j = 0; j < tableCsv[0].length; j++) {
        tableCsv[i][j] = Number(tableCsv[i][j])
      }
    }
    for ( let i = 0; i <tableCsv.length; i++) {
      if(tableCsv[i][0]==0) {
        tableCsv.splice(i,1)
      }
    }
    //tableCsv Sort
  Csv(tableCsv, 'TBE_all.csv');
}
// Helper functions
function Csv (res, type) {
  var csv = ''
  res.forEach(function (row) {
    csv += row.join(',')
    csv += '\n'
  })
  var hiddenElement = document.createElement('a')
  hiddenElement.href = 'data:text/csv;charset=utf-8,' + encodeURI(csv)
  hiddenElement.setAttribute('download', type)
  hiddenElement.click()
}
