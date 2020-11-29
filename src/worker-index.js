/**
 * @author  Ryan Rossiter, ryan@kingsds.network
 * @date    Sep 2019
 * @file    index.js   
 *          This is the entry into the worker module that will load
 *          from the a.out.js and a.out.wasm Emscripten outputs.
 */

// Keep in mind that this whole script will be run every time the work function
// is called. That's why we have to check if self.Module exists here, so it can be reused.
const Module = self.Module = (typeof self.Module !== 'undefined'? self.Module : {
  init: false // flag for checking if the Module needs to be initialized
});

const trajMatch = require('./trajMatch/runTrajHtml.js');

// Add the workerfn to the self namespace so it can be called by whatever wraps the output bundle
const workerfn = self.workerfn = async (...args) => {
  const work = (resolve, reject) => () => {
    let result;
    let date = new Date();
    try {
      progress();
      // console.error("STARTING TRAJMATCH");
      result = trajMatch(...args);

      if (isNaN(result[result.length - 1])) {
        result = [...new Array(args[0].length).fill(0), NaN];
      }
    } catch (e) {
      console.error(e);
      result = [...new Array(args[0].length).fill(0), NaN];
    }
    console.debug(new Date() - date)
    resolve(result);
  }

  var firstInit = false;
  if (!Module.init) {
    Module.init = firstInit = true;

    // Load the WASM file into a byte array, the Emscripten wrapper
    // will see this and use it's contents instead of trying to fetch the
    // WASM file via XHR
    Module.wasmBinary = new Uint8Array(require('./lsoda/lsoda.wasm'))

    // Will patch Module with rest of Emscripten
    require('./lsoda/lsoda.js');
    // console.log("LOADED MODULE");
  } else {
    // Module gets reused from previous execution
    // console.log("Didn't need to load module :)");
  }

  // If the module was just initialized then we have to wait for Emscripten
  // to call the work function in the onRuntimeInitialized callback,
  // otherwise just call the work function.
  return await new Promise((resolve, reject) => {
    if (firstInit) Module['onRuntimeInitialized'] = work(resolve, reject);
    else work(resolve, reject)();
  });
}

export default workerfn;
