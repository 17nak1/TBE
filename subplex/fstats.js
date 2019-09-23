/*
*  @file         dist.js
*                fstats modifies the global variables nfxe,fxstat.
*  @inputs
*   fx          - most recent evaluation of f at best x
*
*   ifxwt       - integer weight for fx
*
*   reset       - logical switch
*                = true  : initialize nfxe,fxstat
*                = false : update nfxe,fxstat
*
*  @author       Nazila Akhavan
*  @date         Sep 2019
*  @references   Tom Rowan, Department of Computer Sciences, University of Texas at Austin
*                https://www.netlib.org/opt/
*/

let fstats = function (fx,ifxwt,reset) {
    let nsv
    if (reset) {
      this.nfxe = ifxwt
      this.fxstat[1] = fx
      this.fxstat[2] = fx
      this.fxstat[3] = fx
      this.fxstat[4] = 0
    } else {
      nsv = this.nfxe
      f1sv = this.fxstat[1]
      this.nfxe = this.nfxe + ifxwt
      this.fxstat[1] = this.fxstat[1] + ifxwt * (fx - this.fxstat[1]) / this.nfxe
      this.fxstat[2] = Math.max(this.fxstat[2], fx)
      this.fxstat[3] = Math.min(this.fxstat[3], fx)
      fscale = Math.max(Math.abs(this.fxstat[2]),Math.abs(this.fxstat[3]),1)
      this.fxstat[4] = fscale * Math.sqrt(((nsv-1)*(this.fxstat[4]/fscale)**2+
                            nsv*((this.fxstat[1]-f1sv)/fscale)**2+
                            ifxwt*((fx-this.fxstat[1])/fscale)**2) / (this.nfxe-1))
    }
    // console.log(this.nfxe,this.fxstat)
    return 0
  }
  
module.exports = fstats;
