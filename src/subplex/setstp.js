let setstp = function(nsubs,n,deltax,step){
    let i
    let dasum,stpfac
    if (nsubs[0] > 1){
      stpfac = Math.min(Math.max(this.dasum(n,deltax,1)/this.dasum(n,step,1),this.omega),1/this.omega)
    }
    else{
      stpfac = this.psi
    }
    this.dscal(n,stpfac,step,1)

    for(i = 1; i <= n ; i++){
      if (deltax[i] !== 0){
        step[i] =  Math.abs(step[i]) * Math.sign(deltax[i])
      }
      else{
        step[i] = -step[i]
      }      
    }
}

module.exports = setstp;