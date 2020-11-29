let evalf = function(f,ns,ips,xs,n,x,sfx,nfe){
    let i,fx;
    for(i = 1; i<= ns; i++){
        x[ips[i]] = xs[i]
    }

      fx = f(n,x)
      sfx[0] = fx
      this.nfe = this.nfe+1
}

module.exports = evalf;