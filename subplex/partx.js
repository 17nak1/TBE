/*
*  @file         partx.js
*                partx partitions the vector x by grouping components of
*                similar magnitude of change.
*  @input
*
*                n      - number of components (problem dimension)
*
*                ip     - permutation vector
*
*                absdx  - vector of magnitude of change in x
*
*                nsvals - integer array dimensioned >= int(n/nsmin)
*
*  @output
*
*                nsubs  - number of subspaces
*
*                nsvals - integer array of subspace dimensions
*
*  @author       Nazila Akhavan
*  @date         Sep 2019
*  @references   Tom Rowan, Department of Computer Sciences, University of Texas at Austin
*                https://www.netlib.org/opt/
*/


// WE need to define these global; common /usubc/ alpha,beta,gamma,delta,psi,omega,nsmin,nsmax,irepl,ifxsw,bonus,fstop,nfstop,nfxe,fxstat(4),ftest,minf,initx,newx

let partx = function(n,ip,absdx,nsubs,nsvals){
  let i,nleft,ns1,ns2,nused;
  let asleft,as1,as1max,as2,gap,gapmax;
  nsubs[0] = 0
  nused = 0
  nleft = n
  asleft = absdx[1];
  for(i = 2; i <= n ; i++){
    asleft = asleft+absdx[i]
  }
  while(nused < n){
    nsubs[0] = nsubs[0]+1
    as1 = 0
    for(i = 1; i <= this.nsmin-1; i++){
      as1 = as1+absdx[ip[nused+i]]
    }
    gapmax = -1
    for(ns1 = this.nsmin; ns1 <= Math.min(this.nsmax,nleft); ns1++){
      as1 = as1+absdx[ip[nused+ns1]]
      ns2 = nleft-ns1
      if (ns2 > 0){
        if (ns2 >= (Math.floor((ns2-1)/this.nsmax)+1)*this.nsmin){
          as2 = asleft-as1
          gap = as1/ns1-as2/ns2
          if (gap > gapmax){
            gapmax = gap
            nsvals[nsubs[0]] = ns1
            as1max = as1
          }
        }
      }
      else if (as1/ns1 > gapmax){
        nsvals[nsubs[0]] = ns1
        return 0;
      }
    }
    nused = nused+nsvals[nsubs[0]]
    nleft = n-nused
    asleft = asleft-as1max
  }
}

module.exports = partx;
// partx(2,[1,2],[.1,.2],1,[1,1])