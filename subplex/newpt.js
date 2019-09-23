
/*
*  @file         newpt.js
*                newpt performs reflections, expansions, contractions, and
*                shrinkages (massive contractions) by computing:
*
*                xbase + coef * (xbase - xold)
*
*                The result is stored in xnew if new equals to true,
*                in xold otherwise.
*
*                use :  coef > 0 to reflect
*                       coef < 0 to expand, contract, or shrink
*  @input
*              ns     - number of components (subspace dimension)
*
*              coef   - one of four simplex method coefficients
*
*              xbase  - double precision ns-vector representing base
*                       point
*
*              xold   - double precision ns-vector representing old
*                       point
*
*              nnew    - logical switch
*                       = true  : store result in xnew
*                       = false : store result in xold, xnew is not
*                                   referenced
*
*  @output
*
*              xold   - unchanged if new .eq. true, contains new
*                       point otherwise
*
*              xnew   - double precision ns-vector representing new
*                       point if  new is true, not referenced
*                       otherwise
*
*              small  - logical flag
*                       = true  : coincident points
*                       = false : otherwise
*
*  @author       Nazila Akhavan
*  @date         Sep 2019
*  @references   Tom Rowan, Department of Computer Sciences, University of Texas at Austin
*                https://www.netlib.org/opt/
*/
// Note: The DBLE function returns a double-precision REAL value given a numeric argument. It is removed in JS.


let newpt = function(ns,coef,xbase,xold,nnew,xnew,small){
    let i
    let xoldi
    let eqbase,eqold

    eqbase = true
    eqold = true
    if (nnew){
        for(i = 1; i<= ns; i++){
            xnew[i] = xbase[i]+coef*(xbase[i]-xold[i])
            eqbase = eqbase && (xnew[i] === xbase[i])
            eqold = eqold && (xnew[i] === xold[i])
        }
    }
    else{
        for(i = 1; i<= ns; i++){
            xoldi = xold[i]
            xold[i] = xbase[i]+coef*(xbase[i]-xold[i])
            eqbase = eqbase && (xold[i] === xbase[i])
            eqold = eqold && (xold[i] === xoldi)
         }
        }
    small[0] =  eqbase || eqold
    // console.log(xold,xnew,small)
}

module.exports = newpt;

