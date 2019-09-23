/* 
*  @file        subplx.js                
*               subplx uses the subplex method to solve unconstrained
*               optimization problems.  The method is well suited for
*               optimizing objective functions that are noisy or are
*               discontinuous at the solution.
*
*               subplx sets default optimization options by calling the
*               subroutine subopt.  The user can override these defaults
*               by calling subopt prior to calling subplx, changing the
*               appropriate common variables, and setting the value of
*               mode as indicated below.
*
*               By default, subplx performs minimization.
*
*  @input
*
*                 f      - user supplied function f(n,x) to be optimized,
*                          declared external in calling routine
*
*                 n      - problem dimension
*
*                 tol    - relative error tolerance for x (tol .ge. 0.)
*
*                 maxnfe - maximum number of function evaluations
*
*                 mode   - integer mode switch with binary expansion
*                          (bit 1) (bit 0) :
*                          bit 0 = 0 : first call to subplx
*                                = 1 : continuation of previous call
*                          bit 1 = 0 : use default options
*                                = 1 : user set options
*
*                 scale  - scale and initial stepsizes for corresponding
*                          components of x
*                          (If scale(1) .lt. 0.,
*                          abs(scale(1)) is used for all components of x,
*                          and scale(2),...,scale(n) are not referenced.)
*
*                 x      - starting guess for optimum
*
*                 work   - double precision work array of dimension .ge.
*                          2*n + nsmax*(nsmax+4) + 1
*                          (nsmax is set in subroutine subopt.
*                          default: nsmax = min(5,n))
*
*                 iwork  - integer work array of dimension .ge.
*                          n + int(n/nsmin)
*                          (nsmin is set in subroutine subopt.
*                          default: nsmin = min(2,n))
*
*  @output
*
*                 x      - computed optimum
*
*                 fx     - value of f at x
*
*                 nfe    - number of function evaluations
*
*                 iflag  - error flag
*                          = -2 : invalid input
*                          = -1 : maxnfe exceeded
*                          =  0 : tol satisfied
*                          =  1 : limit of machine precision
*                          =  2 : fstop reached (fstop usage is determined
*                                 by values of options minf, nfstop, and
*                                 irepl. default: f(x) not tested against
*                                 fstop)
*                          iflag should not be reset between calls to
*                          subplx.
*
*  @author       Nazila Akhavan
*  @date         Sep 2019
*  @references   Tom Rowan, Department of Computer Sciences, University of Texas at Austin
*                https://www.netlib.org/opt/
*/



//       external f,sortd,evalf,partx,setstp,simplx,subopt
//       external dcopy
//       intrinsi*               abs,mod

let subplx = function(f,n,tol,maxnfe,scale,x,fx,nfe,work,iwork,iflag){
  let i,ifsptr,ins,insfnl,insptr,ipptr,isptr,istep,istptr,ns,nsubs=[].dArray();
  let bnsfa = [[-1,-2,0],[1,0,2]];
  let dum = [].dArray() ,scl = [].dArray(),sfx = [].dArray(),xpscl = [].dArray();
  let cmode;

  let goto_variable = 10;
  while (true)
  {
      switch (goto_variable){
        case 10:
          goto_variable = 15;
          if(scale[1] >= 0){
            //
            // 10
            //
            for( i = 1; i <= n; i++){
              xpscl = x[i]+scale[i];
              if (xpscl === x[i]){
                 goto_variable = 120;
                 break;
              }
            }
          }
          else{
            //
            //20
            //
            scl[0] = Math.abs(scale[1])
            for( i = 1; i <= n; i++){
              xpscl = x[i]+scl[0];
              if (xpscl === x[i]){
                goto_variable = 120;
                break;
              }
            }
          }
        break;

        case 15:

          this.subopt(n)

          istptr = n + 1
          isptr = istptr + n;
          ifsptr = isptr + this.nsmax * (this.nsmax + 3)
          insptr = n + 1
          if (scale[0] > 0){
            this.dcopy(n,scale,1,work,1)
            this.dcopy(n,scale,1,work.clone(istptr),1)
          }
          else{
            this.dcopy(n,scl,0,work,1)
            this.dcopy(n,scl,0,work.clone(istptr),1)
          }

          //
          // 30
          //

          for( i = 1; i <= n; i++){
            iwork[i] = i
          }
          
          this.nfe = 0
          this.nfxe = 1
          this.ftest = 0
          cmode = false
          this.initx = true
          this.evalf(f,0,iwork,dum,n,x,sfx,this.nfe)
          this.initx = false
          
          goto_variable = 40;
        break;
        
        case 40:  
          //
          // 40
          //

          //
          //50
          //

          for( i = 1; i <= n; i++){
            work[i] = Math.abs(work[i])
          }
          this.sortd(n,work,iwork)
          this.partx(n,iwork,work,nsubs,iwork.clone(insptr))
          this.dcopy(n,x,1,work,1)
          ins = insptr
          insfnl = insptr + nsubs[0] - 1
          ipptr = 1

          goto_variable = 60;
        break;

        case 60:

          //
          // 60
          //

          ns = iwork[ins]
          //
          // continue
          //

          this.simplx(f,n,work.clone(istptr),ns,iwork.clone(ipptr),maxnfe,cmode,x,sfx,nfe,work.clone(isptr),work.clone(ifsptr),iflag)
          cmode = false
          if (iflag !== 0){
            goto_variable = 110
          }
          else{
            goto_variable = 70;
          }
        break;

        case 70:
          if (ins < insfnl){
            ins = ins + 1
            ipptr = ipptr + ns
            goto_variable = 60;
          }
          else{
            goto_variable = 80;
          }
        break;

        case 80:

          //
          // 80
          //

          for( i = 1; i <= n; i++){
            work[i] = x[i] - work[i]
          }

          //
          // continue
          //
          istep = istptr
          goto_variable = 100;
        break;
        
        case 100:

          //
          // 100
          //
          for( i = 1; i <= n; i++){
            if (Math.max(Math.abs(work[i]),Math.abs(work[istep] * this.psi)) / Math.max(Math.abs(x[i]),1) > tol){
              this.setstp(nsubs,n,work,work.clone(istptr));
              goto_variable = 40;
              break;
            }
            goto_variable = 105;
            istep = istep + 1;
          }
        break;

        case 105:

          iflag = 0
          goto_variable = 110;
        break;

        case 110:
          //
          // 110
          //
          this.fx = sfx[0]
          return 0;
        break;

        case 120:

          //
          //120
          //
          iflag = -2
          return 0;
        break;             
      }
  }

}

module.exports = subplx;