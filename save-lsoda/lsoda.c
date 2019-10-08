/*
  This is a C version of the LSODA library. I acquired the original
  source code from this web page:

    http://www.ccl.net/cca/software/SOURCES/C/kinetics2/index.shtml

  I merged several C files into one and added a simpler interface. I
  also made the array start from zero in functions called by lsoda(),
  and fixed two minor bugs: a) small memory leak in freevectors(); and
  b) misuse of lsoda() in the example.

  The original source code came with no license or copyright
  information. I now release this file under the MIT/X11 license. All
  authors' notes are kept in this file.

  - Heng Li <lh3lh3@gmail.com>
 */

/* The MIT License

   Copyright (c) 2009 Genome Research Ltd (GRL).

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
*/

/* Contact: Heng Li <lh3@sanger.ac.uk> */

typedef void    (*_lsoda_f) (double, double *, double *, void *);

/************
 * idamax.c *
 ************/

#include <math.h>

static int 
idamax(n, dx, incx)
	double         *dx;
	int             n, incx;

/* Purpose : Find largest component of double vector dx


   --- Input ---

   n    : number of elements in input vector
   dx   : double vector with n+1 elements, dx[0] is not used
   incx : storage spacing between elements of dx


   --- Output ---

   idamax : smallest index, 0 if n <= 0


   Find smallest index of maximum magnitude of dx.
   idamax = first i, i=1 to n, to minimize fabs( dx[1-incx+i*incx] ).

*/

{
	double          dmax, xmag;
	int             i, ii, xindex;

	xindex = 0;
	if (n <= 0)
		return xindex;
	xindex = 1;
	if (n <= 1 || incx <= 0)
		return xindex;

/* Code for increments not equal to 1.   */

	if (incx != 1) {
		dmax = fabs(dx[1]);
		ii = 2;
		for (i = 1 + incx; i <= n * incx; i = i + incx) {
			xmag = fabs(dx[i]);
			if (xmag > dmax) {
				xindex = ii;
				dmax = xmag;
			}
			ii++;
		}
		return xindex;
	}
/* Code for increments equal to 1.  */

	dmax = fabs(dx[1]);
	for (i = 2; i <= n; i++) {
		xmag = fabs(dx[i]);
		if (xmag > dmax) {
			xindex = i;
			dmax = xmag;
		}
	}
	return xindex;

}

/***********
 * dscal.c *
 ***********/

void 
dscal(n, da, dx, incx)
	double          da, *dx;
	int             n, incx;

/* Purpose : scalar vector multiplication

   dx = da * dx


   --- Input ---

   n    : number of elements in input vector
   da   : double scale factor
   dx   : double vector with n+1 elements, dx[0] is not used
   incx : storage spacing between elements of dx


   --- Output ---

   dx = da * dx, unchanged if n <= 0


   For i = 0 to n-1, replace dx[1+i*incx] with
   da * dx[1+i*incx].

*/

{
	int             m, i;

	if (n <= 0)
		return;

/* Code for increments not equal to 1.  */

	if (incx != 1) {
		for (i = 1; i <= n * incx; i = i + incx)
			dx[i] = da * dx[i];
		return;
	}
/* Code for increments equal to 1.  */

/* Clean-up loop so remaining vector length is a multiple of 5.  */

	m = n % 5;
	if (m != 0) {
		for (i = 1; i <= m; i++)
			dx[i] = da * dx[i];
		if (n < 5)
			return;
	}
	for (i = m + 1; i <= n; i = i + 5) {
		dx[i] = da * dx[i];
		dx[i + 1] = da * dx[i + 1];
		dx[i + 2] = da * dx[i + 2];
		dx[i + 3] = da * dx[i + 3];
		dx[i + 4] = da * dx[i + 4];
	}
	return;

}

/**********
 * ddot.c *
 **********/

static double 
ddot(n, dx, incx, dy, incy)
	double         *dx, *dy;
	int             n, incx, incy;

/*
   Purpose : Inner product dx . dy


   --- Input ---

   n    : number of elements in input vector(s)
   dx   : double vector with n+1 elements, dx[0] is not used
   incx : storage spacing between elements of dx
   dy   : double vector with n+1 elements, dy[0] is not used
   incy : storage spacing between elements of dy


   --- Output ---

   ddot : dot product dx . dy, 0 if n <= 0


   ddot = sum for i = 0 to n-1 of
   dx[lx+i*incx] * dy[ly+i*incy] where lx = 1 if
   incx >= 0, else lx = (-incx)*(n-1)+1, and ly
   is defined in a similar way using incy.

*/

{
	double          dotprod;
	int             ix, iy, i, m;

	dotprod = 0.;
	if (n <= 0)
		return dotprod;

/* Code for unequal or nonpositive increments.  */

	if (incx != incy || incx < 1) {
		ix = 1;
		iy = 1;
		if (incx < 0)
			ix = (-n + 1) * incx + 1;
		if (incy < 0)
			iy = (-n + 1) * incy + 1;
		for (i = 1; i <= n; i++) {
			dotprod = dotprod + dx[ix] * dy[iy];
			ix = ix + incx;
			iy = iy + incy;
		}
		return dotprod;
	}
/* Code for both increments equal to 1.  */

/* Clean-up loop so remaining vector length is a multiple of 5.  */

	if (incx == 1) {
		m = n % 5;
		if (m != 0) {
			for (i = 1; i <= m; i++)
				dotprod = dotprod + dx[i] * dy[i];
			if (n < 5)
				return dotprod;
		}
		for (i = m + 1; i <= n; i = i + 5)
			dotprod = dotprod + dx[i] * dy[i] + dx[i + 1] * dy[i + 1] +
				dx[i + 2] * dy[i + 2] + dx[i + 3] * dy[i + 3] +
				dx[i + 4] * dy[i + 4];
		return dotprod;
	}
/* Code for positive equal nonunit increments.   */

	for (i = 1; i <= n * incx; i = i + incx)
		dotprod = dotprod + dx[i] * dy[i];
	return dotprod;

}

/***********
 * daxpy.c *
 ***********/

/*
From tam@dragonfly.wri.com Wed Apr 24 15:48:31 1991
Return-Path: <tam>
Date: Wed, 24 Apr 91 17:48:43 CDT
From: tam@dragonfly.wri.com
To: whitbeck@sanjuan.wrc.unr.edu
*/

static void 
daxpy(n, da, dx, incx, dy, incy)
	double          da, *dx, *dy;
	int             n, incx, incy;

/*
   Purpose : To compute

   dy = da * dx + dy


   --- Input ---

   n    : number of elements in input vector(s)
   da   : double scalar multiplier
   dx   : double vector with n+1 elements, dx[0] is not used
   incx : storage spacing between elements of dx
   dy   : double vector with n+1 elements, dy[0] is not used
   incy : storage spacing between elements of dy


   --- Output ---

   dy = da * dx + dy, unchanged if n <= 0


   For i = 0 to n-1, replace dy[ly+i*incy] with
   da*dx[lx+i*incx] + dy[ly+i*incy], where lx = 1
   if  incx >= 0, else lx = (-incx)*(n-1)+1 and ly is
   defined in a similar way using incy.

*/

{
	int             ix, iy, i, m;

	if (n < 0 || da == 0.)
		return;

/* Code for nonequal or nonpositive increments.  */

	if (incx != incy || incx < 1) {
		ix = 1;
		iy = 1;
		if (incx < 0)
			ix = (-n + 1) * incx + 1;
		if (incy < 0)
			iy = (-n + 1) * incy + 1;
		for (i = 1; i <= n; i++) {
			dy[iy] = dy[iy] + da * dx[ix];
			ix = ix + incx;
			iy = iy + incy;
		}
		return;
	}
/* Code for both increments equal to 1.   */

/* Clean-up loop so remaining vector length is a multiple of 4.  */

	if (incx == 1) {
		m = n % 4;
		if (m != 0) {
			for (i = 1; i <= m; i++)
				dy[i] = dy[i] + da * dx[i];
			if (n < 4)
				return;
		}
		for (i = m + 1; i <= n; i = i + 4) {
			dy[i] = dy[i] + da * dx[i];
			dy[i + 1] = dy[i + 1] + da * dx[i + 1];
			dy[i + 2] = dy[i + 2] + da * dx[i + 2];
			dy[i + 3] = dy[i + 3] + da * dx[i + 3];
		}
		return;
	}
/* Code for equal, positive, nonunit increments.   */

	for (i = 1; i <= n * incx; i = i + incx)
		dy[i] = da * dx[i] + dy[i];
	return;

}

/***********
 * dgesl.c *
 ***********/

static void 
dgesl(a, n, ipvt, b, job)
	double        **a, *b;
	int             n, *ipvt, job;

/*
   Purpose : dgesl solves the linear system
   a * x = b or Transpose(a) * x = b
   using the factors computed by dgeco or degfa.


   On Entry :

      a    : double matrix of dimension ( n+1, n+1 ),
             the output from dgeco or dgefa.
             The 0-th row and column are not used.
      n    : the row dimension of a.
      ipvt : the pivot vector from degco or dgefa.
      b    : the right hand side vector.
      job  : = 0       to solve a * x = b,
             = nonzero to solve Transpose(a) * x = b.


   On Return :

      b : the solution vector x.


   Error Condition :

      A division by zero will occur if the input factor contains
      a zero on the diagonal.  Technically this indicates
      singularity but it is often caused by improper argments or
      improper setting of the pointers of a.  It will not occur
      if the subroutines are called correctly and if dgeco has
      set rcond > 0 or dgefa has set info = 0.


   BLAS : daxpy, ddot
*/

{
	int             nm1, k, j;
	double          t;

	nm1 = n - 1;

/*
   Job = 0, solve a * x = b.
*/
	if (job == 0) {
/*
   First solve L * y = b.
*/
		for (k = 1; k <= n; k++) {
			t = ddot(k - 1, a[k], 1, b, 1);
			b[k] = (b[k] - t) / a[k][k];
		}
/*
   Now solve U * x = y.
*/
		for (k = n - 1; k >= 1; k--) {
			b[k] = b[k] + ddot(n - k, a[k] + k, 1, b + k, 1);
			j = ipvt[k];
			if (j != k) {
				t = b[j];
				b[j] = b[k];
				b[k] = t;
			}
		}
		return;
	}
/*
   Job = nonzero, solve Transpose(a) * x = b.

   First solve Transpose(U) * y = b.
*/
	for (k = 1; k <= n - 1; k++) {
		j = ipvt[k];
		t = b[j];
		if (j != k) {
			b[j] = b[k];
			b[k] = t;
		}
		daxpy(n - k, t, a[k] + k, 1, b + k, 1);
	}
/*
   Now solve Transpose(L) * x = y.
*/
	for (k = n; k >= 1; k--) {
		b[k] = b[k] / a[k][k];
		t = -b[k];
		daxpy(k - 1, t, a[k], 1, b, 1);
	}

}

/***********
 * dgefa.c *
 ***********/

void 
dgefa(a, n, ipvt, info)
	double        **a;
	int             n, *ipvt, *info;

/*
   Purpose : dgefa factors a double matrix by Gaussian elimination.

   dgefa is usually called by dgeco, but it can be called directly
   with a saving in time if rcond is not needed.
   (Time for dgeco) = (1+9/n)*(time for dgefa).

   This c version uses algorithm kji rather than the kij in dgefa.f.
   Note that the fortran version input variable lda is not needed.


   On Entry :

      a   : double matrix of dimension ( n+1, n+1 ),
            the 0-th row and column are not used.
            a is created using NewDoubleMatrix, hence
            lda is unnecessary.
      n   : the row dimension of a.

   On Return :

      a     : a lower triangular matrix and the multipliers
              which were used to obtain it.  The factorization
              can be written a = L * U where U is a product of
              permutation and unit upper triangular matrices
              and L is lower triangular.
      ipvt  : an n+1 integer vector of pivot indices.
      *info : = 0 normal value,
              = k if U[k][k] == 0.  This is not an error
                condition for this subroutine, but it does
                indicate that dgesl or dgedi will divide by
                zero if called.  Use rcond in dgeco for
                a reliable indication of singularity.

                Notice that the calling program must use &info.

   BLAS : daxpy, dscal, idamax
*/

{
	int             j, k, i;
	double          t;

/* Gaussian elimination with partial pivoting.   */

	*info = 0;
	for (k = 1; k <= n - 1; k++) {
/*
   Find j = pivot index.  Note that a[k]+k-1 is the address of
   the 0-th element of the row vector whose 1st element is a[k][k].
*/
		j = idamax(n - k + 1, a[k] + k - 1, 1) + k - 1;
		ipvt[k] = j;
/*
   Zero pivot implies this row already triangularized.
*/
		if (a[k][j] == 0.) {
			*info = k;
			continue;
		}
/*
   Interchange if necessary.
*/
		if (j != k) {
			t = a[k][j];
			a[k][j] = a[k][k];
			a[k][k] = t;
		}
/*
   Compute multipliers.
*/
		t = -1. / a[k][k];
		dscal(n - k, t, a[k] + k, 1);
/*
   Column elimination with row indexing.
*/
		for (i = k + 1; i <= n; i++) {
			t = a[i][j];
			if (j != k) {
				a[i][j] = a[i][k];
				a[i][k] = t;
			}
			daxpy(n - k, t, a[k] + k, 1, a[i] + k, 1);
		}
	}			/* end k-loop  */

	ipvt[n] = n;
	if (a[n][n] == 0.)
		*info = n;

}

/***********
 * lsoda.c *
 ***********/

/*
From tam@dragonfly.wri.com Wed Apr 24 01:35:52 1991
Return-Path: <tam>
Date: Wed, 24 Apr 91 03:35:24 CDT
From: tam@dragonfly.wri.com
To: whitbeck@wheeler.wrc.unr.edu
Subject: lsoda.c
Cc: augenbau@sparc0.brc.uconn.edu


I'm told by Steve Nichols at Georgia Tech that you are interested in
a stiff integrator.  Here's a translation of the fortran code LSODA.

Please note
that there is no comment.  The interface is the same as the FORTRAN
code and I believe the documentation in LSODA will suffice.
As usual, a free software comes with no guarantee.

Hon Wah Tam
Wolfram Research, Inc.
tam@wri.com
*/

#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#define max( a , b )  ( (a) > (b) ? (a) : (b) )
#define min( a , b )  ( (a) < (b) ? (a) : (b) )

#define ETA 2.2204460492503131e-16

static void     stoda(int neq, double *y, _lsoda_f f, void *_data);
static void     correction(int neq, double *y, _lsoda_f f, int *corflag, double pnorm, double *del, double *delp, double *told,
						   int *ncf, double *rh, int *m, void *_data);
static void     prja(int neq, double *y, _lsoda_f f, void *_data);
static void     terminate(int *istate);
static void     terminate2(double *y, double *t);
static void     successreturn(double *y, double *t, int itask, int ihit, double tcrit, int *istate);
static void     freevectors(void); /* this function does nothing */
static void     _freevectors(void);
static void     ewset(int itol, double *rtol, double *atol, double *ycur);
static void     resetcoeff(void);
static void     solsy(double *y);
static void     endstoda(void);
static void     orderswitch(double *rhup, double dsm, double *pdh, double *rh, int *orderflag);
static void     intdy(double t, int k, double *dky, int *iflag);
static void     corfailure(double *told, double *rh, int *ncf, int *corflag);
static void     methodswitch(double dsm, double pnorm, double *pdh, double *rh);
static void     cfode(int meth);
static void     scaleh(double *rh, double *pdh);
static double   fnorm(int n, double **a, double *w);
static double   vmnorm(int n, double *v, double *w);

static int      g_nyh = 0, g_lenyh = 0;

/* newly added static variables */

static int      ml, mu, imxer;
static int      mord[3] = {0, 12, 5};
static double   sqrteta, *yp1, *yp2;
static double   sm1[13] = {0., 0.5, 0.575, 0.55, 0.45, 0.35, 0.25, 0.2, 0.15, 0.1, 0.075, 0.05, 0.025};

/* static variables for lsoda() */

static double   ccmax, el0, h, hmin, hmxi, hu, rc, tn;
static int      illin = 0, init = 0, mxstep, mxhnil, nhnil, ntrep = 0, nslast, nyh, ierpj, iersl,
                jcur, jstart, kflag, l, meth, miter, maxord, maxcor, msbp, mxncf, n, nq, nst,
                nfe, nje, nqu;
static double   tsw, pdnorm;
static int      ixpr = 0, jtyp, mused, mxordn, mxords;

/* no static variable for prja(), solsy() */
/* static variables for stoda() */

static double   conit, crate, el[14], elco[13][14], hold, rmax, tesco[13][4];
static int      ialth, ipup, lmax, nslp;
static double   pdest, pdlast, ratio, cm1[13], cm2[6];
static int      icount, irflag;

/* static variables for various vectors and the Jacobian. */

static double **yh, **wm, *ewt, *savf, *acor;
static int     *ipvt;

/*
   The following are useful statistics.

   hu,
   h,
   tn,
   tolsf,
   tsw,
   nst,
   nfe,
   nje,
   nqu,
   nq,
   imxer,
   mused,
   meth
*/


/* Terminate lsoda due to illegal input. */
static void terminate(int *istate)
{
	if (illin == 5) {
		fprintf(stderr, "[lsoda] repeated occurrence of illegal input. run aborted.. apparent infinite loop\n");
	} else {
		illin++;
		*istate = -3;
	}
}


/* Terminate lsoda due to various error conditions. */
static void terminate2(double *y, double *t)
{
	int             i;
	yp1 = yh[1];
	for (i = 1; i <= n; i++)
		y[i] = yp1[i];
	*t = tn;
	illin = 0;
	freevectors();
	return;

}

/*
   The following block handles all successful returns from lsoda.
   If itask != 1, y is loaded from yh and t is set accordingly.
   *Istate is set to 2, the illegal input counter is zeroed, and the
   optional outputs are loaded into the work arrays before returning.
*/

static void successreturn(double *y, double *t, int itask, int ihit, double tcrit, int *istate)
{
	int             i;
	yp1 = yh[1];
	for (i = 1; i <= n; i++)
		y[i] = yp1[i];
	*t = tn;
	if (itask == 4 || itask == 5)
		if (ihit)
			*t = tcrit;
	*istate = 2;
	illin = 0;
	freevectors();
}

/*
c-----------------------------------------------------------------------
c this is the march 30, 1987 version of
c lsoda.. livermore solver for ordinary differential equations, with
c         automatic method switching for stiff and nonstiff problems.
c
c this version is in double precision.
c
c lsoda solves the initial value problem for stiff or nonstiff
c systems of first order ode-s,
c     dy/dt = f(t,y) ,  or, in component form,
c     dy(i)/dt = f(i) = f(i,t,y(1),y(2),...,y(neq)) (i = 1,...,neq).
c
c this a variant version of the lsode package.
c it switches automatically between stiff and nonstiff methods.
c this means that the user does not have to determine whether the
c problem is stiff or not, and the solver will automatically choose the
c appropriate method.  it always starts with the nonstiff method.
c
c authors..
c                linda r. petzold  and  alan c. hindmarsh,
c                computing and mathematics research division, l-316
c                lawrence livermore national laboratory
c                livermore, ca 94550.
c
c references..
c 1.  alan c. hindmarsh,  odepack, a systematized collection of ode
c     solvers, in scientific computing, r. s. stepleman et al. (eds.),
c     north-holland, amsterdam, 1983, pp. 55-64.
c 2.  linda r. petzold, automatic selection of methods for solving
c     stiff and nonstiff systems of ordinary differential equations,
c     siam j. sci. stat. comput. 4 (1983), pp. 136-148.
c-----------------------------------------------------------------------
c summary of usage.
c
c communication between the user and the lsoda package, for normal
c situations, is summarized here.  this summary describes only a subset
c of the full set of options available.  see the full description for
c details, including alternative treatment of the jacobian matrix,
c optional inputs and outputs, nonstandard options, and
c instructions for special situations.  see also the example
c problem (with program and output) following this summary.
c
c a. first provide a subroutine of the form..
c               subroutine f (neq, t, y, ydot)
c               dimension y(neq), ydot(neq)
c which supplies the vector function f by loading ydot(i) with f(i).
c
c b. write a main program which calls subroutine lsoda once for
c each point at which answers are desired.  this should also provide
c for possible use of logical unit 6 for output of error messages
c by lsoda.  on the first call to lsoda, supply arguments as follows..
c f      = name of subroutine for right-hand side vector f.
c          this name must be declared external in calling program.
c neq    = number of first order ode-s.
c y      = array of initial values, of length neq.
c t      = the initial value of the independent variable.
c tout   = first point where output is desired (.ne. t).
c itol   = 1 or 2 according as atol (below) is a scalar or array.
c rtol   = relative tolerance parameter (scalar).
c atol   = absolute tolerance parameter (scalar or array).
c          the estimated local error in y(i) will be controlled so as
c          to be less than
c             ewt(i) = rtol*abs(y(i)) + atol     if itol = 1, or
c             ewt(i) = rtol*abs(y(i)) + atol(i)  if itol = 2.
c          thus the local error test passes if, in each component,
c          either the absolute error is less than atol (or atol(i)),
c          or the relative error is less than rtol.
c          use rtol = 0.0 for pure absolute error control, and
c          use atol = 0.0 (or atol(i) = 0.0) for pure relative error
c          control.  caution.. actual (global) errors may exceed these
c          local tolerances, so choose them conservatively.
c itask  = 1 for normal computation of output values of y at t = tout.
c istate = integer flag (input and output).  set istate = 1.
c iopt   = 0 to indicate no optional inputs used.
c rwork  = real work array of length at least..
c             22 + neq * max(16, neq + 9).
c          see also paragraph e below.
c lrw    = declared length of rwork (in user-s dimension).
c iwork  = integer work array of length at least  20 + neq.
c liw    = declared length of iwork (in user-s dimension).
c jac    = name of subroutine for jacobian matrix.
c          use a dummy name.  see also paragraph e below.
c jt     = jacobian type indicator.  set jt = 2.
c          see also paragraph e below.
c note that the main program must declare arrays y, rwork, iwork,
c and possibly atol.
c
c c. the output from the first call (or any call) is..
c      y = array of computed values of y(t) vector.
c      t = corresponding value of independent variable (normally tout).
c istate = 2  if lsoda was successful, negative otherwise.
c          -1 means excess work done on this call (perhaps wrong jt).
c          -2 means excess accuracy requested (tolerances too small).
c          -3 means illegal input detected (see printed message).
c          -4 means repeated error test failures (check all inputs).
c          -5 means repeated convergence failures (perhaps bad jacobian
c             supplied or wrong choice of jt or tolerances).
c          -6 means error weight became zero during problem. (solution
c             component i vanished, and atol or atol(i) = 0.)
c          -7 means work space insufficient to finish (see messages).
c
c d. to continue the integration after a successful return, simply
c reset tout and call lsoda again.  no other parameters need be reset.
c
c e. note.. if and when lsoda regards the problem as stiff, and
c switches methods accordingly, it must make use of the neq by neq
c jacobian matrix, j = df/dy.  for the sake of simplicity, the
c inputs to lsoda recommended in paragraph b above cause lsoda to
c treat j as a full matrix, and to approximate it internally by
c difference quotients.  alternatively, j can be treated as a band
c matrix (with great potential reduction in the size of the rwork
c array).  also, in either the full or banded case, the user can supply
c j in closed form, with a routine whose name is passed as the jac
c argument.  these alternatives are described in the paragraphs on
c rwork, jac, and jt in the full description of the call sequence below.
c
c-----------------------------------------------------------------------
*/

void lsoda(_lsoda_f f, int neq, double *y, double *t, double tout, int itol, double *rtol, double *atol,
		   int itask, int *istate, int iopt, int jt,
		   int iwork1, int iwork2, int iwork5, int iwork6, int iwork7, int iwork8, int iwork9,
		   double rwork1, double rwork5, double rwork6, double rwork7, void *_data)
/*
void 
lsoda(f, neq, y, t, tout, itol, rtol, atol, itask, istate,
      iopt, jt, iwork1, iwork2, iwork5, iwork6, iwork7, iwork8,
      iwork9, rwork1, rwork5, rwork6, rwork7, _data)
	_lsoda_f        f;
	void           *_data;

	int             neq, itol, itask, *istate, iopt, jt;
	int             iwork1, iwork2, iwork5, iwork6, iwork7, iwork8, iwork9;
	double         *y, *t, tout, *rtol, *atol;
	double          rwork1, rwork5, rwork6, rwork7;
*/
/*
   If the user does not supply any of these values, the calling program
   should initialize those untouched working variables to zero.

   ml = iwork1
   mu = iwork2
   ixpr = iwork5
   mxstep = iwork6
   mxhnil = iwork7
   mxordn = iwork8
   mxords = iwork9

   tcrit = rwork1
   h0 = rwork5
   hmax = rwork6
   hmin = rwork7
*/


{
	int             mxstp0 = 500, mxhnl0 = 10;

	int             i, iflag, lenyh, ihit;
	double          atoli, ayi, big, h0, hmax, hmx, rh, rtoli, tcrit, tdist, tnext, tol,
	                tolsf, tp, size, sum, w0;

	if (*istate == 1) _freevectors();

/*
   Block a.
   This code block is executed on every call.
   It tests *istate and itask for legality and branches appropriately.
   If *istate > 1 but the flag init shows that initialization has not
   yet been done, an error return occurs.
   If *istate = 1 and tout = t, return immediately.
*/

	if (*istate < 1 || *istate > 3) {
		fprintf(stderr, "[lsoda] illegal istate = %d\n", *istate);
		terminate(istate);
		return;
	}
	if (itask < 1 || itask > 5) {
		fprintf(stderr, "[lsoda] illegal itask = %d\n", itask);
		terminate(istate);
		return;
	}
	if (init == 0 && (*istate == 2 || *istate == 3)) {
		fprintf(stderr, "[lsoda] istate > 1 but lsoda not initialized\n");
		terminate(istate);
		return;
	}
	if (*istate == 1) {
		init = 0;
		if (tout == *t) {
			ntrep++;
			if (ntrep < 5) return;
			fprintf(stderr, "[lsoda] repeated calls with istate = 1 and tout = t. run aborted.. apparent infinite loop\n");
			return;
		}
	}
/*
   Block b.
   The next code block is executed for the initial call ( *istate = 1 ),
   or for a continuation call with parameter changes ( *istate = 3 ).
   It contains checking of all inputs and various initializations.

   First check legality of the non-optional inputs neq, itol, iopt,
   jt, ml, and mu.
*/

	if (*istate == 1 || *istate == 3) {
		ntrep = 0;
		if (neq <= 0) {
			fprintf(stderr, "[lsoda] neq = %d is less than 1\n", neq);
			terminate(istate);
			return;
		}
		if (*istate == 3 && neq > n) {
			fprintf(stderr, "[lsoda] istate = 3 and neq increased\n");
			terminate(istate);
			return;
		}
		n = neq;
		if (itol < 1 || itol > 4) {
			fprintf(stderr, "[lsoda] itol = %d illegal\n", itol);
			terminate(istate);
			return;
		}
		if (iopt < 0 || iopt > 1) {
			fprintf(stderr, "[lsoda] iopt = %d illegal\n", iopt);
			terminate(istate);
			return;
		}
		if (jt == 3 || jt < 1 || jt > 5) {
			fprintf(stderr, "[lsoda] jt = %d illegal\n", jt);
			terminate(istate);
			return;
		}
		jtyp = jt;
		if (jt > 2) {
			ml = iwork1;
			mu = iwork2;
			if (ml < 0 || ml >= n) {
				fprintf(stderr, "[lsoda] ml = %d not between 1 and neq\n", ml);
				terminate(istate);
				return;
			}
			if (mu < 0 || mu >= n) {
				fprintf(stderr, "[lsoda] mu = %d not between 1 and neq\n", mu);
				terminate(istate);
				return;
			}
		}
/* Next process and check the optional inpus.   */

/* Default options.   */

		if (iopt == 0) {
			ixpr = 0;
			mxstep = mxstp0;
			mxhnil = mxhnl0;
			hmxi = 0.;
			hmin = 0.;
			if (*istate == 1) {
				h0 = 0.;
				mxordn = mord[1];
				mxords = mord[2];
			}
		}
		/* end if ( iopt == 0 )   */
		 /* Optional inputs.   */ 
		else {		/* if ( iopt = 1 )  */
			ixpr = iwork5;
			if (ixpr < 0 || ixpr > 1) {
				fprintf(stderr, "[lsoda] ixpr = %d is illegal\n", ixpr);
				terminate(istate);
				return;
			}
			mxstep = iwork6;
			if (mxstep < 0) {
				fprintf(stderr, "[lsoda] mxstep < 0\n");
				terminate(istate);
				return;
			}
			if (mxstep == 0) mxstep = mxstp0;
			mxhnil = iwork7;
			if (mxhnil < 0) {
				fprintf(stderr, "[lsoda] mxhnil < 0\n");
				terminate(istate);
				return;
			}
			if (*istate == 1) {
				h0 = rwork5;
				mxordn = iwork8;
				if (mxordn < 0) {
					fprintf(stderr, "[lsoda] mxordn = %d is less than 0\n", mxordn);
					terminate(istate);
					return;
				}
				if (mxordn == 0) mxordn = 100;
				mxordn = min(mxordn, mord[1]);
				mxords = iwork9;
				if (mxords < 0) {
					fprintf(stderr, "[lsoda] mxords = %d is less than 0\n", mxords);
					terminate(istate);
					return;
				}
				if (mxords == 0) mxords = 100;
				mxords = min(mxords, mord[2]);
				if ((tout - *t) * h0 < 0.) {
					fprintf(stderr, "[lsoda] tout = %g behind t = %g. integration direction is given by %g\n",
							tout, *t, h0);
					terminate(istate);
					return;
				}
			}	/* end if ( *istate == 1 )  */
			hmax = rwork6;
			if (hmax < 0.) {
				fprintf(stderr, "[lsoda] hmax < 0.\n");
				terminate(istate);
				return;
			}
			hmxi = 0.;
			if (hmax > 0)
				hmxi = 1. / hmax;
			hmin = rwork7;
			if (hmin < 0.) {
				fprintf(stderr, "[lsoda] hmin < 0.\n");
				terminate(istate);
				return;
			}
		}		/* end else   *//* end iopt = 1   */
	}			/* end if ( *istate == 1 || *istate == 3 )   */
	/*
	   If *istate = 1, meth is initialized to 1.
	
	   Also allocate memory for yh, wm, ewt, savf, acor, ipvt.
	*/
	if (*istate == 1) {
/*
   If memory were not freed, *istate = 3 need not reallocate memory.
   Hence this section is not executed by *istate = 3.
*/
		sqrteta = sqrt(ETA);
		meth = 1;
		g_nyh = nyh = n;
		g_lenyh = lenyh = 1 + max(mxordn, mxords);

		yh = (double **) calloc(1 + lenyh, sizeof(*yh));
		if (yh == NULL) {
			printf("lsoda -- insufficient memory for your problem\n");
			terminate(istate);
			return;
		}
		for (i = 1; i <= lenyh; i++)
			yh[i] = (double *) calloc(1 + nyh, sizeof(double));

		wm = (double **) calloc(1 + nyh, sizeof(*wm));
		if (wm == NULL) {
			free(yh);
			printf("lsoda -- insufficient memory for your problem\n");
			terminate(istate);
			return;
		}
		for (i = 1; i <= nyh; i++)
			wm[i] = (double *) calloc(1 + nyh, sizeof(double));

		ewt = (double *) calloc(1 + nyh, sizeof(double));
		if (ewt == NULL) {
			free(yh);
			free(wm);
			printf("lsoda -- insufficient memory for your problem\n");
			terminate(istate);
			return;
		}
		savf = (double *) calloc(1 + nyh, sizeof(double));
		if (savf == NULL) {
			free(yh);
			free(wm);
			free(ewt);
			printf("lsoda -- insufficient memory for your problem\n");
			terminate(istate);
			return;
		}
		acor = (double *) calloc(1 + nyh, sizeof(double));
		if (acor == NULL) {
			free(yh);
			free(wm);
			free(ewt);
			free(savf);
			printf("lsoda -- insufficient memory for your problem\n");
			terminate(istate);
			return;
		}
		ipvt = (int *) calloc(1 + nyh, sizeof(int));
		if (ipvt == NULL) {
			free(yh);
			free(wm);
			free(ewt);
			free(savf);
			free(acor);
			printf("lsoda -- insufficient memory for your problem\n");
			terminate(istate);
			return;
		}
	}
/*
   Check rtol and atol for legality.
*/
	if (*istate == 1 || *istate == 3) {
		rtoli = rtol[1];
		atoli = atol[1];
		for (i = 1; i <= n; i++) {
			if (itol >= 3)
				rtoli = rtol[i];
			if (itol == 2 || itol == 4)
				atoli = atol[i];
			if (rtoli < 0.) {
				fprintf(stderr, "[lsoda] rtol = %g is less than 0.\n", rtoli);
				terminate(istate);
				freevectors();
				return;
			}
			if (atoli < 0.) {
				fprintf(stderr, "[lsoda] atol = %g is less than 0.\n", atoli);
				terminate(istate);
				freevectors();
				return;
			}
		}		/* end for   */
	}			/* end if ( *istate == 1 || *istate == 3 )   */
	/*
	   If *istate = 3, set flag to signal parameter changes to stoda.
	*/
	if (*istate == 3) {
		jstart = -1;
	}
/*
   Block c.
   The next block is for the initial call only ( *istate = 1 ).
   It contains all remaining initializations, the initial call to f,
   and the calculation of the initial step size.
   The error weights in ewt are inverted after being loaded.
*/
	if (*istate == 1) {
		tn = *t;
		tsw = *t;
		maxord = mxordn;
		if (itask == 4 || itask == 5) {
			tcrit = rwork1;
			if ((tcrit - tout) * (tout - *t) < 0.) {
				fprintf(stderr, "[lsoda] itask = 4 or 5 and tcrit behind tout\n");
				terminate(istate);
				freevectors();
				return;
			}
			if (h0 != 0. && (*t + h0 - tcrit) * h0 > 0.)
				h0 = tcrit - *t;
		}
		jstart = 0;
		nhnil = 0;
		nst = 0;
		nje = 0;
		nslast = 0;
		hu = 0.;
		nqu = 0;
		mused = 0;
		miter = 0;
		ccmax = 0.3;
		maxcor = 3;
		msbp = 20;
		mxncf = 10;
/*
   Initial call to f.
*/
		(*f) (*t, y + 1, yh[2] + 1, _data);
		nfe = 1;
/*
   Load the initial value vector in yh.
*/
		yp1 = yh[1];
		for (i = 1; i <= n; i++)
			yp1[i] = y[i];
/*
   Load and invert the ewt array.  ( h is temporarily set to 1. )
*/
		nq = 1;
		h = 1.;
		ewset(itol, rtol, atol, y);
		for (i = 1; i <= n; i++) {
			if (ewt[i] <= 0.) {
				fprintf(stderr, "[lsoda] ewt[%d] = %g <= 0.\n", i, ewt[i]);
				terminate2(y, t);
				return;
			}
			ewt[i] = 1. / ewt[i];
		}

/*
   The coding below computes the step size, h0, to be attempted on the
   first step, unless the user has supplied a value for this.
   First check that tout - *t differs significantly from zero.
   A scalar tolerance quantity tol is computed, as max(rtol[i])
   if this is positive, or max(atol[i]/fabs(y[i])) otherwise, adjusted
   so as to be between 100*ETA and 0.001.
   Then the computed value h0 is given by

      h0^(-2) = 1. / ( tol * w0^2 ) + tol * ( norm(f) )^2

   where   w0     = max( fabs(*t), fabs(tout) ),
           f      = the initial value of the vector f(t,y), and
           norm() = the weighted vector norm used throughout, given by
                    the vmnorm function routine, and weighted by the
                    tolerances initially loaded into the ewt array.

   The sign of h0 is inferred from the initial values of tout and *t.
   fabs(h0) is made < fabs(tout-*t) in any case.
*/
		if (h0 == 0.) {
			tdist = fabs(tout - *t);
			w0 = max(fabs(*t), fabs(tout));
			if (tdist < 2. * ETA * w0) {
				fprintf(stderr, "[lsoda] tout too close to t to start integration\n ");
				terminate(istate);
				freevectors();
				return;
			}
			tol = rtol[1];
			if (itol > 2) {
				for (i = 2; i <= n; i++)
					tol = max(tol, rtol[i]);
			}
			if (tol <= 0.) {
				atoli = atol[1];
				for (i = 1; i <= n; i++) {
					if (itol == 2 || itol == 4)
						atoli = atol[i];
					ayi = fabs(y[i]);
					if (ayi != 0.)
						tol = max(tol, atoli / ayi);
				}
			}
			tol = max(tol, 100. * ETA);
			tol = min(tol, 0.001);
			sum = vmnorm(n, yh[2], ewt);
			sum = 1. / (tol * w0 * w0) + tol * sum * sum;
			h0 = 1. / sqrt(sum);
			h0 = min(h0, tdist);
			h0 = h0 * ((tout - *t >= 0.) ? 1. : -1.);
		}		/* end if ( h0 == 0. )   */
		/*
		   Adjust h0 if necessary to meet hmax bound.
		*/
		rh = fabs(h0) * hmxi;
		if (rh > 1.)
			h0 /= rh;
/*
   Load h with h0 and scale yh[2] by h0.
*/
		h = h0;
		yp1 = yh[2];
		for (i = 1; i <= n; i++)
			yp1[i] *= h0;
	}			/* if ( *istate == 1 )   */
	/*
	   Block d.
	   The next code block is for continuation calls only ( *istate = 2 or 3 )
	   and is to check stop conditions before taking a step.
	*/
	if (*istate == 2 || *istate == 3) {
		nslast = nst;
		switch (itask) {
		case 1:
			if ((tn - tout) * h >= 0.) {
				intdy(tout, 0, y, &iflag);
				if (iflag != 0) {
					fprintf(stderr, "[lsoda] trouble from intdy, itask = %d, tout = %g\n", itask, tout);
					terminate(istate);
					freevectors();
					return;
				}
				*t = tout;
				*istate = 2;
				illin = 0;
				freevectors();
				return;
			}
			break;
		case 2:
			break;
		case 3:
			tp = tn - hu * (1. + 100. * ETA);
			if ((tp - tout) * h > 0.) {
				fprintf(stderr, "[lsoda] itask = %d and tout behind tcur - hu\n", itask);
				terminate(istate);
				freevectors();
				return;
			}
			if ((tn - tout) * h < 0.) break;
			successreturn(y, t, itask, ihit, tcrit, istate);
			return;
		case 4:
			tcrit = rwork1;
			if ((tn - tcrit) * h > 0.) {
				fprintf(stderr, "[lsoda] itask = 4 or 5 and tcrit behind tcur\n");
				terminate(istate);
				freevectors();
				return;
			}
			if ((tcrit - tout) * h < 0.) {
				fprintf(stderr, "[lsoda] itask = 4 or 5 and tcrit behind tout\n");
				terminate(istate);
				freevectors();
				return;
			}
			if ((tn - tout) * h >= 0.) {
				intdy(tout, 0, y, &iflag);
				if (iflag != 0) {
					fprintf(stderr, "[lsoda] trouble from intdy, itask = %d, tout = %g\n", itask, tout);
					terminate(istate);
					freevectors();
					return;
				}
				*t = tout;
				*istate = 2;
				illin = 0;
				freevectors();
				return;
			}
		case 5:
			if (itask == 5) {
				tcrit = rwork1;
				if ((tn - tcrit) * h > 0.) {
					fprintf(stderr, "[lsoda] itask = 4 or 5 and tcrit behind tcur\n");
					terminate(istate);
					freevectors();
					return;
				}
			}
			hmx = fabs(tn) + fabs(h);
			ihit = fabs(tn - tcrit) <= (100. * ETA * hmx);
			if (ihit) {
				*t = tcrit;
				successreturn(y, t, itask, ihit, tcrit, istate);
				return;
			}
			tnext = tn + h * (1. + 4. * ETA);
			if ((tnext - tcrit) * h <= 0.)
				break;
			h = (tcrit - tn) * (1. - 4. * ETA);
			if (*istate == 2)
				jstart = -2;
			break;
		}		/* end switch   */
	}			/* end if ( *istate == 2 || *istate == 3 )   */
	/*
	   Block e.
	   The next block is normally executed for all calls and contains
	   the call to the one-step core integrator stoda.
	
	   This is a looping point for the integration steps.
	
	   First check for too many steps being taken, update ewt ( if not at
	   start of problem).  Check for too much accuracy being requested, and
	   check for h below the roundoff level in *t.
	*/
	while (1) {
		if (*istate != 1 || nst != 0) {
			if ((nst - nslast) >= mxstep) {
				fprintf(stderr, "[lsoda] %d steps taken before reaching tout\n", mxstep);
				*istate = -1;
				terminate2(y, t);
				return;
			}
			ewset(itol, rtol, atol, yh[1]);
			for (i = 1; i <= n; i++) {
				if (ewt[i] <= 0.) {
					fprintf(stderr, "[lsoda] ewt[%d] = %g <= 0.\n", i, ewt[i]);
					*istate = -6;
					terminate2(y, t);
					return;
				}
				ewt[i] = 1. / ewt[i];
			}
		}
		tolsf = ETA * vmnorm(n, yh[1], ewt);
		if (tolsf > 0.01) {
			tolsf = tolsf * 200.;
			if (nst == 0) {
				fprintf(stderr, "lsoda -- at start of problem, too much accuracy\n");
				fprintf(stderr, "         requested for precision of machine,\n");
				fprintf(stderr, "         suggested scaling factor = %g\n", tolsf);
				terminate(istate);
				freevectors();
				return;
			}
			fprintf(stderr, "lsoda -- at t = %g, too much accuracy requested\n", *t);
			fprintf(stderr, "         for precision of machine, suggested\n");
			fprintf(stderr, "         scaling factor = %g\n", tolsf);
			*istate = -2;
			terminate2(y, t);
			return;
		}
		if ((tn + h) == tn) {
			nhnil++;
			if (nhnil <= mxhnil) {
				fprintf(stderr, "lsoda -- warning..internal t = %g and h = %g are\n", tn, h);
				fprintf(stderr, "         such that in the machine, t + h = t on the next step\n");
				fprintf(stderr, "         solver will continue anyway.\n");
				if (nhnil == mxhnil) {
					fprintf(stderr, "lsoda -- above warning has been issued %d times,\n", nhnil);
					fprintf(stderr, "         it will not be issued again for this problem\n");
				}
			}
		}
/*
   Call stoda
*/
		stoda(neq, y, f, _data);

/*
   printf( "meth= %d,   order= %d,   nfe= %d,   nje= %d\n",
      meth, nq, nfe, nje );
   printf( "t= %20.15e,   h= %20.15e,   nst=%d\n", tn, h, nst );
   printf( "y= %20.15e,   %20.15e,   %20.15e\n\n\n",
      yh[1][1], yh[1][2], yh[1][3] );
*/

		if (kflag == 0) {
/*
   Block f.
   The following block handles the case of a successful return from the
   core integrator ( kflag = 0 ).
   If a method switch was just made, record tsw, reset maxord,
   set jstart to -1 to signal stoda to complete the switch,
   and do extra printing of data if ixpr = 1.
   Then, in any case, check for stop conditions.
*/
			init = 1;
			if (meth != mused) {
				tsw = tn;
				maxord = mxordn;
				if (meth == 2)
					maxord = mxords;
				jstart = -1;
				if (ixpr) {
					if (meth == 2)
						fprintf(stderr, "[lsoda] a switch to the stiff method has occurred ");
					if (meth == 1)
						fprintf(stderr, "[lsoda] a switch to the nonstiff method has occurred");
					fprintf(stderr, "at t = %g, tentative step size h = %g, step nst = %d\n", tn, h, nst);
				}
			}	/* end if ( meth != mused )   */
			/*
			   itask = 1.
			   If tout has been reached, interpolate.
			*/
			if (itask == 1) {
				if ((tn - tout) * h < 0.)
					continue;
				intdy(tout, 0, y, &iflag);
				*t = tout;
				*istate = 2;
				illin = 0;
				freevectors();
				return;
			}
/*
   itask = 2.
*/
			if (itask == 2) {
				successreturn(y, t, itask, ihit, tcrit, istate);
				return;
			}
/*
   itask = 3.
   Jump to exit if tout was reached.
*/
			if (itask == 3) {
				if ((tn - tout) * h >= 0.) {
					successreturn(y, t, itask, ihit, tcrit, istate);
					return;
				}
				continue;
			}
/*
   itask = 4.
   See if tout or tcrit was reached.  Adjust h if necessary.
*/
			if (itask == 4) {
				if ((tn - tout) * h >= 0.) {
					intdy(tout, 0, y, &iflag);
					*t = tout;
					*istate = 2;
					illin = 0;
					freevectors();
					return;
				} else {
					hmx = fabs(tn) + fabs(h);
					ihit = fabs(tn - tcrit) <= (100. * ETA * hmx);
					if (ihit) {
						successreturn(y, t, itask, ihit, tcrit, istate);
						return;
					}
					tnext = tn + h * (1. + 4. * ETA);
					if ((tnext - tcrit) * h <= 0.)
						continue;
					h = (tcrit - tn) * (1. - 4. * ETA);
					jstart = -2;
					continue;
				}
			}	/* end if ( itask == 4 )   */
			/*
			   itask = 5.
			   See if tcrit was reached and jump to exit.
			*/
			if (itask == 5) {
				hmx = fabs(tn) + fabs(h);
				ihit = fabs(tn - tcrit) <= (100. * ETA * hmx);
				successreturn(y, t, itask, ihit, tcrit, istate);
				return;
			}
		}		/* end if ( kflag == 0 )   */
		/*
		   kflag = -1, error test failed repeatedly or with fabs(h) = hmin.
		   kflag = -2, convergence failed repeatedly or with fabs(h) = hmin.
		*/
		if (kflag == -1 || kflag == -2) {
			fprintf(stderr, "lsoda -- at t = %g and step size h = %g, the\n", tn, h);
			if (kflag == -1) {
				fprintf(stderr, "         error test failed repeatedly or\n");
				fprintf(stderr, "         with fabs(h) = hmin\n");
				*istate = -4;
			}
			if (kflag == -2) {
				fprintf(stderr, "         corrector convergence failed repeatedly or\n");
				fprintf(stderr, "         with fabs(h) = hmin\n");
				*istate = -5;
			}
			big = 0.;
			imxer = 1;
			for (i = 1; i <= n; i++) {
				size = fabs(acor[i]) * ewt[i];
				if (big < size) {
					big = size;
					imxer = i;
				}
			}
			terminate2(y, t);
			return;
		}		/* end if ( kflag == -1 || kflag == -2 )   */
	}			/* end while   */

}				/* end lsoda   */


static void stoda(int neq, double *y, _lsoda_f f, void *_data)
{
	int             corflag, orderflag;
	int             i, i1, j, m, ncf;
	double          del, delp, dsm, dup, exup, r, rh, rhup, told;
	double          pdh, pnorm;

/*
   stoda performs one step of the integration of an initial value
   problem for a system of ordinary differential equations.
   Note.. stoda is independent of the value of the iteration method
   indicator miter, when this is != 0, and hence is independent
   of the type of chord method used, or the Jacobian structure.
   Communication with stoda is done with the following variables:

   jstart = an integer used for input only, with the following
            values and meanings:

               0  perform the first step,
             > 0  take a new step continuing from the last,
              -1  take the next step with a new value of h,
                  n, meth, miter, and/or matrix parameters.
              -2  take the next step with a new value of h,
                  but with other inputs unchanged.

   kflag = a completion code with the following meanings:

             0  the step was successful,
            -1  the requested error could not be achieved,
            -2  corrector convergence could not be achieved,
            -3  fatal error in prja or solsy.

   miter = corrector iteration method:

             0  functional iteration,
            >0  a chord method corresponding to jacobian type jt.

*/
	kflag = 0;
	told = tn;
	ncf = 0;
	ierpj = 0;
	iersl = 0;
	jcur = 0;
	delp = 0.;

/*
   On the first call, the order is set to 1, and other variables are
   initialized.  rmax is the maximum ratio by which h can be increased
   in a single step.  It is initially 1.e4 to compensate for the small
   initial h, but then is normally equal to 10.  If a filure occurs
   (in corrector convergence or error test), rmax is set at 2 for
   the next increase.
   cfode is called to get the needed coefficients for both methods.
*/
	if (jstart == 0) {
		lmax = maxord + 1;
		nq = 1;
		l = 2;
		ialth = 2;
		rmax = 10000.;
		rc = 0.;
		el0 = 1.;
		crate = 0.7;
		hold = h;
		nslp = 0;
		ipup = miter;
/*
   Initialize switching parameters.  meth = 1 is assumed initially.
*/
		icount = 20;
		irflag = 0;
		pdest = 0.;
		pdlast = 0.;
		ratio = 5.;
		cfode(2);
		for (i = 1; i <= 5; i++)
			cm2[i] = tesco[i][2] * elco[i][i + 1];
		cfode(1);
		for (i = 1; i <= 12; i++)
			cm1[i] = tesco[i][2] * elco[i][i + 1];
		resetcoeff();
	}			/* end if ( jstart == 0 )   */
	/*
	   The following block handles preliminaries needed when jstart = -1.
	   ipup is set to miter to force a matrix update.
	   If an order increase is about to be considered ( ialth = 1 ),
	   ialth is reset to 2 to postpone consideration one more step.
	   If the caller has changed meth, cfode is called to reset
	   the coefficients of the method.
	   If h is to be changed, yh must be rescaled.
	   If h or meth is being changed, ialth is reset to l = nq + 1
	   to prevent further changes in h for that many steps.
	*/
	if (jstart == -1) {
		ipup = miter;
		lmax = maxord + 1;
		if (ialth == 1)
			ialth = 2;
		if (meth != mused) {
			cfode(meth);
			ialth = l;
			resetcoeff();
		}
		if (h != hold) {
			rh = h / hold;
			h = hold;
			scaleh(&rh, &pdh);
		}
	}			/* if ( jstart == -1 )   */
	if (jstart == -2) {
		if (h != hold) {
			rh = h / hold;
			h = hold;
			scaleh(&rh, &pdh);
		}
	}			/* if ( jstart == -2 )   */
	/*
	   Prediction.
	   This section computes the predicted values by effectively
	   multiplying the yh array by the pascal triangle matrix.
	   rc is the ratio of new to old values of the coefficient h * el[1].
	   When rc differs from 1 by more than ccmax, ipup is set to miter
	   to force pjac to be called, if a jacobian is involved.
	   In any case, prja is called at least every msbp steps.
	*/
	while (1) {
		while (1) {
			if (fabs(rc - 1.) > ccmax)
				ipup = miter;
			if (nst >= nslp + msbp)
				ipup = miter;
			tn += h;
			for (j = nq; j >= 1; j--)
				for (i1 = j; i1 <= nq; i1++) {
					yp1 = yh[i1];
					yp2 = yh[i1 + 1];
					for (i = 1; i <= n; i++)
						yp1[i] += yp2[i];
				}
			pnorm = vmnorm(n, yh[1], ewt);

			correction(neq, y, f, &corflag, pnorm, &del, &delp, &told, &ncf, &rh, &m, _data);
			if (corflag == 0)
				break;
			if (corflag == 1) {
				rh = max(rh, hmin / fabs(h));
				scaleh(&rh, &pdh);
				continue;
			}
			if (corflag == 2) {
				kflag = -2;
				hold = h;
				jstart = 1;
				return;
			}
		}		/* end inner while ( corrector loop )   */
/*
   The corrector has converged.  jcur is set to 0
   to signal that the Jacobian involved may need updating later.
   The local error test is done now.
*/
		jcur = 0;
		if (m == 0)
			dsm = del / tesco[nq][2];
		if (m > 0)
			dsm = vmnorm(n, acor, ewt) / tesco[nq][2];
		if (dsm <= 1.) {
/*
   After a successful step, update the yh array.
   Decrease icount by 1, and if it is -1, consider switching methods.
   If a method switch is made, reset various parameters,
   rescale the yh array, and exit.  If there is no switch,
   consider changing h if ialth = 1.  Otherwise decrease ialth by 1.
   If ialth is then 1 and nq < maxord, then acor is saved for
   use in a possible order increase on the next step.
   If a change in h is considered, an increase or decrease in order
   by one is considered also.  A change in h is made only if it is by
   a factor of at least 1.1.  If not, ialth is set to 3 to prevent
   testing for that many steps.
*/
			kflag = 0;
			nst++;
			hu = h;
			nqu = nq;
			mused = meth;
			for (j = 1; j <= l; j++) {
				yp1 = yh[j];
				r = el[j];
				for (i = 1; i <= n; i++)
					yp1[i] += r * acor[i];
			}
			icount--;
			if (icount < 0) {
				methodswitch(dsm, pnorm, &pdh, &rh);
				if (meth != mused) {
					rh = max(rh, hmin / fabs(h));
					scaleh(&rh, &pdh);
					rmax = 10.;
					endstoda();
					break;
				}
			}
/*
   No method switch is being made.  Do the usual step/order selection.
*/
			ialth--;
			if (ialth == 0) {
				rhup = 0.;
				if (l != lmax) {
					yp1 = yh[lmax];
					for (i = 1; i <= n; i++)
						savf[i] = acor[i] - yp1[i];
					dup = vmnorm(n, savf, ewt) / tesco[nq][3];
					exup = 1. / (double) (l + 1);
					rhup = 1. / (1.4 * pow(dup, exup) + 0.0000014);
				}
				orderswitch(&rhup, dsm, &pdh, &rh, &orderflag);
/*
   No change in h or nq.
*/
				if (orderflag == 0) {
					endstoda();
					break;
				}
/*
   h is changed, but not nq.
*/
				if (orderflag == 1) {
					rh = max(rh, hmin / fabs(h));
					scaleh(&rh, &pdh);
					rmax = 10.;
					endstoda();
					break;
				}
/*
   both nq and h are changed.
*/
				if (orderflag == 2) {
					resetcoeff();
					rh = max(rh, hmin / fabs(h));
					scaleh(&rh, &pdh);
					rmax = 10.;
					endstoda();
					break;
				}
			}	/* end if ( ialth == 0 )   */
			if (ialth > 1 || l == lmax) {
				endstoda();
				break;
			}
			yp1 = yh[lmax];
			for (i = 1; i <= n; i++)
				yp1[i] = acor[i];
			endstoda();
			break;
		}
		/* end if ( dsm <= 1. )   */
		/*
		   The error test failed.  kflag keeps track of multiple failures.
		   Restore tn and the yh array to their previous values, and prepare
		   to try the step again.  Compute the optimum step size for this or
		   one lower.  After 2 or more failures, h is forced to decrease
		   by a factor of 0.2 or less.
		 */ 
		else {
			kflag--;
			tn = told;
			for (j = nq; j >= 1; j--)
				for (i1 = j; i1 <= nq; i1++) {
					yp1 = yh[i1];
					yp2 = yh[i1 + 1];
					for (i = 1; i <= n; i++)
						yp1[i] -= yp2[i];
				}
			rmax = 2.;
			if (fabs(h) <= hmin * 1.00001) {
				kflag = -1;
				hold = h;
				jstart = 1;
				break;
			}
			if (kflag > -3) {
				rhup = 0.;
				orderswitch(&rhup, dsm, &pdh, &rh, &orderflag);
				if (orderflag == 1 || orderflag == 0) {
					if (orderflag == 0)
						rh = min(rh, 0.2);
					rh = max(rh, hmin / fabs(h));
					scaleh(&rh, &pdh);
				}
				if (orderflag == 2) {
					resetcoeff();
					rh = max(rh, hmin / fabs(h));
					scaleh(&rh, &pdh);
				}
				continue;
			}
			/* if ( kflag > -3 )   */
			/*
			   Control reaches this section if 3 or more failures have occurred.
			   If 10 failures have occurred, exit with kflag = -1.
			   It is assumed that the derivatives that have accumulated in the
			   yh array have errors of the wrong order.  Hence the first
			   derivative is recomputed, and the order is set to 1.  Then
			   h is reduced by a factor of 10, and the step is retried,
			   until it succeeds or h reaches hmin.
			 */ 
			else {
				if (kflag == -10) {
					kflag = -1;
					hold = h;
					jstart = 1;
					break;
				} else {
					rh = 0.1;
					rh = max(hmin / fabs(h), rh);
					h *= rh;
					yp1 = yh[1];
					for (i = 1; i <= n; i++)
						y[i] = yp1[i];
					(*f) (tn, y + 1, savf + 1, _data);
					nfe++;
					yp1 = yh[2];
					for (i = 1; i <= n; i++)
						yp1[i] = h * savf[i];
					ipup = miter;
					ialth = 5;
					if (nq == 1)
						continue;
					nq = 1;
					l = 2;
					resetcoeff();
					continue;
				}
			}	/* end else -- kflag <= -3 */
		}		/* end error failure handling   */
	}			/* end outer while   */

}				/* end stoda   */

static void ewset(int itol, double *rtol, double *atol, double *ycur)
{
	int             i;

	switch (itol) {
	case 1:
		for (i = 1; i <= n; i++)
			ewt[i] = rtol[1] * fabs(ycur[i]) + atol[1];
		break;
	case 2:
		for (i = 1; i <= n; i++)
			ewt[i] = rtol[1] * fabs(ycur[i]) + atol[i];
		break;
	case 3:
		for (i = 1; i <= n; i++)
			ewt[i] = rtol[i] * fabs(ycur[i]) + atol[1];
		break;
	case 4:
		for (i = 1; i <= n; i++)
			ewt[i] = rtol[i] * fabs(ycur[i]) + atol[i];
		break;
	}

}				/* end ewset   */

static void intdy(double t, int k, double *dky, int *iflag)

/*
   Intdy computes interpolated values of the k-th derivative of the
   dependent variable vector y, and stores it in dky.  This routine
   is called within the package with k = 0 and *t = tout, but may
   also be called by the user for any k up to the current order.
   ( See detailed instructions in the usage documentation. )

   The computed values in dky are gotten by interpolation using the
   Nordsieck history array yh.  This array corresponds uniquely to a
   vector-valued polynomial of degree nqcur or less, and dky is set
   to the k-th derivative of this polynomial at t.
   The formula for dky is

             q
   dky[i] = sum c[k][j] * ( t - tn )^(j-k) * h^(-j) * yh[j+1][i]
            j=k

   where c[k][j] = j*(j-1)*...*(j-k+1), q = nqcur, tn = tcur, h = hcur.
   The quantities nq = nqcur, l = nq+1, n = neq, tn, and h are declared
   static globally.  The above sum is done in reverse order.
   *iflag is returned negative if either k or t is out of bounds.
*/

{
	int             i, ic, j, jj, jp1;
	double          c, r, s, tp;

	*iflag = 0;
	if (k < 0 || k > nq) {
		fprintf(stderr, "[intdy] k = %d illegal\n", k);
		*iflag = -1;
		return;
	}
	tp = tn - hu - 100. * ETA * (tn + hu);
	if ((t - tp) * (t - tn) > 0.) {
		fprintf(stderr, "intdy -- t = %g illegal. t not in interval tcur - hu to tcur\n", t);
		*iflag = -2;
		return;
	}
	s = (t - tn) / h;
	ic = 1;
	for (jj = l - k; jj <= nq; jj++)
		ic *= jj;
	c = (double) ic;
	yp1 = yh[l];
	for (i = 1; i <= n; i++)
		dky[i] = c * yp1[i];
	for (j = nq - 1; j >= k; j--) {
		jp1 = j + 1;
		ic = 1;
		for (jj = jp1 - k; jj <= j; jj++)
			ic *= jj;
		c = (double) ic;
		yp1 = yh[jp1];
		for (i = 1; i <= n; i++)
			dky[i] = c * yp1[i] + s * dky[i];
	}
	if (k == 0)
		return;
	r = pow(h, (double) (-k));
	for (i = 1; i <= n; i++)
		dky[i] *= r;

}				/* end intdy   */

static void cfode(int meth)
{
	int             i, nq, nqm1, nqp1;
	double          agamq, fnq, fnqm1, pc[13], pint, ragq, rqfac, rq1fac, tsign, xpin;
/*
   cfode is called by the integrator routine to set coefficients
   needed there.  The coefficients for the current method, as
   given by the value of meth, are set for all orders and saved.
   The maximum order assumed here is 12 if meth = 1 and 5 if meth = 2.
   ( A smaller value of the maximum order is also allowed. )
   cfode is called once at the beginning of the problem, and
   is not called again unless and until meth is changed.

   The elco array contains the basic method coefficients.
   The coefficients el[i], 1 < i < nq+1, for the method of
   order nq are stored in elco[nq][i].  They are given by a generating
   polynomial, i.e.,

      l(x) = el[1] + el[2]*x + ... + el[nq+1]*x^nq.

   For the implicit Adams method, l(x) is given by

      dl/dx = (x+1)*(x+2)*...*(x+nq-1)/factorial(nq-1),   l(-1) = 0.

   For the bdf methods, l(x) is given by

      l(x) = (x+1)*(x+2)*...*(x+nq)/k,

   where   k = factorial(nq)*(1+1/2+...+1/nq).

   The tesco array contains test constants used for the
   local error test and the selection of step size and/or order.
   At order nq, tesco[nq][k] is used for the selection of step
   size at order nq-1 if k = 1, at order nq if k = 2, and at order
   nq+1 if k = 3.
*/
	if (meth == 1) {
		elco[1][1] = 1.;
		elco[1][2] = 1.;
		tesco[1][1] = 0.;
		tesco[1][2] = 2.;
		tesco[2][1] = 1.;
		tesco[12][3] = 0.;
		pc[1] = 1.;
		rqfac = 1.;
		for (nq = 2; nq <= 12; nq++) {
/*
   The pc array will contain the coefficients of the polynomial

      p(x) = (x+1)*(x+2)*...*(x+nq-1).

   Initially, p(x) = 1.
*/
			rq1fac = rqfac;
			rqfac = rqfac / (double) nq;
			nqm1 = nq - 1;
			fnqm1 = (double) nqm1;
			nqp1 = nq + 1;
/*
   Form coefficients of p(x)*(x+nq-1).
*/
			pc[nq] = 0.;
			for (i = nq; i >= 2; i--)
				pc[i] = pc[i - 1] + fnqm1 * pc[i];
			pc[1] = fnqm1 * pc[1];
/*
   Compute integral, -1 to 0, of p(x) and x*p(x).
*/
			pint = pc[1];
			xpin = pc[1] / 2.;
			tsign = 1.;
			for (i = 2; i <= nq; i++) {
				tsign = -tsign;
				pint += tsign * pc[i] / (double) i;
				xpin += tsign * pc[i] / (double) (i + 1);
			}
/*
   Store coefficients in elco and tesco.
*/
			elco[nq][1] = pint * rq1fac;
			elco[nq][2] = 1.;
			for (i = 2; i <= nq; i++)
				elco[nq][i + 1] = rq1fac * pc[i] / (double) i;
			agamq = rqfac * xpin;
			ragq = 1. / agamq;
			tesco[nq][2] = ragq;
			if (nq < 12)
				tesco[nqp1][1] = ragq * rqfac / (double) nqp1;
			tesco[nqm1][3] = ragq;
		}		/* end for   */
		return;
	}			/* end if ( meth == 1 )   */
	/*
	   meth = 2.
	*/
	pc[1] = 1.;
	rq1fac = 1.;
/*
   The pc array will contain the coefficients of the polynomial

      p(x) = (x+1)*(x+2)*...*(x+nq).

   Initially, p(x) = 1.
*/
	for (nq = 1; nq <= 5; nq++) {
		fnq = (double) nq;
		nqp1 = nq + 1;
/*
   Form coefficients of p(x)*(x+nq).
*/
		pc[nqp1] = 0.;
		for (i = nq + 1; i >= 2; i--)
			pc[i] = pc[i - 1] + fnq * pc[i];
		pc[1] *= fnq;
/*
   Store coefficients in elco and tesco.
*/
		for (i = 1; i <= nqp1; i++)
			elco[nq][i] = pc[i] / pc[2];
		elco[nq][2] = 1.;
		tesco[nq][1] = rq1fac;
		tesco[nq][2] = ((double) nqp1) / elco[nq][1];
		tesco[nq][3] = ((double) (nq + 2)) / elco[nq][1];
		rq1fac /= fnq;
	}
	return;

}				/* end cfode   */

static void scaleh(double *rh, double *pdh)
{
	double          r;
	int             j, i;
/*
   If h is being changed, the h ratio rh is checked against rmax, hmin,
   and hmxi, and the yh array is rescaled.  ialth is set to l = nq + 1
   to prevent a change of h for that many steps, unless forced by a
   convergence or error test failure.
*/
	*rh = min(*rh, rmax);
	*rh = *rh / max(1., fabs(h) * hmxi * *rh);
/*
   If meth = 1, also restrict the new step size by the stability region.
   If this reduces h, set irflag to 1 so that if there are roundoff
   problems later, we can assume that is the cause of the trouble.
*/
	if (meth == 1) {
		irflag = 0;
		*pdh = max(fabs(h) * pdlast, 0.000001);
		if ((*rh * *pdh * 1.00001) >= sm1[nq]) {
			*rh = sm1[nq] / *pdh;
			irflag = 1;
		}
	}
	r = 1.;
	for (j = 2; j <= l; j++) {
		r *= *rh;
		yp1 = yh[j];
		for (i = 1; i <= n; i++)
			yp1[i] *= r;
	}
	h *= *rh;
	rc *= *rh;
	ialth = l;

}				/* end scaleh   */


static void prja(int neq, double *y, _lsoda_f f, void *_data)
{
	int             i, ier, j;
	double          fac, hl0, r, r0, yj;
/*
   prja is called by stoda to compute and process the matrix
   P = I - h * el[1] * J, where J is an approximation to the Jacobian.
   Here J is computed by finite differencing.
   J, scaled by -h * el[1], is stored in wm.  Then the norm of J ( the
   matrix norm consistent with the weighted max-norm on vectors given
   by vmnorm ) is computed, and J is overwritten by P.  P is then
   subjected to LU decomposition in preparation for later solution
   of linear systems with p as coefficient matrix.  This is done
   by dgefa if miter = 2, and by dgbfa if miter = 5.
*/
	nje++;
	ierpj = 0;
	jcur = 1;
	hl0 = h * el0;
/*
   If miter = 2, make n calls to f to approximate J.
*/
	if (miter != 2) {
		fprintf(stderr, "[prja] miter != 2\n");
		return;
	}
	if (miter == 2) {
		fac = vmnorm(n, savf, ewt);
		r0 = 1000. * fabs(h) * ETA * ((double) n) * fac;
		if (r0 == 0.)
			r0 = 1.;
		for (j = 1; j <= n; j++) {
			yj = y[j];
			r = max(sqrteta * fabs(yj), r0 / ewt[j]);
			y[j] += r;
			fac = -hl0 / r;
			(*f) (tn, y + 1, acor + 1, _data);
			for (i = 1; i <= n; i++)
				wm[i][j] = (acor[i] - savf[i]) * fac;
			y[j] = yj;
		}
		nfe += n;
/*
   Compute norm of Jacobian.
*/
		pdnorm = fnorm(n, wm, ewt) / fabs(hl0);
/*
   Add identity matrix.
*/
		for (i = 1; i <= n; i++)
			wm[i][i] += 1.;
/*
   Do LU decomposition on P.
*/
		dgefa(wm, n, ipvt, &ier);
		if (ier != 0)
			ierpj = 1;
		return;
	}
}				/* end prja   */

static double vmnorm(int n, double *v, double *w)

/*
   This function routine computes the weighted max-norm
   of the vector of length n contained in the array v, with weights
   contained in the array w of length n.

   vmnorm = max( i = 1, ..., n ) fabs( v[i] ) * w[i].
*/

{
	int             i;
	double          vm;

	vm = 0.;
	for (i = 1; i <= n; i++)
		vm = max(vm, fabs(v[i]) * w[i]);
	return vm;

}

static double fnorm(int n, double **a, double *w)

/*
   This subroutine computes the norm of a full n by n matrix,
   stored in the array a, that is consistent with the weighted max-norm
   on vectors, with weights stored in the array w.

      fnorm = max(i=1,...,n) ( w[i] * sum(j=1,...,n) fabs( a[i][j] ) / w[j] )
*/

{
	int             i, j;
	double          an, sum, *ap1;

	an = 0.;
	for (i = 1; i <= n; i++) {
		sum = 0.;
		ap1 = a[i];
		for (j = 1; j <= n; j++)
			sum += fabs(ap1[j]) / w[j];
		an = max(an, sum * w[i]);
	}
	return an;

}

static void correction(int neq, double *y, _lsoda_f f, int *corflag, double pnorm, double *del, double *delp, double *told,
					   int *ncf, double *rh, int *m, void *_data)
/*
   *corflag = 0 : corrector converged,
              1 : step size to be reduced, redo prediction,
              2 : corrector cannot converge, failure flag.
*/

{
	int             i;
	double          rm, rate, dcon;

/*
   Up to maxcor corrector iterations are taken.  A convergence test is
   made on the r.m.s. norm of each correction, weighted by the error
   weight vector ewt.  The sum of the corrections is accumulated in the
   vector acor[i].  The yh array is not altered in the corrector loop.
*/

	*m = 0;
	*corflag = 0;
	rate = 0.;
	*del = 0.;
	yp1 = yh[1];
	for (i = 1; i <= n; i++)
		y[i] = yp1[i];
	(*f) (tn, y + 1, savf + 1, _data);
	nfe++;
/*
   If indicated, the matrix P = I - h * el[1] * J is reevaluated and
   preprocessed before starting the corrector iteration.  ipup is set
   to 0 as an indicator that this has been done.
*/
	while (1) {
		if (*m == 0) {
			if (ipup > 0) {
				prja(neq, y, f, _data);
				ipup = 0;
				rc = 1.;
				nslp = nst;
				crate = 0.7;
				if (ierpj != 0) {
					corfailure(told, rh, ncf, corflag);
					return;
				}
			}
			for (i = 1; i <= n; i++)
				acor[i] = 0.;
		}		/* end if ( *m == 0 )   */
		if (miter == 0) {
/*
   In case of functional iteration, update y directly from
   the result of the last function evaluation.
*/
			yp1 = yh[2];
			for (i = 1; i <= n; i++) {
				savf[i] = h * savf[i] - yp1[i];
				y[i] = savf[i] - acor[i];
			}
			*del = vmnorm(n, y, ewt);
			yp1 = yh[1];
			for (i = 1; i <= n; i++) {
				y[i] = yp1[i] + el[1] * savf[i];
				acor[i] = savf[i];
			}
		}
		/* end functional iteration   */
		/*
		   In the case of the chord method, compute the corrector error,
		   and solve the linear system with that as right-hand side and
		   P as coefficient matrix.
		 */ 
		else {
			yp1 = yh[2];
			for (i = 1; i <= n; i++)
				y[i] = h * savf[i] - (yp1[i] + acor[i]);
			solsy(y);
			*del = vmnorm(n, y, ewt);
			yp1 = yh[1];
			for (i = 1; i <= n; i++) {
				acor[i] += y[i];
				y[i] = yp1[i] + el[1] * acor[i];
			}
		}		/* end chord method   */
/*
   Test for convergence.  If *m > 0, an estimate of the convergence
   rate constant is stored in crate, and this is used in the test.

   We first check for a change of iterates that is the size of
   roundoff error.  If this occurs, the iteration has converged, and a
   new rate estimate is not formed.
   In all other cases, force at least two iterations to estimate a
   local Lipschitz constant estimate for Adams method.
   On convergence, form pdest = local maximum Lipschitz constant
   estimate.  pdlast is the most recent nonzero estimate.
*/
		if (*del <= 100. * pnorm * ETA)
			break;
		if (*m != 0 || meth != 1) {
			if (*m != 0) {
				rm = 1024.0;
				if (*del <= (1024. * *delp))
					rm = *del / *delp;
				rate = max(rate, rm);
				crate = max(0.2 * crate, rm);
			}
			dcon = *del * min(1., 1.5 * crate) / (tesco[nq][2] * conit);
			if (dcon <= 1.) {
				pdest = max(pdest, rate / fabs(h * el[1]));
				if (pdest != 0.)
					pdlast = pdest;
				break;
			}
		}
/*
   The corrector iteration failed to converge.
   If miter != 0 and the Jacobian is out of date, prja is called for
   the next try.   Otherwise the yh array is retracted to its values
   before prediction, and h is reduced, if possible.  If h cannot be
   reduced or mxncf failures have occured, exit with corflag = 2.
*/
		(*m)++;
		if (*m == maxcor || (*m >= 2 && *del > 2. * *delp)) {
			if (miter == 0 || jcur == 1) {
				corfailure(told, rh, ncf, corflag);
				return;
			}
			ipup = miter;
/*
   Restart corrector if Jacobian is recomputed.
*/
			*m = 0;
			rate = 0.;
			*del = 0.;
			yp1 = yh[1];
			for (i = 1; i <= n; i++)
				y[i] = yp1[i];
			(*f) (tn, y + 1, savf + 1, _data);
			nfe++;
		}
/*
   Iterate corrector.
*/
		else {
			*delp = *del;
			(*f) (tn, y + 1, savf + 1, _data);
			nfe++;
		}
	}			/* end while   */
}				/* end correction   */

static void corfailure(double *told, double *rh, int *ncf, int *corflag)
{
	int             j, i1, i;

	ncf++;
	rmax = 2.;
	tn = *told;
	for (j = nq; j >= 1; j--)
		for (i1 = j; i1 <= nq; i1++) {
			yp1 = yh[i1];
			yp2 = yh[i1 + 1];
			for (i = 1; i <= n; i++)
				yp1[i] -= yp2[i];
		}
	if (fabs(h) <= hmin * 1.00001 || *ncf == mxncf) {
		*corflag = 2;
		return;
	}
	*corflag = 1;
	*rh = 0.25;
	ipup = miter;

}

static void solsy(double *y)

/*
   This routine manages the solution of the linear system arising from
   a chord iteration.  It is called if miter != 0.
   If miter is 2, it calls dgesl to accomplish this.
   If miter is 5, it calls dgbsl.

   y = the right-hand side vector on input, and the solution vector
       on output.
*/

{
	iersl = 0;
	if (miter != 2) {
		printf("solsy -- miter != 2\n");
		return;
	}
	if (miter == 2)
		dgesl(wm, n, ipvt, y, 0);
	return;

}

static void methodswitch(double dsm, double pnorm, double *pdh, double *rh)
{
	int             lm1, lm1p1, lm2, lm2p1, nqm1, nqm2;
	double          rh1, rh2, rh1it, exm2, dm2, exm1, dm1, alpha, exsm;

/*
   We are current using an Adams method.  Consider switching to bdf.
   If the current order is greater than 5, assume the problem is
   not stiff, and skip this section.
   If the Lipschitz constant and error estimate are not polluted
   by roundoff, perform the usual test.
   Otherwise, switch to the bdf methods if the last step was
   restricted to insure stability ( irflag = 1 ), and stay with Adams
   method if not.  When switching to bdf with polluted error estimates,
   in the absence of other information, double the step size.

   When the estimates are ok, we make the usual test by computing
   the step size we could have (ideally) used on this step,
   with the current (Adams) method, and also that for the bdf.
   If nq > mxords, we consider changing to order mxords on switching.
   Compare the two step sizes to decide whether to switch.
   The step size advantage must be at least ratio = 5 to switch.
*/
	if (meth == 1) {
		if (nq > 5)
			return;
		if (dsm <= (100. * pnorm * ETA) || pdest == 0.) {
			if (irflag == 0)
				return;
			rh2 = 2.;
			nqm2 = min(nq, mxords);
		} else {
			exsm = 1. / (double) l;
			rh1 = 1. / (1.2 * pow(dsm, exsm) + 0.0000012);
			rh1it = 2. * rh1;
			*pdh = pdlast * fabs(h);
			if ((*pdh * rh1) > 0.00001)
				rh1it = sm1[nq] / *pdh;
			rh1 = min(rh1, rh1it);
			if (nq > mxords) {
				nqm2 = mxords;
				lm2 = mxords + 1;
				exm2 = 1. / (double) lm2;
				lm2p1 = lm2 + 1;
				dm2 = vmnorm(n, yh[lm2p1], ewt) / cm2[mxords];
				rh2 = 1. / (1.2 * pow(dm2, exm2) + 0.0000012);
			} else {
				dm2 = dsm * (cm1[nq] / cm2[nq]);
				rh2 = 1. / (1.2 * pow(dm2, exsm) + 0.0000012);
				nqm2 = nq;
			}
			if (rh2 < ratio * rh1)
				return;
		}
/*
   The method switch test passed.  Reset relevant quantities for bdf.
*/
		*rh = rh2;
		icount = 20;
		meth = 2;
		miter = jtyp;
		pdlast = 0.;
		nq = nqm2;
		l = nq + 1;
		return;
	}			/* end if ( meth == 1 )   */
	/*
	   We are currently using a bdf method, considering switching to Adams.
	   Compute the step size we could have (ideally) used on this step,
	   with the current (bdf) method, and also that for the Adams.
	   If nq > mxordn, we consider changing to order mxordn on switching.
	   Compare the two step sizes to decide whether to switch.
	   The step size advantage must be at least 5/ratio = 1 to switch.
	   If the step size for Adams would be so small as to cause
	   roundoff pollution, we stay with bdf.
	*/
	exsm = 1. / (double) l;
	if (mxordn < nq) {
		nqm1 = mxordn;
		lm1 = mxordn + 1;
		exm1 = 1. / (double) lm1;
		lm1p1 = lm1 + 1;
		dm1 = vmnorm(n, yh[lm1p1], ewt) / cm1[mxordn];
		rh1 = 1. / (1.2 * pow(dm1, exm1) + 0.0000012);
	} else {
		dm1 = dsm * (cm2[nq] / cm1[nq]);
		rh1 = 1. / (1.2 * pow(dm1, exsm) + 0.0000012);
		nqm1 = nq;
		exm1 = exsm;
	}
	rh1it = 2. * rh1;
	*pdh = pdnorm * fabs(h);
	if ((*pdh * rh1) > 0.00001)
		rh1it = sm1[nqm1] / *pdh;
	rh1 = min(rh1, rh1it);
	rh2 = 1. / (1.2 * pow(dsm, exsm) + 0.0000012);
	if ((rh1 * ratio) < (5. * rh2))
		return;
	alpha = max(0.001, rh1);
	dm1 *= pow(alpha, exm1);
	if (dm1 <= 1000. * ETA * pnorm)
		return;
/*
   The switch test passed.  Reset relevant quantities for Adams.
*/
	*rh = rh1;
	icount = 20;
	meth = 1;
	miter = 0;
	pdlast = 0.;
	nq = nqm1;
	l = nq + 1;

}				/* end methodswitch   */


/*
   This routine returns from stoda to lsoda.  Hence freevectors() is
   not executed.
*/
static void endstoda()
{
	double          r;
	int             i;

	r = 1. / tesco[nqu][2];
	for (i = 1; i <= n; i++)
		acor[i] *= r;
	hold = h;
	jstart = 1;

}

static void orderswitch(double *rhup, double dsm, double *pdh, double *rh, int *orderflag)

/*
   Regardless of the success or failure of the step, factors
   rhdn, rhsm, and rhup are computed, by which h could be multiplied
   at order nq - 1, order nq, or order nq + 1, respectively.
   In the case of a failure, rhup = 0. to avoid an order increase.
   The largest of these is determined and the new order chosen
   accordingly.  If the order is to be increased, we compute one
   additional scaled derivative.

   orderflag = 0  : no change in h or nq,
               1  : change in h but not nq,
               2  : change in both h and nq.
*/

{
	int             newq, i;
	double          exsm, rhdn, rhsm, ddn, exdn, r;

	*orderflag = 0;

	exsm = 1. / (double) l;
	rhsm = 1. / (1.2 * pow(dsm, exsm) + 0.0000012);

	rhdn = 0.;
	if (nq != 1) {
		ddn = vmnorm(n, yh[l], ewt) / tesco[nq][1];
		exdn = 1. / (double) nq;
		rhdn = 1. / (1.3 * pow(ddn, exdn) + 0.0000013);
	}
/*
   If meth = 1, limit rh accordinfg to the stability region also.
*/
	if (meth == 1) {
		*pdh = max(fabs(h) * pdlast, 0.000001);
		if (l < lmax)
			*rhup = min(*rhup, sm1[l] / *pdh);
		rhsm = min(rhsm, sm1[nq] / *pdh);
		if (nq > 1)
			rhdn = min(rhdn, sm1[nq - 1] / *pdh);
		pdest = 0.;
	}
	if (rhsm >= *rhup) {
		if (rhsm >= rhdn) {
			newq = nq;
			*rh = rhsm;
		} else {
			newq = nq - 1;
			*rh = rhdn;
			if (kflag < 0 && *rh > 1.)
				*rh = 1.;
		}
	} else {
		if (*rhup <= rhdn) {
			newq = nq - 1;
			*rh = rhdn;
			if (kflag < 0 && *rh > 1.)
				*rh = 1.;
		} else {
			*rh = *rhup;
			if (*rh >= 1.1) {
				r = el[l] / (double) l;
				nq = l;
				l = nq + 1;
				yp1 = yh[l];
				for (i = 1; i <= n; i++)
					yp1[i] = acor[i] * r;
				*orderflag = 2;
				return;
			} else {
				ialth = 3;
				return;
			}
		}
	}
/*
   If meth = 1 and h is restricted by stability, bypass 10 percent test.
*/
	if (meth == 1) {
		if ((*rh * *pdh * 1.00001) < sm1[newq])
			if (kflag == 0 && *rh < 1.1) {
				ialth = 3;
				return;
			}
	} else {
		if (kflag == 0 && *rh < 1.1) {
			ialth = 3;
			return;
		}
	}
	if (kflag <= -2)
		*rh = min(*rh, 0.2);
/*
   If there is a change of order, reset nq, l, and the coefficients.
   In any case h is reset according to rh and the yh array is rescaled.
   Then exit or redo the step.
*/
	if (newq == nq) {
		*orderflag = 1;
		return;
	}
	nq = newq;
	l = nq + 1;
	*orderflag = 2;

}				/* end orderswitch   */


static void resetcoeff()
/*
   The el vector and related constants are reset
   whenever the order nq is changed, or at the start of the problem.
*/
{
	int             i;
	double         *ep1;

	ep1 = elco[nq];
	for (i = 1; i <= l; i++)
		el[i] = ep1[i];
	rc = rc * el[1] / el0;
	el0 = el[1];
	conit = 0.5 / (double) (nq + 2);

}

/* this function does nothing. */
static void freevectors(void)
{
}

static void _freevectors(void)
{
	int i;
	if (wm) for (i = 1; i <= g_nyh; ++i) free(wm[i]);
	if (yh) for (i = 1; i <= g_lenyh; ++i) free(yh[i]);
	free(yh); free(wm); free(ewt); free(savf); free(acor); free(ipvt);
	g_nyh = g_lenyh = 0;
	yh = 0; wm = 0;
	ewt = 0; savf = 0; acor = 0; ipvt = 0;
}

/*****************************
 * more convenient interface *
 *****************************/

int n_lsoda(double y[], int n, double *x, double xout, double eps, const double yscal[], _lsoda_f devis, void *data)
{
	int             i, istate, itask;
	double         *_y, *atol, *rtol;
	_y = (double *) calloc(3 * (n + 1), sizeof(double));
	atol = _y + n + 1;
	rtol = atol + n + 1;
	for (i = 1; i <= n; ++i) {
		_y[i] = y[i - 1];
		atol[i] = eps * yscal[i - 1];
	}
	istate = init? 2 : 1;
	itask = 2;
	lsoda(devis, n, _y, x, xout, 2, rtol, atol, itask, &istate, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0., 0., 0., 0., data);
	for (i = 1; i <= n; ++i) y[i - 1] = _y[i];
	free(_y);
	return istate < 0 ? istate : 0;
}

void n_lsoda_terminate(void)
{
	if (init) _freevectors();
	init = 0;
}


/* Interpolation function*/

double interpolator (double x) {
  int     i;
  double   x0, y0, x1, y1;
  int n = 3601;
  double points1[] = { 
    0,
1.82500000003984,
3.64999999999668,
5.47500000003652,
7.29999999999336,
9.1250000000332,
10.94999999999,
12.7750000000299,
14.5999999999867,
16.4250000000266,
18.2499999999834,
20.0750000000232,
21.8999999999801,
23.7250000000199,
25.5499999999768,
27.3750000000166,
29.1999999999734,
31.0250000000133,
32.8499999999701,
34.67500000001,
36.4999999999668,
38.3250000000066,
40.1499999999635,
41.9750000000033,
43.7999999999602,
45.625,
47.4500000000398,
49.2749999999967,
51.1000000000365,
52.9249999999934,
54.7500000000332,
56.57499999999,
58.4000000000299,
60.2249999999867,
62.0500000000266,
63.8749999999834,
65.7000000000232,
67.5249999999801,
69.3500000000199,
71.1749999999768,
73.0000000000166,
74.8249999999734,
76.6500000000133,
78.4749999999701,
80.30000000001,
82.1249999999668,
83.9500000000066,
85.7749999999635,
87.6000000000033,
89.4249999999602,
91.25,
93.0750000000398,
94.8999999999967,
96.7250000000365,
98.5499999999934,
100.375000000033,
102.19999999999,
104.02500000003,
105.849999999987,
107.675000000027,
109.499999999983,
111.325000000023,
113.14999999998,
114.97500000002,
116.799999999977,
118.625000000017,
120.449999999973,
122.275000000013,
124.09999999997,
125.92500000001,
127.749999999967,
129.575000000007,
131.399999999963,
133.225000000003,
135.04999999996,
136.875,
138.70000000004,
140.524999999997,
142.350000000037,
144.174999999993,
146.000000000033,
147.82499999999,
149.65000000003,
151.474999999987,
153.300000000027,
155.124999999983,
156.950000000023,
158.77499999998,
160.60000000002,
162.424999999977,
164.250000000017,
166.074999999973,
167.900000000013,
169.72499999997,
171.55000000001,
173.374999999967,
175.200000000007,
177.024999999963,
178.850000000003,
180.67499999996,
182.5,
184.32500000004,
186.149999999997,
187.975000000037,
189.799999999993,
191.625000000033,
193.44999999999,
195.27500000003,
197.099999999987,
198.925000000027,
200.749999999983,
202.575000000023,
204.39999999998,
206.22500000002,
208.049999999977,
209.875000000017,
211.699999999973,
213.525000000013,
215.34999999997,
217.17500000001,
218.999999999967,
220.825000000007,
222.649999999963,
224.475000000003,
226.29999999996,
228.125,
229.95000000004,
231.774999999997,
233.600000000037,
235.424999999993,
237.250000000033,
239.07499999999,
240.90000000003,
242.724999999987,
244.550000000027,
246.374999999983,
248.200000000023,
250.02499999998,
251.85000000002,
253.674999999977,
255.500000000017,
257.324999999973,
259.150000000013,
260.97499999997,
262.80000000001,
264.624999999967,
266.450000000007,
268.274999999963,
270.100000000003,
271.92499999996,
273.75,
275.57500000004,
277.399999999997,
279.225000000037,
281.049999999993,
282.875000000033,
284.69999999999,
286.52500000003,
288.349999999987,
290.175000000027,
291.999999999983,
293.825000000023,
295.64999999998,
297.47500000002,
299.299999999977,
301.125000000017,
302.949999999973,
304.775000000013,
306.59999999997,
308.42500000001,
310.249999999967,
312.075000000007,
313.899999999963,
315.725000000003,
317.54999999996,
319.375,
321.20000000004,
323.024999999997,
324.850000000037,
326.674999999993,
328.500000000033,
330.32499999999,
332.15000000003,
333.974999999987,
335.800000000027,
337.624999999983,
339.450000000023,
341.27499999998,
343.10000000002,
344.924999999977,
346.750000000017,
348.574999999973,
350.400000000013,
352.22499999997,
354.05000000001,
355.874999999967,
357.700000000007,
359.524999999963,
361.350000000003,
363.17499999996,
365,
366.82500000004,
368.649999999997,
370.475000000037,
372.299999999993,
374.125000000033,
375.94999999999,
377.77500000003,
379.599999999987,
381.425000000027,
383.249999999983,
385.075000000023,
386.89999999998,
388.72500000002,
390.549999999977,
392.375000000017,
394.199999999973,
396.025000000013,
397.84999999997,
399.67500000001,
401.499999999967,
403.325000000007,
405.149999999963,
406.975000000003,
408.79999999996,
410.625,
412.45000000004,
414.274999999997,
416.100000000037,
417.924999999993,
419.750000000033,
421.57499999999,
423.40000000003,
425.224999999987,
427.050000000027,
428.874999999983,
430.700000000023,
432.52499999998,
434.35000000002,
436.174999999977,
438.000000000017,
439.824999999973,
441.650000000013,
443.47499999997,
445.30000000001,
447.124999999967,
448.950000000007,
450.774999999963,
452.600000000003,
454.42499999996,
456.25,
458.07500000004,
459.899999999997,
461.725000000037,
463.549999999993,
465.375000000033,
467.19999999999,
469.02500000003,
470.849999999987,
472.675000000027,
474.499999999983,
476.325000000023,
478.14999999998,
479.97500000002,
481.799999999977,
483.625000000017,
485.449999999973,
487.275000000013,
489.09999999997,
490.92500000001,
492.749999999967,
494.575000000007,
496.399999999963,
498.225000000003,
500.04999999996,
501.875,
503.70000000004,
505.524999999997,
507.350000000037,
509.174999999993,
511.000000000033,
512.82499999999,
514.65000000003,
516.474999999987,
518.300000000027,
520.124999999983,
521.950000000023,
523.77499999998,
525.60000000002,
527.424999999977,
529.250000000017,
531.074999999973,
532.900000000013,
534.72499999997,
536.55000000001,
538.374999999967,
540.200000000007,
542.024999999963,
543.850000000003,
545.67499999996,
547.5,
549.32500000004,
551.149999999997,
552.975000000037,
554.799999999993,
556.625000000033,
558.44999999999,
560.27500000003,
562.099999999987,
563.925000000027,
565.749999999983,
567.575000000023,
569.39999999998,
571.22500000002,
573.049999999977,
574.875000000017,
576.699999999973,
578.525000000013,
580.34999999997,
582.17500000001,
583.999999999967,
585.825000000007,
587.649999999963,
589.475000000003,
591.29999999996,
593.125,
594.95000000004,
596.774999999997,
598.600000000037,
600.424999999993,
602.250000000033,
604.07499999999,
605.90000000003,
607.724999999987,
609.550000000027,
611.374999999983,
613.200000000023,
615.02499999998,
616.85000000002,
618.674999999977,
620.500000000017,
622.324999999973,
624.150000000013,
625.97499999997,
627.80000000001,
629.624999999967,
631.450000000007,
633.274999999963,
635.100000000003,
636.92499999996,
638.75,
640.57500000004,
642.399999999997,
644.225000000037,
646.049999999993,
647.875000000033,
649.69999999999,
651.52500000003,
653.349999999987,
655.175000000027,
656.999999999983,
658.825000000023,
660.64999999998,
662.47500000002,
664.299999999977,
666.125000000017,
667.949999999973,
669.775000000013,
671.59999999997,
673.42500000001,
675.249999999967,
677.075000000007,
678.899999999963,
680.725000000003,
682.54999999996,
684.375,
686.20000000004,
688.024999999997,
689.850000000037,
691.674999999993,
693.500000000033,
695.32499999999,
697.15000000003,
698.974999999987,
700.800000000027,
702.624999999983,
704.450000000023,
706.27499999998,
708.10000000002,
709.924999999977,
711.750000000017,
713.574999999973,
715.400000000013,
717.22499999997,
719.05000000001,
720.874999999967,
722.700000000007,
724.524999999963,
726.350000000003,
728.17499999996,
730,
731.82500000004,
733.649999999997,
735.475000000037,
737.299999999993,
739.125000000033,
740.94999999999,
742.77500000003,
744.599999999987,
746.425000000027,
748.249999999983,
750.075000000023,
751.89999999998,
753.72500000002,
755.549999999977,
757.375000000017,
759.199999999973,
761.025000000013,
762.84999999997,
764.67500000001,
766.499999999967,
768.325000000007,
770.149999999963,
771.975000000003,
773.79999999996,
775.625,
777.45000000004,
779.274999999997,
781.100000000037,
782.924999999993,
784.750000000033,
786.57499999999,
788.40000000003,
790.224999999987,
792.050000000027,
793.874999999983,
795.700000000023,
797.52499999998,
799.35000000002,
801.174999999977,
803.000000000017,
804.824999999973,
806.650000000013,
808.47499999997,
810.30000000001,
812.124999999967,
813.950000000007,
815.774999999963,
817.600000000003,
819.42499999996,
821.25,
823.07500000004,
824.899999999997,
826.725000000037,
828.549999999993,
830.375000000033,
832.19999999999,
834.02500000003,
835.849999999987,
837.675000000027,
839.499999999983,
841.325000000023,
843.14999999998,
844.97500000002,
846.799999999977,
848.625000000017,
850.449999999973,
852.275000000013,
854.09999999997,
855.92500000001,
857.749999999967,
859.575000000007,
861.399999999963,
863.225000000003,
865.04999999996,
866.875,
868.70000000004,
870.524999999997,
872.350000000037,
874.174999999993,
876.000000000033,
877.82499999999,
879.65000000003,
881.474999999987,
883.300000000027,
885.124999999983,
886.950000000023,
888.77499999998,
890.60000000002,
892.424999999977,
894.250000000017,
896.074999999973,
897.900000000013,
899.72499999997,
901.55000000001,
903.374999999967,
905.200000000007,
907.024999999963,
908.850000000003,
910.67499999996,
912.5,
914.32500000004,
916.149999999997,
917.975000000037,
919.799999999993,
921.625000000033,
923.44999999999,
925.27500000003,
927.099999999987,
928.925000000027,
930.749999999983,
932.575000000023,
934.39999999998,
936.22500000002,
938.049999999977,
939.875000000017,
941.699999999973,
943.525000000013,
945.34999999997,
947.17500000001,
948.999999999967,
950.825000000007,
952.649999999963,
954.475000000003,
956.29999999996,
958.125,
959.95000000004,
961.774999999997,
963.600000000037,
965.424999999993,
967.250000000033,
969.07499999999,
970.90000000003,
972.724999999987,
974.550000000027,
976.374999999983,
978.200000000023,
980.02499999998,
981.85000000002,
983.674999999977,
985.500000000017,
987.324999999973,
989.150000000013,
990.97499999997,
992.80000000001,
994.624999999967,
996.450000000007,
998.274999999963,
1000.1,
1001.92499999996,
1003.75,
1005.57500000004,
1007.4,
1009.22500000004,
1011.04999999999,
1012.87500000003,
1014.69999999999,
1016.52500000003,
1018.34999999999,
1020.17500000003,
1021.99999999998,
1023.82500000002,
1025.64999999998,
1027.47500000002,
1029.29999999998,
1031.12500000002,
1032.94999999997,
1034.77500000001,
1036.59999999997,
1038.42500000001,
1040.24999999997,
1042.07500000001,
1043.89999999996,
1045.725,
1047.54999999996,
1049.375,
1051.20000000004,
1053.025,
1054.85000000004,
1056.67499999999,
1058.50000000003,
1060.32499999999,
1062.15000000003,
1063.97499999999,
1065.80000000003,
1067.62499999998,
1069.45000000002,
1071.27499999998,
1073.10000000002,
1074.92499999998,
1076.75000000002,
1078.57499999997,
1080.40000000001,
1082.22499999997,
1084.05000000001,
1085.87499999997,
1087.70000000001,
1089.52499999996,
1091.35,
1093.17499999996,
1095,
1096.82500000004,
1098.65,
1100.47500000004,
1102.29999999999,
1104.12500000003,
1105.94999999999,
1107.77500000003,
1109.59999999999,
1111.42500000003,
1113.24999999998,
1115.07500000002,
1116.89999999998,
1118.72500000002,
1120.54999999998,
1122.37500000002,
1124.19999999997,
1126.02500000001,
1127.84999999997,
1129.67500000001,
1131.49999999997,
1133.32500000001,
1135.14999999996,
1136.975,
1138.79999999996,
1140.625,
1142.45000000004,
1144.275,
1146.10000000004,
1147.92499999999,
1149.75000000003,
1151.57499999999,
1153.40000000003,
1155.22499999999,
1157.05000000003,
1158.87499999998,
1160.70000000002,
1162.52499999998,
1164.35000000002,
1166.17499999998,
1168.00000000002,
1169.82499999997,
1171.65000000001,
1173.47499999997,
1175.30000000001,
1177.12499999997,
1178.95000000001,
1180.77499999996,
1182.6,
1184.42499999996,
1186.25,
1188.07500000004,
1189.9,
1191.72500000004,
1193.54999999999,
1195.37500000003,
1197.19999999999,
1199.02500000003,
1200.84999999999,
1202.67500000003,
1204.49999999998,
1206.32500000002,
1208.14999999998,
1209.97500000002,
1211.79999999998,
1213.62500000002,
1215.44999999997,
1217.27500000001,
1219.09999999997,
1220.92500000001,
1222.74999999997,
1224.57500000001,
1226.39999999996,
1228.225,
1230.04999999996,
1231.875,
1233.70000000004,
1235.525,
1237.35000000004,
1239.17499999999,
1241.00000000003,
1242.82499999999,
1244.65000000003,
1246.47499999999,
1248.30000000003,
1250.12499999998,
1251.95000000002,
1253.77499999998,
1255.60000000002,
1257.42499999998,
1259.25000000002,
1261.07499999997,
1262.90000000001,
1264.72499999997,
1266.55000000001,
1268.37499999997,
1270.20000000001,
1272.02499999996,
1273.85,
1275.67499999996,
1277.5,
1279.32500000004,
1281.15,
1282.97500000004,
1284.79999999999,
1286.62500000003,
1288.44999999999,
1290.27500000003,
1292.09999999999,
1293.92500000003,
1295.74999999998,
1297.57500000002,
1299.39999999998,
1301.22500000002,
1303.04999999998,
1304.87500000002,
1306.69999999997,
1308.52500000001,
1310.34999999997,
1312.17500000001,
1313.99999999997,
1315.82500000001,
1317.64999999996,
1319.475,
1321.29999999996,
1323.125,
1324.95000000004,
1326.775,
1328.60000000004,
1330.42499999999,
1332.25000000003,
1334.07499999999,
1335.90000000003,
1337.72499999999,
1339.55000000003,
1341.37499999998,
1343.20000000002,
1345.02499999998,
1346.85000000002,
1348.67499999998,
1350.50000000002,
1352.32499999997,
1354.15000000001,
1355.97499999997,
1357.80000000001,
1359.62499999997,
1361.45000000001,
1363.27499999996,
1365.1,
1366.92499999996,
1368.75,
1370.57500000004,
1372.4,
1374.22500000004,
1376.04999999999,
1377.87500000003,
1379.69999999999,
1381.52500000003,
1383.34999999999,
1385.17500000003,
1386.99999999998,
1388.82500000002,
1390.64999999998,
1392.47500000002,
1394.29999999998,
1396.12500000002,
1397.94999999997,
1399.77500000001,
1401.59999999997,
1403.42500000001,
1405.24999999997,
1407.07500000001,
1408.89999999996,
1410.725,
1412.54999999996,
1414.375,
1416.20000000004,
1418.025,
1419.85000000004,
1421.67499999999,
1423.50000000003,
1425.32499999999,
1427.15000000003,
1428.97499999999,
1430.80000000003,
1432.62499999998,
1434.45000000002,
1436.27499999998,
1438.10000000002,
1439.92499999998,
1441.75000000002,
1443.57499999997,
1445.40000000001,
1447.22499999997,
1449.05000000001,
1450.87499999997,
1452.70000000001,
1454.52499999996,
1456.35,
1458.17499999996,
1460,
1461.82500000004,
1463.65,
1465.47500000004,
1467.29999999999,
1469.12500000003,
1470.94999999999,
1472.77500000003,
1474.59999999999,
1476.42500000003,
1478.24999999998,
1480.07500000002,
1481.89999999998,
1483.72500000002,
1485.54999999998,
1487.37500000002,
1489.19999999997,
1491.02500000001,
1492.84999999997,
1494.67500000001,
1496.49999999997,
1498.32500000001,
1500.14999999996,
1501.975,
1503.79999999996,
1505.625,
1507.45000000004,
1509.275,
1511.10000000004,
1512.92499999999,
1514.75000000003,
1516.57499999999,
1518.40000000003,
1520.22499999999,
1522.05000000003,
1523.87499999998,
1525.70000000002,
1527.52499999998,
1529.35000000002,
1531.17499999998,
1533.00000000002,
1534.82499999997,
1536.65000000001,
1538.47499999997,
1540.30000000001,
1542.12499999997,
1543.95000000001,
1545.77499999996,
1547.6,
1549.42499999996,
1551.25,
1553.07500000004,
1554.9,
1556.72500000004,
1558.54999999999,
1560.37500000003,
1562.19999999999,
1564.02500000003,
1565.84999999999,
1567.67500000003,
1569.49999999998,
1571.32500000002,
1573.14999999998,
1574.97500000002,
1576.79999999998,
1578.62500000002,
1580.44999999997,
1582.27500000001,
1584.09999999997,
1585.92500000001,
1587.74999999997,
1589.57500000001,
1591.39999999996,
1593.225,
1595.04999999996,
1596.875,
1598.70000000004,
1600.525,
1602.35000000004,
1604.17499999999,
1606.00000000003,
1607.82499999999,
1609.65000000003,
1611.47499999999,
1613.30000000003,
1615.12499999998,
1616.95000000002,
1618.77499999998,
1620.60000000002,
1622.42499999998,
1624.25000000002,
1626.07499999997,
1627.90000000001,
1629.72499999997,
1631.55000000001,
1633.37499999997,
1635.20000000001,
1637.02499999996,
1638.85,
1640.67499999996,
1642.5,
1644.32500000004,
1646.15,
1647.97500000004,
1649.79999999999,
1651.62500000003,
1653.44999999999,
1655.27500000003,
1657.09999999999,
1658.92500000003,
1660.74999999998,
1662.57500000002,
1664.39999999998,
1666.22500000002,
1668.04999999998,
1669.87500000002,
1671.69999999997,
1673.52500000001,
1675.34999999997,
1677.17500000001,
1678.99999999997,
1680.82500000001,
1682.64999999996,
1684.475,
1686.29999999996,
1688.125,
1689.95000000004,
1691.775,
1693.60000000004,
1695.42499999999,
1697.25000000003,
1699.07499999999,
1700.90000000003,
1702.72499999999,
1704.55000000003,
1706.37499999998,
1708.20000000002,
1710.02499999998,
1711.85000000002,
1713.67499999998,
1715.50000000002,
1717.32499999997,
1719.15000000001,
1720.97499999997,
1722.80000000001,
1724.62499999997,
1726.45000000001,
1728.27499999996,
1730.1,
1731.92499999996,
1733.75,
1735.57500000004,
1737.4,
1739.22500000004,
1741.04999999999,
1742.87500000003,
1744.69999999999,
1746.52500000003,
1748.34999999999,
1750.17500000003,
1751.99999999998,
1753.82500000002,
1755.64999999998,
1757.47500000002,
1759.29999999998,
1761.12500000002,
1762.94999999997,
1764.77500000001,
1766.59999999997,
1768.42500000001,
1770.24999999997,
1772.07500000001,
1773.89999999996,
1775.725,
1777.54999999996,
1779.375,
1781.20000000004,
1783.025,
1784.85000000004,
1786.67499999999,
1788.50000000003,
1790.32499999999,
1792.15000000003,
1793.97499999999,
1795.80000000003,
1797.62499999998,
1799.45000000002,
1801.27499999998,
1803.10000000002,
1804.92499999998,
1806.75000000002,
1808.57499999997,
1810.40000000001,
1812.22499999997,
1814.05000000001,
1815.87499999997,
1817.70000000001,
1819.52499999996,
1821.35,
1823.17499999996,
1825,
1826.82500000004,
1828.65,
1830.47500000004,
1832.29999999999,
1834.12500000003,
1835.94999999999,
1837.77500000003,
1839.59999999999,
1841.42500000003,
1843.24999999998,
1845.07500000002,
1846.89999999998,
1848.72500000002,
1850.54999999998,
1852.37500000002,
1854.19999999997,
1856.02500000001,
1857.84999999997,
1859.67500000001,
1861.49999999997,
1863.32500000001,
1865.14999999996,
1866.975,
1868.79999999996,
1870.625,
1872.45000000004,
1874.275,
1876.10000000004,
1877.92499999999,
1879.75000000003,
1881.57499999999,
1883.40000000003,
1885.22499999999,
1887.05000000003,
1888.87499999998,
1890.70000000002,
1892.52499999998,
1894.35000000002,
1896.17499999998,
1898.00000000002,
1899.82499999997,
1901.65000000001,
1903.47499999997,
1905.30000000001,
1907.12499999997,
1908.95000000001,
1910.77499999996,
1912.6,
1914.42499999996,
1916.25,
1918.07500000004,
1919.9,
1921.72500000004,
1923.54999999999,
1925.37500000003,
1927.19999999999,
1929.02500000003,
1930.84999999999,
1932.67500000003,
1934.49999999998,
1936.32500000002,
1938.14999999998,
1939.97500000002,
1941.79999999998,
1943.62500000002,
1945.44999999997,
1947.27500000001,
1949.09999999997,
1950.92500000001,
1952.74999999997,
1954.57500000001,
1956.39999999996,
1958.225,
1960.04999999996,
1961.875,
1963.70000000004,
1965.525,
1967.35000000004,
1969.17499999999,
1971.00000000003,
1972.82499999999,
1974.65000000003,
1976.47499999999,
1978.30000000003,
1980.12499999998,
1981.95000000002,
1983.77499999998,
1985.60000000002,
1987.42499999998,
1989.25000000002,
1991.07499999997,
1992.90000000001,
1994.72499999997,
1996.55000000001,
1998.37499999997,
2000.20000000001,
2002.02499999996,
2003.85,
2005.67499999996,
2007.5,
2009.32500000004,
2011.15,
2012.97500000004,
2014.79999999999,
2016.62500000003,
2018.44999999999,
2020.27500000003,
2022.09999999999,
2023.92500000003,
2025.74999999998,
2027.57500000002,
2029.39999999998,
2031.22500000002,
2033.04999999998,
2034.87500000002,
2036.69999999997,
2038.52500000001,
2040.34999999997,
2042.17500000001,
2043.99999999997,
2045.82500000001,
2047.64999999996,
2049.475,
2051.29999999996,
2053.125,
2054.95000000004,
2056.775,
2058.60000000004,
2060.42499999999,
2062.25000000003,
2064.07499999999,
2065.90000000003,
2067.72499999999,
2069.55000000003,
2071.37499999998,
2073.20000000002,
2075.02499999998,
2076.85000000002,
2078.67499999998,
2080.50000000002,
2082.32499999997,
2084.15000000001,
2085.97499999997,
2087.80000000001,
2089.62499999997,
2091.45000000001,
2093.27499999996,
2095.1,
2096.92499999996,
2098.75,
2100.57500000004,
2102.4,
2104.22500000004,
2106.04999999999,
2107.87500000003,
2109.69999999999,
2111.52500000003,
2113.34999999999,
2115.17500000003,
2116.99999999998,
2118.82500000002,
2120.64999999998,
2122.47500000002,
2124.29999999998,
2126.12500000002,
2127.94999999997,
2129.77500000001,
2131.59999999997,
2133.42500000001,
2135.24999999997,
2137.07500000001,
2138.89999999996,
2140.725,
2142.54999999996,
2144.375,
2146.20000000004,
2148.025,
2149.85000000004,
2151.67499999999,
2153.50000000003,
2155.32499999999,
2157.15000000003,
2158.97499999999,
2160.80000000003,
2162.62499999998,
2164.45000000002,
2166.27499999998,
2168.10000000002,
2169.92499999998,
2171.75000000002,
2173.57499999997,
2175.40000000001,
2177.22499999997,
2179.05000000001,
2180.87499999997,
2182.70000000001,
2184.52499999996,
2186.35,
2188.17499999996,
2190,
2191.82500000004,
2193.65,
2195.47500000004,
2197.29999999999,
2199.12500000003,
2200.94999999999,
2202.77500000003,
2204.59999999999,
2206.42500000003,
2208.24999999998,
2210.07500000002,
2211.89999999998,
2213.72500000002,
2215.54999999998,
2217.37500000002,
2219.19999999997,
2221.02500000001,
2222.84999999997,
2224.67500000001,
2226.49999999997,
2228.32500000001,
2230.14999999996,
2231.975,
2233.79999999996,
2235.625,
2237.45000000004,
2239.275,
2241.10000000004,
2242.92499999999,
2244.75000000003,
2246.57499999999,
2248.40000000003,
2250.22499999999,
2252.05000000003,
2253.87499999998,
2255.70000000002,
2257.52499999998,
2259.35000000002,
2261.17499999998,
2263.00000000002,
2264.82499999997,
2266.65000000001,
2268.47499999997,
2270.30000000001,
2272.12499999997,
2273.95000000001,
2275.77499999996,
2277.6,
2279.42499999996,
2281.25,
2283.07500000004,
2284.9,
2286.72500000004,
2288.54999999999,
2290.37500000003,
2292.19999999999,
2294.02500000003,
2295.84999999999,
2297.67500000003,
2299.49999999998,
2301.32500000002,
2303.14999999998,
2304.97500000002,
2306.79999999998,
2308.62500000002,
2310.44999999997,
2312.27500000001,
2314.09999999997,
2315.92500000001,
2317.74999999997,
2319.57500000001,
2321.39999999996,
2323.225,
2325.04999999996,
2326.875,
2328.70000000004,
2330.525,
2332.35000000004,
2334.17499999999,
2336.00000000003,
2337.82499999999,
2339.65000000003,
2341.47499999999,
2343.30000000003,
2345.12499999998,
2346.95000000002,
2348.77499999998,
2350.60000000002,
2352.42499999998,
2354.25000000002,
2356.07499999997,
2357.90000000001,
2359.72499999997,
2361.55000000001,
2363.37499999997,
2365.20000000001,
2367.02499999996,
2368.85,
2370.67499999996,
2372.5,
2374.32500000004,
2376.15,
2377.97500000004,
2379.79999999999,
2381.62500000003,
2383.44999999999,
2385.27500000003,
2387.09999999999,
2388.92500000003,
2390.74999999998,
2392.57500000002,
2394.39999999998,
2396.22500000002,
2398.04999999998,
2399.87500000002,
2401.69999999997,
2403.52500000001,
2405.34999999997,
2407.17500000001,
2408.99999999997,
2410.82500000001,
2412.64999999996,
2414.475,
2416.29999999996,
2418.125,
2419.95000000004,
2421.775,
2423.60000000004,
2425.42499999999,
2427.25000000003,
2429.07499999999,
2430.90000000003,
2432.72499999999,
2434.55000000003,
2436.37499999998,
2438.20000000002,
2440.02499999998,
2441.85000000002,
2443.67499999998,
2445.50000000002,
2447.32499999997,
2449.15000000001,
2450.97499999997,
2452.80000000001,
2454.62499999997,
2456.45000000001,
2458.27499999996,
2460.1,
2461.92499999996,
2463.75,
2465.57500000004,
2467.4,
2469.22500000004,
2471.04999999999,
2472.87500000003,
2474.69999999999,
2476.52500000003,
2478.34999999999,
2480.17500000003,
2481.99999999998,
2483.82500000002,
2485.64999999998,
2487.47500000002,
2489.29999999998,
2491.12500000002,
2492.94999999997,
2494.77500000001,
2496.59999999997,
2498.42500000001,
2500.24999999997,
2502.07500000001,
2503.89999999996,
2505.725,
2507.54999999996,
2509.375,
2511.20000000004,
2513.025,
2514.85000000004,
2516.67499999999,
2518.50000000003,
2520.32499999999,
2522.15000000003,
2523.97499999999,
2525.80000000003,
2527.62499999998,
2529.45000000002,
2531.27499999998,
2533.10000000002,
2534.92499999998,
2536.75000000002,
2538.57499999997,
2540.40000000001,
2542.22499999997,
2544.05000000001,
2545.87499999997,
2547.70000000001,
2549.52499999996,
2551.35,
2553.17499999996,
2555,
2556.82500000004,
2558.65,
2560.47500000004,
2562.29999999999,
2564.12500000003,
2565.94999999999,
2567.77500000003,
2569.59999999999,
2571.42500000003,
2573.24999999998,
2575.07500000002,
2576.89999999998,
2578.72500000002,
2580.54999999998,
2582.37500000002,
2584.19999999997,
2586.02500000001,
2587.84999999997,
2589.67500000001,
2591.49999999997,
2593.32500000001,
2595.14999999996,
2596.975,
2598.79999999996,
2600.625,
2602.45000000004,
2604.275,
2606.10000000004,
2607.92499999999,
2609.75000000003,
2611.57499999999,
2613.40000000003,
2615.22499999999,
2617.05000000003,
2618.87499999998,
2620.70000000002,
2622.52499999998,
2624.35000000002,
2626.17499999998,
2628.00000000002,
2629.82499999997,
2631.65000000001,
2633.47499999997,
2635.30000000001,
2637.12499999997,
2638.95000000001,
2640.77499999996,
2642.6,
2644.42499999996,
2646.25,
2648.07500000004,
2649.9,
2651.72500000004,
2653.54999999999,
2655.37500000003,
2657.19999999999,
2659.02500000003,
2660.84999999999,
2662.67500000003,
2664.49999999998,
2666.32500000002,
2668.14999999998,
2669.97500000002,
2671.79999999998,
2673.62500000002,
2675.44999999997,
2677.27500000001,
2679.09999999997,
2680.92500000001,
2682.74999999997,
2684.57500000001,
2686.39999999996,
2688.225,
2690.04999999996,
2691.875,
2693.70000000004,
2695.525,
2697.35000000004,
2699.17499999999,
2701.00000000003,
2702.82499999999,
2704.65000000003,
2706.47499999999,
2708.30000000003,
2710.12499999998,
2711.95000000002,
2713.77499999998,
2715.60000000002,
2717.42499999998,
2719.25000000002,
2721.07499999997,
2722.90000000001,
2724.72499999997,
2726.55000000001,
2728.37499999997,
2730.20000000001,
2732.02499999996,
2733.85,
2735.67499999996,
2737.5,
2739.32500000004,
2741.15,
2742.97500000004,
2744.79999999999,
2746.62500000003,
2748.44999999999,
2750.27500000003,
2752.09999999999,
2753.92500000003,
2755.74999999998,
2757.57500000002,
2759.39999999998,
2761.22500000002,
2763.04999999998,
2764.87500000002,
2766.69999999997,
2768.52500000001,
2770.34999999997,
2772.17500000001,
2773.99999999997,
2775.82500000001,
2777.64999999996,
2779.475,
2781.29999999996,
2783.125,
2784.95000000004,
2786.775,
2788.60000000004,
2790.42499999999,
2792.25000000003,
2794.07499999999,
2795.90000000003,
2797.72499999999,
2799.55000000003,
2801.37499999998,
2803.20000000002,
2805.02499999998,
2806.85000000002,
2808.67499999998,
2810.50000000002,
2812.32499999997,
2814.15000000001,
2815.97499999997,
2817.80000000001,
2819.62499999997,
2821.45000000001,
2823.27499999996,
2825.1,
2826.92499999996,
2828.75,
2830.57500000004,
2832.4,
2834.22500000004,
2836.04999999999,
2837.87500000003,
2839.69999999999,
2841.52500000003,
2843.34999999999,
2845.17500000003,
2846.99999999998,
2848.82500000002,
2850.64999999998,
2852.47500000002,
2854.29999999998,
2856.12500000002,
2857.94999999997,
2859.77500000001,
2861.59999999997,
2863.42500000001,
2865.24999999997,
2867.07500000001,
2868.89999999996,
2870.725,
2872.54999999996,
2874.375,
2876.20000000004,
2878.025,
2879.85000000004,
2881.67499999999,
2883.50000000003,
2885.32499999999,
2887.15000000003,
2888.97499999999,
2890.80000000003,
2892.62499999998,
2894.45000000002,
2896.27499999998,
2898.10000000002,
2899.92499999998,
2901.75000000002,
2903.57499999997,
2905.40000000001,
2907.22499999997,
2909.05000000001,
2910.87499999997,
2912.70000000001,
2914.52499999996,
2916.35,
2918.17499999996,
2920,
2921.82500000004,
2923.65,
2925.47500000004,
2927.29999999999,
2929.12500000003,
2930.94999999999,
2932.77500000003,
2934.59999999999,
2936.42500000003,
2938.24999999998,
2940.07500000002,
2941.89999999998,
2943.72500000002,
2945.54999999998,
2947.37500000002,
2949.19999999997,
2951.02500000001,
2952.84999999997,
2954.67500000001,
2956.49999999997,
2958.32500000001,
2960.14999999996,
2961.975,
2963.79999999996,
2965.625,
2967.45000000004,
2969.275,
2971.10000000004,
2972.92499999999,
2974.75000000003,
2976.57499999999,
2978.40000000003,
2980.22499999999,
2982.05000000003,
2983.87499999998,
2985.70000000002,
2987.52499999998,
2989.35000000002,
2991.17499999998,
2993.00000000002,
2994.82499999997,
2996.65000000001,
2998.47499999997,
3000.30000000001,
3002.12499999997,
3003.95000000001,
3005.77499999996,
3007.6,
3009.42499999996,
3011.25,
3013.07500000004,
3014.9,
3016.72500000004,
3018.54999999999,
3020.37500000003,
3022.19999999999,
3024.02500000003,
3025.84999999999,
3027.67500000003,
3029.49999999998,
3031.32500000002,
3033.14999999998,
3034.97500000002,
3036.79999999998,
3038.62500000002,
3040.44999999997,
3042.27500000001,
3044.09999999997,
3045.92500000001,
3047.74999999997,
3049.57500000001,
3051.39999999996,
3053.225,
3055.04999999996,
3056.875,
3058.70000000004,
3060.525,
3062.35000000004,
3064.17499999999,
3066.00000000003,
3067.82499999999,
3069.65000000003,
3071.47499999999,
3073.30000000003,
3075.12499999998,
3076.95000000002,
3078.77499999998,
3080.60000000002,
3082.42499999998,
3084.25000000002,
3086.07499999997,
3087.90000000001,
3089.72499999997,
3091.55000000001,
3093.37499999997,
3095.20000000001,
3097.02499999996,
3098.85,
3100.67499999996,
3102.5,
3104.32500000004,
3106.15,
3107.97500000004,
3109.79999999999,
3111.62500000003,
3113.44999999999,
3115.27500000003,
3117.09999999999,
3118.92500000003,
3120.74999999998,
3122.57500000002,
3124.39999999998,
3126.22500000002,
3128.04999999998,
3129.87500000002,
3131.69999999997,
3133.52500000001,
3135.34999999997,
3137.17500000001,
3138.99999999997,
3140.82500000001,
3142.64999999996,
3144.475,
3146.29999999996,
3148.125,
3149.95000000004,
3151.775,
3153.60000000004,
3155.42499999999,
3157.25000000003,
3159.07499999999,
3160.90000000003,
3162.72499999999,
3164.55000000003,
3166.37499999998,
3168.20000000002,
3170.02499999998,
3171.85000000002,
3173.67499999998,
3175.50000000002,
3177.32499999997,
3179.15000000001,
3180.97499999997,
3182.80000000001,
3184.62499999997,
3186.45000000001,
3188.27499999996,
3190.1,
3191.92499999996,
3193.75,
3195.57500000004,
3197.4,
3199.22500000004,
3201.04999999999,
3202.87500000003,
3204.69999999999,
3206.52500000003,
3208.34999999999,
3210.17500000003,
3211.99999999998,
3213.82500000002,
3215.64999999998,
3217.47500000002,
3219.29999999998,
3221.12500000002,
3222.94999999997,
3224.77500000001,
3226.59999999997,
3228.42500000001,
3230.24999999997,
3232.07500000001,
3233.89999999996,
3235.725,
3237.54999999996,
3239.375,
3241.20000000004,
3243.025,
3244.85000000004,
3246.67499999999,
3248.50000000003,
3250.32499999999,
3252.15000000003,
3253.97499999999,
3255.80000000003,
3257.62499999998,
3259.45000000002,
3261.27499999998,
3263.10000000002,
3264.92499999998,
3266.75000000002,
3268.57499999997,
3270.40000000001,
3272.22499999997,
3274.05000000001,
3275.87499999997,
3277.70000000001,
3279.52499999996,
3281.35,
3283.17499999996,
3285,
3286.82500000004,
3288.65,
3290.47500000004,
3292.29999999999,
3294.12500000003,
3295.94999999999,
3297.77500000003,
3299.59999999999,
3301.42500000003,
3303.24999999998,
3305.07500000002,
3306.89999999998,
3308.72500000002,
3310.54999999998,
3312.37500000002,
3314.19999999997,
3316.02500000001,
3317.84999999997,
3319.67500000001,
3321.49999999997,
3323.32500000001,
3325.14999999996,
3326.975,
3328.79999999996,
3330.625,
3332.45000000004,
3334.275,
3336.10000000004,
3337.92499999999,
3339.75000000003,
3341.57499999999,
3343.40000000003,
3345.22499999999,
3347.05000000003,
3348.87499999998,
3350.70000000002,
3352.52499999998,
3354.35000000002,
3356.17499999998,
3358.00000000002,
3359.82499999997,
3361.65000000001,
3363.47499999997,
3365.30000000001,
3367.12499999997,
3368.95000000001,
3370.77499999996,
3372.6,
3374.42499999996,
3376.25,
3378.07500000004,
3379.9,
3381.72500000004,
3383.54999999999,
3385.37500000003,
3387.19999999999,
3389.02500000003,
3390.84999999999,
3392.67500000003,
3394.49999999998,
3396.32500000002,
3398.14999999998,
3399.97500000002,
3401.79999999998,
3403.62500000002,
3405.44999999997,
3407.27500000001,
3409.09999999997,
3410.92500000001,
3412.74999999997,
3414.57500000001,
3416.39999999996,
3418.225,
3420.04999999996,
3421.875,
3423.70000000004,
3425.525,
3427.35000000004,
3429.17499999999,
3431.00000000003,
3432.82499999999,
3434.65000000003,
3436.47499999999,
3438.30000000003,
3440.12499999998,
3441.95000000002,
3443.77499999998,
3445.60000000002,
3447.42499999998,
3449.25000000002,
3451.07499999997,
3452.90000000001,
3454.72499999997,
3456.55000000001,
3458.37499999997,
3460.20000000001,
3462.02499999996,
3463.85,
3465.67499999996,
3467.5,
3469.32500000004,
3471.15,
3472.97500000004,
3474.79999999999,
3476.62500000003,
3478.44999999999,
3480.27500000003,
3482.09999999999,
3483.92500000003,
3485.74999999998,
3487.57500000002,
3489.39999999998,
3491.22500000002,
3493.04999999998,
3494.87500000002,
3496.69999999997,
3498.52500000001,
3500.34999999997,
3502.17500000001,
3503.99999999997,
3505.82500000001,
3507.64999999996,
3509.475,
3511.29999999996,
3513.125,
3514.95000000004,
3516.775,
3518.60000000004,
3520.42499999999,
3522.25000000003,
3524.07499999999,
3525.90000000003,
3527.72499999999,
3529.55000000003,
3531.37499999998,
3533.20000000002,
3535.02499999998,
3536.85000000002,
3538.67499999998,
3540.50000000002,
3542.32499999997,
3544.15000000001,
3545.97499999997,
3547.80000000001,
3549.62499999997,
3551.45000000001,
3553.27499999996,
3555.1,
3556.92499999996,
3558.75,
3560.57500000004,
3562.4,
3564.22500000004,
3566.04999999999,
3567.87500000003,
3569.69999999999,
3571.52500000003,
3573.34999999999,
3575.17500000003,
3576.99999999998,
3578.82500000002,
3580.64999999998,
3582.47500000002,
3584.29999999998,
3586.12500000002,
3587.94999999997,
3589.77500000001,
3591.59999999997,
3593.42500000001,
3595.24999999997,
3597.07500000001,
3598.89999999996,
3600.725,
3602.54999999996,
3604.375,
3606.20000000004,
3608.025,
3609.85000000004,
3611.67499999999,
3613.50000000003,
3615.32499999999,
3617.15000000003,
3618.97499999999,
3620.80000000003,
3622.62499999998,
3624.45000000002,
3626.27499999998,
3628.10000000002,
3629.92499999998,
3631.75000000002,
3633.57499999997,
3635.40000000001,
3637.22499999997,
3639.05000000001,
3640.87499999997,
3642.70000000001,
3644.52499999996,
3646.35,
3648.17499999996,
3650,
3651.82500000004,
3653.65,
3655.47500000004,
3657.29999999999,
3659.12500000003,
3660.94999999999,
3662.77500000003,
3664.59999999999,
3666.42500000003,
3668.24999999998,
3670.07500000002,
3671.89999999998,
3673.72500000002,
3675.54999999998,
3677.37500000002,
3679.19999999997,
3681.02500000001,
3682.84999999997,
3684.67500000001,
3686.49999999997,
3688.32500000001,
3690.14999999996,
3691.975,
3693.79999999996,
3695.625,
3697.45000000004,
3699.275,
3701.10000000004,
3702.92499999999,
3704.75000000003,
3706.57499999999,
3708.40000000003,
3710.22499999999,
3712.05000000003,
3713.87499999998,
3715.70000000002,
3717.52499999998,
3719.35000000002,
3721.17499999998,
3723.00000000002,
3724.82499999997,
3726.65000000001,
3728.47499999997,
3730.30000000001,
3732.12499999997,
3733.95000000001,
3735.77499999996,
3737.6,
3739.42499999996,
3741.25,
3743.07500000004,
3744.9,
3746.72500000004,
3748.54999999999,
3750.37500000003,
3752.19999999999,
3754.02500000003,
3755.84999999999,
3757.67500000003,
3759.49999999998,
3761.32500000002,
3763.14999999998,
3764.97500000002,
3766.79999999998,
3768.62500000002,
3770.44999999997,
3772.27500000001,
3774.09999999997,
3775.92500000001,
3777.74999999997,
3779.57500000001,
3781.39999999996,
3783.225,
3785.04999999996,
3786.875,
3788.70000000004,
3790.525,
3792.35000000004,
3794.17499999999,
3796.00000000003,
3797.82499999999,
3799.65000000003,
3801.47499999999,
3803.30000000003,
3805.12499999998,
3806.95000000002,
3808.77499999998,
3810.60000000002,
3812.42499999998,
3814.25000000002,
3816.07499999997,
3817.90000000001,
3819.72499999997,
3821.55000000001,
3823.37499999997,
3825.20000000001,
3827.02499999996,
3828.85,
3830.67499999996,
3832.5,
3834.32500000004,
3836.15,
3837.97500000004,
3839.79999999999,
3841.62500000003,
3843.44999999999,
3845.27500000003,
3847.09999999999,
3848.92500000003,
3850.74999999998,
3852.57500000002,
3854.39999999998,
3856.22500000002,
3858.04999999998,
3859.87500000002,
3861.69999999997,
3863.52500000001,
3865.34999999997,
3867.17500000001,
3868.99999999997,
3870.82500000001,
3872.64999999996,
3874.475,
3876.29999999996,
3878.125,
3879.95000000004,
3881.775,
3883.60000000004,
3885.42499999999,
3887.25000000003,
3889.07499999999,
3890.90000000003,
3892.72499999999,
3894.55000000003,
3896.37499999998,
3898.20000000002,
3900.02499999998,
3901.85000000002,
3903.67499999998,
3905.50000000002,
3907.32499999997,
3909.15000000001,
3910.97499999997,
3912.80000000001,
3914.62499999997,
3916.45000000001,
3918.27499999996,
3920.1,
3921.92499999996,
3923.75,
3925.57500000004,
3927.4,
3929.22500000004,
3931.04999999999,
3932.87500000003,
3934.69999999999,
3936.52500000003,
3938.34999999999,
3940.17500000003,
3941.99999999998,
3943.82500000002,
3945.64999999998,
3947.47500000002,
3949.29999999998,
3951.12500000002,
3952.94999999997,
3954.77500000001,
3956.59999999997,
3958.42500000001,
3960.24999999997,
3962.07500000001,
3963.89999999996,
3965.725,
3967.54999999996,
3969.375,
3971.20000000004,
3973.025,
3974.85000000004,
3976.67499999999,
3978.50000000003,
3980.32499999999,
3982.15000000003,
3983.97499999999,
3985.80000000003,
3987.62499999998,
3989.45000000002,
3991.27499999998,
3993.10000000002,
3994.92499999998,
3996.75000000002,
3998.57499999997,
4000.40000000001,
4002.22499999997,
4004.05000000001,
4005.87499999997,
4007.70000000001,
4009.52499999996,
4011.35,
4013.17499999996,
4015,
4016.82500000004,
4018.65,
4020.47500000004,
4022.29999999999,
4024.12500000003,
4025.94999999999,
4027.77500000003,
4029.59999999999,
4031.42500000003,
4033.24999999998,
4035.07500000002,
4036.89999999998,
4038.72500000002,
4040.54999999998,
4042.37500000002,
4044.19999999997,
4046.02500000001,
4047.84999999997,
4049.67500000001,
4051.49999999997,
4053.32500000001,
4055.14999999996,
4056.975,
4058.79999999996,
4060.625,
4062.45000000004,
4064.275,
4066.10000000004,
4067.92499999999,
4069.75000000003,
4071.57499999999,
4073.40000000003,
4075.22499999999,
4077.05000000003,
4078.87499999998,
4080.70000000002,
4082.52499999998,
4084.35000000002,
4086.17499999998,
4088.00000000002,
4089.82499999997,
4091.65000000001,
4093.47499999997,
4095.30000000001,
4097.12499999997,
4098.95000000001,
4100.77499999996,
4102.6,
4104.42499999996,
4106.25,
4108.07500000004,
4109.9,
4111.72500000004,
4113.54999999999,
4115.37500000003,
4117.19999999999,
4119.02500000003,
4120.84999999999,
4122.67500000003,
4124.49999999998,
4126.32500000002,
4128.14999999998,
4129.97500000002,
4131.79999999998,
4133.62500000002,
4135.44999999997,
4137.27500000001,
4139.09999999997,
4140.92500000001,
4142.74999999997,
4144.57500000001,
4146.39999999996,
4148.225,
4150.04999999996,
4151.875,
4153.70000000004,
4155.525,
4157.35000000004,
4159.17499999999,
4161.00000000003,
4162.82499999999,
4164.65000000003,
4166.47499999999,
4168.30000000003,
4170.12499999998,
4171.95000000002,
4173.77499999998,
4175.60000000002,
4177.42499999998,
4179.25000000002,
4181.07499999997,
4182.90000000001,
4184.72499999997,
4186.55000000001,
4188.37499999997,
4190.20000000001,
4192.02499999996,
4193.85,
4195.67499999996,
4197.5,
4199.32500000004,
4201.15,
4202.97500000004,
4204.79999999999,
4206.62500000003,
4208.44999999999,
4210.27500000003,
4212.09999999999,
4213.92500000003,
4215.74999999998,
4217.57500000002,
4219.39999999998,
4221.22500000002,
4223.04999999998,
4224.87500000002,
4226.69999999997,
4228.52500000001,
4230.34999999997,
4232.17500000001,
4233.99999999997,
4235.82500000001,
4237.64999999996,
4239.475,
4241.29999999996,
4243.125,
4244.95000000004,
4246.775,
4248.60000000004,
4250.42499999999,
4252.25000000003,
4254.07499999999,
4255.90000000003,
4257.72499999999,
4259.55000000003,
4261.37499999998,
4263.20000000002,
4265.02499999998,
4266.85000000002,
4268.67499999998,
4270.50000000002,
4272.32499999997,
4274.15000000001,
4275.97499999997,
4277.80000000001,
4279.62499999997,
4281.45000000001,
4283.27499999996,
4285.1,
4286.92499999996,
4288.75,
4290.57500000004,
4292.4,
4294.22500000004,
4296.04999999999,
4297.87500000003,
4299.69999999999,
4301.52500000003,
4303.34999999999,
4305.17500000003,
4306.99999999998,
4308.82500000002,
4310.64999999998,
4312.47500000002,
4314.29999999998,
4316.12500000002,
4317.94999999997,
4319.77500000001,
4321.59999999997,
4323.42500000001,
4325.24999999997,
4327.07500000001,
4328.89999999996,
4330.725,
4332.54999999996,
4334.375,
4336.20000000004,
4338.025,
4339.85000000004,
4341.67499999999,
4343.50000000003,
4345.32499999999,
4347.15000000003,
4348.97499999999,
4350.80000000003,
4352.62499999998,
4354.45000000002,
4356.27499999998,
4358.10000000002,
4359.92499999998,
4361.75000000002,
4363.57499999997,
4365.40000000001,
4367.22499999997,
4369.05000000001,
4370.87499999997,
4372.70000000001,
4374.52499999996,
4376.35,
4378.17499999996,
4380,
4381.82500000004,
4383.65,
4385.47500000004,
4387.29999999999,
4389.12500000003,
4390.94999999999,
4392.77500000003,
4394.59999999999,
4396.42500000003,
4398.24999999998,
4400.07500000002,
4401.89999999998,
4403.72500000002,
4405.54999999998,
4407.37500000002,
4409.19999999997,
4411.02500000001,
4412.84999999997,
4414.67500000001,
4416.49999999997,
4418.32500000001,
4420.14999999996,
4421.975,
4423.79999999996,
4425.625,
4427.45000000004,
4429.275,
4431.10000000004,
4432.92499999999,
4434.75000000003,
4436.57499999999,
4438.40000000003,
4440.22499999999,
4442.05000000003,
4443.87499999998,
4445.70000000002,
4447.52499999998,
4449.35000000002,
4451.17499999998,
4453.00000000002,
4454.82499999997,
4456.65000000001,
4458.47499999997,
4460.30000000001,
4462.12499999997,
4463.95000000001,
4465.77499999996,
4467.6,
4469.42499999996,
4471.25,
4473.07500000004,
4474.9,
4476.72500000004,
4478.54999999999,
4480.37500000003,
4482.19999999999,
4484.02500000003,
4485.84999999999,
4487.67500000003,
4489.49999999998,
4491.32500000002,
4493.14999999998,
4494.97500000002,
4496.79999999998,
4498.62500000002,
4500.44999999997,
4502.27500000001,
4504.09999999997,
4505.92500000001,
4507.74999999997,
4509.57500000001,
4511.39999999996,
4513.225,
4515.04999999996,
4516.875,
4518.70000000004,
4520.525,
4522.35000000004,
4524.17499999999,
4526.00000000003,
4527.82499999999,
4529.65000000003,
4531.47499999999,
4533.30000000003,
4535.12499999998,
4536.95000000002,
4538.77499999998,
4540.60000000002,
4542.42499999998,
4544.25000000002,
4546.07499999997,
4547.90000000001,
4549.72499999997,
4551.55000000001,
4553.37499999997,
4555.20000000001,
4557.02499999996,
4558.85,
4560.67499999996,
4562.5,
4564.32500000004,
4566.15,
4567.97500000004,
4569.79999999999,
4571.62500000003,
4573.44999999999,
4575.27500000003,
4577.09999999999,
4578.92500000003,
4580.74999999998,
4582.57500000002,
4584.39999999998,
4586.22500000002,
4588.04999999998,
4589.87500000002,
4591.69999999997,
4593.52500000001,
4595.34999999997,
4597.17500000001,
4598.99999999997,
4600.82500000001,
4602.64999999996,
4604.475,
4606.29999999996,
4608.125,
4609.95000000004,
4611.775,
4613.60000000004,
4615.42499999999,
4617.25000000003,
4619.07499999999,
4620.90000000003,
4622.72499999999,
4624.55000000003,
4626.37499999998,
4628.20000000002,
4630.02499999998,
4631.85000000002,
4633.67499999998,
4635.50000000002,
4637.32499999997,
4639.15000000001,
4640.97499999997,
4642.80000000001,
4644.62499999997,
4646.45000000001,
4648.27499999996,
4650.1,
4651.92499999996,
4653.75,
4655.57500000004,
4657.4,
4659.22500000004,
4661.04999999999,
4662.87500000003,
4664.69999999999,
4666.52500000003,
4668.34999999999,
4670.17500000003,
4671.99999999998,
4673.82500000002,
4675.64999999998,
4677.47500000002,
4679.29999999998,
4681.12500000002,
4682.94999999997,
4684.77500000001,
4686.59999999997,
4688.42500000001,
4690.24999999997,
4692.07500000001,
4693.89999999996,
4695.725,
4697.54999999996,
4699.375,
4701.20000000004,
4703.025,
4704.85000000004,
4706.67499999999,
4708.50000000003,
4710.32499999999,
4712.15000000003,
4713.97499999999,
4715.80000000003,
4717.62499999998,
4719.45000000002,
4721.27499999998,
4723.10000000002,
4724.92499999998,
4726.75000000002,
4728.57499999997,
4730.40000000001,
4732.22499999997,
4734.05000000001,
4735.87499999997,
4737.70000000001,
4739.52499999996,
4741.35,
4743.17499999996,
4745,
4746.82500000004,
4748.65,
4750.47500000004,
4752.29999999999,
4754.12500000003,
4755.94999999999,
4757.77500000003,
4759.59999999999,
4761.42500000003,
4763.24999999998,
4765.07500000002,
4766.89999999998,
4768.72500000002,
4770.54999999998,
4772.37500000002,
4774.19999999997,
4776.02500000001,
4777.84999999997,
4779.67500000001,
4781.49999999997,
4783.32500000001,
4785.14999999996,
4786.975,
4788.79999999996,
4790.625,
4792.45000000004,
4794.275,
4796.10000000004,
4797.92499999999,
4799.75000000003,
4801.57499999999,
4803.40000000003,
4805.22499999999,
4807.05000000003,
4808.87499999998,
4810.70000000002,
4812.52499999998,
4814.35000000002,
4816.17499999998,
4818.00000000002,
4819.82499999997,
4821.65000000001,
4823.47499999997,
4825.30000000001,
4827.12499999997,
4828.95000000001,
4830.77499999996,
4832.6,
4834.42499999996,
4836.25,
4838.07500000004,
4839.9,
4841.72500000004,
4843.54999999999,
4845.37500000003,
4847.19999999999,
4849.02500000003,
4850.84999999999,
4852.67500000003,
4854.49999999998,
4856.32500000002,
4858.14999999998,
4859.97500000002,
4861.79999999998,
4863.62500000002,
4865.44999999997,
4867.27500000001,
4869.09999999997,
4870.92500000001,
4872.74999999997,
4874.57500000001,
4876.39999999996,
4878.225,
4880.04999999996,
4881.875,
4883.70000000004,
4885.525,
4887.35000000004,
4889.17499999999,
4891.00000000003,
4892.82499999999,
4894.65000000003,
4896.47499999999,
4898.30000000003,
4900.12499999998,
4901.95000000002,
4903.77499999998,
4905.60000000002,
4907.42499999998,
4909.25000000002,
4911.07499999997,
4912.90000000001,
4914.72499999997,
4916.55000000001,
4918.37499999997,
4920.20000000001,
4922.02499999996,
4923.85,
4925.67499999996,
4927.5,
4929.32500000004,
4931.15,
4932.97500000004,
4934.79999999999,
4936.62500000003,
4938.44999999999,
4940.27500000003,
4942.09999999999,
4943.92500000003,
4945.74999999998,
4947.57500000002,
4949.39999999998,
4951.22500000002,
4953.04999999998,
4954.87500000002,
4956.69999999997,
4958.52500000001,
4960.34999999997,
4962.17500000001,
4963.99999999997,
4965.82500000001,
4967.64999999996,
4969.475,
4971.29999999996,
4973.125,
4974.95000000004,
4976.775,
4978.60000000004,
4980.42499999999,
4982.25000000003,
4984.07499999999,
4985.90000000003,
4987.72499999999,
4989.55000000003,
4991.37499999998,
4993.20000000002,
4995.02499999998,
4996.85000000002,
4998.67499999998,
5000.50000000002,
5002.32499999997,
5004.15000000001,
5005.97499999997,
5007.80000000001,
5009.62499999997,
5011.45000000001,
5013.27499999996,
5015.1,
5016.92499999996,
5018.75,
5020.57500000004,
5022.4,
5024.22500000004,
5026.04999999999,
5027.87500000003,
5029.69999999999,
5031.52500000003,
5033.34999999999,
5035.17500000003,
5036.99999999998,
5038.82500000002,
5040.64999999998,
5042.47500000002,
5044.29999999998,
5046.12500000002,
5047.94999999997,
5049.77500000001,
5051.59999999997,
5053.42500000001,
5055.24999999997,
5057.07500000001,
5058.89999999996,
5060.725,
5062.54999999996,
5064.375,
5066.20000000004,
5068.025,
5069.85000000004,
5071.67499999999,
5073.50000000003,
5075.32499999999,
5077.15000000003,
5078.97499999999,
5080.80000000003,
5082.62499999998,
5084.45000000002,
5086.27499999998,
5088.10000000002,
5089.92499999998,
5091.75000000002,
5093.57499999997,
5095.40000000001,
5097.22499999997,
5099.05000000001,
5100.87499999997,
5102.70000000001,
5104.52499999996,
5106.35,
5108.17499999996,
5110,
5111.82500000004,
5113.65,
5115.47500000004,
5117.29999999999,
5119.12500000003,
5120.94999999999,
5122.77500000003,
5124.59999999999,
5126.42500000003,
5128.24999999998,
5130.07500000002,
5131.89999999998,
5133.72500000002,
5135.54999999998,
5137.37500000002,
5139.19999999997,
5141.02500000001,
5142.84999999997,
5144.67500000001,
5146.49999999997,
5148.32500000001,
5150.14999999996,
5151.975,
5153.79999999996,
5155.625,
5157.45000000004,
5159.275,
5161.10000000004,
5162.92499999999,
5164.75000000003,
5166.57499999999,
5168.40000000003,
5170.22499999999,
5172.05000000003,
5173.87499999998,
5175.70000000002,
5177.52499999998,
5179.35000000002,
5181.17499999998,
5183.00000000002,
5184.82499999997,
5186.65000000001,
5188.47499999997,
5190.30000000001,
5192.12499999997,
5193.95000000001,
5195.77499999996,
5197.6,
5199.42499999996,
5201.25,
5203.07500000004,
5204.9,
5206.72500000004,
5208.54999999999,
5210.37500000003,
5212.19999999999,
5214.02500000003,
5215.84999999999,
5217.67500000003,
5219.49999999998,
5221.32500000002,
5223.14999999998,
5224.97500000002,
5226.79999999998,
5228.62500000002,
5230.44999999997,
5232.27500000001,
5234.09999999997,
5235.92500000001,
5237.74999999997,
5239.57500000001,
5241.39999999996,
5243.225,
5245.04999999996,
5246.875,
5248.70000000004,
5250.525,
5252.35000000004,
5254.17499999999,
5256.00000000003,
5257.82499999999,
5259.65000000003,
5261.47499999999,
5263.30000000003,
5265.12499999998,
5266.95000000002,
5268.77499999998,
5270.60000000002,
5272.42499999998,
5274.25000000002,
5276.07499999997,
5277.90000000001,
5279.72499999997,
5281.55000000001,
5283.37499999997,
5285.20000000001,
5287.02499999996,
5288.85,
5290.67499999996,
5292.5,
5294.32500000004,
5296.15,
5297.97500000004,
5299.79999999999,
5301.62500000003,
5303.44999999999,
5305.27500000003,
5307.09999999999,
5308.92500000003,
5310.74999999998,
5312.57500000002,
5314.39999999998,
5316.22500000002,
5318.04999999998,
5319.87500000002,
5321.69999999997,
5323.52500000001,
5325.34999999997,
5327.17500000001,
5328.99999999997,
5330.82500000001,
5332.64999999996,
5334.475,
5336.29999999996,
5338.125,
5339.95000000004,
5341.775,
5343.60000000004,
5345.42499999999,
5347.25000000003,
5349.07499999999,
5350.90000000003,
5352.72499999999,
5354.55000000003,
5356.37499999998,
5358.20000000002,
5360.02499999998,
5361.85000000002,
5363.67499999998,
5365.50000000002,
5367.32499999997,
5369.15000000001,
5370.97499999997,
5372.80000000001,
5374.62499999997,
5376.45000000001,
5378.27499999996,
5380.1,
5381.92499999996,
5383.75,
5385.57500000004,
5387.4,
5389.22500000004,
5391.04999999999,
5392.87500000003,
5394.69999999999,
5396.52500000003,
5398.34999999999,
5400.17500000003,
5401.99999999998,
5403.82500000002,
5405.64999999998,
5407.47500000002,
5409.29999999998,
5411.12500000002,
5412.94999999997,
5414.77500000001,
5416.59999999997,
5418.42500000001,
5420.24999999997,
5422.07500000001,
5423.89999999996,
5425.725,
5427.54999999996,
5429.375,
5431.20000000004,
5433.025,
5434.85000000004,
5436.67499999999,
5438.50000000003,
5440.32499999999,
5442.15000000003,
5443.97499999999,
5445.80000000003,
5447.62499999998,
5449.45000000002,
5451.27499999998,
5453.10000000002,
5454.92499999998,
5456.75000000002,
5458.57499999997,
5460.40000000001,
5462.22499999997,
5464.05000000001,
5465.87499999997,
5467.70000000001,
5469.52499999996,
5471.35,
5473.17499999996,
5475,
5476.82500000004,
5478.65,
5480.47500000004,
5482.29999999999,
5484.12500000003,
5485.94999999999,
5487.77500000003,
5489.59999999999,
5491.42500000003,
5493.24999999998,
5495.07500000002,
5496.89999999998,
5498.72500000002,
5500.54999999998,
5502.37500000002,
5504.19999999997,
5506.02500000001,
5507.84999999997,
5509.67500000001,
5511.49999999997,
5513.32500000001,
5515.14999999996,
5516.975,
5518.79999999996,
5520.625,
5522.45000000004,
5524.275,
5526.10000000004,
5527.92499999999,
5529.75000000003,
5531.57499999999,
5533.40000000003,
5535.22499999999,
5537.05000000003,
5538.87499999998,
5540.70000000002,
5542.52499999998,
5544.35000000002,
5546.17499999998,
5548.00000000002,
5549.82499999997,
5551.65000000001,
5553.47499999997,
5555.30000000001,
5557.12499999997,
5558.95000000001,
5560.77499999996,
5562.6,
5564.42499999996,
5566.25,
5568.07500000004,
5569.9,
5571.72500000004,
5573.54999999999,
5575.37500000003,
5577.19999999999,
5579.02500000003,
5580.84999999999,
5582.67500000003,
5584.49999999998,
5586.32500000002,
5588.14999999998,
5589.97500000002,
5591.79999999998,
5593.62500000002,
5595.44999999997,
5597.27500000001,
5599.09999999997,
5600.92500000001,
5602.74999999997,
5604.57500000001,
5606.39999999996,
5608.225,
5610.04999999996,
5611.875,
5613.70000000004,
5615.525,
5617.35000000004,
5619.17499999999,
5621.00000000003,
5622.82499999999,
5624.65000000003,
5626.47499999999,
5628.30000000003,
5630.12499999998,
5631.95000000002,
5633.77499999998,
5635.60000000002,
5637.42499999998,
5639.25000000002,
5641.07499999997,
5642.90000000001,
5644.72499999997,
5646.55000000001,
5648.37499999997,
5650.20000000001,
5652.02499999996,
5653.85,
5655.67499999996,
5657.5,
5659.32500000004,
5661.15,
5662.97500000004,
5664.79999999999,
5666.62500000003,
5668.44999999999,
5670.27500000003,
5672.09999999999,
5673.92500000003,
5675.74999999998,
5677.57500000002,
5679.39999999998,
5681.22500000002,
5683.04999999998,
5684.87500000002,
5686.69999999997,
5688.52500000001,
5690.34999999997,
5692.17500000001,
5693.99999999997,
5695.82500000001,
5697.64999999996,
5699.475,
5701.29999999996,
5703.125,
5704.95000000004,
5706.775,
5708.60000000004,
5710.42499999999,
5712.25000000003,
5714.07499999999,
5715.90000000003,
5717.72499999999,
5719.55000000003,
5721.37499999998,
5723.20000000002,
5725.02499999998,
5726.85000000002,
5728.67499999998,
5730.50000000002,
5732.32499999997,
5734.15000000001,
5735.97499999997,
5737.80000000001,
5739.62499999997,
5741.45000000001,
5743.27499999996,
5745.1,
5746.92499999996,
5748.75,
5750.57500000004,
5752.4,
5754.22500000004,
5756.04999999999,
5757.87500000003,
5759.69999999999,
5761.52500000003,
5763.34999999999,
5765.17500000003,
5766.99999999998,
5768.82500000002,
5770.64999999998,
5772.47500000002,
5774.29999999998,
5776.12500000002,
5777.94999999997,
5779.77500000001,
5781.59999999997,
5783.42500000001,
5785.24999999997,
5787.07500000001,
5788.89999999996,
5790.725,
5792.54999999996,
5794.375,
5796.20000000004,
5798.025,
5799.85000000004,
5801.67499999999,
5803.50000000003,
5805.32499999999,
5807.15000000003,
5808.97499999999,
5810.80000000003,
5812.62499999998,
5814.45000000002,
5816.27499999998,
5818.10000000002,
5819.92499999998,
5821.75000000002,
5823.57499999997,
5825.40000000001,
5827.22499999997,
5829.05000000001,
5830.87499999997,
5832.70000000001,
5834.52499999996,
5836.35,
5838.17499999996,
5840,
5841.82500000004,
5843.65,
5845.47500000004,
5847.29999999999,
5849.12500000003,
5850.94999999999,
5852.77500000003,
5854.59999999999,
5856.42500000003,
5858.24999999998,
5860.07500000002,
5861.89999999998,
5863.72500000002,
5865.54999999998,
5867.37500000002,
5869.19999999997,
5871.02500000001,
5872.84999999997,
5874.67500000001,
5876.49999999997,
5878.32500000001,
5880.14999999996,
5881.975,
5883.79999999996,
5885.625,
5887.45000000004,
5889.275,
5891.10000000004,
5892.92499999999,
5894.75000000003,
5896.57499999999,
5898.40000000003,
5900.22499999999,
5902.05000000003,
5903.87499999998,
5905.70000000002,
5907.52499999998,
5909.35000000002,
5911.17499999998,
5913.00000000002,
5914.82499999997,
5916.65000000001,
5918.47499999997,
5920.30000000001,
5922.12499999997,
5923.95000000001,
5925.77499999996,
5927.6,
5929.42499999996,
5931.25,
5933.07500000004,
5934.9,
5936.72500000004,
5938.54999999999,
5940.37500000003,
5942.19999999999,
5944.02500000003,
5945.84999999999,
5947.67500000003,
5949.49999999998,
5951.32500000002,
5953.14999999998,
5954.97500000002,
5956.79999999998,
5958.62500000002,
5960.44999999997,
5962.27500000001,
5964.09999999997,
5965.92500000001,
5967.74999999997,
5969.57500000001,
5971.39999999996,
5973.225,
5975.04999999996,
5976.875,
5978.70000000004,
5980.525,
5982.35000000004,
5984.17499999999,
5986.00000000003,
5987.82499999999,
5989.65000000003,
5991.47499999999,
5993.30000000003,
5995.12499999998,
5996.95000000002,
5998.77499999998,
6000.60000000002,
6002.42499999998,
6004.25000000002,
6006.07499999997,
6007.90000000001,
6009.72499999997,
6011.55000000001,
6013.37499999997,
6015.20000000001,
6017.02499999996,
6018.85,
6020.67499999996,
6022.5,
6024.32500000004,
6026.15,
6027.97500000004,
6029.79999999999,
6031.62500000003,
6033.44999999999,
6035.27500000003,
6037.09999999999,
6038.92500000003,
6040.74999999998,
6042.57500000002,
6044.39999999998,
6046.22500000002,
6048.04999999998,
6049.87500000002,
6051.69999999997,
6053.52500000001,
6055.34999999997,
6057.17500000001,
6058.99999999997,
6060.82500000001,
6062.64999999996,
6064.475,
6066.29999999996,
6068.125,
6069.95000000004,
6071.775,
6073.60000000004,
6075.42499999999,
6077.25000000003,
6079.07499999999,
6080.90000000003,
6082.72499999999,
6084.55000000003,
6086.37499999998,
6088.20000000002,
6090.02499999998,
6091.85000000002,
6093.67499999998,
6095.50000000002,
6097.32499999997,
6099.15000000001,
6100.97499999997,
6102.80000000001,
6104.62499999997,
6106.45000000001,
6108.27499999996,
6110.1,
6111.92499999996,
6113.75,
6115.57500000004,
6117.4,
6119.22500000004,
6121.04999999999,
6122.87500000003,
6124.69999999999,
6126.52500000003,
6128.34999999999,
6130.17500000003,
6131.99999999998,
6133.82500000002,
6135.64999999998,
6137.47500000002,
6139.29999999998,
6141.12500000002,
6142.94999999997,
6144.77500000001,
6146.59999999997,
6148.42500000001,
6150.24999999997,
6152.07500000001,
6153.89999999996,
6155.725,
6157.54999999996,
6159.375,
6161.20000000004,
6163.025,
6164.85000000004,
6166.67499999999,
6168.50000000003,
6170.32499999999,
6172.15000000003,
6173.97499999999,
6175.80000000003,
6177.62499999998,
6179.45000000002,
6181.27499999998,
6183.10000000002,
6184.92499999998,
6186.75000000002,
6188.57499999997,
6190.40000000001,
6192.22499999997,
6194.05000000001,
6195.87499999997,
6197.70000000001,
6199.52499999996,
6201.35,
6203.17499999996,
6205,
6206.82500000004,
6208.65,
6210.47500000004,
6212.29999999999,
6214.12500000003,
6215.94999999999,
6217.77500000003,
6219.59999999999,
6221.42500000003,
6223.24999999998,
6225.07500000002,
6226.89999999998,
6228.72500000002,
6230.54999999998,
6232.37500000002,
6234.19999999997,
6236.02500000001,
6237.84999999997,
6239.67500000001,
6241.49999999997,
6243.32500000001,
6245.14999999996,
6246.975,
6248.79999999996,
6250.625,
6252.45000000004,
6254.275,
6256.10000000004,
6257.92499999999,
6259.75000000003,
6261.57499999999,
6263.40000000003,
6265.22499999999,
6267.05000000003,
6268.87499999998,
6270.70000000002,
6272.52499999998,
6274.35000000002,
6276.17499999998,
6278.00000000002,
6279.82499999997,
6281.65000000001,
6283.47499999997,
6285.30000000001,
6287.12499999997,
6288.95000000001,
6290.77499999996,
6292.6,
6294.42499999996,
6296.25,
6298.07500000004,
6299.9,
6301.72500000004,
6303.54999999999,
6305.37500000003,
6307.19999999999,
6309.02500000003,
6310.84999999999,
6312.67500000003,
6314.49999999998,
6316.32500000002,
6318.14999999998,
6319.97500000002,
6321.79999999998,
6323.62500000002,
6325.44999999997,
6327.27500000001,
6329.09999999997,
6330.92500000001,
6332.74999999997,
6334.57500000001,
6336.39999999996,
6338.225,
6340.04999999996,
6341.875,
6343.70000000004,
6345.525,
6347.35000000004,
6349.17499999999,
6351.00000000003,
6352.82499999999,
6354.65000000003,
6356.47499999999,
6358.30000000003,
6360.12499999998,
6361.95000000002,
6363.77499999998,
6365.60000000002,
6367.42499999998,
6369.25000000002,
6371.07499999997,
6372.90000000001,
6374.72499999997,
6376.55000000001,
6378.37499999997,
6380.20000000001,
6382.02499999996,
6383.85,
6385.67499999996,
6387.5,
6389.32500000004,
6391.15,
6392.97500000004,
6394.79999999999,
6396.62500000003,
6398.44999999999,
6400.27500000003,
6402.09999999999,
6403.92500000003,
6405.74999999998,
6407.57500000002,
6409.39999999998,
6411.22500000002,
6413.04999999998,
6414.87500000002,
6416.69999999997,
6418.52500000001,
6420.34999999997,
6422.17500000001,
6423.99999999997,
6425.82500000001,
6427.64999999996,
6429.475,
6431.29999999996,
6433.125,
6434.95000000004,
6436.775,
6438.60000000004,
6440.42499999999,
6442.25000000003,
6444.07499999999,
6445.90000000003,
6447.72499999999,
6449.55000000003,
6451.37499999998,
6453.20000000002,
6455.02499999998,
6456.85000000002,
6458.67499999998,
6460.50000000002,
6462.32499999997,
6464.15000000001,
6465.97499999997,
6467.80000000001,
6469.62499999997,
6471.45000000001,
6473.27499999996,
6475.1,
6476.92499999996,
6478.75,
6480.57500000004,
6482.4,
6484.22500000004,
6486.04999999999,
6487.87500000003,
6489.69999999999,
6491.52500000003,
6493.34999999999,
6495.17500000003,
6496.99999999998,
6498.82500000002,
6500.64999999998,
6502.47500000002,
6504.29999999998,
6506.12500000002,
6507.94999999997,
6509.77500000001,
6511.59999999997,
6513.42500000001,
6515.24999999997,
6517.07500000001,
6518.89999999996,
6520.725,
6522.54999999996,
6524.375,
6526.20000000004,
6528.025,
6529.85000000004,
6531.67499999999,
6533.50000000003,
6535.32499999999,
6537.15000000003,
6538.97499999999,
6540.80000000003,
6542.62499999998,
6544.45000000002,
6546.27499999998,
6548.10000000002,
6549.92499999998,
6551.75000000002,
6553.57499999997,
6555.40000000001,
6557.22499999997,
6559.05000000001,
6560.87499999997,
6562.70000000001,
6564.52499999996,
6566.35,
6568.17499999996,
6570};

  double points2[] = {
    -0.493548387,
-0.470516128919476,
-0.44748387084,
-0.424451612759476,
-0.40141935468,
-0.378387096599476,
-0.35535483852,
-0.332322580439476,
-0.30929032236,
-0.286258064279476,
-0.2632258062,
-0.240193548119476,
-0.21716129004,
-0.194129031959476,
-0.17109677388,
-0.148064515799476,
-0.12503225772,
-0.167841013483809,
-0.342331796919365,
-0.516822580362857,
-0.691313363798413,
-0.865804147241904,
-1.04029493067746,
-1.21478571412095,
-1.38927649755651,
-1.563767281,
-1.73825806444349,
-1.91274884787905,
-2.08723963132254,
-2.2617304147581,
-2.43622119820159,
-2.61071198163714,
-2.78520276508063,
-2.95969354851619,
-2.63714285728,
-2.06607142871298,
-1.49500000012,
-0.923928571552985,
-0.35285714296,
0.218214285607016,
0.7892857142,
1.36035714276702,
1.93142857136,
2.50249999992701,
3.07357142852,
3.64464285708702,
4.21571428568,
4.78678571424702,
5.35785714284,
5.92892857140702,
6.5,
6.6144000000226,
6.72880000004,
6.8432000000626,
6.95760000008,
7.0720000001026,
7.18640000012,
7.3008000001426,
7.41520000016,
7.5296000001826,
7.6440000002,
7.7584000002226,
7.87280000024,
7.9872000002626,
8.10160000028,
8.2160000003026,
8.33040000032,
8.46537204326384,
8.64148817203936,
8.81760430082288,
8.9937204295984,
9.16983655838192,
9.34595268715744,
9.52206881594096,
9.69818494471648,
9.8743010735,
10.0504172022835,
10.226533331059,
10.4026494598426,
10.5787655886181,
10.7548817174016,
10.9309978461771,
11.1071139749606,
11.2832301037362,
11.5778580608,
11.931741931992,
12.2856258032,
12.639509674392,
12.9933935456,
13.347277416792,
13.701161288,
14.055045159192,
14.4089290304,
14.762812901592,
15.1166967728,
15.470580643992,
15.8244645152,
16.178348386392,
16.5322322576,
16.886116128792,
17.24,
17.458051613005,
17.676103226,
17.894154839005,
18.112206452,
18.330258065005,
18.548309678,
18.766361291005,
18.984412904,
19.202464517005,
19.42051613,
19.638567743005,
19.856619356,
20.074670969005,
20.292722582,
20.510774195005,
20.728825808,
20.8516129047985,
20.7838709692002,
20.7161290335989,
20.6483870980006,
20.5806451623993,
20.512903226801,
20.4451612911996,
20.3774193556014,
20.30967742,
20.2419354843986,
20.1741935488004,
20.106451613199,
20.0387096776007,
19.9709677419994,
19.9032258064011,
19.8354838707998,
19.7677419352015,
19.5998881716,
19.381978494005,
19.1640688164,
18.946159138805,
18.7282494612,
18.510339783605,
18.292430106,
18.074520428405,
17.8566107508,
17.638701073205,
17.4207913956,
17.202881718005,
16.9849720404,
16.767062362805,
16.5491526852,
16.331243007605,
16.11333333,
15.6367913947092,
15.16024945944,
14.6837075241492,
14.20716558888,
13.7306236535892,
13.25408171832,
12.7775397830292,
12.30099784776,
11.8244559124692,
11.3479139772,
10.8713720419092,
10.39483010664,
9.91828817134916,
9.44174623608,
8.96520430078916,
8.48866236552,
8.09721505381517,
7.87595698928081,
7.65469892473638,
7.43344086020201,
7.21218279565759,
6.99092473112322,
6.76966666657879,
6.54840860204443,
6.3271505375,
6.10589247295557,
5.88463440842121,
5.66337634387678,
5.44211827934241,
5.22086021479799,
4.99960215026362,
4.7783440857192,
4.55708602118483,
4.22916129,
3.84790322550867,
3.466645161,
3.08538709650867,
2.704129032,
2.32287096750867,
1.941612903,
1.56035483850867,
1.179096774,
0.797838709508669,
0.416580645,
0.035322580508669,
-0.345935484,
-0.727193548491331,
-1.108451613,
-1.48970967749133,
-1.870967742,
-1.72445161295667,
-1.57793548392,
-1.43141935487667,
-1.28490322584,
-1.13838709679667,
-0.99187096776,
-0.845354838716668,
-0.69883870968,
-0.552322580636669,
-0.4058064516,
-0.259290322556669,
-0.11277419352,
0.033741935523332,
0.18025806456,
0.326774193603331,
0.47329032264,
0.61403114578282,
0.74322135711953,
0.872411568462115,
1.00160177979883,
1.13079199114141,
1.25998220247812,
1.3891724138207,
1.51836262515741,
1.6475528365,
1.77674304784258,
1.9059332591793,
2.03512347052188,
2.16431368185859,
2.29350389320118,
2.42269410453788,
2.55188431588047,
2.68107452721718,
2.81955951052,
2.96269187979675,
3.10582424908,
3.24895661835675,
3.39208898764,
3.53522135691675,
3.6783537262,
3.82148609547675,
3.96461846476,
4.10775083403674,
4.25088320332,
4.39401557259674,
4.53714794188,
4.68028031115674,
4.82341268044,
4.96654504971674,
5.109677419,
5.41729677406699,
5.72491612912,
6.03253548418699,
6.34015483924,
6.64777419430699,
6.95539354936,
7.26301290442699,
7.57063225948,
7.87825161454699,
8.1858709696,
8.493490324667,
8.80110967972,
9.10872903478699,
9.41634838984,
9.72396774490699,
10.03158709996,
10.3324494656063,
10.619797852399,
10.9071462392047,
11.1944946259974,
11.4818430128031,
11.7691913995958,
12.0565397864016,
12.3438881731942,
12.63123656,
12.9185849468057,
13.2059333335984,
13.4932817204042,
13.7806301071969,
14.0679784940026,
14.3553268807953,
14.642675267601,
14.9300236543937,
15.1650408588,
15.3738924719953,
15.5827440852,
15.7915956983953,
16.0004473116,
16.2092989247953,
16.418150538,
16.6270021511953,
16.8358537644,
17.0447053775952,
17.2535569908,
17.4624086039953,
17.6712602172,
17.8801118303953,
18.0889634436,
18.2978150567953,
18.50666667,
18.6632344116036,
18.8198021532,
18.9763698948036,
19.1329376364,
19.2895053780036,
19.4460731196,
19.6026408612036,
19.7592086028,
19.9157763444036,
20.072344086,
20.2289118276036,
20.3854795692,
20.5420473108036,
20.6986150524,
20.8551827940036,
21.0117505356,
21.1816129010043,
21.3780645139993,
21.5745161270032,
21.7709677399982,
21.9674193530021,
22.1638709659971,
22.3603225790011,
22.5567741919961,
22.753225805,
22.9496774180039,
23.1461290309989,
23.3425806440029,
23.5390322569979,
23.7354838700018,
23.9319354829968,
24.1283870960007,
24.3248387089957,
24.0375096768,
23.508290322012,
22.9790709672,
22.449851612412,
21.9206322576,
21.391412902812,
20.862193548,
20.332974193212,
19.8037548384,
19.274535483612,
18.7453161288,
18.216096774012,
17.6868774192,
17.157658064412,
16.6284387096,
16.099219354812,
15.57,
15.1599290322507,
14.74985806452,
14.3397870967707,
13.92971612904,
13.5196451612907,
13.10957419356,
12.6995032258107,
12.28943225808,
11.8793612903307,
11.4692903226,
11.0592193548507,
10.64914838712,
10.2390774193707,
9.82900645164,
9.41893548389067,
9.00886451616,
8.65477419357472,
8.41264516132088,
8.17051612905604,
7.9283870968022,
7.68625806453736,
7.44412903228352,
7.20200000001868,
6.95987096776484,
6.7177419355,
6.47561290323516,
6.23348387098132,
5.99135483871648,
5.74922580646264,
5.5070967741978,
5.26496774194396,
5.02283870967912,
4.78070967742529,
4.52554838708,
4.26387096770595,
4.00219354832,
3.74051612894595,
3.47883870956,
3.21716129018595,
2.9554838708,
2.69380645142595,
2.43212903204,
2.17045161266595,
1.90877419328,
1.64709677390595,
1.38541935452,
1.12374193514595,
0.86206451576,
0.600387096385949,
0.338709677,
0.362709677000546,
0.386709677,
0.410709677000546,
0.434709677,
0.458709677000546,
0.482709677,
0.506709677000546,
0.530709677,
0.554709677000546,
0.578709677,
0.602709677000546,
0.626709677,
0.650709677000546,
0.674709677,
0.698709677000546,
0.722709677,
0.699364054877423,
0.581327188520429,
0.463290322158068,
0.345253455801074,
0.227216589438712,
0.109179723081718,
-0.008857143280644,
-0.126894009637638,
-0.244930876,
-0.362967742362362,
-0.481004608719356,
-0.599041475081718,
-0.717078341438712,
-0.835115207801074,
-0.953152074158068,
-1.07118894052043,
-1.18922580687742,
-1.01684792668,
-0.699262673207221,
-0.38167741972,
-0.064092166247221,
0.25349308724,
0.571078340712779,
0.8886635942,
1.20624884767278,
1.52383410116,
1.84141935463278,
2.15900460812,
2.47658986159278,
2.79417511508,
3.11176036855278,
3.42934562204,
3.74693087551278,
4.064516129,
4.43544516146843,
4.80637419392,
5.17730322638843,
5.54823225884,
5.91916129130843,
6.29009032376,
6.66101935622843,
7.03194838868,
7.40287742114843,
7.7738064536,
8.14473548606843,
8.51566451852,
8.88659355098843,
9.25752258344,
9.62845161590843,
9.99938064836,
10.3857333366091,
10.8029333363985,
11.2201333362068,
11.6373333359962,
12.0545333358046,
12.4717333355939,
12.8889333354023,
13.3061333351917,
13.723333335,
14.1405333348083,
14.5577333345977,
14.9749333344061,
15.3921333341954,
15.8093333340038,
16.2265333337932,
16.6437333336015,
17.0609333333909,
17.2406666668,
17.3016666669986,
17.3626666672,
17.4236666673986,
17.4846666676,
17.5456666677986,
17.606666668,
17.6676666681986,
17.7286666684,
17.7896666685986,
17.8506666688,
17.9116666689986,
17.9726666692,
18.0336666693986,
18.0946666696,
18.1556666697986,
18.21666667,
18.2737311862013,
18.3307957024,
18.3878602186013,
18.4449247348,
18.5019892510013,
18.5590537672,
18.6161182834013,
18.6731827996,
18.7302473158013,
18.787311832,
18.8443763482013,
18.9014408644,
18.9585053806013,
19.0155698968,
19.0726344130013,
19.1296989292,
19.1805161334008,
19.2188387135999,
19.2571612938006,
19.2954838739997,
19.3338064542004,
19.3721290343994,
19.4104516146002,
19.4487741947992,
19.487096775,
19.5254193552008,
19.5637419353998,
19.6020645156006,
19.6403870957996,
19.6787096760003,
19.7170322561994,
19.7553548364001,
19.7936774165992,
19.6163268788,
19.3311397820065,
19.0459526852,
18.7607655884065,
18.4755784916,
18.1903913948065,
17.905204298,
17.6200172012065,
17.3348301044,
17.0496430076065,
16.7644559108,
16.4792688140065,
16.1940817172,
15.9088946204065,
15.6237075236,
15.3385204268065,
15.05333333,
14.7938752657941,
14.5344172016,
14.2749591373941,
14.0155010732,
13.7560430089941,
13.4965849448,
13.2371268805941,
12.9776688164,
12.7182107521941,
12.458752688,
12.1992946237941,
11.9398365596,
11.6803784953941,
11.4209204312,
11.1614623669941,
10.9020043028,
10.5324516147871,
9.94270967920215,
9.35296774359035,
8.76322580800536,
8.17348387239356,
7.58374193680858,
6.99400000119678,
6.4042580656118,
5.81451613,
5.2247741943882,
4.63503225880322,
4.04529032319142,
3.45554838760644,
2.86580645199464,
2.27606451640965,
1.68632258079785,
1.09658064521287,
0.90387096776,
0.909677419399868,
0.91548387104,
0.921290322679868,
0.92709677432,
0.932903225959868,
0.9387096776,
0.944516129239868,
0.95032258088,
0.956129032519868,
0.96193548416,
0.967741935799868,
0.97354838744,
0.979354839079868,
0.98516129072,
0.990967742359868,
0.996774194,
1.12819354880299,
1.2596129036,
1.39103225840299,
1.5224516132,
1.65387096800299,
1.7852903228,
1.91670967760299,
2.0481290324,
2.17954838720299,
2.310967742,
2.44238709680299,
2.5738064516,
2.70522580640299,
2.8366451612,
2.96806451600299,
3.0994838708,
3.13485483851658,
2.97812903208057,
2.82140322563743,
2.66467741920143,
2.50795161275829,
2.35122580632228,
2.19449999987914,
2.03777419344314,
1.881048387,
1.72432258055686,
1.56759677412085,
1.41087096767772,
1.25414516124171,
1.09741935479857,
0.940693548362566,
0.78396774191943,
0.627241935483421,
0.8747096774,
1.32427419348978,
1.7738387096,
2.22340322568978,
2.6729677418,
3.12253225788978,
3.572096774,
4.02166129008978,
4.4712258062,
4.92079032228978,
5.3703548384,
5.81991935448978,
6.2694838706,
6.71904838668978,
7.1686129028,
7.61817741888978,
8.067741935,
8.1820774188826,
8.29641290276,
8.4107483866426,
8.52508387052,
8.6394193544026,
8.75375483828,
8.8680903221626,
8.98242580604,
9.0967612899226,
9.2110967738,
9.3254322576826,
9.43976774156,
9.5541032254426,
9.66843870932,
9.7827741932026,
9.89710967708,
10.0692215051463,
10.356886021559,
10.6445505379847,
10.9322150543974,
11.2198795708231,
11.5075440872358,
11.7952086036616,
12.0828731200742,
12.3705376365,
12.6582021529258,
12.9458666693384,
13.2335311857642,
13.5211957021769,
13.8088602186026,
14.0965247350153,
14.384189251441,
14.6718537678537,
14.9192989292,
15.1466344129948,
15.3739698968,
15.6013053805948,
15.8286408644,
16.0559763481948,
16.283311832,
16.5106473157948,
16.7379827996,
16.9653182833948,
17.1926537672,
17.4199892509948,
17.6473247348,
17.8746602185948,
18.1019957024,
18.3293311861948,
18.55666667,
18.7950086054054,
19.0333505408,
19.2716924762054,
19.5100344116,
19.7483763470054,
19.9867182824,
20.2250602178054,
20.4634021532,
20.7017440886054,
20.940086024,
21.1784279594054,
21.4167698948,
21.6551118302054,
21.8934537656,
22.1317957010054,
22.3701376364,
22.5088387115987,
22.4482580664002,
22.387677421199,
22.3270967760006,
22.2665161307993,
22.2059354856009,
22.1453548403997,
22.0847741952012,
22.02419355,
21.9636129047988,
21.9030322596003,
21.8424516143991,
21.7818709692007,
21.7212903239994,
21.660709678801,
21.6001290335998,
21.5395483884013,
21.3381806464,
21.0664193560062,
20.7946580656,
20.5228967752062,
20.2511354848,
19.9793741944062,
19.707612904,
19.4358516136062,
19.1640903232,
18.8923290328062,
18.6205677424,
18.3488064520062,
18.0770451616,
17.8052838712062,
17.5335225808,
17.2617612904062,
16.99,
16.4335677419073,
15.87713548384,
15.3207032257473,
14.76427096768,
14.2078387095873,
13.65140645152,
13.0949741934273,
12.53854193536,
11.9821096772673,
11.4256774192,
10.8692451611073,
10.31281290304,
9.75638064494735,
9.19994838688,
8.64351612878735,
8.08708387072,
7.69700645135875,
7.63963870944021,
7.58227096751906,
7.52490322560052,
7.46753548367937,
7.41016774176084,
7.35279999983969,
7.29543225792115,
7.238064516,
7.18069677407885,
7.12332903216031,
7.06596129023916,
7.00859354832063,
6.95122580639948,
6.89385806448094,
6.83649032255979,
6.77912258064125,
6.53721290324,
6.2030322581076,
5.86885161296,
5.5346709678276,
5.20049032268,
4.8663096775476,
4.5321290324,
4.1979483872676,
3.86376774212,
3.5295870969876,
3.19540645184,
2.8612258067076,
2.52704516156,
2.1928645164276,
1.85868387128,
1.5245032261476,
1.190322581,
1.08561290353762,
0.98090322608,
0.876193548617619,
0.77148387116,
0.666774193697619,
0.56206451624,
0.457354838777619,
0.35264516132,
0.247935483857619,
0.1432258064,
0.038516128937619,
-0.06619354852,
-0.170903225982381,
-0.27561290344,
-0.380322580902381,
-0.48503225836,
-0.455527650073497,
-0.157594470321084,
0.140338709444878,
0.43827188919729,
0.736205068963252,
1.03413824871566,
1.33207142848163,
1.63000460823404,
1.927937788,
2.22587096776596,
2.52380414751837,
2.82173732728434,
3.11967050703675,
3.41760368680271,
3.71553686655512,
4.01347004632108,
4.3114032260735,
4.40693087584,
4.40125576060013,
4.39558064536,
4.38990553012013,
4.38423041488,
4.37855529964013,
4.3728801844,
4.36720506916013,
4.36152995392,
4.35585483868013,
4.35017972344,
4.34450460820013,
4.33882949296,
4.33315437772013,
4.32747926248,
4.32180414724013,
4.316129032,
4.67156129008808,
5.02699354816,
5.38242580624808,
5.73785806432,
6.09329032240808,
6.44872258048,
6.80415483856808,
7.15958709664,
7.51501935472808,
7.8704516128,
8.22588387088808,
8.58131612896,
8.93674838704808,
9.29218064512,
9.64761290320808,
10.00304516128,
10.3221677420054,
10.5686709679991,
10.815174194004,
11.0616774199978,
11.3081806460027,
11.5546838719964,
11.8011870980013,
12.0476903239951,
12.29419355,
12.5406967760049,
12.7872000019987,
13.0337032280036,
13.2802064539973,
13.5267096800022,
13.773212905996,
14.0197161320009,
14.2662193579946,
14.4501849492,
14.6028817229965,
14.7555784968,
14.9082752705965,
15.0609720444,
15.2136688181965,
15.366365592,
15.5190623657965,
15.6717591396,
15.8244559133965,
15.9771526872,
16.1298494609965,
16.2825462348,
16.4352430085965,
16.5879397824,
16.7406365561965,
16.89333333,
17.2072817172071,
17.5212301044,
17.8351784916071,
18.1491268788,
18.4630752660071,
18.7770236532,
19.0909720404071,
19.4049204276,
19.7188688148071,
20.032817202,
20.3467655892071,
20.6607139764,
20.9746623636071,
21.2886107508,
21.6025591380071,
21.9165075252,
22.0610967725958,
21.8669677404007,
21.6728387081968,
21.4787096760018,
21.2845806437979,
21.0904516116028,
20.8963225793989,
20.7021935472039,
20.508064515,
20.3139354827961,
20.1198064506011,
19.9256774183972,
19.7315483862021,
19.5374193539982,
19.3432903218032,
19.1491612895993,
18.9550322574042,
18.6807096768,
18.3662903220071,
18.0518709672,
17.7374516124071,
17.4230322576,
17.1086129028071,
16.794193548,
16.4797741932071,
16.1653548384,
15.8509354836071,
15.5365161288,
15.2220967740071,
14.9076774192,
14.5932580644071,
14.2788387096,
13.9644193548071,
13.65,
13.4519032259955,
13.253806452,
13.0557096779955,
12.857612904,
12.6595161299955,
12.461419356,
12.2633225819955,
12.065225808,
11.8671290339955,
11.66903226,
11.4709354859955,
11.272838712,
11.0747419379955,
10.876645164,
10.6785483899955,
10.480451616,
10.1936860246499,
9.72958279864169,
9.2654795726124,
8.80137634660422,
8.33727312057493,
7.87316989456675,
7.40906666853747,
6.94496344252929,
6.4808602165,
6.01675699047071,
5.55265376446253,
5.08855053843325,
4.62444731242507,
4.16034408639578,
3.6962408603876,
3.23213763435831,
2.76803440835013,
2.47512258032,
2.26780645130471,
2.06049032228,
1.85317419326471,
1.64585806424,
1.43854193522471,
1.2312258062,
1.02390967718471,
0.81659354816,
0.609277419144714,
0.40196129012,
0.194645161104714,
-0.01267096792,
-0.219987096935286,
-0.42730322596,
-0.634619354975286,
-0.841935484,
-0.956516129142605,
-1.07109677428,
-1.18567741942261,
-1.30025806456,
-1.41483870970261,
-1.52941935484,
-1.64399999998261,
-1.75858064512,
-1.87316129026261,
-1.9877419354,
-2.10232258054261,
-2.21690322568,
-2.33148387082261,
-2.44606451596,
-2.56064516110261,
-2.67522580624,
-2.77575305874158,
-2.84817352595974,
-2.92059399318119,
-2.99301446039934,
-3.06543492762079,
-3.13785539483895,
-3.2102758620604,
-3.28269632927855,
-3.3551167965,
-3.42753726372145,
-3.49995773093961,
-3.57237819816105,
-3.64479866537921,
-3.71721913260066,
-3.78963959981881,
-3.86206006704026,
-3.93448053425842,
-3.75666295916,
-3.45372636290689,
-3.15078976664,
-2.84785317038689,
-2.54491657412,
-2.24197997786689,
-1.9390433816,
-1.63610678534689,
-1.33317018908,
-1.03023359282689,
-0.72729699656,
-0.424360400306889,
-0.12142380404,
0.181512792213112,
0.48444938848,
0.787385984733111,
1.090322581,
1.60650322613174,
2.12268387124,
2.63886451637174,
3.15504516148,
3.67122580661174,
4.18740645172,
4.70358709685174,
5.21976774196,
5.73594838709174,
6.2521290322,
6.76830967733174,
7.28449032244,
7.80067096757174,
8.31685161268,
8.83303225781174,
9.34921290292,
9.81120860174772,
10.1648344079587,
10.5184602141858,
10.8720860203968,
11.2257118266239,
11.5793376328349,
11.9329634390619,
12.2865892452729,
12.6402150515,
12.9938408577271,
13.3474666639381,
13.7010924701651,
14.0547182763761,
14.4083440826032,
14.7619698888142,
15.1155956950413,
15.4692215012523,
15.7168128992,
15.9113870929956,
16.1059612868,
16.3005354805956,
16.4951096744,
16.6896838681956,
16.884258062,
17.0788322557956,
17.2734064496,
17.4679806433956,
17.6625548372,
17.8571290309956,
18.0517032248,
18.2462774185956,
18.4408516124,
18.6354258061956,
18.83,
18.7931677417992,
18.7563354836,
18.7195032253992,
18.6826709672,
18.6458387089992,
18.6090064508,
18.5721741925992,
18.5353419344,
18.4985096761992,
18.461677418,
18.4248451597992,
18.3880129016,
18.3511806433992,
18.3143483852,
18.2775161269992,
18.2406838688,
18.2238064494005,
18.2468387075999,
18.2698709658004,
18.2929032239998,
18.3159354822003,
18.3389677403997,
18.3619999986001,
18.3850322567995,
18.408064515,
18.4310967732005,
18.4541290313999,
18.4771612896003,
18.5001935477997,
18.5232258060002,
18.5462580641996,
18.5692903224001,
18.5923225805995,
18.3345333332,
17.936333333009,
17.5381333328,
17.1399333326091,
16.7417333324,
16.3435333322091,
15.9453333320001,
15.5471333318091,
15.1489333316,
14.750733331409,
14.3525333312,
13.9543333310091,
13.5561333308,
13.1579333306091,
12.7597333304001,
12.3615333302091,
11.96333333,
11.8608236527977,
11.7583139756,
11.6558042983977,
11.5532946212,
11.4507849439977,
11.3482752668,
11.2457655895977,
11.1432559124,
11.0407462351977,
10.938236558,
10.8357268807976,
10.7332172036,
10.6307075263977,
10.5281978492,
10.4256881719977,
10.3231784948,
10.1760752691348,
9.9397849465608,
9.70349462397608,
9.46720430140213,
9.23091397881741,
8.99462365624346,
8.75833333365873,
8.52204301108472,
8.2857526885,
8.04946236591528,
7.81317204334127,
7.57688172075654,
7.34059139818259,
7.10430107559787,
6.86801075302392,
6.63172043043919,
6.39543010786519,
5.95754838740006,
5.41887096801224,
4.88019354859999,
4.3415161292123,
3.80283870980005,
3.26416129041236,
2.72548387100011,
2.18680645161228,
1.64812903220003,
1.10945161281221,
0.570774193399957,
0.032096774012269,
-0.506580645399984,
-1.04525806478767,
-1.58393548419992,
-2.12261290358775,
-2.661290323,
-2.67638709718034,
-2.69148387136,
-2.70658064554035,
-2.72167741972,
-2.73677419390034,
-2.75187096808,
-2.76696774226034,
-2.78206451644,
-2.79716129062034,
-2.8122580648,
-2.82735483898035,
-2.84245161316,
-2.85754838734034,
-2.87264516152,
-2.88774193570034,
-2.90283870988,
-2.80093087575263,
-2.46501382504115,
-2.12909677431443,
-1.79317972360303,
-1.45726267287631,
-1.12134562216491,
-0.785428571438199,
-0.449511520726716,
-0.11359447,
0.222322580726717,
0.558239631438199,
0.894156682164915,
1.23007373287631,
1.56599078360303,
1.90190783431443,
2.23782488504115,
2.57374193575263,
2.78254377907998,
2.9277880186967,
3.07303225832,
3.21827649793668,
3.36352073755999,
3.50876497717667,
3.65400921679997,
3.79925345641669,
3.94449769603999,
4.08974193565671,
4.23498617528001,
4.38023041489669,
4.52547465452,
4.67071889413668,
4.81596313375998,
4.9612073733767,
5.106451613,
5.22646451624273,
5.34647741948002,
5.46649032272275,
5.58650322596,
5.70651612920273,
5.82652903243999,
5.94654193568272,
6.06655483892001,
6.18656774216274,
6.30658064540002,
6.42659354864275,
6.54660645188001,
6.66661935512274,
6.78663225836,
6.90664516160273,
7.02665806484001,
7.27433978527104,
7.77735914003829,
8.28037849482834,
8.78339784959546,
9.28641720438552,
9.78943655915264,
10.2924559139427,
10.7954752687099,
11.2984946235,
11.8015139782901,
12.3045333330573,
12.8075526878474,
13.3105720426145,
13.8135913974045,
14.3166107521717,
14.8196301069617,
15.322649461729,
15.61124301,
15.7926236549959,
15.9740043,
16.1553849449959,
16.33676559,
16.5181462349958,
16.69952688,
16.8809075249959,
17.06228817,
17.2436688149959,
17.42504946,
17.6064301049959,
17.78781075,
17.9691913949958,
18.15057204,
18.3319526849959,
18.51333333,
18.5456301042007,
18.5779268784,
18.6102236526007,
18.6425204268,
18.6748172010007,
18.7071139752,
18.7394107494007,
18.7717075236,
18.8040042978007,
18.836301072,
18.8685978462007,
18.9008946204,
18.9331913946007,
18.9654881688,
18.9977849430007,
19.0300817172,
19.0616774162007,
19.0918709647999,
19.1220645134005,
19.1522580619997,
19.1824516106003,
19.2126451591996,
19.2428387078002,
19.2730322563994,
19.303225805,
19.3334193536006,
19.3636129021998,
19.3938064508004,
19.4239999993997,
19.4541935480003,
19.4843870965995,
19.5145806452001,
19.5447741937993,
19.3679784948,
19.0876881720064,
18.8073978492,
18.5271075264064,
18.2468172036,
17.9665268808064,
17.6862365580001,
17.4059462352064,
17.1256559124,
16.8453655896064,
16.5650752668,
16.2847849440064,
16.0044946212,
15.7242042984064,
15.4439139756,
15.1636236528064,
14.88333333,
14.4269784914896,
13.9706236529999,
13.5142688144896,
13.057913976,
12.6015591374896,
12.145204299,
11.6888494604897,
11.232494622,
10.7761397834896,
10.3197849449999,
9.86343010648953,
9.40707526799996,
8.95072042948958,
8.49436559100001,
8.03801075248963,
7.58165591399995,
7.22227096789637,
7.05682580660056,
6.89138064529726,
6.72593548400149,
6.56049032269818,
6.39504516140242,
6.22960000009911,
6.06415483880331,
5.8987096775,
5.73326451619669,
5.56781935490089,
5.40237419359758,
5.23692903230181,
5.07148387099851,
4.90603870970274,
4.74059354839944,
4.57514838710363,
4.40126451612002,
4.22316129030405,
4.04505806448,
3.86695483866407,
3.68885161284002,
3.51074838702409,
3.33264516120004,
3.15454193538406,
2.97643870956001,
2.79833548374404,
2.62023225791999,
2.44212903210406,
2.26402580628001,
2.08592258046408,
1.90781935464002,
1.72971612882405,
1.551612903,
1.50864516107902,
1.46567741915999,
1.42270967723902,
1.37974193532,
1.33677419339902,
1.29380645148,
1.25083870955903,
1.20787096764,
1.16490322571902,
1.12193548379999,
1.07896774187901,
1.03599999996,
0.993032258039019,
0.950064516120001,
0.907096774199024,
0.864129032279995,
0.909774193584891,
1.13264516131924,
1.3555161290637,
1.57838709679799,
1.80125806454245,
2.02412903227674,
2.24700000002119,
2.46987096775554,
2.6927419355,
2.91561290324446,
3.1384838709788,
3.36135483872326,
3.58422580645755,
3.80709677420201,
4.0299677419363,
4.25283870968076,
4.47570967741511,
4.52464516128,
4.48661290320086,
4.44858064512,
4.41054838704087,
4.37251612896,
4.33448387088087,
4.29645161280001,
4.25841935472087,
4.22038709664,
4.18235483856086,
4.14432258048,
4.10629032240087,
4.06825806432,
4.03022580624087,
3.99219354816001,
3.95416129008087,
3.916129032,
4.35076129008989,
4.78539354816006,
5.22002580624995,
5.65465806432001,
6.0892903224099,
6.52392258047997,
6.95855483856985,
7.39318709664003,
7.82781935472991,
8.26245161280009,
8.69708387088997,
9.13171612896004,
9.56634838704992,
10.00098064512,
10.4356129032099,
10.8702451612801,
11.2349290322049,
11.4597161287992,
11.6845032254037,
11.909290321998,
12.1340774186025,
12.3588645151967,
12.5836516118012,
12.8084387083955,
13.033225805,
13.2580129016045,
13.4827999981988,
13.7075870948033,
13.9323741913975,
14.157161288002,
14.3819483845963,
14.6067354812008,
14.8315225777951,
15.0745935456,
15.3268064489943,
15.5790193524,
15.8312322557942,
16.0834451592,
16.3356580625942,
16.587870966,
16.8400838693942,
17.0922967728,
17.3445096761943,
17.5967225796,
17.8489354829943,
18.1011483864,
18.3533612897942,
18.6055741932,
18.8577870965943,
19.11,
19.1701741938014,
19.2303483876,
19.2905225814014,
19.3506967752,
19.4108709690014,
19.4710451628,
19.5312193566014,
19.5913935504,
19.6515677442014,
19.711741938,
19.7719161318014,
19.8320903256,
19.8922645194014,
19.9524387132,
20.0126129070014,
20.0727871008,
20.1224516170006,
20.1510967779999,
20.1797419390005,
20.2083870999997,
20.2370322610003,
20.2656774219996,
20.2943225830002,
20.3229677439994,
20.351612905,
20.3802580660006,
20.4089032269998,
20.4375483880004,
20.4661935489997,
20.4948387100003,
20.5234838709995,
20.5521290320001,
20.5807741929994,
20.3345763436,
19.9509569890087,
19.5673376344,
19.1837182798088,
18.8000989252,
18.4164795706088,
18.0328602160001,
17.6492408614087,
17.2656215068,
16.8820021522087,
16.4983827976,
16.1147634430087,
15.7311440884,
15.3475247338088,
14.9639053792001,
14.5802860246087,
14.19666667,
13.9667376375948,
13.7368086052,
13.5068795727947,
13.2769505404,
13.0470215079948,
12.8170924756,
12.5871634431948,
12.3572344108,
12.1273053783948,
11.897376346,
11.6674473135947,
11.4375182812,
11.2075892487948,
10.9776602164,
10.7477311839948,
10.5178021516,
10.2044258073895,
9.72415483960164,
9.24388387179203,
8.76361290400433,
8.28334193619473,
7.80307096840703,
7.32280000059742,
6.8425290328096,
6.362258065,
5.8819870971904,
5.40171612940258,
4.92144516159297,
4.44117419380527,
3.96090322599567,
3.48063225820797,
3.00036129039836,
2.52009032261054,
2.16701935484003,
1.87754838710658,
1.58807741935999,
1.29860645162661,
1.00913548388003,
0.719664516146642,
0.430193548400058,
0.140722580666601,
-0.148748387079983,
-0.438219354813439,
-0.727690322560023,
-1.01716129029341,
-1.30663225803999,
-1.59610322577338,
-1.88557419351996,
-2.17504516125342,
-2.464516129,
-2.3723870967379,
-2.28025806447999,
-2.18812903221789,
-2.09599999996,
-2.0038709676979,
-1.91174193544001,
-1.81961290317791,
-1.72748387091999,
-1.6353548386579,
-1.54322580639998,
-1.45109677413789,
-1.35896774187999,
-1.2668387096179,
-1.17470967736,
-1.08258064509791,
-0.990451612839989,
-0.906308755698504,
-0.838138248800232,
-0.769967741898869,
-0.701797235000615,
-0.633626728099252,
-0.565456221200997,
-0.497285714299634,
-0.429115207401363,
-0.3609447005,
-0.292774193598637,
-0.224603686700366,
-0.156433179799003,
-0.088262672900748,
-0.020092165999385,
0.048078340898869,
0.116248847800232,
0.184419354698504,
0.458986174959955,
0.836751151891415,
1.21451612884001,
1.59228110577137,
1.97004608271997,
2.34781105965133,
2.72557603659992,
3.10334101353139,
3.48110599047998,
3.85887096741144,
4.23663594436003,
4.6144009212914,
4.99216589823999,
5.36993087517136,
5.74769585211995,
6.12546082905141,
6.503225806,
6.78243225784635,
7.06163870968004,
7.34084516152639,
7.62005161336001,
7.89925806520636,
8.17846451703998,
8.45767096888633,
8.73687742072002,
9.01608387256637,
9.29529032440005,
9.57449677624641,
9.85370322808003,
10.1329096799264,
10.41211613176,
10.6913225836063,
10.97052903544,
11.2418559172056,
11.4974236587991,
11.7529914004042,
12.0085591419977,
12.2641268836028,
12.5196946251963,
12.7752623668014,
13.0308301083949,
13.28639785,
13.5419655916051,
13.7975333331986,
14.0531010748037,
14.3086688163972,
14.5642365580023,
14.8198042995958,
15.0753720412009,
15.3309397827944,
15.5337505356,
15.710182793996,
15.8866150524,
16.063047310796,
16.2394795692,
16.415911827596,
16.592344086,
16.768776344396,
16.9452086028,
17.121640861196,
17.2980731196,
17.474505377996,
17.6509376364,
17.827369894796,
18.0038021532,
18.180234411596,
18.35666667,
18.5077182828034,
18.6587698956,
18.8098215084035,
18.9608731212,
19.1119247340034,
19.2629763468,
19.4140279596034,
19.5650795724,
19.7161311852034,
19.867182798,
20.0182344108035,
20.1692860236,
20.3203376364035,
20.4713892492,
20.6224408620034,
20.7734924748,
20.8341935499974,
20.7141935500004,
20.594193549998,
20.4741935500011,
20.3541935499987,
20.2341935500018,
20.1141935499994,
19.9941935500024,
19.87419355,
19.7541935499976,
19.6341935500006,
19.5141935499982,
19.3941935500013,
19.2741935499989,
19.154193550002,
19.0341935499996,
18.9141935500026,
18.8054924748,
18.7024408620023,
18.5993892492,
18.4963376364024,
18.3932860236,
18.2902344108024,
18.187182798,
18.0841311852024,
17.9810795724,
17.8780279596023,
17.7749763468,
17.6719247340023,
17.5688731212,
17.4658215084024,
17.3627698956,
17.2597182828023,
17.15666667,
16.7398473149905,
16.3230279599999,
15.9062086049905,
15.48938925,
15.0725698949905,
14.65575054,
14.2389311849906,
13.82211183,
13.4052924749905,
12.9884731199999,
12.5716537649904,
12.15483441,
11.7380150549905,
11.3211957,
10.9043763449905,
10.48755699,
10.0657505382505,
9.63396989304147,
9.20218924781284,
8.7704086026039,
8.33862795737526,
7.90684731216632,
7.47506666693768,
7.04328602172863,
6.6115053765,
6.17972473127137,
5.74794408606232,
5.31616344083368,
4.88438279562474,
4.45260215039611,
4.02082150518716,
3.58904085995853,
3.15726021474948,
2.89731612872002,
2.72329032230395,
2.54926451588,
2.37523870946397,
2.20121290304002,
2.02718709662399,
1.85316129020003,
1.67913548378397,
1.50510967736001,
1.33108387094394,
1.15705806451999,
0.983032258103963,
0.809006451680005,
0.634980645263982,
0.460954838840024,
0.286929032423958,
0.112903226,
-0.047741935303654,
-0.208387096600022,
-0.369032257903676,
-0.529677419200005,
-0.690322580503659,
-0.850967741799987,
-1.01161290310364,
-1.17225806440001,
-1.33290322570366,
-1.49354838700003,
-1.65419354830369,
-1.81483870960001,
-1.97548387090367,
-2.1361290322,
-2.29677419350365,
-2.45741935480002,
-2.44122580641188,
-2.07135483868126,
-1.70148387093387,
-1.33161290320334,
-0.961741935455941,
-0.591870967725412,
-0.221999999978016,
0.147870967752605,
0.5177419355,
0.887612903247395,
1.25748387097802,
1.62735483872541,
1.99722580645594,
2.36709677420334,
2.73696774193387,
3.10683870968126,
3.47670967741188,
3.70361290323998,
3.85903225809647,
4.01445161296,
4.16987096781645,
4.32529032267999,
4.48070967753643,
4.63612903239997,
4.79154838725646,
4.94696774211999,
5.10238709697648,
5.25780645184001,
5.41322580669646,
5.56864516156,
5.72406451641644,
5.87948387127998,
6.03490322613647,
6.190322581,
6.61430322634964,
7.03828387168006,
7.4622645170297,
7.88624516236001,
8.31022580770966,
8.73420645303997,
9.15818709838961,
9.58216774372002,
10.0061483890697,
10.4301290344001,
10.8541096797497,
11.27809032508,
11.7020709704297,
12.12605161576,
12.5500322611096,
12.9740129064401,
13.3280494656047,
13.5421978523993,
13.7563462392036,
13.9704946259981,
14.1846430128024,
14.3987913995969,
14.6129397864011,
14.8270881731957,
15.04123656,
15.2553849468043,
15.4695333335989,
15.6836817204031,
15.8978301071977,
16.1119784940019,
16.3261268807965,
16.5402752676007,
16.7544236543953,
16.9597075252,
17.1605591379954,
17.3614107508,
17.5622623635954,
17.7631139764,
17.9639655891954,
18.164817202,
18.3656688147954,
18.5665204276,
18.7673720403954,
18.9682236532,
19.1690752659954,
19.3699268788,
19.5707784915954,
19.7716301044,
19.9724817171954,
20.17333333,
20.1027397815984,
20.0321462332,
19.9615526847984,
19.8909591364,
19.8203655879984,
19.7497720396,
19.6791784911984,
19.6085849428,
19.5379913943984,
19.467397846,
19.3968042975984,
19.3262107492,
19.2556172007984,
19.1850236524,
19.1144301039984,
19.0438365556,
19.0468387062033,
19.1970322547995,
19.3472258034025,
19.4974193519986,
19.6476129006016,
19.7978064491978,
19.9479999978008,
20.098193546397,
20.248387095,
20.398580643603,
20.5487741921992,
20.6989677408022,
20.8491612893984,
20.9993548380014,
21.1495483865975,
21.2997419352005,
21.4499354837967,
21.2370666668,
20.842666667009,
20.4482666672,
20.053866667409,
19.6594666676,
19.265066667809,
18.8706666680001,
18.476266668209,
18.0818666684,
17.6874666686089,
17.2930666688,
16.898666669009,
16.5042666692,
16.109866669409,
15.7154666696001,
15.321066669809,
14.92666667,
14.7682924759964,
14.609918282,
14.4515440879964,
14.293169894,
14.1347956999964,
13.976421506,
13.8180473119964,
13.659673118,
13.5012989239964,
13.34292473,
13.1845505359964,
13.026176342,
12.8678021479964,
12.709427954,
12.5510537599964,
12.392679566,
12.1984881679342,
11.9326623617609,
11.6668365555756,
11.4010107494024,
11.1351849432171,
10.8693591370439,
10.6035333308586,
10.3377075246853,
10.0718817185,
9.80605591231468,
9.54023010614143,
9.27440429995611,
9.00857849378292,
8.7427526875976,
8.47692688142441,
8.21110107523909,
7.94527526906583,
7.61427096808004,
7.25067741970826,
6.88708387131999,
6.5234903229483,
6.15989677456003,
5.79630322618834,
5.43270967780007,
5.06911612942829,
4.70552258104002,
4.34192903266824,
3.97833548427997,
3.61474193590828,
3.25114838752001,
2.88755483914832,
2.52396129076005,
2.16036774238827,
1.796774194,
1.71258064557808,
1.62838709715999,
1.54419354873807,
1.46000000032,
1.37580645189808,
1.29161290348001,
1.20741935505809,
1.12322580663999,
1.03903225821808,
0.954838709799983,
0.870645161378068,
0.786451612959992,
0.702258064538078,
0.618064516120002,
0.533870967698087,
0.44967741927999,
0.45946313354434,
0.657207373159326,
0.85495161278328,
1.05269585239822,
1.25044009202217,
1.44818433163711,
1.64592857126106,
1.84367281087605,
2.0414170505,
2.23916129012395,
2.43690552973894,
2.63464976936289,
2.83239400897783,
3.03013824860178,
3.22788248821672,
3.42562672784067,
3.62337096745566,
3.84674654351997,
4.08293778779463,
4.31912903208,
4.55532027635461,
4.79151152063998,
5.02770276491458,
5.26389400919995,
5.50008525347461,
5.73627649775999,
5.97246774203465,
6.20865898632002,
6.44485023059462,
6.68104147487999,
6.91723271915459,
7.15342396343997,
7.38961520771463,
7.625806452,
7.73525806488249,
7.84470967776002,
7.9541612906425,
8.06361290352,
8.17306451640249,
8.28251612927999,
8.39196774216248,
8.50141935504001,
8.6108709679225,
8.72032258080002,
8.82977419368251,
8.93922580656001,
9.0486774194425,
9.15812903232,
9.26758064520249,
9.37703225808001,
9.59764516120972,
10.0405806447985,
10.4835161284073,
10.926451611996,
11.3693870956049,
11.8123225791935,
12.2552580628024,
12.6981935463911,
13.14112903,
13.5840645136089,
14.0269999971976,
14.4699354808065,
14.9128709643951,
15.355806448004,
15.7987419315927,
16.2416774152015,
16.6846128987903,
16.8589677376,
16.8990322539991,
16.9390967704,
16.9791612867991,
17.0192258032,
17.0592903195991,
17.099354836,
17.1394193523991,
17.1794838688,
17.2195483851991,
17.2596129016,
17.2996774179991,
17.3397419344,
17.3798064507991,
17.4198709672,
17.4599354835991,
17.5,
17.6920000000044,
17.884,
18.0760000000044,
18.268,
18.4600000000044,
18.652,
18.8440000000043,
19.036,
19.2280000000044,
19.42,
19.6120000000044,
19.804,
19.9960000000044,
20.188,
20.3800000000044,
20.572,
20.7213548388014,
20.7854193551998,
20.8494838716011,
20.9135483879994,
20.9776129044007,
21.0416774207991,
21.1057419372003,
21.1698064535987,
21.23387097,
21.2979354864013,
21.3620000027997,
21.4260645192009,
21.4901290355993,
21.5541935520006,
21.6182580683989,
21.6823225848002,
21.7463871011986,
21.4446322624001,
20.959967746011,
20.4753032296,
19.9906387132111,
19.5059741968,
19.0213096804111,
18.5366451640001,
18.0519806476111,
17.5673161312,
17.082651614811,
16.5979870984,
16.113322582011,
15.6286580656,
15.1439935492111,
14.6593290328001,
14.174664516411,
13.69,
13.6604064513993,
13.6308129028,
13.6012193541993,
13.5716258056,
13.5420322569993,
13.5124387084,
13.4828451597993,
13.4532516112,
13.4236580625993,
13.394064514,
13.3644709653993,
13.3348774168,
13.3052838681993,
13.2756903196,
13.2460967709993,
13.2165032224,
13.0067053728475,
12.4364989214419,
11.8662924700105,
11.2960860186051,
10.7258795671737,
10.1556731157683,
9.58546666433694,
9.0152602129314,
8.4450537615,
7.8748473100686,
7.30464085866306,
6.73443440723166,
6.16422795582626,
5.59402150439486,
5.02381505298946,
4.45360860155806,
3.88340215015251,
3.41359999968005,
2.99399999970954,
2.57439999971999,
2.15479999974958,
1.73519999976004,
1.31559999978963,
0.895999999800084,
0.476399999829569,
0.056799999840025,
-0.362800000130489,
-0.782400000120034,
-1.20200000009044,
-1.62160000007999,
-2.0412000000504,
-2.46080000003994,
-2.88040000001046,
-3.3,
-3.09619354835536,
-2.89238709671997,
-2.68858064507534,
-2.48477419343999,
-2.28096774179536,
-2.07716129016002,
-1.87335483851538,
-1.66954838687999,
-1.46574193523535,
-1.26193548359996,
-1.05812903195532,
-0.854322580319982,
-0.650516128675346,
-0.446709677040004,
-0.242903225395369,
-0.039096773759975,
0.188695852986052,
0.46446082991906,
0.740225806864574,
1.01599078379751,
1.29175576074303,
1.56752073767597,
1.84328571462148,
2.11905069155449,
2.3948156685,
2.67058064544551,
2.94634562237852,
3.22211059932404,
3.49787557625697,
3.77364055320249,
4.04940553013543,
4.32517050708094,
4.60093548401395,
4.77236866371999,
4.89163594479729,
5.01090322588,
5.13017050695728,
5.24943778803999,
5.36870506911726,
5.48797235019998,
5.60723963127728,
5.72650691235999,
5.8457741934373,
5.96504147452001,
6.08430875559731,
6.20357603667997,
6.32284331775727,
6.44211059883998,
6.56137787991729,
6.680645161,
6.86640645136423,
7.05216774172003,
7.23792903208425,
7.42369032244005,
7.60945161280418,
7.79521290315999,
7.98097419352421,
8.16673548388001,
8.35249677424424,
8.53825806460004,
8.72401935496426,
8.90978064531997,
9.09554193568429,
9.28130322604,
9.46706451640422,
9.65282580676002,
9.92571397886981,
10.3728559144385,
10.8199978500274,
11.2671397855961,
11.7142817211848,
12.1614236567535,
12.6085655923424,
13.0557075279111,
13.5028494635,
13.9499913990889,
14.3971333346576,
14.8442752702465,
15.2914172058152,
15.7385591414039,
16.1857010769726,
16.6328430125615,
17.0799849481302,
17.3574709696,
17.5501290339956,
17.7427870984,
17.9354451627956,
18.1281032272,
18.3207612915956,
18.513419356,
18.7060774203956,
18.8987354848,
19.0913935491956,
19.2840516136,
19.4767096779957,
19.6693677423999,
19.8620258067956,
20.0546838712,
20.2473419355956,
20.44,
20.5173419356018,
20.5946838712,
20.6720258068018,
20.7493677424,
20.8267096780017,
20.9040516136,
20.9813935492018,
21.0587354848,
21.1360774204018,
21.213419356,
21.2907612916018,
21.3681032272,
21.4454451628018,
21.5227870984,
21.6001290340018,
21.6774709696,
21.7031612921983,
21.6255483888003,
21.5479354853987,
21.4703225820007,
21.3927096785992,
21.3150967752011,
21.2374838717996,
21.1598709684016,
21.082258065,
21.0046451615984,
20.9270322582004,
20.8494193547989,
20.7718064514008,
20.6941935479993,
20.6165806446013,
20.5389677411997,
20.4613548378017,
20.2012645152,
19.849935483008,
19.4986064508,
19.1472774186081,
18.7959483863999,
18.4446193542081,
18.0932903220001,
17.741961289808,
17.3906322576,
17.039303225408,
16.6879741932,
16.3366451610079,
15.9853161288001,
15.633987096608,
15.2826580644,
14.931329032208,
14.58,
14.2914580645134,
14.00291612904,
13.7143741935534,
13.4258322580799,
13.1372903225935,
12.84874838712,
12.5602064516335,
12.27166451616,
11.9831225806734,
11.6945806451999,
11.4060387097134,
11.11749677424,
10.8289548387533,
10.54041290328,
10.2518709677934,
9.96332903231997,
9.72881505381722,
9.60235698928043,
9.4758989247379,
9.34944086020111,
9.22298279565864,
9.09652473112185,
8.97006666657932,
8.84360860204253,
8.7171505375,
8.59069247295747,
8.46423440842068,
8.33777634387815,
8.21131827934136,
8.08486021479889,
7.9584021502621,
7.83194408571957,
7.70548602118278,
7.33576774160006,
6.84441935451117,
6.35307096739999,
5.86172258031134,
5.37037419319992,
4.87902580611127,
4.3876774190001,
3.8963290319112,
3.40498064480003,
2.91363225771114,
2.42228387059996,
1.93093548351107,
1.43958709640014,
0.948238709311244,
0.456890322200068,
-0.034458064888824,
-0.525806452,
-0.641741935842637,
-0.757677419680016,
-0.873612903522653,
-0.989548387360032,
-1.10548387120261,
-1.22141935503999,
-1.33735483888263,
-1.45329032272001,
-1.56922580656264,
-1.68516129040002,
-1.80109677424266,
-1.91703225807998,
-2.03296774192268,
-2.14890322576,
-2.26483870960263,
-2.38077419344001,
-2.45233179709962,
-2.43513364040006,
-2.41793548369971,
-2.40073732700015,
-2.38353917029982,
-2.36634101360025,
-2.34914285689991,
-2.33194470020034,
-2.3147465435,
-2.29754838679966,
-2.28035023010009,
-2.26315207339975,
-2.24595391670018,
-2.22875575999985,
-2.21155760330029,
-2.19435944659994,
-2.17716128990038,
-1.85941013784006,
-1.39138248811064,
-0.923354838359991,
-0.455327188630804,
0.012700461120075,
0.480728110849261,
0.948755760599906,
1.41678341032933,
1.88481106007997,
2.35283870980939,
2.82086635956004,
3.28889400928946,
3.75692165903987,
4.22494930876929,
4.69297695851993,
5.16100460824935,
5.629032258,
5.85249032250508,
6.07594838700003,
6.29940645150511,
6.52286451600006,
6.74632258050503,
6.96978064499998,
7.19323870950506,
7.41669677400001,
7.6401548385051,
7.86361290300004,
8.08707096750513,
8.31052903199996,
8.53398709650516,
8.757445161,
8.98090322550508,
9.20436129000003,
9.51878279535089,
10.0151311823583,
10.5114795693882,
11.0078279563956,
11.5041763434253,
12.0005247304327,
12.4968731174627,
12.9932215044701,
13.4895698915,
13.9859182785299,
14.4822666655373,
14.9786150525673,
15.4749634395747,
15.9713118266044,
16.4676602136118,
16.9640086006417,
17.4603569876491,
17.801174192,
18.064225804994,
18.327277418,
18.5903290309939,
18.853380644,
19.116432256994,
19.3794838699999,
19.642535482994,
19.905587096,
20.168638708994,
20.431690322,
20.6947419349941,
20.9577935479999,
21.220845160994,
21.483896774,
21.746948386994,
22.01,
21.9824967739994,
21.954993548,
21.9274903219994,
21.899987096,
21.8724838699994,
21.844980644,
21.8174774179994,
21.789974192,
21.7624709659994,
21.73496774,
21.7074645139994,
21.679961288,
21.6524580619994,
21.624954836,
21.5974516099994,
21.569948384,
21.5899354808025,
21.7049032231996,
21.8198709656019,
21.934838707999,
22.0498064504012,
22.1647741927983,
22.2797419352006,
22.3947096775977,
22.50967742,
22.6246451624023,
22.7396129047994,
22.8545806472017,
22.9695483895988,
23.084516132001,
23.1994838743981,
23.3144516168004,
23.4294193591975,
23.1338322624001,
22.6329677460114,
22.1321032296,
21.6312387132116,
21.1303741967999,
20.6295096804115,
20.1286451640001,
19.6277806476114,
19.1269161312,
18.6260516148114,
18.1251870984,
17.6243225820113,
17.1234580656001,
16.6225935492115,
16.1217290328001,
15.6208645164114,
15.12,
14.6761548386899,
14.2323096773999,
13.7884645160898,
13.3446193547999,
12.90077419349,
12.4569290322,
12.0130838708899,
11.5692387096,
11.1253935482899,
10.6815483869999,
10.2377032256898,
9.79385806440007,
9.35001290308975,
8.90616774180001,
8.46232258048991,
8.01847741919995,
7.69452903209815,
7.61037419340029,
7.5262193546986,
7.44206451600074,
7.3579096772991,
7.27375483860123,
7.18959999989955,
7.10544516120168,
7.0212903225,
6.93713548379832,
6.85298064510045,
6.76882580639877,
6.6846709677009,
6.60051612899926,
6.5163612903014,
6.43220645159971,
6.34805161290185,
6.07132903224004,
5.69832258060848,
5.32531612895999,
4.95230967732861,
4.57930322567994,
4.20629677404856,
3.83329032240007,
3.46028387076851,
3.08727741912002,
2.71427096748845,
2.34126451583997,
1.9682580642084,
1.5952516125601,
1.22224516092854,
0.849238709280052,
0.476232257648484,
0.103225806,
-0.03438709722313,
-0.172000000440019,
-0.309612903663149,
-0.447225806880039,
-0.5848387101031,
-0.722451613319989,
-0.860064516543119,
-0.997677419760008,
-1.13529032298314,
-1.27290322620003,
-1.41051612942316,
-1.54812903263998,
-1.68574193586318,
-1.82335483908,
-1.96096774230313,
-2.09858064552002,
-2.12189543971549,
-1.9166140158807,
-1.7113325920366,
-1.5060511682018,
-1.3007697443578,
-1.095488320523,
-0.890206896678899,
-0.684925472844105,
-0.479644049,
-0.274362625155895,
-0.069081201321101,
0.136200222523004,
0.341481646357798,
0.546763070201801,
0.752044494036595,
0.9573259178807,
1.16260734171549,
1.33366407143998,
1.4876084540965,
1.64155283676,
1.79549721941645,
1.94944160208002,
2.10338598473647,
2.25733036739997,
2.41127475005649,
2.56521913271999,
2.71916351537651,
2.87310789804001,
3.02705228069653,
3.18099666335996,
3.33494104601648,
3.48888542867998,
3.6428298113365,
3.796774194,
4.2143677425695,
4.63196129112006,
5.04955483968956,
5.46714838824012,
5.88474193680941,
6.30233548535997,
6.71992903392946,
7.13752258248002,
7.55511613104952,
7.97270967960008,
8.39030322816958,
8.80789677671993,
9.22549032528964,
9.64308387383999,
10.0606774224095,
10.4782709709601,
10.802436562403,
10.9397462395995,
11.0770559168023,
11.2143655939988,
11.3516752712015,
11.488984948398,
11.6262946256007,
11.7636043027973,
11.90091398,
12.0382236572027,
12.1755333343993,
12.312843011602,
12.4501526887985,
12.5874623660012,
12.7247720431977,
12.8620817204005,
12.999391397597,
13.2138881716,
13.4669784939942,
13.7200688164,
13.9731591387942,
14.2262494612,
14.4793397835942,
14.7324301059999,
14.9855204283942,
15.2386107508,
15.4917010731943,
15.7447913956,
15.9978817179943,
16.2509720403999,
16.5040623627942,
16.7571526852,
17.0102430075942,
17.26333333,
17.3921139754029,
17.5208946208,
17.6496752662029,
17.7784559116,
17.9072365570029,
18.0360172024,
18.1647978478029,
18.2935784932,
18.4223591386029,
18.551139784,
18.679920429403,
18.8087010748,
18.937481720203,
19.0662623656,
19.1950430110029,
19.3238236564,
19.4170967748005,
19.4393548391999,
19.4616129036004,
19.4838709679998,
19.5061290324002,
19.5283870967997,
19.5506451612001,
19.5729032255996,
19.59516129,
19.6174193544004,
19.6396774187999,
19.6619354832003,
19.6841935475998,
19.7064516120002,
19.7287096763996,
19.7509677408001,
19.7732258051995,
19.5902193536,
19.3045806440065,
19.0189419344,
18.7333032248066,
18.4476645152,
18.1620258056066,
17.8763870960001,
17.5907483864065,
17.3051096768,
17.0194709672065,
16.7338322576,
16.4481935480064,
16.1625548384001,
15.8769161288065,
15.5912774192,
15.3056387096065,
15.02,
14.801832258195,
14.5836645164,
14.365496774595,
14.1473290327999,
13.9291612909951,
13.7109935492,
13.4928258073951,
13.2746580656,
13.056490323795,
12.838322582,
12.620154840195,
12.4019870984,
12.183819356595,
11.9656516148,
11.747483872995,
11.5293161312,
11.2504602172512,
10.8502279590414,
10.4499957008134,
10.0497634426035,
9.64953118437571,
9.24929892616586,
8.84906666793785,
8.448834409728,
8.0486021515,
7.648369893272,
7.24813763506215,
6.84790537683414,
6.44767311862429,
6.04744086039649,
5.64720860218664,
5.24697634395863,
4.84674408574878,
4.53344516096003,
4.26361290290613,
3.99378064483999,
3.72394838678623,
3.45411612871996,
3.18428387066619,
2.91445161260005,
2.64461935454615,
2.37478709648002,
2.10495483842612,
1.83512258035998,
1.56529032230608,
1.29545806424008,
1.02562580618617,
0.755793548120037,
0.485961290066137,
0.216129032,
0.175870967479084,
0.135612902959994,
0.095354838439079,
0.055096773919989,
0.014838709399093,
-0.025419355119997,
-0.065677419640912,
-0.105935484160002,
-0.146193548680918,
-0.186451613200008,
-0.226709677720924,
-0.266967742239994,
-0.307225806760929,
-0.347483871279999,
-0.387741935800915,
-0.428000000320005,
-0.503670507223215,
-0.650165898879501,
-0.79666129054243,
-0.943156682198715,
-1.08965207386157,
-1.23614746551786,
-1.38264285718079,
-1.52913824883707,
-1.6756336405,
-1.82212903216293,
-1.96862442381921,
-2.11511981548214,
-2.26161520713843,
-2.40811059880128,
-2.55460599045757,
-2.7011013821205,
-2.84759677377678,
-2.66844239592004,
-2.32646313330777,
-1.98448387067999,
-1.64250460806789,
-1.30052534543995,
-0.958546082827846,
-0.616566820200068,
-0.274587557587799,
0.06739170503998,
0.409370967652249,
0.751350230280027,
1.0933294928923,
1.4353087555199,
1.77728801813217,
2.11926728075995,
2.46124654337222,
2.803225806,
3.28203225745089,
3.76083870888007,
4.23964516033096,
4.71845161176013,
5.19725806321078,
5.67606451463996,
6.15487096609085,
6.63367741752003,
7.11248386897092,
7.59129032040009,
8.07009677185098,
8.54890322327992,
9.02770967473105,
9.50651612615999,
9.98532257761088,
10.4641290290401,
10.8819247280065,
11.177698921999,
11.4734731160049,
11.7692473099974,
12.0650215040032,
12.3607956979957,
12.6565698920016,
12.9523440859941,
13.24811828,
13.5438924740059,
13.8396666679984,
14.1354408620043,
14.4312150559968,
14.7269892500026,
15.0227634439951,
15.318537638001,
15.6143118319935,
15.819320434,
15.9789462399964,
16.138572046,
16.2981978519963,
16.457823658,
16.6174494639963,
16.77707527,
16.9367010759964,
17.096326882,
17.2559526879964,
17.415578494,
17.5752042999964,
17.734830106,
17.8944559119963,
18.054081718,
18.2137075239964,
18.37333333,
18.460094620602,
18.5468559112,
18.633617201802,
18.7203784924,
18.807139783002,
18.8939010736,
18.980662364202,
19.0674236548,
19.154184945402,
19.240946236,
19.327707526602,
19.4144688172,
19.501230107802,
19.5879913984,
19.674752689002,
19.7615139796,
19.7787096785973,
19.6567741944004,
19.534838710198,
19.4129032260011,
19.2909677417987,
19.1690322576018,
19.0470967733993,
18.9251612892024,
18.803225805,
18.6812903207976,
18.5593548366007,
18.4374193523982,
18.3154838682013,
18.1935483839989,
18.071612899802,
17.9496774155996,
17.8277419314027,
17.705479566,
17.5830537600028,
17.460627954,
17.3382021480028,
17.215776342,
17.0933505360028,
16.97092473,
16.8484989240028,
16.726073118,
16.6036473120028,
16.481221506,
16.3587957000028,
16.236369894,
16.1139440880028,
15.991518282,
15.8690924760028,
15.74666667,
15.4260602183927,
15.1054537668,
14.7848473151927,
14.4642408635999,
14.1436344119928,
13.8230279604,
13.5024215087927,
13.1818150572,
12.8612086055927,
12.5406021539999,
12.2199957023926,
11.8993892508001,
11.5787827991926,
11.2581763476,
10.9375698959927,
10.6169634444,
10.2618946271307,
9.83790107856145,
9.41390752997297,
8.98991398140372,
8.56592043281545,
8.1419268842462,
7.71793333565773,
7.29393978708848,
6.8699462385,
6.44595268991152,
6.02195914134227,
5.5979655927538,
5.17397204418455,
4.74997849559628,
4.32598494702703,
3.90199139843855,
3.4779978498693,
3.20655483904002,
3.01138709710443,
2.81621935516,
2.6210516132245,
2.42588387127997,
2.23071612934448,
2.03554838740004,
1.84038064546445,
1.64521290352001,
1.45004516158442,
1.25487741963998,
1.0597096777044,
0.864541935760054,
0.669374193824466,
0.474206451880027,
0.279038709944439,
0.083870968,
-0.163483870705626,
-0.410838709400035,
-0.658193548105661,
-0.905548386800069,
-1.15290322550557,
-1.40025806419998,
-1.64761290290561,
-1.89496774160001,
-2.14232258030564,
-2.38967741900005,
-2.63703225770568,
-2.88438709639996,
-3.13174193510571,
-3.3790967738,
-3.62645161250562,
-3.87380645120003,
-3.96493548345514,
-3.74361290284075,
-3.52229032221633,
-3.30096774160194,
-3.07964516097763,
-2.85832258036324,
-2.63699999973881,
-2.41567741912443,
-2.1943548385,
-1.97303225787558,
-1.75170967726119,
-1.53038709663676,
-1.30906451602237,
-1.08774193539806,
-0.866419354783671,
-0.645096774159246,
-0.423774193544858,
-0.193032258080028,
0.042419354794649,
0.277870967680005,
0.513322580554565,
0.748774193440038,
0.984225806314598,
1.21967741919995,
1.45512903207463,
1.69058064495999,
1.92603225783466,
2.16148387072002,
2.3969354835947,
2.63238709647993,
2.86783870935461,
3.10329032223997,
3.33874193511464,
3.574193548,
4.02874193513034,
4.48329032224006,
4.9378387093704,
5.39238709648013,
5.84693548361024,
6.30148387071996,
6.7560322578503,
7.21058064496003,
7.66512903209037,
8.11967741920009,
8.57422580633043,
9.02877419343993,
9.48332258057049,
9.93787096767999,
10.3924193548103,
10.8469677419201,
11.2174516130044,
11.4198064519993,
11.6221612910034,
11.8245161299982,
12.0268709690022,
12.229225807997,
12.4315806470011,
12.633935485996,
12.836290325,
13.038645164004,
13.2410000029989,
13.443354842003,
13.6457096809978,
13.8480645200018,
14.0504193589966,
14.2527741980007,
14.4551290369956,
14.693277424,
14.9493225849942,
15.205367746,
15.4614129069941,
15.717458068,
15.9735032289941,
16.2295483899999,
16.4855935509942,
16.741638712,
16.9976838729942,
17.253729034,
17.5097741949942,
17.7658193559999,
18.0218645169941,
18.277909678,
18.5339548389942,
18.79,
19.0122129034051,
19.2344258068,
19.4566387102051,
19.6788516136001,
19.901064517005,
20.1232774204,
20.345490323805,
20.5677032272,
20.7899161306051,
21.012129034,
21.2343419374051,
21.4565548408,
21.6787677442051,
21.9009806476,
22.1231935510051,
22.3454064544,
22.3951612931935,
22.100000002801,
21.8048387123951,
21.5096774220026,
21.2145161315968,
20.9193548412043,
20.6241935507984,
20.3290322604059,
20.03387097,
19.7387096795941,
19.4435483892016,
19.1483870987957,
18.8532258084032,
18.5580645179974,
18.2629032276049,
17.967741937199,
17.6725806468065,
17.5401591412,
17.4891075280012,
17.4380559148,
17.3870043016012,
17.3359526884,
17.2849010752012,
17.233849462,
17.1827978488012,
17.1317462356,
17.0806946224012,
17.0296430092,
16.9785913960011,
16.9275397828,
16.8764881696012,
16.8254365564,
16.7743849432012,
16.72333333,
16.4273526849933,
16.13137204,
15.8353913949932,
15.5394107499999,
15.2434301049933,
14.94744946,
14.6514688149933,
14.35548817,
14.0595075249933,
13.7635268799999,
13.4675462349932,
13.17156559,
12.8755849449932,
12.5796043,
12.2836236549933,
11.98764301,
11.6791161283927,
11.3454967736011,
11.0118774187945,
10.6782580640029,
10.3446387091964,
10.0110193544049,
9.67739999959821,
9.34378064480667,
9.01016129,
8.67654193519333,
8.34292258040179,
8.00930322559512,
7.67568387080358,
7.34206451599707,
7.00844516120553,
6.67482580639886,
6.34120645160732,
6.08634838708003,
5.8708709677049,
5.65539354832,
5.43991612894497,
5.22443870955997,
5.00896129018494,
4.79348387080004,
4.57800645142491,
4.36252903204001,
4.14705161266488,
3.93157419327998,
3.71609677390485,
3.50061935452006,
3.28514193514493,
3.06966451576003,
2.8541870963849,
2.638709677,
2.68593548350107,
2.73316129000001,
2.78038709650108,
2.82761290300001,
2.87483870950106,
2.922064516,
2.96929032250107,
3.016516129,
3.06374193550108,
3.11096774200001,
3.15819354850108,
3.20541935499999,
3.25264516150109,
3.299870968,
3.34709677450107,
3.39432258100001,
3.45550460868195,
3.5445990787197,
3.63369354876148,
3.72278801879922,
3.81188248884096,
3.9009769588787,
3.99007142892048,
4.07916589895822,
4.168260369,
4.25735483904178,
4.34644930907952,
4.4355437791213,
4.52463824915904,
4.61373271920078,
4.70282718923852,
4.7919216592803,
4.88101612931804,
5.00822119843998,
5.15448156709668,
5.30074193576,
5.44700230441662,
5.59326267308002,
5.73952304173664,
5.88578341039997,
6.03204377905666,
6.17830414771999,
6.32456451637669,
6.47082488504001,
6.61708525369671,
6.76334562235996,
6.90960599101665,
7.05586635967998,
7.20212672833667,
7.348387097,
7.66648387118723,
7.98458064536004,
8.30267741954728,
8.62077419372009,
8.93887096790717,
9.25696774207997,
9.57506451626721,
9.89316129044002,
10.2112580646273,
10.5293548388001,
10.8474516129873,
11.1655483871599,
11.4836451613473,
11.80174193552,
12.1198387097072,
12.43793548388,
12.7346129032056,
12.9884516127991,
13.2422903224042,
13.4961290319978,
13.7499677416027,
14.0038064511963,
14.2576451608014,
14.5114838703949,
14.76532258,
15.0191612896051,
15.2729999991986,
15.5268387088037,
15.7806774183973,
16.0345161280022,
16.2883548375958,
16.5421935472009,
16.7960322567944,
17.0475526868,
17.2979139769943,
17.5482752672,
17.7986365573942,
18.0489978476,
18.2993591377943,
18.549720428,
18.8000817181943,
19.0504430084,
19.3008042985943,
19.5511655888,
19.8015268789944,
20.0518881691999,
20.3022494593943,
20.5526107496,
20.8029720397943,
21.05333333,
21.1205849432015,
21.1878365564,
21.2550881696015,
21.3223397828,
21.3895913960015,
21.4568430092,
21.5240946224015,
21.5913462356,
21.6585978488015,
21.725849462,
21.7931010752015,
21.8603526884,
21.9276043016016,
21.9948559148,
22.0621075280015,
22.1293591412,
22.1423870983979,
22.0469677436003,
21.9515483887984,
21.8561290340008,
21.760709679199,
21.6652903244014,
21.5698709695995,
21.4744516148019,
21.37903226,
21.2836129051981,
21.1881935504005,
21.0927741955986,
20.997354840801,
20.9019354859992,
20.8065161312016,
20.7110967763997,
20.6156774216021,
20.3102494644,
19.8998172060093,
19.4893849476,
19.0789526892095,
18.6685204307999,
18.2580881724094,
17.8476559140001,
17.4372236556094,
17.0267913972,
16.6163591388093,
16.2059268804,
15.7954946220092,
15.3850623636001,
14.9746301052094,
14.5641978468001,
14.1537655884093,
13.74333333,
13.4699591366338,
13.19658494328,
12.9232107499137,
12.6498365565599,
12.3764623631938,
12.10308816984,
11.8297139764738,
11.55633978312,
11.2829655897538,
11.0095913963999,
10.7362172030337,
10.46284300968,
10.1894688163137,
9.91609462296001,
9.64272042959379,
9.36934623623997,
9.08188817185307,
8.76626236544108,
8.45063655901477,
8.13501075260277,
7.81938494617662,
7.50375913976462,
7.18813333333831,
6.87250752692631,
6.5568817205,
6.24125591407369,
5.92563010766169,
5.61000430123538,
5.29437849482339,
4.97875268839723,
4.66312688198524,
4.34750107555892,
4.03187526914693,
3.75166451644003,
3.48916129060597,
3.22665806475999,
2.96415483892606,
2.70165161307996,
2.43914838724602,
2.17664516140005,
1.91414193556599,
1.65163870972002,
1.38913548388595,
1.12663225803998,
0.864129032205913,
0.601625806360073,
0.339122580526007,
0.076619354680036,
-0.185883871154029,
-0.448387097,
-0.289096774396377,
-0.129806451799978,
0.029483870803645,
0.188774193400044,
0.348064516003588,
0.507354838599987,
0.66664516120361,
0.82593548380001,
0.985225806403632,
1.14451612900003,
1.30380645160365,
1.46309677419997,
1.62238709680368,
1.7816774194,
1.94096774200362,
2.10025806460002,
2.23756396006205,
2.33090100123968,
2.42423804242155,
2.51757508359918,
2.610912124781,
2.70424916595863,
2.7975862071405,
2.89092324831813,
2.9842602895,
3.07759733068187,
3.1709343718595,
3.26427141304137,
3.357608454219,
3.45094549540082,
3.54428253657845,
3.63761957776032,
3.73095661893795,
3.85752169123998,
4.00070077909675,
4.14387986696,
4.2870589548167,
4.43023804268002,
4.57341713053671,
4.71659621839997,
4.85977530625673,
5.00295439411999,
5.14613348197675,
5.28931256984001,
5.43249165769677,
5.57567074555996,
5.71884983341672,
5.86202892127998,
6.00520800913674,
6.148387097,
6.43948387118662,
6.73058064536004,
7.02167741954666,
7.31277419372008,
7.60387096790656,
7.89496774207998,
8.1860645162666,
8.47716129044002,
8.76825806462664,
9.05935483880006,
9.35045161298668,
9.64154838715995,
9.93264516134672,
10.22374193552,
10.5148387097066,
10.80593548388,
11.0992258064065,
11.396903225599,
11.6945806448049,
11.9922580639974,
12.2899354832032,
12.5876129023956,
12.8852903216016,
13.182967740794,
13.48064516,
13.778322579206,
14.0759999983984,
14.3736774176044,
14.6713548367968,
14.9690322560026,
15.2667096751951,
15.564387094401,
15.8620645135935,
16.111105374,
16.3358279549949,
16.560550536,
16.7852731169948,
17.009995698,
17.2347182789948,
17.45944086,
17.6841634409949,
17.908886022,
18.1336086029949,
18.358331184,
18.5830537649949,
18.8077763459999,
19.0324989269949,
19.257221508,
19.4819440889949,
19.70666667,
19.7623957020013,
19.818124734,
19.8738537660013,
19.929582798,
19.9853118300013,
20.041040862,
20.0967698940013,
20.152498926,
20.2082279580013,
20.26395699,
20.3196860220013,
20.375415054,
20.4311440860013,
20.486873118,
20.5426021500013,
20.598331182,
20.6236774183992,
20.5882580636001,
20.5528387087994,
20.5174193540003,
20.4819999991996,
20.4465806444005,
20.4111612895998,
20.3757419348007,
20.34032258,
20.3049032251993,
20.2694838704002,
20.2340645155995,
20.1986451608004,
20.1632258059997,
20.1278064512006,
20.0923870963999,
20.0569677416008,
19.8430881716,
19.5399784940069,
19.2368688164,
18.933759138807,
18.6306494612,
18.327539783607,
18.0244301060001,
17.7213204284069,
17.4182107508,
17.1151010732069,
16.8119913956,
16.5088817180068,
16.2057720404001,
15.9026623628069,
15.5995526852,
15.2964430076069,
14.99333333,
14.7550881691946,
14.5168430084,
14.2785978475945,
14.0403526867999,
13.8021075259946,
13.5638623652,
13.3256172043946,
13.0873720436,
12.8491268827946,
12.610881722,
12.3726365611945,
12.1343914004,
11.8961462395945,
11.6579010788,
11.4196559179946,
11.1814107572,
10.9278623703338,
10.643707531361,
10.3595526923753,
10.0753978534025,
9.79124301441695,
9.50708817544416,
9.22293333645848,
8.93877849748568,
8.6546236585,
8.37046881951432,
8.08631398054152,
7.80215914155584,
7.51800430258305,
7.23384946359751,
6.94969462462471,
6.66553978563903,
6.38138494666624,
6.12849032292003,
5.89122580680539,
5.65396129068,
5.41669677456548,
5.17943225843996,
4.94216774232544,
4.70490322620005,
4.46763871008541,
4.23037419396001,
3.99310967784538,
3.75584516171998,
3.51858064560534,
3.28131612948007,
3.04405161336543,
2.80678709724003,
2.5695225811254,
2.332258065}; 

  for (i = 0; i < n ; i++) {
    x0 = points1[i];
    y0 = points2[i];
    x1 = points1[i + 1];
    y1 = points2[i + 1];
    if (x >= x0 && x <= x1){ // find the point interval
      if (x <= x0) { return y0; }
      if (x >= x1) { return y1; }
      return y0 + (x - x0) * (y1 - y0) / (x1 - x0);
    }
  }
  return 0.0;
} 

// int main(void)
// {
//   printf("%f\n", interpolator(98.067945));
//   return 0;
// }
static void skel(double t, double *y, double *ydot, void *void_data)
{ 
  double  p, omega, delta, mu_e, mu_ql, mu_el, mu_qn, mu_en, mu_qa, mu_ea, mu_h, beta_nh, beta_hl, beta_hn, lambda_l, lambda_n;
  double  lambda_a, alpha, f_l, f_n, f_a, kappa, c, Tf, obsprob, T_min_l, gamma, temperature;
  double  E, QL, EL_s, EL_i, QN_s, QN_i, EN_s, EN_i, QA_s, QA_i, EA, H_s, H_i, cases;
  double *data = (double *)void_data;
  
  temperature = interpolator(t);
  p = data[0];
  omega = data[1];
  delta = data[2];
  mu_e = data[3];
  mu_ql = data[4];
  mu_el = data[5];
  mu_qn = data[6];
  mu_en = data[7];
  mu_qa = data[8];
  mu_ea = data[9];
  mu_h = data[10];
  beta_nh = data[11];
  beta_hl = data[12];
  beta_hn = data[13];
  lambda_l = data[14];
  lambda_n = data[15];
  lambda_a = data[16];
  alpha = data[17];
  f_l = data[18];
  f_n = data[19];
  f_a = data[20];
  kappa = data[21];
  c = data[22];
  Tf = data[23];
  obsprob = data[24];
  T_min_l = data[25];
  gamma= data[26];
  // temperature = data[27];

  E = y[0];
  QL = y[1];
  EL_s = y[2];
  EL_i = y[3];
  QN_s = y[4];
  QN_i = y[5];
  EN_s = y[6];
  EN_i = y[7];
  QA_s = y[8];
  QA_i = y[9];
  EA = y[10];
  H_s = y[11];
  H_i = y[12];
  cases = y[13];

  double d_el = 0;
  if (temperature>=8.4) {
  d_el = -0.00001*pow(temperature,2) + 0.002*temperature - 0.019;
  if (d_el<0) {d_el = 0;}
  }
                 
  double d_ln = 0;
  if (temperature>=7.4) {
  d_ln = 0.00003*pow(temperature,2) + 0.00073*temperature - 0.007;
  if (d_ln<0) {d_ln = 0;}
  }
                 
  double d_na = 0;
  if (temperature>=8.7) {
  d_na = - 0.000008*pow(temperature,2) + 0.0019*temperature - 0.016;
  if (d_na<0) {d_na = 0;}
  }
                 
  double d_pop = 0;
  if (temperature>=4) {
  d_pop = -0.00001867*pow(temperature,3) + 0.0008724*pow(temperature,2) - 0.006195*temperature + 0.01802;
  if (d_pop<0) {d_pop = 0;}
  }
                 
  double a_l = 0;
  double pQL = 0;
  if (temperature >= T_min_l)  {
  pQL =  1;
  if (pQL<0) { pQL = 0;}
  }
  a_l = pQL * lambda_l;
                 
  double a_n = 0;
  double pQN = 0;
  if (temperature >= 7)  {
  pQN = 1;
  if (pQN<0) { pQN = 0;}
  }
  a_n = pQN * lambda_n;
                 
                 
  double a_a = 0;
  double pQA = 0;
  if (temperature >= 7)  {
  pQA = 1;
  if (pQA<0) { pQA = 0;}
  }
  a_a = pQA * lambda_a;
                 
  double lambda_hum = alpha * exp(0.058*temperature);
  double beta_n = kappa * lambda_hum * pQN;
  double beta_a = lambda_hum * pQA;
  double d = 1 - pow((1-c),Tf*a_n*QN_i);
                 
  double DE = p*delta*d_pop*exp(-omega*delta*d_pop*EA)*EA - d_el*E - mu_e*E;
  double DQL = d_el*E - a_l*QL - mu_ql*QL;
  double DEL_s = (1-d)*((1-beta_hl)*H_i+(1-H_i))*f_l*a_l*QL - d_ln*EL_s - mu_el*EL_s; 
  double DEL_i = d*((1-beta_hl)*H_i+(1-H_i))*f_l*a_l*QL + beta_hl*f_l*a_l*QL*H_i - d_ln*EL_i - mu_el*EL_i; 
  double DQN_s = d_ln*EL_s - a_n*QN_s - mu_qn*QN_s;
  double DQN_i = d_ln*EL_i - a_n*QN_i - mu_qn*QN_i;
  double DEN_s = (1-d)*((1-beta_hn)*H_i+(1-H_i))*f_n*a_n*QN_s  - d_na*EN_s - mu_en*EN_s;
  double DEN_i = d*((1-beta_hn)*H_i+(1-H_i))*f_n*a_n*QN_s + beta_hn*f_n*a_n*QN_s*H_i + f_n*a_n*QN_i- d_na*EN_i - mu_en*EN_i;
  double DQA_s = d_na*EN_s - a_a*QA_s - mu_qa*QA_s;
  double DQA_i = d_na*EN_i - a_a*QA_i - mu_qa*QA_i;
  double DEA = f_a*a_a*(QA_s+QA_i) - d_pop*EA - mu_ea*EA;
  double DH_s = mu_h - beta_nh*a_n*QN_i*H_s - mu_h * H_s;
  double DH_i = beta_nh*a_n*QN_i*H_s - gamma*H_i - mu_h * H_i;
  double Dcases = beta_n*QN_i + beta_a*QA_i;
  // printf("%f %f\n" , t , temperature);
  ydot[0] = DE;
  ydot[1] = DQL;
  ydot[2] = DEL_s;
  ydot[3] = DEL_i;
  ydot[4] = DQN_s;
  ydot[5] = DQN_i;
  ydot[6] = DEN_s;
  ydot[7] = DEN_i;
  ydot[8] = DQA_s;
  ydot[9] = DQA_i;
  ydot[10] = DEA;
  ydot[11] = DH_s;
  ydot[12] = DH_i;
  ydot[13] = Dcases;
} 


int run_me(int lengthBuffer, double* yout, double y1, double y2, double y3, double y4, double y5, double y6, double y7, double y8, double y9, double y10, double y11, double y12,
           double y13, double y14 , double data0, double data1, double data2, double data3, double data4 , double data5 , double data6 , double data7 , double data8 ,
            double data9 , double data10 , double data11 , double data12 , double data13 , double data14 , double data15, double data16, double data17 , double data18 , double data19 , double data20 , double data21 ,
             double data22 , double data23 , double data24 , double data25 , double data26, double *x, int lengthx)
{

  
  double          rwork1, rwork5, rwork6, rwork7;
  double          atol[15], rtol[15], t, y[15];
  int             iwork1, iwork2, iwork5, iwork6, iwork7, iwork8, iwork9;
  int             neq = 14;
  int             itol, itask, istate, iopt, jt, iout, ijs;
  double          data[27];
  double          tout;
  //printf("%f %f %f \n", x[1], x[2],x[3]);
  // printf(" %d , %14.6e, %14.6e, \n", lengthBuffer, tout);
  iwork1 = iwork2 = iwork5 = iwork6 = iwork7 = iwork8 = iwork9 = 0;
  rwork1 = rwork5 = rwork6 = rwork7 = 0.0;
  y[1] = y1;
  y[2] = y2;
  y[3] = y3;
  y[4] = y4;
  y[5] = y5;
  y[6] = y6;
  y[7] = y7;
  y[8] = y8;
  y[9] = y9;
  y[10] = y10;
  y[11] = y11;
  y[12] = y12;
  y[13] = y13;
  y[14] = y14;
  data[0] = data0;                                
  data[1] = data1;
  data[2] = data2;
  data[3] = data3;
  data[4] = data4;
  data[5] = data5;
  data[6] = data6;
  data[7] = data7;
  data[8] = data8;
  data[9] = data9;
  data[10] = data10;
  data[11] = data11;
  data[12] = data12;
  data[13] = data13;
  data[14] = data14;
  data[15] = data15;
  data[16] = data16;
  data[17] = data17;
  data[18] = data18;
  data[19] = data19;
  data[20] = data20;
  data[21] = data21;
  data[22] = data22;
  data[23] = data23;
  data[24] = data24;
  data[25] = data25;
  data[26] = data26;
  
  t = 0.0E0;
  tout = 0.0E0;
  itol = 2;
  rtol[0] = 0.0;
  atol[0] = 0.0;
  rtol[1] = rtol[3] = 1.0E-6;
  rtol[2] = rtol[4] = rtol[5] = rtol[6] = rtol[7] = rtol[8] = rtol[9] = rtol[10] = rtol[11] =rtol[12] = rtol[13] = rtol[14] =   1.0E-6;
  atol[1] = 1.0E-6;
  atol[2] = 1.0E-6;
  atol[3] = 1.0E-6;
  atol[4] = 1.0E-6;
  atol[5] = 1.0E-6;
  atol[6] = 1.0E-6;
  atol[7] = 1.0E-6;
  atol[8] = 1.0E-6;
  atol[9] = 1.0E-6;
  atol[10] = 1.0E-6;
  atol[11] = 1.0E-6;
  atol[12] = 1.0E-6;
  atol[13] = 1.0E-6;
  atol[14] = 1.0E-6;
  
  itask = 1;
  istate = 1;
  iopt = 0;
  jt = 2;
  
  double timesJs[] = {7.01923076921162,14.0384615384232,21.0576923077178,28.0769230769295,35.0961538461411,42.1153846153527,49.1346153846473,56.1538461538589,63.1730769230705,70.1923076922822,77.2115384615768,84.2307692307884,91.25,98.2692307692116,105.288461538423,112.307692307718,119.326923076929,126.346153846141,133.365384615353,140.384615384647,147.403846153859,154.423076923071,161.442307692282,168.461538461577,175.480769230788,182.5,189.519230769212,196.538461538423,203.557692307718,210.576923076929,217.596153846141,224.615384615353,231.634615384647,238.653846153859,245.673076923071,252.692307692282,259.711538461577,266.730769230788,273.75,280.769230769212,287.788461538423,294.807692307718,301.826923076929,308.846153846141,315.865384615353,322.884615384647,329.903846153859,336.923076923071,343.942307692282,350.961538461577,357.980769230788,365,372.019230769212,379.038461538423,386.057692307718,393.076923076929,400.096153846141,407.115384615353,414.134615384647,421.153846153859,428.173076923071,435.192307692282,442.211538461577,449.230769230788,456.25,463.269230769212,470.288461538423,477.307692307718,484.326923076929,491.346153846141,498.365384615353,505.384615384647,512.403846153859,519.423076923071,526.442307692282,533.461538461577,540.480769230788,547.5,554.519230769212,561.538461538423,568.557692307718,575.576923076929,582.596153846141,589.615384615353,596.634615384647,603.653846153859,610.673076923071,617.692307692282,624.711538461577,631.730769230788,638.75,645.769230769212,652.788461538423,659.807692307718,666.826923076929,673.846153846141,680.865384615353,687.884615384647,694.903846153859,701.923076923071,708.942307692282,715.961538461577,722.980769230788,730,737.019230769212,744.038461538423,751.057692307718,758.076923076929,765.096153846141,772.115384615353,779.134615384647,786.153846153859,793.173076923071,800.192307692282,807.211538461577,814.230769230788,821.25,828.269230769212,835.288461538423,842.307692307718,849.326923076929,856.346153846141,863.365384615353,870.384615384647,877.403846153859,884.423076923071,891.442307692282,898.461538461577,905.480769230788,912.5,919.519230769212,926.538461538423,933.557692307718,940.576923076929,947.596153846141,954.615384615353,961.634615384647,968.653846153859,975.673076923071,982.692307692282,989.711538461577,996.730769230788,1003.75,1010.76923076921,1017.78846153842,1024.80769230772,1031.82692307693,1038.84615384614,1045.86538461535,1052.88461538465,1059.90384615386,1066.92307692307,1073.94230769228,1080.96153846158,1087.98076923079,1095,1102.01923076921,1109.03846153842,1116.05769230772,1123.07692307693,1130.09615384614,1137.11538461535,1144.13461538465,1151.15384615386,1158.17307692307,1165.19230769228,1172.21153846158,1179.23076923079,1186.25,1193.26923076921,1200.28846153842,1207.30769230772,1214.32692307693,1221.34615384614,1228.36538461535,1235.38461538465,1242.40384615386,1249.42307692307,1256.44230769228,1263.46153846158,1270.48076923079,1277.5,1284.51923076921,1291.53846153842,1298.55769230772,1305.57692307693,1312.59615384614,1319.61538461535,1326.63461538465,1333.65384615386,1340.67307692307,1347.69230769228,1354.71153846158,1361.73076923079,1368.75,1375.76923076921,1382.78846153842,1389.80769230772,1396.82692307693,1403.84615384614,1410.86538461535,1417.88461538465,1424.90384615386,1431.92307692307,1438.94230769228,1445.96153846158,1452.98076923079,1460,1467.01923076921,1474.03846153842,1481.05769230772,1488.07692307693,1495.09615384614,1502.11538461535,1509.13461538465,1516.15384615386,1523.17307692307,1530.19230769228,1537.21153846158,1544.23076923079,1551.25,1558.26923076921,1565.28846153842,1572.30769230772,1579.32692307693,1586.34615384614,1593.36538461535,1600.38461538465,1607.40384615386,1614.42307692307,1621.44230769228,1628.46153846158,1635.48076923079,1642.5,1649.51923076921,1656.53846153842,1663.55769230772,1670.57692307693,1677.59615384614,1684.61538461535,1691.63461538465,1698.65384615386,1705.67307692307,1712.69230769228,1719.71153846158,1726.73076923079,1733.75,1740.76923076921,1747.78846153842,1754.80769230772,1761.82692307693,1768.84615384614,1775.86538461535,1782.88461538465,1789.90384615386,1796.92307692307,1803.94230769228,1810.96153846158,1817.98076923079,1825,1832.01923076921,1839.03846153842,1846.05769230772,1853.07692307693,1860.09615384614,1867.11538461535,1874.13461538465,1881.15384615386,1888.17307692307,1895.19230769228,1902.21153846158,1909.23076923079,1916.25,1923.26923076921,1930.28846153842,1937.30769230772,1944.32692307693,1951.34615384614,1958.36538461535,1965.38461538465,1972.40384615386,1979.42307692307,1986.44230769228,1993.46153846158,2000.48076923079,2007.5,2014.51923076921,2021.53846153842,2028.55769230772,2035.57692307693,2042.59615384614,2049.61538461535,2056.63461538465,2063.65384615386,2070.67307692307,2077.69230769228,2084.71153846158,2091.73076923079,2098.75,2105.76923076921,2112.78846153842,2119.80769230772,2126.82692307693,2133.84615384614,2140.86538461535,2147.88461538465,2154.90384615386,2161.92307692307,2168.94230769228,2175.96153846158,2182.98076923079,2190,2197.01923076921,2204.03846153842,2211.05769230772,2218.07692307693,2225.09615384614,2232.11538461535,2239.13461538465,2246.15384615386,2253.17307692307,2260.19230769228,2267.21153846158,2274.23076923079,2281.25,2288.26923076921,2295.28846153842,2302.30769230772,2309.32692307693,2316.34615384614,2323.36538461535,2330.38461538465,2337.40384615386,2344.42307692307,2351.44230769228,2358.46153846158,2365.48076923079,2372.5,2379.51923076921,2386.53846153842,2393.55769230772,2400.57692307693,2407.59615384614,2414.61538461535,2421.63461538465,2428.65384615386,2435.67307692307,2442.69230769228,2449.71153846158,2456.73076923079,2463.75,2470.76923076921,2477.78846153842,2484.80769230772,2491.82692307693,2498.84615384614,2505.86538461535,2512.88461538465,2519.90384615386,2526.92307692307,2533.94230769228,2540.96153846158,2547.98076923079,2555,2562.01923076921,2569.03846153842,2576.05769230772,2583.07692307693,2590.09615384614,2597.11538461535,2604.13461538465,2611.15384615386,2618.17307692307,2625.19230769228,2632.21153846158,2639.23076923079,2646.25,2653.26923076921,2660.28846153842,2667.30769230772,2674.32692307693,2681.34615384614,2688.36538461535,2695.38461538465,2702.40384615386,2709.42307692307,2716.44230769228,2723.46153846158,2730.48076923079,2737.5,2744.51923076921,2751.53846153842,2758.55769230772,2765.57692307693,2772.59615384614,2779.61538461535,2786.63461538465,2793.65384615386,2800.67307692307,2807.69230769228,2814.71153846158,2821.73076923079,2828.75,2835.76923076921,2842.78846153842,2849.80769230772,2856.82692307693,2863.84615384614,2870.86538461535,2877.88461538465,2884.90384615386,2891.92307692307,2898.94230769228,2905.96153846158,2912.98076923079,2920,2927.01923076921,2934.03846153842,2941.05769230772,2948.07692307693,2955.09615384614,2962.11538461535,2969.13461538465,2976.15384615386,2983.17307692307,2990.19230769228,2997.21153846158,3004.23076923079,3011.25,3018.26923076921,3025.28846153842,3032.30769230772,3039.32692307693,3046.34615384614,3053.36538461535,3060.38461538465,3067.40384615386,3074.42307692307,3081.44230769228,3088.46153846158,3095.48076923079,3102.5,3109.51923076921,3116.53846153842,3123.55769230772,3130.57692307693,3137.59615384614,3144.61538461535,3151.63461538465,3158.65384615386,3165.67307692307,3172.69230769228,3179.71153846158,3186.73076923079,3193.75,3200.76923076921,3207.78846153842,3214.80769230772,3221.82692307693,3228.84615384614,3235.86538461535,3242.88461538465,3249.90384615386,3256.92307692307,3263.94230769228,3270.96153846158,3277.98076923079,3285,3292.01923076921,3299.03846153842,3306.05769230772,3313.07692307693,3320.09615384614,3327.11538461535,3334.13461538465,3341.15384615386,3348.17307692307,3355.19230769228,3362.21153846158,3369.23076923079,3376.25,3383.26923076921,3390.28846153842,3397.30769230772,3404.32692307693,3411.34615384614,3418.36538461535,3425.38461538465,3432.40384615386,3439.42307692307,3446.44230769228,3453.46153846158,3460.48076923079,3467.5,3474.51923076921,3481.53846153842,3488.55769230772,3495.57692307693,3502.59615384614,3509.61538461535,3516.63461538465,3523.65384615386,3530.67307692307,3537.69230769228,3544.71153846158,3551.73076923079,3558.75,3565.76923076921,3572.78846153842,3579.80769230772,3586.82692307693,3593.84615384614,3600.86538461535,3607.88461538465,3614.90384615386,3621.92307692307,3628.94230769228,3635.96153846158,3642.98076923079,3650,3657.01923076921,3664.03846153842,3671.05769230772,3678.07692307693,3685.09615384614,3692.11538461535,3699.13461538465,3706.15384615386,3713.17307692307,3720.19230769228,3727.21153846158,3734.23076923079,3741.25,3748.26923076921,3755.28846153842,3762.30769230772,3769.32692307693,3776.34615384614,3783.36538461535,3790.38461538465,3797.40384615386,3804.42307692307,3811.44230769228,3818.46153846158,3825.48076923079,3832.5,3839.51923076921,3846.53846153842,3853.55769230772,3860.57692307693,3867.59615384614,3874.61538461535,3881.63461538465,3888.65384615386,3895.67307692307,3902.69230769228,3909.71153846158,3916.73076923079,3923.75,3930.76923076921,3937.78846153842,3944.80769230772,3951.82692307693,3958.84615384614,3965.86538461535,3972.88461538465,3979.90384615386,3986.92307692307,3993.94230769228,4000.96153846158,4007.98076923079,4015,4022.01923076921,4029.03846153842,4036.05769230772,4043.07692307693,4050.09615384614,4057.11538461535,4064.13461538465,4071.15384615386,4078.17307692307,4085.19230769228,4092.21153846158,4099.23076923079,4106.25,4113.26923076921,4120.28846153842,4127.30769230772,4134.32692307693,4141.34615384614,4148.36538461535,4155.38461538465,4162.40384615386,4169.42307692307,4176.44230769228,4183.46153846158,4190.48076923079,4197.5,4204.51923076921,4211.53846153842,4218.55769230772,4225.57692307693,4232.59615384614,4239.61538461535,4246.63461538465,4253.65384615386,4260.67307692307,4267.69230769228,4274.71153846158,4281.73076923079,4288.75,4295.76923076921,4302.78846153842,4309.80769230772,4316.82692307693,4323.84615384614,4330.86538461535,4337.88461538465,4344.90384615386,4351.92307692307,4358.94230769228,4365.96153846158,4372.98076923079,4380,4387.01923076921,4394.03846153842,4401.05769230772,4408.07692307693,4415.09615384614,4422.11538461535,4429.13461538465,4436.15384615386,4443.17307692307,4450.19230769228,4457.21153846158,4464.23076923079,4471.25,4478.26923076921,4485.28846153842,4492.30769230772,4499.32692307693,4506.34615384614,4513.36538461535,4520.38461538465,4527.40384615386,4534.42307692307,4541.44230769228,4548.46153846158,4555.48076923079,4562.5,4569.51923076921,4576.53846153842,4583.55769230772,4590.57692307693,4597.59615384614,4604.61538461535,4611.63461538465,4618.65384615386,4625.67307692307,4632.69230769228,4639.71153846158,4646.73076923079,4653.75,4660.76923076921,4667.78846153842,4674.80769230772,4681.82692307693,4688.84615384614,4695.86538461535,4702.88461538465,4709.90384615386,4716.92307692307,4723.94230769228,4730.96153846158,4737.98076923079,4745,4752.01923076921,4759.03846153842,4766.05769230772,4773.07692307693,4780.09615384614,4787.11538461535,4794.13461538465,4801.15384615386,4808.17307692307,4815.19230769228,4822.21153846158,4829.23076923079,4836.25,4843.26923076921,4850.28846153842,4857.30769230772,4864.32692307693,4871.34615384614,4878.36538461535,4885.38461538465,4892.40384615386,4899.42307692307,4906.44230769228,4913.46153846158,4920.48076923079,4927.5,4934.51923076921,4941.53846153842,4948.55769230772,4955.57692307693,4962.59615384614,4969.61538461535,4976.63461538465,4983.65384615386,4990.67307692307,4997.69230769228,5004.71153846158,5011.73076923079,5018.75,5025.76923076921,5032.78846153842,5039.80769230772,5046.82692307693,5053.84615384614,5060.86538461535,5067.88461538465,5074.90384615386,5081.92307692307,5088.94230769228,5095.96153846158,5102.98076923079,5110,5117.01923076921,5124.03846153842,5131.05769230772,5138.07692307693,5145.09615384614,5152.11538461535,5159.13461538465,5166.15384615386,5173.17307692307,5180.19230769228,5187.21153846158,5194.23076923079,5201.25,5208.26923076921,5215.28846153842,5222.30769230772,5229.32692307693,5236.34615384614,5243.36538461535,5250.38461538465,5257.40384615386,5264.42307692307,5271.44230769228,5278.46153846158,5285.48076923079,5292.5,5299.51923076921,5306.53846153842,5313.55769230772,5320.57692307693,5327.59615384614,5334.61538461535,5341.63461538465,5348.65384615386,5355.67307692307,5362.69230769228,5369.71153846158,5376.73076923079,5383.75,5390.76923076921,5397.78846153842,5404.80769230772,5411.82692307693,5418.84615384614,5425.86538461535,5432.88461538465,5439.90384615386,5446.92307692307,5453.94230769228,5460.96153846158,5467.98076923079,5475,5482.01923076921,5489.03846153842,5496.05769230772,5503.07692307693,5510.09615384614,5517.11538461535,5524.13461538465,5531.15384615386,5538.17307692307,5545.19230769228,5552.21153846158,5559.23076923079,5566.25,5573.26923076921,5580.28846153842,5587.30769230772,5594.32692307693,5601.34615384614,5608.36538461535,5615.38461538465,5622.40384615386,5629.42307692307,5636.44230769228,5643.46153846158,5650.48076923079,5657.5,5664.51923076921,5671.53846153842,5678.55769230772,5685.57692307693,5692.59615384614,5699.61538461535,5706.63461538465,5713.65384615386,5720.67307692307,5727.69230769228,5734.71153846158,5741.73076923079,5748.75,5755.76923076921,5762.78846153842,5769.80769230772,5776.82692307693,5783.84615384614,5790.86538461535,5797.88461538465,5804.90384615386,5811.92307692307,5818.94230769228,5825.96153846158,5832.98076923079,5840,5847.01923076921,5854.03846153842,5861.05769230772,5868.07692307693,5875.09615384614,5882.11538461535,5889.13461538465,5896.15384615386,5903.17307692307,5910.19230769228,5917.21153846158,5924.23076923079,5931.25,5938.26923076921,5945.28846153842,5952.30769230772,5959.32692307693,5966.34615384614,5973.36538461535,5980.38461538465,5987.40384615386,5994.42307692307,6001.44230769228,6008.46153846158,6015.48076923079,6022.5,6029.51923076921,6036.53846153842,6043.55769230772,6050.57692307693,6057.59615384614,6064.61538461535,6071.63461538465,6078.65384615386,6085.67307692307,6092.69230769228,6099.71153846158,6106.73076923079,6113.75,6120.76923076921,6127.78846153842,6134.80769230772,6141.82692307693,6148.84615384614,6155.86538461535,6162.88461538465,6169.90384615386,6176.92307692307,6183.94230769228,6190.96153846158,6197.98076923079,6205,6212.01923076921,6219.03846153842,6226.05769230772,6233.07692307693,6240.09615384614,6247.11538461535,6254.13461538465,6261.15384615386,6268.17307692307,6275.19230769228,6282.21153846158,6289.23076923079,6296.25,6303.26923076921,6310.28846153842,6317.30769230772,6324.32692307693,6331.34615384614,6338.36538461535,6345.38461538465,6352.40384615386,6359.42307692307,6366.44230769228,6373.46153846158,6380.48076923079,6387.5,6394.51923076921,6401.53846153842,6408.55769230772,6415.57692307693,6422.59615384614,6429.61538461535,6436.63461538465,6443.65384615386,6450.67307692307,6457.69230769228,6464.71153846158,6471.73076923079,6478.75,6485.76923076921,6492.78846153842,6499.80769230772,6506.82692307693,6513.84615384614,6520.86538461535,6527.88461538465,6534.90384615386,6541.92307692307,6548.94230769228,6555.96153846158,6562.98076923079,6570};
  for (iout = 0; iout < lengthBuffer; iout++) {
    lsoda(skel, neq, y, &t, tout, itol, rtol, atol, itask, &istate, iopt, jt,
          iwork1, iwork2, iwork5, iwork6, iwork7, iwork8, iwork9,
          rwork1, rwork5, rwork6, rwork7, data);
    yout[iout] = y[14];
    // printf(" %14.6e \n",yout[iout]);
    if (istate <= 0) {
      printf("error istate = %d\n", istate);
      // exit(0);
			n_lsoda_terminate();
			return -1;
    }
    tout = timesJs[iout];
  }
    
  n_lsoda_terminate();

  return 0;
}


