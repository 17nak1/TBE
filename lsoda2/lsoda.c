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

typedef void    (*_lsoda_f) (double, double *, double *, double *, double *, double *);

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

static void     stoda(int neq, double *y, _lsoda_f f, double *_data, double * covar1_p, double * covar2_p);
static void     correction(int neq, double *y, _lsoda_f f, int *corflag, double pnorm, double *del, double *delp, double *told,
						   int *ncf, double *rh, int *m, double *_data, double * covar1_p, double * covar2_p);
static void     prja(int neq, double *y, _lsoda_f f, double *_data, double * covar1_p, double * covar2_p);
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
		   double rwork1, double rwork5, double rwork6, double rwork7, double *_data, double *covar1_p, double *covar2_p)
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
		(*f) (*t, y + 1, yh[2] + 1, _data,covar1_p, covar2_p);
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
		stoda(neq, y, f, _data,covar1_p, covar2_p);

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


static void stoda(int neq, double *y, _lsoda_f f, double *_data ,double *covar1_p,double * covar2_p)
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

			correction(neq, y, f, &corflag, pnorm, &del, &delp, &told, &ncf, &rh, &m, _data, covar1_p,covar2_p);
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
					(*f) (tn, y + 1, savf + 1, _data ,covar1_p, covar2_p);
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


static void prja(int neq, double *y, _lsoda_f f, double *_data, double * covar1_p, double * covar2_p)
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
			(*f) (tn, y + 1, acor + 1, _data ,covar1_p, covar2_p);
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
					   int *ncf, double *rh, int *m, double *_data, double * covar1_p, double * covar2_p)
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
	(*f) (tn, y + 1, savf + 1, _data ,covar1_p, covar2_p);
	nfe++;
/*
   If indicated, the matrix P = I - h * el[1] * J is reevaluated and
   preprocessed before starting the corrector iteration.  ipup is set
   to 0 as an indicator that this has been done.
*/
	while (1) {
		if (*m == 0) {
			if (ipup > 0) {
				prja(neq, y, f, _data, covar1_p, covar2_p);
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
			(*f) (tn, y + 1, savf + 1, _data ,covar1_p, covar2_p);
			nfe++;
		}
/*
   Iterate corrector.
*/
		else {
			*delp = *del;
			(*f) (tn, y + 1, savf + 1, _data ,covar1_p, covar2_p );
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

int n_lsoda(double y[], int n, double *x, double xout, double eps, const double yscal[], _lsoda_f devis, double *data, double * covar1_p, double * covar2_p)
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
	lsoda(devis, n, _y, x, xout, 2, rtol, atol, itask, &istate, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0., 0., 0., 0., data, covar1_p, covar2_p);
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

double interpolator (double x, double * covar1_p, double * covar2_p, double _length) {
  int     i;
  double   x0, y0, x1, y1;
  int n = (int) _length;

  for (i = 0; i < _length ; i++) {
    x0 = covar1_p[i];
    y0 = covar2_p[i];
    x1 = covar1_p[i + 1];
    y1 = covar2_p[i + 1];
    if (x >= x0 && x <= x1){ // find the point interval
      if (x <= x0) { return y0; }
      if (x >= x1) { return y1; }
      return y0 + (x - x0) * (y1 - y0) / (x1 - x0);
    }
  }
  return 0.0;
} 


/**
 * 
 *  @param data     array of double
 */
static void skel(double t, double *y, double *ydot, double *data, double *covar1_p, double *covar2_p)
{ 
  double  p, omega, delta, mu_e, mu_ql, mu_el, mu_qn, mu_en, mu_qa, mu_ea, mu_h, beta_nh, beta_hl, beta_hn, lambda_l, lambda_n;
  double  lambda_a, alpha, f_l, f_n, f_a, kappa, c, Tf, obsprob, T_min_l, gamma, temperature;
  double  temp[3];
  double  E, QL, EL_s, EL_i, QN_s, QN_i, EN_s, EN_i, QA_s, QA_i, EA, H_s, H_i, cases;
  temperature = interpolator(t, covar1_p, covar2_p,data[27]);
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


int run_me(int lengthBuffer, double * yout, double * times,  double * covar1_p, double * covar2_p, double y1, double y2, double y3, double y4, double y5, double y6, double y7, double y8, double y9, double y10, double y11, double y12,
           double y13, double y14 , double data0, double data1, double data2, double data3, double data4 , double data5 , double data6 , double data7 , double data8 ,
            double data9 , double data10 , double data11 , double data12 , double data13 , double data14 , double data15, double data16, double data17 , double data18 , double data19 , double data20 , double data21 ,
             double data22 , double data23 , double data24 , double data25 , double data26, double data27)
{

  double          rwork1, rwork5, rwork6, rwork7;
  double          atol[15], rtol[15], t, y[15];
  int             iwork1, iwork2, iwork5, iwork6, iwork7, iwork8, iwork9;
  int             neq = 14;
  int             itol, itask, istate, iopt, jt, iout, ijs;
  double          data[28];
  double          tout;
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
  data[27] = data27;
 
  // double timesJs[] = {0.0, 7.01923076921162,14.0384615384232,21.0576923077178,28.0769230769295,35.0961538461411,42.1153846153527,49.1346153846473,56.1538461538589,63.1730769230705,70.1923076922822,77.2115384615768,84.2307692307884,91.25,98.2692307692116,105.288461538423,112.307692307718,119.326923076929,126.346153846141,133.365384615353,140.384615384647,147.403846153859,154.423076923071,161.442307692282,168.461538461577,175.480769230788,182.5,189.519230769212,196.538461538423,203.557692307718,210.576923076929,217.596153846141,224.615384615353,231.634615384647,238.653846153859,245.673076923071,252.692307692282,259.711538461577,266.730769230788,273.75,280.769230769212,287.788461538423,294.807692307718,301.826923076929,308.846153846141,315.865384615353,322.884615384647,329.903846153859,336.923076923071,343.942307692282,350.961538461577,357.980769230788,365,372.019230769212,379.038461538423,386.057692307718,393.076923076929,400.096153846141,407.115384615353,414.134615384647,421.153846153859,428.173076923071,435.192307692282,442.211538461577,449.230769230788,456.25,463.269230769212,470.288461538423,477.307692307718,484.326923076929,491.346153846141,498.365384615353,505.384615384647,512.403846153859,519.423076923071,526.442307692282,533.461538461577,540.480769230788,547.5,554.519230769212,561.538461538423,568.557692307718,575.576923076929,582.596153846141,589.615384615353,596.634615384647,603.653846153859,610.673076923071,617.692307692282,624.711538461577,631.730769230788,638.75,645.769230769212,652.788461538423,659.807692307718,666.826923076929,673.846153846141,680.865384615353,687.884615384647,694.903846153859,701.923076923071,708.942307692282,715.961538461577,722.980769230788,730,737.019230769212,744.038461538423,751.057692307718,758.076923076929,765.096153846141,772.115384615353,779.134615384647,786.153846153859,793.173076923071,800.192307692282,807.211538461577,814.230769230788,821.25,828.269230769212,835.288461538423,842.307692307718,849.326923076929,856.346153846141,863.365384615353,870.384615384647,877.403846153859,884.423076923071,891.442307692282,898.461538461577,905.480769230788,912.5,919.519230769212,926.538461538423,933.557692307718,940.576923076929,947.596153846141,954.615384615353,961.634615384647,968.653846153859,975.673076923071,982.692307692282,989.711538461577,996.730769230788,1003.75,1010.76923076921,1017.78846153842,1024.80769230772,1031.82692307693,1038.84615384614,1045.86538461535,1052.88461538465,1059.90384615386,1066.92307692307,1073.94230769228,1080.96153846158,1087.98076923079,1095,1102.01923076921,1109.03846153842,1116.05769230772,1123.07692307693,1130.09615384614,1137.11538461535,1144.13461538465,1151.15384615386,1158.17307692307,1165.19230769228,1172.21153846158,1179.23076923079,1186.25,1193.26923076921,1200.28846153842,1207.30769230772,1214.32692307693,1221.34615384614,1228.36538461535,1235.38461538465,1242.40384615386,1249.42307692307,1256.44230769228,1263.46153846158,1270.48076923079,1277.5,1284.51923076921,1291.53846153842,1298.55769230772,1305.57692307693,1312.59615384614,1319.61538461535,1326.63461538465,1333.65384615386,1340.67307692307,1347.69230769228,1354.71153846158,1361.73076923079,1368.75,1375.76923076921,1382.78846153842,1389.80769230772,1396.82692307693,1403.84615384614,1410.86538461535,1417.88461538465,1424.90384615386,1431.92307692307,1438.94230769228,1445.96153846158,1452.98076923079,1460,1467.01923076921,1474.03846153842,1481.05769230772,1488.07692307693,1495.09615384614,1502.11538461535,1509.13461538465,1516.15384615386,1523.17307692307,1530.19230769228,1537.21153846158,1544.23076923079,1551.25,1558.26923076921,1565.28846153842,1572.30769230772,1579.32692307693,1586.34615384614,1593.36538461535,1600.38461538465,1607.40384615386,1614.42307692307,1621.44230769228,1628.46153846158,1635.48076923079,1642.5,1649.51923076921,1656.53846153842,1663.55769230772,1670.57692307693,1677.59615384614,1684.61538461535,1691.63461538465,1698.65384615386,1705.67307692307,1712.69230769228,1719.71153846158,1726.73076923079,1733.75,1740.76923076921,1747.78846153842,1754.80769230772,1761.82692307693,1768.84615384614,1775.86538461535,1782.88461538465,1789.90384615386,1796.92307692307,1803.94230769228,1810.96153846158,1817.98076923079,1825,1832.01923076921,1839.03846153842,1846.05769230772,1853.07692307693,1860.09615384614,1867.11538461535,1874.13461538465,1881.15384615386,1888.17307692307,1895.19230769228,1902.21153846158,1909.23076923079,1916.25,1923.26923076921,1930.28846153842,1937.30769230772,1944.32692307693,1951.34615384614,1958.36538461535,1965.38461538465,1972.40384615386,1979.42307692307,1986.44230769228,1993.46153846158,2000.48076923079,2007.5,2014.51923076921,2021.53846153842,2028.55769230772,2035.57692307693,2042.59615384614,2049.61538461535,2056.63461538465,2063.65384615386,2070.67307692307,2077.69230769228,2084.71153846158,2091.73076923079,2098.75,2105.76923076921,2112.78846153842,2119.80769230772,2126.82692307693,2133.84615384614,2140.86538461535,2147.88461538465,2154.90384615386,2161.92307692307,2168.94230769228,2175.96153846158,2182.98076923079,2190,2197.01923076921,2204.03846153842,2211.05769230772,2218.07692307693,2225.09615384614,2232.11538461535,2239.13461538465,2246.15384615386,2253.17307692307,2260.19230769228,2267.21153846158,2274.23076923079,2281.25,2288.26923076921,2295.28846153842,2302.30769230772,2309.32692307693,2316.34615384614,2323.36538461535,2330.38461538465,2337.40384615386,2344.42307692307,2351.44230769228,2358.46153846158,2365.48076923079,2372.5,2379.51923076921,2386.53846153842,2393.55769230772,2400.57692307693,2407.59615384614,2414.61538461535,2421.63461538465,2428.65384615386,2435.67307692307,2442.69230769228,2449.71153846158,2456.73076923079,2463.75,2470.76923076921,2477.78846153842,2484.80769230772,2491.82692307693,2498.84615384614,2505.86538461535,2512.88461538465,2519.90384615386,2526.92307692307,2533.94230769228,2540.96153846158,2547.98076923079,2555,2562.01923076921,2569.03846153842,2576.05769230772,2583.07692307693,2590.09615384614,2597.11538461535,2604.13461538465,2611.15384615386,2618.17307692307,2625.19230769228,2632.21153846158,2639.23076923079,2646.25,2653.26923076921,2660.28846153842,2667.30769230772,2674.32692307693,2681.34615384614,2688.36538461535,2695.38461538465,2702.40384615386,2709.42307692307,2716.44230769228,2723.46153846158,2730.48076923079,2737.5,2744.51923076921,2751.53846153842,2758.55769230772,2765.57692307693,2772.59615384614,2779.61538461535,2786.63461538465,2793.65384615386,2800.67307692307,2807.69230769228,2814.71153846158,2821.73076923079,2828.75,2835.76923076921,2842.78846153842,2849.80769230772,2856.82692307693,2863.84615384614,2870.86538461535,2877.88461538465,2884.90384615386,2891.92307692307,2898.94230769228,2905.96153846158,2912.98076923079,2920,2927.01923076921,2934.03846153842,2941.05769230772,2948.07692307693,2955.09615384614,2962.11538461535,2969.13461538465,2976.15384615386,2983.17307692307,2990.19230769228,2997.21153846158,3004.23076923079,3011.25,3018.26923076921,3025.28846153842,3032.30769230772,3039.32692307693,3046.34615384614,3053.36538461535,3060.38461538465,3067.40384615386,3074.42307692307,3081.44230769228,3088.46153846158,3095.48076923079,3102.5,3109.51923076921,3116.53846153842,3123.55769230772,3130.57692307693,3137.59615384614,3144.61538461535,3151.63461538465,3158.65384615386,3165.67307692307,3172.69230769228,3179.71153846158,3186.73076923079,3193.75,3200.76923076921,3207.78846153842,3214.80769230772,3221.82692307693,3228.84615384614,3235.86538461535,3242.88461538465,3249.90384615386,3256.92307692307,3263.94230769228,3270.96153846158,3277.98076923079,3285,3292.01923076921,3299.03846153842,3306.05769230772,3313.07692307693,3320.09615384614,3327.11538461535,3334.13461538465,3341.15384615386,3348.17307692307,3355.19230769228,3362.21153846158,3369.23076923079,3376.25,3383.26923076921,3390.28846153842,3397.30769230772,3404.32692307693,3411.34615384614,3418.36538461535,3425.38461538465,3432.40384615386,3439.42307692307,3446.44230769228,3453.46153846158,3460.48076923079,3467.5,3474.51923076921,3481.53846153842,3488.55769230772,3495.57692307693,3502.59615384614,3509.61538461535,3516.63461538465,3523.65384615386,3530.67307692307,3537.69230769228,3544.71153846158,3551.73076923079,3558.75,3565.76923076921,3572.78846153842,3579.80769230772,3586.82692307693,3593.84615384614,3600.86538461535,3607.88461538465,3614.90384615386,3621.92307692307,3628.94230769228,3635.96153846158,3642.98076923079,3650,3657.01923076921,3664.03846153842,3671.05769230772,3678.07692307693,3685.09615384614,3692.11538461535,3699.13461538465,3706.15384615386,3713.17307692307,3720.19230769228,3727.21153846158,3734.23076923079,3741.25,3748.26923076921,3755.28846153842,3762.30769230772,3769.32692307693,3776.34615384614,3783.36538461535,3790.38461538465,3797.40384615386,3804.42307692307,3811.44230769228,3818.46153846158,3825.48076923079,3832.5,3839.51923076921,3846.53846153842,3853.55769230772,3860.57692307693,3867.59615384614,3874.61538461535,3881.63461538465,3888.65384615386,3895.67307692307,3902.69230769228,3909.71153846158,3916.73076923079,3923.75,3930.76923076921,3937.78846153842,3944.80769230772,3951.82692307693,3958.84615384614,3965.86538461535,3972.88461538465,3979.90384615386,3986.92307692307,3993.94230769228,4000.96153846158,4007.98076923079,4015,4022.01923076921,4029.03846153842,4036.05769230772,4043.07692307693,4050.09615384614,4057.11538461535,4064.13461538465,4071.15384615386,4078.17307692307,4085.19230769228,4092.21153846158,4099.23076923079,4106.25,4113.26923076921,4120.28846153842,4127.30769230772,4134.32692307693,4141.34615384614,4148.36538461535,4155.38461538465,4162.40384615386,4169.42307692307,4176.44230769228,4183.46153846158,4190.48076923079,4197.5,4204.51923076921,4211.53846153842,4218.55769230772,4225.57692307693,4232.59615384614,4239.61538461535,4246.63461538465,4253.65384615386,4260.67307692307,4267.69230769228,4274.71153846158,4281.73076923079,4288.75,4295.76923076921,4302.78846153842,4309.80769230772,4316.82692307693,4323.84615384614,4330.86538461535,4337.88461538465,4344.90384615386,4351.92307692307,4358.94230769228,4365.96153846158,4372.98076923079,4380,4387.01923076921,4394.03846153842,4401.05769230772,4408.07692307693,4415.09615384614,4422.11538461535,4429.13461538465,4436.15384615386,4443.17307692307,4450.19230769228,4457.21153846158,4464.23076923079,4471.25,4478.26923076921,4485.28846153842,4492.30769230772,4499.32692307693,4506.34615384614,4513.36538461535,4520.38461538465,4527.40384615386,4534.42307692307,4541.44230769228,4548.46153846158,4555.48076923079,4562.5,4569.51923076921,4576.53846153842,4583.55769230772,4590.57692307693,4597.59615384614,4604.61538461535,4611.63461538465,4618.65384615386,4625.67307692307,4632.69230769228,4639.71153846158,4646.73076923079,4653.75,4660.76923076921,4667.78846153842,4674.80769230772,4681.82692307693,4688.84615384614,4695.86538461535,4702.88461538465,4709.90384615386,4716.92307692307,4723.94230769228,4730.96153846158,4737.98076923079,4745,4752.01923076921,4759.03846153842,4766.05769230772,4773.07692307693,4780.09615384614,4787.11538461535,4794.13461538465,4801.15384615386,4808.17307692307,4815.19230769228,4822.21153846158,4829.23076923079,4836.25,4843.26923076921,4850.28846153842,4857.30769230772,4864.32692307693,4871.34615384614,4878.36538461535,4885.38461538465,4892.40384615386,4899.42307692307,4906.44230769228,4913.46153846158,4920.48076923079,4927.5,4934.51923076921,4941.53846153842,4948.55769230772,4955.57692307693,4962.59615384614,4969.61538461535,4976.63461538465,4983.65384615386,4990.67307692307,4997.69230769228,5004.71153846158,5011.73076923079,5018.75,5025.76923076921,5032.78846153842,5039.80769230772,5046.82692307693,5053.84615384614,5060.86538461535,5067.88461538465,5074.90384615386,5081.92307692307,5088.94230769228,5095.96153846158,5102.98076923079,5110,5117.01923076921,5124.03846153842,5131.05769230772,5138.07692307693,5145.09615384614,5152.11538461535,5159.13461538465,5166.15384615386,5173.17307692307,5180.19230769228,5187.21153846158,5194.23076923079,5201.25,5208.26923076921,5215.28846153842,5222.30769230772,5229.32692307693,5236.34615384614,5243.36538461535,5250.38461538465,5257.40384615386,5264.42307692307,5271.44230769228,5278.46153846158,5285.48076923079,5292.5,5299.51923076921,5306.53846153842,5313.55769230772,5320.57692307693,5327.59615384614,5334.61538461535,5341.63461538465,5348.65384615386,5355.67307692307,5362.69230769228,5369.71153846158,5376.73076923079,5383.75,5390.76923076921,5397.78846153842,5404.80769230772,5411.82692307693,5418.84615384614,5425.86538461535,5432.88461538465,5439.90384615386,5446.92307692307,5453.94230769228,5460.96153846158,5467.98076923079,5475,5482.01923076921,5489.03846153842,5496.05769230772,5503.07692307693,5510.09615384614,5517.11538461535,5524.13461538465,5531.15384615386,5538.17307692307,5545.19230769228,5552.21153846158,5559.23076923079,5566.25,5573.26923076921,5580.28846153842,5587.30769230772,5594.32692307693,5601.34615384614,5608.36538461535,5615.38461538465,5622.40384615386,5629.42307692307,5636.44230769228,5643.46153846158,5650.48076923079,5657.5,5664.51923076921,5671.53846153842,5678.55769230772,5685.57692307693,5692.59615384614,5699.61538461535,5706.63461538465,5713.65384615386,5720.67307692307,5727.69230769228,5734.71153846158,5741.73076923079,5748.75,5755.76923076921,5762.78846153842,5769.80769230772,5776.82692307693,5783.84615384614,5790.86538461535,5797.88461538465,5804.90384615386,5811.92307692307,5818.94230769228,5825.96153846158,5832.98076923079,5840,5847.01923076921,5854.03846153842,5861.05769230772,5868.07692307693,5875.09615384614,5882.11538461535,5889.13461538465,5896.15384615386,5903.17307692307,5910.19230769228,5917.21153846158,5924.23076923079,5931.25,5938.26923076921,5945.28846153842,5952.30769230772,5959.32692307693,5966.34615384614,5973.36538461535,5980.38461538465,5987.40384615386,5994.42307692307,6001.44230769228,6008.46153846158,6015.48076923079,6022.5,6029.51923076921,6036.53846153842,6043.55769230772,6050.57692307693,6057.59615384614,6064.61538461535,6071.63461538465,6078.65384615386,6085.67307692307,6092.69230769228,6099.71153846158,6106.73076923079,6113.75,6120.76923076921,6127.78846153842,6134.80769230772,6141.82692307693,6148.84615384614,6155.86538461535,6162.88461538465,6169.90384615386,6176.92307692307,6183.94230769228,6190.96153846158,6197.98076923079,6205,6212.01923076921,6219.03846153842,6226.05769230772,6233.07692307693,6240.09615384614,6247.11538461535,6254.13461538465,6261.15384615386,6268.17307692307,6275.19230769228,6282.21153846158,6289.23076923079,6296.25,6303.26923076921,6310.28846153842,6317.30769230772,6324.32692307693,6331.34615384614,6338.36538461535,6345.38461538465,6352.40384615386,6359.42307692307,6366.44230769228,6373.46153846158,6380.48076923079,6387.5,6394.51923076921,6401.53846153842,6408.55769230772,6415.57692307693,6422.59615384614,6429.61538461535,6436.63461538465,6443.65384615386,6450.67307692307,6457.69230769228,6464.71153846158,6471.73076923079,6478.75,6485.76923076921,6492.78846153842,6499.80769230772,6506.82692307693,6513.84615384614,6520.86538461535,6527.88461538465,6534.90384615386,6541.92307692307,6548.94230769228,6555.96153846158,6562.98076923079,6570};
  // t = 0.0E0;
  // tout = timesJs[0];
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
  for (iout = 0; iout < lengthBuffer; iout++) {
    t = times[iout];
    tout = times[iout + 1];
    lsoda(skel, neq, y, &t, tout, itol, rtol, atol, itask, &istate, iopt, jt,
          iwork1, iwork2, iwork5, iwork6, iwork7, iwork8, iwork9,
          rwork1, rwork5, rwork6, rwork7, data, covar1_p, covar2_p);
    yout[iout] = y[14];
    // printf("%14.6e \n", yout[iout]);
    if (istate <= 0) {
      printf("error istate = %d\n", istate);
      // exit(0);
      n_lsoda_terminate();
      return -1;
    }
    // t = timesJs[iout];
    // tout = timesJs[iout + 1];
    
  } 
  n_lsoda_terminate();

  return 0;
}


