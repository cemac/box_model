/* slsode.f -- translated by f2c (version 20160102).
   You must link the resulting object file with libf2c:
	on Microsoft Windows system, link with libf2c.lib;
	on Linux or Unix systems, link with .../path/to/libf2c.a -lm
	or, if you install libf2c.a in a standard place, with -lf2c -lm
	-- in that order, at the end of the command line, as in
		cc *.o -lf2c -lm
	Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,

		http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

/* Common Block Declarations */

union {
    struct {
	real rowns[209], ccmax, el0, h__, hmin, hmxi, hu, rc, tn, uround;
	integer init, mxstep, mxhnil, nhnil, nslast, nyh, iowns[6], icf, 
		ierpj, iersl, jcur, jstart, kflag, l, lyh, lewt, lacor, lsavf,
		 lwm, liwm, meth, miter, maxord, maxcor, msbp, mxncf, n, nq, 
		nst, nfe, nje, nqu;
    } _1;
    struct {
	real rowns[209], ccmax, el0, h__, hmin, hmxi, hu, rc, tn, uround;
	integer iownd[6], iowns[6], icf, ierpj, iersl, jcur, jstart, kflag, l,
		 lyh, lewt, lacor, lsavf, lwm, liwm, meth, miter, maxord, 
		maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu;
    } _2;
    struct {
	real rls[218];
	integer ils[37];
    } _3;
    struct {
	real conit, crate, el[13], elco[156]	/* was [13][12] */, hold, 
		rmax, tesco[36]	/* was [3][12] */, ccmax, el0, h__, hmin, 
		hmxi, hu, rc, tn, uround;
	integer iownd[6], ialth, ipup, lmax, meo, nqnyh, nslp, icf, ierpj, 
		iersl, jcur, jstart, kflag, l, lyh, lewt, lacor, lsavf, lwm, 
		liwm, meth, miter, maxord, maxcor, msbp, mxncf, n, nq, nst, 
		nfe, nje, nqu;
    } _4;
} sls001_;

#define sls001_1 (sls001_._1)
#define sls001_2 (sls001_._2)
#define sls001_3 (sls001_._3)
#define sls001_4 (sls001_._4)

/* Table of constant values */

static integer c__0 = 0;
static real c_b86 = 1.f;
static integer c__1 = 1;

/* DECK SLSODE */
/* Subroutine */ int slsode_(S_fp f, integer *neq, real *y, real *t, real *
	tout, integer *itol, real *rtol, real *atol, integer *itask, integer *
	istate, integer *iopt, real *rwork, integer *lrw, integer *iwork, 
	integer *liw, U_fp jac, integer *mf)
{
    /* Initialized data */

    static integer mord[2] = { 12,5 };
    static integer mxstp0 = 500;
    static integer mxhnl0 = 10;

    /* System generated locals */
    integer i__1, i__2;
    real r__1, r__2;

    /* Builtin functions */
    double sqrt(doublereal), r_sign(real *, real *);

    /* Local variables */
    static integer i__;
    static real h0;
    static integer i1, i2;
    static real w0;
    static integer ml;
    static real rh;
    static integer mu;
    static real tp;
    static integer lf0;
    static real big;
    static integer kgo;
    static real ayi, hmx, tol, sum, hmax;
    static logical ihit;
    static real ewti, size;
    static integer iflag;
    static real atoli;
    static integer leniw, lenwm, imxer;
    static real tcrit;
    static integer lenrw;
    static real tdist, rtoli, tolsf, tnext;
    extern doublereal rumach_(void);
    extern /* Subroutine */ int sstode_(integer *, real *, real *, integer *, 
	    real *, real *, real *, real *, real *, integer *, S_fp, U_fp, 
	    U_fp, U_fp);
    extern /* Subroutine */ int sprepj_();
    extern /* Subroutine */ int sewset_(integer *, integer *, real *, real *, 
	    real *, real *), sintdy_(real *, integer *, real *, integer *, 
	    real *, integer *);
    extern doublereal svnorm_(integer *, real *, real *);
    extern /* Subroutine */ int ssolsy_();

/* ***BEGIN PROLOGUE  SLSODE */
/* ***PURPOSE  Livermore Solver for Ordinary Differential Equations. */
/*            SLSODE solves the initial-value problem for stiff or */
/*            nonstiff systems of first-order ODE's, */
/*               dy/dt = f(t,y),   or, in component form, */
/*               dy(i)/dt = f(i) = f(i,t,y(1),y(2),...,y(N)),  i=1,...,N. */
/* ***CATEGORY  I1A */
/* ***TYPE      SINGLE PRECISION (SLSODE-S, DLSODE-D) */
/* ***KEYWORDS  ORDINARY DIFFERENTIAL EQUATIONS, INITIAL VALUE PROBLEM, */
/*             STIFF, NONSTIFF */
/* ***AUTHOR  Hindmarsh, Alan C., (LLNL) */
/*             Center for Applied Scientific Computing, L-561 */
/*             Lawrence Livermore National Laboratory */
/*             Livermore, CA 94551. */
/* ***DESCRIPTION */

/*     NOTE: The "Usage" and "Arguments" sections treat only a subset of */
/*           available options, in condensed fashion.  The options */
/*           covered and the information supplied will support most */
/*           standard uses of SLSODE. */

/*           For more sophisticated uses, full details on all options are */
/*           given in the concluding section, headed "Long Description." */
/*           A synopsis of the SLSODE Long Description is provided at the */
/*           beginning of that section; general topics covered are: */
/*           - Elements of the call sequence; optional input and output */
/*           - Optional supplemental routines in the SLSODE package */
/*           - internal COMMON block */

/* *Usage: */
/*     Communication between the user and the SLSODE package, for normal */
/*     situations, is summarized here.  This summary describes a subset */
/*     of the available options.  See "Long Description" for complete */
/*     details, including optional communication, nonstandard options, */
/*     and instructions for special situations. */

/*     A sample program is given in the "Examples" section. */

/*     Refer to the argument descriptions for the definitions of the */
/*     quantities that appear in the following sample declarations. */

/*     For MF = 10, */
/*        PARAMETER  (LRW = 20 + 16*NEQ,           LIW = 20) */
/*     For MF = 21 or 22, */
/*        PARAMETER  (LRW = 22 +  9*NEQ + NEQ**2,  LIW = 20 + NEQ) */
/*     For MF = 24 or 25, */
/*        PARAMETER  (LRW = 22 + 10*NEQ + (2*ML+MU)*NEQ, */
/*       *                                         LIW = 20 + NEQ) */

/*        EXTERNAL F, JAC */
/*        INTEGER  NEQ, ITOL, ITASK, ISTATE, IOPT, LRW, IWORK(LIW), */
/*       *         LIW, MF */
/*        REAL Y(NEQ), T, TOUT, RTOL, ATOL(ntol), RWORK(LRW) */

/*        CALL SLSODE (F, NEQ, Y, T, TOUT, ITOL, RTOL, ATOL, ITASK, */
/*       *            ISTATE, IOPT, RWORK, LRW, IWORK, LIW, JAC, MF) */

/* *Arguments: */
/*     F     :EXT    Name of subroutine for right-hand-side vector f. */
/*                   This name must be declared EXTERNAL in calling */
/*                   program.  The form of F must be: */

/*                   SUBROUTINE  F (NEQ, T, Y, YDOT) */
/*                   INTEGER  NEQ */
/*                   REAL T, Y(*), YDOT(*) */

/*                   The inputs are NEQ, T, Y.  F is to set */

/*                   YDOT(i) = f(i,T,Y(1),Y(2),...,Y(NEQ)), */
/*                                                     i = 1, ..., NEQ . */

/*     NEQ   :IN     Number of first-order ODE's. */

/*     Y     :INOUT  Array of values of the y(t) vector, of length NEQ. */
/*                   Input:  For the first call, Y should contain the */
/*                           values of y(t) at t = T. (Y is an input */
/*                           variable only if ISTATE = 1.) */
/*                   Output: On return, Y will contain the values at the */
/*                           new t-value. */

/*     T     :INOUT  Value of the independent variable.  On return it */
/*                   will be the current value of t (normally TOUT). */

/*     TOUT  :IN     Next point where output is desired (.NE. T). */

/*     ITOL  :IN     1 or 2 according as ATOL (below) is a scalar or */
/*                   an array. */

/*     RTOL  :IN     Relative tolerance parameter (scalar). */

/*     ATOL  :IN     Absolute tolerance parameter (scalar or array). */
/*                   If ITOL = 1, ATOL need not be dimensioned. */
/*                   If ITOL = 2, ATOL must be dimensioned at least NEQ. */

/*                   The estimated local error in Y(i) will be controlled */
/*                   so as to be roughly less (in magnitude) than */

/*                   EWT(i) = RTOL*ABS(Y(i)) + ATOL     if ITOL = 1, or */
/*                   EWT(i) = RTOL*ABS(Y(i)) + ATOL(i)  if ITOL = 2. */

/*                   Thus the local error test passes if, in each */
/*                   component, either the absolute error is less than */
/*                   ATOL (or ATOL(i)), or the relative error is less */
/*                   than RTOL. */

/*                   Use RTOL = 0.0 for pure absolute error control, and */
/*                   use ATOL = 0.0 (or ATOL(i) = 0.0) for pure relative */
/*                   error control.  Caution:  Actual (global) errors may */
/*                   exceed these local tolerances, so choose them */
/*                   conservatively. */

/*     ITASK :IN     Flag indicating the task SLSODE is to perform. */
/*                   Use ITASK = 1 for normal computation of output */
/*                   values of y at t = TOUT. */

/*     ISTATE:INOUT  Index used for input and output to specify the state */
/*                   of the calculation. */
/*                   Input: */
/*                    1   This is the first call for a problem. */
/*                    2   This is a subsequent call. */
/*                   Output: */
/*                    1   Nothing was done, as TOUT was equal to T. */
/*                    2   SLSODE was successful (otherwise, negative). */
/*                        Note that ISTATE need not be modified after a */
/*                        successful return. */
/*                   -1   Excess work done on this call (perhaps wrong */
/*                        MF). */
/*                   -2   Excess accuracy requested (tolerances too */
/*                        small). */
/*                   -3   Illegal input detected (see printed message). */
/*                   -4   Repeated error test failures (check all */
/*                        inputs). */
/*                   -5   Repeated convergence failures (perhaps bad */
/*                        Jacobian supplied or wrong choice of MF or */
/*                        tolerances). */
/*                   -6   Error weight became zero during problem */
/*                        (solution component i vanished, and ATOL or */
/*                        ATOL(i) = 0.). */

/*     IOPT  :IN     Flag indicating whether optional inputs are used: */
/*                   0   No. */
/*                   1   Yes.  (See "Optional inputs" under "Long */
/*                       Description," Part 1.) */

/*     RWORK :WORK   Real work array of length at least: */
/*                   20 + 16*NEQ                    for MF = 10, */
/*                   22 +  9*NEQ + NEQ**2           for MF = 21 or 22, */
/*                   22 + 10*NEQ + (2*ML + MU)*NEQ  for MF = 24 or 25. */

/*     LRW   :IN     Declared length of RWORK (in user's DIMENSION */
/*                   statement). */

/*     IWORK :WORK   Integer work array of length at least: */
/*                   20        for MF = 10, */
/*                   20 + NEQ  for MF = 21, 22, 24, or 25. */

/*                   If MF = 24 or 25, input in IWORK(1),IWORK(2) the */
/*                   lower and upper Jacobian half-bandwidths ML,MU. */

/*                   On return, IWORK contains information that may be */
/*                   of interest to the user: */

/*            Name   Location   Meaning */
/*            -----  ---------  ----------------------------------------- */
/*            NST    IWORK(11)  Number of steps taken for the problem so */
/*                              far. */
/*            NFE    IWORK(12)  Number of f evaluations for the problem */
/*                              so far. */
/*            NJE    IWORK(13)  Number of Jacobian evaluations (and of */
/*                              matrix LU decompositions) for the problem */
/*                              so far. */
/*            NQU    IWORK(14)  Method order last used (successfully). */
/*            LENRW  IWORK(17)  Length of RWORK actually required.  This */
/*                              is defined on normal returns and on an */
/*                              illegal input return for insufficient */
/*                              storage. */
/*            LENIW  IWORK(18)  Length of IWORK actually required.  This */
/*                              is defined on normal returns and on an */
/*                              illegal input return for insufficient */
/*                              storage. */

/*     LIW   :IN     Declared length of IWORK (in user's DIMENSION */
/*                   statement). */

/*     JAC   :EXT    Name of subroutine for Jacobian matrix (MF = */
/*                   21 or 24).  If used, this name must be declared */
/*                   EXTERNAL in calling program.  If not used, pass a */
/*                   dummy name.  The form of JAC must be: */

/*                   SUBROUTINE JAC (NEQ, T, Y, ML, MU, PD, NROWPD) */
/*                   INTEGER NEQ, ML, MU, NROWPD */
/*                   REAL T, Y(*), PD(NROWPD,*) */

/*                   See item c, under "Description" below for more */
/*                   information about JAC. */

/*     MF    :IN     Method flag.  Standard values are: */
/*                   10  Nonstiff (Adams) method, no Jacobian used. */
/*                   21  Stiff (BDF) method, user-supplied full Jacobian. */
/*                   22  Stiff method, internally generated full */
/*                       Jacobian. */
/*                   24  Stiff method, user-supplied banded Jacobian. */
/*                   25  Stiff method, internally generated banded */
/*                       Jacobian. */

/* *Description: */
/*     SLSODE solves the initial value problem for stiff or nonstiff */
/*     systems of first-order ODE's, */

/*        dy/dt = f(t,y) , */

/*     or, in component form, */

/*        dy(i)/dt = f(i) = f(i,t,y(1),y(2),...,y(NEQ)) */
/*                                                  (i = 1, ..., NEQ) . */

/*     SLSODE is a package based on the GEAR and GEARB packages, and on */
/*     the October 23, 1978, version of the tentative ODEPACK user */
/*     interface standard, with minor modifications. */

/*     The steps in solving such a problem are as follows. */

/*     a. First write a subroutine of the form */

/*           SUBROUTINE  F (NEQ, T, Y, YDOT) */
/*           INTEGER  NEQ */
/*           REAL T, Y(*), YDOT(*) */

/*        which supplies the vector function f by loading YDOT(i) with */
/*        f(i). */

/*     b. Next determine (or guess) whether or not the problem is stiff. */
/*        Stiffness occurs when the Jacobian matrix df/dy has an */
/*        eigenvalue whose real part is negative and large in magnitude */
/*        compared to the reciprocal of the t span of interest.  If the */
/*        problem is nonstiff, use method flag MF = 10.  If it is stiff, */
/*        there are four standard choices for MF, and SLSODE requires the */
/*        Jacobian matrix in some form.  This matrix is regarded either */
/*        as full (MF = 21 or 22), or banded (MF = 24 or 25).  In the */
/*        banded case, SLSODE requires two half-bandwidth parameters ML */
/*        and MU. These are, respectively, the widths of the lower and */
/*        upper parts of the band, excluding the main diagonal.  Thus the */
/*        band consists of the locations (i,j) with */

/*           i - ML <= j <= i + MU , */

/*        and the full bandwidth is ML + MU + 1 . */

/*     c. If the problem is stiff, you are encouraged to supply the */
/*        Jacobian directly (MF = 21 or 24), but if this is not feasible, */
/*        SLSODE will compute it internally by difference quotients (MF = */
/*        22 or 25).  If you are supplying the Jacobian, write a */
/*        subroutine of the form */

/*           SUBROUTINE  JAC (NEQ, T, Y, ML, MU, PD, NROWPD) */
/*           INTEGER  NEQ, ML, MU, NRWOPD */
/*           REAL T, Y(*), PD(NROWPD,*) */

/*        which provides df/dy by loading PD as follows: */
/*        - For a full Jacobian (MF = 21), load PD(i,j) with df(i)/dy(j), */
/*          the partial derivative of f(i) with respect to y(j).  (Ignore */
/*          the ML and MU arguments in this case.) */
/*        - For a banded Jacobian (MF = 24), load PD(i-j+MU+1,j) with */
/*          df(i)/dy(j); i.e., load the diagonal lines of df/dy into the */
/*          rows of PD from the top down. */
/*        - In either case, only nonzero elements need be loaded. */

/*     d. Write a main program that calls subroutine SLSODE once for each */
/*        point at which answers are desired.  This should also provide */
/*        for possible use of logical unit 6 for output of error messages */
/*        by SLSODE. */

/*        Before the first call to SLSODE, set ISTATE = 1, set Y and T to */
/*        the initial values, and set TOUT to the first output point.  To */
/*        continue the integration after a successful return, simply */
/*        reset TOUT and call SLSODE again.  No other parameters need be */
/*        reset. */

/* *Examples: */
/*     The following is a simple example problem, with the coding needed */
/*     for its solution by SLSODE. The problem is from chemical kinetics, */
/*     and consists of the following three rate equations: */

/*        dy1/dt = -.04*y1 + 1.E4*y2*y3 */
/*        dy2/dt = .04*y1 - 1.E4*y2*y3 - 3.E7*y2**2 */
/*        dy3/dt = 3.E7*y2**2 */

/*     on the interval from t = 0.0 to t = 4.E10, with initial conditions */
/*     y1 = 1.0, y2 = y3 = 0. The problem is stiff. */

/*     The following coding solves this problem with SLSODE, using */
/*     MF = 21 and printing results at t = .4, 4., ..., 4.E10.  It uses */
/*     ITOL = 2 and ATOL much smaller for y2 than for y1 or y3 because y2 */
/*     has much smaller values.  At the end of the run, statistical */
/*     quantities of interest are printed. */

/*        EXTERNAL  FEX, JEX */
/*        INTEGER  IOPT, IOUT, ISTATE, ITASK, ITOL, IWORK(23), LIW, LRW, */
/*       *         MF, NEQ */
/*        REAL  ATOL(3), RTOL, RWORK(58), T, TOUT, Y(3) */
/*        NEQ = 3 */
/*        Y(1) = 1. */
/*        Y(2) = 0. */
/*        Y(3) = 0. */
/*        T = 0. */
/*        TOUT = .4 */
/*        ITOL = 2 */
/*        RTOL = 1.E-4 */
/*        ATOL(1) = 1.E-6 */
/*        ATOL(2) = 1.E-10 */
/*        ATOL(3) = 1.E-6 */
/*        ITASK = 1 */
/*        ISTATE = 1 */
/*        IOPT = 0 */
/*        LRW = 58 */
/*        LIW = 23 */
/*        MF = 21 */
/*        DO 40 IOUT = 1,12 */
/*          CALL SLSODE (FEX, NEQ, Y, T, TOUT, ITOL, RTOL, ATOL, ITASK, */
/*       *               ISTATE, IOPT, RWORK, LRW, IWORK, LIW, JEX, MF) */
/*          WRITE(6,20)  T, Y(1), Y(2), Y(3) */
/*    20    FORMAT(' At t =',E12.4,'   y =',3E14.6) */
/*          IF (ISTATE .LT. 0)  GO TO 80 */
/*    40    TOUT = TOUT*10. */
/*        WRITE(6,60)  IWORK(11), IWORK(12), IWORK(13) */
/*    60  FORMAT(/' No. steps =',i4,',  No. f-s =',i4,',  No. J-s =',i4) */
/*        STOP */
/*    80  WRITE(6,90)  ISTATE */
/*    90  FORMAT(///' Error halt.. ISTATE =',I3) */
/*        STOP */
/*        END */

/*        SUBROUTINE  FEX (NEQ, T, Y, YDOT) */
/*        INTEGER  NEQ */
/*        REAL  T, Y(3), YDOT(3) */
/*        YDOT(1) = -.04*Y(1) + 1.E4*Y(2)*Y(3) */
/*        YDOT(3) = 3.E7*Y(2)*Y(2) */
/*        YDOT(2) = -YDOT(1) - YDOT(3) */
/*        RETURN */
/*        END */

/*        SUBROUTINE  JEX (NEQ, T, Y, ML, MU, PD, NRPD) */
/*        INTEGER  NEQ, ML, MU, NRPD */
/*        REAL  T, Y(3), PD(NRPD,3) */
/*        PD(1,1) = -.04 */
/*        PD(1,2) = 1.E4*Y(3) */
/*        PD(1,3) = 1.E4*Y(2) */
/*        PD(2,1) = .04 */
/*        PD(2,3) = -PD(1,3) */
/*        PD(3,2) = 6.E7*Y(2) */
/*        PD(2,2) = -PD(1,2) - PD(3,2) */
/*        RETURN */
/*        END */

/*     The output from this program (on a Cray-1 in single precision) */
/*     is as follows. */

/*     At t =  4.0000e-01   y =  9.851726e-01  3.386406e-05  1.479357e-02 */
/*     At t =  4.0000e+00   y =  9.055142e-01  2.240418e-05  9.446344e-02 */
/*     At t =  4.0000e+01   y =  7.158050e-01  9.184616e-06  2.841858e-01 */
/*     At t =  4.0000e+02   y =  4.504846e-01  3.222434e-06  5.495122e-01 */
/*     At t =  4.0000e+03   y =  1.831701e-01  8.940379e-07  8.168290e-01 */
/*     At t =  4.0000e+04   y =  3.897016e-02  1.621193e-07  9.610297e-01 */
/*     At t =  4.0000e+05   y =  4.935213e-03  1.983756e-08  9.950648e-01 */
/*     At t =  4.0000e+06   y =  5.159269e-04  2.064759e-09  9.994841e-01 */
/*     At t =  4.0000e+07   y =  5.306413e-05  2.122677e-10  9.999469e-01 */
/*     At t =  4.0000e+08   y =  5.494530e-06  2.197825e-11  9.999945e-01 */
/*     At t =  4.0000e+09   y =  5.129458e-07  2.051784e-12  9.999995e-01 */
/*     At t =  4.0000e+10   y = -7.170603e-08 -2.868241e-13  1.000000e+00 */

/*     No. steps = 330,  No. f-s = 405,  No. J-s = 69 */

/* *Accuracy: */
/*     The accuracy of the solution depends on the choice of tolerances */
/*     RTOL and ATOL.  Actual (global) errors may exceed these local */
/*     tolerances, so choose them conservatively. */

/* *Cautions: */
/*     The work arrays should not be altered between calls to SLSODE for */
/*     the same problem, except possibly for the conditional and optional */
/*     inputs. */

/* *Portability: */
/*     Since NEQ is dimensioned inside SLSODE, some compilers may object */
/*     to a call to SLSODE with NEQ a scalar variable.  In this event, */
/*     use DIMENSION NEQ(1).  Similar remarks apply to RTOL and ATOL. */

/*     Note to Cray users: */
/*     For maximum efficiency, use the CFT77 compiler.  Appropriate */
/*     compiler optimization directives have been inserted for CFT77. */

/* *Reference: */
/*     Alan C. Hindmarsh, "ODEPACK, A Systematized Collection of ODE */
/*     Solvers," in Scientific Computing, R. S. Stepleman, et al., Eds. */
/*     (North-Holland, Amsterdam, 1983), pp. 55-64. */

/* *Long Description: */
/*     The following complete description of the user interface to */
/*     SLSODE consists of four parts: */

/*     1.  The call sequence to subroutine SLSODE, which is a driver */
/*         routine for the solver.  This includes descriptions of both */
/*         the call sequence arguments and user-supplied routines. */
/*         Following these descriptions is a description of optional */
/*         inputs available through the call sequence, and then a */
/*         description of optional outputs in the work arrays. */

/*     2.  Descriptions of other routines in the SLSODE package that may */
/*         be (optionally) called by the user.  These provide the ability */
/*         to alter error message handling, save and restore the internal */
/*         COMMON, and obtain specified derivatives of the solution y(t). */

/*     3.  Descriptions of COMMON block to be declared in overlay or */
/*         similar environments, or to be saved when doing an interrupt */
/*         of the problem and continued solution later. */

/*     4.  Description of two routines in the SLSODE package, either of */
/*         which the user may replace with his own version, if desired. */
/*         These relate to the measurement of errors. */


/*                         Part 1.  Call Sequence */
/*                         ---------------------- */

/*     Arguments */
/*     --------- */
/*     The call sequence parameters used for input only are */

/*        F, NEQ, TOUT, ITOL, RTOL, ATOL, ITASK, IOPT, LRW, LIW, JAC, MF, */

/*     and those used for both input and output are */

/*        Y, T, ISTATE. */

/*     The work arrays RWORK and IWORK are also used for conditional and */
/*     optional inputs and optional outputs.  (The term output here */
/*     refers to the return from subroutine SLSODE to the user's calling */
/*     program.) */

/*     The legality of input parameters will be thoroughly checked on the */
/*     initial call for the problem, but not checked thereafter unless a */
/*     change in input parameters is flagged by ISTATE = 3 on input. */

/*     The descriptions of the call arguments are as follows. */

/*     F        The name of the user-supplied subroutine defining the ODE */
/*              system.  The system must be put in the first-order form */
/*              dy/dt = f(t,y), where f is a vector-valued function of */
/*              the scalar t and the vector y. Subroutine F is to compute */
/*              the function f. It is to have the form */

/*                 SUBROUTINE F (NEQ, T, Y, YDOT) */
/*                 REAL T, Y(*), YDOT(*) */

/*              where NEQ, T, and Y are input, and the array YDOT = */
/*              f(T,Y) is output.  Y and YDOT are arrays of length NEQ. */
/*              Subroutine F should not alter Y(1),...,Y(NEQ).  F must be */
/*              declared EXTERNAL in the calling program. */

/*              Subroutine F may access user-defined quantities in */
/*              NEQ(2),... and/or in Y(NEQ(1)+1),..., if NEQ is an array */
/*              (dimensioned in F) and/or Y has length exceeding NEQ(1). */
/*              See the descriptions of NEQ and Y below. */

/*              If quantities computed in the F routine are needed */
/*              externally to SLSODE, an extra call to F should be made */
/*              for this purpose, for consistent and accurate results. */
/*              If only the derivative dy/dt is needed, use SINTDY */
/*              instead. */

/*     NEQ      The size of the ODE system (number of first-order */
/*              ordinary differential equations).  Used only for input. */
/*              NEQ may be decreased, but not increased, during the */
/*              problem.  If NEQ is decreased (with ISTATE = 3 on input), */
/*              the remaining components of Y should be left undisturbed, */
/*              if these are to be accessed in F and/or JAC. */

/*              Normally, NEQ is a scalar, and it is generally referred */
/*              to as a scalar in this user interface description. */
/*              However, NEQ may be an array, with NEQ(1) set to the */
/*              system size.  (The SLSODE package accesses only NEQ(1).) */
/*              In either case, this parameter is passed as the NEQ */
/*              argument in all calls to F and JAC.  Hence, if it is an */
/*              array, locations NEQ(2),... may be used to store other */
/*              integer data and pass it to F and/or JAC.  Subroutines */
/*              F and/or JAC must include NEQ in a DIMENSION statement */
/*              in that case. */

/*     Y        A real array for the vector of dependent variables, of */
/*              length NEQ or more.  Used for both input and output on */
/*              the first call (ISTATE = 1), and only for output on */
/*              other calls.  On the first call, Y must contain the */
/*              vector of initial values.  On output, Y contains the */
/*              computed solution vector, evaluated at T. If desired, */
/*              the Y array may be used for other purposes between */
/*              calls to the solver. */

/*              This array is passed as the Y argument in all calls to F */
/*              and JAC.  Hence its length may exceed NEQ, and locations */
/*              Y(NEQ+1),... may be used to store other real data and */
/*              pass it to F and/or JAC.  (The SLSODE package accesses */
/*              only Y(1),...,Y(NEQ).) */

/*     T        The independent variable.  On input, T is used only on */
/*              the first call, as the initial point of the integration. */
/*              On output, after each call, T is the value at which a */
/*              computed solution Y is evaluated (usually the same as */
/*              TOUT).  On an error return, T is the farthest point */
/*              reached. */

/*     TOUT     The next value of T at which a computed solution is */
/*              desired.  Used only for input. */

/*              When starting the problem (ISTATE = 1), TOUT may be equal */
/*              to T for one call, then should not equal T for the next */
/*              call.  For the initial T, an input value of TOUT .NE. T */
/*              is used in order to determine the direction of the */
/*              integration (i.e., the algebraic sign of the step sizes) */
/*              and the rough scale of the problem.  Integration in */
/*              either direction (forward or backward in T) is permitted. */

/*              If ITASK = 2 or 5 (one-step modes), TOUT is ignored */
/*              after the first call (i.e., the first call with */
/*              TOUT .NE. T).  Otherwise, TOUT is required on every call. */

/*              If ITASK = 1, 3, or 4, the values of TOUT need not be */
/*              monotone, but a value of TOUT which backs up is limited */
/*              to the current internal T interval, whose endpoints are */
/*              TCUR - HU and TCUR.  (See "Optional Outputs" below for */
/*              TCUR and HU.) */


/*     ITOL     An indicator for the type of error control.  See */
/*              description below under ATOL.  Used only for input. */

/*     RTOL     A relative error tolerance parameter, either a scalar or */
/*              an array of length NEQ.  See description below under */
/*              ATOL.  Input only. */

/*     ATOL     An absolute error tolerance parameter, either a scalar or */
/*              an array of length NEQ.  Input only. */

/*              The input parameters ITOL, RTOL, and ATOL determine the */
/*              error control performed by the solver.  The solver will */
/*              control the vector e = (e(i)) of estimated local errors */
/*              in Y, according to an inequality of the form */

/*                 rms-norm of ( e(i)/EWT(i) ) <= 1, */

/*              where */

/*                 EWT(i) = RTOL(i)*ABS(Y(i)) + ATOL(i), */

/*              and the rms-norm (root-mean-square norm) here is */

/*                 rms-norm(v) = SQRT(sum v(i)**2 / NEQ). */

/*              Here EWT = (EWT(i)) is a vector of weights which must */
/*              always be positive, and the values of RTOL and ATOL */
/*              should all be nonnegative.  The following table gives the */
/*              types (scalar/array) of RTOL and ATOL, and the */
/*              corresponding form of EWT(i). */

/*              ITOL    RTOL      ATOL      EWT(i) */
/*              ----    ------    ------    ----------------------------- */
/*              1       scalar    scalar    RTOL*ABS(Y(i)) + ATOL */
/*              2       scalar    array     RTOL*ABS(Y(i)) + ATOL(i) */
/*              3       array     scalar    RTOL(i)*ABS(Y(i)) + ATOL */
/*              4       array     array     RTOL(i)*ABS(Y(i)) + ATOL(i) */

/*              When either of these parameters is a scalar, it need not */
/*              be dimensioned in the user's calling program. */

/*              If none of the above choices (with ITOL, RTOL, and ATOL */
/*              fixed throughout the problem) is suitable, more general */
/*              error controls can be obtained by substituting */
/*              user-supplied routines for the setting of EWT and/or for */
/*              the norm calculation.  See Part 4 below. */

/*              If global errors are to be estimated by making a repeated */
/*              run on the same problem with smaller tolerances, then all */
/*              components of RTOL and ATOL (i.e., of EWT) should be */
/*              scaled down uniformly. */

/*     ITASK    An index specifying the task to be performed.  Input */
/*              only.  ITASK has the following values and meanings: */
/*              1   Normal computation of output values of y(t) at */
/*                  t = TOUT (by overshooting and interpolating). */
/*              2   Take one step only and return. */
/*              3   Stop at the first internal mesh point at or beyond */
/*                  t = TOUT and return. */
/*              4   Normal computation of output values of y(t) at */
/*                  t = TOUT but without overshooting t = TCRIT.  TCRIT */
/*                  must be input as RWORK(1).  TCRIT may be equal to or */
/*                  beyond TOUT, but not behind it in the direction of */
/*                  integration.  This option is useful if the problem */
/*                  has a singularity at or beyond t = TCRIT. */
/*              5   Take one step, without passing TCRIT, and return. */
/*                  TCRIT must be input as RWORK(1). */

/*              Note:  If ITASK = 4 or 5 and the solver reaches TCRIT */
/*              (within roundoff), it will return T = TCRIT (exactly) to */
/*              indicate this (unless ITASK = 4 and TOUT comes before */
/*              TCRIT, in which case answers at T = TOUT are returned */
/*              first). */

/*     ISTATE   An index used for input and output to specify the state */
/*              of the calculation. */

/*              On input, the values of ISTATE are as follows: */
/*              1   This is the first call for the problem */
/*                  (initializations will be done).  See "Note" below. */
/*              2   This is not the first call, and the calculation is to */
/*                  continue normally, with no change in any input */
/*                  parameters except possibly TOUT and ITASK.  (If ITOL, */
/*                  RTOL, and/or ATOL are changed between calls with */
/*                  ISTATE = 2, the new values will be used but not */
/*                  tested for legality.) */
/*              3   This is not the first call, and the calculation is to */
/*                  continue normally, but with a change in input */
/*                  parameters other than TOUT and ITASK.  Changes are */
/*                  allowed in NEQ, ITOL, RTOL, ATOL, IOPT, LRW, LIW, MF, */
/*                  ML, MU, and any of the optional inputs except H0. */
/*                  (See IWORK description for ML and MU.) */

/*              Note:  A preliminary call with TOUT = T is not counted as */
/*              a first call here, as no initialization or checking of */
/*              input is done.  (Such a call is sometimes useful for the */
/*              purpose of outputting the initial conditions.)  Thus the */
/*              first call for which TOUT .NE. T requires ISTATE = 1 on */
/*              input. */

/*              On output, ISTATE has the following values and meanings: */
/*               1  Nothing was done, as TOUT was equal to T with */
/*                  ISTATE = 1 on input. */
/*               2  The integration was performed successfully. */
/*              -1  An excessive amount of work (more than MXSTEP steps) */
/*                  was done on this call, before completing the */
/*                  requested task, but the integration was otherwise */
/*                  successful as far as T. (MXSTEP is an optional input */
/*                  and is normally 500.)  To continue, the user may */
/*                  simply reset ISTATE to a value >1 and call again (the */
/*                  excess work step counter will be reset to 0).  In */
/*                  addition, the user may increase MXSTEP to avoid this */
/*                  error return; see "Optional Inputs" below. */
/*              -2  Too much accuracy was requested for the precision of */
/*                  the machine being used.  This was detected before */
/*                  completing the requested task, but the integration */
/*                  was successful as far as T. To continue, the */
/*                  tolerance parameters must be reset, and ISTATE must */
/*                  be set to 3. The optional output TOLSF may be used */
/*                  for this purpose.  (Note:  If this condition is */
/*                  detected before taking any steps, then an illegal */
/*                  input return (ISTATE = -3) occurs instead.) */
/*              -3  Illegal input was detected, before taking any */
/*                  integration steps.  See written message for details. */
/*                  (Note:  If the solver detects an infinite loop of */
/*                  calls to the solver with illegal input, it will cause */
/*                  the run to stop.) */
/*              -4  There were repeated error-test failures on one */
/*                  attempted step, before completing the requested task, */
/*                  but the integration was successful as far as T.  The */
/*                  problem may have a singularity, or the input may be */
/*                  inappropriate. */
/*              -5  There were repeated convergence-test failures on one */
/*                  attempted step, before completing the requested task, */
/*                  but the integration was successful as far as T. This */
/*                  may be caused by an inaccurate Jacobian matrix, if */
/*                  one is being used. */
/*              -6  EWT(i) became zero for some i during the integration. */
/*                  Pure relative error control (ATOL(i)=0.0) was */
/*                  requested on a variable which has now vanished.  The */
/*                  integration was successful as far as T. */

/*              Note:  Since the normal output value of ISTATE is 2, it */
/*              does not need to be reset for normal continuation.  Also, */
/*              since a negative input value of ISTATE will be regarded */
/*              as illegal, a negative output value requires the user to */
/*              change it, and possibly other inputs, before calling the */
/*              solver again. */

/*     IOPT     An integer flag to specify whether any optional inputs */
/*              are being used on this call.  Input only.  The optional */
/*              inputs are listed under a separate heading below. */
/*              0   No optional inputs are being used.  Default values */
/*                  will be used in all cases. */
/*              1   One or more optional inputs are being used. */

/*     RWORK    A real working array (single precision).  The length of */
/*              RWORK must be at least */

/*                 20 + NYH*(MAXORD + 1) + 3*NEQ + LWM */

/*              where */
/*                 NYH = the initial value of NEQ, */
/*              MAXORD = 12 (if METH = 1) or 5 (if METH = 2) (unless a */
/*                       smaller value is given as an optional input), */
/*                 LWM = 0           if MITER = 0, */
/*                 LWM = NEQ**2 + 2  if MITER = 1 or 2, */
/*                 LWM = NEQ + 2     if MITER = 3, and */
/*                 LWM = (2*ML + MU + 1)*NEQ + 2 */
/*                                   if MITER = 4 or 5. */
/*              (See the MF description below for METH and MITER.) */

/*              Thus if MAXORD has its default value and NEQ is constant, */
/*              this length is: */
/*              20 + 16*NEQ                    for MF = 10, */
/*              22 + 16*NEQ + NEQ**2           for MF = 11 or 12, */
/*              22 + 17*NEQ                    for MF = 13, */
/*              22 + 17*NEQ + (2*ML + MU)*NEQ  for MF = 14 or 15, */
/*              20 +  9*NEQ                    for MF = 20, */
/*              22 +  9*NEQ + NEQ**2           for MF = 21 or 22, */
/*              22 + 10*NEQ                    for MF = 23, */
/*              22 + 10*NEQ + (2*ML + MU)*NEQ  for MF = 24 or 25. */

/*              The first 20 words of RWORK are reserved for conditional */
/*              and optional inputs and optional outputs. */

/*              The following word in RWORK is a conditional input: */
/*              RWORK(1) = TCRIT, the critical value of t which the */
/*                         solver is not to overshoot.  Required if ITASK */
/*                         is 4 or 5, and ignored otherwise.  See ITASK. */

/*     LRW      The length of the array RWORK, as declared by the user. */
/*              (This will be checked by the solver.) */

/*     IWORK    An integer work array.  Its length must be at least */
/*              20       if MITER = 0 or 3 (MF = 10, 13, 20, 23), or */
/*              20 + NEQ otherwise (MF = 11, 12, 14, 15, 21, 22, 24, 25). */
/*              (See the MF description below for MITER.)  The first few */
/*              words of IWORK are used for conditional and optional */
/*              inputs and optional outputs. */

/*              The following two words in IWORK are conditional inputs: */
/*              IWORK(1) = ML   These are the lower and upper half- */
/*              IWORK(2) = MU   bandwidths, respectively, of the banded */
/*                              Jacobian, excluding the main diagonal. */
/*                         The band is defined by the matrix locations */
/*                         (i,j) with i - ML <= j <= i + MU. ML and MU */
/*                         must satisfy 0 <= ML,MU <= NEQ - 1. These are */
/*                         required if MITER is 4 or 5, and ignored */
/*                         otherwise.  ML and MU may in fact be the band */
/*                         parameters for a matrix to which df/dy is only */
/*                         approximately equal. */

/*     LIW      The length of the array IWORK, as declared by the user. */
/*              (This will be checked by the solver.) */

/*     Note:  The work arrays must not be altered between calls to SLSODE */
/*     for the same problem, except possibly for the conditional and */
/*     optional inputs, and except for the last 3*NEQ words of RWORK. */
/*     The latter space is used for internal scratch space, and so is */
/*     available for use by the user outside SLSODE between calls, if */
/*     desired (but not for use by F or JAC). */

/*     JAC      The name of the user-supplied routine (MITER = 1 or 4) to */
/*              compute the Jacobian matrix, df/dy, as a function of the */
/*              scalar t and the vector y.  (See the MF description below */
/*              for MITER.)  It is to have the form */

/*                 SUBROUTINE JAC (NEQ, T, Y, ML, MU, PD, NROWPD) */
/*                 REAL T, Y(*), PD(NROWPD,*) */

/*              where NEQ, T, Y, ML, MU, and NROWPD are input and the */
/*              array PD is to be loaded with partial derivatives */
/*              (elements of the Jacobian matrix) on output.  PD must be */
/*              given a first dimension of NROWPD.  T and Y have the same */
/*              meaning as in subroutine F. */

/*              In the full matrix case (MITER = 1), ML and MU are */
/*              ignored, and the Jacobian is to be loaded into PD in */
/*              columnwise manner, with df(i)/dy(j) loaded into PD(i,j). */

/*              In the band matrix case (MITER = 4), the elements within */
/*              the band are to be loaded into PD in columnwise manner, */
/*              with diagonal lines of df/dy loaded into the rows of PD. */
/*              Thus df(i)/dy(j) is to be loaded into PD(i-j+MU+1,j).  ML */
/*              and MU are the half-bandwidth parameters (see IWORK). */
/*              The locations in PD in the two triangular areas which */
/*              correspond to nonexistent matrix elements can be ignored */
/*              or loaded arbitrarily, as they are overwritten by SLSODE. */

/*              JAC need not provide df/dy exactly. A crude approximation */
/*              (possibly with a smaller bandwidth) will do. */

/*              In either case, PD is preset to zero by the solver, so */
/*              that only the nonzero elements need be loaded by JAC. */
/*              Each call to JAC is preceded by a call to F with the same */
/*              arguments NEQ, T, and Y. Thus to gain some efficiency, */
/*              intermediate quantities shared by both calculations may */
/*              be saved in a user COMMON block by F and not recomputed */
/*              by JAC, if desired.  Also, JAC may alter the Y array, if */
/*              desired.  JAC must be declared EXTERNAL in the calling */
/*              program. */

/*              Subroutine JAC may access user-defined quantities in */
/*              NEQ(2),... and/or in Y(NEQ(1)+1),... if NEQ is an array */
/*              (dimensioned in JAC) and/or Y has length exceeding */
/*              NEQ(1).  See the descriptions of NEQ and Y above. */

/*     MF       The method flag.  Used only for input.  The legal values */
/*              of MF are 10, 11, 12, 13, 14, 15, 20, 21, 22, 23, 24, */
/*              and 25.  MF has decimal digits METH and MITER: */
/*                 MF = 10*METH + MITER . */

/*              METH indicates the basic linear multistep method: */
/*              1   Implicit Adams method. */
/*              2   Method based on backward differentiation formulas */
/*                  (BDF's). */

/*              MITER indicates the corrector iteration method: */
/*              0   Functional iteration (no Jacobian matrix is */
/*                  involved). */
/*              1   Chord iteration with a user-supplied full (NEQ by */
/*                  NEQ) Jacobian. */
/*              2   Chord iteration with an internally generated */
/*                  (difference quotient) full Jacobian (using NEQ */
/*                  extra calls to F per df/dy value). */
/*              3   Chord iteration with an internally generated */
/*                  diagonal Jacobian approximation (using one extra call */
/*                  to F per df/dy evaluation). */
/*              4   Chord iteration with a user-supplied banded Jacobian. */
/*              5   Chord iteration with an internally generated banded */
/*                  Jacobian (using ML + MU + 1 extra calls to F per */
/*                  df/dy evaluation). */

/*              If MITER = 1 or 4, the user must supply a subroutine JAC */
/*              (the name is arbitrary) as described above under JAC. */
/*              For other values of MITER, a dummy argument can be used. */

/*     Optional Inputs */
/*     --------------- */
/*     The following is a list of the optional inputs provided for in the */
/*     call sequence.  (See also Part 2.)  For each such input variable, */
/*     this table lists its name as used in this documentation, its */
/*     location in the call sequence, its meaning, and the default value. */
/*     The use of any of these inputs requires IOPT = 1, and in that case */
/*     all of these inputs are examined.  A value of zero for any of */
/*     these optional inputs will cause the default value to be used. */
/*     Thus to use a subset of the optional inputs, simply preload */
/*     locations 5 to 10 in RWORK and IWORK to 0.0 and 0 respectively, */
/*     and then set those of interest to nonzero values. */

/*     Name    Location   Meaning and default value */
/*     ------  ---------  ----------------------------------------------- */
/*     H0      RWORK(5)   Step size to be attempted on the first step. */
/*                        The default value is determined by the solver. */
/*     HMAX    RWORK(6)   Maximum absolute step size allowed.  The */
/*                        default value is infinite. */
/*     HMIN    RWORK(7)   Minimum absolute step size allowed.  The */
/*                        default value is 0.  (This lower bound is not */
/*                        enforced on the final step before reaching */
/*                        TCRIT when ITASK = 4 or 5.) */
/*     MAXORD  IWORK(5)   Maximum order to be allowed.  The default value */
/*                        is 12 if METH = 1, and 5 if METH = 2. (See the */
/*                        MF description above for METH.)  If MAXORD */
/*                        exceeds the default value, it will be reduced */
/*                        to the default value.  If MAXORD is changed */
/*                        during the problem, it may cause the current */
/*                        order to be reduced. */
/*     MXSTEP  IWORK(6)   Maximum number of (internally defined) steps */
/*                        allowed during one call to the solver.  The */
/*                        default value is 500. */
/*     MXHNIL  IWORK(7)   Maximum number of messages printed (per */
/*                        problem) warning that T + H = T on a step */
/*                        (H = step size).  This must be positive to */
/*                        result in a nondefault value.  The default */
/*                        value is 10. */

/*     Optional Outputs */
/*     ---------------- */
/*     As optional additional output from SLSODE, the variables listed */
/*     below are quantities related to the performance of SLSODE which */
/*     are available to the user.  These are communicated by way of the */
/*     work arrays, but also have internal mnemonic names as shown. */
/*     Except where stated otherwise, all of these outputs are defined on */
/*     any successful return from SLSODE, and on any return with ISTATE = */
/*     -1, -2, -4, -5, or -6.  On an illegal input return (ISTATE = -3), */
/*     they will be unchanged from their existing values (if any), except */
/*     possibly for TOLSF, LENRW, and LENIW.  On any error return, */
/*     outputs relevant to the error will be defined, as noted below. */

/*     Name   Location   Meaning */
/*     -----  ---------  ------------------------------------------------ */
/*     HU     RWORK(11)  Step size in t last used (successfully). */
/*     HCUR   RWORK(12)  Step size to be attempted on the next step. */
/*     TCUR   RWORK(13)  Current value of the independent variable which */
/*                       the solver has actually reached, i.e., the */
/*                       current internal mesh point in t. On output, */
/*                       TCUR will always be at least as far as the */
/*                       argument T, but may be farther (if interpolation */
/*                       was done). */
/*     TOLSF  RWORK(14)  Tolerance scale factor, greater than 1.0, */
/*                       computed when a request for too much accuracy */
/*                       was detected (ISTATE = -3 if detected at the */
/*                       start of the problem, ISTATE = -2 otherwise). */
/*                       If ITOL is left unaltered but RTOL and ATOL are */
/*                       uniformly scaled up by a factor of TOLSF for the */
/*                       next call, then the solver is deemed likely to */
/*                       succeed.  (The user may also ignore TOLSF and */
/*                       alter the tolerance parameters in any other way */
/*                       appropriate.) */
/*     NST    IWORK(11)  Number of steps taken for the problem so far. */
/*     NFE    IWORK(12)  Number of F evaluations for the problem so far. */
/*     NJE    IWORK(13)  Number of Jacobian evaluations (and of matrix LU */
/*                       decompositions) for the problem so far. */
/*     NQU    IWORK(14)  Method order last used (successfully). */
/*     NQCUR  IWORK(15)  Order to be attempted on the next step. */
/*     IMXER  IWORK(16)  Index of the component of largest magnitude in */
/*                       the weighted local error vector ( e(i)/EWT(i) ), */
/*                       on an error return with ISTATE = -4 or -5. */
/*     LENRW  IWORK(17)  Length of RWORK actually required.  This is */
/*                       defined on normal returns and on an illegal */
/*                       input return for insufficient storage. */
/*     LENIW  IWORK(18)  Length of IWORK actually required.  This is */
/*                       defined on normal returns and on an illegal */
/*                       input return for insufficient storage. */

/*     The following two arrays are segments of the RWORK array which may */
/*     also be of interest to the user as optional outputs.  For each */
/*     array, the table below gives its internal name, its base address */
/*     in RWORK, and its description. */

/*     Name  Base address  Description */
/*     ----  ------------  ---------------------------------------------- */
/*     YH    21            The Nordsieck history array, of size NYH by */
/*                         (NQCUR + 1), where NYH is the initial value of */
/*                         NEQ.  For j = 0,1,...,NQCUR, column j + 1 of */
/*                         YH contains HCUR**j/factorial(j) times the jth */
/*                         derivative of the interpolating polynomial */
/*                         currently representing the solution, evaluated */
/*                         at t = TCUR. */
/*     ACOR  LENRW-NEQ+1   Array of size NEQ used for the accumulated */
/*                         corrections on each step, scaled on output to */
/*                         represent the estimated local error in Y on */
/*                         the last step.  This is the vector e in the */
/*                         description of the error control.  It is */
/*                         defined only on successful return from SLSODE. */


/*                    Part 2.  Other Callable Routines */
/*                    -------------------------------- */

/*     The following are optional calls which the user may make to gain */
/*     additional capabilities in conjunction with SLSODE. */

/*     Form of call              Function */
/*     ------------------------  ---------------------------------------- */
/*     CALL XSETUN(LUN)          Set the logical unit number, LUN, for */
/*                               output of messages from SLSODE, if the */
/*                               default is not desired.  The default */
/*                               value of LUN is 6. This call may be made */
/*                               at any time and will take effect */
/*                               immediately. */
/*     CALL XSETF(MFLAG)         Set a flag to control the printing of */
/*                               messages by SLSODE.  MFLAG = 0 means do */
/*                               not print.  (Danger:  this risks losing */
/*                               valuable information.)  MFLAG = 1 means */
/*                               print (the default).  This call may be */
/*                               made at any time and will take effect */
/*                               immediately. */
/*     CALL SSRCOM(RSAV,ISAV,JOB)  Saves and restores the contents of the */
/*                               internal COMMON blocks used by SLSODE */
/*                               (see Part 3 below).  RSAV must be a */
/*                               real array of length 218 or more, and */
/*                               ISAV must be an integer array of length */
/*                               37 or more.  JOB = 1 means save COMMON */
/*                               into RSAV/ISAV.  JOB = 2 means restore */
/*                               COMMON from same.  SSRCOM is useful if */
/*                               one is interrupting a run and restarting */
/*                               later, or alternating between two or */
/*                               more problems solved with SLSODE. */
/*     CALL SINTDY(,,,,,)        Provide derivatives of y, of various */
/*     (see below)               orders, at a specified point t, if */
/*                               desired.  It may be called only after a */
/*                               successful return from SLSODE.  Detailed */
/*                               instructions follow. */

/*     Detailed instructions for using SINTDY */
/*     -------------------------------------- */
/*     The form of the CALL is: */

/*           CALL SINTDY (T, K, RWORK(21), NYH, DKY, IFLAG) */

/*     The input parameters are: */

/*     T          Value of independent variable where answers are */
/*                desired (normally the same as the T last returned by */
/*                SLSODE).  For valid results, T must lie between */
/*                TCUR - HU and TCUR.  (See "Optional Outputs" above */
/*                for TCUR and HU.) */
/*     K          Integer order of the derivative desired.  K must */
/*                satisfy 0 <= K <= NQCUR, where NQCUR is the current */
/*                order (see "Optional Outputs").  The capability */
/*                corresponding to K = 0, i.e., computing y(t), is */
/*                already provided by SLSODE directly.  Since */
/*                NQCUR >= 1, the first derivative dy/dt is always */
/*                available with SINTDY. */
/*     RWORK(21)  The base address of the history array YH. */
/*     NYH        Column length of YH, equal to the initial value of NEQ. */

/*     The output parameters are: */

/*     DKY        Real array of length NEQ containing the computed value */
/*                of the Kth derivative of y(t). */
/*     IFLAG      Integer flag, returned as 0 if K and T were legal, */
/*                -1 if K was illegal, and -2 if T was illegal. */
/*                On an error return, a message is also written. */


/*                          Part 3.  Common Blocks */
/*                          ---------------------- */

/*     If SLSODE is to be used in an overlay situation, the user must */
/*     declare, in the primary overlay, the variables in: */
/*     (1) the call sequence to SLSODE, */
/*     (2) the internal COMMON block /SLS001/, of length 255 */
/*         (218 single precision words followed by 37 integer words). */

/*     If SLSODE is used on a system in which the contents of internal */
/*     COMMON blocks are not preserved between calls, the user should */
/*     declare the above COMMON block in his main program to insure that */
/*     its contents are preserved. */

/*     If the solution of a given problem by SLSODE is to be interrupted */
/*     and then later continued, as when restarting an interrupted run or */
/*     alternating between two or more problems, the user should save, */
/*     following the return from the last SLSODE call prior to the */
/*     interruption, the contents of the call sequence variables and the */
/*     internal COMMON block, and later restore these values before the */
/*     next SLSODE call for that problem.   In addition, if XSETUN and/or */
/*     XSETF was called for non-default handling of error messages, then */
/*     these calls must be repeated.  To save and restore the COMMON */
/*     block, use subroutine SSRCOM (see Part 2 above). */


/*              Part 4.  Optionally Replaceable Solver Routines */
/*              ----------------------------------------------- */

/*     Below are descriptions of two routines in the SLSODE package which */
/*     relate to the measurement of errors.  Either routine can be */
/*     replaced by a user-supplied version, if desired.  However, since */
/*     such a replacement may have a major impact on performance, it */
/*     should be done only when absolutely necessary, and only with great */
/*     caution.  (Note:  The means by which the package version of a */
/*     routine is superseded by the user's version may be system- */
/*     dependent.) */

/*     SEWSET */
/*     ------ */
/*     The following subroutine is called just before each internal */
/*     integration step, and sets the array of error weights, EWT, as */
/*     described under ITOL/RTOL/ATOL above: */

/*           SUBROUTINE SEWSET (NEQ, ITOL, RTOL, ATOL, YCUR, EWT) */

/*     where NEQ, ITOL, RTOL, and ATOL are as in the SLSODE call */
/*     sequence, YCUR contains the current dependent variable vector, */
/*     and EWT is the array of weights set by SEWSET. */

/*     If the user supplies this subroutine, it must return in EWT(i) */
/*     (i = 1,...,NEQ) a positive quantity suitable for comparing errors */
/*     in Y(i) to.  The EWT array returned by SEWSET is passed to the */
/*     SVNORM routine (see below), and also used by SLSODE in the */
/*     computation of the optional output IMXER, the diagonal Jacobian */
/*     approximation, and the increments for difference quotient */
/*     Jacobians. */

/*     In the user-supplied version of SEWSET, it may be desirable to use */
/*     the current values of derivatives of y. Derivatives up to order NQ */
/*     are available from the history array YH, described above under */
/*     optional outputs.  In SEWSET, YH is identical to the YCUR array, */
/*     extended to NQ + 1 columns with a column length of NYH and scale */
/*     factors of H**j/factorial(j).  On the first call for the problem, */
/*     given by NST = 0, NQ is 1 and H is temporarily set to 1.0. */
/*     NYH is the initial value of NEQ.  The quantities NQ, H, and NST */
/*     can be obtained by including in SEWSET the statements: */
/*           REAL RLS */
/*           COMMON /SLS001/ RLS(218),ILS(37) */
/*           NQ = ILS(33) */
/*           NST = ILS(34) */
/*           H = RLS(212) */
/*     Thus, for example, the current value of dy/dt can be obtained as */
/*     YCUR(NYH+i)/H (i=1,...,NEQ) (and the division by H is unnecessary */
/*     when NST = 0). */

/*     SVNORM */
/*     ------ */
/*     SVNORM is a real function routine which computes the weighted */
/*     root-mean-square norm of a vector v: */

/*        d = SVNORM (n, v, w) */

/*     where: */
/*     n = the length of the vector, */
/*     v = real array of length n containing the vector, */
/*     w = real array of length n containing weights, */
/*     d = SQRT( (1/n) * sum(v(i)*w(i))**2 ). */

/*     SVNORM is called with n = NEQ and with w(i) = 1.0/EWT(i), where */
/*     EWT is as set by subroutine SEWSET. */

/*     If the user supplies this function, it should return a nonnegative */
/*     value of SVNORM suitable for use in the error control in SLSODE. */
/*     None of the arguments should be altered by SVNORM.  For example, a */
/*     user-supplied SVNORM routine might: */
/*     - Substitute a max-norm of (v(i)*w(i)) for the rms-norm, or */
/*     - Ignore some components of v in the norm, with the effect of */
/*       suppressing the error control on those components of Y. */
/*  --------------------------------------------------------------------- */
/* ***ROUTINES CALLED  SEWSET, SINTDY, RUMACH, SSTODE, SVNORM, XERRWV */
/* ***COMMON BLOCKS    SLS001 */
/* ***REVISION HISTORY  (YYYYMMDD) */
/* 19791129  DATE WRITTEN */
/* 19791213  Minor changes to declarations; DELP init. in STODE. */
/* 19800118  Treat NEQ as array; integer declarations added throughout; */
/*           minor changes to prologue. */
/* 19800306  Corrected TESCO(1,NQP1) setting in CFODE. */
/* 19800519  Corrected access of YH on forced order reduction; */
/*           numerous corrections to prologues and other comments. */
/* 19800617  In main driver, added loading of SQRT(UROUND) in RWORK; */
/*           minor corrections to main prologue. */
/* 19800923  Added zero initialization of HU and NQU. */
/* 19801218  Revised XERRWV routine; minor corrections to main prologue. */
/* 19810401  Minor changes to comments and an error message. */
/* 19810814  Numerous revisions: replaced EWT by 1/EWT; used flags */
/*           JCUR, ICF, IERPJ, IERSL between STODE and subordinates; */
/*           added tuning parameters CCMAX, MAXCOR, MSBP, MXNCF; */
/*           reorganized returns from STODE; reorganized type decls.; */
/*           fixed message length in XERRWV; changed default LUNIT to 6; */
/*           changed Common lengths; changed comments throughout. */
/* 19870330  Major update by ACH: corrected comments throughout; */
/*           removed TRET from Common; rewrote EWSET with 4 loops; */
/*           fixed t test in INTDY; added Cray directives in STODE; */
/*           in STODE, fixed DELP init. and logic around PJAC call; */
/*           combined routines to save/restore Common; */
/*           passed LEVEL = 0 in error message calls (except run abort). */
/* 19890426  Modified prologue to SLATEC/LDOC format.  (FNF) */
/* 19890501  Many improvements to prologue.  (FNF) */
/* 19890503  A few final corrections to prologue.  (FNF) */
/* 19890504  Minor cosmetic changes.  (FNF) */
/* 19890510  Corrected description of Y in Arguments section.  (FNF) */
/* 19890517  Minor corrections to prologue.  (FNF) */
/* 19920514  Updated with prologue edited 891025 by G. Shaw for manual. */
/* 19920515  Converted source lines to upper case.  (FNF) */
/* 19920603  Revised XERRWV calls using mixed upper-lower case.  (ACH) */
/* 19920616  Revised prologue comment regarding CFT.  (ACH) */
/* 19921116  Revised prologue comments regarding Common.  (ACH). */
/* 19930326  Added comment about non-reentrancy.  (FNF) */
/* 19930723  Changed R1MACH to RUMACH. (FNF) */
/* 19930801  Removed ILLIN and NTREP from Common (affects driver logic); */
/*           minor changes to prologue and internal comments; */
/*           changed Hollerith strings to quoted strings; */
/*           changed internal comments to mixed case; */
/*           replaced XERRWV with new version using character type; */
/*           changed dummy dimensions from 1 to *. (ACH) */
/* 19930809  Changed to generic intrinsic names; changed names of */
/*           subprograms and Common blocks to SLSODE etc. (ACH) */
/* 19930929  Eliminated use of REAL intrinsic; other minor changes. (ACH) */
/* 20010412  Removed all 'own' variables from Common block /SLS001/ */
/*           (affects declarations in 6 routines). (ACH) */
/* 20010509  Minor corrections to prologue. (ACH) */
/* 20031105  Restored 'own' variables to Common block /SLS001/, to */
/*           enable interrupt/restart feature. (ACH) */
/* 20031112  Added SAVE statements for data-loaded constants. */

/* ***  END PROLOGUE  SLSODE */

/* *Internal Notes: */

/* Other Routines in the SLSODE Package. */

/* In addition to Subroutine SLSODE, the SLSODE package includes the */
/* following subroutines and function routines: */
/*  SINTDY   computes an interpolated value of the y vector at t = TOUT. */
/*  SSTODE   is the core integrator, which does one step of the */
/*           integration and the associated error control. */
/*  SCFODE   sets all method coefficients and test constants. */
/*  SPREPJ   computes and preprocesses the Jacobian matrix J = df/dy */
/*           and the Newton iteration matrix P = I - h*l0*J. */
/*  SSOLSY   manages solution of linear system in chord iteration. */
/*  SEWSET   sets the error weight vector EWT before each step. */
/*  SVNORM   computes the weighted R.M.S. norm of a vector. */
/*  SSRCOM   is a user-callable routine to save and restore */
/*           the contents of the internal Common block. */
/*  SGEFA and SGESL   are routines from LINPACK for solving full */
/*           systems of linear algebraic equations. */
/*  SGBFA and SGBSL   are routines from LINPACK for solving banded */
/*           linear systems. */
/*  RUMACH   computes the unit roundoff in a machine-independent manner. */
/*  XERRWV, XSETUN, XSETF, IXSAV, IUMACH   handle the printing of all */
/*           error messages and warnings.  XERRWV is machine-dependent. */
/* Note: SVNORM, RUMACH, IXSAV, and IUMACH are function routines. */
/* All the others are subroutines. */

/* **End */

/*  Declare externals. */

/*  Declare all other variables. */
/* ----------------------------------------------------------------------- */
/* The following internal Common block contains */
/* (a) variables which are local to any subroutine but whose values must */
/*     be preserved between calls to the routine ("own" variables), and */
/* (b) variables which are communicated between subroutines. */
/* The block SLS001 is declared in subroutines SLSODE, SINTDY, SSTODE, */
/* SPREPJ, and SSOLSY. */
/* Groups of variables are replaced by dummy arrays in the Common */
/* declarations in routines where those variables are not used. */
/* ----------------------------------------------------------------------- */

    /* Parameter adjustments */
    --neq;
    --y;
    --rtol;
    --atol;
    --rwork;
    --iwork;

    /* Function Body */
/* ----------------------------------------------------------------------- */
/* Block A. */
/* This code block is executed on every call. */
/* It tests ISTATE and ITASK for legality and branches appropriately. */
/* If ISTATE .GT. 1 but the flag INIT shows that initialization has */
/* not yet been done, an error return occurs. */
/* If ISTATE = 1 and TOUT = T, return immediately. */
/* ----------------------------------------------------------------------- */

/* ***FIRST EXECUTABLE STATEMENT  SLSODE */
    if (*istate < 1 || *istate > 3) {
	goto L601;
    }
    if (*itask < 1 || *itask > 5) {
	goto L602;
    }
    if (*istate == 1) {
	goto L10;
    }
    if (sls001_1.init == 0) {
	goto L603;
    }
    if (*istate == 2) {
	goto L200;
    }
    goto L20;
L10:
    sls001_1.init = 0;
    if (*tout == *t) {
	return 0;
    }
/* ----------------------------------------------------------------------- */
/* Block B. */
/* The next code block is executed for the initial call (ISTATE = 1), */
/* or for a continuation call with parameter changes (ISTATE = 3). */
/* It contains checking of all inputs and various initializations. */

/* First check legality of the non-optional inputs NEQ, ITOL, IOPT, */
/* MF, ML, and MU. */
/* ----------------------------------------------------------------------- */
L20:
    if (neq[1] <= 0) {
	goto L604;
    }
    if (*istate == 1) {
	goto L25;
    }
    if (neq[1] > sls001_1.n) {
	goto L605;
    }
L25:
    sls001_1.n = neq[1];
    if (*itol < 1 || *itol > 4) {
	goto L606;
    }
    if (*iopt < 0 || *iopt > 1) {
	goto L607;
    }
    sls001_1.meth = *mf / 10;
    sls001_1.miter = *mf - sls001_1.meth * 10;
    if (sls001_1.meth < 1 || sls001_1.meth > 2) {
	goto L608;
    }
    if (sls001_1.miter < 0 || sls001_1.miter > 5) {
	goto L608;
    }
    if (sls001_1.miter <= 3) {
	goto L30;
    }
    ml = iwork[1];
    mu = iwork[2];
    if (ml < 0 || ml >= sls001_1.n) {
	goto L609;
    }
    if (mu < 0 || mu >= sls001_1.n) {
	goto L610;
    }
L30:
/* Next process and check the optional inputs. -------------------------- */
    if (*iopt == 1) {
	goto L40;
    }
    sls001_1.maxord = mord[sls001_1.meth - 1];
    sls001_1.mxstep = mxstp0;
    sls001_1.mxhnil = mxhnl0;
    if (*istate == 1) {
	h0 = 0.f;
    }
    sls001_1.hmxi = 0.f;
    sls001_1.hmin = 0.f;
    goto L60;
L40:
    sls001_1.maxord = iwork[5];
    if (sls001_1.maxord < 0) {
	goto L611;
    }
    if (sls001_1.maxord == 0) {
	sls001_1.maxord = 100;
    }
/* Computing MIN */
    i__1 = sls001_1.maxord, i__2 = mord[sls001_1.meth - 1];
    sls001_1.maxord = min(i__1,i__2);
    sls001_1.mxstep = iwork[6];
    if (sls001_1.mxstep < 0) {
	goto L612;
    }
    if (sls001_1.mxstep == 0) {
	sls001_1.mxstep = mxstp0;
    }
    sls001_1.mxhnil = iwork[7];
    if (sls001_1.mxhnil < 0) {
	goto L613;
    }
    if (sls001_1.mxhnil == 0) {
	sls001_1.mxhnil = mxhnl0;
    }
    if (*istate != 1) {
	goto L50;
    }
    h0 = rwork[5];
    if ((*tout - *t) * h0 < 0.f) {
	goto L614;
    }
L50:
    hmax = rwork[6];
    if (hmax < 0.f) {
	goto L615;
    }
    sls001_1.hmxi = 0.f;
    if (hmax > 0.f) {
	sls001_1.hmxi = 1.f / hmax;
    }
    sls001_1.hmin = rwork[7];
    if (sls001_1.hmin < 0.f) {
	goto L616;
    }
/* ----------------------------------------------------------------------- */
/* Set work array pointers and check lengths LRW and LIW. */
/* Pointers to segments of RWORK and IWORK are named by prefixing L to */
/* the name of the segment.  E.g., the segment YH starts at RWORK(LYH). */
/* Segments of RWORK (in order) are denoted  YH, WM, EWT, SAVF, ACOR. */
/* ----------------------------------------------------------------------- */
L60:
    sls001_1.lyh = 21;
    if (*istate == 1) {
	sls001_1.nyh = sls001_1.n;
    }
    sls001_1.lwm = sls001_1.lyh + (sls001_1.maxord + 1) * sls001_1.nyh;
    if (sls001_1.miter == 0) {
	lenwm = 0;
    }
    if (sls001_1.miter == 1 || sls001_1.miter == 2) {
	lenwm = sls001_1.n * sls001_1.n + 2;
    }
    if (sls001_1.miter == 3) {
	lenwm = sls001_1.n + 2;
    }
    if (sls001_1.miter >= 4) {
	lenwm = ((ml << 1) + mu + 1) * sls001_1.n + 2;
    }
    sls001_1.lewt = sls001_1.lwm + lenwm;
    sls001_1.lsavf = sls001_1.lewt + sls001_1.n;
    sls001_1.lacor = sls001_1.lsavf + sls001_1.n;
    lenrw = sls001_1.lacor + sls001_1.n - 1;
    iwork[17] = lenrw;
    sls001_1.liwm = 1;
    leniw = sls001_1.n + 20;
    if (sls001_1.miter == 0 || sls001_1.miter == 3) {
	leniw = 20;
    }
    iwork[18] = leniw;
    if (lenrw > *lrw) {
	goto L617;
    }
    if (leniw > *liw) {
	goto L618;
    }
/* Check RTOL and ATOL for legality. ------------------------------------ */
    rtoli = rtol[1];
    atoli = atol[1];
    i__1 = sls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (*itol >= 3) {
	    rtoli = rtol[i__];
	}
	if (*itol == 2 || *itol == 4) {
	    atoli = atol[i__];
	}
	if (rtoli < 0.f) {
	    goto L619;
	}
	if (atoli < 0.f) {
	    goto L620;
	}
/* L70: */
    }
    if (*istate == 1) {
	goto L100;
    }
/* If ISTATE = 3, set flag to signal parameter changes to SSTODE. ------- */
    sls001_1.jstart = -1;
    if (sls001_1.nq <= sls001_1.maxord) {
	goto L90;
    }
/* MAXORD was reduced below NQ.  Copy YH(*,MAXORD+2) into SAVF. --------- */
    i__1 = sls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L80: */
	rwork[i__ + sls001_1.lsavf - 1] = rwork[i__ + sls001_1.lwm - 1];
    }
/* Reload WM(1) = RWORK(LWM), since LWM may have changed. --------------- */
L90:
    if (sls001_1.miter > 0) {
	rwork[sls001_1.lwm] = sqrt(sls001_1.uround);
    }
    if (sls001_1.n == sls001_1.nyh) {
	goto L200;
    }
/* NEQ was reduced.  Zero part of YH to avoid undefined references. ----- */
    i1 = sls001_1.lyh + sls001_1.l * sls001_1.nyh;
    i2 = sls001_1.lyh + (sls001_1.maxord + 1) * sls001_1.nyh - 1;
    if (i1 > i2) {
	goto L200;
    }
    i__1 = i2;
    for (i__ = i1; i__ <= i__1; ++i__) {
/* L95: */
	rwork[i__] = 0.f;
    }
    goto L200;
/* ----------------------------------------------------------------------- */
/* Block C. */
/* The next block is for the initial call only (ISTATE = 1). */
/* It contains all remaining initializations, the initial call to F, */
/* and the calculation of the initial step size. */
/* The error weights in EWT are inverted after being loaded. */
/* ----------------------------------------------------------------------- */
L100:
    sls001_1.uround = rumach_();
    sls001_1.tn = *t;
    if (*itask != 4 && *itask != 5) {
	goto L110;
    }
    tcrit = rwork[1];
    if ((tcrit - *tout) * (*tout - *t) < 0.f) {
	goto L625;
    }
    if (h0 != 0.f && (*t + h0 - tcrit) * h0 > 0.f) {
	h0 = tcrit - *t;
    }
L110:
    sls001_1.jstart = 0;
    if (sls001_1.miter > 0) {
	rwork[sls001_1.lwm] = sqrt(sls001_1.uround);
    }
    sls001_1.nhnil = 0;
    sls001_1.nst = 0;
    sls001_1.nje = 0;
    sls001_1.nslast = 0;
    sls001_1.hu = 0.f;
    sls001_1.nqu = 0;
    sls001_1.ccmax = .3f;
    sls001_1.maxcor = 3;
    sls001_1.msbp = 20;
    sls001_1.mxncf = 10;
/* Initial call to F.  (LF0 points to YH(*,2).) ------------------------- */
    lf0 = sls001_1.lyh + sls001_1.nyh;
    (*f)(&neq[1], t, &y[1], &rwork[lf0]);
    sls001_1.nfe = 1;
/* Load the initial value vector in YH. --------------------------------- */
    i__1 = sls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L115: */
	rwork[i__ + sls001_1.lyh - 1] = y[i__];
    }
/* Load and invert the EWT array.  (H is temporarily set to 1.0.) ------- */
    sls001_1.nq = 1;
    sls001_1.h__ = 1.f;
    sewset_(&sls001_1.n, itol, &rtol[1], &atol[1], &rwork[sls001_1.lyh], &
	    rwork[sls001_1.lewt]);
    i__1 = sls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (rwork[i__ + sls001_1.lewt - 1] <= 0.f) {
	    goto L621;
	}
/* L120: */
	rwork[i__ + sls001_1.lewt - 1] = 1.f / rwork[i__ + sls001_1.lewt - 1];
    }
/* ----------------------------------------------------------------------- */
/* The coding below computes the step size, H0, to be attempted on the */
/* first step, unless the user has supplied a value for this. */
/* First check that TOUT - T differs significantly from zero. */
/* A scalar tolerance quantity TOL is computed, as MAX(RTOL(I)) */
/* if this is positive, or MAX(ATOL(I)/ABS(Y(I))) otherwise, adjusted */
/* so as to be between 100*UROUND and 1.0E-3. */
/* Then the computed value H0 is given by.. */
/*                                      NEQ */
/*   H0**2 = TOL / ( w0**-2 + (1/NEQ) * SUM ( f(i)/ywt(i) )**2  ) */
/*                                       1 */
/* where   w0     = MAX ( ABS(T), ABS(TOUT) ), */
/*         f(i)   = i-th component of initial value of f, */
/*         ywt(i) = EWT(i)/TOL  (a weight for y(i)). */
/* The sign of H0 is inferred from the initial values of TOUT and T. */
/* ----------------------------------------------------------------------- */
    if (h0 != 0.f) {
	goto L180;
    }
    tdist = (r__1 = *tout - *t, dabs(r__1));
/* Computing MAX */
    r__1 = dabs(*t), r__2 = dabs(*tout);
    w0 = dmax(r__1,r__2);
    if (tdist < sls001_1.uround * 2.f * w0) {
	goto L622;
    }
    tol = rtol[1];
    if (*itol <= 2) {
	goto L140;
    }
    i__1 = sls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L130: */
/* Computing MAX */
	r__1 = tol, r__2 = rtol[i__];
	tol = dmax(r__1,r__2);
    }
L140:
    if (tol > 0.f) {
	goto L160;
    }
    atoli = atol[1];
    i__1 = sls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (*itol == 2 || *itol == 4) {
	    atoli = atol[i__];
	}
	ayi = (r__1 = y[i__], dabs(r__1));
	if (ayi != 0.f) {
/* Computing MAX */
	    r__1 = tol, r__2 = atoli / ayi;
	    tol = dmax(r__1,r__2);
	}
/* L150: */
    }
L160:
/* Computing MAX */
    r__1 = tol, r__2 = sls001_1.uround * 100.f;
    tol = dmax(r__1,r__2);
    tol = dmin(tol,.001f);
    sum = svnorm_(&sls001_1.n, &rwork[lf0], &rwork[sls001_1.lewt]);
/* Computing 2nd power */
    r__1 = sum;
    sum = 1.f / (tol * w0 * w0) + tol * (r__1 * r__1);
    h0 = 1.f / sqrt(sum);
    h0 = dmin(h0,tdist);
    r__1 = *tout - *t;
    h0 = r_sign(&h0, &r__1);
/* Adjust H0 if necessary to meet HMAX bound. --------------------------- */
L180:
    rh = dabs(h0) * sls001_1.hmxi;
    if (rh > 1.f) {
	h0 /= rh;
    }
/* Load H with H0 and scale YH(*,2) by H0. ------------------------------ */
    sls001_1.h__ = h0;
    i__1 = sls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L190: */
	rwork[i__ + lf0 - 1] = h0 * rwork[i__ + lf0 - 1];
    }
    goto L270;
/* ----------------------------------------------------------------------- */
/* Block D. */
/* The next code block is for continuation calls only (ISTATE = 2 or 3) */
/* and is to check stop conditions before taking a step. */
/* ----------------------------------------------------------------------- */
L200:
    sls001_1.nslast = sls001_1.nst;
    switch (*itask) {
	case 1:  goto L210;
	case 2:  goto L250;
	case 3:  goto L220;
	case 4:  goto L230;
	case 5:  goto L240;
    }
L210:
    if ((sls001_1.tn - *tout) * sls001_1.h__ < 0.f) {
	goto L250;
    }
    sintdy_(tout, &c__0, &rwork[sls001_1.lyh], &sls001_1.nyh, &y[1], &iflag);
    *t = *tout;
    goto L420;
L220:
    tp = sls001_1.tn - sls001_1.hu * (sls001_1.uround * 100.f + 1.f);
    if ((tp - *tout) * sls001_1.h__ > 0.f) {
	goto L623;
    }
    if ((sls001_1.tn - *tout) * sls001_1.h__ < 0.f) {
	goto L250;
    }
    goto L400;
L230:
    tcrit = rwork[1];
    if ((sls001_1.tn - tcrit) * sls001_1.h__ > 0.f) {
	goto L624;
    }
    if ((tcrit - *tout) * sls001_1.h__ < 0.f) {
	goto L625;
    }
    if ((sls001_1.tn - *tout) * sls001_1.h__ < 0.f) {
	goto L245;
    }
    sintdy_(tout, &c__0, &rwork[sls001_1.lyh], &sls001_1.nyh, &y[1], &iflag);
    *t = *tout;
    goto L420;
L240:
    tcrit = rwork[1];
    if ((sls001_1.tn - tcrit) * sls001_1.h__ > 0.f) {
	goto L624;
    }
L245:
    hmx = dabs(sls001_1.tn) + dabs(sls001_1.h__);
    ihit = (r__1 = sls001_1.tn - tcrit, dabs(r__1)) <= sls001_1.uround * 
	    100.f * hmx;
    if (ihit) {
	goto L400;
    }
    tnext = sls001_1.tn + sls001_1.h__ * (sls001_1.uround * 4.f + 1.f);
    if ((tnext - tcrit) * sls001_1.h__ <= 0.f) {
	goto L250;
    }
    sls001_1.h__ = (tcrit - sls001_1.tn) * (1.f - sls001_1.uround * 4.f);
    if (*istate == 2) {
	sls001_1.jstart = -2;
    }
/* ----------------------------------------------------------------------- */
/* Block E. */
/* The next block is normally executed for all calls and contains */
/* the call to the one-step core integrator SSTODE. */

/* This is a looping point for the integration steps. */

/* First check for too many steps being taken, update EWT (if not at */
/* start of problem), check for too much accuracy being requested, and */
/* check for H below the roundoff level in T. */
/* ----------------------------------------------------------------------- */
L250:
    if (sls001_1.nst - sls001_1.nslast >= sls001_1.mxstep) {
	goto L500;
    }
    sewset_(&sls001_1.n, itol, &rtol[1], &atol[1], &rwork[sls001_1.lyh], &
	    rwork[sls001_1.lewt]);
    i__1 = sls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (rwork[i__ + sls001_1.lewt - 1] <= 0.f) {
	    goto L510;
	}
/* L260: */
	rwork[i__ + sls001_1.lewt - 1] = 1.f / rwork[i__ + sls001_1.lewt - 1];
    }
L270:
    tolsf = sls001_1.uround * svnorm_(&sls001_1.n, &rwork[sls001_1.lyh], &
	    rwork[sls001_1.lewt]);
    if (tolsf <= 1.f) {
	goto L280;
    }
    tolsf *= 2.f;
    if (sls001_1.nst == 0) {
	goto L626;
    }
    goto L520;
L280:
    if (sls001_1.tn + sls001_1.h__ != sls001_1.tn) {
	goto L290;
    }
    ++sls001_1.nhnil;
    if (sls001_1.nhnil > sls001_1.mxhnil) {
	goto L290;
    }
    if (sls001_1.nhnil < sls001_1.mxhnil) {
	goto L290;
    }
L290:
/* ----------------------------------------------------------------------- */
/*  CALL SSTODE(NEQ,Y,YH,NYH,YH,EWT,SAVF,ACOR,WM,IWM,F,JAC,SPREPJ,SSOLSY) */
/* ----------------------------------------------------------------------- */
    sstode_(&neq[1], &y[1], &rwork[sls001_1.lyh], &sls001_1.nyh, &rwork[
	    sls001_1.lyh], &rwork[sls001_1.lewt], &rwork[sls001_1.lsavf], &
	    rwork[sls001_1.lacor], &rwork[sls001_1.lwm], &iwork[sls001_1.liwm]
	    , (S_fp)f, (U_fp)jac, (U_fp)sprepj_, (U_fp)ssolsy_);
    kgo = 1 - sls001_1.kflag;
    switch (kgo) {
	case 1:  goto L300;
	case 2:  goto L530;
	case 3:  goto L540;
    }
/* ----------------------------------------------------------------------- */
/* Block F. */
/* The following block handles the case of a successful return from the */
/* core integrator (KFLAG = 0).  Test for stop conditions. */
/* ----------------------------------------------------------------------- */
L300:
    sls001_1.init = 1;
    switch (*itask) {
	case 1:  goto L310;
	case 2:  goto L400;
	case 3:  goto L330;
	case 4:  goto L340;
	case 5:  goto L350;
    }
/* ITASK = 1.  If TOUT has been reached, interpolate. ------------------- */
L310:
    if ((sls001_1.tn - *tout) * sls001_1.h__ < 0.f) {
	goto L250;
    }
    sintdy_(tout, &c__0, &rwork[sls001_1.lyh], &sls001_1.nyh, &y[1], &iflag);
    *t = *tout;
    goto L420;
/* ITASK = 3.  Jump to exit if TOUT was reached. ------------------------ */
L330:
    if ((sls001_1.tn - *tout) * sls001_1.h__ >= 0.f) {
	goto L400;
    }
    goto L250;
/* ITASK = 4.  See if TOUT or TCRIT was reached.  Adjust H if necessary. */
L340:
    if ((sls001_1.tn - *tout) * sls001_1.h__ < 0.f) {
	goto L345;
    }
    sintdy_(tout, &c__0, &rwork[sls001_1.lyh], &sls001_1.nyh, &y[1], &iflag);
    *t = *tout;
    goto L420;
L345:
    hmx = dabs(sls001_1.tn) + dabs(sls001_1.h__);
    ihit = (r__1 = sls001_1.tn - tcrit, dabs(r__1)) <= sls001_1.uround * 
	    100.f * hmx;
    if (ihit) {
	goto L400;
    }
    tnext = sls001_1.tn + sls001_1.h__ * (sls001_1.uround * 4.f + 1.f);
    if ((tnext - tcrit) * sls001_1.h__ <= 0.f) {
	goto L250;
    }
    sls001_1.h__ = (tcrit - sls001_1.tn) * (1.f - sls001_1.uround * 4.f);
    sls001_1.jstart = -2;
    goto L250;
/* ITASK = 5.  See if TCRIT was reached and jump to exit. --------------- */
L350:
    hmx = dabs(sls001_1.tn) + dabs(sls001_1.h__);
    ihit = (r__1 = sls001_1.tn - tcrit, dabs(r__1)) <= sls001_1.uround * 
	    100.f * hmx;
/* ----------------------------------------------------------------------- */
/* Block G. */
/* The following block handles all successful returns from SLSODE. */
/* If ITASK .NE. 1, Y is loaded from YH and T is set accordingly. */
/* ISTATE is set to 2, and the optional outputs are loaded into the */
/* work arrays before returning. */
/* ----------------------------------------------------------------------- */
L400:
    i__1 = sls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L410: */
	y[i__] = rwork[i__ + sls001_1.lyh - 1];
    }
    *t = sls001_1.tn;
    if (*itask != 4 && *itask != 5) {
	goto L420;
    }
    if (ihit) {
	*t = tcrit;
    }
L420:
    *istate = 2;
    rwork[11] = sls001_1.hu;
    rwork[12] = sls001_1.h__;
    rwork[13] = sls001_1.tn;
    iwork[11] = sls001_1.nst;
    iwork[12] = sls001_1.nfe;
    iwork[13] = sls001_1.nje;
    iwork[14] = sls001_1.nqu;
    iwork[15] = sls001_1.nq;
    return 0;
/* ----------------------------------------------------------------------- */
/* Block H. */
/* The following block handles all unsuccessful returns other than */
/* those for illegal input.  First the error message routine is called. */
/* If there was an error test or convergence test failure, IMXER is set. */
/* Then Y is loaded from YH and T is set to TN.  The optional outputs */
/* are loaded into the work arrays before returning. */
/* ----------------------------------------------------------------------- */
/* The maximum number of steps was taken before reaching TOUT. ---------- */
L500:
    *istate = -1;
    goto L580;
/* EWT(I) .LE. 0.0 for some I (not at start of problem). ---------------- */
L510:
    ewti = rwork[sls001_1.lewt + i__ - 1];
    *istate = -6;
    goto L580;
/* Too much accuracy requested for machine precision. ------------------- */
L520:
    rwork[14] = tolsf;
    *istate = -2;
    goto L580;
/* KFLAG = -1.  Error test failed repeatedly or with ABS(H) = HMIN. ----- */
L530:
    *istate = -4;
    goto L560;
/* KFLAG = -2.  Convergence failed repeatedly or with ABS(H) = HMIN. ---- */
L540:
    *istate = -5;
/* Compute IMXER if relevant. ------------------------------------------- */
L560:
    big = 0.f;
    imxer = 1;
    i__1 = sls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	size = (r__1 = rwork[i__ + sls001_1.lacor - 1] * rwork[i__ + 
		sls001_1.lewt - 1], dabs(r__1));
	if (big >= size) {
	    goto L570;
	}
	big = size;
	imxer = i__;
L570:
	;
    }
    iwork[16] = imxer;
/* Set Y vector, T, and optional outputs. ------------------------------- */
L580:
    i__1 = sls001_1.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L590: */
	y[i__] = rwork[i__ + sls001_1.lyh - 1];
    }
    *t = sls001_1.tn;
    rwork[11] = sls001_1.hu;
    rwork[12] = sls001_1.h__;
    rwork[13] = sls001_1.tn;
    iwork[11] = sls001_1.nst;
    iwork[12] = sls001_1.nfe;
    iwork[13] = sls001_1.nje;
    iwork[14] = sls001_1.nqu;
    iwork[15] = sls001_1.nq;
    return 0;
/* ----------------------------------------------------------------------- */
/* Block I. */
/* The following block handles all error returns due to illegal input */
/* (ISTATE = -3), as detected before calling the core integrator. */
/* First the error message routine is called.  If the illegal input */
/* is a negative ISTATE, the run is aborted (apparent infinite loop). */
/* ----------------------------------------------------------------------- */
L601:
    if (*istate < 0) {
	goto L800;
    }
    goto L700;
L602:
    goto L700;
L603:
    goto L700;
L604:
    goto L700;
L605:
    goto L700;
L606:
    goto L700;
L607:
    goto L700;
L608:
    goto L700;
L609:
    goto L700;
L610:
    goto L700;
L611:
    goto L700;
L612:
    goto L700;
L613:
    goto L700;
L614:
    goto L700;
L615:
    goto L700;
L616:
    goto L700;
L617:
    goto L700;
L618:
    goto L700;
L619:
    goto L700;
L620:
    goto L700;
L621:
    ewti = rwork[sls001_1.lewt + i__ - 1];
    goto L700;
L622:
    goto L700;
L623:
    goto L700;
L624:
    goto L700;
L625:
    goto L700;
L626:
    rwork[14] = tolsf;
    goto L700;

L700:
    *istate = -3;
    return 0;

L800:
    return 0;
/* ----------------------- END OF SUBROUTINE SLSODE ---------------------- */
} /* slsode_ */

/* DECK RUMACH */
doublereal rumach_(void)
{
    /* System generated locals */
    real ret_val;

    /* Local variables */
    static real u, comp;
    extern /* Subroutine */ int rumsum_(real *, real *, real *);

/* ***BEGIN PROLOGUE  RUMACH */
/* ***PURPOSE  Compute the unit roundoff of the machine. */
/* ***CATEGORY  R1 */
/* ***TYPE      SINGLE PRECISION (RUMACH-S, DUMACH-D) */
/* ***KEYWORDS  MACHINE CONSTANTS */
/* ***AUTHOR  Hindmarsh, Alan C., (LLNL) */
/* ***DESCRIPTION */
/* *Usage: */
/*        REAL  A, RUMACH */
/*        A = RUMACH() */

/* *Function Return Values: */
/*     A : the unit roundoff of the machine. */

/* *Description: */
/*     The unit roundoff is defined as the smallest positive machine */
/*     number u such that  1.0 + u .ne. 1.0.  This is computed by RUMACH */
/*     in a machine-independent manner. */

/* ***REFERENCES  (NONE) */
/* ***ROUTINES CALLED  RUMSUM */
/* ***REVISION HISTORY  (YYYYMMDD) */
/*   19930216  DATE WRITTEN */
/*   19930818  Added SLATEC-format prologue.  (FNF) */
/*   20030707  Added RUMSUM to force normal storage of COMP.  (ACH) */
/* ***END PROLOGUE  RUMACH */

/* ***FIRST EXECUTABLE STATEMENT  RUMACH */
    u = 1.f;
L10:
    u *= .5f;
    rumsum_(&c_b86, &u, &comp);
    if (comp != 1.f) {
	goto L10;
    }
    ret_val = u * 2.f;
    return ret_val;
/* ----------------------- End of Function RUMACH ------------------------ */
} /* rumach_ */

/* Subroutine */ int rumsum_(real *a, real *b, real *c__)
{
/*     Routine to force normal storing of A + B, for RUMACH. */
    *c__ = *a + *b;
    return 0;
} /* rumsum_ */

/* DECK SCFODE */
/* Subroutine */ int scfode_(integer *meth, real *elco, real *tesco)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, ib;
    static real pc[12];
    static integer nq;
    static real fnq;
    static integer nqm1, nqp1;
    static real ragq, pint, xpin, fnqm1, agamq, rqfac, tsign, rq1fac;

/* ***BEGIN PROLOGUE  SCFODE */
/* ***SUBSIDIARY */
/* ***PURPOSE  Set ODE integrator coefficients. */
/* ***TYPE      SINGLE PRECISION (SCFODE-S, DCFODE-D) */
/* ***AUTHOR  Hindmarsh, Alan C., (LLNL) */
/* ***DESCRIPTION */

/*  SCFODE is called by the integrator routine to set coefficients */
/*  needed there.  The coefficients for the current method, as */
/*  given by the value of METH, are set for all orders and saved. */
/*  The maximum order assumed here is 12 if METH = 1 and 5 if METH = 2. */
/*  (A smaller value of the maximum order is also allowed.) */
/*  SCFODE is called once at the beginning of the problem, */
/*  and is not called again unless and until METH is changed. */

/*  The ELCO array contains the basic method coefficients. */
/*  The coefficients el(i), 1 .le. i .le. nq+1, for the method of */
/*  order nq are stored in ELCO(i,nq).  They are given by a genetrating */
/*  polynomial, i.e., */
/*      l(x) = el(1) + el(2)*x + ... + el(nq+1)*x**nq. */
/*  For the implicit Adams methods, l(x) is given by */
/*      dl/dx = (x+1)*(x+2)*...*(x+nq-1)/factorial(nq-1),    l(-1) = 0. */
/*  For the BDF methods, l(x) is given by */
/*      l(x) = (x+1)*(x+2)* ... *(x+nq)/K, */
/*  where         K = factorial(nq)*(1 + 1/2 + ... + 1/nq). */

/*  The TESCO array contains test constants used for the */
/*  local error test and the selection of step size and/or order. */
/*  At order nq, TESCO(k,nq) is used for the selection of step */
/*  size at order nq - 1 if k = 1, at order nq if k = 2, and at order */
/*  nq + 1 if k = 3. */

/* ***SEE ALSO  SLSODE */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791129  DATE WRITTEN */
/*   890501  Modified prologue to SLATEC/LDOC format.  (FNF) */
/*   890503  Minor cosmetic changes.  (FNF) */
/*   930809  Renamed to allow single/double precision versions. (ACH) */
/* ***END PROLOGUE  SCFODE */
/* **End */

/* ***FIRST EXECUTABLE STATEMENT  SCFODE */
    /* Parameter adjustments */
    tesco -= 4;
    elco -= 14;

    /* Function Body */
    switch (*meth) {
	case 1:  goto L100;
	case 2:  goto L200;
    }

L100:
    elco[14] = 1.f;
    elco[15] = 1.f;
    tesco[4] = 0.f;
    tesco[5] = 2.f;
    tesco[7] = 1.f;
    tesco[39] = 0.f;
    pc[0] = 1.f;
    rqfac = 1.f;
    for (nq = 2; nq <= 12; ++nq) {
/* ----------------------------------------------------------------------- */
/* The PC array will contain the coefficients of the polynomial */
/*     p(x) = (x+1)*(x+2)*...*(x+nq-1). */
/* Initially, p(x) = 1. */
/* ----------------------------------------------------------------------- */
	rq1fac = rqfac;
	rqfac /= nq;
	nqm1 = nq - 1;
	fnqm1 = (real) nqm1;
	nqp1 = nq + 1;
/* Form coefficients of p(x)*(x+nq-1). ---------------------------------- */
	pc[nq - 1] = 0.f;
	i__1 = nqm1;
	for (ib = 1; ib <= i__1; ++ib) {
	    i__ = nqp1 - ib;
/* L110: */
	    pc[i__ - 1] = pc[i__ - 2] + fnqm1 * pc[i__ - 1];
	}
	pc[0] = fnqm1 * pc[0];
/* Compute integral, -1 to 0, of p(x) and x*p(x). ----------------------- */
	pint = pc[0];
	xpin = pc[0] / 2.f;
	tsign = 1.f;
	i__1 = nq;
	for (i__ = 2; i__ <= i__1; ++i__) {
	    tsign = -tsign;
	    pint += tsign * pc[i__ - 1] / i__;
/* L120: */
	    xpin += tsign * pc[i__ - 1] / (i__ + 1);
	}
/* Store coefficients in ELCO and TESCO. -------------------------------- */
	elco[nq * 13 + 1] = pint * rq1fac;
	elco[nq * 13 + 2] = 1.f;
	i__1 = nq;
	for (i__ = 2; i__ <= i__1; ++i__) {
/* L130: */
	    elco[i__ + 1 + nq * 13] = rq1fac * pc[i__ - 1] / i__;
	}
	agamq = rqfac * xpin;
	ragq = 1.f / agamq;
	tesco[nq * 3 + 2] = ragq;
	if (nq < 12) {
	    tesco[nqp1 * 3 + 1] = ragq * rqfac / nqp1;
	}
	tesco[nqm1 * 3 + 3] = ragq;
/* L140: */
    }
    return 0;

L200:
    pc[0] = 1.f;
    rq1fac = 1.f;
    for (nq = 1; nq <= 5; ++nq) {
/* ----------------------------------------------------------------------- */
/* The PC array will contain the coefficients of the polynomial */
/*     p(x) = (x+1)*(x+2)*...*(x+nq). */
/* Initially, p(x) = 1. */
/* ----------------------------------------------------------------------- */
	fnq = (real) nq;
	nqp1 = nq + 1;
/* Form coefficients of p(x)*(x+nq). ------------------------------------ */
	pc[nqp1 - 1] = 0.f;
	i__1 = nq;
	for (ib = 1; ib <= i__1; ++ib) {
	    i__ = nq + 2 - ib;
/* L210: */
	    pc[i__ - 1] = pc[i__ - 2] + fnq * pc[i__ - 1];
	}
	pc[0] = fnq * pc[0];
/* Store coefficients in ELCO and TESCO. -------------------------------- */
	i__1 = nqp1;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* L220: */
	    elco[i__ + nq * 13] = pc[i__ - 1] / pc[1];
	}
	elco[nq * 13 + 2] = 1.f;
	tesco[nq * 3 + 1] = rq1fac;
	tesco[nq * 3 + 2] = nqp1 / elco[nq * 13 + 1];
	tesco[nq * 3 + 3] = (nq + 2) / elco[nq * 13 + 1];
	rq1fac /= fnq;
/* L230: */
    }
    return 0;
/* ----------------------- END OF SUBROUTINE SCFODE ---------------------- */
} /* scfode_ */

/* DECK SINTDY */
/* Subroutine */ int sintdy_(real *t, integer *k, real *yh, integer *nyh, 
	real *dky, integer *iflag)
{
    /* System generated locals */
    integer yh_dim1, yh_offset, i__1, i__2;
    real r__1;

    /* Builtin functions */
    double r_sign(real *, real *), pow_ri(real *, integer *);

    /* Local variables */
    static real c__;
    static integer i__, j;
    static real r__, s;
    static integer ic, jb, jj;
    static real tp;
    static integer jb2, jj1, jp1;

/* ***BEGIN PROLOGUE  SINTDY */
/* ***SUBSIDIARY */
/* ***PURPOSE  Interpolate solution derivatives. */
/* ***TYPE      SINGLE PRECISION (SINTDY-S, DINTDY-D) */
/* ***AUTHOR  Hindmarsh, Alan C., (LLNL) */
/* ***DESCRIPTION */

/*  SINTDY computes interpolated values of the K-th derivative of the */
/*  dependent variable vector y, and stores it in DKY.  This routine */
/*  is called within the package with K = 0 and T = TOUT, but may */
/*  also be called by the user for any K up to the current order. */
/*  (See detailed instructions in the usage documentation.) */

/*  The computed values in DKY are gotten by interpolation using the */
/*  Nordsieck history array YH.  This array corresponds uniquely to a */
/*  vector-valued polynomial of degree NQCUR or less, and DKY is set */
/*  to the K-th derivative of this polynomial at T. */
/*  The formula for DKY is: */
/*               q */
/*   DKY(i)  =  sum  c(j,K) * (T - tn)**(j-K) * h**(-j) * YH(i,j+1) */
/*              j=K */
/*  where  c(j,K) = j*(j-1)*...*(j-K+1), q = NQCUR, tn = TCUR, h = HCUR. */
/*  The quantities  nq = NQCUR, l = nq+1, N = NEQ, tn, and h are */
/*  communicated by COMMON.  The above sum is done in reverse order. */
/*  IFLAG is returned negative if either K or T is out of bounds. */

/* ***SEE ALSO  SLSODE */
/* ***ROUTINES CALLED  XERRWV */
/* ***COMMON BLOCKS    SLS001 */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791129  DATE WRITTEN */
/*   890501  Modified prologue to SLATEC/LDOC format.  (FNF) */
/*   890503  Minor cosmetic changes.  (FNF) */
/*   930809  Renamed to allow single/double precision versions. (ACH) */
/*   010412  Reduced size of Common block /SLS001/. (ACH) */
/*   031105  Restored 'own' variables to Common block /SLS001/, to */
/*           enable interrupt/restart feature. (ACH) */
/*   050427  Corrected roundoff decrement in TP. (ACH) */
/* ***END PROLOGUE  SINTDY */
/* **End */

/* ***FIRST EXECUTABLE STATEMENT  SINTDY */
    /* Parameter adjustments */
    yh_dim1 = *nyh;
    yh_offset = 1 + yh_dim1;
    yh -= yh_offset;
    --dky;

    /* Function Body */
    *iflag = 0;
    if (*k < 0 || *k > sls001_2.nq) {
	goto L80;
    }
    r__1 = dabs(sls001_2.tn) + dabs(sls001_2.hu);
    tp = sls001_2.tn - sls001_2.hu - sls001_2.uround * 100.f * r_sign(&r__1, &
	    sls001_2.hu);
    if ((*t - tp) * (*t - sls001_2.tn) > 0.f) {
	goto L90;
    }

    s = (*t - sls001_2.tn) / sls001_2.h__;
    ic = 1;
    if (*k == 0) {
	goto L15;
    }
    jj1 = sls001_2.l - *k;
    i__1 = sls001_2.nq;
    for (jj = jj1; jj <= i__1; ++jj) {
/* L10: */
	ic *= jj;
    }
L15:
    c__ = (real) ic;
    i__1 = sls001_2.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L20: */
	dky[i__] = c__ * yh[i__ + sls001_2.l * yh_dim1];
    }
    if (*k == sls001_2.nq) {
	goto L55;
    }
    jb2 = sls001_2.nq - *k;
    i__1 = jb2;
    for (jb = 1; jb <= i__1; ++jb) {
	j = sls001_2.nq - jb;
	jp1 = j + 1;
	ic = 1;
	if (*k == 0) {
	    goto L35;
	}
	jj1 = jp1 - *k;
	i__2 = j;
	for (jj = jj1; jj <= i__2; ++jj) {
/* L30: */
	    ic *= jj;
	}
L35:
	c__ = (real) ic;
	i__2 = sls001_2.n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L40: */
	    dky[i__] = c__ * yh[i__ + jp1 * yh_dim1] + s * dky[i__];
	}
/* L50: */
    }
    if (*k == 0) {
	return 0;
    }
L55:
    i__1 = -(*k);
    r__ = pow_ri(&sls001_2.h__, &i__1);
    i__1 = sls001_2.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L60: */
	dky[i__] = r__ * dky[i__];
    }
    return 0;

L80:
    *iflag = -1;
    return 0;
L90:
    *iflag = -2;
    return 0;
/* ----------------------- END OF SUBROUTINE SINTDY ---------------------- */
} /* sintdy_ */

/* DECK SPREPJ */
/* Subroutine */ int sprepj_(integer *neq, real *y, real *yh, integer *nyh, 
	real *ewt, real *ftem, real *savf, real *wm, integer *iwm, S_fp f, 
	S_fp jac)
{
    /* System generated locals */
    integer yh_dim1, yh_offset, i__1, i__2, i__3, i__4;
    real r__1, r__2;

    /* Local variables */
    static integer i__, j;
    static real r__;
    static integer i1, i2, j1;
    static real r0, di;
    static integer ii, jj, ml, mu;
    static real yi, yj, hl0;
    static integer ml3, np1;
    static real fac;
    static integer mba, ier;
    static real con, yjj;
    static integer meb1, lenp;
    static real srur;
    static integer mband;
    extern /* Subroutine */ int sgbfa_(real *, integer *, integer *, integer *
	    , integer *, integer *, integer *), sgefa_(real *, integer *, 
	    integer *, integer *, integer *);
    static integer meband;
    extern doublereal svnorm_(integer *, real *, real *);

/* ***BEGIN PROLOGUE  SPREPJ */
/* ***SUBSIDIARY */
/* ***PURPOSE  Compute and process Newton iteration matrix. */
/* ***TYPE      SINGLE PRECISION (SPREPJ-S, DPREPJ-D) */
/* ***AUTHOR  Hindmarsh, Alan C., (LLNL) */
/* ***DESCRIPTION */

/*  SPREPJ is called by SSTODE to compute and process the matrix */
/*  P = I - h*el(1)*J , where J is an approximation to the Jacobian. */
/*  Here J is computed by the user-supplied routine JAC if */
/*  MITER = 1 or 4, or by finite differencing if MITER = 2, 3, or 5. */
/*  If MITER = 3, a diagonal approximation to J is used. */
/*  J is stored in WM and replaced by P.  If MITER .ne. 3, P is then */
/*  subjected to LU decomposition in preparation for later solution */
/*  of linear systems with P as coefficient matrix.  This is done */
/*  by SGEFA if MITER = 1 or 2, and by SGBFA if MITER = 4 or 5. */

/*  In addition to variables described in SSTODE and SLSODE prologues, */
/*  communication with SPREPJ uses the following: */
/*  Y     = array containing predicted values on entry. */
/*  FTEM  = work array of length N (ACOR in SSTODE). */
/*  SAVF  = array containing f evaluated at predicted y. */
/*  WM    = real work space for matrices.  On output it contains the */
/*          inverse diagonal matrix if MITER = 3 and the LU decomposition */
/*          of P if MITER is 1, 2 , 4, or 5. */
/*          Storage of matrix elements starts at WM(3). */
/*          WM also contains the following matrix-related data: */
/*          WM(1) = SQRT(UROUND), used in numerical Jacobian increments. */
/*          WM(2) = H*EL0, saved for later use if MITER = 3. */
/*  IWM   = integer work space containing pivot information, starting at */
/*          IWM(21), if MITER is 1, 2, 4, or 5.  IWM also contains band */
/*          parameters ML = IWM(1) and MU = IWM(2) if MITER is 4 or 5. */
/*  EL0   = EL(1) (input). */
/*  IERPJ = output error flag,  = 0 if no trouble, .gt. 0 if */
/*          P matrix found to be singular. */
/*  JCUR  = output flag = 1 to indicate that the Jacobian matrix */
/*          (or approximation) is now current. */
/*  This routine also uses the COMMON variables EL0, H, TN, UROUND, */
/*  MITER, N, NFE, and NJE. */

/* ***SEE ALSO  SLSODE */
/* ***ROUTINES CALLED  SGBFA, SGEFA, SVNORM */
/* ***COMMON BLOCKS    SLS001 */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791129  DATE WRITTEN */
/*   890501  Modified prologue to SLATEC/LDOC format.  (FNF) */
/*   890504  Minor cosmetic changes.  (FNF) */
/*   930809  Renamed to allow single/double precision versions. (ACH) */
/*   010412  Reduced size of Common block /SLS001/. (ACH) */
/*   031105  Restored 'own' variables to Common block /SLS001/, to */
/*           enable interrupt/restart feature. (ACH) */
/* ***END PROLOGUE  SPREPJ */
/* **End */

/* ***FIRST EXECUTABLE STATEMENT  SPREPJ */
    /* Parameter adjustments */
    --neq;
    --y;
    yh_dim1 = *nyh;
    yh_offset = 1 + yh_dim1;
    yh -= yh_offset;
    --ewt;
    --ftem;
    --savf;
    --wm;
    --iwm;

    /* Function Body */
    ++sls001_2.nje;
    sls001_2.ierpj = 0;
    sls001_2.jcur = 1;
    hl0 = sls001_2.h__ * sls001_2.el0;
    switch (sls001_2.miter) {
	case 1:  goto L100;
	case 2:  goto L200;
	case 3:  goto L300;
	case 4:  goto L400;
	case 5:  goto L500;
    }
/* If MITER = 1, call JAC and multiply by scalar. ----------------------- */
L100:
    lenp = sls001_2.n * sls001_2.n;
    i__1 = lenp;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L110: */
	wm[i__ + 2] = 0.f;
    }
    (*jac)(&neq[1], &sls001_2.tn, &y[1], &c__0, &c__0, &wm[3], &sls001_2.n);
    con = -hl0;
    i__1 = lenp;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L120: */
	wm[i__ + 2] *= con;
    }
    goto L240;
/* If MITER = 2, make N calls to F to approximate J. -------------------- */
L200:
    fac = svnorm_(&sls001_2.n, &savf[1], &ewt[1]);
    r0 = dabs(sls001_2.h__) * 1e3f * sls001_2.uround * sls001_2.n * fac;
    if (r0 == 0.f) {
	r0 = 1.f;
    }
    srur = wm[1];
    j1 = 2;
    i__1 = sls001_2.n;
    for (j = 1; j <= i__1; ++j) {
	yj = y[j];
/* Computing MAX */
	r__1 = srur * dabs(yj), r__2 = r0 / ewt[j];
	r__ = dmax(r__1,r__2);
	y[j] += r__;
	fac = -hl0 / r__;
	(*f)(&neq[1], &sls001_2.tn, &y[1], &ftem[1]);
	i__2 = sls001_2.n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L220: */
	    wm[i__ + j1] = (ftem[i__] - savf[i__]) * fac;
	}
	y[j] = yj;
	j1 += sls001_2.n;
/* L230: */
    }
    sls001_2.nfe += sls001_2.n;
/* Add identity matrix. ------------------------------------------------- */
L240:
    j = 3;
    np1 = sls001_2.n + 1;
    i__1 = sls001_2.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	wm[j] += 1.f;
/* L250: */
	j += np1;
    }
/* Do LU decomposition on P. -------------------------------------------- */
    sgefa_(&wm[3], &sls001_2.n, &sls001_2.n, &iwm[21], &ier);
    if (ier != 0) {
	sls001_2.ierpj = 1;
    }
    return 0;
/* If MITER = 3, construct a diagonal approximation to J and P. --------- */
L300:
    wm[2] = hl0;
    r__ = sls001_2.el0 * .1f;
    i__1 = sls001_2.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L310: */
	y[i__] += r__ * (sls001_2.h__ * savf[i__] - yh[i__ + (yh_dim1 << 1)]);
    }
    (*f)(&neq[1], &sls001_2.tn, &y[1], &wm[3]);
    ++sls001_2.nfe;
    i__1 = sls001_2.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	r0 = sls001_2.h__ * savf[i__] - yh[i__ + (yh_dim1 << 1)];
	di = r0 * .1f - sls001_2.h__ * (wm[i__ + 2] - savf[i__]);
	wm[i__ + 2] = 1.f;
	if (dabs(r0) < sls001_2.uround / ewt[i__]) {
	    goto L320;
	}
	if (dabs(di) == 0.f) {
	    goto L330;
	}
	wm[i__ + 2] = r0 * .1f / di;
L320:
	;
    }
    return 0;
L330:
    sls001_2.ierpj = 1;
    return 0;
/* If MITER = 4, call JAC and multiply by scalar. ----------------------- */
L400:
    ml = iwm[1];
    mu = iwm[2];
    ml3 = ml + 3;
    mband = ml + mu + 1;
    meband = mband + ml;
    lenp = meband * sls001_2.n;
    i__1 = lenp;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L410: */
	wm[i__ + 2] = 0.f;
    }
    (*jac)(&neq[1], &sls001_2.tn, &y[1], &ml, &mu, &wm[ml3], &meband);
    con = -hl0;
    i__1 = lenp;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L420: */
	wm[i__ + 2] *= con;
    }
    goto L570;
/* If MITER = 5, make MBAND calls to F to approximate J. ---------------- */
L500:
    ml = iwm[1];
    mu = iwm[2];
    mband = ml + mu + 1;
    mba = min(mband,sls001_2.n);
    meband = mband + ml;
    meb1 = meband - 1;
    srur = wm[1];
    fac = svnorm_(&sls001_2.n, &savf[1], &ewt[1]);
    r0 = dabs(sls001_2.h__) * 1e3f * sls001_2.uround * sls001_2.n * fac;
    if (r0 == 0.f) {
	r0 = 1.f;
    }
    i__1 = mba;
    for (j = 1; j <= i__1; ++j) {
	i__2 = sls001_2.n;
	i__3 = mband;
	for (i__ = j; i__3 < 0 ? i__ >= i__2 : i__ <= i__2; i__ += i__3) {
	    yi = y[i__];
/* Computing MAX */
	    r__1 = srur * dabs(yi), r__2 = r0 / ewt[i__];
	    r__ = dmax(r__1,r__2);
/* L530: */
	    y[i__] += r__;
	}
	(*f)(&neq[1], &sls001_2.tn, &y[1], &ftem[1]);
	i__3 = sls001_2.n;
	i__2 = mband;
	for (jj = j; i__2 < 0 ? jj >= i__3 : jj <= i__3; jj += i__2) {
	    y[jj] = yh[jj + yh_dim1];
	    yjj = y[jj];
/* Computing MAX */
	    r__1 = srur * dabs(yjj), r__2 = r0 / ewt[jj];
	    r__ = dmax(r__1,r__2);
	    fac = -hl0 / r__;
/* Computing MAX */
	    i__4 = jj - mu;
	    i1 = max(i__4,1);
/* Computing MIN */
	    i__4 = jj + ml;
	    i2 = min(i__4,sls001_2.n);
	    ii = jj * meb1 - ml + 2;
	    i__4 = i2;
	    for (i__ = i1; i__ <= i__4; ++i__) {
/* L540: */
		wm[ii + i__] = (ftem[i__] - savf[i__]) * fac;
	    }
/* L550: */
	}
/* L560: */
    }
    sls001_2.nfe += mba;
/* Add identity matrix. ------------------------------------------------- */
L570:
    ii = mband + 2;
    i__1 = sls001_2.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	wm[ii] += 1.f;
/* L580: */
	ii += meband;
    }
/* Do LU decomposition of P. -------------------------------------------- */
    sgbfa_(&wm[3], &meband, &sls001_2.n, &ml, &mu, &iwm[21], &ier);
    if (ier != 0) {
	sls001_2.ierpj = 1;
    }
    return 0;
/* ----------------------- END OF SUBROUTINE SPREPJ ---------------------- */
} /* sprepj_ */

/* DECK SSOLSY */
/* Subroutine */ int ssolsy_(real *wm, integer *iwm, real *x, real *tem)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;
    static real r__, di;
    static integer ml, mu;
    static real hl0, phl0;
    extern /* Subroutine */ int sgbsl_(real *, integer *, integer *, integer *
	    , integer *, integer *, real *, integer *), sgesl_(real *, 
	    integer *, integer *, integer *, real *, integer *);
    static integer meband;

/* ***BEGIN PROLOGUE  SSOLSY */
/* ***SUBSIDIARY */
/* ***PURPOSE  ODEPACK linear system solver. */
/* ***TYPE      SINGLE PRECISION (SSOLSY-S, DSOLSY-D) */
/* ***AUTHOR  Hindmarsh, Alan C., (LLNL) */
/* ***DESCRIPTION */

/*  This routine manages the solution of the linear system arising from */
/*  a chord iteration.  It is called if MITER .ne. 0. */
/*  If MITER is 1 or 2, it calls SGESL to accomplish this. */
/*  If MITER = 3 it updates the coefficient h*EL0 in the diagonal */
/*  matrix, and then computes the solution. */
/*  If MITER is 4 or 5, it calls SGBSL. */
/*  Communication with SSOLSY uses the following variables: */
/*  WM    = real work space containing the inverse diagonal matrix if */
/*          MITER = 3 and the LU decomposition of the matrix otherwise. */
/*          Storage of matrix elements starts at WM(3). */
/*          WM also contains the following matrix-related data: */
/*          WM(1) = SQRT(UROUND) (not used here), */
/*          WM(2) = HL0, the previous value of h*EL0, used if MITER = 3. */
/*  IWM   = integer work space containing pivot information, starting at */
/*          IWM(21), if MITER is 1, 2, 4, or 5.  IWM also contains band */
/*          parameters ML = IWM(1) and MU = IWM(2) if MITER is 4 or 5. */
/*  X     = the right-hand side vector on input, and the solution vector */
/*          on output, of length N. */
/*  TEM   = vector of work space of length N, not used in this version. */
/*  IERSL = output flag (in COMMON).  IERSL = 0 if no trouble occurred. */
/*          IERSL = 1 if a singular matrix arose with MITER = 3. */
/*  This routine also uses the COMMON variables EL0, H, MITER, and N. */

/* ***SEE ALSO  SLSODE */
/* ***ROUTINES CALLED  SGBSL, SGESL */
/* ***COMMON BLOCKS    SLS001 */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791129  DATE WRITTEN */
/*   890501  Modified prologue to SLATEC/LDOC format.  (FNF) */
/*   890503  Minor cosmetic changes.  (FNF) */
/*   930809  Renamed to allow single/double precision versions. (ACH) */
/*   010412  Reduced size of Common block /SLS001/. (ACH) */
/*   031105  Restored 'own' variables to Common block /SLS001/, to */
/*           enable interrupt/restart feature. (ACH) */
/* ***END PROLOGUE  SSOLSY */
/* **End */

/* ***FIRST EXECUTABLE STATEMENT  SSOLSY */
    /* Parameter adjustments */
    --tem;
    --x;
    --iwm;
    --wm;

    /* Function Body */
    sls001_2.iersl = 0;
    switch (sls001_2.miter) {
	case 1:  goto L100;
	case 2:  goto L100;
	case 3:  goto L300;
	case 4:  goto L400;
	case 5:  goto L400;
    }
L100:
    sgesl_(&wm[3], &sls001_2.n, &sls001_2.n, &iwm[21], &x[1], &c__0);
    return 0;

L300:
    phl0 = wm[2];
    hl0 = sls001_2.h__ * sls001_2.el0;
    wm[2] = hl0;
    if (hl0 == phl0) {
	goto L330;
    }
    r__ = hl0 / phl0;
    i__1 = sls001_2.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	di = 1.f - r__ * (1.f - 1.f / wm[i__ + 2]);
	if (dabs(di) == 0.f) {
	    goto L390;
	}
/* L320: */
	wm[i__ + 2] = 1.f / di;
    }
L330:
    i__1 = sls001_2.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L340: */
	x[i__] = wm[i__ + 2] * x[i__];
    }
    return 0;
L390:
    sls001_2.iersl = 1;
    return 0;

L400:
    ml = iwm[1];
    mu = iwm[2];
    meband = (ml << 1) + mu + 1;
    sgbsl_(&wm[3], &meband, &sls001_2.n, &ml, &mu, &iwm[21], &x[1], &c__0);
    return 0;
/* ----------------------- END OF SUBROUTINE SSOLSY ---------------------- */
} /* ssolsy_ */

/* DECK SSRCOM */
/* Subroutine */ int ssrcom_(real *rsav, integer *isav, integer *job)
{
    /* Initialized data */

    static integer lenils = 37;
    static integer lenrls = 218;

    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__;

/* ***BEGIN PROLOGUE  SSRCOM */
/* ***SUBSIDIARY */
/* ***PURPOSE  Save/restore ODEPACK COMMON blocks. */
/* ***TYPE      SINGLE PRECISION (SSRCOM-S, DSRCOM-D) */
/* ***AUTHOR  Hindmarsh, Alan C., (LLNL) */
/* ***DESCRIPTION */

/*  This routine saves or restores (depending on JOB) the contents of */
/*  the COMMON block SLS001, which is used internally */
/*  by one or more ODEPACK solvers. */

/*  RSAV = real array of length 218 or more. */
/*  ISAV = integer array of length 37 or more. */
/*  JOB  = flag indicating to save or restore the COMMON blocks: */
/*         JOB  = 1 if COMMON is to be saved (written to RSAV/ISAV) */
/*         JOB  = 2 if COMMON is to be restored (read from RSAV/ISAV) */
/*         A call with JOB = 2 presumes a prior call with JOB = 1. */

/* ***SEE ALSO  SLSODE */
/* ***ROUTINES CALLED  (NONE) */
/* ***COMMON BLOCKS    SLS001 */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791129  DATE WRITTEN */
/*   890501  Modified prologue to SLATEC/LDOC format.  (FNF) */
/*   890503  Minor cosmetic changes.  (FNF) */
/*   921116  Deleted treatment of block /EH0001/.  (ACH) */
/*   930801  Reduced Common block length by 2.  (ACH) */
/*   930809  Renamed to allow single/double precision versions. (ACH) */
/*   010412  Reduced Common block length by 209+12. (ACH) */
/*   031105  Restored 'own' variables to Common block /SLS001/, to */
/*           enable interrupt/restart feature. (ACH) */
/*   031112  Added SAVE statement for data-loaded constants. */
/* ***END PROLOGUE  SSRCOM */
/* **End */
    /* Parameter adjustments */
    --isav;
    --rsav;

    /* Function Body */

/* ***FIRST EXECUTABLE STATEMENT  SSRCOM */
    if (*job == 2) {
	goto L100;
    }

    i__1 = lenrls;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L10: */
	rsav[i__] = sls001_3.rls[i__ - 1];
    }
    i__1 = lenils;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L20: */
	isav[i__] = sls001_3.ils[i__ - 1];
    }
    return 0;

L100:
    i__1 = lenrls;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L110: */
	sls001_3.rls[i__ - 1] = rsav[i__];
    }
    i__1 = lenils;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L120: */
	sls001_3.ils[i__ - 1] = isav[i__];
    }
    return 0;
/* ----------------------- END OF SUBROUTINE SSRCOM ---------------------- */
} /* ssrcom_ */

/* DECK SSTODE */
/* Subroutine */ int sstode_(integer *neq, real *y, real *yh, integer *nyh, 
	real *yh1, real *ewt, real *savf, real *acor, real *wm, integer *iwm, 
	S_fp f, U_fp jac, S_fp pjac, S_fp slvs)
{
    /* System generated locals */
    integer yh_dim1, yh_offset, i__1, i__2;
    real r__1, r__2, r__3;
    doublereal d__1, d__2;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *);

    /* Local variables */
    static integer i__, j, m;
    static real r__;
    static integer i1, jb;
    static real rh, del, ddn;
    static integer ncf;
    static real dsm, dup, dcon, delp, rhdn, exdn;
    static integer iret;
    static real told, rhsm;
    static integer newq;
    static real exsm, rhup, exup;
    static integer iredo;
    extern /* Subroutine */ int scfode_(integer *, real *, real *);
    extern doublereal svnorm_(integer *, real *, real *);

/* ***BEGIN PROLOGUE  SSTODE */
/* ***SUBSIDIARY */
/* ***PURPOSE  Performs one step of an ODEPACK integration. */
/* ***TYPE      SINGLE PRECISION (SSTODE-S, DSTODE-D) */
/* ***AUTHOR  Hindmarsh, Alan C., (LLNL) */
/* ***DESCRIPTION */

/*  SSTODE performs one step of the integration of an initial value */
/*  problem for a system of ordinary differential equations. */
/*  Note:  SSTODE is independent of the value of the iteration method */
/*  indicator MITER, when this is .ne. 0, and hence is independent */
/*  of the type of chord method used, or the Jacobian structure. */
/*  Communication with SSTODE is done with the following variables: */

/*  NEQ    = integer array containing problem size in NEQ(1), and */
/*           passed as the NEQ argument in all calls to F and JAC. */
/*  Y      = an array of length .ge. N used as the Y argument in */
/*           all calls to F and JAC. */
/*  YH     = an NYH by LMAX array containing the dependent variables */
/*           and their approximate scaled derivatives, where */
/*           LMAX = MAXORD + 1.  YH(i,j+1) contains the approximate */
/*           j-th derivative of y(i), scaled by h**j/factorial(j) */
/*           (j = 0,1,...,NQ).  on entry for the first step, the first */
/*           two columns of YH must be set from the initial values. */
/*  NYH    = a constant integer .ge. N, the first dimension of YH. */
/*  YH1    = a one-dimensional array occupying the same space as YH. */
/*  EWT    = an array of length N containing multiplicative weights */
/*           for local error measurements.  Local errors in Y(i) are */
/*           compared to 1.0/EWT(i) in various error tests. */
/*  SAVF   = an array of working storage, of length N. */
/*           Also used for input of YH(*,MAXORD+2) when JSTART = -1 */
/*           and MAXORD .lt. the current order NQ. */
/*  ACOR   = a work array of length N, used for the accumulated */
/*           corrections.  On a successful return, ACOR(i) contains */
/*           the estimated one-step local error in Y(i). */
/*  WM,IWM = real and integer work arrays associated with matrix */
/*           operations in chord iteration (MITER .ne. 0). */
/*  PJAC   = name of routine to evaluate and preprocess Jacobian matrix */
/*           and P = I - h*el0*JAC, if a chord method is being used. */
/*  SLVS   = name of routine to solve linear system in chord iteration. */
/*  CCMAX  = maximum relative change in h*el0 before PJAC is called. */
/*  H      = the step size to be attempted on the next step. */
/*           H is altered by the error control algorithm during the */
/*           problem.  H can be either positive or negative, but its */
/*           sign must remain constant throughout the problem. */
/*  HMIN   = the minimum absolute value of the step size h to be used. */
/*  HMXI   = inverse of the maximum absolute value of h to be used. */
/*           HMXI = 0.0 is allowed and corresponds to an infinite hmax. */
/*           HMIN and HMXI may be changed at any time, but will not */
/*           take effect until the next change of h is considered. */
/*  TN     = the independent variable. TN is updated on each step taken. */
/*  JSTART = an integer used for input only, with the following */
/*           values and meanings: */
/*                0  perform the first step. */
/*            .gt.0  take a new step continuing from the last. */
/*               -1  take the next step with a new value of H, MAXORD, */
/*                     N, METH, MITER, and/or matrix parameters. */
/*               -2  take the next step with a new value of H, */
/*                     but with other inputs unchanged. */
/*           On return, JSTART is set to 1 to facilitate continuation. */
/*  KFLAG  = a completion code with the following meanings: */
/*                0  the step was succesful. */
/*               -1  the requested error could not be achieved. */
/*               -2  corrector convergence could not be achieved. */
/*               -3  fatal error in PJAC or SLVS. */
/*           A return with KFLAG = -1 or -2 means either */
/*           abs(H) = HMIN or 10 consecutive failures occurred. */
/*           On a return with KFLAG negative, the values of TN and */
/*           the YH array are as of the beginning of the last */
/*           step, and H is the last step size attempted. */
/*  MAXORD = the maximum order of integration method to be allowed. */
/*  MAXCOR = the maximum number of corrector iterations allowed. */
/*  MSBP   = maximum number of steps between PJAC calls (MITER .gt. 0). */
/*  MXNCF  = maximum number of convergence failures allowed. */
/*  METH/MITER = the method flags.  See description in driver. */
/*  N      = the number of first-order differential equations. */
/*  The values of CCMAX, H, HMIN, HMXI, TN, JSTART, KFLAG, MAXORD, */
/*  MAXCOR, MSBP, MXNCF, METH, MITER, and N are communicated via COMMON. */

/* ***SEE ALSO  SLSODE */
/* ***ROUTINES CALLED  SCFODE, SVNORM */
/* ***COMMON BLOCKS    SLS001 */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791129  DATE WRITTEN */
/*   890501  Modified prologue to SLATEC/LDOC format.  (FNF) */
/*   890503  Minor cosmetic changes.  (FNF) */
/*   930809  Renamed to allow single/double precision versions. (ACH) */
/*   010413  Reduced size of Common block /SLS001/. (ACH) */
/*   031105  Restored 'own' variables to Common block /SLS001/, to */
/*           enable interrupt/restart feature. (ACH) */
/* ***END PROLOGUE  SSTODE */
/* **End */

/* ***FIRST EXECUTABLE STATEMENT  SSTODE */
    /* Parameter adjustments */
    --neq;
    --y;
    yh_dim1 = *nyh;
    yh_offset = 1 + yh_dim1;
    yh -= yh_offset;
    --yh1;
    --ewt;
    --savf;
    --acor;
    --wm;
    --iwm;

    /* Function Body */
    sls001_4.kflag = 0;
    told = sls001_4.tn;
    ncf = 0;
    sls001_4.ierpj = 0;
    sls001_4.iersl = 0;
    sls001_4.jcur = 0;
    sls001_4.icf = 0;
    delp = 0.f;
    if (sls001_4.jstart > 0) {
	goto L200;
    }
    if (sls001_4.jstart == -1) {
	goto L100;
    }
    if (sls001_4.jstart == -2) {
	goto L160;
    }
/* ----------------------------------------------------------------------- */
/* On the first call, the order is set to 1, and other variables are */
/* initialized.  RMAX is the maximum ratio by which H can be increased */
/* in a single step.  It is initially 1.E4 to compensate for the small */
/* initial H, but then is normally equal to 10.  If a failure */
/* occurs (in corrector convergence or error test), RMAX is set to 2 */
/* for the next increase. */
/* ----------------------------------------------------------------------- */
    sls001_4.lmax = sls001_4.maxord + 1;
    sls001_4.nq = 1;
    sls001_4.l = 2;
    sls001_4.ialth = 2;
    sls001_4.rmax = 1e4f;
    sls001_4.rc = 0.f;
    sls001_4.el0 = 1.f;
    sls001_4.crate = .7f;
    sls001_4.hold = sls001_4.h__;
    sls001_4.meo = sls001_4.meth;
    sls001_4.nslp = 0;
    sls001_4.ipup = sls001_4.miter;
    iret = 3;
    goto L140;
/* ----------------------------------------------------------------------- */
/* The following block handles preliminaries needed when JSTART = -1. */
/* IPUP is set to MITER to force a matrix update. */
/* If an order increase is about to be considered (IALTH = 1), */
/* IALTH is reset to 2 to postpone consideration one more step. */
/* If the caller has changed METH, SCFODE is called to reset */
/* the coefficients of the method. */
/* If the caller has changed MAXORD to a value less than the current */
/* order NQ, NQ is reduced to MAXORD, and a new H chosen accordingly. */
/* If H is to be changed, YH must be rescaled. */
/* If H or METH is being changed, IALTH is reset to L = NQ + 1 */
/* to prevent further changes in H for that many steps. */
/* ----------------------------------------------------------------------- */
L100:
    sls001_4.ipup = sls001_4.miter;
    sls001_4.lmax = sls001_4.maxord + 1;
    if (sls001_4.ialth == 1) {
	sls001_4.ialth = 2;
    }
    if (sls001_4.meth == sls001_4.meo) {
	goto L110;
    }
    scfode_(&sls001_4.meth, sls001_4.elco, sls001_4.tesco);
    sls001_4.meo = sls001_4.meth;
    if (sls001_4.nq > sls001_4.maxord) {
	goto L120;
    }
    sls001_4.ialth = sls001_4.l;
    iret = 1;
    goto L150;
L110:
    if (sls001_4.nq <= sls001_4.maxord) {
	goto L160;
    }
L120:
    sls001_4.nq = sls001_4.maxord;
    sls001_4.l = sls001_4.lmax;
    i__1 = sls001_4.l;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L125: */
	sls001_4.el[i__ - 1] = sls001_4.elco[i__ + sls001_4.nq * 13 - 14];
    }
    sls001_4.nqnyh = sls001_4.nq * *nyh;
    sls001_4.rc = sls001_4.rc * sls001_4.el[0] / sls001_4.el0;
    sls001_4.el0 = sls001_4.el[0];
    sls001_4.conit = .5f / (sls001_4.nq + 2);
    ddn = svnorm_(&sls001_4.n, &savf[1], &ewt[1]) / sls001_4.tesco[sls001_4.l 
	    * 3 - 3];
    exdn = 1.f / sls001_4.l;
    d__1 = (doublereal) ddn;
    d__2 = (doublereal) exdn;
    rhdn = 1.f / (pow_dd(&d__1, &d__2) * 1.3f + 1.3e-6f);
    rh = dmin(rhdn,1.f);
    iredo = 3;
    if (sls001_4.h__ == sls001_4.hold) {
	goto L170;
    }
/* Computing MIN */
    r__2 = rh, r__3 = (r__1 = sls001_4.h__ / sls001_4.hold, dabs(r__1));
    rh = dmin(r__2,r__3);
    sls001_4.h__ = sls001_4.hold;
    goto L175;
/* ----------------------------------------------------------------------- */
/* SCFODE is called to get all the integration coefficients for the */
/* current METH.  Then the EL vector and related constants are reset */
/* whenever the order NQ is changed, or at the start of the problem. */
/* ----------------------------------------------------------------------- */
L140:
    scfode_(&sls001_4.meth, sls001_4.elco, sls001_4.tesco);
L150:
    i__1 = sls001_4.l;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L155: */
	sls001_4.el[i__ - 1] = sls001_4.elco[i__ + sls001_4.nq * 13 - 14];
    }
    sls001_4.nqnyh = sls001_4.nq * *nyh;
    sls001_4.rc = sls001_4.rc * sls001_4.el[0] / sls001_4.el0;
    sls001_4.el0 = sls001_4.el[0];
    sls001_4.conit = .5f / (sls001_4.nq + 2);
    switch (iret) {
	case 1:  goto L160;
	case 2:  goto L170;
	case 3:  goto L200;
    }
/* ----------------------------------------------------------------------- */
/* If H is being changed, the H ratio RH is checked against */
/* RMAX, HMIN, and HMXI, and the YH array rescaled.  IALTH is set to */
/* L = NQ + 1 to prevent a change of H for that many steps, unless */
/* forced by a convergence or error test failure. */
/* ----------------------------------------------------------------------- */
L160:
    if (sls001_4.h__ == sls001_4.hold) {
	goto L200;
    }
    rh = sls001_4.h__ / sls001_4.hold;
    sls001_4.h__ = sls001_4.hold;
    iredo = 3;
    goto L175;
L170:
/* Computing MAX */
    r__1 = rh, r__2 = sls001_4.hmin / dabs(sls001_4.h__);
    rh = dmax(r__1,r__2);
L175:
    rh = dmin(rh,sls001_4.rmax);
/* Computing MAX */
    r__1 = 1.f, r__2 = dabs(sls001_4.h__) * sls001_4.hmxi * rh;
    rh /= dmax(r__1,r__2);
    r__ = 1.f;
    i__1 = sls001_4.l;
    for (j = 2; j <= i__1; ++j) {
	r__ *= rh;
	i__2 = sls001_4.n;
	for (i__ = 1; i__ <= i__2; ++i__) {
/* L180: */
	    yh[i__ + j * yh_dim1] *= r__;
	}
    }
    sls001_4.h__ *= rh;
    sls001_4.rc *= rh;
    sls001_4.ialth = sls001_4.l;
    if (iredo == 0) {
	goto L690;
    }
/* ----------------------------------------------------------------------- */
/* This section computes the predicted values by effectively */
/* multiplying the YH array by the Pascal Triangle matrix. */
/* RC is the ratio of new to old values of the coefficient  H*EL(1). */
/* When RC differs from 1 by more than CCMAX, IPUP is set to MITER */
/* to force PJAC to be called, if a Jacobian is involved. */
/* In any case, PJAC is called at least every MSBP steps. */
/* ----------------------------------------------------------------------- */
L200:
    if ((r__1 = sls001_4.rc - 1.f, dabs(r__1)) > sls001_4.ccmax) {
	sls001_4.ipup = sls001_4.miter;
    }
    if (sls001_4.nst >= sls001_4.nslp + sls001_4.msbp) {
	sls001_4.ipup = sls001_4.miter;
    }
    sls001_4.tn += sls001_4.h__;
    i1 = sls001_4.nqnyh + 1;
    i__2 = sls001_4.nq;
    for (jb = 1; jb <= i__2; ++jb) {
	i1 -= *nyh;
/* dir$ ivdep */
	i__1 = sls001_4.nqnyh;
	for (i__ = i1; i__ <= i__1; ++i__) {
/* L210: */
	    yh1[i__] += yh1[i__ + *nyh];
	}
/* L215: */
    }
/* ----------------------------------------------------------------------- */
/* Up to MAXCOR corrector iterations are taken.  A convergence test is */
/* made on the R.M.S. norm of each correction, weighted by the error */
/* weight vector EWT.  The sum of the corrections is accumulated in the */
/* vector ACOR(i).  The YH array is not altered in the corrector loop. */
/* ----------------------------------------------------------------------- */
L220:
    m = 0;
    i__2 = sls001_4.n;
    for (i__ = 1; i__ <= i__2; ++i__) {
/* L230: */
	y[i__] = yh[i__ + yh_dim1];
    }
    (*f)(&neq[1], &sls001_4.tn, &y[1], &savf[1]);
    ++sls001_4.nfe;
    if (sls001_4.ipup <= 0) {
	goto L250;
    }
/* ----------------------------------------------------------------------- */
/* If indicated, the matrix P = I - h*el(1)*J is reevaluated and */
/* preprocessed before starting the corrector iteration.  IPUP is set */
/* to 0 as an indicator that this has been done. */
/* ----------------------------------------------------------------------- */
    (*pjac)(&neq[1], &y[1], &yh[yh_offset], nyh, &ewt[1], &acor[1], &savf[1], 
	    &wm[1], &iwm[1], (S_fp)f, (U_fp)jac);
    sls001_4.ipup = 0;
    sls001_4.rc = 1.f;
    sls001_4.nslp = sls001_4.nst;
    sls001_4.crate = .7f;
    if (sls001_4.ierpj != 0) {
	goto L430;
    }
L250:
    i__2 = sls001_4.n;
    for (i__ = 1; i__ <= i__2; ++i__) {
/* L260: */
	acor[i__] = 0.f;
    }
L270:
    if (sls001_4.miter != 0) {
	goto L350;
    }
/* ----------------------------------------------------------------------- */
/* In the case of functional iteration, update Y directly from */
/* the result of the last function evaluation. */
/* ----------------------------------------------------------------------- */
    i__2 = sls001_4.n;
    for (i__ = 1; i__ <= i__2; ++i__) {
	savf[i__] = sls001_4.h__ * savf[i__] - yh[i__ + (yh_dim1 << 1)];
/* L290: */
	y[i__] = savf[i__] - acor[i__];
    }
    del = svnorm_(&sls001_4.n, &y[1], &ewt[1]);
    i__2 = sls001_4.n;
    for (i__ = 1; i__ <= i__2; ++i__) {
	y[i__] = yh[i__ + yh_dim1] + sls001_4.el[0] * savf[i__];
/* L300: */
	acor[i__] = savf[i__];
    }
    goto L400;
/* ----------------------------------------------------------------------- */
/* In the case of the chord method, compute the corrector error, */
/* and solve the linear system with that as right-hand side and */
/* P as coefficient matrix. */
/* ----------------------------------------------------------------------- */
L350:
    i__2 = sls001_4.n;
    for (i__ = 1; i__ <= i__2; ++i__) {
/* L360: */
	y[i__] = sls001_4.h__ * savf[i__] - (yh[i__ + (yh_dim1 << 1)] + acor[
		i__]);
    }
    (*slvs)(&wm[1], &iwm[1], &y[1], &savf[1]);
    if (sls001_4.iersl < 0) {
	goto L430;
    }
    if (sls001_4.iersl > 0) {
	goto L410;
    }
    del = svnorm_(&sls001_4.n, &y[1], &ewt[1]);
    i__2 = sls001_4.n;
    for (i__ = 1; i__ <= i__2; ++i__) {
	acor[i__] += y[i__];
/* L380: */
	y[i__] = yh[i__ + yh_dim1] + sls001_4.el[0] * acor[i__];
    }
/* ----------------------------------------------------------------------- */
/* Test for convergence.  If M.gt.0, an estimate of the convergence */
/* rate constant is stored in CRATE, and this is used in the test. */
/* ----------------------------------------------------------------------- */
L400:
    if (m != 0) {
/* Computing MAX */
	r__1 = sls001_4.crate * .2f, r__2 = del / delp;
	sls001_4.crate = dmax(r__1,r__2);
    }
/* Computing MIN */
    r__1 = 1.f, r__2 = sls001_4.crate * 1.5f;
    dcon = del * dmin(r__1,r__2) / (sls001_4.tesco[sls001_4.nq * 3 - 2] * 
	    sls001_4.conit);
    if (dcon <= 1.f) {
	goto L450;
    }
    ++m;
    if (m == sls001_4.maxcor) {
	goto L410;
    }
    if (m >= 2 && del > delp * 2.f) {
	goto L410;
    }
    delp = del;
    (*f)(&neq[1], &sls001_4.tn, &y[1], &savf[1]);
    ++sls001_4.nfe;
    goto L270;
/* ----------------------------------------------------------------------- */
/* The corrector iteration failed to converge. */
/* If MITER .ne. 0 and the Jacobian is out of date, PJAC is called for */
/* the next try.  Otherwise the YH array is retracted to its values */
/* before prediction, and H is reduced, if possible.  If H cannot be */
/* reduced or MXNCF failures have occurred, exit with KFLAG = -2. */
/* ----------------------------------------------------------------------- */
L410:
    if (sls001_4.miter == 0 || sls001_4.jcur == 1) {
	goto L430;
    }
    sls001_4.icf = 1;
    sls001_4.ipup = sls001_4.miter;
    goto L220;
L430:
    sls001_4.icf = 2;
    ++ncf;
    sls001_4.rmax = 2.f;
    sls001_4.tn = told;
    i1 = sls001_4.nqnyh + 1;
    i__2 = sls001_4.nq;
    for (jb = 1; jb <= i__2; ++jb) {
	i1 -= *nyh;
/* dir$ ivdep */
	i__1 = sls001_4.nqnyh;
	for (i__ = i1; i__ <= i__1; ++i__) {
/* L440: */
	    yh1[i__] -= yh1[i__ + *nyh];
	}
/* L445: */
    }
    if (sls001_4.ierpj < 0 || sls001_4.iersl < 0) {
	goto L680;
    }
    if (dabs(sls001_4.h__) <= sls001_4.hmin * 1.00001f) {
	goto L670;
    }
    if (ncf == sls001_4.mxncf) {
	goto L670;
    }
    rh = .25f;
    sls001_4.ipup = sls001_4.miter;
    iredo = 1;
    goto L170;
/* ----------------------------------------------------------------------- */
/* The corrector has converged.  JCUR is set to 0 */
/* to signal that the Jacobian involved may need updating later. */
/* The local error test is made and control passes to statement 500 */
/* if it fails. */
/* ----------------------------------------------------------------------- */
L450:
    sls001_4.jcur = 0;
    if (m == 0) {
	dsm = del / sls001_4.tesco[sls001_4.nq * 3 - 2];
    }
    if (m > 0) {
	dsm = svnorm_(&sls001_4.n, &acor[1], &ewt[1]) / sls001_4.tesco[
		sls001_4.nq * 3 - 2];
    }
    if (dsm > 1.f) {
	goto L500;
    }
/* ----------------------------------------------------------------------- */
/* After a successful step, update the YH array. */
/* Consider changing H if IALTH = 1.  Otherwise decrease IALTH by 1. */
/* If IALTH is then 1 and NQ .lt. MAXORD, then ACOR is saved for */
/* use in a possible order increase on the next step. */
/* If a change in H is considered, an increase or decrease in order */
/* by one is considered also.  A change in H is made only if it is by a */
/* factor of at least 1.1.  If not, IALTH is set to 3 to prevent */
/* testing for that many steps. */
/* ----------------------------------------------------------------------- */
    sls001_4.kflag = 0;
    iredo = 0;
    ++sls001_4.nst;
    sls001_4.hu = sls001_4.h__;
    sls001_4.nqu = sls001_4.nq;
    i__2 = sls001_4.l;
    for (j = 1; j <= i__2; ++j) {
	i__1 = sls001_4.n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* L470: */
	    yh[i__ + j * yh_dim1] += sls001_4.el[j - 1] * acor[i__];
	}
    }
    --sls001_4.ialth;
    if (sls001_4.ialth == 0) {
	goto L520;
    }
    if (sls001_4.ialth > 1) {
	goto L700;
    }
    if (sls001_4.l == sls001_4.lmax) {
	goto L700;
    }
    i__1 = sls001_4.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L490: */
	yh[i__ + sls001_4.lmax * yh_dim1] = acor[i__];
    }
    goto L700;
/* ----------------------------------------------------------------------- */
/* The error test failed.  KFLAG keeps track of multiple failures. */
/* Restore TN and the YH array to their previous values, and prepare */
/* to try the step again.  Compute the optimum step size for this or */
/* one lower order.  After 2 or more failures, H is forced to decrease */
/* by a factor of 0.2 or less. */
/* ----------------------------------------------------------------------- */
L500:
    --sls001_4.kflag;
    sls001_4.tn = told;
    i1 = sls001_4.nqnyh + 1;
    i__1 = sls001_4.nq;
    for (jb = 1; jb <= i__1; ++jb) {
	i1 -= *nyh;
/* dir$ ivdep */
	i__2 = sls001_4.nqnyh;
	for (i__ = i1; i__ <= i__2; ++i__) {
/* L510: */
	    yh1[i__] -= yh1[i__ + *nyh];
	}
/* L515: */
    }
    sls001_4.rmax = 2.f;
    if (dabs(sls001_4.h__) <= sls001_4.hmin * 1.00001f) {
	goto L660;
    }
    if (sls001_4.kflag <= -3) {
	goto L640;
    }
    iredo = 2;
    rhup = 0.f;
    goto L540;
/* ----------------------------------------------------------------------- */
/* Regardless of the success or failure of the step, factors */
/* RHDN, RHSM, and RHUP are computed, by which H could be multiplied */
/* at order NQ - 1, order NQ, or order NQ + 1, respectively. */
/* In the case of failure, RHUP = 0.0 to avoid an order increase. */
/* The largest of these is determined and the new order chosen */
/* accordingly.  If the order is to be increased, we compute one */
/* additional scaled derivative. */
/* ----------------------------------------------------------------------- */
L520:
    rhup = 0.f;
    if (sls001_4.l == sls001_4.lmax) {
	goto L540;
    }
    i__1 = sls001_4.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L530: */
	savf[i__] = acor[i__] - yh[i__ + sls001_4.lmax * yh_dim1];
    }
    dup = svnorm_(&sls001_4.n, &savf[1], &ewt[1]) / sls001_4.tesco[
	    sls001_4.nq * 3 - 1];
    exup = 1.f / (sls001_4.l + 1);
    d__1 = (doublereal) dup;
    d__2 = (doublereal) exup;
    rhup = 1.f / (pow_dd(&d__1, &d__2) * 1.4f + 1.4e-6f);
L540:
    exsm = 1.f / sls001_4.l;
    d__1 = (doublereal) dsm;
    d__2 = (doublereal) exsm;
    rhsm = 1.f / (pow_dd(&d__1, &d__2) * 1.2f + 1.2e-6f);
    rhdn = 0.f;
    if (sls001_4.nq == 1) {
	goto L560;
    }
    ddn = svnorm_(&sls001_4.n, &yh[sls001_4.l * yh_dim1 + 1], &ewt[1]) / 
	    sls001_4.tesco[sls001_4.nq * 3 - 3];
    exdn = 1.f / sls001_4.nq;
    d__1 = (doublereal) ddn;
    d__2 = (doublereal) exdn;
    rhdn = 1.f / (pow_dd(&d__1, &d__2) * 1.3f + 1.3e-6f);
L560:
    if (rhsm >= rhup) {
	goto L570;
    }
    if (rhup > rhdn) {
	goto L590;
    }
    goto L580;
L570:
    if (rhsm < rhdn) {
	goto L580;
    }
    newq = sls001_4.nq;
    rh = rhsm;
    goto L620;
L580:
    newq = sls001_4.nq - 1;
    rh = rhdn;
    if (sls001_4.kflag < 0 && rh > 1.f) {
	rh = 1.f;
    }
    goto L620;
L590:
    newq = sls001_4.l;
    rh = rhup;
    if (rh < 1.1f) {
	goto L610;
    }
    r__ = sls001_4.el[sls001_4.l - 1] / sls001_4.l;
    i__1 = sls001_4.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L600: */
	yh[i__ + (newq + 1) * yh_dim1] = acor[i__] * r__;
    }
    goto L630;
L610:
    sls001_4.ialth = 3;
    goto L700;
L620:
    if (sls001_4.kflag == 0 && rh < 1.1f) {
	goto L610;
    }
    if (sls001_4.kflag <= -2) {
	rh = dmin(rh,.2f);
    }
/* ----------------------------------------------------------------------- */
/* If there is a change of order, reset NQ, l, and the coefficients. */
/* In any case H is reset according to RH and the YH array is rescaled. */
/* Then exit from 690 if the step was OK, or redo the step otherwise. */
/* ----------------------------------------------------------------------- */
    if (newq == sls001_4.nq) {
	goto L170;
    }
L630:
    sls001_4.nq = newq;
    sls001_4.l = sls001_4.nq + 1;
    iret = 2;
    goto L150;
/* ----------------------------------------------------------------------- */
/* Control reaches this section if 3 or more failures have occured. */
/* If 10 failures have occurred, exit with KFLAG = -1. */
/* It is assumed that the derivatives that have accumulated in the */
/* YH array have errors of the wrong order.  Hence the first */
/* derivative is recomputed, and the order is set to 1.  Then */
/* H is reduced by a factor of 10, and the step is retried, */
/* until it succeeds or H reaches HMIN. */
/* ----------------------------------------------------------------------- */
L640:
    if (sls001_4.kflag == -10) {
	goto L660;
    }
    rh = .1f;
/* Computing MAX */
    r__1 = sls001_4.hmin / dabs(sls001_4.h__);
    rh = dmax(r__1,rh);
    sls001_4.h__ *= rh;
    i__1 = sls001_4.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L645: */
	y[i__] = yh[i__ + yh_dim1];
    }
    (*f)(&neq[1], &sls001_4.tn, &y[1], &savf[1]);
    ++sls001_4.nfe;
    i__1 = sls001_4.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L650: */
	yh[i__ + (yh_dim1 << 1)] = sls001_4.h__ * savf[i__];
    }
    sls001_4.ipup = sls001_4.miter;
    sls001_4.ialth = 5;
    if (sls001_4.nq == 1) {
	goto L200;
    }
    sls001_4.nq = 1;
    sls001_4.l = 2;
    iret = 3;
    goto L150;
/* ----------------------------------------------------------------------- */
/* All returns are made through this section.  H is saved in HOLD */
/* to allow the caller to change H on the next step. */
/* ----------------------------------------------------------------------- */
L660:
    sls001_4.kflag = -1;
    goto L720;
L670:
    sls001_4.kflag = -2;
    goto L720;
L680:
    sls001_4.kflag = -3;
    goto L720;
L690:
    sls001_4.rmax = 10.f;
L700:
    r__ = 1.f / sls001_4.tesco[sls001_4.nqu * 3 - 2];
    i__1 = sls001_4.n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L710: */
	acor[i__] *= r__;
    }
L720:
    sls001_4.hold = sls001_4.h__;
    sls001_4.jstart = 1;
    return 0;
/* ----------------------- END OF SUBROUTINE SSTODE ---------------------- */
} /* sstode_ */

/* DECK SEWSET */
/* Subroutine */ int sewset_(integer *n, integer *itol, real *rtol, real *
	atol, real *ycur, real *ewt)
{
    /* System generated locals */
    integer i__1;
    real r__1;

    /* Local variables */
    static integer i__;

/* ***BEGIN PROLOGUE  SEWSET */
/* ***SUBSIDIARY */
/* ***PURPOSE  Set error weight vector. */
/* ***TYPE      SINGLE PRECISION (SEWSET-S, DEWSET-D) */
/* ***AUTHOR  Hindmarsh, Alan C., (LLNL) */
/* ***DESCRIPTION */

/*  This subroutine sets the error weight vector EWT according to */
/*      EWT(i) = RTOL(i)*ABS(YCUR(i)) + ATOL(i),  i = 1,...,N, */
/*  with the subscript on RTOL and/or ATOL possibly replaced by 1 above, */
/*  depending on the value of ITOL. */

/* ***SEE ALSO  SLSODE */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791129  DATE WRITTEN */
/*   890501  Modified prologue to SLATEC/LDOC format.  (FNF) */
/*   890503  Minor cosmetic changes.  (FNF) */
/*   930809  Renamed to allow single/double precision versions. (ACH) */
/* ***END PROLOGUE  SEWSET */
/* **End */

/* ***FIRST EXECUTABLE STATEMENT  SEWSET */
    /* Parameter adjustments */
    --ewt;
    --ycur;
    --rtol;
    --atol;

    /* Function Body */
    switch (*itol) {
	case 1:  goto L10;
	case 2:  goto L20;
	case 3:  goto L30;
	case 4:  goto L40;
    }
L10:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L15: */
	ewt[i__] = rtol[1] * (r__1 = ycur[i__], dabs(r__1)) + atol[1];
    }
    return 0;
L20:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L25: */
	ewt[i__] = rtol[1] * (r__1 = ycur[i__], dabs(r__1)) + atol[i__];
    }
    return 0;
L30:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L35: */
	ewt[i__] = rtol[i__] * (r__1 = ycur[i__], dabs(r__1)) + atol[1];
    }
    return 0;
L40:
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L45: */
	ewt[i__] = rtol[i__] * (r__1 = ycur[i__], dabs(r__1)) + atol[i__];
    }
    return 0;
/* ----------------------- END OF SUBROUTINE SEWSET ---------------------- */
} /* sewset_ */

/* DECK SVNORM */
doublereal svnorm_(integer *n, real *v, real *w)
{
    /* System generated locals */
    integer i__1;
    real ret_val, r__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    static integer i__;
    static real sum;

/* ***BEGIN PROLOGUE  SVNORM */
/* ***SUBSIDIARY */
/* ***PURPOSE  Weighted root-mean-square vector norm. */
/* ***TYPE      SINGLE PRECISION (SVNORM-S, DVNORM-D) */
/* ***AUTHOR  Hindmarsh, Alan C., (LLNL) */
/* ***DESCRIPTION */

/*  This function routine computes the weighted root-mean-square norm */
/*  of the vector of length N contained in the array V, with weights */
/*  contained in the array W of length N: */
/*    SVNORM = SQRT( (1/N) * SUM( V(i)*W(i) )**2 ) */

/* ***SEE ALSO  SLSODE */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791129  DATE WRITTEN */
/*   890501  Modified prologue to SLATEC/LDOC format.  (FNF) */
/*   890503  Minor cosmetic changes.  (FNF) */
/*   930809  Renamed to allow single/double precision versions. (ACH) */
/* ***END PROLOGUE  SVNORM */
/* **End */

/* ***FIRST EXECUTABLE STATEMENT  SVNORM */
    /* Parameter adjustments */
    --w;
    --v;

    /* Function Body */
    sum = 0.f;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* L10: */
/* Computing 2nd power */
	r__1 = v[i__] * w[i__];
	sum += r__1 * r__1;
    }
    ret_val = sqrt(sum / *n);
    return ret_val;
/* ----------------------- END OF FUNCTION SVNORM ------------------------ */
} /* svnorm_ */

/* DECK SGEFA */
/* Subroutine */ int sgefa_(real *a, integer *lda, integer *n, integer *ipvt, 
	integer *info)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3;

    /* Local variables */
    static integer j, k, l;
    static real t;
    static integer kp1, nm1;
    extern /* Subroutine */ int sscal_(integer *, real *, real *, integer *), 
	    saxpy_(integer *, real *, real *, integer *, real *, integer *);
    extern integer isamax_(integer *, real *, integer *);

/* ***BEGIN PROLOGUE  SGEFA */
/* ***PURPOSE  Factor a matrix using Gaussian elimination. */
/* ***CATEGORY  D2A1 */
/* ***TYPE      SINGLE PRECISION (SGEFA-S, DGEFA-D, CGEFA-C) */
/* ***KEYWORDS  GENERAL MATRIX, LINEAR ALGEBRA, LINPACK, */
/*             MATRIX FACTORIZATION */
/* ***AUTHOR  Moler, C. B., (U. of New Mexico) */
/* ***DESCRIPTION */

/*     SGEFA factors a real matrix by Gaussian elimination. */

/*     SGEFA is usually called by SGECO, but it can be called */
/*     directly with a saving in time if  RCOND  is not needed. */
/*     (Time for SGECO) = (1 + 9/N)*(Time for SGEFA) . */

/*     On Entry */

/*        A       REAL(LDA, N) */
/*                the matrix to be factored. */

/*        LDA     INTEGER */
/*                the leading dimension of the array  A . */

/*        N       INTEGER */
/*                the order of the matrix  A . */

/*     On Return */

/*        A       an upper triangular matrix and the multipliers */
/*                which were used to obtain it. */
/*                The factorization can be written  A = L*U , where */
/*                L  is a product of permutation and unit lower */
/*                triangular matrices and  U  is upper triangular. */

/*        IPVT    INTEGER(N) */
/*                an integer vector of pivot indices. */

/*        INFO    INTEGER */
/*                = 0  normal value. */
/*                = K  if  U(K,K) .EQ. 0.0 .  This is not an error */
/*                     condition for this subroutine, but it does */
/*                     indicate that SGESL or SGEDI will divide by zero */
/*                     if called.  Use  RCOND  in SGECO for a reliable */
/*                     indication of singularity. */

/* ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W. */
/*                 Stewart, LINPACK Users' Guide, SIAM, 1979. */
/* ***ROUTINES CALLED  ISAMAX, SAXPY, SSCAL */
/* ***REVISION HISTORY  (YYMMDD) */
/*   780814  DATE WRITTEN */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900326  Removed duplicate information from DESCRIPTION section. */
/*           (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  SGEFA */


/*     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING */

/* ***FIRST EXECUTABLE STATEMENT  SGEFA */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --ipvt;

    /* Function Body */
    *info = 0;
    nm1 = *n - 1;
    if (nm1 < 1) {
	goto L70;
    }
    i__1 = nm1;
    for (k = 1; k <= i__1; ++k) {
	kp1 = k + 1;

/*        FIND L = PIVOT INDEX */

	i__2 = *n - k + 1;
	l = isamax_(&i__2, &a[k + k * a_dim1], &c__1) + k - 1;
	ipvt[k] = l;

/*        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED */

	if (a[l + k * a_dim1] == 0.f) {
	    goto L40;
	}

/*           INTERCHANGE IF NECESSARY */

	if (l == k) {
	    goto L10;
	}
	t = a[l + k * a_dim1];
	a[l + k * a_dim1] = a[k + k * a_dim1];
	a[k + k * a_dim1] = t;
L10:

/*           COMPUTE MULTIPLIERS */

	t = -1.f / a[k + k * a_dim1];
	i__2 = *n - k;
	sscal_(&i__2, &t, &a[k + 1 + k * a_dim1], &c__1);

/*           ROW ELIMINATION WITH COLUMN INDEXING */

	i__2 = *n;
	for (j = kp1; j <= i__2; ++j) {
	    t = a[l + j * a_dim1];
	    if (l == k) {
		goto L20;
	    }
	    a[l + j * a_dim1] = a[k + j * a_dim1];
	    a[k + j * a_dim1] = t;
L20:
	    i__3 = *n - k;
	    saxpy_(&i__3, &t, &a[k + 1 + k * a_dim1], &c__1, &a[k + 1 + j * 
		    a_dim1], &c__1);
/* L30: */
	}
	goto L50;
L40:
	*info = k;
L50:
/* L60: */
	;
    }
L70:
    ipvt[*n] = *n;
    if (a[*n + *n * a_dim1] == 0.f) {
	*info = *n;
    }
    return 0;
} /* sgefa_ */

/* DECK SGESL */
/* Subroutine */ int sgesl_(real *a, integer *lda, integer *n, integer *ipvt, 
	real *b, integer *job)
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2;

    /* Local variables */
    static integer k, l;
    static real t;
    static integer kb, nm1;
    extern doublereal sdot_(integer *, real *, integer *, real *, integer *);
    extern /* Subroutine */ int saxpy_(integer *, real *, real *, integer *, 
	    real *, integer *);

/* ***BEGIN PROLOGUE  SGESL */
/* ***PURPOSE  Solve the real system A*X=B or TRANS(A)*X=B using the */
/*            factors of SGECO or SGEFA. */
/* ***CATEGORY  D2A1 */
/* ***TYPE      SINGLE PRECISION (SGESL-S, DGESL-D, CGESL-C) */
/* ***KEYWORDS  LINEAR ALGEBRA, LINPACK, MATRIX, SOLVE */
/* ***AUTHOR  Moler, C. B., (U. of New Mexico) */
/* ***DESCRIPTION */

/*     SGESL solves the real system */
/*     A * X = B  or  TRANS(A) * X = B */
/*     using the factors computed by SGECO or SGEFA. */

/*     On Entry */

/*        A       REAL(LDA, N) */
/*                the output from SGECO or SGEFA. */

/*        LDA     INTEGER */
/*                the leading dimension of the array  A . */

/*        N       INTEGER */
/*                the order of the matrix  A . */

/*        IPVT    INTEGER(N) */
/*                the pivot vector from SGECO or SGEFA. */

/*        B       REAL(N) */
/*                the right hand side vector. */

/*        JOB     INTEGER */
/*                = 0         to solve  A*X = B , */
/*                = nonzero   to solve  TRANS(A)*X = B  where */
/*                            TRANS(A)  is the transpose. */

/*     On Return */

/*        B       the solution vector  X . */

/*     Error Condition */

/*        A division by zero will occur if the input factor contains a */
/*        zero on the diagonal.  Technically, this indicates singularity, */
/*        but it is often caused by improper arguments or improper */
/*        setting of LDA .  It will not occur if the subroutines are */
/*        called correctly and if SGECO has set RCOND .GT. 0.0 */
/*        or SGEFA has set INFO .EQ. 0 . */

/*     To compute  INVERSE(A) * C  where  C  is a matrix */
/*     with  P  columns */
/*           CALL SGECO(A,LDA,N,IPVT,RCOND,Z) */
/*           IF (RCOND is too small) GO TO ... */
/*           DO 10 J = 1, P */
/*              CALL SGESL(A,LDA,N,IPVT,C(1,J),0) */
/*        10 CONTINUE */

/* ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W. */
/*                 Stewart, LINPACK Users' Guide, SIAM, 1979. */
/* ***ROUTINES CALLED  SAXPY, SDOT */
/* ***REVISION HISTORY  (YYMMDD) */
/*   780814  DATE WRITTEN */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900326  Removed duplicate information from DESCRIPTION section. */
/*           (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  SGESL */

/* ***FIRST EXECUTABLE STATEMENT  SGESL */
    /* Parameter adjustments */
    a_dim1 = *lda;
    a_offset = 1 + a_dim1;
    a -= a_offset;
    --ipvt;
    --b;

    /* Function Body */
    nm1 = *n - 1;
    if (*job != 0) {
	goto L50;
    }

/*        JOB = 0 , SOLVE  A * X = B */
/*        FIRST SOLVE  L*Y = B */

    if (nm1 < 1) {
	goto L30;
    }
    i__1 = nm1;
    for (k = 1; k <= i__1; ++k) {
	l = ipvt[k];
	t = b[l];
	if (l == k) {
	    goto L10;
	}
	b[l] = b[k];
	b[k] = t;
L10:
	i__2 = *n - k;
	saxpy_(&i__2, &t, &a[k + 1 + k * a_dim1], &c__1, &b[k + 1], &c__1);
/* L20: */
    }
L30:

/*        NOW SOLVE  U*X = Y */

    i__1 = *n;
    for (kb = 1; kb <= i__1; ++kb) {
	k = *n + 1 - kb;
	b[k] /= a[k + k * a_dim1];
	t = -b[k];
	i__2 = k - 1;
	saxpy_(&i__2, &t, &a[k * a_dim1 + 1], &c__1, &b[1], &c__1);
/* L40: */
    }
    goto L100;
L50:

/*        JOB = NONZERO, SOLVE  TRANS(A) * X = B */
/*        FIRST SOLVE  TRANS(U)*Y = B */

    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	i__2 = k - 1;
	t = sdot_(&i__2, &a[k * a_dim1 + 1], &c__1, &b[1], &c__1);
	b[k] = (b[k] - t) / a[k + k * a_dim1];
/* L60: */
    }

/*        NOW SOLVE TRANS(L)*X = Y */

    if (nm1 < 1) {
	goto L90;
    }
    i__1 = nm1;
    for (kb = 1; kb <= i__1; ++kb) {
	k = *n - kb;
	i__2 = *n - k;
	b[k] += sdot_(&i__2, &a[k + 1 + k * a_dim1], &c__1, &b[k + 1], &c__1);
	l = ipvt[k];
	if (l == k) {
	    goto L70;
	}
	t = b[l];
	b[l] = b[k];
	b[k] = t;
L70:
/* L80: */
	;
    }
L90:
L100:
    return 0;
} /* sgesl_ */

/* DECK SGBFA */
/* Subroutine */ int sgbfa_(real *abd, integer *lda, integer *n, integer *ml, 
	integer *mu, integer *ipvt, integer *info)
{
    /* System generated locals */
    integer abd_dim1, abd_offset, i__1, i__2, i__3, i__4;

    /* Local variables */
    static integer i__, j, k, l, m;
    static real t;
    static integer i0, j0, j1, lm, mm, ju, jz, kp1, nm1;
    extern /* Subroutine */ int sscal_(integer *, real *, real *, integer *), 
	    saxpy_(integer *, real *, real *, integer *, real *, integer *);
    extern integer isamax_(integer *, real *, integer *);

/* ***BEGIN PROLOGUE  SGBFA */
/* ***PURPOSE  Factor a band matrix using Gaussian elimination. */
/* ***CATEGORY  D2A2 */
/* ***TYPE      SINGLE PRECISION (SGBFA-S, DGBFA-D, CGBFA-C) */
/* ***KEYWORDS  BANDED, LINEAR ALGEBRA, LINPACK, MATRIX FACTORIZATION */
/* ***AUTHOR  Moler, C. B., (U. of New Mexico) */
/* ***DESCRIPTION */

/*     SGBFA factors a real band matrix by elimination. */

/*     SGBFA is usually called by SBGCO, but it can be called */
/*     directly with a saving in time if  RCOND  is not needed. */

/*     On Entry */

/*        ABD     REAL(LDA, N) */
/*                contains the matrix in band storage.  The columns */
/*                of the matrix are stored in the columns of  ABD  and */
/*                the diagonals of the matrix are stored in rows */
/*                ML+1 through 2*ML+MU+1 of  ABD . */
/*                See the comments below for details. */

/*        LDA     INTEGER */
/*                the leading dimension of the array  ABD . */
/*                LDA must be .GE. 2*ML + MU + 1 . */

/*        N       INTEGER */
/*                the order of the original matrix. */

/*        ML      INTEGER */
/*                number of diagonals below the main diagonal. */
/*                0 .LE. ML .LT. N . */

/*        MU      INTEGER */
/*                number of diagonals above the main diagonal. */
/*                0 .LE. MU .LT. N . */
/*                More efficient if  ML .LE. MU . */
/*     On Return */

/*        ABD     an upper triangular matrix in band storage and */
/*                the multipliers which were used to obtain it. */
/*                The factorization can be written  A = L*U , where */
/*                L  is a product of permutation and unit lower */
/*                triangular matrices and  U  is upper triangular. */

/*        IPVT    INTEGER(N) */
/*                an integer vector of pivot indices. */

/*        INFO    INTEGER */
/*                = 0  normal value. */
/*                = K  if  U(K,K) .EQ. 0.0 .  This is not an error */
/*                     condition for this subroutine, but it does */
/*                     indicate that SGBSL will divide by zero if */
/*                     called.  Use  RCOND  in SBGCO for a reliable */
/*                     indication of singularity. */

/*     Band Storage */

/*           If  A  is a band matrix, the following program segment */
/*           will set up the input. */

/*                   ML = (band width below the diagonal) */
/*                   MU = (band width above the diagonal) */
/*                   M = ML + MU + 1 */
/*                   DO 20 J = 1, N */
/*                      I1 = MAX(1, J-MU) */
/*                      I2 = MIN(N, J+ML) */
/*                      DO 10 I = I1, I2 */
/*                         K = I - J + M */
/*                         ABD(K,J) = A(I,J) */
/*                10    CONTINUE */
/*                20 CONTINUE */

/*           This uses rows  ML+1  through  2*ML+MU+1  of  ABD . */
/*           In addition, the first  ML  rows in  ABD  are used for */
/*           elements generated during the triangularization. */
/*           The total number of rows needed in  ABD  is  2*ML+MU+1 . */
/*           The  ML+MU by ML+MU  upper left triangle and the */
/*           ML by ML  lower right triangle are not referenced. */

/* ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W. */
/*                 Stewart, LINPACK Users' Guide, SIAM, 1979. */
/* ***ROUTINES CALLED  ISAMAX, SAXPY, SSCAL */
/* ***REVISION HISTORY  (YYMMDD) */
/*   780814  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900326  Removed duplicate information from DESCRIPTION section. */
/*           (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  SGBFA */


/* ***FIRST EXECUTABLE STATEMENT  SGBFA */
    /* Parameter adjustments */
    abd_dim1 = *lda;
    abd_offset = 1 + abd_dim1;
    abd -= abd_offset;
    --ipvt;

    /* Function Body */
    m = *ml + *mu + 1;
    *info = 0;

/*     ZERO INITIAL FILL-IN COLUMNS */

    j0 = *mu + 2;
    j1 = min(*n,m) - 1;
    if (j1 < j0) {
	goto L30;
    }
    i__1 = j1;
    for (jz = j0; jz <= i__1; ++jz) {
	i0 = m + 1 - jz;
	i__2 = *ml;
	for (i__ = i0; i__ <= i__2; ++i__) {
	    abd[i__ + jz * abd_dim1] = 0.f;
/* L10: */
	}
/* L20: */
    }
L30:
    jz = j1;
    ju = 0;

/*     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING */

    nm1 = *n - 1;
    if (nm1 < 1) {
	goto L130;
    }
    i__1 = nm1;
    for (k = 1; k <= i__1; ++k) {
	kp1 = k + 1;

/*        ZERO NEXT FILL-IN COLUMN */

	++jz;
	if (jz > *n) {
	    goto L50;
	}
	if (*ml < 1) {
	    goto L50;
	}
	i__2 = *ml;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    abd[i__ + jz * abd_dim1] = 0.f;
/* L40: */
	}
L50:

/*        FIND L = PIVOT INDEX */

/* Computing MIN */
	i__2 = *ml, i__3 = *n - k;
	lm = min(i__2,i__3);
	i__2 = lm + 1;
	l = isamax_(&i__2, &abd[m + k * abd_dim1], &c__1) + m - 1;
	ipvt[k] = l + k - m;

/*        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED */

	if (abd[l + k * abd_dim1] == 0.f) {
	    goto L100;
	}

/*           INTERCHANGE IF NECESSARY */

	if (l == m) {
	    goto L60;
	}
	t = abd[l + k * abd_dim1];
	abd[l + k * abd_dim1] = abd[m + k * abd_dim1];
	abd[m + k * abd_dim1] = t;
L60:

/*           COMPUTE MULTIPLIERS */

	t = -1.f / abd[m + k * abd_dim1];
	sscal_(&lm, &t, &abd[m + 1 + k * abd_dim1], &c__1);

/*           ROW ELIMINATION WITH COLUMN INDEXING */

/* Computing MIN */
/* Computing MAX */
	i__3 = ju, i__4 = *mu + ipvt[k];
	i__2 = max(i__3,i__4);
	ju = min(i__2,*n);
	mm = m;
	if (ju < kp1) {
	    goto L90;
	}
	i__2 = ju;
	for (j = kp1; j <= i__2; ++j) {
	    --l;
	    --mm;
	    t = abd[l + j * abd_dim1];
	    if (l == mm) {
		goto L70;
	    }
	    abd[l + j * abd_dim1] = abd[mm + j * abd_dim1];
	    abd[mm + j * abd_dim1] = t;
L70:
	    saxpy_(&lm, &t, &abd[m + 1 + k * abd_dim1], &c__1, &abd[mm + 1 + 
		    j * abd_dim1], &c__1);
/* L80: */
	}
L90:
	goto L110;
L100:
	*info = k;
L110:
/* L120: */
	;
    }
L130:
    ipvt[*n] = *n;
    if (abd[m + *n * abd_dim1] == 0.f) {
	*info = *n;
    }
    return 0;
} /* sgbfa_ */

/* DECK SGBSL */
/* Subroutine */ int sgbsl_(real *abd, integer *lda, integer *n, integer *ml, 
	integer *mu, integer *ipvt, real *b, integer *job)
{
    /* System generated locals */
    integer abd_dim1, abd_offset, i__1, i__2, i__3;

    /* Local variables */
    static integer k, l, m;
    static real t;
    static integer kb, la, lb, lm, nm1;
    extern doublereal sdot_(integer *, real *, integer *, real *, integer *);
    extern /* Subroutine */ int saxpy_(integer *, real *, real *, integer *, 
	    real *, integer *);

/* ***BEGIN PROLOGUE  SGBSL */
/* ***PURPOSE  Solve the real band system A*X=B or TRANS(A)*X=B using */
/*            the factors computed by SGBCO or SGBFA. */
/* ***CATEGORY  D2A2 */
/* ***TYPE      SINGLE PRECISION (SGBSL-S, DGBSL-D, CGBSL-C) */
/* ***KEYWORDS  BANDED, LINEAR ALGEBRA, LINPACK, MATRIX, SOLVE */
/* ***AUTHOR  Moler, C. B., (U. of New Mexico) */
/* ***DESCRIPTION */

/*     SGBSL solves the real band system */
/*     A * X = B  or  TRANS(A) * X = B */
/*     using the factors computed by SBGCO or SGBFA. */

/*     On Entry */

/*        ABD     REAL(LDA, N) */
/*                the output from SBGCO or SGBFA. */

/*        LDA     INTEGER */
/*                the leading dimension of the array  ABD . */

/*        N       INTEGER */
/*                the order of the original matrix. */

/*        ML      INTEGER */
/*                number of diagonals below the main diagonal. */

/*        MU      INTEGER */
/*                number of diagonals above the main diagonal. */

/*        IPVT    INTEGER(N) */
/*                the pivot vector from SBGCO or SGBFA. */

/*        B       REAL(N) */
/*                the right hand side vector. */

/*        JOB     INTEGER */
/*                = 0         to solve  A*X = B , */
/*                = nonzero   to solve  TRANS(A)*X = B , where */
/*                            TRANS(A)  is the transpose. */

/*     On Return */

/*        B       the solution vector  X . */

/*     Error Condition */

/*        A division by zero will occur if the input factor contains a */
/*        zero on the diagonal.  Technically, this indicates singularity, */
/*        but it is often caused by improper arguments or improper */
/*        setting of LDA .  It will not occur if the subroutines are */
/*        called correctly and if SBGCO has set RCOND .GT. 0.0 */
/*        or SGBFA has set INFO .EQ. 0 . */

/*     To compute  INVERSE(A) * C  where  C  is a matrix */
/*     with  P  columns */
/*           CALL SBGCO(ABD,LDA,N,ML,MU,IPVT,RCOND,Z) */
/*           If (RCOND is too small) GO TO ... */
/*           DO 10 J = 1, P */
/*              CALL SGBSL(ABD,LDA,N,ML,MU,IPVT,C(1,J),0) */
/*        10 CONTINUE */

/* ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W. */
/*                 Stewart, LINPACK Users' Guide, SIAM, 1979. */
/* ***ROUTINES CALLED  SAXPY, SDOT */
/* ***REVISION HISTORY  (YYMMDD) */
/*   780814  DATE WRITTEN */
/*   890531  Changed all specific intrinsics to generic.  (WRB) */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900326  Removed duplicate information from DESCRIPTION section. */
/*           (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  SGBSL */

/* ***FIRST EXECUTABLE STATEMENT  SGBSL */
    /* Parameter adjustments */
    abd_dim1 = *lda;
    abd_offset = 1 + abd_dim1;
    abd -= abd_offset;
    --ipvt;
    --b;

    /* Function Body */
    m = *mu + *ml + 1;
    nm1 = *n - 1;
    if (*job != 0) {
	goto L50;
    }

/*        JOB = 0 , SOLVE  A * X = B */
/*        FIRST SOLVE L*Y = B */

    if (*ml == 0) {
	goto L30;
    }
    if (nm1 < 1) {
	goto L30;
    }
    i__1 = nm1;
    for (k = 1; k <= i__1; ++k) {
/* Computing MIN */
	i__2 = *ml, i__3 = *n - k;
	lm = min(i__2,i__3);
	l = ipvt[k];
	t = b[l];
	if (l == k) {
	    goto L10;
	}
	b[l] = b[k];
	b[k] = t;
L10:
	saxpy_(&lm, &t, &abd[m + 1 + k * abd_dim1], &c__1, &b[k + 1], &c__1);
/* L20: */
    }
L30:

/*        NOW SOLVE  U*X = Y */

    i__1 = *n;
    for (kb = 1; kb <= i__1; ++kb) {
	k = *n + 1 - kb;
	b[k] /= abd[m + k * abd_dim1];
	lm = min(k,m) - 1;
	la = m - lm;
	lb = k - lm;
	t = -b[k];
	saxpy_(&lm, &t, &abd[la + k * abd_dim1], &c__1, &b[lb], &c__1);
/* L40: */
    }
    goto L100;
L50:

/*        JOB = NONZERO, SOLVE  TRANS(A) * X = B */
/*        FIRST SOLVE  TRANS(U)*Y = B */

    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	lm = min(k,m) - 1;
	la = m - lm;
	lb = k - lm;
	t = sdot_(&lm, &abd[la + k * abd_dim1], &c__1, &b[lb], &c__1);
	b[k] = (b[k] - t) / abd[m + k * abd_dim1];
/* L60: */
    }

/*        NOW SOLVE TRANS(L)*X = Y */

    if (*ml == 0) {
	goto L90;
    }
    if (nm1 < 1) {
	goto L90;
    }
    i__1 = nm1;
    for (kb = 1; kb <= i__1; ++kb) {
	k = *n - kb;
/* Computing MIN */
	i__2 = *ml, i__3 = *n - k;
	lm = min(i__2,i__3);
	b[k] += sdot_(&lm, &abd[m + 1 + k * abd_dim1], &c__1, &b[k + 1], &
		c__1);
	l = ipvt[k];
	if (l == k) {
	    goto L70;
	}
	t = b[l];
	b[l] = b[k];
	b[k] = t;
L70:
/* L80: */
	;
    }
L90:
L100:
    return 0;
} /* sgbsl_ */

/* DECK SAXPY */
/* Subroutine */ int saxpy_(integer *n, real *sa, real *sx, integer *incx, 
	real *sy, integer *incy)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i__, m, ix, iy, ns, mp1;

/* ***BEGIN PROLOGUE  SAXPY */
/* ***PURPOSE  Compute a constant times a vector plus a vector. */
/* ***CATEGORY  D1A7 */
/* ***TYPE      SINGLE PRECISION (SAXPY-S, DAXPY-D, CAXPY-C) */
/* ***KEYWORDS  BLAS, LINEAR ALGEBRA, TRIAD, VECTOR */
/* ***AUTHOR  Lawson, C. L., (JPL) */
/*           Hanson, R. J., (SNLA) */
/*           Kincaid, D. R., (U. of Texas) */
/*           Krogh, F. T., (JPL) */
/* ***DESCRIPTION */

/*                B L A S  Subprogram */
/*    Description of Parameters */

/*     --Input-- */
/*        N  number of elements in input vector(s) */
/*       SA  single precision scalar multiplier */
/*       SX  single precision vector with N elements */
/*     INCX  storage spacing between elements of SX */
/*       SY  single precision vector with N elements */
/*     INCY  storage spacing between elements of SY */

/*     --Output-- */
/*       SY  single precision result (unchanged if N .LE. 0) */

/*     Overwrite single precision SY with single precision SA*SX +SY. */
/*     For I = 0 to N-1, replace  SY(LY+I*INCY) with SA*SX(LX+I*INCX) + */
/*       SY(LY+I*INCY), */
/*     where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is */
/*     defined in a similar way using INCY. */

/* ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T. */
/*                 Krogh, Basic linear algebra subprograms for Fortran */
/*                 usage, Algorithm No. 539, Transactions on Mathematical */
/*                 Software 5, 3 (September 1979), pp. 308-323. */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791001  DATE WRITTEN */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   920310  Corrected definition of LX in DESCRIPTION.  (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  SAXPY */
/* ***FIRST EXECUTABLE STATEMENT  SAXPY */
    /* Parameter adjustments */
    --sy;
    --sx;

    /* Function Body */
    if (*n <= 0 || *sa == 0.f) {
	return 0;
    }
    if (*incx == *incy) {
	if ((i__1 = *incx - 1) < 0) {
	    goto L5;
	} else if (i__1 == 0) {
	    goto L20;
	} else {
	    goto L60;
	}
    }

/*     Code for unequal or nonpositive increments. */

L5:
    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sy[iy] += *sa * sx[ix];
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;

/*     Code for both increments equal to 1. */

/*     Clean-up loop so remaining vector length is a multiple of 4. */

L20:
    m = *n % 4;
    if (m == 0) {
	goto L40;
    }
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sy[i__] += *sa * sx[i__];
/* L30: */
    }
    if (*n < 4) {
	return 0;
    }
L40:
    mp1 = m + 1;
    i__1 = *n;
    for (i__ = mp1; i__ <= i__1; i__ += 4) {
	sy[i__] += *sa * sx[i__];
	sy[i__ + 1] += *sa * sx[i__ + 1];
	sy[i__ + 2] += *sa * sx[i__ + 2];
	sy[i__ + 3] += *sa * sx[i__ + 3];
/* L50: */
    }
    return 0;

/*     Code for equal, positive, non-unit increments. */

L60:
    ns = *n * *incx;
    i__1 = ns;
    i__2 = *incx;
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
	sy[i__] = *sa * sx[i__] + sy[i__];
/* L70: */
    }
    return 0;
} /* saxpy_ */

/* DECK SDOT */
doublereal sdot_(integer *n, real *sx, integer *incx, real *sy, integer *incy)
{
    /* System generated locals */
    integer i__1, i__2;
    real ret_val;

    /* Local variables */
    static integer i__, m, ix, iy, ns, mp1;

/* ***BEGIN PROLOGUE  SDOT */
/* ***PURPOSE  Compute the inner product of two vectors. */
/* ***CATEGORY  D1A4 */
/* ***TYPE      SINGLE PRECISION (SDOT-S, DDOT-D, CDOTU-C) */
/* ***KEYWORDS  BLAS, INNER PRODUCT, LINEAR ALGEBRA, VECTOR */
/* ***AUTHOR  Lawson, C. L., (JPL) */
/*           Hanson, R. J., (SNLA) */
/*           Kincaid, D. R., (U. of Texas) */
/*           Krogh, F. T., (JPL) */
/* ***DESCRIPTION */

/*                B L A S  Subprogram */
/*    Description of Parameters */

/*     --Input-- */
/*        N  number of elements in input vector(s) */
/*       SX  single precision vector with N elements */
/*     INCX  storage spacing between elements of SX */
/*       SY  single precision vector with N elements */
/*     INCY  storage spacing between elements of SY */

/*     --Output-- */
/*     SDOT  single precision dot product (zero if N .LE. 0) */

/*     Returns the dot product of single precision SX and SY. */
/*     SDOT = sum for I = 0 to N-1 of  SX(LX+I*INCX) * SY(LY+I*INCY), */
/*     where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is */
/*     defined in a similar way using INCY. */

/* ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T. */
/*                 Krogh, Basic linear algebra subprograms for Fortran */
/*                 usage, Algorithm No. 539, Transactions on Mathematical */
/*                 Software 5, 3 (September 1979), pp. 308-323. */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791001  DATE WRITTEN */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   920310  Corrected definition of LX in DESCRIPTION.  (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  SDOT */
/* ***FIRST EXECUTABLE STATEMENT  SDOT */
    /* Parameter adjustments */
    --sy;
    --sx;

    /* Function Body */
    ret_val = 0.f;
    if (*n <= 0) {
	return ret_val;
    }
    if (*incx == *incy) {
	if ((i__1 = *incx - 1) < 0) {
	    goto L5;
	} else if (i__1 == 0) {
	    goto L20;
	} else {
	    goto L60;
	}
    }

/*     Code for unequal or nonpositive increments. */

L5:
    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ret_val += sx[ix] * sy[iy];
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return ret_val;

/*     Code for both increments equal to 1. */

/*     Clean-up loop so remaining vector length is a multiple of 5. */

L20:
    m = *n % 5;
    if (m == 0) {
	goto L40;
    }
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ret_val += sx[i__] * sy[i__];
/* L30: */
    }
    if (*n < 5) {
	return ret_val;
    }
L40:
    mp1 = m + 1;
    i__1 = *n;
    for (i__ = mp1; i__ <= i__1; i__ += 5) {
	ret_val = ret_val + sx[i__] * sy[i__] + sx[i__ + 1] * sy[i__ + 1] + 
		sx[i__ + 2] * sy[i__ + 2] + sx[i__ + 3] * sy[i__ + 3] + sx[
		i__ + 4] * sy[i__ + 4];
/* L50: */
    }
    return ret_val;

/*     Code for equal, positive, non-unit increments. */

L60:
    ns = *n * *incx;
    i__1 = ns;
    i__2 = *incx;
    for (i__ = 1; i__2 < 0 ? i__ >= i__1 : i__ <= i__1; i__ += i__2) {
	ret_val += sx[i__] * sy[i__];
/* L70: */
    }
    return ret_val;
} /* sdot_ */

/* DECK SSCAL */
/* Subroutine */ int sscal_(integer *n, real *sa, real *sx, integer *incx)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, m, ix, mp1;

/* ***BEGIN PROLOGUE  SSCAL */
/* ***PURPOSE  Multiply a vector by a constant. */
/* ***CATEGORY  D1A6 */
/* ***TYPE      SINGLE PRECISION (SSCAL-S, DSCAL-D, CSCAL-C) */
/* ***KEYWORDS  BLAS, LINEAR ALGEBRA, SCALE, VECTOR */
/* ***AUTHOR  Lawson, C. L., (JPL) */
/*           Hanson, R. J., (SNLA) */
/*           Kincaid, D. R., (U. of Texas) */
/*           Krogh, F. T., (JPL) */
/* ***DESCRIPTION */

/*                B L A S  Subprogram */
/*    Description of Parameters */

/*     --Input-- */
/*        N  number of elements in input vector(s) */
/*       SA  single precision scale factor */
/*       SX  single precision vector with N elements */
/*     INCX  storage spacing between elements of SX */

/*     --Output-- */
/*       SX  single precision result (unchanged if N .LE. 0) */

/*     Replace single precision SX by single precision SA*SX. */
/*     For I = 0 to N-1, replace SX(IX+I*INCX) with  SA * SX(IX+I*INCX), */
/*     where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX. */

/* ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T. */
/*                 Krogh, Basic linear algebra subprograms for Fortran */
/*                 usage, Algorithm No. 539, Transactions on Mathematical */
/*                 Software 5, 3 (September 1979), pp. 308-323. */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791001  DATE WRITTEN */
/*   890831  Modified array declarations.  (WRB) */
/*   890831  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900821  Modified to correct problem with a negative increment. */
/*           (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/* ***END PROLOGUE  SSCAL */
/* ***FIRST EXECUTABLE STATEMENT  SSCAL */
    /* Parameter adjustments */
    --sx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1) {
	goto L20;
    }

/*     Code for increment not equal to 1. */

    ix = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sx[ix] = *sa * sx[ix];
	ix += *incx;
/* L10: */
    }
    return 0;

/*     Code for increment equal to 1. */

/*     Clean-up loop so remaining vector length is a multiple of 5. */

L20:
    m = *n % 5;
    if (m == 0) {
	goto L40;
    }
    i__1 = m;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sx[i__] = *sa * sx[i__];
/* L30: */
    }
    if (*n < 5) {
	return 0;
    }
L40:
    mp1 = m + 1;
    i__1 = *n;
    for (i__ = mp1; i__ <= i__1; i__ += 5) {
	sx[i__] = *sa * sx[i__];
	sx[i__ + 1] = *sa * sx[i__ + 1];
	sx[i__ + 2] = *sa * sx[i__ + 2];
	sx[i__ + 3] = *sa * sx[i__ + 3];
	sx[i__ + 4] = *sa * sx[i__ + 4];
/* L50: */
    }
    return 0;
} /* sscal_ */

/* DECK ISAMAX */
integer isamax_(integer *n, real *sx, integer *incx)
{
    /* System generated locals */
    integer ret_val, i__1;
    real r__1;

    /* Local variables */
    static integer i__, ix;
    static real xmag, smax;

/* ***BEGIN PROLOGUE  ISAMAX */
/* ***PURPOSE  Find the smallest index of that component of a vector */
/*            having the maximum magnitude. */
/* ***CATEGORY  D1A2 */
/* ***TYPE      SINGLE PRECISION (ISAMAX-S, IDAMAX-D, ICAMAX-C) */
/* ***KEYWORDS  BLAS, LINEAR ALGEBRA, MAXIMUM COMPONENT, VECTOR */
/* ***AUTHOR  Lawson, C. L., (JPL) */
/*           Hanson, R. J., (SNLA) */
/*           Kincaid, D. R., (U. of Texas) */
/*           Krogh, F. T., (JPL) */
/* ***DESCRIPTION */

/*                B L A S  Subprogram */
/*    Description of Parameters */

/*     --Input-- */
/*        N  number of elements in input vector(s) */
/*       SX  single precision vector with N elements */
/*     INCX  storage spacing between elements of SX */

/*     --Output-- */
/*   ISAMAX  smallest index (zero if N .LE. 0) */

/*     Find smallest index of maximum magnitude of single precision SX. */
/*     ISAMAX = first I, I = 1 to N, to maximize  ABS(SX(IX+(I-1)*INCX)), */
/*     where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX. */

/* ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T. */
/*                 Krogh, Basic linear algebra subprograms for Fortran */
/*                 usage, Algorithm No. 539, Transactions on Mathematical */
/*                 Software 5, 3 (September 1979), pp. 308-323. */
/* ***ROUTINES CALLED  (NONE) */
/* ***REVISION HISTORY  (YYMMDD) */
/*   791001  DATE WRITTEN */
/*   861211  REVISION DATE from Version 3.2 */
/*   891214  Prologue converted to Version 4.0 format.  (BAB) */
/*   900821  Modified to correct problem with a negative increment. */
/*           (WRB) */
/*   920501  Reformatted the REFERENCES section.  (WRB) */
/*   920618  Slight restructuring of code.  (RWC, WRB) */
/* ***END PROLOGUE  ISAMAX */
/* ***FIRST EXECUTABLE STATEMENT  ISAMAX */
    /* Parameter adjustments */
    --sx;

    /* Function Body */
    ret_val = 0;
    if (*n <= 0) {
	return ret_val;
    }
    ret_val = 1;
    if (*n == 1) {
	return ret_val;
    }

    if (*incx == 1) {
	goto L20;
    }

/*     Code for increment not equal to 1. */

    ix = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    smax = (r__1 = sx[ix], dabs(r__1));
    ix += *incx;
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	xmag = (r__1 = sx[ix], dabs(r__1));
	if (xmag > smax) {
	    ret_val = i__;
	    smax = xmag;
	}
	ix += *incx;
/* L10: */
    }
    return ret_val;

/*     Code for increments equal to 1. */

L20:
    smax = dabs(sx[1]);
    i__1 = *n;
    for (i__ = 2; i__ <= i__1; ++i__) {
	xmag = (r__1 = sx[i__], dabs(r__1));
	if (xmag > smax) {
	    ret_val = i__;
	    smax = xmag;
	}
/* L30: */
    }
    return ret_val;
} /* isamax_ */

/* Main program */ int MAIN__(void)
{
    return 0;
} /* MAIN__ */

