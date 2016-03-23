/*  $Id: atti_flt_utils_prototypes.h,v 1.51 2004/08/31 15:43:37 wib Exp $ */

#define MaxCol	36		/* to check filter with no stochastics
                                   need invert 18x18 and we append the
                                   identity to an 18x18*/
#define MaxRow	36		/* 2Np+Nx+Nmeas = 24+6+6 = 36 */
#define MaxParam	18
#define MaxProcNoise	12
#define Eps	1e-37
#define NO_STOCHMAP -1
#ifndef _atti_flt_utils_prototypes_h_
#define _atti_flt_utils_prototypes_h_
#include "atti.h"

int thhc(			/* HH triangularization of matrix */

/* Input: */
	int	Ncol,		/* total number of columns of A matrix */
	int	C0,		/* first column number to be transformed */
	int	CL,		/* last column number to be transformed */
	int	R0,		/* first row number to be transformed */
        int	RL,		/* last row number to be transformed */
/* Input & Output */
        double	A[][MaxCol]	/* matrix to be transformed */

/* Return 0 if normal operation; 1 otherwise */
	  );

int thhc_col(			/* HH annihilation of single matrix column */

/* Input: */
	int	Ncol,		/* total number of columns of A matrix */
	int	C0,		/* first column number to be transformed */
	int	R0,		/* first row number to be transformed */
        int	RL,		/* last row number to be transformed */
/* Input & Output */
        double	A[][MaxCol]	/* matrix to be transformed */

/* Return 0 if normal operation; 1 otherwise */
	  );

int R_inv(			/* Inversion of an upper triangularized matrix */

/* Input: */
        double	A[][MaxCol],	/* upper triangular matrix to be inverted */
	int	NA,		/* matrix dimension */
	int	N1,		/* index number of starting row of A[][]
				   to be inverted (N1=0 if all) */
/* Output */
        double	B[][MaxCol]	/* A inversed
				   (upper N1 rows & columns undefined if N1 > 0) */

/* Return 0 if normal operation;
	 -1 if NA >= MaxCol;
	 -2 if N1 >= NA;
	  j if failed because R[j][j] is too small */
	  );

int solve_RX_Z(

/* Input: */
	int	mode,		/* > 0 if column N to N+M-1 contain Z */
				/* = 0 if Z given separately */
        double	R[][MaxCol],	/* upper triangular matrix */
	int	NR,		/* matrix row dimension */
	int	N1,		/* first index number corresponding to which
				   solutions are requested (N1=0 if all) */
	int	M,		/* number of columns in Z */
        double	Z[][MaxCol],	/* N x M matrix */
/* Output */
        double	X[][MaxCol]	/* solution N x M matrix */

/* Return 1 if normal operation;
	  2 if NA >= MaxCol;
	 -j if failed because R[j][j] is too small */
        );

int Ainv_X(			/* compute (A-inverse)*X by THH and back substitution */

/* Input: */
        double	A[][MaxCol],	/* square matrix */
	int	N,		/* matrix dimension */
	int	M,		/* number of columns in X */
        double	X[][MaxCol],	/* N x M matrix */
/* Output */
        double	Y[][MaxCol]	/* solution N x M matrix */

/* Return 0 if normal operation;
	 -1 if N+M > 2*MaxCol;
	  1 if operation fails in calling thhc
	  2 if operation fails in calling solve_RX_Z		*/
	  );

int A_Binv(			/* compute A * (B-inversed) by THH and back substitution */

/* Input: */
        double	A[][MaxCol],	/* M x N matrix */
	int	M,		/* number of rows in A */
	int	N,		/* matrix dimension of B (and colums in A) */
        double	B[][MaxCol],	/* square matrix */
/* Output */
        double	Y[][MaxParam]	/* solution M x N matrix */

/* Return 0 if normal operation;
	 -1 if N+M > MaxCol;
	  1 if operation fails in calling thhc
	  2 if operation fails in calling solve_RX_Z		*/
	  );

int Cov2RI(			/* Choleski factorization of a summetric square matrix P */
				/*    P -> U U* where U* = transpose of U */
/* Input: */
        double	P[][MaxCol],	/* symmetrical square matrix to be factorized */
				/* (destroyed upon return) */
	int	N,		/* matrix dimension */
/* Output */
        double	U[][MaxCol]	/* factorized upper triangular matrix */

/* Return 0 if normal operation;
	 -1 if N >= MaxCol;
	  j if failed because P[j][j] is < 0 */
	  );

int R2Cov(			/*Construct covariance matrix from
				  triangularized information matrix */
/* Input: */
        double	R[][MaxCol],	/* upper triangularied information matrix */
	int	N,		/* matrix dimension */
	int	N1,		/* index number of starting row of R[][] for which
				   covariance are requested (N1=0 if all) */
/* Output */
        double	P[][MaxCol]	/* covariance matrix 
				   (first N1 row & columns undefined if N1 > 0 ) */

/* Return 0 if normal operation;
	 -1 if N >= MaxCol;
	 -2 if N1 >= N		*/
	  );

int R2Sig(			/* Extract standard deviations from */
				/* triangularized information matrix */
/* Input: */
	int	mode,		/* = 1 if Proj_Quat sigma is mapped to R-P-Y in SRF;
				   = 0 otherwise  */
        double	R[][MaxCol],	/* upper triangularied information matrix */
	int	N,		/* matrix dimension */
	int	N1,		/* index number of starting row of R[][] for which
				   sigmas are requested (N1=0 if all) */
	int	Npq,		/* index number of R[][] corresponding to the first
				   Proj_Quat parameter (needed only if mode = 1) */
	double	Qis[4],		/* model Qis quaternion (needed only if mode = 1) */
	double	dQis_dP[4][3],	/* partials of Qis wrt. Proj_Quat (needed only if mode = 1) */
/* Output */
        double	Sig[]		/* standard deviations
				   (first N1 undefined if N1 > 0 ) */

/* Return 0 if normal operation;
	 -1 if N >= MaxCol;		*/
	  );

int A_inv(			/* Inverse of a square matrix by THH and back substitution */

/* Input: */
        double	A[][MaxCol],	/* square matrix */
	int	N,		/* matrix dimension */
/* Output */
        double	B[][MaxCol]	/* inverse of A */

/* Return 0 if normal operation;
	 -1 if N >= MaxCol;
	  1 if operation fails in calling thhc
	  2 if operation fails in calling solve_RX_Z		*/
	  );

int TimeUpdateSRIF(		/* Time Updating SRIF matrix of 2Np+Nx+1 columns */

/* Input: */
        double	R[MaxRow][MaxCol],	/* measurement updated (trianglurarized)
					   informtation matrix (2Np+Nx+1 columns) at time t ;
					   time-updated to t+1 upon return     */
	int	Np,		/* number of process-noise parameters */
	int	Nx,		/* number of non-process-noise parameters */
	double	Vxp[MaxParam][MaxProcNoise],	/* variational partials of X(t+1) wrt. P(t) */
	double	Vxi[MaxParam][MaxParam],		/* variational partial of X(t) wrt. X(t+1) */
	double	M[],		/* m = exp( -dT / tau ); tau= correlation time */
	double	Q[]		/* q = sigP * sqrt( 1 - m**2 ); sigP = steady-state sigma */
/* Output
		R[][] gets time-updated */

/* Return 0 if normal operation;
         -1 if Nx > MaxParam
         -2 if Np > MaxProcNoise
         -3 if 2Np+Nx+1 > MaxCol
         -4 if 2Np+Nx > MaxRow		*/
	  );

int MeasUpdateSRIF(		/* Measurement updating SRIF matrix of 2Np+Nx+1 columns */

/* Input: */
        double	R[][MaxCol],	/* time updated informtation matrix (2Np+Nx+1 columns)
					   (overwritten upon return!!) */
	int	Np,		/* number of process-noise parameters */
	int	Nx,		/* number of non-process-noise parameters */
	double	A[][MaxCol],	/* measurement A-matrix (Na rows by 2Np+Nx+1 columns) */
	int	NA		/* number of rows of A[][] */

/* Output:	measurement updated R matrix (triangularized) ovewriting R[][] */

/* Return 0 if normal operation;
	 -1 if 2Np+Nx+1 > MaxCol;
	 -2 if Np+Nx+NA > MaxRow;	*/
	  );

int AprioriSRIF(

/* Input: */
	int	Index[],	/* parameter index mapping from filter/smoother order
				   to standard input order (flt_info->Param_order_inv) */
	int	Np,		/* number of process-noise parameters */
	int	Nx,		/* number of non-process-noise parameters */
	double	Z[],		/* a priori estimates of parameters */
	double	Sig[],		/* a priori sigmas of parameters */
/* Output */
        double	R[][MaxCol]	/* a priori R matrix */

/* Return 0 if normal operation;
	 -1 if 2Np+Nx+1 > MaxCol;	*/
        );

int SaveSmoothCoef(

/* Input: */
        double  R[][MaxCol],    /* time updated information matrix (2Np+Nx+1 columns) */
        int     Np,             /* number of process-noise parameters */
        int     Nx,             /* number of non-process-noise parameters */
        double  Time,           /* time tag for smooth coefficients in R matrix */
        double  DeltaT,         /* time tag spacing in seconds */
        int     CloseFileFlag   /* if == 1 update header information and close smooth file */

/* Return 0 if normal operation;
         -1 if error writing to SMOOTH_FILE
         -2 if 2Np+Nx+1 > MaxCol; 
         -3 2Np+Nx+1 != previous 2Np+Nx+1 
         -4 DeltaT is not equal to first call DeltaT
         -5 Current time is not spaced at DeltaT seconds from previous time */
          );

int ReadSmoothCoef(

/* Input: */
        int     Np,             /* number of process-noise parameters */
        int     Nx,             /* number of non-process-noise parameters */
        double  Time,           /* time tag for smooth coefficients in R matrix */
/* Output: */
        double  R[][MaxCol],     /* time updated information matrix (2Np+Nx+1 columns) */
/*Input: */
        int CloseFileFlag       /* 0 read , 1 close file */
        );

/* Return 0 if normal operation
         -1 error reading header
         -2 if Np != Header_Np
         -3 if Nx != Header_Nx
         -4 if 2*Np+Nx+1 > MaxCol
         -5 requested time is out of time range
         -6 error in fseek of file SMOOTH
         -7 requested input time is not equal to file time tag
*/

int SmoothSRIF(			/* Smoothing SRIF matrix of 2Np+Nx+1 columns
				   by backward time update */
/* Input: */
        double	SM[][MaxCol],	/* smooth coefficients at time t, of size
				   Np x (2*Np + Nx + 1), PADsrif Eq. 14 */
	int	Np,		/* number of process-noise parameters */
	int	Nx,		/* number of non-process-noise parameters */
	double	Vxp[MaxParam][MaxProcNoise],	/* variational partials of X(t+1) wrt. P(t) */
	double	Vx[MaxParam][MaxParam],		/* variational partial of X(t+1) wrt. X(t) */
/* Input/Output: */
        double	R[MaxRow][MaxCol]	/* smoothed informtation matrix at time t+1
					   (2Np+Nx rows by 2Np+Nx+1 columns); upon return,
					   replaced by that mapped and smoothed at time t */

/* Return 0 if normal operation;
         -1 if Nx > MaxParam
         -2 if Np > MaxProcNoise
         -3 if 2Np+Nx+1 > MaxCol
         -4 if 2Np+Nx > MaxRow          */
	  );

int SaveMapping(

/* Input: */
        double  Vx[MaxParam][MaxParam],     /* variational partial of X(t+1) wrt. X(t) */
        double  Vxp[MaxParam][MaxProcNoise],  /* variational partials of X(t+1) wrt. P(t) */
        double  VxInv[MaxParam][MaxParam],  /* variational partial of X(t+1) wrt. X(t) */
        double  Time,           /* time tag for smooth coefficients in R matrix */
        int     Np,             /* number of process-noise parameters */
        int     Nx,             /* number of non-process-noise parameters */
        double  DeltaT,         /* time tag spacing in seconds */
        double  dQis_dP[4][3],  /* partial matrix of Qis wrt. Projection components */
        double  Qis[4],         /* model Qis quaternion */
        int     CloseFileFlag   /* if == 1 update header information and close smooth file */
        );

/* Return 0 if normal operation
         -1 if error writing to MAPPING_FILE
         -2 if Np != Header_Np
         -3 Np > MaxCol
         -4 Nx != Header_Np
         -5 Nx > MaxCol
         -6 DeltaT is not equal to first call DeltaT
         -7 Current time is not spaced at DeltaT seconds from previous time 
*/


int ReadMapping(

/* Input: */
        int     Np,             /* number of process-noise parameters */
        int     Nx,             /* number of non-process-noise parameters */
        double  Time,           /* time tag for smooth coefficients in R matrix */
                                /* Time can only be specified in multiples of dT */
                                /* where Time = T0 + k*dT T0 <= Time <= Tf */
                                /* (T0,Tf and dT are stored in header of smoothfile */
/* Output: */
        double  Vx[MaxParam][MaxParam],   /* variational partial of X(t+1) wrt. X(t) PADsrif Eqs. 7 */
        double  Vxp[MaxParam][MaxProcNoise],  /* variational partials of X(t+1) wrt. P(t) PADsrif Eqs. 7*/
        double  VxInv[MaxParam][MaxParam],/* inverse variational partial of X(t+1) wrt. X(t) PADsrif Eqs. 8*/
        double  dQis_dP[4][3],  /* partial matrix of Qis wrt. Projection components */
        double  Qis[4],          /* model Qis quaternion */
/*Input: */
        int CloseFileFlag       /* 0 read , 1 close file */
        );
  
/* Return 0 if normal operation
         -1 error reading header
         -2 if Np != Header_Np 
         -3 if Nx != Header_Nx
         -4 if Nx > MaxCol || Np > MaxCol
         -5 requested time is out of time range
         -6 error in fseek of file MAPPING
         -7 requested input time is not equal to file time tag
*/

int deterministic_maps(		/* variational partials for deterministic mapping
				   according to PADsrif Eqs. 7 & 8 */
/* Input: */
	int	Index[],	/* parameter index mapping from filter/smoother order
				   to standard input order (flt_info->Param_order_inv) */
	int	Np,		/* number of process-noise parameters */
	int	Nx,		/* number of non-process-noise parameters */
        double	Vt1[NSTATE][NDYN],	/* variational partials p[][] at t */
        double	Vt2[NSTATE][NDYN],	/* variational partials p[][] at t+1 */
        
/* Output */
	double	Vx[MaxParam][MaxParam],	/* variational partial of X(t+1) wrt. X(t) */
	double	Vxi[MaxParam][MaxParam],	/* variational partial of X(t) wrt. X(t+1) */
	double	Vxp[MaxParam][MaxProcNoise]	/* variational partials of X(t+1) wrt. P(t) */

/* Return 0 if normal operation;
	 -1 if Nx > MaxCol;
	 -2 if Np > MaxCol;
	 -3 if Nx > MaxRow;
	 -4 if Index[] < 0 for state parameters X;
	 -4 if Index[] > 5 for state parameters X;
	 -5 if Index[] < 6 for process-noise parameters P;
	 -6 if Index[] > 17 for process-noise parameters P;
	  1 if Vx1 not invertible;
	  2 if Vx2 not invertible	*/
	  );

long ReadSolHeader(
/* Input: */
        double  *start_time,     /* first time tag for solution/sigmas in SOL */
        double  *final_time,     /* final time tag for solution/sigmas in SOL */
        double  *header_DeltaT,  /* time tag spacing in seconds */
        int     *Np,             /* number of process-noise parameters */
        int     *Nx,             /* number of non-process-noise parameters */
        int     *Param_order_inv,/* mapping from input parameter order to standard parameter order */
        FILE    *SOL             /* pointer to SOL file */
        );

/* Return 0 if normal read;
         -1 if error reading from SOL
         -2 first element of header not in SOLFILE -> empty file  */

long WriteSolHeader(
/* Input: */
        double  *start_time,     /* first time tag for solution/sigmas in SOL */
        double  *final_time,     /* final time tag for solution/sigmas in SOL */
        double  *header_DeltaT,  /* time tag spacing in seconds */
        int     *Np,             /* number of process-noise parameters */
        int     *Nx,             /* number of non-process-noise parameters */
        int     *Param_order_inv,/* mapping from input parameter order to standard parameter order */
        FILE    *SOL          /* pointer to SOL file */
        );

int ReadSol(

/* Input: */
        double  Time,           /* time tag for solution and sigmas */
/* Ouput: */
        double  *DeltaT,         /* time tag spacing in seconds */
        int     *Np,             /* number of process-noise parameters */
        int     *Nx,             /* number of non-process-noise parameters */
        double  Sig[MaxParam],   /* sigmas of all parameters in standard order (MaxParam elements)*/
        double  Sol[MaxParam],   /* solutions of all parameters in standard order(MaxParam elements)*/
        int     *Param_order_inv,/* mapping from input parameter order to standard parameter order */
        double  *tstart,         /* start time of file */
        double  *tend,           /* end time of file */
/* Input: */
        int     CloseFileFlag    /* if == 1 close SOL file */
        );

/* exit 0 if normal operation
         -1 if error writing to SOL
         -2 if Np != Header_Np
         -3 Nx != Header_Nx
         -4 Np < 0 || Np > MaxCol Nx < 0 || Nx > MaxCol
         -5 DeltaT is not equal to first call DeltaT
         -6 Current time is not spaced at DeltaT seconds from previous time
         -7 Param_order_inv != Param_order_inv read from header file
         -8 Requested time is out of range of existing file
         -9 Error in fseek within existing file
*/

int SaveSol(

/* Input: */
        double  Time,           /* time tag for solution and sigmas */
        double  DeltaT,         /* time tag spacing in seconds */
        int     Np,             /* number of process-noise parameters */
        int     Nx,             /* number of non-process-noise parameters */
        double  Sig[MaxParam], /* Np + Nx sigma in filter order */
        double  Sol[MaxParam],   /* Np + Nx  solutions in filter order */
        int     *Param_order_inv,/* mapping from input parameter order to standard parameter order */
        atti_mem_info *ts_mem_info, /* pointer to storage information for pq and omega timeseries */
        int     CloseFileFlag   /* if == 1 update header information and close SOL file */
        );

/* exit 0 if normal operation
         -1 if error writing to SOL
         -2 if Np != Header_Np
         -3 Nx != Header_Nx
         -4 Np < 0 || Np > MaxCol Nx < 0 || Nx > MaxCol
         -5 DeltaT is not equal to first call DeltaT
         -6 Current time is not spaced at DeltaT seconds from previous time 
         -7 Param_order_inv != Param_order_inv read from header file
         -8 Requested time is out of range of existing file
         -9 Error in fseek within existing file
*/

int InterpSol(

/* Input */
              double time,
/* Output */            
              double Sol[MaxParam] /* Note that the state is not correct in 
                                      (Tend - DeltaT, Tend] , bias and scale??   */
              );


#endif
