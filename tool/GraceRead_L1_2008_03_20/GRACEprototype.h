/* @(#) GRACEprototype.h      1.30 05/14/03 */

vector vector_create	( int size );
void vector_destroy	( vector );
vint vint_create	( int size );
void vint_destroy	( vint );
matrix matrix_create	( int row_dim, int col_dim );
void matrix_destroy	( matrix );
int vdiff( vector v1, vector v2, vector v3 );
real v_mag ( vector a );
real dot( vector x, vector y );
void cross ( vector v1, vector v2, vector v3 );
void normalize ( vector a );
void quat_diff( quaternion Q, quaternion P, real *RA, real *Dec, real *Twist );
void quat_diff_Q( quaternion Q, quaternion P, quaternion *D );
void quat2angl( quaternion Q, real *RA, real *Dec, real *Twist );
void quat_rot( quaternion Q, vector u, vector v );
void quat_prod( quaternion Q, quaternion P, quaternion *QP );
void quat2rot( quaternion qp, matrix m );
int M2_invrs( real A[2][2], real B[2][2] );
int M3_invrs( real A[3][3], real B[3][3] );
int M4_invrs( real A[4][4], real B[4][4] );
FILE *file_open( const char *envvar, const char *mode );
long linlsq(double *x, double *y,double *a, double *b, double *a_sigma,
            double *b_sigma, double *rms, double *rms0, long n);
long linlsq_weight(double *x, double *y, double *w, double *a, double *b,
                   double *a_sigma, double *b_sigma, double *rms, double *rms0,
                   long n);
long linlsq_weight_sigma_edit(
                   double *x, double *y, double *w, double *a, double *b, 
                   double *a_sigma, double *b_sigma, double *rms, double *rms0,
                   long n, double SigmaFactor,long *niterations,long *nedit);

void KBR_combine
               (
/* input */
                real    K_phase_A,      /* K-band phase from S/C A (cycles) */
                real    Ka_phase_A,     /* Ka-band phase from S/C A (cycles) */
                real    K_phase_B,      /* K-band phase from S/C B (cycles) */
                real    Ka_phase_B,     /* Ka-band phase from S/C B (cycles) */
                int     Wrap_K_A,       /* K-band S/C A+B wraps (1e8 cycles) */
                int     Wrap_Ka_A,      /* Ka-band S/C A+B wraps (1e8 cycles) */
                real    Freq_K_A,       /* K-band frequency from S/C A (Hz) */
                real    Freq_K_B,       /* K-band frequency from S/C B (Hz) */
                real    Freq_Ka_A,      /* Ka-band frequency from S/C A (Hz) */
                real    Freq_Ka_B,      /* Ka-band frequency from S/C B (Hz) */
/* output */
                real    *Range,         /* dual-one-way range (m) */
                real    *IonCorr_Ka     /* Ka range ionosphere correction (m) */
               );

boolean CRN_fltr
	       (
/* input */
		real	*raw_time,	/* raw data timetag array (sec) */
		real	*raw_data,	/* raw data array */
		int	fs,		/* raw data rate (samples/sec) */
		real	B,		/* target bandwidth (Hz) */
		int	NC,		/* number of self convolutions */
		int	Nf,		/* number of data points used in filter */
		int	level,		/* 0 -> output only same quantity as input */
					/* 1 -> also rate of input */
					/* 2 -> also acceleration of input */
/* output */
		real	*cmprsd_time,	/* compressed data timetag (sec) */
		real	cmprsd_data[]	/* compressed data */
	       );
/* return:	1	normal return
                0	short of raw_data points 
*/

void Normal_Pt
              (
/* input */
                real    *raw_time,      /* raw data timetag array */
                real    *raw_data,      /* raw data array */
                int     fs,             /* raw data rate (samples/sec) */
                int     Lc,             /* fit with every Lc raw_data points */
                int     Tc,             /* data span for each quadratic fit (sec) */
                int     Level,          /* 0 -> output only same quantity as input */
                                        /* 1 -> also rate of input */
                                        /* 2 -> also acceleration of input */
/* output */
                real    *cmprsd_time,   /* compressed data timetag */
                real    cmprsd_data[]   /* compressed data */
               );

int fill_gap
	       (
/* input */
		int	Epoch,		/* data Epoch (sec) to which timetag is referenced */
                char    SC,             /*  spacecraft name (A or B) */
		int	NF,		/*  index of first missing point */
		int	NL,		/*  index of last data point */
                matrix  in_data,        /* input data arrays, first column is timetag */
                real    dT,             /* interpolated data interval (sec) */
                int     BigGap          /* widest gap fillable */
               );
/* return:      number of filled (new) data points (*in_data are expanded) */
/*              or -1 if fail in filling */

void Resample
               (
/* input */
                real    dT,             /* data interval (sec) */
                matrix  in_data,        /* input data arrays, first column is timetag */
                int     N,              /* first time index of in_data to be resampled */
                int     N_Max,          /* Maximum valid N for in_data */
                real    Corr[],         /* timetag correction (sec) */
                boolean Flag,           /* =1 if input data timetags are integer multiples of dT */
                                        /* (precise timetag correction of data used) */
/* output */
                matrix  out_data        /* resampled data arrays, first column is timetag */
               );

void LightTimeCorr
               (
/* input */
                vector  Pos_A,          /* position vector of S/C A */
                vector  Vel_A,          /* velocity vector of S/C A */
                vector  Acc_A,          /* acceleration vector of S/C A */
                vector  Pos_B,          /* position vector of S/C B */
                vector  Vel_B,          /* velocity vector of S/C B */
                vector  Acc_B,          /* acceleration vector of S/C B */
                real    Freq_K_A,       /* K  frequency of S/C A  */
                real    Freq_Ka_A,      /* Ka frequency of S/C A  */
                real    Freq_K_B,       /* K  frequency of S/C B  */
                real    Freq_Ka_B,      /* Ka frequency of S/C B  */
/* output */
                real    *RangeCorr      /* range correction (m) */
               );

int QuadrFit
               (
/* input */
		int	Npt,		/* number of data points */
		real	X[],		/* array of independent variable */
		real	Y[],		/* array of dependent variable */
/* output */
		real	P[],		/* Quadratic fit parameters */
		real	*Sig		/* RMS post-fit residual */
	       );
int CubicFit
               (
/* input */
		int	Npt,		/* number of data points */
		real	X[],		/* array of independent variable */
		real	Y[],		/* array of dependent variable */
/* output */
		real	P[],		/* Cubic fit parameters */
		real	*Sig		/* RMS post-fit residual */
	       );

void SigmaEdit (double *obs, long nobs, double SigmaFactor, double *mean,
                double *rms   , long *edit, double auxilary[NAUXMAX],
                double NoValue);
