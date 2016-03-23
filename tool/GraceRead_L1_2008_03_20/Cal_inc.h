#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

/* Version Information: %Z% %M%       %I% %G% */

typedef double Real;

typedef struct {
  int n;
  double *x;
} Vec;

typedef struct {
  int nr, nc;
  double **x;
} Mat;

typedef struct {
  char *name;
  FILE *fp;
} File_t;

typedef struct {
  int n;
  double ep;
  double dt;
  double *p[3];
  double *v[3];
  char *name;
} Eci_t;

#define MAXLINE 2048
#define MAXDEG 16
#define MAXN 2000
#define MAXDIM 2000
#define NFIT 9

#define elsif else if
#define streq(A,B) (!strcmp(A,B))

#define loop(I,N) for(I=0;I<N;I++)
#define loop3(I) loop(I,3)
#define loop33(I,J) loop3(I) loop3(J)

#define rnd(A) floor(A + 0.5)
#define ACC (1.0e-12)


/* global variables */

#ifdef MAIN
#define EXT
#define EXTINIT(A,B) A = B
#else
#define EXT extern
#define EXTINIT(A,B) extern A
#endif

EXT Vec OldSoln, OldSig, Soln2, Sig2;

EXT Real PI, D2R, R2D, a_K, a_Ka;
EXT Real xhat[3], yhat[3], zhat[3];
EXT Vec doff;
EXT Real xdum[16];
EXT int idum[16];
EXT char cdum[16];

EXT char *pname;
EXT char *GRACE_id;
EXT File_t File_log, File_acc, File_mag, File_sca, File_kbr, File_eci, File_eci2,
  File_cg, File_old, File_new, File_soln, File_param, File_oldsoln, File_soln2;
EXTINIT ( FILE *fp_log, stdout );

EXT Eci_t eci, eci2;

EXTINIT ( char *xyz, "xyz" );

EXTINIT ( Real solx, 0 );  /* meters */
EXTINIT ( Real soly, 0 );  /* meters */
EXTINIT ( Real solz, 0 );  /* meters */

EXTINIT ( Real sig_ap, 1e3 );  /* meters */
EXTINIT ( Real sig_tight, 1e-9 );  /* meters */
EXT Real t1, t2, period;
EXT int dummy_set;
EXTINIT ( int t1_set, 0 );
EXTINIT ( int t2_set, 0 );
EXTINIT ( int period_set, 0 );
EXTINIT ( int do_real, 0 );
EXTINIT ( int do_sim, 0 );
EXTINIT ( int do_hola, 0 );
EXTINIT ( int GRACE_id_set, 0 );
EXTINIT ( int do_write, 0 );

EXT Real Mass, Lhorn;
EXT Mat MoI, iMoI;
EXT Mat Normal_AA, Cov;
EXT Vec Normal_Ay, Soln, Sig, sigA, wtA;
EXT Mat Amat, iAmat;
EXT Vec Yvec, Bvec;
EXT char Line[1024];

EXTINIT ( Real Xfac, 1.0 );
EXTINIT ( Real Yfac, 1.0 );
EXTINIT ( Real Zfac, 1.0 );

EXTINIT ( int acc_n, 0 );
EXTINIT ( int mag_n, 0 );
EXTINIT ( int sca_n, 0 );
EXTINIT ( int kbr_n, 0 );
EXTINIT ( int pos_n, 0 );
EXT Real acc_t[MAXN], mag_t[MAXN], sca_t[MAXN], kbr_t[MAXN], pos_t[MAXN], kbr_X[MAXN];
EXT Mat acc_A, mag_al, sca_Q, pos_R;

/* prototypes */

/* from Cal_CG.c */
int read_normal_eqns();
int write_normal_eqns();

/* from cmdline.c */
int Get_Real_Arg(Real *p, int *flg);
int Get_String_Arg(char **p, int *flg);
int File_Open_Arg(File_t *f, const char *mode);
int No_Arg();
int read_cmdline(int argc, char **argv);
int do_usage();

/* from rx.c */
int bomb(char *error_text);
int mat_inv(Mat a, Mat ai);
int mat_trans(Mat a);
int my_ludcmp(Real **a, int n, int *indx, Real *d);
int my_lubksb(Real **a, int n, int *indx, Real *b);
Real pythag(Real a, Real b);
int tred2(Real **a, int n, Real *d, Real *e);
int tqli(Real *d, Real *e, int n, Real **z);
Real fsign(Real a, Real b);

/* from utils.c */
Vec Vec_alloc(int n);
Mat Mat_alloc(int nrows, int ncols);
int fit_pd(int n, Real tep, Real *ts, Real *xs, Real *sigs, Real pd, int *flags, Real *as, Real *amp, Real *res);
int set_v(Vec vn, Vec v);
int m_v(Vec mv, Mat m, Vec v);
int v_m(Vec vm, Vec v, Mat m);
int m_m(Mat mab, Mat ma, Mat mb);
int rot_x(Mat mat, Real th);
int rot_y(Mat mat, Real th);
int rot_z(Mat mat, Real th);
int rot_gen(Mat mat, Real th, Vec ax, Vec u, Vec v);
Real atan3(Real x, Real y);
Real theta(Real x, Real y);
Real Vnorm(Vec vhat, Vec v);
Real Vdot(Vec a, Vec b);
int cross(Vec axb, Vec a, Vec b);
int diff(Vec ab, Vec a, Vec b);
Real costh(Vec a, Vec b);
int ahola(int n);
int neg(Vec a);
int getq(Vec q, Vec x, Vec y, Vec z, Vec u, Vec v, Vec w);
Real getrand();

