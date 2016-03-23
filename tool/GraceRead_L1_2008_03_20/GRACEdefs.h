/* @(#) GRACEdefs.h      1.15 03/08/02 */

#ifndef _GRACEdefs_h_
#define _GRACEdefs_h_

#define spdLit (299792458.0)
#define PI (3.14159265358979323846264)
#define D2R (PI/180.0)
#define R2D (180.0/PI)
/* define Freq_K_A (24.527232e+9*(1+8.602818910174442e-08))	 to be modified */
/* define Freq_K_B (24.527734524e+9*(1+8.602818910174442e-08))	 to be modified */
/* define Freq_Ka_A (32.702976e+9*(1+4.512255438949924e-08))	 to be modified */
/* define Freq_Ka_B (32.703646032e+9*(1+4.512255438949924e-08))	 to be modified */
/*#define Freq_K_A (24.527232e+9)	 to be read from file */
/*#define Freq_K_B (24.527734524e+9)	 to be read from file */
/*#define Freq_Ka_A (32.702976e+9)	 to be read from file */
/*#define Freq_Ka_B (32.703646032e+9)	 to be read from file */
/*#define Freq_K_A_B  (-0.502524e+6)	 to be computed from Freq_K_A & Freq_K_B */
/*#define Freq_Ka_A_B (-0.670032e+6)	 to be computed from Freq_Ka_A & Freq_Ka_B */

#define NAUXMAX     9  /* number of elements in auxilary array                */
#define EDITFLAG   -1  /* value indicating that an observation has been edited*/

#define IPSMIN      0  /* minimum of obs after sigma editing                  */
#define IPSMAX      1  /* maximum of obs after sigma editing                  */
#define IPSNOBS     2  /* number of obs after sigma editing                   */
#define IPSNITER    3  /* number of iterations to converge sigma editing      */
#define IPSORGMEAN  4  /* mean of all obs before sigma editing                */
#define IPSORGSTDV  5  /* standard deviation of all obs before sigma editing  */
#define IPSORGMIN   6  /* minimum of all obs before sigma editing             */
#define IPSORGMAX   7  /* maximum of all obs before sigma editing             */
#define IPSORGNOBS  8  /* number of good obs in orignal data                  */


#define _INCLUDE_POSIX_SOURCE
#include <sys/types.h>
#undef _INCLUDE_POSIX_SOURCE

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#ifndef  PI
#define PI          (4.0*atan(1.0))
#endif

#define loop(A,B) for(A=0;A<B;A++)

#ifndef  FALSE
#define  FALSE False
#endif

#ifndef  false
#define  false False
#endif

#ifndef  TRUE
#define  TRUE  True
#endif

#ifndef  true
#define  true  True
#endif

#ifndef  bool
#define  bool boolean
#endif

typedef double real;

typedef char byte;

typedef enum boolean
	{
	False,
	True
	} boolean;

typedef struct vector
        {
        real* value;
        int   size;          /* number of elements of v */
        }  vector;

typedef struct vint{
  int*   value;
  int    size;
} vint;

typedef struct matrix
        {
        int row_dim;            /* number of elements in a column */
        int col_dim;            /* number of elements in a row */
        real** value;
        }  matrix;              /* row_dim X col_dim matrix */

typedef struct table{
  int size;                     /* number of entries in table */

  real *x;                      /* independent table variable  */
  real *y;                      /* dependnet table variable*/
} table;

typedef struct  time_EpDel
        {
        real	epoch;		/* seconds past J2000 */
        real	delta;		/* seconds past epoch */
        } time_EpDel;

typedef struct quaternion
        {
        real	q0;
        real	q1;
        real	q2;
        real	q3;
        } quaternion;

#endif	/* _GRACEdefs_h_ */

