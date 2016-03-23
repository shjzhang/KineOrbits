/* @(#) QuadrFit.c      1.5 12/19/02

   Purpose:
        Qudratic Fit of Y[] against X[] arrays ( X may be non-evenly spaced )

   06/18/2001   Sien Wu         Created
*/
#include "GRACEdefs.h"
#include "GRACEprototype.h"

int QuadrFit
	(
/* input */
	 int	Npt,		/* number of data points */
	 real	X[],		/* array of independent variable */
	 real	Y[],		/* array of dependent variable */
/* output */
	 real	C[],		/* Quadratic fit parameters */
	 real	*Sig		/* RMS post-fit residual */
	)
{
 static char SccsId[] = "@(#) QuadrFit.c      1.5 12/19/02";

 real	A[3][3];	/* A-xpose * A matrix */
 real	B[3][3];	/* inverse of A[][] */
 real	p;		/* power of X[] */
 real	px[5];		/* sum of x**k, k = 0, 1, ..., 4 */
 real	py[3];		/* A-xpose * Y matrix = sum of y*x**k, k = 0, 1, 2 */
 int	i, j, n;

 if( Npt < 3 )
  {
   fprintf(stderr,"QuadrFit fails: Npt = %d < 3 !!\n",Npt);
   exit( -1 );
  }

/* ATA matrix */

 loop( i, 5 ) px[i] = 0;
 loop( i, 3 ) py[i] = 0;

 px[0] = Npt;
 loop( n, Npt )
  {
   py[0] += Y[n];
   for( p=1, j=1; j<5; j++ )
    {
     p *= X[n];
     px[j] += p;
     if( j <= 2 ) py[j] += p * Y[n];
    }
  }
 loop( i, 3 ) loop( j, 3 ) A[i][j] = px[i+j];

/* ATA inversed */

 if( M3_invrs( A, B ) ) return 1;

/* coefficient solutions */

 loop( i, 3 )
  {
   C[i] = 0;
   loop ( j, 3 ) C[i] += B[i][j] * py[j];
  }

/* RMS post-fit residual */

 *Sig = 0;

 if( Npt > 3)
  {
   loop( n, Npt )
     *Sig += pow( C[0] + ( C[1] + C[2] * X[n] ) * X[n] - Y[n] , 2 );
   *Sig = sqrt( *Sig / (Npt-3) );
  }

 return 0;
}
