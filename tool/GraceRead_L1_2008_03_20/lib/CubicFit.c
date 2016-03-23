/* @(#) CubicFit.c      1.3 12/19/02

   Purpose:
        Cubic Fit of Y[] against X[] arrays ( X may be unevenly spaced )

   06/18/2001   Sien Wu         Created
*/
#include <string.h>
#include "GRACEdefs.h"
#include "GRACEprototype.h"

int CubicFit
	(
/* input */
	 int	Npt,		/* number of data points */
	 real	X[],		/* array of independent variable */
	 real	Y[],		/* array of dependent variable */
/* output */
	 real	C[],		/* Cubic fit parameters */
	 real	*Sig		/* RMS post-fit residual */
	)
{
 static char SccsId[] = "@(#) CubicFit.c      1.3 12/19/02";

 real	A[4][4];	/* A-xpose * A matrix */
 real	B[4][4];	/* inverse of A[][] */
 real	p;		/* power of X[] */
 real	px[7];		/* sum of x**k, k = 0, 1, ..., 6 */
 real	py[4];		/* A-xpose * Y matrix = sum of y*x**k, k = 0, 1, ..., 3 */
 int	i, j, n;

 if( Npt < 4 )
  {
   fprintf(stderr,"CubicFit fails: Npt = %d < 4 !!\n",Npt);
   exit( -1 );
  }

/* ATA matrix */

 loop( i, 7 ) px[i] = 0;
 loop( i, 4 ) py[i] = 0;

 px[0] = Npt;
 loop( n, Npt )
  {
   py[0] += Y[n];
   for( p=1, j=1; j<7; j++ )
    {
     p *= X[n];
     px[j] += p;
     if( j <= 3 ) py[j] += p * Y[n];
    }
  }
 loop( i, 4 ) loop( j, 4 ) A[i][j] = px[i+j];

/* ATA inversed */

 if( M4_invrs( A, B ) ) return 1;

/* coefficient solutions */

 loop( i, 4 )
  {
   C[i] = 0;
   loop ( j, 4 ) C[i] += B[i][j] * py[j];
  }

/* RMS post-fit residual */

 *Sig = 0;
 if( Npt > 4 )
  {
   loop( n, Npt )
     *Sig += pow( C[0] + ( C[1] +  ( C[2] + C[3] * X[n] ) * X[n] ) * X[n] - Y[n] , 2 );
   *Sig = sqrt( *Sig / (Npt-4) );
  }

 return 0;
}
