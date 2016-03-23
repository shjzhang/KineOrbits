/* @(#) M3_invrs.c      1.1 06/19/01

   Purpose:
        Inversion of a 3 x 3 matrix

   06/18/2001   Sien Wu         Created
*/
#include "GRACEdefs.h"
#include "GRACEprototype.h"

int M3_invrs   (
/* input */
		real	A[3][3],	/* input 3 x 3 matrix */
/* output */
		real    B[3][3]         /* inverse of A */

               ) /* return 1 if matrix is singular; 0 otherwise */
{
 static char SccsId[] = "@(#) M3_invrs.c      1.1 06/19/01";

/* submatrices
                  |   S11   (2x2)  |  A[i][2] (2x1) |
            A ==> | ---------------+--------------- |
                  | A[2][j] (1x2)  |  A[2][2] (1x1) |
*/
 real	S11[2][2];
 real	X[2];			/* S11^-1 * A[i][2]		*/
 real	Y[2];			/* A[2][j] * S11^-1		*/
 real	Q;			/* A[2][2] - A[2][j] * X	*/
 real	T11[2][2];		/* inverse of S11		*/
 int	i, j, k;

 loop( i, 2 ) loop( j, 2 ) S11[i][j] = A[i][j];

 if( M2_invrs( S11, T11 ) ) return 1;

 loop( i, 2 )
  {
   X[i] = 0;
   loop( k, 2 ) X[i] += T11[i][k] * A[k][2];
  }
 Q = A[2][2];
 loop( k, 2 ) Q -= A[2][k] * X[k];

 if( Q == 0 ) return 1;

 loop( j, 2 )
  {
   Y[j] = 0;
   loop( k, 2 ) Y[j] += A[2][k] * T11[k][j];
  }
 
 loop( i, 2 ) loop( j, 2 ) B[i][j] = T11[i][j] + X[i] * Y[j] / Q;
 loop( i, 2 )
  {
   B[2][i] = -Y[i] / Q;
   B[i][2] = -X[i] / Q;
  }
 B[2][2] = 1 / Q;

 return 0;
}
