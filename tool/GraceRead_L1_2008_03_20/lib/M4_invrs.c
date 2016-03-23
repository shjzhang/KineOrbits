/* @(#) M4_invrs.c      1.2 06/18/01

   Purpose:
        Inversion of a 4 x 4 matrix

   06/18/2001   Sien Wu         Created
*/
#include "GRACEdefs.h"
#include "GRACEprototype.h"

int M4_invrs   (
/* input */
		real	A[4][4],	/* input 4 x 4 matrix */
/* output */
		real    B[4][4]         /* inverse of A */

               ) /* return 1 if matrix is singular; 0 otherwise */
{
 static char SccsId[] = "@(#) M4_invrs.c      1.2 06/18/01";

/* submatrices
                  | S11 (2x2)  |  S12 (2x2) |
            A ==> | -----------+----------- |
                  | S21 (2x2)  |  S22 (2x2) |
*/
 real	S11[2][2], S12[2][2], S21[2][2], S22[2][2];
 real	X[2][2];		/* S11^-1 * S12		*/
 real	Y[2][2];		/* S21 * S11^-1		*/
 real	Q[2][2];		/* S22 - S21 * X	*/
 real	T11[2][2], QI[2][2];	/* inverse of S11, Q	*/
 real	XQI[2][2];		/* X * QI		*/
 int	i, j, k;

 loop( i, 2 ) loop( j, 2 )
  {
   S11[i][j] = A[i][j];
   S12[i][j] = A[i][j+2];
   S21[i][j] = A[i+2][j];
   S22[i][j] = A[i+2][j+2];
   X[i][j] = 0;
   Y[i][j] = 0;
   Q[i][j] = S22[i][j];
   QI[i][j] = 0;
   XQI[i][j] = 0;
  }
 loop( i, 4 ) loop( j, 4 ) B[i][j] = 0;

 if( M2_invrs( S11, T11 ) ) return 1;

 loop( i, 2 ) loop( j, 2 ) loop( k, 2 ) X[i][j] += T11[i][k] * S12[k][j];
 loop( i, 2 ) loop( j, 2 ) loop( k, 2 ) Q[i][j] -= S21[i][k] * X[k][j];

 if( M2_invrs( Q, QI ) ) return 1;

 loop( i, 2 ) loop( j, 2 ) loop( k, 2 ) Y[i][j] += S21[i][k] * T11[k][j];
 loop( i, 2 ) loop( j, 2 ) loop( k, 2 ) XQI[i][j] += X[i][k] * QI[k][j];
 
 loop( i, 2 ) loop( j, 2 )
  {
   B[i][j] = T11[i][j];
   B[i+2][j+2] = QI[i][j];
   loop( k, 2 )
    {
     B[i][j]  += XQI[i][k] * Y[k][j];
     B[i+2][j] -= QI[i][k] * Y[k][j];
     B[i][j+2] -= X[i][k]  * QI[k][j];
    }
  }
return 0;
}
