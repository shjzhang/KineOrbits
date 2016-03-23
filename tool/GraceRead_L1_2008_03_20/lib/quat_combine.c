/* 
   Purpose:
	Combine two quaternions with given ratio of formal errors
        First one is assumed to point (nominally) in +Y / +Z quadrant

Initial coding:
   03/02/2001      L. Romans

*/

#include "GRACEdefs.h"
#include "GRACEprototype.h"

void quat_combine(

/* Input: */
		quaternion Q,           /* first input quaternion */
		quaternion R,           /* second input quaternion */
                real sigR_over_sigQ,    /* ratio of formal errors */
/* Output: */
		quaternion *S           /* output combined quaternion */
	      )
{
/* Local: */
  static char SccsId[] = "";

  int i, i1, i2;
  real A[4], B[4], Ai[4], X[4], T[4], dQ[4], sq2 = 1.414213562373095;
  real Ca[3], Cb[3], Ci[3];

  Ca[0] = Ca[1] = 1;
  Ca[2] = 1/64.0;
  Ca[2] = 1;  /* hola */

  Cb[0] = Cb[2] = 1/(sigR_over_sigQ * sigR_over_sigQ);
  Cb[1] = Cb[0]/64.0;
  Cb[1] = Cb[0];  /* hola */

  A[0] = Q.q0;
  A[1] = Q.q1;
  A[2] = (Q.q2 + Q.q3)/sq2;
  A[3] = (Q.q2 - Q.q3)/sq2;

  B[0] = R.q0;
  B[1] = R.q1;
  B[2] = (R.q2 + R.q3)/sq2;
  B[3] = (R.q2 - R.q3)/sq2;

  dQ[0] = A[0]*B[0] + A[1]*B[1] + A[2]*B[2] + A[3]*B[3];
  loop (i,3) {
    i1 = (i+1)%3;
    i2 = (i+2)%3;
    dQ[1+i] = -A[1+i]*B[0] + A[0]*B[1+i] + A[1+i1]*B[1+i2] - A[1+i2]*B[1+i1];
  }

  if (dQ[0] < 0) loop(i,4) dQ[i] = -dQ[i];

  loop(i,3) X[1+i] = dQ[1+i]*Cb[i]/(Ca[i] + Cb[i]);

  X[0] = sqrt(1 - X[1]*X[1] - X[2]*X[2] - X[3]*X[3]);

  T[0] = X[0]*A[0] - X[1]*A[1] - X[2]*A[2] - X[3]*A[3];
  loop (i,3) {
    i1 = (i+1)%3;
    i2 = (i+2)%3;
    T[1+i] = X[1+i]*A[0] + X[0]*A[1+i] + X[1+i1]*A[1+i2] - X[1+i2]*A[1+i1];
  }

  S->q0 = T[0];
  S->q1 = T[1];
  S->q2 = (T[2]+T[3])/sq2;
  S->q3 = (T[2]-T[3])/sq2;
}

