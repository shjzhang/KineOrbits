/* @(#) quat_prod.c      1.1 01/19/01
   Purpose:
	Compute product of two quaternions:  QP = Q*P

Initial coding:
   02/21/1996	Sien Wu
   01/19/2001	Sien Wu		re-normalize product

*/

#include "GRACEdefs.h"
#include "GRACEprototype.h"

void quat_prod(

/* Input: */
		quaternion Q,           /* first input quaternion */
		quaternion P,           /* second input quaternion */
/* Output: */
		quaternion *QP          /* output quaternion product*/
	      )
{
/* Local: */
  static char SccsId[] = "@(#) quat_prod.c      1.1 01/19/01";

  real	a;				/* normalizing factor */

  QP->q0 = Q.q0*P.q0 - Q.q1*P.q1 - Q.q2*P.q2 - Q.q3*P.q3 ;
  QP->q1 = Q.q1*P.q0 + Q.q0*P.q1 - Q.q3*P.q2 + Q.q2*P.q3 ;
  QP->q2 = Q.q2*P.q0 + Q.q3*P.q1 + Q.q0*P.q2 - Q.q1*P.q3 ;
  QP->q3 = Q.q3*P.q0 - Q.q2*P.q1 + Q.q1*P.q2 + Q.q0*P.q3 ;

  if( ( a = pow(QP->q0, 2) + pow(QP->q1, 2) + pow(QP->q2, 2) + pow(QP->q3, 2) ) > 1 )
   {
    a = sqrt( a );
    QP->q0 /= a;
    QP->q1 /= a;
    QP->q2 /= a;
    QP->q3 /= a;
   }
  return ;
}

