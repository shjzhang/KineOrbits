/*   @(#) quat2angl.c    1.1     11/30/01

   Purpose:
          convert a quaternion Q into 3 rotation angles

   11/30/2001	Sien Wu		created 

*/
#include "GRACEdefs.h"
#include "GRACEprototype.h"

void quat2angl(

/* Input: */
        quaternion	Q,	/* quaternion */

/* Output: */
        real		*RA,	/* rotation angles (radians) */
        real		*Dec,
        real		*Twist
           )
{
/* Local: */
  static char SccsId[] = "@(#) quat2angl.c      1.1 11/30/01";

  real	q00, q11, q22, q33 ;

  q00 = Q.q0*Q.q0 ;
  q11 = Q.q1*Q.q1 ;
  q22 = Q.q2*Q.q2 ;
  q33 = Q.q3*Q.q3 ;

  *RA    = atan2( 2*(Q.q1*Q.q2 + Q.q0*Q.q3), q00+q11-q22-q33 ); 
  *Dec   =  asin( 2*(Q.q0*Q.q2 - Q.q1*Q.q3) );
  *Twist = atan2( 2*(Q.q2*Q.q3 + Q.q0*Q.q1), q00-q11-q22+q33 ); 

  return ;
}
