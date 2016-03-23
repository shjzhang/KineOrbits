/* @(#) quat_rot.c      1.4 05/29/02
   Purpose:
          rotate vector u into vector v with a quaternian Q 

Initial coding:
   02/21/1996	Sien Wu
   01/19/2001	Sien Wu		Clearer comments

*/

#include "GRACEdefs.h"
#include "GRACEprototype.h"

void quat_rot(

/* Input: */
        quaternion	Q,	/* rotation quaternion */
				/* cos(f/2), x*sin(f/2), y*sin(f/2), z*sin(f/2) */

        vector		u,	/* 3-vector to be rotated */
/* Output: */
        vector		v	/* rotated 3-vector */
           )
{
/* Local: */
  static char SccsId[] = "@(#) quat_rot.c      1.4 05/29/02";

  real d ;			/* = R'u */
  vector R ;			/* containing q1, q2, q3 of Q */

  R = vector_create( 3 ) ;
/*
    The rotation matrix based on q is: M = (Q.q0^2 - R'R)I + 2RR' - 2Q.q0Eijk(R)
    where Eijk is the permutation tensor. After multiplying with a vector u
    the result can be written (using R'R = 1 - Q.q0^2):
    v = (2Q.q0^2 - 1)u + 2RR'u - 2Q.q0(R x u)
*/ 
/* Do the cross product first */

  R.value[0] = Q.q1 ;
  R.value[1] = Q.q2 ;
  R.value[2] = Q.q3 ;
  cross( R, u, v) ;

/* Add the dot product and the scalar product */

  d = Q.q1*u.value[0] + Q.q2*u.value[1] + Q.q3*u.value[2] ;

  v.value[0] = (2*Q.q0*Q.q0-1)*u.value[0] + 2*d*Q.q1 - 2*Q.q0*v.value[0] ;
  v.value[1] = (2*Q.q0*Q.q0-1)*u.value[1] + 2*d*Q.q2 - 2*Q.q0*v.value[1] ;
  v.value[2] = (2*Q.q0*Q.q0-1)*u.value[2] + 2*d*Q.q3 - 2*Q.q0*v.value[2] ;

  vector_destroy( R ) ;
  return ;
}
