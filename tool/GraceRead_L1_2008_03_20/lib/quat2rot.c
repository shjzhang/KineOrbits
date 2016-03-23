/****************************************************************************
*      RTG Source Code,                                                     *
*      Copyright (C) 1996, California Institute of Technology               *
*      U.S. Government Sponsorship under NASA Contract NAS7-1260            *
*                    (as may be time to time amended)                       *
*                                                                           *
*      RTG is a trademark of the California Institute of Technology.        *
*                                                                           *
*      written by Yoaz Bar-Sever, Willy Bertiger, Bruce Haines,             *
*                 Angelyn Moore, Ron Muellerschoen, Tim Munson,             *
*                 Larry Romans, and Sien Wu                                 *
*                                                                           *
*      modified by Gerhard L.H. Kruizinga               2003-02-06          *
****************************************************************************/

/* Purpose:  output the matrix associated with rotatation quaternion
    note that this is the transpose of quat2rotMat.c
    quaternion      Q      rotation quaternion
    looks like: cos(f/2), xsin(f/2), ysin(f/2), zsin(f/2)
*/

#include "GRACEdefs.h"
#include "GRACEprototype.h"
#include <math.h>

static char SccsId[] = "@(#) quat2rot.c       1.1 02/06/03";

void quat2rot( quaternion qp, matrix m )
{
   m.value[0][0] = qp.q0*qp.q0 + qp.q1*qp.q1 - qp.q2*qp.q2 - qp.q3*qp.q3;
   m.value[0][1] = 2*qp.q1*qp.q2 - 2*qp.q0*qp.q3;
   m.value[0][2] = 2*qp.q1*qp.q3 + 2*qp.q0*qp.q2;
   m.value[1][0] = 2*qp.q1*qp.q2 + 2*qp.q0*qp.q3;
   m.value[1][1] = qp.q0*qp.q0 - qp.q1*qp.q1 + qp.q2*qp.q2 - qp.q3*qp.q3;
   m.value[1][2] = 2*qp.q2*qp.q3 - 2*qp.q0*qp.q1;
   m.value[2][0] = 2*qp.q1*qp.q3 - 2*qp.q0*qp.q2;
   m.value[2][1] = 2*qp.q2*qp.q3 + 2*qp.q0*qp.q1;
   m.value[2][2] = qp.q0*qp.q0 - qp.q1*qp.q1 - qp.q2*qp.q2 + qp.q3*qp.q3;
}
