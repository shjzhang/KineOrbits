#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "GRACEiolib.h"
#include "GRACEdefs.h"
#include "GRACEgpslib.h"

static char SccsId[] = "$Id: eci2quaternion.c,v 1.8 2004/08/30 21:03:32 wib Exp $";

#define MAXCHAR 1000
#define AE      6378.1363
#define FLAT   .00335281317789691420
#define N       3

long eci2quaternion(long T2000_int, double T2000_frac ,quaternion *Q,
                    long norder_int, FILE *eci)
/*----------------------------------------------------------------------------->
/ Purpose: return orbit (in xyz and llh) at time tag "time"
/
/ Coded by: Gerhard L.H. Kruizinga                    07/28/98
/ Modified: Gerhard L.H. Kruizinga                    12/11/01
/ Modified: Gerhard L.H. Kruizinga                    08/23/04
/
/ input:   T2000_int  GPS time seconds past 01/01/2000 12:00:00
/          T2000_frac fractional seconds (sec)
/          eci        pointer to open eci file
/          norder_int order to be used for lagrangian orbit interpolation
/ output:  Q          quaternion  given pointing information of the spacecraft 
/                    (inertial to orbit_local frame, defined by position,
/                     velocity and cross product of these two vectors) 
/ 
/
/ eci2quaternion returns  0: for succesful interpolation/quaternion conversion
/                         1: requested time past orbit file interval
/                        -1: requested time before orbit file interval
<-----------------------------------------------------------------------------*/
{
  long out,i;

  double xyz[N],llh[N],xyzdot[N],ae,flat;
  double Norm,Xbod[N],Ybod[N],Zbod[N];
  double Rot[N][N];

  char *eci_filename;

  static long first = 1;

  ae    = AE;
  flat  = FLAT;
  Q->q0 = -2.0;
  Q->q1 = -2.0;
  Q->q2 = -2.0;
  Q->q3 = -2.0;

  /*>>>> check if eci pointer is set if not querry enviroment variable ECIFILE <<<<<*/

  if (eci == NULL && first)
  {
    first = 0;

    if ( (eci_filename = getenv("ECIFILE") ) == NULL  )
    {
      fprintf( stderr,"\neci2quaternion: No environment variable defined : ECIFILE\n");
      exit(1);
    }

    if ( ( eci  = fopen(eci_filename, "r" ) ) == NULL )
    {
      fprintf( stdout,"\neci2quaternion: ECIFILE  %s can not be opened. \n", eci_filename );
      exit(1);
    }
  }

  /*>>>> interpolate orbit to current gps time <<<<*/

  if ((out = OrbitEciInt(T2000_int,T2000_frac,xyz,llh, xyzdot,ae,flat,norder_int,eci)) != 0)
     return out;

  /* compute unit vectors for the body fixed xbod,ybod,zbod in the inertial
   coordinate system */

  /* z-axis */

  Zbod[0] = -xyz[0];
  Zbod[1] = -xyz[1];
  Zbod[2] = -xyz[2];

  Norm = VectorNorm(Zbod, N);

  loop(i,N) Zbod[i] = Zbod[i]/Norm;

  /* y-axis */

  Xbod[0] = xyzdot[0];
  Xbod[1] = xyzdot[1];
  Xbod[2] = xyzdot[2];

  CrossProduct(Zbod, Xbod, N , Ybod);

  Norm = VectorNorm(Ybod, N);

  loop(i,N) Ybod[i] = Ybod[i]/Norm;

  /* x-axis */

  CrossProduct(Ybod, Zbod, N , Xbod);

  Norm = VectorNorm(Xbod, N);


  loop(i,N) Xbod[i] = Xbod[i]/Norm;

/*
  fprintf(stderr,"\nXbod : %lf %lf %lf \n",Xbod[0],Xbod[1],Xbod[2]);
  fprintf(stderr,"Ybod : %lf %lf %lf \n",Ybod[0],Ybod[1],Ybod[2]);
  fprintf(stderr,"Zbod : %lf %lf %lf \n",Zbod[0],Zbod[1],Zbod[2]);
*/

  /* compute rotation matrix */

  Rot[0][0] = Xbod[0]; Rot[0][1] = Ybod[0]; Rot[0][2] = Zbod[0];
  Rot[1][0] = Xbod[1]; Rot[1][1] = Ybod[1]; Rot[1][2] = Zbod[1];
  Rot[2][0] = Xbod[2]; Rot[2][1] = Ybod[2]; Rot[2][2] = Zbod[2];

  /* convert rotation matrix into quaternions */

  Q->q0 = sqrt(1.0 + Rot[0][0] + Rot[1][1] + Rot[2][2])/2.0;

  if (fabs(Q->q0) < 1e-6)
  {
    fprintf(stderr,"eci2quaternion: Q0 = %.16g < 1e-6, quaternion maybe corrupted at t = %.16g\n",
                   Q->q0,T2000_frac+(double)T2000_int);
  }

  Q->q1 = -(Rot[1][2] - Rot[2][1])/4.0/Q->q0;
  Q->q2 = -(Rot[2][0] - Rot[0][2])/4.0/Q->q0;
  Q->q3 = -(Rot[0][1] - Rot[1][0])/4.0/Q->q0;

  return 0L;
}
void CrossProduct(double *x, double *y, long n, double *cross)
/*----------------------------------------------------------------------------->
/ purpose:  Compute CrossProduct x X y and return result in cross
/
/ coded by: Gerhard L.H. Kruizinga                07/13/99
/
/ input:    x,y input vectors, n length of vector (should be 3 always)
/
/ output:   cross, vector containing cross product
/
<-----------------------------------------------------------------------------*/
{

  cross[0] = x[1]*y[2] - y[1]*x[2];
  cross[1] = y[0]*x[2] - x[0]*y[2];
  cross[2] = x[0]*y[1] - y[0]*x[1];

}
/* ************************************************************************** */
double DotProduct(double *x, double *y, long n )
/*----------------------------------------------------------------------------->
/ purpose:  Compute Dot Product x . y and return result
/
/ coded by: Gerhard L.H. Kruizinga                07/13/99
/
/ input:    x,y input vectors, n length of vector
/
/ output:   dot product is returned by routine
/
<-----------------------------------------------------------------------------*/
{
 double normx,normy;
 double dotproduct;

 long i;

 normx = VectorNorm(x,n);
 normy = VectorNorm(y,n);

 dotproduct = 0;

 loop(i,n) dotproduct = dotproduct + x[i]*y[i];

 return dotproduct/normx/normy;
}
/* ************************************************************************** */
double VectorNorm(double *x, long n)
/*----------------------------------------------------------------------------->
/ purpose:  Compute Dot Product x . y and return result
/
/ coded by: Gerhard L.H. Kruizinga                07/13/99
/
/ input:    x,y input vectors, n length of vector
/
/ output:   dot product is returned by routine
/
<-----------------------------------------------------------------------------*/
{
 double norm;

 long i;

 norm = 0.0;

 loop(i,n) norm = norm + x[i]*x[i];

 return sqrt(norm);
}

