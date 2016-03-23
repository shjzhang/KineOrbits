#include <stdio.h>
#include <string.h>
#include <math.h>
#include "GRACEiolib.h"
#include "GRACEdefs.h"
#include "GRACEgpslib.h"

static char SccsId[] = "$Id: eci2grace_quaternion.c,v 1.8 2004/08/30 21:03:32 wib Exp $";

#define MAXCHAR 1000
#define AE      6378.1363
#define FLAT   .00335281317789691420
#define N       3

long eci2grace_quaternion(long T2000_int, double T2000_frac , double clkcorrA,
                          double clkcorrB,
                          quaternion *Q_A, quaternion *Q_B, long norder_int, 
                          FILE *eciA, FILE *eciB)
/*----------------------------------------------------------------------------->
/ Purpose: return quaternion for GRACE A and GRACE B based on both orbits in
/          eci format at time tag "time"
/
/ Coded by: Gerhard L.H. Kruizinga                    07/28/98
/ Modified: Gerhard L.H. Kruizinga                    12/11/01
/ Modified: Gerhard L.H. Kruizinga                    01/16/02
/
/ input:   T2000_int  GPS time seconds past 01/01/2000 12:00:00
/          T2000_frac fractional seconds (sec)
/          clkcorrA   clock correction for Satellite A (added to time tag)
/          clkcorrB   clock correction for Satellite B (added to time tag)
/          eciA       pointer to open eciA file
/          eciB       pointer to open eciB file
/          norder_int order to be used for lagrangian orbit interpolation
/ output:  Q_A        quaternion  given pointing information of GRACE A S/C
/                    (inertial to orbit_local frame, defined by position,
/                     Line Of Site (LOS) vector between GRACE A and GRACE B) 
/          Q_B        quaternion  given pointing information of GRACE A S/C
/                    (inertial to orbit_local frame, defined by position,
/                     Line Of Site (LOS) vector between GRACE A and GRACE B) 
/ 
/
/ eci2quaternion returns  0: for succesful interpolation/quaternion conversion
/                         1: requested time past orbit file interval
/                        -1: requested time before orbit file interval
<-----------------------------------------------------------------------------*/
{
  long out,i;

  double xyzA[N],llhA[N],xyzdotA[N],ae,flat;
  double xyzB[N],llhB[N],xyzdotB[N];
  double LOSepochA[N];
  double LOSepochB[N];
  double Norm,Xbod[N],Ybod[N],Zbod[N];
  double Rot[N][N];
  double time,time_frac;

  long   time_int;

  ae    = AE;
  flat  = FLAT;

  Q_A->q0 = -2.0;
  Q_A->q1 = -2.0;
  Q_A->q2 = -2.0;
  Q_A->q3 = -2.0;

  Q_B->q0 = -2.0;
  Q_B->q1 = -2.0;
  Q_B->q2 = -2.0;
  Q_B->q3 = -2.0;

  /*>>>> interpolate orbit to current gps time for GRACEA <<<<*/

  time = T2000_int + T2000_frac;
  time += clkcorrA;
  time_int = (long) time;  
  time_frac = time - (double) time_int;

  if ((out = OrbitEciInt(time_int,time_frac,xyzA,llhA, xyzdotA,ae,flat,
       norder_int,eciA)) != 0) return out;

  if ((out = OrbitEciInt_second(time_int,time_frac,LOSepochA,llhB, xyzdotB,ae,flat,
       norder_int,eciB)) != 0) return out;

  /*>>>> interpolate orbit to current gps time for GRACEB <<<<*/

  time = T2000_int + T2000_frac;
  time += clkcorrB;
  time_int = (long) time;  
  time_frac = time - (double) time_int;

  if ((out = OrbitEciInt(time_int,time_frac,LOSepochB,llhA, xyzdotA,ae,flat,
       norder_int,eciA)) != 0) return out;

  if ((out = OrbitEciInt_second(time_int,time_frac,xyzB,llhB, xyzdotB,ae,flat,
       norder_int,eciB)) != 0) return out;

  /*>>>> compute LOS vector GRACE A -> GRACE B at GRACE A epoch<<<<*/

  LOSepochA[0] -= xyzA[0];
  LOSepochA[1] -= xyzA[1];
  LOSepochA[2] -= xyzA[2];

  /*>>>> compute LOS vector GRACE B -> GRACE A at GRACE B epoch<<<<*/

  LOSepochB[0] -= xyzB[0];
  LOSepochB[1] -= xyzB[1];
  LOSepochB[2] -= xyzB[2];

  /* compute unit vectors for the body fixed xbod,ybod,zbod in the inertial
   coordinate system for GRACE A */

  /* x-axis */

  Xbod[0] = LOSepochA[0];
  Xbod[1] = LOSepochA[1];
  Xbod[2] = LOSepochA[2];

  Norm = VectorNorm(Xbod, N);

  loop(i,N) Xbod[i] = Xbod[i]/Norm;

  /* y-axis */

  Zbod[0] = -xyzA[0];
  Zbod[1] = -xyzA[1];
  Zbod[2] = -xyzA[2];

  Norm = VectorNorm(Zbod, N);

  loop(i,N) Zbod[i] = Zbod[i]/Norm;

  CrossProduct(Zbod, Xbod, N , Ybod);

  Norm = VectorNorm(Ybod, N);

  loop(i,N) Ybod[i] = Ybod[i]/Norm;

  /* z-axis */

  CrossProduct(Xbod, Ybod, N , Zbod);

  Norm = VectorNorm(Zbod, N);

  loop(i,N) Zbod[i] = Zbod[i]/Norm;

/*
  fprintf(stderr,"\nLOS A->B: %lf %lf %lf\n\n",LOSepochA[0],LOSepochA[1],LOSepochA[2]);
  fprintf(stderr,"\nLOS B->A: %lf %lf %lf\n\n",LOSepochB[0],LOSepochB[1],LOSepochB[2]);
  fprintf(stderr,"\nXbod A: %lf %lf %lf \n",Xbod[0],Xbod[1],Xbod[2]);
  fprintf(stderr,"Ybod A: %lf %lf %lf \n",Ybod[0],Ybod[1],Ybod[2]);
  fprintf(stderr,"Zbod A: %lf %lf %lf \n",Zbod[0],Zbod[1],Zbod[2]);
*/

  /* compute rotation matrix */

  Rot[0][0] = Xbod[0]; Rot[0][1] = Ybod[0]; Rot[0][2] = Zbod[0];
  Rot[1][0] = Xbod[1]; Rot[1][1] = Ybod[1]; Rot[1][2] = Zbod[1];
  Rot[2][0] = Xbod[2]; Rot[2][1] = Ybod[2]; Rot[2][2] = Zbod[2];

  /* convert rotation matrix into quaternions */

  Q_A->q0 = sqrt(1.0 + Rot[0][0] + Rot[1][1] + Rot[2][2])/2.0;

  if (fabs(Q_A->q0) < 1e-6)
  {
    fprintf(stderr,"eci2grace_quaternion: Q0_A = %.16g < 1e-6, quaternion maybe corrupted at t = %.16g\n",
                   Q_A->q0,T2000_frac+(double)T2000_int);
  }

  Q_A->q1 = -(Rot[1][2] - Rot[2][1])/4.0/Q_A->q0;
  Q_A->q2 = -(Rot[2][0] - Rot[0][2])/4.0/Q_A->q0;
  Q_A->q3 = -(Rot[0][1] - Rot[1][0])/4.0/Q_A->q0;

  /* compute unit vectors for the body fixed xbod,ybod,zbod in the inertial
   coordinate system for GRACE B */

  /* x-axis */

  Xbod[0] = LOSepochB[0];
  Xbod[1] = LOSepochB[1];
  Xbod[2] = LOSepochB[2];

  Norm = VectorNorm(Xbod, N);

  loop(i,N) Xbod[i] = Xbod[i]/Norm;

  /* y-axis */

  Zbod[0] = -xyzB[0];
  Zbod[1] = -xyzB[1];
  Zbod[2] = -xyzB[2];

  Norm = VectorNorm(Zbod, N);

  loop(i,N) Zbod[i] = Zbod[i]/Norm;

  CrossProduct(Zbod, Xbod, N , Ybod);

  Norm = VectorNorm(Ybod, N);

  loop(i,N) Ybod[i] = Ybod[i]/Norm;

  /* z-axis */

  CrossProduct(Xbod, Ybod, N , Zbod);

  Norm = VectorNorm(Zbod, N);

  loop(i,N) Zbod[i] = Zbod[i]/Norm;

/*
  fprintf(stderr,"\nXbod B: %lf %lf %lf \n",Xbod[0],Xbod[1],Xbod[2]);
  fprintf(stderr,"Ybod B: %lf %lf %lf \n",Ybod[0],Ybod[1],Ybod[2]);
  fprintf(stderr,"Zbod B: %lf %lf %lf \n",Zbod[0],Zbod[1],Zbod[2]);
*/

  /* compute rotation matrix */

  Rot[0][0] = Xbod[0]; Rot[0][1] = Ybod[0]; Rot[0][2] = Zbod[0];
  Rot[1][0] = Xbod[1]; Rot[1][1] = Ybod[1]; Rot[1][2] = Zbod[1];
  Rot[2][0] = Xbod[2]; Rot[2][1] = Ybod[2]; Rot[2][2] = Zbod[2];

  /* convert rotation matrix into quaternions */

  Q_B->q0 = sqrt(1.0 + Rot[0][0] + Rot[1][1] + Rot[2][2])/2.0;

  if (fabs(Q_B->q0) < 1e-6)
  {
    fprintf(stderr,"eci2grace_quaternion: Q0_B = %.16g < 1e-6, quaternion maybe corrupted at t = %.16g\n",
                   Q_B->q0,T2000_frac+(double)T2000_int);
  }

  Q_B->q1 = -(Rot[1][2] - Rot[2][1])/4.0/Q_B->q0;
  Q_B->q2 = -(Rot[2][0] - Rot[0][2])/4.0/Q_B->q0;
  Q_B->q3 = -(Rot[0][1] - Rot[1][0])/4.0/Q_B->q0;

  return 0L;
}
