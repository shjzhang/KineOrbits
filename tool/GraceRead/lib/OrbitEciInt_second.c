#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "GRACEiolib.h"
#include "GRACEgpslib.h"
#include "TimeLib.h"

static char SccsId[] = "$Id: OrbitEciInt_second.c,v 1.3 2004/08/30 21:03:32 wib Exp $";

#define MAXCHAR 1000

long OrbitEciInt_second(long seconds, double sec_frac, double xyz[3], double llh[3], 
             double xyzdot[3], double ae, double flat, long norder_int,
             FILE *eci)
/*----------------------------------------------------------------------------->
/ Purpose: return orbit (in xyz and llh) at time tag "time"
/
/ Coded by: Gerhard L.H. Kruizinga                    07/28/98
/ Modified: Gerhard L.H. Kruizinga                    12/11/01
/
/ input:    (long) seconds       Seconds past 2000 (GPS Time) integer part
/           (double) sec_frac    Fractional part seconds past 2000 (GPS Time)
/           (double) ae          Equatorial radius of the ref. ellipsoid (km)
/           (double) flat        flattening for the ref. ellipsoid
/            norder_int          order to be used for lagrangian orbit 
/                                interpolation
/           
/
/ output:   (double) xyz[3]      x,y,z coordinates (in orbit file units)
/           (double) llh[3]      lat,lon (degrees) and 
/                                height (in orbit file units)
/
/ OrbitInt returns  0: for succesful interpolation
/                   1: requested time past orbit file interval
/                  -1: requested time before orbit file interval
/
/ Notes: a)On the first call this routine will open file with pointer "eci"
/          This file is a standard eci format and the time tags in this file 
/          are assumed to be in UTC time.
/        b)a 7th order polynomial is used for the orbit interpolation
<-----------------------------------------------------------------------------*/
{

  static int      first=1;
  static double   *Xorb, *Yorb, *Zorb, *TimeOrb, TimeZero;
  static double   *Xdot, *Ydot, *Zdot;
  double          GPStime,UTCtime;
  double          sec_frac_new,inside;
  double          deriv;
  
  int             compute_deriv=0;

  static long     Norb,i;

  static table    leap;

  if (first == 1)
  {

    LoadECIOrbitFile(&Xorb,&Yorb,&Zorb,&Xdot,&Ydot,&Zdot,&TimeOrb,&TimeZero,
                  &Norb,eci);

    fprintf(stdout," numb of lines : %d\n",Norb);
    fprintf(stdout," order interpol: %d\n",norder_int);
    fprintf(stdout," TimeZero      : %20.7f\n",TimeZero);

    first = 0;
  }

  GPStime = sec_frac + (double)seconds - TimeZero;

/*----------------------------------------------------------------------------->
/ interpolate x,y,z to GPS time tag
<-----------------------------------------------------------------------------*/

  inside = lagrange_int( GPStime, &xyz[0], Norb, TimeOrb, Xorb,
                         norder_int, compute_deriv, &deriv);
  inside = lagrange_int( GPStime, &xyz[1], Norb, TimeOrb, Yorb,
                         norder_int, compute_deriv, &deriv);
  inside = lagrange_int( GPStime, &xyz[2], Norb, TimeOrb, Zorb,
                         norder_int, compute_deriv, &deriv);
  inside = lagrange_int( GPStime, &xyzdot[0], Norb, TimeOrb, Xdot,
                         norder_int, compute_deriv, &deriv);
  inside = lagrange_int( GPStime, &xyzdot[1], Norb, TimeOrb, Ydot,
                         norder_int, compute_deriv, &deriv);
  inside = lagrange_int( GPStime, &xyzdot[2], Norb, TimeOrb, Zdot,
                         norder_int, compute_deriv, &deriv);

  if (inside != 0) return inside;

/*----------------------------------------------------------------------------->
/ compute lat lon and height
<-----------------------------------------------------------------------------*/

   xyz2llh(xyz,ae,flat,llh);

/*----------------------------------------------------------------------------->
  loop(i,Norb)
  {
    fprintf(stdout,"t,x,y,z %d %15.8f %15.8f %15.8f %15.8f\n",i,
                   *(TimeOrb+i),*(Xorb+i),*(Yorb+i),*(Zorb+i));
  }
<-----------------------------------------------------------------------------*/
  return inside; 
}
