#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "GRACEiolib.h"
#include "GRACEgpslib.h"
#include "TimeLib.h"

static char SccsId[] = "$Id: OrbitEciInt.c,v 1.2 2004/08/30 21:03:32 wib Exp $";

#define MAXCHAR 1000

long OrbitEciInt(long seconds, double sec_frac, double xyz[3], double llh[3], 
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

void LoadECIOrbitFile(double **pXorb, double **pYorb, double **pZorb, 
                   double **pXdot, double **pYdot, double **pZdot, 
                   double **pTimeOrb, double *pTimeZero, long *pNorb, FILE *eci)
/*----------------------------------------------------------------------------->
/ Purpose:  load orbit file information into memory from file ORBIT_JPL_TXT
/
/ Coded by: Gerhard L.H. Kruizinga                    07/28/98
/
/ output:    (double) **Xorb     pointer to pointer to x coordinate array
/            (double) **Yorb     pointer to pointer to y coordinate array
/            (double) **Zorb     pointer to pointer to z coordinate array
/            (double) **Xdot     pointer to pointer to x velocity array
/            (double) **Ydot     pointer to pointer to y velocity array
/            (double) **Zdot     pointer to pointer to z velocity array
/            (double) **Timeorb  pointer to pointer to relatvie time tag array 
/                                with respect to TimeZero
/            (double) *TimeZero  pointer to time tag offset for 
/                                Timeorb (GPS time) sec2000
/            (long)   *Norb      pointer to number of points 
/                                in x,y,z and TimeOrb
/
<-----------------------------------------------------------------------------*/
{
  double   xdot,ydot,zdot,seconds,sec_frac;
  double   RefDelta,Delta;
  double   T2000,GPS2000;

  int      year,month,day,hour,min;

  long     count,gap_count,sc_id;

  char     line[MAXCHAR];

  table leap_table;

  leap_table = LoadLeapSeconds();

/*----------------------------------------------------------------------------->
/ Read ECI orbit file and determine number of lines in this file
<-----------------------------------------------------------------------------*/

  count     = 0;
  gap_count = 0;

  while ( fgets(line,MAXCHAR,eci) != NULL ) count++; 

  (*pNorb) = count;

  rewind (eci);

/*----------------------------------------------------------------------------->
/ Read orbit information into arrays
<-----------------------------------------------------------------------------*/

  (*pXorb)     = (double *) calloc( (size_t) *(pNorb), sizeof(double) );
  (*pYorb)     = (double *) calloc( (size_t) *(pNorb), sizeof(double) );
  (*pZorb)     = (double *) calloc( (size_t) *(pNorb), sizeof(double) );
  (*pXdot)     = (double *) calloc( (size_t) *(pNorb), sizeof(double) );
  (*pYdot)     = (double *) calloc( (size_t) *(pNorb), sizeof(double) );
  (*pZdot)     = (double *) calloc( (size_t) *(pNorb), sizeof(double) );
  (*pTimeOrb)  = (double *) calloc( (size_t) *(pNorb), sizeof(double) );

  count = 0;
  while ( fgets(line,MAXCHAR,eci) != NULL )
  {
    sscanf(line,"%ld %ld %ld %ld %ld %ld %lf %lf %lf %lf %lf %lf %lf",
                &sc_id,&year,&month,&day,&hour,&min,&seconds,
                (*pXorb+count),(*pYorb+count),(*pZorb+count),
                (*pXdot+count),(*pYdot+count),(*pZdot+count));
 
    sec_frac           = seconds - (double)( (int)seconds);
 
    T2000 =  calsec (year,month,day,hour,min,(int)seconds,sec_frac);

    GPS2000 = utc2gps(T2000,leap_table);

    sec_frac           = seconds - (double)( (int)seconds);

    if (count == 0) *(pTimeZero) = GPS2000;
 
    *(*pTimeOrb+count) = GPS2000 - *(pTimeZero);

    if (count > 1)
    {
      Delta = *(*pTimeOrb+count) - *(*pTimeOrb+count-1);
      if (count == 2) RefDelta = Delta;
      if (RefDelta != Delta)
      {
        fprintf(stdout," ECI Orbit file has data gap of %f at %f\n with reference spacing %f\n",
        Delta,*(*pTimeOrb+count-1) + *(pTimeZero) ,RefDelta);
        gap_count++;
      }      
    }

    count++; 
  }

  if (gap_count != 0)
  {
    fprintf(stdout,"\n Orbit gaps have been detected, proceed with caution!!\n");
    fprintf(stdout," Orbit interpolation results maybe corrupted within and near data gaps\n\n");
  }

}
