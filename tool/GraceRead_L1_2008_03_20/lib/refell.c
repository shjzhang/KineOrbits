#include <stdio.h>
#include <math.h>
#include "GRACEdefs.h"
#include "GRACEgpslib.h"

static char SccsId[] = "$Id: refell.c,v 1.3 2004/08/30 21:03:33 wib Exp $";

void xyz2llh(double xyz[3] , double ae, double flat, double llh[3])

{
/*----------------------------------------------------------------------------->
/
/   purpose: compute the height above the reference ellipsoid, geodetic
/            latitude and longitude of a point in body-fixed coordinates
/
/   coded by: j ries - university of texas - august 1978
/   modified: Gerhard L.H. Kruizinga - converted into c - 07/23/98
/
/   formal parameters:
/   input:    xyz      position vector in body-fixed coordinates
/             ae       equatorial radius of earth
/             flat     flattening of the earth
/
/   output:   llh[0]   geodetic latitude of sub-satellite point
/             llh[1]   longitude of sub-satellite point
/             llh[2]   height above reference ellipsoid
/
<-----------------------------------------------------------------------------*/

  double    e2,xtild2,t,enph,sine,zt,cosine;
  double    sine2,en,pi,twopi,tnew,rad;

  long      i;

  pi    = 4.0e0*atan(1.0e0);
  twopi = 2.0e0*pi;
  rad   = pi/180.0e0;

/*----------------------------------------------------------------------------->
/   compute height, latitude and longitude
<-----------------------------------------------------------------------------*/

  e2     = flat*(2.0e0 - flat);
  xtild2 = xyz[0] * xyz[0] + xyz[1] * xyz[1];
  i      = 0;
  if (fabs(xyz[2]) < 1.0e-6) xyz[2] = 1.0e-6;
  t      = e2*xyz[2];
  tnew   = t + 1.0;
  while ( fabs(tnew - t) > 1.0e-6)
  {
    if (i == 0) tnew = tnew - 1.0;
    t      = tnew;
    zt     = xyz[2]+t;
    enph   = sqrt(xtild2+zt*zt);
    sine   = zt/enph;
    cosine = 1.0e0-e2*(sine*sine);
    if (cosine < 0.0e+0 || i > 20) 
    {
       fprintf(stderr,"\n Error in xyz2ell : cosine %.16g : iter %d\n\n",cosine,i);
       exit(1);
    }
    en=ae/sqrt(cosine);
    tnew=en*e2*sine;
    i=i+1;
  }
 
  zt     = xyz[2]+tnew;
  enph   = sqrt(xtild2+zt*zt);
  sine   = zt/enph;
  sine2  = sine*sine;
  en     = ae/sqrt(1.0-e2*sine2);
  llh[2] = enph-en;
  llh[0] = asin(sine);
  llh[1] = atan2(xyz[1],xyz[0]);

  if (llh[1] < 0.0e+0) llh[1] = llh[1] + twopi;

  llh[0] = llh[0] / rad;
  llh[1] = llh[1] / rad;

  return;
}
void llh2xyz(double xyz[3] , double ae, double flat, double llh[3])

{
/*----------------------------------------------------------------------------->
/
/   purpose: convert the height above the reference ellipsoid, geodetic
/            latitude and longitude of a point into body-fixed coordinates
/            based on ~ljr/bin/llh2xyz
/
/   coded by: Gerhard L.H. Kruizinga - converted into c - 07/23/98
/
/   formal parameters:

/   input:    llh[0]   geodetic latitude of sub-satellite point (deg)
/             llh[1]   longitude of sub-satellite point         (deg)
/             llh[2]   height above reference ellipsoid        
/             ae       equatorial radius of earth
/             flat     flattening of the earth
/
/   output:   xyz      position vector in body-fixed coordinates
/
<-----------------------------------------------------------------------------*/

  double    pi,twopi,rad;
  double    ba,s,c,p;

  pi     = 4.0e0*atan(1.0e0);
  twopi  = 2.0e0*pi;
  rad    = pi/180.0e0;

  ba     = (1.0 - flat)*(1.0 - flat);
  s      = sin(llh[0]*rad);
  c      = cos(llh[0]*rad);
  p      = ae/sqrt(c*c + ba*s*s);
  xyz[0] = (p + llh[2])*c*cos(llh[1]*rad);
  xyz[1] = (p + llh[2])*c*sin(llh[1]*rad);
  xyz[2] = (ba*p + llh[2]) * s;

  return;
}
