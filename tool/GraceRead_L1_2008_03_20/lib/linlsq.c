#include <stdio.h>
#include "GRACEiolib.h"
#include "GRACEprototype.h"

static char SccsId[] = "@(#) linlsq.c       1.5 02/16/02";

long linlsq(double *x, double *y,double *a, double *b, double *a_sigma,
            double *b_sigma, double *rms, double *rms0, long n)
/*----------------------------------------------------------------------------->
/ purpose: compute leastsquares fit of a straight line
/          (y = ax + b) to the data set x,y
/
/ coded by: Gerard L.H. Kruizinga           05/03/94
/ converted to c                            09/09/01
/  
/ input:
/          x       : array containing x-coordinates
/          y       : array containing y-coordinates
/          n       : dimension of array
/
/ output:
/          a       : slope of fit
/          b       : bias of fit
/          rms0    : rms of fit about0
/          rms     : rms wrt line
/
/ return   0L      : successful fit
/          1L      : insufficients points for fit
<-----------------------------------------------------------------------------*/
{
  double sx2,sx,sum,sxy,sy,sy2;
  double det;

  long   i;


  if (n <= 1) 
  {
    *rms     = 0.0;
    *rms0    = 0.0;
    *a       = 0.0;
    *b       = 0.0;
    *a_sigma = 0.0;
    *b_sigma = 0.0;
    return 1L;
  }

  sx2 = 0.0;
  sx  = 0.0;
  sum = 0.0;
  sxy = 0.0;
  sy  = 0.0;
  sy2 = 0.0;

  loop(i,n)
  {
    sx2 +=  (x[i]-x[0])*(x[i]-x[0]);
    sx  +=  x[i]-x[0];
    sum +=  1.0;
    sxy +=  (x[i]-x[0])*(y[i]-y[0]);
    sy  +=  y[i]-y[0];
    sy2 +=  (y[i]-y[0])*(y[i]-y[0]);
  }

  det  = sx2*sum - sx*sx;

  *a    = (sxy*sum - sy*sx )/det;
  *b    = (sx2*sy  - sx*sxy)/det;

  *b    = y[0] - *a * x[0] + *b;

  /* compute linear predicted RMS */

  *rms0 = 0.0;
  *rms  = 0.0;

  if (n != 2) 
  {
    loop(i,n)
    {
      *rms0 += y[i]*y[i];
      *rms  += (y[i] - *a*x[i] - *b)*(y[i] - *a*x[i] - *b);
    } 

    *rms0 = sqrt(*rms0/(double)n);
    *rms  = sqrt(*rms/(double)n);
  }
  else
  {
    *rms0 = 0.0;
    *rms  = 0.0;
  }


  *a_sigma = sqrt(sum/det);
  *b_sigma = sqrt(sx2/det);

  return 0L;
}
