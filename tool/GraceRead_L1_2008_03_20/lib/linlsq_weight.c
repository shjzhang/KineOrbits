#include <stdio.h>
#include "GRACEiolib.h"
#include "GRACEprototype.h"

static char SccsId[] = "@(#) linlsq_weight.c       1.3 02/16/02";

long linlsq_weight(double *x, double *y, double *w, double *a, double *b, 
                   double *a_sigma, double *b_sigma, double *rms, double *rms0,
                   long n)
/*----------------------------------------------------------------------------->
/ purpose: compute leastsquares fit of a straight line
/          (y = ax + b) to the data set x,y and weight observation w
/
/ coded by: Gerard L.H. Kruizinga           05/03/94
/ converted to c                            09/09/01
/ added observation weights                 02/12/02
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
    sx2 +=  w[i]*(x[i]-x[0])*(x[i]-x[0]);
    sx  +=  w[i]*(x[i]-x[0]);
    sum +=  w[i];
    sxy +=  w[i]*(x[i]-x[0])*(y[i]-y[0]);
    sy  +=  w[i]*(y[i]-y[0]);
    sy2 +=  w[i]*(y[i]-y[0])*(y[i]-y[0]);
  }

  fprintf(stdout," sx2 = %.16g\n",sx2);
  fprintf(stdout," sx  = %.16g\n",sx );
  fprintf(stdout," sum = %.16g\n",sum);
  fprintf(stdout," sxy = %.16g\n",sxy);
  fprintf(stdout," sy  = %.16g\n",sy );
  fprintf(stdout," sy2 = %.16g\n",sy2);

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
