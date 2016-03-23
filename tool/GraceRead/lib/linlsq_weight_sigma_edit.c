#include <stdio.h>
#include "GRACEiolib.h"
#include "GRACEprototype.h"

#define NITERMAX 10

static char SccsId[] = "@(#) linlsq_weight_sigma_edit.c       1.2 01/15/03";

long linlsq_weight_sigma_edit(
                   double *x, double *y, double *w, double *a, double *b, 
                   double *a_sigma, double *b_sigma, double *rms, double *rms0,
                   long n, double SigmaFactor,long *niterations,long *nedit)
/*----------------------------------------------------------------------------->
/ purpose: compute leastsquares fit of a straight line
/          (y = ax + b) to the data set x,y and weight observation w
/          also iterate on solution by Sigma editing the fit residuals
/
/ coded by: Gerard L.H. Kruizinga           05/03/94
/ converted to c                            09/09/01
/ added observation weights                 02/12/02
/  
/ input:
/          x           : array containing x-coordinates
/          y           : array containing y-coordinates
/          y           : array containing respective weight of y obs
/          SigmaFactor : SigmaFactor to be used in sigma editing
/          n           : dimension of array
/
/ output:
/          a           : slope of fit
/          b           : bias of fit
/          rms0        : rms of fit about0
/          rms         : rms wrt line
/          niterations : number of iterations for sigma editing
/          nedit       : number of points editted
/
/ return   0L          : successful fit
/          1L          : insufficients points for fit
/          NITERMAX    : iterations exceeds NITERMAX
<-----------------------------------------------------------------------------*/
{
  double sx2,sx,sum,sxy,sy,sy2;
  double det,res;

  char   *edit;
  long   i,niter,npoints,nedit_curr,nedit_prev;

  /*>>>> set default solution values<<<<*/

  *rms     = 0.0;
  *rms0    = 0.0;
  *a       = 0.0;
  *b       = 0.0;
  *a_sigma = 0.0;
  *b_sigma = 0.0;
  
  niter      =  0;
  nedit_prev = -1;
  nedit_curr = -2;

  /*>>>> check number of points in fit<<<<*/

  if (n <= 1) return 1L;

  /*>>>> allocate memory for edit array<<<<*/

  edit = (char *) malloc((size_t)n*sizeof(char));

  loop(i,n) edit[i] = 0;

  while (nedit_curr != nedit_prev && niter < NITERMAX)
  {

    sx2     = 0.0;
    sx      = 0.0;
    sum     = 0.0;
    sxy     = 0.0;
    sy      = 0.0;
    sy2     = 0.0;

    npoints = n;

    loop(i,n)
    {
      if (edit[i]) {npoints--;continue;}

      sx2 +=  w[i]*(x[i]-x[0])*(x[i]-x[0]);
      sx  +=  w[i]*(x[i]-x[0]);
      sum +=  w[i];
      sxy +=  w[i]*(x[i]-x[0])*(y[i]-y[0]);
      sy  +=  w[i]*(y[i]-y[0]);
      sy2 +=  w[i]*(y[i]-y[0])*(y[i]-y[0]);
      
    }

    if (npoints < 2)
    {
      *rms     = 0.0;
      *rms0    = 0.0;
      *a       = 0.0;
      *b       = 0.0;
      *a_sigma = 0.0;
      *b_sigma = 0.0;
      *niterations = niter;
      *nedit       = nedit_curr;
      free(edit);
      return 1L;
    }

/*
    fprintf(stdout," sx2 = %.16g\n",sx2);
    fprintf(stdout," sx  = %.16g\n",sx );
    fprintf(stdout," sum = %.16g\n",sum);
    fprintf(stdout," sxy = %.16g\n",sxy);
    fprintf(stdout," sy  = %.16g\n",sy );
    fprintf(stdout," sy2 = %.16g\n",sy2);
*/

    det  = sx2*sum - sx*sx;

    *a    = (sxy*sum - sy*sx )/det;
    *b    = (sx2*sy  - sx*sxy)/det;

    *b    = y[0] - *a * x[0] + *b;

    *a_sigma = sqrt(sum/det);
    *b_sigma = sqrt(sx2/det);

    /* compute linear predicted RMS */

    *rms0 = 0.0;
    *rms  = 0.0;

    if (npoints >= 2) 
    {
      loop(i,n)
      {
        if (edit[i]) continue;
        *rms0 += y[i]*y[i];
        *rms  += (y[i] - *a*x[i] - *b)*(y[i] - *a*x[i] - *b);
      } 

      *rms0 = sqrt(*rms0/(double)n);
      *rms  = sqrt(*rms/(double)n);

      /*>>>> reset edit array <<<<*/

      nedit_prev = nedit_curr;
      nedit_curr = 0;

      loop(i,n)
      {
        res = y[i] - *a*x[i] - *b;
        edit[i] = 0;
        if (fabs(res) > SigmaFactor* (*rms)) {edit[i] = 1,nedit_curr++;}
      }
    }
    else
    {
      *rms0 = 0.0;
      *rms  = 0.0;
    }



    niter++;
    if (niter > NITERMAX) 
    {
      free (edit);
      return NITERMAX;
    }
  }

  *niterations = niter;
  *nedit       = nedit_curr;

  free(edit);

  return 0L;
}
