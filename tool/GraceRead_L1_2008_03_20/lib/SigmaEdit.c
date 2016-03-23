#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include "GRACEdefs.h"
#include "GRACEprototype.h"

#define NITERMAX  20
#define loops(A,B,C) for(A=B;A<=C;A++)

static char SccsId[] = "@(#) SigmaEdit.c       1.5 01/15/03";

void SigmaEdit (double *obs, long nobs, double SigmaFactor, double *mean, 
                double *rms   , long *edit, double auxilary[NAUXMAX], 
                double NoValue)

{

/*----------------------------------------------------------------------------->
/   purpose: compute statistics of input array by performing a 
/            SigmaFactor*sigma iteration
/
/   coded by: Gerhard L.H. Kruizinga                    08/07/98
/
/   input:    (double) *obs        input data
/             (long)   nobs        number of input data
/             (double) SigmaFactor factor used in sigma editing
/
/   output:   (double) mean        mean of *obs after sigma editing
/             (double) rms         standard deviation after sigma editing
/             (int)    edit        index array indicating which values are edited
/             (double) auxilary[]  array containing other relevant information
/                                  pertaining to the statistics computation:
/
/             auxilary[IPSMIN]     minimum of obs after sigma editing
/             auxilary[IPSMAX]     maximum of obs after sigma editing
/             auxilary[IPSNOBS]    number of obs after sigma editing
/             auxilary[IPSNITER]   number of iterations to converge sigma edit
/             auxilary[IPSORGMEAN] mean of all obs before sigma editing         
/             auxilary[IPSORGSTDV] standard deviation of all obs before 
/                                  sigma editing         
/             auxilary[IPSORGMIN]  minimum of all obs before sigma editing         
/             auxilary[IPSORGMAX]  maximum of all obs before sigma editing
/             (double) NoValue     value indicating no data in array indexed
  $dx = $x - $avg;
  $avg += $dx/$n;
  $ddx = $x - $avg;
  $ssd += $dx*$ddx;
<-----------------------------------------------------------------------------*/

  double  tmin,tmax;
  double  dx,ddx,avg,ssd;

  long    ndx,nedt,ntobs,niter,NobsCount;

/*----------------------------------------------------------------------------->
/ First pass thru the data
<-----------------------------------------------------------------------------*/
  
  niter     = 0;

  avg       = 0.0;
  ssd       = 0.0;
  tmin      = 1.0e99;
  tmax      =-1.0e99;
  NobsCount = 0;

  loops (ndx,0,nobs-1)
  {
    if (obs[ndx] == NoValue) continue;
    NobsCount++;

    dx    = obs[ndx] - avg;
    avg  += dx/(double)NobsCount;
    ddx   = obs[ndx] -avg;
    ssd  += dx*ddx;

    if (obs[ndx] < tmin) tmin = obs[ndx];
    if (obs[ndx] > tmax) tmax = obs[ndx];
  }

  ssd   = sqrt(fabs(ssd/(double)NobsCount));

  auxilary[IPSORGMEAN] = avg;
  auxilary[IPSORGSTDV] = ssd;
  auxilary[IPSORGMIN]  = tmin;
  auxilary[IPSORGMAX]  = tmax;
  auxilary[IPSORGNOBS] = NobsCount;

/*----------------------------------------------------------------------------->
/ Flag all data that fail the SigmaFactor*RMS criteria
<-----------------------------------------------------------------------------*/


  nedt = 0;

  loops (ndx,0,nobs-1)
  {
    edit[ndx] = 0;
    if (obs[ndx] == NoValue) {edit[ndx] = EDITFLAG; continue;}
    if (fabs(obs[ndx]-avg) > SigmaFactor*ssd) {edit[ndx] = EDITFLAG; nedt++;}
  }

/*----------------------------------------------------------------------------->
/ Check if Iteration is required, if not set appropriate values and return
/ Note: iteration not required if rms of original data is zero 
<-----------------------------------------------------------------------------*/

  if (nedt == 0 || ssd == 0.0) 
  {
    *mean              = avg;
    *rms               = ssd;
    auxilary[IPSNOBS]  = (double) NobsCount;
    auxilary[IPSNITER] = (double) niter;
    auxilary[IPSMIN]   = tmin;
    auxilary[IPSMAX]   = tmax;
    return;
  }

/*----------------------------------------------------------------------------->
/ Iterate until statistics converge
<-----------------------------------------------------------------------------*/

  while (nedt !=0 && niter < NITERMAX)
  {
    niter++;

/*----------------------------------------------------------------------------->
/   Reaccumulate statistics
<-----------------------------------------------------------------------------*/

    avg       = 0.0;
    ssd       = 0.0;
    tmin  = 1.0e99;
    tmax  =-1.0e99;
    ntobs = 0;
    nedt  = 0;
    
    loops (ndx,0,nobs-1)
    {
      if (edit[ndx] == EDITFLAG) continue;
      ntobs++;
      dx    = obs[ndx] - avg;
      avg  += dx/(double)ntobs;
      ddx   = obs[ndx] -avg;
      ssd  += dx*ddx;

      if (obs[ndx] < tmin) tmin = obs[ndx];
      if (obs[ndx] > tmax) tmax = obs[ndx];
    }

    ssd   = sqrt(fabs(ssd/(double)ntobs));

/*
  fprintf(stdout,"avg,ssd,min,max,count %.16g %.16g %.16g %.16g %d\n",
                  avg,ssd,tmin,tmax,ntobs);
*/


/*----------------------------------------------------------------------------->
/   Reflag data
<-----------------------------------------------------------------------------*/

    loops (ndx,0,nobs-1)
    {
      if (edit[ndx] == EDITFLAG) continue; 
      if (fabs(obs[ndx]-avg) > SigmaFactor*ssd) {edit[ndx]=EDITFLAG; nedt++;}
    }

  }

  *mean              = avg;
  *rms               = ssd;
  auxilary[IPSNOBS]  = (double) ntobs;
  auxilary[IPSNITER] = (double) niter;
  auxilary[IPSMIN]   = tmin;
  auxilary[IPSMAX]   = tmax;
   
}
