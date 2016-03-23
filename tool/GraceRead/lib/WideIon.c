#include <stdio.h>
#include <string.h>
#include <math.h>
#include "GRACEiolib.h"
#include "GRACEgpslib.h"

static char SccsId[] = "$Id: WideIon.c,v 1.2 2005/12/27 19:46:56 wib Exp $";

void WideIon (double f1, double f2, double L1, double L2, double *Ion)
/*----------------------------------------------------------------------------->
/ purpose: Form (L1-L2)*coeff/lambda_w combination of GPS data.
/          Works for both carrier phase and pseudorange data provided
/          units are in LENGTH.
/
/ coded by: G.L.H. Kruizinga        04/09/01
/
/ based on wideion.f by Geoffrey Blewitt
/
/ input:  f1      double  frequency of L1 (Hz)
/         f2      double  freqenncy of L2 (Hz)
/         L1      double  L1 oberservable (units of length, meters)
/         L2      double  L2 oberservable (units of length, meters)
/ output: Ion     double* (L1-L2)*coeff/lambda_w (units of length cycles)
/
/-----------------------------------------------------------------------------*/
{
  double freq_diff,wlambda,coeff; 

  freq_diff = f1-f2;
  wlambda   = 1e3*CSPEED/freq_diff; /* meters */
  coeff     = 1.00/(f1/f2-f2/f1);

  *Ion = (L1-L2)*coeff/wlambda;

}
