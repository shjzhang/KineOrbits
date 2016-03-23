#include <stdio.h>
#include <string.h>
#include <math.h>
#include "GRACEiolib.h"
#include "GRACEgpslib.h"

static char SccsId[] = "$Id: ComputeLC_PC.c,v 1.2 2004/08/30 21:03:32 wib Exp $";

void ComputeLC_PC (double f1, double f2, double L1, double L2, double L1sigma,
                   double L2sigma, double *LC, double *LCsigma)
/*----------------------------------------------------------------------------->
/ purpose: Form ionosphere-free linear combination of GPS data.
/          Works for both carrier phase and pseudorange data provided
/          units are in LENGTH.
/
/ coded by: G.L.H. Kruizinga        04/09/01
/
/ based on combfree.f by Geoffrey Blewitt
/
/ input:  f1      double  frequency of L1 (Hz)
/         f2      double  freqenncy of L2 (Hz)
/         L1      double  L1 oberservable (units of length)
/         L2      double  L2 oberservable (units of length)
/         L1sigma double  sigma of L1 oberservable (units of length)
/         L2sigma double  sigma of L2 oberservable (units of length)
/ output: LC      double* Linear combined observable (units of length)
/         LCsigma double* sigma of Linear combined observable (units of length)
/
/-----------------------------------------------------------------------------*/
{
  double fratio,a1,a2,tmp; 

  fratio = f1/f2;
  a2 = 1.0/(1.0 - fratio*fratio);
  a1 = -a2 + 1.0;

  *LC = a1*L1 + a2*L2;

  tmp = a1*L1sigma*a1*L1sigma +a2*L2sigma*a2*L2sigma;

  if (tmp > 1e-10)
   *LCsigma = sqrt(tmp);
  else
   *LCsigma = 1e-2;
}
