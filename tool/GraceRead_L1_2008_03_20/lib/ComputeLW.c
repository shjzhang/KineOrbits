#include <stdio.h>
#include <string.h>
#include <math.h>
#include "GRACEiolib.h"
#include "GRACEgpslib.h"

static char SccsId[] = "%Z% %M%       %I% %G%";

void ComputeLW (double f1, double f2, double L1, double L2, double *LW)
/*----------------------------------------------------------------------------->
/ purpose: Form wide lane linear combination of GPS data.
/          Works for both carrier phase and pseudorange data provided
/          units are in LENGTH.
/
/ coded by: G.L.H. Kruizinga        04/09/01
/
/ based on widecode.f by Geoffrey Blewitt
/
/ input:  f1      double  frequency of L1 (Hz)
/         f2      double  freqenncy of L2 (Hz)
/         L1      double  L1 oberservable (units of length)
/         L2      double  L2 oberservable (units of length)
/ output: LW      double* Linear combined observable (units of length)
/
/-----------------------------------------------------------------------------*/
{
  double freq_diff; 

  freq_diff = f1-f2;

  *LW = (f1*L1 - f2*L2)/freq_diff;

}
