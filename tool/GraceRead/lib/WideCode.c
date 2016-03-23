#include <stdio.h>
#include <string.h>
#include <math.h>
#include "GRACEiolib.h"
#include "GRACEgpslib.h"

static char SccsId[] = "$Id: WideCode.c,v 1.2 2005/12/27 19:46:56 wib Exp $";

void WideCode (double f1, double f2, double PW, double LW, double *Code)
/*----------------------------------------------------------------------------->
/ purpose: Form (LW-PW)/lambda_w combination of GPS data.
/          Works for both carrier phase and pseudorange data provided
/          units are in LENGTH.
/
/ coded by: G.L.H. Kruizinga        04/09/01
/
/ based on widecode.f by Geoffrey Blewitt
/
/ input:  f1      double  frequency of L1 (Hz)
/         f2      double  freqenncy of L2 (Hz)
/         PW      double  Range Wide Lane (units of length, meters)
/         LW      double  Phase Wide Lane (units of length, meters)
/ output: Code    double* LW-PW/lambda_w (units of cycles)
/
/-----------------------------------------------------------------------------*/
{
  double freq_diff,wlambda; 

  freq_diff = f1-f2;
  wlambda   = 1e3*CSPEED/freq_diff; /* CSPEED km/sec , wlambda unit meters*/

  *Code = (LW-PW)/wlambda;

}
