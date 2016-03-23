#include <stdio.h>
#include <string.h>
#include <math.h>
#include "GRACEiolib.h"
#include "GRACEgpslib.h"

static char SccsId[] = "$Id: ComputePW.c,v 1.1 2005/12/22 22:54:38 wib Exp $";

void ComputePW (double f1, double f2, double P1, double P2, double *LW)
/*----------------------------------------------------------------------------->
/ purpose: Form wide lane linear combination of GPS data.
/          Works for both carrier phase and pseudorange data provided
/          units are in LENGTH.
/
/ coded by: G.L.H. Kruizinga        04/09/01
/
/ based on widecode.f by Geoffrey Blewitt
/
/ input:  f1      double  frequency of P1 (Hz)
/         f2      double  freqenncy of P2 (Hz)
/         P1      double  P1 oberservable (units of length)
/         P2      double  P2 oberservable (units of length)
/ output: LW      double* Linear combined observable (units of length)
/
/-----------------------------------------------------------------------------*/
{
  double freq_sum; 

  freq_sum = f1+f2;

  *LW = (f1*P1 + f2*P2)/freq_sum;

}
