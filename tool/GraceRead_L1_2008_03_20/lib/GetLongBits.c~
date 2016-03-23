#include "GRACEiolib.h"

#define NBITSMAX 32 

static char SccsId[] = "$Id: GetLongBits.c,v 1.2 2004/08/30 21:03:33 wib Exp $";

void GetLongBits(long value, char *bits)
/*----------------------------------------------------------------------------->
/ purpose:  return bits in long value
/
/ coded by: Gerhard L.H. Kruizinga                11/13/98
/ borrowed from Gerhard and modified by Jean      06/22/00
/
<-----------------------------------------------------------------------------*/
{
  int i;

  for ( i = 31 ; i >= 0; i-- )
  {
    bits[i] = (value & (1<<i)) ? 1 : 0;
  }
}
