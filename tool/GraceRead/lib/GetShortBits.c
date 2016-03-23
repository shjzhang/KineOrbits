#include "GRACEiolib.h"

#define NBITSMAX 16

static char SccsId[] = "$Id: GetShortBits.c,v 1.2 2004/08/30 21:03:33 wib Exp $";

void GetShortBits(short value, char *bits)
/*----------------------------------------------------------------------------->
/ purpose:  return bits in short value
/
/ coded by: Gerhard L.H. Kruizinga                11/13/98
/
<-----------------------------------------------------------------------------*/
{
  int i;

  for ( i = 15 ; i >= 0; i-- )
  {
    bits[i] = (value & (1<<i)) ? 1 : 0;
  }
}
