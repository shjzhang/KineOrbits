#include "GRACEiolib.h"

static char SccsId[] = "$Id: GetCharBits.c,v 1.2 2004/08/30 21:03:33 wib Exp $";

void GetCharBits(unsigned char value, char *bits)
/*----------------------------------------------------------------------------->
/ purpose:  return bits in short value
/
/ coded by: Gerhard L.H. Kruizinga                11/13/98
/
<-----------------------------------------------------------------------------*/
{
  int i;

  for ( i = 7 ; i >= 0; i-- )
  {
    bits[i] = (value & (1<<i)) ? 1 : 0;
  }
}
