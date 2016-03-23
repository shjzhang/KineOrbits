#include "GRACEiolib.h"
#include "GRACEio_utils.h"

static char SccsId[] = "$Id: SetLongBit.c,v 1.4 2004/08/30 21:03:37 wib Exp $";

void SetLongBit(unsigned long *value, long nth_bit)
/*----------------------------------------------------------------------------->
/ purpose:  set n-th bit to one in value
/
/ coded by: Gerhard L.H. Kruizinga                07/13/00
/ modified: Gerhard L.H. Kruizinga                08/23/00
/
<-----------------------------------------------------------------------------*/
{
  unsigned long x;

  int           px,py,n;

  x  = ~0;
  px = (int) nth_bit;
  py = px;
  n  = 1;

  if (nth_bit < 0 || nth_bit > 31) 
  {
    fprintf(stderr,"\n Invalid bit location in SetLongBit specified = %d \n",
                    nth_bit);
    fprintf(stderr," Bit Range should be 0 <= nth_bit <= 31\n\n");
  }

  if (PutLongBits(x,px,value,py,n) == -1)
  {
    fprintf(stderr,"\n Invalid call to PutLongBits in SetLongBit\n",
                    nth_bit);
    fprintf(stderr," Check call statement !!\n\n");
  }
}
