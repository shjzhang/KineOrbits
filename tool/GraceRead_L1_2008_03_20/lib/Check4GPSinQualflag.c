#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"
#include "GRACEio_utils.h"
#include <string.h>

static char SccsId[] = "$Id: Check4GPSinQualflag.c,v 1.2 2004/08/30 21:03:33 wib Exp $";

long Check4GPSinQualflag(unsigned char qualflg)
/*----------------------------------------------------------------------------->
/ purpose:  Return if time in qualflag is GPS time
/
/ coded by: Gerhard L.H. Kruizinga                10/08/01
/
/ return 1L if bit 0 = 0 (GPS time)
/        0L if bit 1 = 1 (Space Craft Elapsed Time SCET) 
<-----------------------------------------------------------------------------*/
{
  char           bits8[8];

  GetCharBits(qualflg,bits8);

  if (bits8[0] == 0) return 1L;
  if (bits8[1] == 0) return 0L;
}
