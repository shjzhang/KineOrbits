#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


static char SccsId[] = "$Id: WrAsciiGPS1BFRecord.c,v 1.2 2004/08/30 21:03:37 wib Exp $";


boolean WrAsciiGPS1BFRecord(FILE *dst, GPS1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Dump ascii records of flight data from the file pointed to by dst 
/
/ coded by: J. E. Patterson                         07/18/00
/
/ input:  *dst    Pointer to GPS1B Data Format File
/         *record Pointer to GPS1B Data struct (GPS1B_t)
<-----------------------------------------------------------------------------*/
{

  if (WrAsciiGFD1XFRecord(dst, record))
  {
    return True;
  }
  else
  {
    return False;
  }
 
}
