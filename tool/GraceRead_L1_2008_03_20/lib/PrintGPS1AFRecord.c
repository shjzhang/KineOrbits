#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


static char SccsId[] = "$Id: PrintGPS1AFRecord.c,v 1.2 2004/08/30 21:03:34 wib Exp $";


boolean PrintGPS1AFRecord(FILE *dst, GPS1A_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Print a detailed ascii description of GPS 1A Flight Data Format 
/          record to file pointer dst
/
/ input:  *dst    pointer to GPS 1A Flight Data Format File
/         *record Pointer to GPS 1A Flight Data struct (GPS1A_t)
<-----------------------------------------------------------------------------*/
{

  if (PrintGFD1X(dst, record))
  {
    return True;
  }
  else
  {
    return False;
  }
}
