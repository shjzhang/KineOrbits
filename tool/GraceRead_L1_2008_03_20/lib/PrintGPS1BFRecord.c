#include <string.h>
#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


static char SccsId[] = "$Id: PrintGPS1BFRecord.c,v 1.2 2004/08/30 21:03:34 wib Exp $";


boolean PrintGPS1BFRecord(FILE *dst, GPS1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Print a detailed ascii description of GPS 1B Flight Data Format 
/          record to file pointer dst
/
/ input:  *dst    pointer to GPS 1B Flight Data Format File
/         *record Pointer to GPS 1B Flight Data struct (GPS1B_t)
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
