#include "GRACEiolib.h"
#include "GRACEio_utils.h"
#include "GRACEio_prototypes.h"

#define Failure 0


#define NBITSMAX 16

static char SccsId[] = "$Id: WriteAHK1XFRecord.c,v 1.3 2004/08/30 21:03:38 wib Exp $";

boolean WriteAHK1XFRecord(FILE *dst, AHK1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: write level1a ACC Housekeeping dat to dst file 
/          using generic routine WriteACC1AFRecord.c
/
/ input:  *dst    pointer to GPS Flight Data Format File
/ output: *record Pointer to AHK struct (AHK1X_t)
/
/ return:      1       normal return
/              0       End Of File reached
<-----------------------------------------------------------------------------*/
{

  if (WriteACC1AFRecord(dst, record))
  {
    return True;
  }
  else
  {
    return False;
  }
}
