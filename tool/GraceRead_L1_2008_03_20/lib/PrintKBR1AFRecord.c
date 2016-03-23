#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


static char SccsId[] = "$Id: PrintKBR1AFRecord.c,v 1.2 2004/08/30 21:03:34 wib Exp $";


boolean PrintKBR1AFRecord(FILE *dst, KBR1A_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Print a detailed ascii description of KBR 1A Flight Data Format 
/          record to file pointer dst
/
/ input:  *dst    pointer to KBR 1A Flight Data Format File
/         *record Pointer to KBR 1A Flight Data struct (KBR1A_t)
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
