#include <stdio.h>
#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Success 1
#define Failure 0
#define MAXCHAR 1000

static char SccsId[] = "$Id: WriteILG1XFRecord.c,v 1.2 2004/08/30 21:03:38 wib Exp $";

boolean WriteILG1XFRecord(FILE *dst, ILG1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: write an IPU log message file to file pointer dst
/
/ coded by: Gerhard L.H. Kruizinga 10/15/00
/
/ input:  *dst    pointer to ILG1X Data Format File
/ output: *record Pointer to ILG1X Data struct (ILG1X_t)
/
/ return:      1       normal return
/              0       End Of File reached
<-----------------------------------------------------------------------------*/
{


  fprintf(dst,"%d %d %c >%s\n",record->rcv_time, record->pkt_count,
                              record->GRACE_id,record->logpacket);
  
  return True;

}
