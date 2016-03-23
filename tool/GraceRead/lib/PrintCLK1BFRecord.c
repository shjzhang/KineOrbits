#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


static char SccsId[] = "$Id: PrintCLK1BFRecord.c,v 1.6 2004/08/30 21:03:34 wib Exp $";

void PrintCLK1BFRecord(FILE *dst, CLK1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Print a detailed ascii description of Clock offset Level 1B 
/          Data Format record to file pointer dst
/
/ coded by: J. E. Patterson                         06/21/00
/
/ input:  *dst    Pointer to CLK1B Data Format File
/         *record Pointer to CLK1B Data struct (CLK1B_t)
<-----------------------------------------------------------------------------*/
{
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/
  fprintf(dst," %-20s = %ld\n","record->rcv_time",record->rcv_time);
  fprintf(dst," %-20s = %c\n","record->GRACE_id",record->GRACE_id);
  fprintf(dst," %-20s = %d\n","record->clock_id",record->clock_id);
  fprintf(dst," %-20s = %le\n","record->eps_time",record->eps_time);
  fprintf(dst," %-20s = %le\n","record->eps_err",record->eps_err);
  fprintf(dst," %-20s = %le\n","record->eps_drift",record->eps_drift);
  fprintf(dst," %-20s = %le\n","record->drift_err",record->drift_err);
  fprintf(dst," %-20s = %d\n","record->qualflg",record->qualflg);


}
