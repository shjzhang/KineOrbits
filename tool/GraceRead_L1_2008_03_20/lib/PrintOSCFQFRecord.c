#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

static char SccsId[] = "$Id: PrintOSCFQFRecord.c,v 1.6 2004/08/30 21:03:34 wib Exp $";


void PrintOSCFQFRecord(FILE *dst, OSCFQ_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Print a detailed ascii description of Ultra Stable Oscillator 
/          Data Format record to file pointer dst
/
/ coded by: J. E. Patterson                  06/21/00
/
/ modified name of routine and structure     02/15/01
/
/ input:  *dst    Pointer to OSCFQ Data Format File
/         *record Pointer to OSCFQ Data struct (OSCFQ_t)
<-----------------------------------------------------------------------------*/
{

/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/
  fprintf(dst," %-20s = %ld\n","record->gps_time",record->gps_time);
  fprintf(dst," %-20s = %c\n","record->GRACE_id",record->GRACE_id);
  fprintf(dst," %-20s = %d\n","record->uso_id",record->uso_id);
  fprintf(dst," %-20s = %lf\n","record->uso_freq",record->uso_freq);
  fprintf(dst," %-20s = %lf\n","record->K_freq",record->K_freq);
  fprintf(dst," %-20s = %lf\n","record->Ka_freq",record->Ka_freq);
  fprintf(dst," %-20s = %d\n","record->qualflg",record->qualflg);

}
