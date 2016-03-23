#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

static char SccsId[] = "$Id: PrintPCI1AFRecord.c,v 1.2 2004/08/30 21:03:35 wib Exp $";

void PrintPCI1AFRecord(FILE *dst, PCI1A_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Print a detailed ascii description of PCI Level 1A Data Format 
/          record to file pointer dst
/
/ coded by: Gerhard Kruizinga                        03/18/03
/
/ input:  *dst    Pointer to PCI Level 1A Flight Data Format File
/         *record Pointer to PCI Level 1A Flight Data struct (PCI1A_t)
<-----------------------------------------------------------------------------*/
{

 
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/
  fprintf(dst," %-20s = %ld\n","record->gps_time",record->gps_time);
  fprintf(dst," %-20s = %c\n","record->GRACE_id",record->GRACE_id);
  fprintf(dst," %-20s = %le\n","record->ant_centr_corr",record->ant_centr_corr);
  fprintf(dst," %-20s = %le\n","record->ant_centr_rate",record->ant_centr_rate);
  fprintf(dst," %-20s = %le\n","record->ant_centr_accl",record->ant_centr_accl);
  fprintf(dst," %-20s = %d\n","record->qualflg",record->qualflg);
}
