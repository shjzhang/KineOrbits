#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0

static char SccsId[] = "$Id: WritePCI1AFRecord.c,v 1.2 2004/08/30 21:03:38 wib Exp $";

boolean WritePCI1AFRecord(FILE *dst, PCI1A_t *record)
/*----------------------------------------------------------------------------->
/ purpose: write PCI Level 1A Data Format record to file pointer dst
/
/ coded by: J. Patterson                     06/21/00
/
/ input:  *dst    Pointer to PCI Level 1A Data Format File
/         *record Pointer to PCI Level 1A Data struct (PCI1A_t)
<-----------------------------------------------------------------------------*/
{
 
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/
  if (fwrite_grace(&record->gps_time,sizeof(record->gps_time),1,dst) != 1)
  {
    printf("PCI1A: Error writing field 'gps_time' \n");
    return Failure;
  }

  if (fwrite_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,dst) != 1)
  {
    printf("PCI1A: Error writing field 'GRACE_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->ant_centr_corr,sizeof(record->ant_centr_corr),1,dst) != 1)
  {
    printf("PCI1A: Error writing field 'ant_centr_corr' \n");
    return Failure;
  }

  if (fwrite_grace(&record->ant_centr_rate,sizeof(record->ant_centr_rate),1,dst) != 1)
  {
    printf("PCI1A: Error writing field 'ant_centr_rate' \n");
    return Failure;
  }

  if (fwrite_grace(&record->ant_centr_accl,sizeof(record->ant_centr_accl),1,dst) != 1)
  {
    printf("PCI1A: Error writing field 'ant_centr_accl' \n");
    return Failure;
  }

  if (fwrite_grace(&record->qualflg,sizeof(record->qualflg),1,dst) != 1)
  {
    printf("PCI1A: Error writing field 'qualflg' \n");
    return Failure;
  }

  
  return True;
}
