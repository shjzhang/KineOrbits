#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0

static char SccsId[] = "$Id: WriteOSCFQFRecord.c,v 1.7 2004/08/30 21:03:38 wib Exp $";


boolean WriteOSCFQFRecord(FILE *dst, OSCFQ_t *record)
/*----------------------------------------------------------------------------->
/ purpose: write Ultra Stable Oscillator Data Format record to file pointer dst
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
  if (fwrite_grace(&record->gps_time,sizeof(record->gps_time),1,dst) != 1)
  {
    printf("OSCFQ: Error writing field 'gps_time' \n");
    return Failure;
  }

  if (fwrite_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,dst) != 1)
  {
    printf("OSCFQ: Error writing field 'GRACE_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->uso_id,sizeof(record->uso_id),1,dst) != 1)
  {
    printf("OSCFQ: Error writing field 'uso_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->uso_freq,sizeof(record->uso_freq),1,dst) != 1)
  {
    printf("OSCFQ: Error writing field 'uso_freq' \n");
    return Failure;
  }

  if (fwrite_grace(&record->K_freq,sizeof(record->K_freq),1,dst) != 1)
  {
    printf("OSCFQ: Error writing field 'K_freq' \n");
    return Failure;
  }

  if (fwrite_grace(&record->Ka_freq,sizeof(record->Ka_freq),1,dst) != 1)
  {
    printf("OSCFQ: Error writing field 'Ka_freq' \n");
    return Failure;
  }

  if (fwrite_grace(&record->qualflg,sizeof(record->qualflg),1,dst) != 1)
  {
    printf("OSCFQ: Error writing field 'qualflg' \n");
    return Failure;
  }

  return True;
}
