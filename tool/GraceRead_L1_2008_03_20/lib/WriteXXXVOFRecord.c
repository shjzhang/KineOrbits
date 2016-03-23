#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0


static char SccsId[] = "$Id: WriteXXXVOFRecord.c,v 1.5 2004/08/30 21:03:39 wib Exp $";



boolean WriteXXXVOFRecord(FILE *dst, XXXVO_t *record)
/*----------------------------------------------------------------------------->
/ purpose: write Vector Orientation Data Format record to file pointer dst
/
/ coded by: J. E. Patterson                  06/21/00
/
/ modified name of routine and structure     02/15/01
/
/ input:  *dst    Pointer to XXXVO Data Format File
/         *record Pointer to XXXVO Data struct (XXXVO_t)
<-----------------------------------------------------------------------------*/
{

 
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/
  if (fwrite_grace(&record->gps_time,sizeof(record->gps_time),1,dst) != 1)
  {
    printf("XXXVO: Error writing field 'gps_time' \n");
    return Failure;
  }

  if (fwrite_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,dst) != 1)
  {
    printf("XXXVO: Error writing field 'GRACE_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->mag,sizeof(record->mag),1,dst) != 1)
  {
    printf("XXXVO: Error writing field 'mag' \n");
    return Failure;
  }

  if (fwrite_grace(&record->cosx,sizeof(record->cosx),1,dst) != 1)
  {
    printf("XXXVO: Error writing field 'cosx' \n");
    return Failure;
  }

  if (fwrite_grace(&record->cosy,sizeof(record->cosy),1,dst) != 1)
  {
    printf("XXXVO: Error writing field 'cosy' \n");
    return Failure;
  }

  if (fwrite_grace(&record->cosz,sizeof(record->cosz),1,dst) != 1)
  {
    printf("XXXVO: Error writing field 'cosz' \n");
    return Failure;
  }

  if (fwrite_grace(&record->qualflg,sizeof(record->qualflg),1,dst) != 1)
  {
    printf("XXXVO: Error writing field 'qualflg' \n");
    return Failure;
  }

  return True;
}
