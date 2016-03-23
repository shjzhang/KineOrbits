#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0

static char SccsId[] = "$Id: WriteIOA1BFRecord.c,v 1.5 2004/08/30 21:03:38 wib Exp $";


boolean WriteIOA1BFRecord(FILE *dst, IOA1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: write the Inerital Orientation of the ACC Data Format record 
/          to file pointer dst
/
/ coded by: J. E. Patterson                  06/21/00
/
/ modified name of routine and structure     02/15/01
/
/ input:  *dst    Pointer to IOA1B Data Format File
/         *record Pointer to IOA1B Data struct (IOA1B_t)
<-----------------------------------------------------------------------------*/
{

 
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/
  if (fwrite_grace(&record->gps_time,sizeof(record->gps_time),1,dst) != 1)
  {
    printf("IOA1B: Error writing field 'gps_time' \n");
    return Failure;
  }

  if (fwrite_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,dst) != 1)
  {
    printf("IOA1B: Error writing field 'GRACE_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->quatangle,sizeof(record->quatangle),1,dst) != 1)
  {
    printf("IOA1B: Error writing field 'quatangle' \n");
    return Failure;
  }

  if (fwrite_grace(&record->quaticoeff,sizeof(record->quaticoeff),1,dst) != 1)
  {
    printf("IOA1B: Error writing field 'quaticoeff' \n");
    return Failure;
  }

  if (fwrite_grace(&record->quatjcoeff,sizeof(record->quatjcoeff),1,dst) != 1)
  {
    printf("IOA1B: Error writing field 'quatjcoeff' \n");
    return Failure;
  }

  if (fwrite_grace(&record->quatkcoeff,sizeof(record->quatkcoeff),1,dst) != 1)
  {
    printf("IOA1B: Error writing field 'quatkcoefff' \n");
    return Failure;
  }

  if (fwrite_grace(&record->qualflg,sizeof(record->qualflg),1,dst) != 1)
  {
    printf("IOA1B: Error writing field 'qualflg' \n");
    return Failure;
  }


  return True;
}
