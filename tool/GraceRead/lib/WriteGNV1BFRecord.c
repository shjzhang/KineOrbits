#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


#define Failure 0

static char SccsId[] = "$Id: WriteGNV1BFRecord.c,v 1.6 2004/08/30 21:03:38 wib Exp $";


boolean WriteGNV1BFRecord(FILE *dst, GNV1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: write GPS Navigation Level 1B Data Format record to file pointer dst
/
/ coded by: J. E. Patterson                          06/14/00
/
/ input:  *dst    Pointer to GNV1B Data Format File
/         *record Pointer to GNV1B Data struct (GNV1B_t)
<-----------------------------------------------------------------------------*/
{

 
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/


  if (fwrite_grace(&record->gps_time,sizeof(record->gps_time),1,dst) != 1)
  {
    printf("GNV1B: Error writing field 'gps_time' \n");
    return Failure;
  }

  if (fwrite_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,dst) != 1)
  {
    printf("GNV1B: Error writing field 'GRACE_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->coord_ref,sizeof(record->coord_ref),1,dst) != 1)
  {
    printf("GNV1B: Error writing field 'coord_ref' \n");
    return Failure;
  }

  if (fwrite_grace(&record->xpos,sizeof(record->xpos),1,dst) != 1)
  {
    printf("GNV1B: Error writing field 'xpos' \n");
    return Failure;
  }

  if (fwrite_grace(&record->ypos,sizeof(record->ypos),1,dst) != 1)
  {
    printf("GNV1B: Error writing field 'ypos' \n");
    return Failure;
  }

  if (fwrite_grace(&record->zpos,sizeof(record->zpos),1,dst) != 1)
  {
    printf("GNV1B: Error writing field 'zpos' \n");
    return Failure;
  }

  if (fwrite_grace(&record->xpos_err,sizeof(record->xpos_err),1,dst) != 1)
  {
    printf("GNV1B: Error writing field 'xpos_err' \n");
    return Failure;
  }

  if (fwrite_grace(&record->ypos_err,sizeof(record->ypos_err),1,dst) != 1)
  {
    printf("GNV1B: Error writing field 'ypos_err' \n");
    return Failure;
  }

  if (fwrite_grace(&record->zpos_err,sizeof(record->zpos_err),1,dst) != 1)
  {
    printf("GNV1B: Error writing field 'zpos_err' \n");
    return Failure;
  }

  if (fwrite_grace(&record->xvel,sizeof(record->xvel),1,dst) != 1)
  {
    printf("GNV1B: Error writing field 'xvel' \n");
    return Failure;
  }

  if (fwrite_grace(&record->yvel,sizeof(record->yvel),1,dst) != 1)
  {
    printf("GNV1B: Error writing field 'yvel' \n");
    return Failure;
  }

  if (fwrite_grace(&record->zvel,sizeof(record->zvel),1,dst) != 1)
  {
    printf("GNV1B: Error writing field 'zvel' \n");
    return Failure;
  }

  if (fwrite_grace(&record->xvel_err,sizeof(record->xvel_err),1,dst) != 1)
  {
    printf("GNV1B: Error writing field 'xvel_err' \n");
    return Failure;
  }

  if (fwrite_grace(&record->yvel_err,sizeof(record->yvel_err),1,dst) != 1)
  {
    printf("GNV1B: Error writing field 'yvel_err' \n");
    return Failure;
  }

  if (fwrite_grace(&record->zvel_err,sizeof(record->zvel_err),1,dst) != 1)
  {
    printf("GNV1B: Error writing field 'zvel_err' \n");
    return Failure;
  }

  if (fwrite_grace(&record->qualflg,sizeof(record->qualflg),1,dst) != 1)
  {
    printf("GNV1B: Error writing field 'qualflg' \n");
    return Failure;
  }


  return True;
}
