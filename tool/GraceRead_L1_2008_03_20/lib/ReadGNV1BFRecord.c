#include <string.h>
#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0

static char SccsId[] = "$Id: ReadGNV1BFRecord.c,v 1.6 2004/08/30 21:03:36 wib Exp $";


boolean ReadGNV1BFRecord(FILE *src, GNV1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: read GPS Navigation Level 1B Data Format record from file pointer src
/
/ coded by: J. E. Patterson                         06/14/00
/
/ input:  *src    Pointer to GNV1B Data Format File
/ output: *record Pointer to GNV1B Data struct (GNV1B_t)
/
/ return:      1       normal return
/              0       End Of File reached
<-----------------------------------------------------------------------------*/
{
  unsigned long gps_time;

  int           retrn;


/*----------------------------------------------------------------------------->
/ Test for EOF
<-----------------------------------------------------------------------------*/

  retrn = fread_grace(&gps_time,sizeof(gps_time),1,src);
  if (feof(src) != 0) return False;
  if (retrn != 1)
  {
    printf("GNV1B: Error reading field 'gps_time' \n");
    return Failure;
  }

  record->gps_time = gps_time;
  
/*----------------------------------------------------------------------------->
/ Read Header from src
<-----------------------------------------------------------------------------*/
  if (fread_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,src) != 1)
  {
    printf("GNV1BF: Error reading field 'GRACE_id' \n");
    return Failure;
  }

  if (fread_grace(&record->coord_ref,sizeof(record->coord_ref),1,src) != 1)
  {
    printf("GNV1BF: Error reading field 'coord_ref' \n");
    return Failure;
  }

  if (fread_grace(&record->xpos,sizeof(record->xpos),1,src) != 1)
  {
    printf("GNV1BF: Error reading field 'xpos' \n");
    return Failure;
  }

  if (fread_grace(&record->ypos,sizeof(record->ypos),1,src) != 1)
  {
    printf("GNV1BF: Error reading field 'ypos' \n");
    return Failure;
  }

  if (fread_grace(&record->zpos,sizeof(record->zpos),1,src) != 1)
  {
    printf("GNV1BF: Error reading field 'zpos' \n");
    return Failure;
  }

  if (fread_grace(&record->xpos_err,sizeof(record->xpos_err),1,src) != 1)
  {
    printf("GNV1BF: Error reading field 'xpos_err' \n");
    return Failure;
  }

  if (fread_grace(&record->ypos_err,sizeof(record->ypos_err),1,src) != 1)
  {
    printf("GNV1BF: Error reading field 'ypos_err' \n");
    return Failure;
  }

  if (fread_grace(&record->zpos_err,sizeof(record->zpos_err),1,src) != 1)
  {
    printf("GNV1BF: Error reading field 'zpos_err' \n");
    return Failure;
  }

  if (fread_grace(&record->xvel,sizeof(record->xvel),1,src) != 1)
  {
    printf("GNV1BF: Error reading field 'xvel' \n");
    return Failure;
  }

  if (fread_grace(&record->yvel,sizeof(record->yvel),1,src) != 1)
  {
    printf("GNV1BF: Error reading field 'yvel' \n");
    return Failure;
  }

  if (fread_grace(&record->zvel,sizeof(record->zvel),1,src) != 1)
  {
    printf("GNV1BF: Error reading field 'zvel' \n");
    return Failure;
  }

  if (fread_grace(&record->xvel_err,sizeof(record->xvel_err),1,src) != 1)
  {
    printf("GNV1BF: Error reading field 'xvel_err' \n");
    return Failure;
  }

  if (fread_grace(&record->yvel_err,sizeof(record->yvel_err),1,src) != 1)
  {
    printf("GNV1BF: Error reading field 'yvel_err' \n");
    return Failure;
  }

  if (fread_grace(&record->zvel_err,sizeof(record->zvel_err),1,src) != 1)
  {
    printf("GNV1BF: Error reading field 'zvel_err' \n");
    return Failure;
  }

  if (fread_grace(&record->qualflg,sizeof(record->qualflg),1,src) != 1)
  {
    printf("GNV1BF: Error reading field 'qualflg' \n");
    return Failure;
  }


  return True;

}
