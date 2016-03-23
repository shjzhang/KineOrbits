#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0

static char SccsId[] = "$Id: WriteACC1BFRecord.c,v 1.6 2004/08/30 21:03:38 wib Exp $";



boolean WriteACC1BFRecord(FILE *dst, ACC1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: write Accelerometer Level 1B Data Format record to file pointer dst
/
/ coded by: J. E. Patterson                  06/21/00
/ modified: Gerhard L.H. Kruizinga                  01/03/02
/
/ input:  *dst    Pointer to ACC1B Data Format File
/         *record Pointer to ACC1B Data struct (ACC1B_t)
<-----------------------------------------------------------------------------*/
{

 
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/


  if (fwrite_grace(&record->gps_time,sizeof(record->gps_time),1,dst) != 1)
  {
    printf("ACC1B: Error writing field 'gps_time' \n");
    return Failure;
  }

  if (fwrite_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,dst) != 1)
  {
    printf("ACC1B: Error writing field 'GRACE_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->lin_accl_x,sizeof(record->lin_accl_x),1,dst) != 1)
  {
    printf("ACC1B: Error writing field 'lin_accl_x' \n");
    return Failure;
  }

  if (fwrite_grace(&record->lin_accl_y,sizeof(record->lin_accl_y),1,dst) != 1)
  {
    printf("ACC1B: Error writing field 'lin_accl_y' \n");
    return Failure;
  }

  if (fwrite_grace(&record->lin_accl_z,sizeof(record->lin_accl_z),1,dst) != 1)
  {
    printf("ACC1B: Error writing field 'lin_accl_z' \n");
    return Failure;
  }

  if (fwrite_grace(&record->ang_accl_x,sizeof(record->ang_accl_x),1,dst) != 1)
  {
    printf("ACC1B: Error writing field 'ang_accl_x' \n");
    return Failure;
  }

  if (fwrite_grace(&record->ang_accl_y,sizeof(record->ang_accl_y),1,dst) != 1)
  {
    printf("ACC1B: Error writing field 'ang_accl_y' \n");
    return Failure;
  }

  if (fwrite_grace(&record->ang_accl_z,sizeof(record->ang_accl_z),1,dst) != 1)
  {
    printf("ACC1B: Error writing field 'ang_accl_z' \n");
    return Failure;
  }

  if (fwrite_grace(&record->acl_x_res,sizeof(record->acl_x_res),1,dst) != 1)
  {
    printf("ACC1B: Error writing field 'acl_x_res' \n");
    return Failure;
  }

  if (fwrite_grace(&record->acl_y_res,sizeof(record->acl_y_res),1,dst) != 1)
  {
    printf("ACC1B: Error writing field 'acl_y_res' \n");
    return Failure;
  }

  if (fwrite_grace(&record->acl_z_res,sizeof(record->acl_z_res),1,dst) != 1)
  {
    printf("ACC1B: Error writing field 'acl_z_res' \n");
    return Failure;
  }


  if (fwrite_grace(&record->qualflg,sizeof(record->qualflg),1,dst) != 1)
  {
    printf("ACC1B: Error writing field 'qualflg' \n");
    return Failure;
  }


  return True;
}
