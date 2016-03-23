#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0


static char SccsId[] = "$Id: ReadACC1BFRecord.c,v 1.6 2004/08/30 21:03:35 wib Exp $";



boolean ReadACC1BFRecord(FILE *src, ACC1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: read Accelerometer Level 1B Data Format record from file 
/          pointer src
/
/ coded by: J. E. Patterson                         07/18/00
/ modified: Gerhard L.H. Kruizinga                  01/03/02
/
/ input:  *src    Pointer to ACC1B Data Format File
/ output: *record Pointer to ACC1B Data struct (ACC1B_t)
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

  retrn = fread_grace(&gps_time,sizeof(record->gps_time),1,src);
  if (feof(src) != 0) return False;
  if (retrn != 1)
  {
    printf("ACC1B: Error reading field 'gps_time' \n");
    return Failure;
  }
  record->gps_time = gps_time;
  
/*----------------------------------------------------------------------------->
/ Read remaining record elements from src
<-----------------------------------------------------------------------------*/
  if (fread_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,src) != 1)
  {
    printf("ACC1B: Error reading field 'GRACE_id' \n");
    return Failure;
  }

  if (fread_grace(&record->lin_accl_x,sizeof(record->lin_accl_x),1,src) != 1)
  {
    printf("ACC1B: Error reading field 'lin_accl_x' \n");
    return Failure;
  }

  if (fread_grace(&record->lin_accl_y,sizeof(record->lin_accl_y),1,src) != 1)
  {
    printf("ACC1B: Error reading field 'lin_accl_y' \n");
    return Failure;
  }

  if (fread_grace(&record->lin_accl_z,sizeof(record->lin_accl_z),1,src) != 1)
  {
    printf("ACC1B: Error reading field 'lin_accl_z' \n");
    return Failure;
  }

  if (fread_grace(&record->ang_accl_x,sizeof(record->ang_accl_x),1,src) != 1)
  {
    printf("ACC1B: Error reading field 'ang_accl_x' \n");
    return Failure;
  }
  
  if (fread_grace(&record->ang_accl_y,sizeof(record->ang_accl_y),1,src) != 1)
  {
    printf("ACC1B: Error reading field 'ang_accl_y' \n");
    return Failure;
  }

  if (fread_grace(&record->ang_accl_z,sizeof(record->ang_accl_z),1,src) != 1)
  {
    printf("ACC1B: Error reading field 'ang_accl_z' \n");
    return Failure;
  }

  if (fread_grace(&record->acl_x_res,sizeof(record->acl_x_res),1,src) != 1)
  {
    printf("ACC1B: Error reading field 'acl_x_res' \n");
    return Failure;
  }

  if (fread_grace(&record->acl_y_res,sizeof(record->acl_y_res),1,src) != 1)
  {
    printf("ACC1B: Error reading field 'acl_y_res' \n");
    return Failure;
  }

  if (fread_grace(&record->acl_z_res,sizeof(record->acl_z_res),1,src) != 1)
  {
    printf("ACC1B: Error reading field 'acl_z_res' \n");
    return Failure;
  }


  if (fread_grace(&record->qualflg,sizeof(record->qualflg),1,src) != 1)
  {
    printf("ACC1B: Error reading field 'qualflg' \n");
    return Failure;
  }


  return True;

}
