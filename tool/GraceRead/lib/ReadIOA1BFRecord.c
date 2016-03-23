#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0


static char SccsId[] = "$Id: ReadIOA1BFRecord.c,v 1.5 2004/08/30 21:03:36 wib Exp $";



boolean ReadIOA1BFRecord(FILE *src, IOA1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Read Inertial Orientation of ACC Data Format record from file 
/          pointer src
/
/ coded by: J. E. Patterson                  06/21/00
/
/ modified name of routine and structure     02/15/01
/
/ input:  *src    Pointer to IOA1B Data Format File
/ output: *record Pointer to IOA1B Data struct (IOA1B_t)
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

  retrn =  fread_grace(&gps_time,sizeof(gps_time),1,src);
  if (feof(src) != 0) return False;
  if (retrn != 1)
  {
    printf("IOA1B: Error reading field 'gps_time' \n");
    return Failure;
  }

  record->gps_time = gps_time;
  
/*----------------------------------------------------------------------------->
/ Read remaining record elements from src
<-----------------------------------------------------------------------------*/
  if (fread_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,src) != 1)
  {
    printf("IOA1B: Error reading field 'GRACE_id' \n");                               
    return Failure;
  }

  if (fread_grace(&record->quatangle,sizeof(record->quatangle),1,src) != 1)
  {
    printf("IOA1B: Error reading field 'quatangle' \n");
    return Failure;
  }

  if (fread_grace(&record->quaticoeff,sizeof(record->quaticoeff),1,src) != 1)
  {
    printf("IOA1B: Error reading field 'quaticoeff' \n");
    return Failure;
  }

  if (fread_grace(&record->quatjcoeff,sizeof(record->quatjcoeff),1,src) != 1)
  {
    printf("IOA1B: Error reading field 'quatjcoeff' \n");
    return Failure;
  }

  if (fread_grace(&record->quatkcoeff,sizeof(record->quatkcoeff),1,src) != 1)
  {
    printf("IOA1B: Error reading field 'quatkcoeff' \n");
    return Failure;
  }

  if (fread_grace(&record->qualflg,sizeof(record->qualflg),1,src) != 1)
  {
    printf("IOA1B: Error reading field 'qualflg' \n");
    return Failure;
  }


  return True;

}
