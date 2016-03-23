#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0

#define NBITSMAX 32 

static char SccsId[] = "$Id: ReadXXXVOFRecord.c,v 1.5 2004/08/30 21:03:36 wib Exp $";


boolean ReadXXXVOFRecord(FILE *src, XXXVO_t *record)
/*----------------------------------------------------------------------------->
/ purpose: read Vector Orientation Level 1B Data Format record from file 
/          pointer src
/
/ coded by: J. E. Patterson                  06/14/00
/
/ modified name of routine and structure     02/15/01                           
/
/
/ input:  *src    Pointer to XXXVO Data Format File
/ output: *record Pointer to XXXVO Data struct (XXXVO_t)
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
    printf("XXXVO: Error reading field 'gps_time' \n");
    return Failure;
  }

  record->gps_time = gps_time;
  
/*----------------------------------------------------------------------------->
/ Read remaining record elements from src
<-----------------------------------------------------------------------------*/

  if (fread_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,src) != 1)
  {
    printf("XXXVO: Error reading field 'GRACE_id' \n");
    return Failure;
  }

  
  if (fread_grace(&record->mag,sizeof(record->mag),1,src) != 1)
  {
    printf("XXXVO: Error reading field 'mag' \n");
    return Failure;
  }

  if (fread_grace(&record->cosx,sizeof(record->cosx),1,src) != 1)
  {
    printf("XXXVO: Error reading field 'cosx' \n");
    return Failure;
  }

  if (fread_grace(&record->cosy,sizeof(record->cosy),1,src) != 1)
  {
    printf("XXXVO: Error reading field 'cosy' \n");
    return Failure;
  }

  if (fread_grace(&record->cosz,sizeof(record->cosz),1,src) != 1)
  {
    printf("XXXVO: Error reading field 'cosz' \n");
    return Failure;
  }

  if (fread_grace(&record->qualflg,sizeof(record->qualflg),1,src) != 1)
  {
    printf("XXXVO: Error reading field 'qualflg' \n");
    return Failure;
  }



  return True;

}
