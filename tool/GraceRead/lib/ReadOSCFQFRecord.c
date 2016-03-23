#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0


static char SccsId[] = "$Id: ReadOSCFQFRecord.c,v 1.7 2004/08/30 21:03:36 wib Exp $";



boolean ReadOSCFQFRecord(FILE *src, OSCFQ_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Read Ultra Stable Oscillator Data Format record from file 
/          pointer src
/
/ coded by: J. E. Patterson                  06/21/00
/
/ modified name of routine and structure     02/15/01
/
/ input:  *src    Pointer to OSCFQ Data Format File
/ output: *record Pointer to OSCFQ Data struct (OSCFQ_t)
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
    printf("OSCFQ: Error in reading field 'gps_time' \n");
    return Failure;
  }

  record->gps_time = gps_time;
  
/*----------------------------------------------------------------------------->
/ Read remaining record elements from src
<-----------------------------------------------------------------------------*/
  
  if (fread_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,src) != 1)
  {
    printf("OSCFQ: Error in reading field 'GRACE_id' \n");
    return Failure;
  }

  if (fread_grace(&record->uso_id,sizeof(record->uso_id),1,src) != 1)
  {
    printf("OSCFQ: Error in reading field 'uso_id' \n");
    return Failure;
  }

  if (fread_grace(&record->uso_freq,sizeof(record->uso_freq),1,src) != 1)
  {
    printf("OSCFQ: Error in reading field 'uso_freq' \n");
    return Failure;
  }

  if (fread_grace(&record->K_freq,sizeof(record->K_freq),1,src) != 1)
  {
    printf("OSCFQ: Error in reading field 'K_freq' \n");
    return Failure;
  }

  if (fread_grace(&record->Ka_freq,sizeof(record->Ka_freq),1,src) != 1)
  {
    printf("OSCFQ: Error in reading field 'Ka_freq' \n");
    return Failure;
  }

  if (fread_grace(&record->qualflg,sizeof(record->qualflg),1,src) != 1)
  {
    printf("OSCFQ: Error in reading field 'qualflg' \n");
    return Failure;
  }


  return True;

}
