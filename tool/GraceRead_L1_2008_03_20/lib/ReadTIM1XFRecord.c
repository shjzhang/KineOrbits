#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0

static char SccsId[] = "$Id: ReadTIM1XFRecord.c,v 1.2 2004/08/30 21:03:36 wib Exp $";


boolean ReadTIM1XFRecord(FILE *src, TIM1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Read OBDH mapping to GPS time Format record from file pointer src
/
/ coded by: Gerhard Kruizinga             10/19/01
/
/ input:  *src    pointer to TIM1X Data Format File
/         *record Pointer to TIM1X Data struct (TIM1X_t)
<-----------------------------------------------------------------------------*/
{
  unsigned long obdh_time;

  int           retrn;


/*----------------------------------------------------------------------------->
/ Test for EOF
<-----------------------------------------------------------------------------*/

  retrn = fread_grace(&obdh_time,sizeof(obdh_time),1,src);
  if (feof(src) != 0) return False;
  if (retrn != 1)
  {
    printf("TIM1X: Error reading field 'obdh_time' \n");
    return Failure;
  }
  
  record->obdh_time = obdh_time;

/*----------------------------------------------------------------------------->
/ Read record elements from src
<-----------------------------------------------------------------------------*/

  if (fread_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,src) != 1)
  {
    printf("TIM1X: Error reading field 'GRACE_id' \n");
    return Failure;
  }

  if (fread_grace(&record->TS_suppid,sizeof(record->TS_suppid),1,src) != 1)
  {
    printf("TIM1X: Error reading field 'TS_suppid' \n");
    return Failure;
  }

  if (fread_grace(&record->gpstime_intg,sizeof(record->gpstime_intg),1,src) != 1)
  {
    printf("TIM1X: Error reading field 'gpstime_intg' \n");
    return Failure;
  }

  if (fread_grace(&record->gpstime_frac,sizeof(record->gpstime_frac),1,src) != 1)
  {
    printf("TIM1X: Error reading field 'gpstime_frac' \n");
    return Failure;
  }

  if (fread_grace(&record->first_icu_blknr,sizeof(record->first_icu_blknr),1,src) != 1)
  {
    printf("TIM1X: Error reading field 'first_icu_blknr' \n");
    return Failure;
  }

  if (fread_grace(&record->final_icu_blknr,sizeof(record->final_icu_blknr),1,src) != 1)
  {
    printf("TIM1X: Error reading field 'final_icu_blknr' \n");
    return Failure;
  }

  if (fread_grace(&record->qualflg,sizeof(record->qualflg),1,src) != 1)
  {
    printf("TIM1X: Error reading field 'qualflg' \n");
    return Failure;
  }


  return True;
}
