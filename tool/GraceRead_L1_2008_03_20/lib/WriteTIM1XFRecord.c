#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0

static char SccsId[] = "$Id: WriteTIM1XFRecord.c,v 1.2 2004/08/30 21:03:39 wib Exp $";


boolean WriteTIM1XFRecord(FILE *dst, TIM1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Write OBDH mapping to GPS time Format record to file pointer dst
/
/ coded by: Gerhard Kruizinga             10/19/01
/
/ input:  *dst    pointer to TIM1X Data Format File
/         *record Pointer to TIM1X Data struct (TIM1X_t)
<-----------------------------------------------------------------------------*/
{

/*----------------------------------------------------------------------------->
/ Write record elements to dst
<-----------------------------------------------------------------------------*/

  if (fwrite_grace(&record->obdh_time,sizeof(record->obdh_time),1,dst) != 1)
  {
    printf("TIM1X: Error writing field 'obdh_time' \n");
    return Failure;
  }

  if (fwrite_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,dst) != 1)
  {
    printf("TIM1X: Error writing field 'GRACE_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->TS_suppid,sizeof(record->TS_suppid),1,dst) != 1)
  {
    printf("TIM1X: Error writing field 'TS_suppid' \n");
    return Failure;
  }

  if (fwrite_grace(&record->gpstime_intg,sizeof(record->gpstime_intg),1,dst) != 1)
  {
    printf("TIM1X: Error writing field 'gpstime_intg' \n");
    return Failure;
  }

  if (fwrite_grace(&record->gpstime_frac,sizeof(record->gpstime_frac),1,dst) != 1)
  {
    printf("TIM1X: Error writing field 'gpstime_frac' \n");
    return Failure;
  }

  if (fwrite_grace(&record->first_icu_blknr,sizeof(record->first_icu_blknr),1,dst) != 1)
  {
    printf("TIM1X: Error writing field 'first_icu_blknr' \n");
    return Failure;
  }

  if (fwrite_grace(&record->final_icu_blknr,sizeof(record->final_icu_blknr),1,dst) != 1)
  {
    printf("TIM1X: Error writing field 'final_icu_blknr' \n");
    return Failure;
  }

  if (fwrite_grace(&record->qualflg,sizeof(record->qualflg),1,dst) != 1)
  {
    printf("TIM1X: Error writing field 'qualflg' \n");
    return Failure;
  }


  return True;
}
