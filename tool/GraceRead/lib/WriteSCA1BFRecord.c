#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0

static char SccsId[] = "$Id: WriteSCA1BFRecord.c,v 1.6 2004/08/30 21:03:39 wib Exp $";



boolean WriteSCA1BFRecord(FILE *dst, SCA1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Write Star Camera Assembtly Format record to file pointer dst
/
/ coded by: J. Patterson                  06/21/00
/
/ input:  *dst    pointer to SCA1B Data Format File
/         *record Pointer to SCA1B Data struct (SCA1B_t)
<-----------------------------------------------------------------------------*/
{

/*----------------------------------------------------------------------------->
/ Write record elements to dst
<-----------------------------------------------------------------------------*/

  if (fwrite_grace(&record->gps_time,sizeof(record->gps_time),1,dst) != 1)
  {
    printf("SCA1B: Error writing field 'gps_time' \n");
    return Failure;
  }

  if (fwrite_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,dst) != 1)
  {
    printf("SCA1B: Error writing field 'GRACE_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->sca_id,sizeof(record->sca_id),1,dst) != 1)
  {
    printf("SCA1B: Error writing field 'sca_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->quatangle,sizeof(record->quatangle),1,dst) != 1)
  {
    printf("SCA1B: Error writing field 'quatangle' \n");
    return Failure;
  }

  if (fwrite_grace(&record->quaticoeff,sizeof(record->quaticoeff),1,dst) != 1)
  {
    printf("SCA1B: Error writing field 'quaticoeff' \n");
    return Failure;
  }

  if (fwrite_grace(&record->quatjcoeff,sizeof(record->quatjcoeff),1,dst) != 1)
  {
    printf("SCA1B: Error writing field 'quatjcoeff' \n");
    return Failure;
  }

  if (fwrite_grace(&record->quatkcoeff,sizeof(record->quatkcoeff),1,dst) != 1)
  {
    printf("SCA1B: Error writing field 'quatkcoeff' \n");
    return Failure;
  }

  if (fwrite_grace(&record->qual_rss,sizeof(record->qual_rss),1,dst) != 1)
  {
    printf("SCA1B: Error writing field 'qual_rss' \n");
    return Failure;
  }

  if (fwrite_grace(&record->qualflg,sizeof(record->qualflg),1,dst) != 1)
  {
    printf("SCA1B: Error writing field 'qualflg' \n");
    return Failure;
  }


  return True;
}
