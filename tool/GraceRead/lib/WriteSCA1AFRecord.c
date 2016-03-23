#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0

static char SccsId[] = "$Id: WriteSCA1AFRecord.c,v 1.7 2004/08/30 21:03:38 wib Exp $";


boolean WriteSCA1AFRecord(FILE *dst, SCA1A_t *record)
/*----------------------------------------------------------------------------->
/ purpose: write Star Camera Assembtly Format record to file pointer dst
/
/ coded by: J. Patterson                  06/14/00
/
/ input:  *dst    pointer to SCA1A Data Format File
/         *record Pointer to SCA1A Data struct (SCA1A_t)
<-----------------------------------------------------------------------------*/
{

  long i;

/*----------------------------------------------------------------------------->
/ Write record elements to dst
<-----------------------------------------------------------------------------*/

  if (fwrite_grace(&record->rcv_time,sizeof(record->rcv_time),1,dst) !=1)
  {
    printf("SCA1A: Error writing field 'rcv_time' \n");
    return Failure;
  }

  if (fwrite_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,dst) !=1)
  {
    printf("SCA1A: Error writing field 'GRACE_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->sca_id,sizeof(record->sca_id),1,dst) !=1)
  {
    printf("SCA1A: Error writing field 'sca_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->sca_desig,sizeof(record->sca_desig),1,dst) !=1)
  {
    printf("SCA1A: Error writing field 'sca_desig' \n");
    return Failure;
  }

  if (fwrite_grace(&record->quatangle,sizeof(record->quatangle),1,dst) !=1)
  {
    printf("SCA1A: Error writing field 'quatangle' \n");
    return Failure;
  }

  if (fwrite_grace(&record->quaticoeff,sizeof(record->quaticoeff),1,dst) !=1)
  {
    printf("SCA1A: Error writing field 'quaticoeff' \n");
    return Failure;
  }

  if (fwrite_grace(&record->quatjcoeff,sizeof(record->quatjcoeff),1,dst) !=1)
  {
    printf("SCA1A: Error writing field 'quatjcoeff' \n");
    return Failure;
  }

  if (fwrite_grace(&record->quatkcoeff,sizeof(record->quatkcoeff),1,dst) !=1)
  {
    printf("SCA1A: Error writing field 'quatkcoeff' \n");
    return Failure;
  }

  if (fwrite_grace(&record->nlocks,sizeof(record->nlocks),1,dst) !=1)
  {
    printf("SCA1A: Error writing field 'nlocks' \n");
    return Failure;
  }

  if (fwrite_grace(&record->nstars,sizeof(record->nstars),1,dst) !=1)
  {
    printf("SCA1A: Error writing field 'nstars' \n");
    return Failure;
  }

  loop(i,3)
  {
    if (fwrite_grace(&record->sca_config[i],sizeof(char),1,dst) !=1)
    {
      printf("SCA1A: Error writing field 'sca_config index = %d' \n",i);
      return Failure;
    }
  }

  if (fwrite_grace(&record->sca_mode,sizeof(record->sca_mode),1,dst) !=1)
  {
    printf("SCA1A: Error writing field 'sca_mode' \n");
    return Failure;
  }

  if (fwrite_grace(&record->qualflg,sizeof(record->qualflg),1,dst) !=1)
  {
    printf("SCA1A: Error writing field 'qualflg' \n");
    return Failure;
  }

  return True;
}
