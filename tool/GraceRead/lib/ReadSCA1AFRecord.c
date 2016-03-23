#include <stdio.h>
#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0


static char SccsId[] = "$Id: ReadSCA1AFRecord.c,v 1.7 2004/08/30 21:03:36 wib Exp $";


boolean ReadSCA1AFRecord(FILE *src, SCA1A_t *record)
/*----------------------------------------------------------------------------->
/ purpose: read Star Camera Assembly 1A  Data Format record from file 
/          pointer src
/
/ coded by: J. Patterson          06/14/00
/
/ input:  *src    pointer to SCA1A Data Format File
/ output: *record Pointer to SCA1A Data struct (SCA1A_t)
/
/ return:      1       normal return
/              0       End Of File reached
<-----------------------------------------------------------------------------*/
{
  unsigned long rcv_time;

  int           retrn;
  long          i;

/*----------------------------------------------------------------------------->
/ Test for EOF
<-----------------------------------------------------------------------------*/

  retrn = fread_grace(&rcv_time,sizeof(rcv_time),1,src);
  if (feof(src) != 0) return False;
  if (retrn != 1)
  {
    printf("SCA1A: Error reading field 'rcv_time' \n");
    return Failure;
  }

  record->rcv_time = rcv_time;
  
/*----------------------------------------------------------------------------->
/ Read remaining record elements from src
<-----------------------------------------------------------------------------*/

  if (fread_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,src) !=1)
  {
    printf("SCA1A: Error reading field 'GRACE_id' \n");
    return Failure;
  }

  if (fread_grace(&record->sca_id,sizeof(record->sca_id),1,src) !=1)
  {
    printf("SCA1A: Error reading field 'sca_id' \n");
    return Failure;
  }

  if (fread_grace(&record->sca_desig,sizeof(record->sca_desig),1,src) !=1)
  {
    printf("SCA1A: Error reading field 'sca_desig' \n");
    return Failure;
  }

  if (fread_grace(&record->quatangle,sizeof(record->quatangle),1,src) !=1)
  {
    printf("SCA1A: Error reading field 'quatangle' \n");
    return Failure;
  }

  if (fread_grace(&record->quaticoeff,sizeof(record->quaticoeff),1,src) !=1)
  {
    printf("SCA1A: Error reading field 'quaticoeff' \n");
    return Failure;
  }

  if (fread_grace(&record->quatjcoeff,sizeof(record->quatjcoeff),1,src) != 1)
  {
    printf("SCA1A: Error reading field 'quatjcoeff' \n");
    return Failure;
  }

  if (fread_grace(&record->quatkcoeff,sizeof(record->quatkcoeff),1,src) != 1)
  {
    printf("SCA1A: Error reading field 'quatkcoeff' \n");
    return Failure;
  }

  if (fread_grace(&record->nlocks,sizeof(record->nlocks),1,src) != 1)
  {
    printf("SCA1A: Error reading field 'nlocks' \n");
    return Failure;
  }

  if (fread_grace(&record->nstars,sizeof(record->nstars),1,src) != 1)
  {
    printf("SCA1A: Error reading field 'nstars' \n");
    return Failure;
  }

  loop(i,3)
  {
    if (fread_grace(&record->sca_config[i],sizeof(char),1,src) != 1)
    {
      printf("SCA1A: Error reading field 'sca_config index = %d ' \n",i);
      return Failure;
    }
  }

  if (fread_grace(&record->sca_mode,sizeof(record->sca_mode),1,src) != 1) 
  {
    printf("SCA1A: Error reading field 'sca_mode' \n");
    return Failure;
  }

  if (fread_grace(&record->qualflg,sizeof(record->qualflg),1,src) != 1)
  {
    printf("SCA1A: Error reading field 'qualflg' \n");
    return Failure;
  }


  return True;

}
