#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0

static char SccsId[] = "$Id: WriteIHK1XFRecord.c,v 1.2 2004/08/30 21:03:38 wib Exp $";

boolean WriteIHK1XFRecord(FILE *dst, IHK1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: write the IPU HK 1X Data Format record to file pointer dst
/
/ coded by: Gerhard L.H. Kruizinga           09/25/01
/
/ input:  *dst    Pointer to IHK1XF Data Format File
/         *record Pointer to IHK1XF Data struct (IHK1X_t)
<-----------------------------------------------------------------------------*/
{
  int n_len,i;
 
  if (fwrite_grace(&record->time_intg,sizeof(record->time_intg),1,dst) != 1)
  {
    fprintf(stderr,"IHK1X: Error writing field 'time_intg' \n");
    return Failure;
  }
    
  if (fwrite_grace(&record->time_frac,sizeof(record->time_frac),1,dst) != 1)
  {
    fprintf(stderr,"IHK1X: Error writing field 'time_frac' \n");
    return Failure;
  }
    
  if (fwrite_grace(&record->time_ref,sizeof(record->time_ref),1,dst) != 1)
  {
    fprintf(stderr,"IHK1X: Error writing field 'time_ref' \n");
    return Failure;
  }
    
  if (fwrite_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,dst) != 1)
  {
    fprintf(stderr,"IHK1X: Error writing field 'GRACE_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->qualflg,sizeof(record->qualflg),1,dst) != 1)
  {
    fprintf(stderr,"IHK1X: Error writing field 'qualflg' \n");
    return Failure;
  }
    
  if (fwrite_grace(&record->sensortype,sizeof(record->sensortype),1,dst) != 1)
  {
    fprintf(stderr,"IHK1X: Error writing field 'sensortype' \n");
    return Failure;
  }

  if (fwrite_grace(&record->sensorvalue,sizeof(record->sensorvalue),1,dst) != 1)
  {
    fprintf(stderr,"IHK1X: Error writing field 'sensorvalue' \n");
    return Failure;
  }

  n_len = strlen(record->sensorname);

  loop(i,n_len+1)
  {
    if (fwrite_grace(&record->sensorname[i],sizeof(char),1,dst) != 1)
    {
      fprintf(stderr,"IHK1X: Error writing field 'sensorname[%d]' \n",i);
      return Failure;
    }
  }

  return True;
}
