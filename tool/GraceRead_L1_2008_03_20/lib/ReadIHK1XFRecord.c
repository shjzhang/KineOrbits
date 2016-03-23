#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0

static char SccsId[] = "$Id: ReadIHK1XFRecord.c,v 1.2 2004/08/30 21:03:36 wib Exp $";

boolean ReadIHK1XFRecord(FILE *src, IHK1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: read the IPU HK 1X Data Format record from file pointer src
/
/ coded by: Gerhard L.H. Kruizinga           09/25/01
/
/ input:  *src    Pointer to IHK1XF Data Format File
/         *record Pointer to IHK1XF Data struct (IHK1X_t)
<-----------------------------------------------------------------------------*/
{
  int    i,retrn;
  long   time_intg;
 
/*----------------------------------------------------------------------------->
/ Test for EOF
<-----------------------------------------------------------------------------*/

  retrn = fread_grace(&time_intg,sizeof(record->time_intg),1,src);
  if (feof(src) != 0) return False;
  if (retrn != 1)
  {
    printf("MAS1X: Error reading field 'time_intg' \n");
    return Failure;
  }
  record->time_intg = time_intg;

/*----------------------------------------------------------------------------->
/ Read remainder of record
<-----------------------------------------------------------------------------*/

  if (fread_grace(&record->time_frac,sizeof(record->time_frac),1,src) != 1)
  {
    fprintf(stderr,"IHK1X: Error reading field 'time_frac' \n");
    return Failure;
  }
    
  if (fread_grace(&record->time_ref,sizeof(record->time_ref),1,src) != 1)
  {
    fprintf(stderr,"IHK1X: Error reading field 'time_ref' \n");
    return Failure;
  }
    
  if (fread_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,src) != 1)
  {
    fprintf(stderr,"IHK1X: Error reading field 'GRACE_id' \n");
    return Failure;
  }

  if (fread_grace(&record->qualflg,sizeof(record->qualflg),1,src) != 1)
  {
    fprintf(stderr,"IHK1X: Error reading field 'qualflg' \n");
    return Failure;
  }
    
  if (fread_grace(&record->sensortype,sizeof(record->sensortype),1,src) != 1)
  {
    fprintf(stderr,"IHK1X: Error reading field 'sensortype' \n");
    return Failure;
  }

  if (fread_grace(&record->sensorvalue,sizeof(record->sensorvalue),1,src) != 1)
  {
    fprintf(stderr,"IHK1X: Error reading field 'sensorvalue' \n");
    return Failure;
  }

  i=0;

  if (fread_grace(&record->sensorname[i],sizeof(char),1,src) != 1)
  {
    fprintf(stderr,"IHK1X: Error reading field 'sensorname[%d]' \n",i);
    return Failure;
  }

  while(record->sensorname[i] != '\0')
  {
    i++;
    if (fread_grace(&record->sensorname[i],sizeof(char),1,src) != 1)
    {
      fprintf(stderr,"IHK1X: Error reading field 'sensorname[%d]' \n",i);
      return Failure;
    }
  }

  return True;
}
