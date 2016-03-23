#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0


static char SccsId[] = "$Id: ReadTHR1XFRecord.c,v 1.6 2004/08/30 21:03:36 wib Exp $";



boolean ReadTHR1XFRecord(FILE *src, THR1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: read Thruster Level 1B Data Format record from file 
/          pointer src
/
/ Initial version: J. E. Patterson           09/06/00
/
/ modified name of routine and structure     02/15/01
/
/ input:  *src    Pointer to THR1X Data Format File
/ output: *record Pointer to THR1X Data struct (THR1X_t)
/
/ return:      1       normal return
/              0       End Of File reached
<-----------------------------------------------------------------------------*/
{
  unsigned long time_intg;

  int    retrn,i;


/*----------------------------------------------------------------------------->
/ Test for EOF
<-----------------------------------------------------------------------------*/

  retrn = fread_grace(&time_intg,sizeof(record->time_intg),1,src);
  if (feof(src) != 0) return False;
  if (retrn != 1)
  {
    printf("THR1X: Error reading field 'time_intg' \n");
    return Failure;
  }
  record->time_intg = time_intg;
  
/*----------------------------------------------------------------------------->
/ Read remaining record elements from src
<-----------------------------------------------------------------------------*/

  if (fread_grace(&record->time_frac,sizeof(record->time_frac),1,src) != 1)
  {
    printf("THR1X: Error reading field 'time_frac' \n");
    return Failure;
  }

  if (fread_grace(&record->time_ref,sizeof(record->time_ref),1,src) != 1)
  {
    printf("THR1X: Error reading field 'time_ref' \n");
    return Failure;
  }

  if (fread_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,src) != 1)
  {
    printf("THR1X: Error reading field 'GRACE_id' \n");
    return Failure;
  }

  loop(i,MAXTHRSTRS)
  {
    if (fread_grace(&record->thrust_count[i],sizeof(record->thrust_count[i]),1,src) != 1)
    {
      printf("THR1X: Error reading field 'thrust_count[%d]' \n",i);
      return Failure;
    }
  }

  loop(i,MAXTHRSTRS)
  {
    if (fread_grace(&record->on_time[i],sizeof(record->on_time[i]),1,src) != 1)
    {
      printf("THR1X: Error reading field 'on_time[%d]' \n",i);
      return Failure;
    }
  }

  loop(i,MAXTHRSTRS)
  {
    if (fread_grace(&record->accum_dur[i],sizeof(record->accum_dur[i]),1,src) != 1)
    {
      printf("THR1X: Error reading field 'accum_dur[%d]' \n",i);
      return Failure;
    }
  }
  
  if (fread_grace(&record->qualflg,sizeof(record->qualflg),1,src) != 1)
  {
    printf("THR1X: Error reading field 'qualflg' \n");
    return Failure;
  }


  return True;

}
