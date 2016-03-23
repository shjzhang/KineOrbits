#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0

static char SccsId[] = "$Id: WriteTHR1XFRecord.c,v 1.6 2004/08/30 21:03:39 wib Exp $";


boolean WriteTHR1XFRecord(FILE *dst, THR1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: write the Thruster Level 1B Data Format record to file pointer dst
/
/ coded by: J. E. Patterson                  06/21/00
/
/ modified name of routine and structure     02/15/01
/
/ input:  *dst    Pointer to THR1XF Data Format File
/         *record Pointer to THR1XF Data struct (THR1X_t)
<-----------------------------------------------------------------------------*/
{
  int n_thrusters,i;
 
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/
  if (fwrite_grace(&record->time_intg,sizeof(record->time_intg),1,dst) != 1)
  {
    printf("THR1X: Error writing field 'time_intg' \n");
    return Failure;
  }
    
  if (fwrite_grace(&record->time_frac,sizeof(record->time_frac),1,dst) != 1)
  {
    printf("THR1X: Error writing field 'time_frac' \n");
    return Failure;
  }
    
  if (fwrite_grace(&record->time_ref,sizeof(record->time_ref),1,dst) != 1)
  {
    printf("THR1X: Error writing field 'time_ref' \n");
    return Failure;
  }
    
  if (fwrite_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,dst) != 1)
  {
    printf("THR1X: Error writing field 'GRACE_id' \n");
    return Failure;
  }

  loop(i,MAXTHRSTRS)
  {   
    if (fwrite_grace(&record->thrust_count[i],sizeof(record->thrust_count[i]),
         1,dst) != 1)
    {
      printf("THR1X: Error writing field 'thrust_count[%d]' \n",i);
      return Failure;
    }
  }

  loop(i,MAXTHRSTRS)
  {   
    if (fwrite_grace(&record->on_time[i],sizeof(record->on_time[i]),
         1,dst) != 1)
    {
      printf("THR1X: Error writing field 'on_time[%d]' \n",i);
      return Failure;
    }
  }
    
  loop(i,MAXTHRSTRS)
  {   
    if (fwrite_grace(&record->accum_dur[i],sizeof(record->accum_dur[i]),
         1,dst) != 1)
    {
      printf("THR1X: Error writing field 'accum_dur[%d]' \n",i);
      return Failure;
    }
  }
  
  if (fwrite_grace(&record->qualflg,sizeof(record->qualflg),1,dst) != 1)
  {
    printf("THR1X: Error writing field 'qualflg' \n");
    return Failure;
  }
    

  return True;
}
