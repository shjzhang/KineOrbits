#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define NBITSMAX 8

#define Success 1
#define Failure 0

static char SccsId[] = "$Id: WriteTNK1XFRecord.c,v 1.7 2004/08/30 21:03:39 wib Exp $";


boolean WriteTNK1XFRecord(FILE *dst, TNK1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: write the Cold Gas Tank Data Format record to file pointer dst
/
/ coded by: J. E. Patterson                  02/13/01
/
/
/ input:  *dst    Pointer to TNK1XF Data Format File
/         *record Pointer to TNK1XF Data struct (TNK1X_t)
<-----------------------------------------------------------------------------*/
{
  long i;

  char bits[NBITSMAX];
 
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/
  if (fwrite_grace(&record->time_intg,sizeof(record->time_intg),1,dst) != 1)
  {
    printf("TNK1X: Error writing field 'time_intg' \n");
    return Failure;
  }
    
  if (fwrite_grace(&record->time_frac,sizeof(record->time_frac),1,dst) != 1)
  {
    printf("TNK1X: Error writing field 'time_frac' \n");
    return Failure;
  }
    
  if (fwrite_grace(&record->time_ref,sizeof(record->time_ref),1,dst) != 1)
  {
    printf("TNK1X: Error writing field 'time_ref' \n");
    return Failure;
  }
    
  if (fwrite_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,dst) != 1)
  {
    printf("TNK1X: Error writing field 'GRACE_id' \n");
    return Failure;
  }
    
  if (fwrite_grace(&record->tank_id,sizeof(record->tank_id),1,dst) != 1)
  {
    printf("TNK1X: Error writing field 'tank_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->qualflg,sizeof(record->qualflg),1,dst) != 1)
  {
    printf("TNK1X: Error writing field 'qualflg' \n");
    return Failure;
  }

  if (fwrite_grace(&record->prod_flag,sizeof(record->prod_flag),1,dst) != 1)
  {
    printf("TNK1X: Error writing field 'prod_flag' \n");
    return Failure;
  }
/*----------------------------------------------------------------------------->
/ Decode Product flag
/-----------------------------------------------------------------------------*/
  
  GetCharBits(record->prod_flag,bits);
    
/*----------------------------------------------------------------------------->
/ writing all products specified by prod_flag from src
/-----------------------------------------------------------------------------*/

  loop(i,NBITSMAX)
  {
    if (bits[i] == 1)
    {
      switch(i)
      {
         case 0:
                if (fwrite_grace(&record->tank_pres,sizeof(record->tank_pres),1,dst) != 1)
                {
                  printf("TNK1X: Error writing field 'tank_pres' \n");
                  return Failure;
                }
                break;
         case 1:

                if (fwrite_grace(&record->reg_pres,sizeof(record->reg_pres),1,dst) != 1)
                {
                  printf("TNK1X: Error writing field 'reg_pres' \n");
                  return Failure;
                }
                break;
         case 2:
                if (fwrite_grace(&record->skin_temp,sizeof(record->skin_temp),1,dst) != 1)
                {
                  printf("TNK1X: Error writing field 'skin_temp' \n");
                  return Failure;
                }
                break;
         case 3:
                if (fwrite_grace(&record->skin_temp_r,sizeof(record->skin_temp_r),1,dst) != 1)
                {
                  printf("TNK1X: Error writing field 'skin_temp_r' \n");
                  return Failure;
                }
                break;
         case 4:
                if (fwrite_grace(&record->adap_temp,sizeof(record->adap_temp),1,dst) != 1)
                {
                  printf("TNK1X: Error writing field 'valve_temp' \n");
                  return Failure;
                }
                break;
          default:
          fprintf(stderr,"Product Flag index %d in ReadTNK1X is invalid!!! \n\n",i);
          exit(0);
      }
    }
  }

    
    

  return True;
}
