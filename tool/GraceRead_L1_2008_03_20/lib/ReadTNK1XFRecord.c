#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define NBITSMAX 8

#define Success 1
#define Failure 0


static char SccsId[] = "$Id: ReadTNK1XFRecord.c,v 1.7 2004/08/30 21:03:36 wib Exp $";



boolean ReadTNK1XFRecord(FILE *src, TNK1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: read the Cold Gas Tank Data Format record from file 
/          pointer src
/
/ Initial version: J. E. Patterson             02/13/01
/
/
/ input:  *src    Pointer to TNK1X Data Format File
/ output: *record Pointer to TNK1X Data struct (TNK1X_t)
/
/ return:      1       normal return
/              0       End Of File reached
<-----------------------------------------------------------------------------*/
{
  unsigned long time_intg;

  int    retrn,i;

  char   bits[NBITSMAX];

/*----------------------------------------------------------------------------->
/ Test for EOF
<-----------------------------------------------------------------------------*/

  retrn = fread_grace(&time_intg,sizeof(record->time_intg),1,src);
  if (feof(src) != 0) return False;
  if (retrn != 1)
  {
    printf("TNK1X: Error reading field 'time_intg' \n");
    return Failure;
  }
  record->time_intg = time_intg;
  
/*----------------------------------------------------------------------------->
/ Read remaining record elements from src
<-----------------------------------------------------------------------------*/

  if (fread_grace(&record->time_frac,sizeof(record->time_frac),1,src) != 1)
  {
    printf("TNK1X: Error reading field 'time_frac' \n");
    return Failure;
  }

  if (fread_grace(&record->time_ref,sizeof(record->time_ref),1,src) != 1)
  {
    printf("TNK1X: Error reading field 'time_ref' \n");
    return Failure;
  }

  if (fread_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,src) != 1)
  {
    printf("TNK1X: Error reading field 'GRACE_id' \n");
    return Failure;
  }

  if (fread_grace(&record->tank_id,sizeof(record->tank_id),1,src) != 1)
  {
    printf("TNK1X: Error reading field 'tank_id' \n");
    return Failure;
  }

  if (fread_grace(&record->qualflg,sizeof(record->qualflg),1,src) != 1)
  {
     printf("TNK1X: Error reading field 'qualflg' \n");
     return Failure;
  }

  if (fread_grace(&record->prod_flag,sizeof(record->prod_flag),1,src) != 1)
  {
     printf("TNK1X: Error reading field 'prod_flag' \n");
     return Failure;
  }

/*----------------------------------------------------------------------------->
/ Decode Product flag
/-----------------------------------------------------------------------------*/
  
  GetCharBits(record->prod_flag,bits);
    
/*----------------------------------------------------------------------------->
/ reading all product specified by prod_flag from src
/-----------------------------------------------------------------------------*/

  loop(i,NBITSMAX)
  {
    if (bits[i] == 1)
    {
      switch(i)
      {
         case 0:
                if (fread_grace(&record->tank_pres,sizeof(record->tank_pres),1,src) != 1)
                {
                  printf("TNK1X: Error reading field 'tank_pres' \n");
                  return Failure;
                }
                break;
         case 1:

                if (fread_grace(&record->reg_pres,sizeof(record->reg_pres),1,src) != 1)
                {
                  printf("TNK1X: Error reading field 'reg_pres' \n");
                  return Failure;
                }
                break;
         case 2:
                if (fread_grace(&record->skin_temp,sizeof(record->skin_temp),1,src) != 1)
                {
                  printf("TNK1X: Error reading field 'skin_temp' \n");
                  return Failure;
                }
                break;
         case 3:
                if (fread_grace(&record->skin_temp_r,sizeof(record->skin_temp_r),1,src) != 1)
                {
                  printf("TNK1X: Error reading field 'skin_temp_r' \n");
                  return Failure;
                }
                break;
         case 4:
                if (fread_grace(&record->adap_temp,sizeof(record->adap_temp),1,src) != 1)
                {
                  printf("TNK1X: Error reading field 'valve_temp' \n");
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
