#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define NBITSMAX 8

static char SccsId[] = "$Id: PrintTNK1XFRecord.c,v 1.6 2004/08/30 21:03:35 wib Exp $";

void PrintTNK1XFRecord(FILE *dst, TNK1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Print a detailed ascii description of Cold Gas Tank Data 
/          Data Format record to file pointer dst
/
/ coded by: J. E. Patterson                         02/13/01
/
/ input:  *dst    Pointer to TNK1X Data Format File
/         *record Pointer to TNK1X Data struct (TNK1X_t)
<-----------------------------------------------------------------------------*/
{

  long i;

  char bits[NBITSMAX];


/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/

  fprintf(dst," %-21s = %ld\n","record->time_intg",record->time_intg);
  fprintf(dst," %-21s = %ld\n","record->time_frac",record->time_frac);
  fprintf(dst," %-21s = %c\n","record->time_ref",record->time_ref);
  fprintf(dst," %-21s = %c\n","record->GRACE_id",record->GRACE_id);
  fprintf(dst," %-21s = %c\n","record->tank_id",record->tank_id);
  fprintf(dst," %-21s = %ld [","record->qualflg",record->qualflg);
  GetCharBits(record->qualflg,bits);
  loop(i,NBITSMAX)fprintf(dst,"%d",bits[NBITSMAX-1-i]);
  fprintf(stderr,"]\n");

  fprintf(dst," %-21s = %ld [","record->prod_flag",record->prod_flag);
/*----------------------------------------------------------------------------->
/ Decode Product flag
<-----------------------------------------------------------------------------*/
  GetCharBits(record->prod_flag,bits);
  loop(i,NBITSMAX)fprintf(dst,"%d",bits[NBITSMAX-1-i]);
  fprintf(stderr,"]\n");

/*----------------------------------------------------------------------------->
/ priting all products specified by prod_flag 
/-----------------------------------------------------------------------------*/

  loop(i,NBITSMAX)
  {
    if (bits[i] == 1)
    {
      switch(i)
      {
         case 0:
                fprintf(dst," %-21s = %.16g\n","record->tank_pres",record->tank_pres);
                break;
         case 1:
                fprintf(dst," %-21s = %.16g\n","record->reg_pres",record->reg_pres);
                break;
         case 2:      
                fprintf(dst," %-21s = %.16g\n","record->skin_temp",record->skin_temp);
                break;
         case 3:      
                fprintf(dst," %-21s = %.16g\n","record->skin_temp_r",record->skin_temp_r);
                break;
         case 4:      
                fprintf(dst," %-21s = %.16g\n","record->adap_temp",record->adap_temp);
                break;
          default:
          fprintf(stderr,"Product Flag index %d in PrintTNK1X is invalid!!! \n\n",i);
          exit(0);
      }
    }
  }
 
}
