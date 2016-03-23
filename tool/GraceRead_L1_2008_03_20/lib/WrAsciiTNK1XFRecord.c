#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define NBITSMAX 8

#define Success 1 
#define Failure 0


static char SccsId[] = "$Id: WrAsciiTNK1XFRecord.c,v 1.8 2004/08/30 21:03:37 wib Exp $";


boolean WrAsciiTNK1XFRecord(FILE *dst, TNK1X_t *record)
/*-----------------------------------------------------------------------------'
/ purpose: Dump ascii records of the Cold Gas Tank data 
/          from the file pointed to by dst 
/
/ coded by: J. E. Patterson                         02/13/01
/
/ input:  *dst    Pointer to TNK1X Data Format File
/         *record Pointer to TNK1X Data struct (TNK1X_t)
'-----------------------------------------------------------------------------*/
{
 long i;

 char bits8[NBITSMAX];                                                                         


 fprintf(dst,"%d %d %c %c %c",
          record->time_intg, record->time_frac, record->time_ref, 
          record->GRACE_id,record->tank_id);

 GetCharBits(record->qualflg,bits8);                                                    

 fprintf(dst," ");                                                                     
 loop(i,NBITSMAX)fprintf(dst,"%d",bits8[NBITSMAX-1-i]);                                                 

/*----------------------------------------------------------------------------->
/ Decode Product flag
/-----------------------------------------------------------------------------*/
 GetCharBits(record->prod_flag,bits8);                                                    

 fprintf(dst," ");                                                                     
 loop(i,NBITSMAX)fprintf(dst,"%d",bits8[NBITSMAX-1-i]);                                                 
    
/*----------------------------------------------------------------------------->
/ reading all product specified by prod_flag from src
/-----------------------------------------------------------------------------*/

  loop(i,NBITSMAX)
  {
    if (bits8[i] == 1)
    {
      switch(i)
      {
         case 0:
                fprintf(dst," %.16g",record->tank_pres);
                break;
         case 1:
                fprintf(dst," %.16g",record->reg_pres);
                break;
         case 2:
                fprintf(dst," %.16g",record->skin_temp);
                break;
         case 3:
                fprintf(dst," %.16g",record->skin_temp_r);
                break;
         case 4:
                fprintf(dst," %.16g",record->adap_temp);
                break;
          default:
          fprintf(stderr,"Product Flag index %d in WrAsciiTNK1X is invalid!!! \n\n",i);
          exit(0);
      }
    }
  }

  fprintf(dst,"\n");
  
 return Success;
}
