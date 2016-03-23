#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define NBITSMAX 8

#define Success 1 
#define Failure 0

static char SccsId[] = "$Id: WrAsciiMAS1XFRecord.c,v 1.3 2004/08/30 21:03:37 wib Exp $";

boolean WrAsciiMAS1XFRecord(FILE *dst, MAS1X_t *record)
/*-----------------------------------------------------------------------------'
/ purpose: Dump ascii records of the SC Mass data format 
/          from the file pointed to by dst 
/
/ coded by: Gerhard L.H. Kruizinga                  09/25/01
/
/ input:  *dst    Pointer to MAS1X Data Format File
/         *record Pointer to MAS1X Data struct (MAS1X_t)
'-----------------------------------------------------------------------------*/
{
 long i;

 char bits8[NBITSMAX];                                                                         


 fprintf(dst,"%d %d %c %c ",
          record->time_intg, record->time_frac, record->time_ref,
          record->GRACE_id);

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
                fprintf(dst," %.16g",record->mass_thr);
                break;
         case 1:
                fprintf(dst," %.16g",record->mass_thr_err);
                break;
         case 2:
                fprintf(dst," %.16g",record->mass_tnk);
                break;
         case 3:
                fprintf(dst," %.16g",record->mass_tnk_err);
                break;
         case 4:
                fprintf(dst," %.16g",record->gas_mass_thr1);
                break;
         case 5:
                fprintf(dst," %.16g",record->gas_mass_thr2);
                break;
         case 6:
                fprintf(dst," %.16g",record->gas_mass_tnk1);
                break;
         case 7:
                fprintf(dst," %.16g",record->gas_mass_tnk2);
                break;
          default:
          fprintf(stderr,"Product Flag index %d in WrAsciiMAS1X is invalid!!! \n\n",i);
          exit(0);
      }
    }
  }

  fprintf(dst,"\n");
  
 return Success;
}
