#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define NBITSMAX 32 
#define Success 1 
#define Failure 0

static char SccsId[] = "$Id: WrAsciiACC1AFRecord.c,v 1.6 2004/08/30 21:03:37 wib Exp $";

boolean WrAsciiACC1AFRecord(FILE *dst, ACC1A_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Dump ascii records of level 1A Accelerometer data
/
/ coded by: J. E. Patterson                         07/24/01
/
/ input:  *dst    Pointer to GFD1X Data Format File
/         *record Pointer to ACC1A Data struct (ACC1A_t)
<-----------------------------------------------------------------------------*/
{
 long i,j;

 char bits[NBITSMAX];
 char bits8[8];
 
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/

  fprintf(dst,"%d",record->rcvtime_intg);
  fprintf(dst,"  %d",record->rcvtime_frac);
  fprintf(dst,"  %c",record->time_ref);
  fprintf(dst,"  %c",record->GRACE_id);

  GetCharBits(record->qualflg,bits8);

  fprintf(dst,"  ");
  loop(i,8)fprintf(dst,"%d",bits8[7-i]);
  fprintf(dst," ");

/*----------------------------------------------------------------------------->
/ Decode Product flag 
<-----------------------------------------------------------------------------*/

  GetLongBits(record->prod_flag,bits);

  fprintf(dst,"  ");
  loop(i,NBITSMAX)fprintf(dst,"%d",bits[31-i]);
  fprintf(dst," ");

/*----------------------------------------------------------------------------->
/ Write all product specified by prod_flag to dst
<-----------------------------------------------------------------------------*/

  loop(i,NBITSMAX)
  {
    if (bits[i] == 1)
    {
      switch(i)
      {
       case  0:
         fprintf(dst," %.16g",record->lin_accl_x);
         break;

       case  1:
         fprintf(dst," %.16g",record->lin_accl_y);
         break;

       case  2:
         fprintf(dst," %.16g",record->lin_accl_z);
         break;

       case  3:
         fprintf(dst," %.16g",record->ang_accl_x);
         break;

       case  4:
         fprintf(dst," %.16g",record->ang_accl_y);
         break;

       case  5:
         fprintf(dst," %.16g",record->ang_accl_z);
         break;

       case  6:
         fprintf(dst," %.16g",record->bias_vol);
         break;

       case  7:
         fprintf(dst," %.10g",record->vd);
         break;

       case  8:
         fprintf(dst," %.10g",record->x1_out);
         break;

       case  9:
         fprintf(dst," %.10g",record->x2_out);
         break;

       case 10:
         fprintf(dst," %.10g",record->x3_out);
         break;

       case 11:
         fprintf(dst," %.10g",record->y1_out);
         break;

       case 12:
         fprintf(dst," %.10g",record->y2_out);
         break;

       case 13:
         fprintf(dst," %.10g",record->z1_out);
         break;

       case 14:
         fprintf(dst," %.10g",record->tesu);
         break;

       case 15:
         fprintf(dst," %.10g",record->taicu);
         break;

       case 16:
         fprintf(dst," %.10g",record->tisu);
         break;

       case 17:
         fprintf(dst," %.10g",record->v15picu);
         break;

       case 18:
         fprintf(dst," %.10g",record->v15micu);
         break;

       case 19:
         fprintf(dst," %.10g",record->vr5picu);
         break;

       case 20:
         fprintf(dst," %.10g",record->tcicu);
         break;

       case 21:
         fprintf(dst," %.10g",record->v15psu);
         break;

       case 22:
         fprintf(dst," %.10g",record->v15msu);
         break;

       case 23:
         fprintf(dst," %.10g",record->v48psu);
         break;

       case 24:
         fprintf(dst," %.10g",record->v48msu);
         break;
 
       case 25:
         GetCharBits(record->status,bits8);

         fprintf(dst,"  ");
         loop(j,8)fprintf(dst,"%d",bits8[7-j]);
         fprintf(dst," ");
         break;

       case 26:
         fprintf(dst," %d",record->icu_blk_nr);
         break;
       case 27:
         fprintf(dst," %d",(long)record->Tenhz_count);
         break;
       case 28:
         fprintf(dst," %d",record->Mhz_count);
         break;
 
       default:
         fprintf(stderr,"\n Product Flag index %d is invalid!!!\n\n",i);
         exit(0);
      }
    }
  }
  fprintf(dst,"\n");
    
  return Success;
}
