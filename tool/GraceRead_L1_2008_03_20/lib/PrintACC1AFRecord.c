#include <string.h>
#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


#define NBITSMAX 32 

static char SccsId[] = "$Id: PrintACC1AFRecord.c,v 1.9 2004/08/30 21:03:34 wib Exp $";

void PrintACC1AFRecord(FILE *dst, ACC1A_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Print a detailed ascii description of Accelerometer Level 1A 
/          Data Format record to file pointer dst
/
/ coded by: J. E. Patterson                         06/14/00
/
/ input:  *dst    Pointer to ACC1A Data Format File
/         *record Pointer to ACC1A Data struct (ACC1A_t)
<-----------------------------------------------------------------------------*/
{

 long i;
 int  j;
 char bits[NBITSMAX];
 char bits8[8];
 
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/
  fprintf(dst," %-20s = %ld\n","record->rcvtime_intg",record->rcvtime_intg);
  fprintf(dst," %-20s = %ld\n","record->rcvtime_frac",record->rcvtime_frac);
  fprintf(dst," %-20s = %c\n","record->time_ref",record->time_ref);
  fprintf(dst," %-20s = %c\n","record->GRACE_id",record->GRACE_id);
  fprintf(dst," %-20s = %d\n","record->qualflg",record->qualflg);
  fprintf(dst," %-20s = %ld [","record->prod_flag",record->prod_flag);
/*----------------------------------------------------------------------------->
/ Decode Product flag 
<-----------------------------------------------------------------------------*/
  GetLongBits(record->prod_flag,bits);
  loop(i,NBITSMAX)fprintf(dst,"%d",bits[31-i]);  
  fprintf(stderr,"]\n");


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
               fprintf(dst," %-20s = %.16g\n","record->lin_accl_x",record->lin_accl_x);
               break;
       case  1:
               fprintf(dst," %-20s = %.16g\n","record->lin_accl_y",record->lin_accl_y);
               break;
       case  2:
               fprintf(dst," %-20s = %.16g\n","record->lin_accl_z",record->lin_accl_z);
               break;
       case  3:
               fprintf(dst," %-20s = %.16g\n","record->ang_accl_x",record->ang_accl_x);
               break;
       case  4:
               fprintf(dst," %-20s = %.16g\n","record->ang_accl_y",record->ang_accl_y);
               break;
       case  5:
               fprintf(dst," %-20s = %.16g\n","record->ang_accl_z",record->ang_accl_z);
               break;
       case  6:
               fprintf(dst," %-20s = %.16g\n","record->bias_vol",record->bias_vol);
               break;
       case  7:
               fprintf(dst," %-20s = %.10g\n","record->vd",record->vd);
               break;
       case  8:
               fprintf(dst," %-20s = %.10g\n","record->x1_out",record->x1_out);
               break;
       case  9:
               fprintf(dst," %-20s = %.10g\n","record->x2_out",record->x2_out);
               break;
       case 10:
               fprintf(dst," %-20s = %.10g\n","record->x3_out",record->x3_out);
               break;
       case 11:
               fprintf(dst," %-20s = %.10g\n","record->y1_out",record->y1_out);
               break;
       case 12:
               fprintf(dst," %-20s = %.10g\n","record->y2_out",record->y2_out);
               break;
       case 13:
               fprintf(dst," %-20s = %.10g\n","record->z1_out",record->z1_out);
               break;
       case 14:
               fprintf(dst," %-20s = %.10g\n","record->tesu",record->tesu);
               break;
       case 15:
               fprintf(dst," %-20s = %.10g\n","record->taicu",record->taicu);
               break;
       case 16:
               fprintf(dst," %-20s = %.10g\n","record->tisu",record->tisu);
               break;
       case 17:
               fprintf(dst," %-20s = %.10g\n","record->v15picu",record->v15picu);
               break;
       case 18:
               fprintf(dst," %-20s = %.10g\n","record->v15micu",record->v15micu);
               break;
       case 19:
               fprintf(dst," %-20s = %.10g\n","record->vr5picu",record->vr5picu);
               break;
       case 20:
               fprintf(dst," %-20s = %.10g\n","record->tcicu",record->tcicu);
               break;
       case 21:
               fprintf(dst," %-20s = %.10g\n","record->v15psu",record->v15psu);
               break;
       case 22:
               fprintf(dst," %-20s = %.10g\n","record->v15msu",record->v15msu);
               break;
       case 23:
               fprintf(dst," %-20s = %.10g\n","record->v48psu",record->v48psu);
               break;
       case 24:
               fprintf(dst," %-20s = %.10g\n","record->v48msu",record->v48msu);
               break;
       case 25:
               GetCharBits(record->status,bits8);
               fprintf(dst," %-20s = %ld [","record->status",record->status);
               loop(j,8)fprintf(dst,"%d",bits8[7-j]);  
               fprintf(stderr,"]\n");
               break;
       case 26:
               fprintf(dst," %-20s = %d\n","record->icu_blk_nr",record->icu_blk_nr);
               break;
       case 27:
               fprintf(dst," %-20s = %d\n","record->Tenhz_count",(long)record->Tenhz_count);
               break;
       case 28:
               fprintf(dst," %-20s = %d\n","record->Mhz_count",record->Mhz_count);
               break;
       default:
               fprintf(stderr,"\n Product Flag index %d in PrintACC1A is invalid!!!\n\n",i);
               exit(0);
      }
    }
  }
}
