#include "GRACEiolib.h"
#include "GRACEio_utils.h"
#include "GRACEio_prototypes.h"

#define Failure 0


#define NBITSMAX 32 

static char SccsId[] = "$Id: WriteACC1AFRecord.c,v 1.9 2004/08/30 21:03:38 wib Exp $";



boolean WriteACC1AFRecord(FILE *dst, ACC1A_t *record)
/*----------------------------------------------------------------------------->
/ purpose: write Accelerometer Level 1A Data Format record to file pointer dst
/
/ coded by: J. E. Patterson                  06/14/00
/
/ input:  *dst    Pointer to ACC1A Data Format File
/         *record Pointer to ACC1A Data struct (ACC1A_t)
<-----------------------------------------------------------------------------*/
{

 long i;

 char bits[NBITSMAX];
 
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/
  if (fwrite_grace(&record->rcvtime_intg,sizeof(record->rcvtime_intg),1,dst) != 1)
  {
    printf("ACC1A: Error writing field 'rcvtime_intg' \n");
    return Failure;
  }

  if (fwrite_grace(&record->rcvtime_frac,sizeof(record->rcvtime_frac),1,dst) != 1)
  {
    printf("ACC1A: Error writing field 'rcvtime_frac' \n");
    return Failure;
  }

  if (fwrite_grace(&record->time_ref,sizeof(record->time_ref),1,dst) != 1)
  {
    printf("ACC1A: Error writing field 'time_ref' \n");
    return Failure;
  }

  if (fwrite_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,dst) != 1)
  {
    printf("ACC1A: Error writing field 'GRACE_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->qualflg,sizeof(record->qualflg),1,dst) != 1)
  {
    printf("ACC1A: Error writing field 'qualflg' \n");
    return Failure;
  }

  if (fwrite_grace(&record->prod_flag,sizeof(record->prod_flag),1,dst) != 1)
  {
    printf("ACC1A: Error writing field 'prod_flag' \n");
    return Failure;
  }


/*----------------------------------------------------------------------------->
/ Decode Product flag 
<-----------------------------------------------------------------------------*/

  GetLongBits(record->prod_flag,bits);

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
         if (fwrite_grace(&record->lin_accl_x,sizeof(record->lin_accl_x),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'lin_accl_x' \n");
           return Failure;
         }

         break;
       case  1:
         if (fwrite_grace(&record->lin_accl_y,sizeof(record->lin_accl_y),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'lin_accl_y' \n");
           return Failure;
         }

         break;
       case  2:
         if (fwrite_grace(&record->lin_accl_z,sizeof(record->lin_accl_z),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'lin_accl_z' \n");
           return Failure;
         }

         break;
       case  3:
         if (fwrite_grace(&record->ang_accl_x,sizeof(record->ang_accl_x),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'ang_accl_x' \n");
           return Failure;
         }

         break;
       case  4:
         if (fwrite_grace(&record->ang_accl_y,sizeof(record->ang_accl_y),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'ang_accl_y' \n");
           return Failure;
         }

         break;
       case  5:
         if (fwrite_grace(&record->ang_accl_z,sizeof(record->ang_accl_z),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'ang_accl_z' \n");
           return Failure;
         }

         break;
       case  6:
         if (fwrite_grace(&record->bias_vol,sizeof(record->bias_vol),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'bias_vol' \n");
           return Failure;
         }

         break;
       case  7:
         if (fwrite_grace(&record->vd,sizeof(record->vd),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'vd' \n");
           return Failure;
         }

         break;
       case  8:
         if (fwrite_grace(&record->x1_out,sizeof(record->x1_out),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'x1_out' \n");
           return Failure;
         }

         break;
       case  9:
         if (fwrite_grace(&record->x2_out,sizeof(record->x2_out),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'x2_out' \n");
           return Failure;
         }

         break;
       case 10:
         if (fwrite_grace(&record->x3_out,sizeof(record->x3_out),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'x3_out' \n");
           return Failure;
         }

         break;
       case 11:
         if (fwrite_grace(&record->y1_out,sizeof(record->y1_out),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'y1_out' \n");
           return Failure;
         }

         break;
       case 12:
         if (fwrite_grace(&record->y2_out,sizeof(record->y2_out),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'y2_out' \n");
           return Failure;
         }

         break;
       case 13:
         if (fwrite_grace(&record->z1_out,sizeof(record->z1_out),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'z1_out' \n");
           return Failure;
         }

         break;
       case 14:
         if (fwrite_grace(&record->tesu,sizeof(record->tesu),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'tesu' \n");
           return Failure;
         }

         break;
       case 15:
         if (fwrite_grace(&record->taicu,sizeof(record->taicu),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'taicu' \n");
           return Failure;
         }

         break;
       case 16:
         if (fwrite_grace(&record->tisu,sizeof(record->tisu),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'tisu' \n");
           return Failure;
         }

         break;
       case 17:
         if (fwrite_grace(&record->v15picu,sizeof(record->v15picu),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'v15picu' \n");
           return Failure;
         }

         break;
       case 18:
         if (fwrite_grace(&record->v15micu,sizeof(record->v15micu),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'v15micu' \n");
           return Failure;
         }

         break;
       case 19:
         if (fwrite_grace(&record->vr5picu,sizeof(record->vr5picu),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'vr5picu' \n");
           return Failure;
         }

         break;
       case 20:
         if (fwrite_grace(&record->tcicu,sizeof(record->tcicu),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'tcicu' \n");
           return Failure;
         }

         break;
       case 21:
         if (fwrite_grace(&record->v15psu,sizeof(record->v15psu),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'v15psu' \n");
           return Failure;
         }

         break;
       case 22:
         if (fwrite_grace(&record->v15msu,sizeof(record->v15msu),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'v15msu' \n");
           return Failure;
         }

         break;
       case 23:
         if (fwrite_grace(&record->v48psu,sizeof(record->v48psu),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'v48psu' \n");
           return Failure;
         }

         break;
       case 24:
         if (fwrite_grace(&record->v48msu,sizeof(record->v48msu),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'v48msu' \n");
           return Failure;
         }

         break;
       case 25:
         if (fwrite_grace(&record->status,sizeof(record->status),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'status' \n");
           return Failure;
         }

         break;
       case 26:
         if (fwrite_grace(&record->icu_blk_nr,sizeof(record->icu_blk_nr),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'icu_blk_nr' \n");
           return Failure;
         }

         break;
       case 27:
         if (fwrite_grace(&record->Tenhz_count,sizeof(record->Tenhz_count),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'Tenhz_count' \n");
           return Failure;
         }

         break;
       case 28:
         if (fwrite_grace(&record->Mhz_count,sizeof(record->Mhz_count),1,dst) != 1)
         {
           printf("ACC1A: Error writing field 'Mhz_count' \n");
           return Failure;
         }

         break;
       default:
         fprintf(stderr,"\n Product Flag index %d in WriteACC1A is invalid!!!\n\n",i);
         exit(0);
      }
    }
  }

  return True;
}
