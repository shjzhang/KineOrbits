#include <string.h>
#include "GRACEiolib.h"
#include "GRACEio_utils.h"
#include "GRACEio_prototypes.h"


#define NBITSMAX 32 

#define Failure 0

static char SccsId[] = "$Id: ReadACC1AFRecord.c,v 1.10 2004/08/30 21:03:35 wib Exp $";

boolean ReadACC1AFRecord(FILE *src, ACC1A_t *record)
/*----------------------------------------------------------------------------->
/ purpose: reading Accelerometer Level 1A Data Format record from file 
/          pointer src
/
/ coded by: J. E. Patterson         06/14/00
/
/ input:  *src    Pointer to ACC1A Data Format File
/ output: *record Pointer to ACC1A Data struct (ACC1A_t)
/
/ return:      1       normal return
/              0       End Of File reached
'-----------------------------------------------------------------------------*/
{
  unsigned long rcvtime_intg;

  int           retrn;

  long          i;

  char          bits[NBITSMAX];

/*----------------------------------------------------------------------------->
/ Test for EOF
'-----------------------------------------------------------------------------*/

  retrn = fread_grace(&rcvtime_intg,sizeof(rcvtime_intg),1,src);
  if (feof(src) != 0) return False;
  if (retrn != 1)
  {
    printf("ACC1A: Error reading field 'rcvtime_intg' \n");
    return Failure;
  }
  record->rcvtime_intg = rcvtime_intg;
  
/*----------------------------------------------------------------------------->
/ reading remaining record elements from src
'-----------------------------------------------------------------------------*/
  if (fread_grace(&record->rcvtime_frac,sizeof(record->rcvtime_frac),1,src) != 1)
  {
    printf("ACC1A: Error reading field 'rcvtime_frac' \n");
    return Failure;
  }

  if (fread_grace(&record->time_ref,sizeof(record->time_ref),1,src) != 1)
  {
    printf("ACC1A: Error reading field 'time_ref' \n");
    return Failure;
  }

  if (fread_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,src) != 1)
  {
    printf("ACC1A: Error reading field 'GRACE_id' \n");
    return Failure;
  }

  if (fread_grace(&record->qualflg,sizeof(record->qualflg),1,src) != 1)
  {
    printf("ACC1A: Error reading field 'qualflg' \n");
    return Failure;
  }

  if (fread_grace(&record->prod_flag,sizeof(record->prod_flag),1,src) !=1)
  {
    printf("ACC1A: Error reading field 'prod_flag' \n");
    return Failure;
  }

/*----------------------------------------------------------------------------->
/ Decode Product flag 
'-----------------------------------------------------------------------------*/

  GetLongBits(record->prod_flag,bits);

/*----------------------------------------------------------------------------->
/ reading all product specified by prod_flag from src
'-----------------------------------------------------------------------------*/

  loop(i,NBITSMAX)
  {
    if (bits[i] == 1)
    {
      switch(i)
      {
       case  0:
          if (fread_grace(&record->lin_accl_x,sizeof(record->lin_accl_x),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'lin_accl_x' \n");
            return Failure;
          }
          break;
       case  1:
          if (fread_grace(&record->lin_accl_y,sizeof(record->lin_accl_y),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'lin_accl_y' \n");
            return Failure;
          }
          break;
       case  2:
          if (fread_grace(&record->lin_accl_z,sizeof(record->lin_accl_z),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'lin_accl_z' \n");
            return Failure;
          }
          break;
       case  3:
          if (fread_grace(&record->ang_accl_x,sizeof(record->ang_accl_x),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'ang_accl_x' \n");
            return Failure;
          }
          break;
       case  4:
          if (fread_grace(&record->ang_accl_y,sizeof(record->ang_accl_y),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'ang_accl_y' \n");
            return Failure;
          }
          break;
       case  5:
          if (fread_grace(&record->ang_accl_z,sizeof(record->ang_accl_z),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'ang_accl_z' \n");
            return Failure;
          }
          break;
       case  6:
          if (fread_grace(&record->bias_vol,sizeof(record->bias_vol),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'bias_vol' \n");
            return Failure;
          }
          break;
       case 07:
          if (fread_grace(&record->vd,sizeof(record->vd),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'vd' \n");
            return Failure;
          }
          break;
       case  8:
          if (fread_grace(&record->x1_out,sizeof(record->x1_out),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'x1_out' \n");
            return Failure;
          }
          break;
       case  9:
          if (fread_grace(&record->x2_out,sizeof(record->x2_out),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'x2_out' \n");
            return Failure;
          }
          break;
       case 10:
          if (fread_grace(&record->x3_out,sizeof(record->x3_out),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'x3_out' \n");
            return Failure;
          }
          break;
       case 11:
          if (fread_grace(&record->y1_out,sizeof(record->y1_out),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'y1_out' \n");
            return Failure;
          }
          break;
       case 12:
          if (fread_grace(&record->y2_out,sizeof(record->y2_out),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'y2_out' \n");
            return Failure;
          }
          break;
       case 13:
          if (fread_grace(&record->z1_out,sizeof(record->z1_out),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'z1_out' \n");
            return Failure;
          }
          break;
       case 14:
          if (fread_grace(&record->tesu,sizeof(record->tesu),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'tesu' \n");
            return Failure;
          }
          break;
       case 15:
          if (fread_grace(&record->taicu,sizeof(record->taicu),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'taicu' \n");
            return Failure;
          }
          break;
       case 16:
          if (fread_grace(&record->tisu,sizeof(record->tisu),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'tisu' \n");
            return Failure;
          }
          break;
       case 17:
          if (fread_grace(&record->v15picu,sizeof(record->v15picu),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'v15picu' \n");
            return Failure;
          }
          break;
       case 18:
          if (fread_grace(&record->v15micu,sizeof(record->v15micu),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'v15micu' \n");
            return Failure;
          }
          break;
       case 19:
          if (fread_grace(&record->vr5picu,sizeof(record->vr5picu),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'vr5picu' \n");
            return Failure;
          }
          break;
       case 20:
          if (fread_grace(&record->tcicu,sizeof(record->tcicu),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'tcicu' \n");
            return Failure;
          }
          break;
       case 21:
          if (fread_grace(&record->v15psu,sizeof(record->v15psu),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'v15psu' \n");
            return Failure;
          }
          break;
       case 22:
          if (fread_grace(&record->v15msu,sizeof(record->v15msu),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'v15msu' \n");
            return Failure;
          }
          break;
       case 23:
          if (fread_grace(&record->v48psu,sizeof(record->v48psu),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'v48psu' \n");
            return Failure;
          }
          break;
       case 24:
          if (fread_grace(&record->v48msu,sizeof(record->v48msu),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'v48msu' \n");
            return Failure;
          }
          break;
       case 25:
          if (fread_grace(&record->status,sizeof(record->status),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'status' \n");
            return Failure;
          }
          break;
       case 26:
          if (fread_grace(&record->icu_blk_nr,sizeof(record->icu_blk_nr),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'icu_blk_nr' \n");
            return Failure;
          }
          break;
       case 27:
          if (fread_grace(&record->Tenhz_count,sizeof(record->Tenhz_count),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'Tenhz_count' \n");
            return Failure;
          }
          break;
       case 28:
          if (fread_grace(&record->Mhz_count,sizeof(record->Mhz_count),1,src) != 1)
          {
            printf("ACC1A: Error reading field 'Mhz_count' \n");
            return Failure;
          }
          break;
       default:
          fprintf(stderr,"Product Flag index %d in ReadACC1A is invalid!!! \n\n",i);
          exit(0);
      }
    }
  }

  return True;

}
