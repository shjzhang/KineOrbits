#include <string.h>
#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0

static char SccsId[] = "$Id: ReadGNV1AFRecord.c,v 1.7 2004/08/30 21:03:35 wib Exp $";


boolean ReadGNV1AFRecord(FILE *src, GNV1A_t *record)
/*----------------------------------------------------------------------------->
/ purpose: read GPS Navigation 1A Data Format record from file pointer src
/
/ coded by: Jean E. Patterson  06/13/00 
/
/ input:  *src    pointer to GPS Navigation 1A Data Format File
/ output: *record Pointer to GPS Navigation 1A Data struct (GNV1A_t)
/
/ return:      1       normal return
/              0       End Of File reached
<-----------------------------------------------------------------------------*/
{
  unsigned long rcv_time;

  int     retrn;
  int     i;
  int     n_prns;


/*----------------------------------------------------------------------------->
/ Test for EOF
<-----------------------------------------------------------------------------*/

  retrn = fread_grace(&rcv_time,sizeof(rcv_time),1,src);
  if (feof(src) != 0) return False;
  if (retrn != 1)
  {
    printf("GNV1A: Error reading field 'rcv_time' \n");
    return Failure;
  }
 
  record->rcv_time = rcv_time;
  
/*----------------------------------------------------------------------------->
/ Read Record elements from src
<-----------------------------------------------------------------------------*/
  if (fread_grace(&record->n_prns,sizeof(record->n_prns),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'n_prns' \n");
    return Failure;
  }

  if (fread_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'GRACE_id' \n");
    return Failure;
  }

  if (fread_grace(&record->chisq,sizeof(record->chisq),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'chisq' \n");
    return Failure;
  }

  if (fread_grace(&record->cov_mult,sizeof(record->cov_mult),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'cov_mult' \n");
    return Failure;
  }

  if (fread_grace(&record->voltage,sizeof(record->voltage),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'voltage' \n");
    return Failure;
  }

  if (fread_grace(&record->xpos,sizeof(record->xpos),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'xpos' \n");
    return Failure;
  }

  if (fread_grace(&record->ypos,sizeof(record->ypos),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'ypos' \n");
    return Failure;
  }

  if (fread_grace(&record->zpos,sizeof(record->zpos),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'zpos' \n");
    return Failure;
  }

  if (fread_grace(&record->xpos_err,sizeof(record->xpos_err),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'xpos_err' \n");
    return Failure;
  }

  if (fread_grace(&record->ypos_err,sizeof(record->ypos_err),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'ypos_err' \n");
    return Failure;
  }

  if (fread_grace(&record->zpos_err,sizeof(record->zpos_err),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'zpos_err' \n");
    return Failure;
  }

  if (fread_grace(&record->xvel,sizeof(record->xvel),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'xvel' \n");
    return Failure;
  }

  if (fread_grace(&record->yvel,sizeof(record->yvel),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'yvel' \n");
    return Failure;
  }

  if (fread_grace(&record->zvel,sizeof(record->zvel),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'zveltime' \n");
    return Failure;
  }

  if (fread_grace(&record->xvel_err,sizeof(record->xvel_err),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'xvel_err' \n");
    return Failure;
  }

  if (fread_grace(&record->yvel_err,sizeof(record->yvel_err),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'yvel_err' \n");
    return Failure;
  }

  if (fread_grace(&record->zvel_err,sizeof(record->zvel_err),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'zvel_err' \n");
    return Failure;
  }

  if (fread_grace(&record->time_offset,sizeof(record->time_offset),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'time_offset' \n");
    return Failure;
  }

  if (fread_grace(&record->time_offset_err,sizeof(record->time_offset_err),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'time_offset_err' \n");
    return Failure;
  }

  if (fread_grace(&record->time_drift,sizeof(record->time_drift),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'time_drift' \n");
    return Failure;
  }

  if (fread_grace(&record->err_drift,sizeof(record->err_drift),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'err_drift' \n");
    return Failure;
  }

  if (fread_grace(&record->qualflg,sizeof(record->qualflg),1,src) != 1)
  {
    printf("GNV1A: Error reading field 'qualflg' \n");
    return Failure;
  }

  n_prns = (int)record->n_prns;  
  for (i = 0; i < n_prns; i++)
  {
    if (fread_grace(&record->prn_id[i],sizeof(record->prn_id[i]),1,src) != 1)
    {
      printf("GNV1A: Error reading field 'prn_id[%d]' \n",i);
      return Failure;
    }
    if (fread_grace(&record->el_prn[i],sizeof(record->el_prn[i]),1,src) != 1)
    {
      printf("GNV1A: Error reading field 'el_prn[%d]' \n",i);
      return Failure;
    }
    if (fread_grace(&record->az_prn[i],sizeof(record->az_prn[i]),1,src) != 1)
    {
      printf("GNV1A: Error reading field 'az_prn[%d]' \n",i);
      return Failure;
    }
  }

  return True;

}
