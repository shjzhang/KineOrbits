#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Success 1
#define Failure 0

static char SccsId[] = "%Z% %M%       %I% %G";

boolean WriteGNV1AFRecord(FILE *dst, GNV1A_t *record)
/*----------------------------------------------------------------------------->
/ PURpose: write GPS Navigation 1A  Data Format record to file pointer dst
/
/ coded by: Jean E. Patterson                  06/13/00
/
/ input:  *dst    pointer to GPS Navigation 1A Data Format File
/         *record Pointer to GPS Navigation 1A Data struct (GNV1A_t)
<-----------------------------------------------------------------------------*/
{
  int     i;
  char   *ptr_prn_id;
  double *ptr_elev_prn;
  double *ptr_az_prn;
 
/*----------------------------------------------------------------------------->
/ Write Record elements to dst
<-----------------------------------------------------------------------------*/
  if (fwrite_grace(&record->rcv_time,sizeof(record->rcv_time),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'rcv_time' \n");
    return Failure;
  }

  if (fwrite_grace(&record->n_prns,sizeof(record->n_prns),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'n_prns' \n");
    return Failure;
  }


  if (fwrite_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,dst) != 1)
  {
    printf("GNV1A: Error writing field 'GRACE_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->chisq,sizeof(record->chisq),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'chisq' \n");
    return Failure;
  }

  if (fwrite_grace(&record->cov_mult,sizeof(record->cov_mult),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'cov_mult' \n");
    return Failure;
  }

  if (fwrite_grace(&record->voltage,sizeof(record->voltage),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'voltage' \n");
    return Failure;
  }

  if (fwrite_grace(&record->xpos,sizeof(record->xpos),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'xpos' \n");
    return Failure;
  }

  if (fwrite_grace(&record->ypos,sizeof(record->ypos),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'ypos' \n");
    return Failure;
  }

  if (fwrite_grace(&record->zpos,sizeof(record->zpos),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'zpos' \n");
    return Failure;
  }

  if (fwrite_grace(&record->xpos_err,sizeof(record->xpos_err),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'xpos_err' \n");
    return Failure;
  }

  if (fwrite_grace(&record->ypos_err,sizeof(record->ypos_err),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'ypos_err' \n");
    return Failure;
  }

  if (fwrite_grace(&record->zpos_err,sizeof(record->zpos_err),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'zpos_err' \n");
    return Failure;
  }

  if (fwrite_grace(&record->xvel,sizeof(record->xvel),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'xvel' \n");
    return Failure;
  }

  if (fwrite_grace(&record->yvel,sizeof(record->yvel),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'yvel' \n");
    return Failure;
  }

  if (fwrite_grace(&record->zvel,sizeof(record->zvel),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'zvel' \n");
    return Failure;
  }

  if (fwrite_grace(&record->xvel_err,sizeof(record->xvel_err),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'xvel_err' \n");
    return Failure;
  }

  if (fwrite_grace(&record->yvel_err,sizeof(record->yvel_err),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'yvel_err' \n");
    return Failure;
  }

  if (fwrite_grace(&record->zvel_err,sizeof(record->zvel_err),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'zvel_err' \n");
    return Failure;
  }

  if (fwrite_grace(&record->time_offset,sizeof(record->time_offset),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'time_offset' \n");
    return Failure;
  }

  if (fwrite_grace(&record->time_offset_err,sizeof(record->time_offset_err),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'time_offset_err' \n");
    return Failure;
  }

  if (fwrite_grace(&record->time_drift,sizeof(record->time_drift),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'time_drift' \n");
    return Failure;
  }

  if (fwrite_grace(&record->err_drift,sizeof(record->err_drift),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'err_drift' \n");
    return Failure;
  }

  if (fwrite_grace(&record->qualflg,sizeof(record->qualflg),1,dst) !=1)
  {
    printf("GNV1A: Error writing field 'qualflg' \n");
    return Failure;
  }


  /* This is not correct in form or syntax.  Fix this up once we decide 
     how to declare these data 
  */

  ptr_prn_id   = record->prn_id;
  ptr_elev_prn = record->el_prn;
  ptr_az_prn   = record->az_prn;
  
  for (i=0; i<record->n_prns; i++,ptr_prn_id++,ptr_elev_prn++,ptr_az_prn++)
  {
    if (fwrite_grace(ptr_prn_id,sizeof(record->prn_id[0]),1,dst) != 1)
    {
      printf("GNV1A: Error writing field 'prn_id %d' \n",i);
      return Failure;
    }
    if (fwrite_grace(ptr_elev_prn,sizeof(record->el_prn[0]),1,dst) != 1)
    {
      printf("GNV1A: Error writing field 'el_prn %d' \n",i);
      return Failure;
    }
    if (fwrite_grace(ptr_az_prn,sizeof(record->az_prn[0]),1,dst) != 1)
    {
      printf("GNV1A: Error writing field 'az_prn %d' \n",i);
      return Failure;
    }
  }
  return Success;
}
