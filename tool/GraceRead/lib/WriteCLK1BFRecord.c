#include "GRACEiolib.h"

#define Success 1
#define Failure 0


static char SccsId[] = "$Id: WriteCLK1BFRecord.c,v 1.5 2004/08/30 21:03:38 wib Exp $";


boolean WriteCLK1BFRecord(FILE *dst, CLK1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: write Clock offset Level 1B Data Format record to file pointer dst
/
/ coded by: J. E. Patterson                  06/21/00
/
/ input:  *dst    Pointer to CLK1B Data Format File
/         *record Pointer to CLK1B Data struct (CLK1B_t)
<-----------------------------------------------------------------------------*/
{

 
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/
  if (fwrite_grace(&record->rcv_time,sizeof(record->rcv_time),1,dst) != 1)
  {
    printf("CLK1B: Error writing field 'rcv_time' \n");
    return Failure;
  }

  if (fwrite_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,dst) != 1)
  {
    printf("CLK1B: Error writing field 'GRACE_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->clock_id,sizeof(record->clock_id),1,dst) != 1)
  {
    printf("CLK1B: Error writing field 'clock_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->eps_time,sizeof(record->eps_time),1,dst) != 1)
  {
    printf("CLK1B: Error writing field 'eps_time' \n");
    return Failure;
  }

  if (fwrite_grace(&record->eps_err,sizeof(record->eps_err),1,dst) != 1)
  {
    printf("CLK1B: Error writing field 'eps_err' \n");
    return Failure;
  }

  if (fwrite_grace(&record->eps_drift,sizeof(record->eps_drift),1,dst) != 1)
  {
    printf("CLK1B: Error writing field 'eps_drift' \n");
    return Failure;
  }

  if (fwrite_grace(&record->drift_err,sizeof(record->drift_err),1,dst) != 1)
  {
    printf("CLK1B: Error writing field 'drift_err' \n");
    return Failure;
  }

  if (fwrite_grace(&record->qualflg,sizeof(record->qualflg),1,dst) != 1)
  {
    printf("CLK1B: Error writing field 'qualflg' \n");
    return Failure;
  }


  return True;

}
