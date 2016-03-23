#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


#define Failure 0


static char SccsId[] = "$Id: ReadCLK1BFRecord.c,v 1.5 2004/08/30 21:03:35 wib Exp $";



boolean ReadCLK1BFRecord(FILE *src, CLK1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: read Clock Level 1B Data Format record from file 
/          pointer src
/
/ Initial version: J. E. Patterson             09/06/00
/
/
/ input:  *src    Pointer to CLK1B Data Format File
/ output: *record Pointer to CLK1B Data struct (CLK1B_t)
/
/ return:      1       normal return
/              0       End Of File reached
<-----------------------------------------------------------------------------*/
{
  unsigned long rcv_time;

  int           retrn;


/*----------------------------------------------------------------------------->
/ Test for EOF
<-----------------------------------------------------------------------------*/

  retrn = fread_grace(&rcv_time,sizeof(record->rcv_time),1,src);
  if (feof(src) != 0) return False;
  if (retrn != 1)
  {
    printf("CLK1B: Error reading field 'rcv_time' \n");
    return Failure;
  }
  record->rcv_time = rcv_time;
  
/*----------------------------------------------------------------------------->
/ Read remaining record elements from src
<-----------------------------------------------------------------------------*/

  if (fread_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,src) != 1)
  {
    printf("CLK1B: Error reading field 'GRACE_id' \n");
    return Failure;
  }

  if (fread_grace(&record->clock_id,sizeof(record->clock_id),1,src) != 1)
  {
    printf("CLK1B: Error reading field 'clock_id' \n");
    return Failure;
  }

  if (fread_grace(&record->eps_time,sizeof(record->eps_time),1,src) != 1)
  {
    printf("CLK1B: Error reading field 'eps_time' \n");
    return Failure;
  }

  if (fread_grace(&record->eps_err,sizeof(record->eps_err),1,src) != 1)
  {
    printf("CLK1B: Error reading field 'eps_err' \n");
    return Failure;
  }
  
  if (fread_grace(&record->eps_drift,sizeof(record->eps_drift),1,src) != 1)
  {
    printf("CLK1B: Error reading field 'eps_drift' \n");
    return Failure;
  }

  if (fread_grace(&record->drift_err,sizeof(record->drift_err),1,src) != 1)
  {
    printf("CLK1B: Error reading field 'drift_err' \n");
    return Failure;
  }

  if (fread_grace(&record->qualflg,sizeof(record->qualflg),1,src) != 1)
  {
    printf("CLK1B: Error reading field 'qualflg' \n");
    return Failure;
  }


  return True;

}
