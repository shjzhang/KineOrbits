#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0

static char SccsId[] = "$Id: ReadMAG1XFRecord.c,v 1.7 2004/08/30 21:03:36 wib Exp $";



boolean ReadMAG1XFRecord(FILE *src, MAG1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: read Magnetometer/Magnettorquer Level 1B Data Format record from 
/          file pointer src
/
/ Initial version: J. E. Patterson           09/06/00
/
/ modified name of routine and structure     02/15/01
/
/
/ input:  *src    Pointer to MAG1X Data Format File
/ output: *record Pointer to MAG1X Data struct (MAG1X_t)
/
/ return:      1       normal return
/              0       End Of File reached
<-----------------------------------------------------------------------------*/
{
  unsigned long time_intg;

  int    retrn;


/*----------------------------------------------------------------------------->
/ Test for EOF
<-----------------------------------------------------------------------------*/

  retrn = fread_grace(&time_intg,sizeof(record->time_intg),1,src);
  if (feof(src) != 0) return False;
  if (retrn != 1)
  {
    printf("MAG1X: Error reading field 'time_intg' \n");
    return Failure;
  }
  record->time_intg = time_intg;
  
/*----------------------------------------------------------------------------->
/ Read remaining record elements from src
<-----------------------------------------------------------------------------*/

  if (fread_grace(&record->time_frac,sizeof(record->time_frac),1,src) != 1)
  {
    printf("MAG1X: Error reading field 'time_frac' \n");
    return Failure;
  }

  if (fread_grace(&record->time_ref,sizeof(record->time_ref),1,src) != 1)
  {
    printf("MAG1X: Error reading field 'time_ref' \n");
    return Failure;
  }

  if (fread_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,src) != 1)
  {
    printf("MAG1X: Error reading field 'GRACE_id' \n");
    return Failure;
  }

  if (fread_grace(&record->MfvX_RAW,sizeof(record->MfvX_RAW),1,src) != 1)
  {
    printf("MAG1X: Error reading field 'MFvX_RAW' \n");
    return Failure;
  }

  if (fread_grace(&record->MfvY_RAW,sizeof(record->MfvY_RAW),1,src) != 1)
  {
    printf("MAG1X: Error reading field 'MFvY_RAW' \n");
    return Failure;
  }

  if (fread_grace(&record->MfvZ_RAW,sizeof(record->MfvZ_RAW),1,src) != 1)
  {
    printf("MAG1X: Error reading field 'MFvZ_RAW' \n");
    return Failure;
  }

  if (fread_grace(&record->torque1A,sizeof(record->torque1A),1,src) != 1)
  {
    printf("MAG1X: Error reading field 'torque1A' \n");
    return Failure;
  }

  if (fread_grace(&record->torque2A,sizeof(record->torque2A),1,src) != 1)
  {
    printf("MAG1X: Error reading field 'torque2A' \n");
    return Failure;
  }

  if (fread_grace(&record->torque3A,sizeof(record->torque3A),1,src) != 1)
  {
    printf("MAG1X: Error reading field 'torque3A' \n");
    return Failure;
  }

  if (fread_grace(&record->torque1B,sizeof(record->torque1B),1,src) != 1)
  {
    printf("MAG1X: Error reading field 'torque1B' \n");
    return Failure;
  }

  if (fread_grace(&record->torque2B,sizeof(record->torque2B),1,src) != 1)
  {
    printf("MAG1X: Error reading field 'torque2B' \n");
    return Failure;
  }

  if (fread_grace(&record->torque3B,sizeof(record->torque3B),1,src) != 1)
  {
    printf("MAG1X: Error reading field 'torque3B' \n");
    return Failure;
  }

  if (fread_grace(&record->MF_BCalX,sizeof(record->MF_BCalX),1,src) != 1)
  {
    printf("MAG1X: Error reading field 'MF_BCalX' \n");
    return Failure;
  }

  if (fread_grace(&record->MF_BCalY,sizeof(record->MF_BCalY),1,src) != 1)
  {
    printf("MAG1X: Error reading field 'MF_BCalY' \n");
    return Failure;
  }

  if (fread_grace(&record->MF_BCalZ,sizeof(record->MF_BCalZ),1,src) != 1)
  {
    printf("MAG1X: Error reading field 'MF_BCalZ' \n");
    return Failure;
  }

  if (fread_grace(&record->torque_cal,sizeof(record->torque_cal),1,src) != 1)
  {
    printf("MAG1X: Error reading field 'torque_cal' \n");
    return Failure;
  }

  if (fread_grace(&record->qualflg,sizeof(record->qualflg),1,src) != 1)
  {
    printf("MAG1X: Error reading field 'qualflg' \n");
    return Failure;
  }


  return True;

}
