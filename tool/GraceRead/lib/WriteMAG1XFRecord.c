#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0

static char SccsId[] = "$Id: WriteMAG1XFRecord.c,v 1.6 2004/08/30 21:03:38 wib Exp $";




boolean WriteMAG1XFRecord(FILE *dst, MAG1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: write Magnetometer/Magnettorquer Level 1B Data Format record 
/          to file pointer dst
/
/ coded by: J. E. Patterson                  09/06/00
/
/
/ modified name of routine and structure     02/15/01
/
/ input:  *dst    Pointer to MAG1X Data Format File
/         *record Pointer to MAG1X Data struct (MAG1X_t)
<-----------------------------------------------------------------------------*/
{

 
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/
  if (fwrite_grace(&record->time_intg,sizeof(record->time_intg),1,dst) != 1)
  {
    printf("MAG1X: Error writing field 'time_intg' \n");
    return Failure;
  }

  if (fwrite_grace(&record->time_frac,sizeof(record->time_frac),1,dst) != 1)
  {
    printf("MAG1X: Error writing field 'time_frac' \n");
    return Failure;
  }

  if (fwrite_grace(&record->time_ref,sizeof(record->time_ref),1,dst) != 1)
  {
    printf("MAG1X: Error writing field 'time_ref' \n");
    return Failure;
  }

  if (fwrite_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,dst) != 1)
  {
    printf("MAG1X: Error writing field 'GRACE_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->MfvX_RAW,sizeof(record->MfvX_RAW),1,dst) != 1)
  {
    printf("MAG1X: Error writing field 'MfvX_RAW' \n");
    return Failure;
  }

  if (fwrite_grace(&record->MfvY_RAW,sizeof(record->MfvY_RAW),1,dst) != 1)
  {
    printf("MAG1X: Error writing field 'MfvY_RAW' \n");
    return Failure;
  }

  if (fwrite_grace(&record->MfvZ_RAW,sizeof(record->MfvZ_RAW),1,dst) != 1)
  {
    printf("MAG1X: Error writing field 'MfvZ_RAW' \n");
    return Failure;
  }

  if (fwrite_grace(&record->torque1A,sizeof(record->torque1A),1,dst) != 1)
  {
    printf("MAG1X: Error writing field 'torque1A' \n");
    return Failure;
  }

  if (fwrite_grace(&record->torque2A,sizeof(record->torque2A),1,dst) != 1)
  {
    printf("MAG1X: Error writing field 'torque2A' \n");
    return Failure;
  }

  if (fwrite_grace(&record->torque3A,sizeof(record->torque3A),1,dst) != 1)
  {
    printf("MAG1X: Error writing field 'torque3A' \n");
    return Failure;
  }

  if (fwrite_grace(&record->torque1B,sizeof(record->torque1B),1,dst) != 1)
  {
    printf("MAG1X: Error writing field 'torque1B' \n");
    return Failure;
  }

  if (fwrite_grace(&record->torque2B,sizeof(record->torque2B),1,dst) != 1)
  {
    printf("MAG1X: Error writing field 'torque2B' \n");
    return Failure;
  }

  if (fwrite_grace(&record->torque3B,sizeof(record->torque3B),1,dst) != 1)
  {
    printf("MAG1X: Error writing field 'torque3B' \n");
    return Failure;
  }

  if (fwrite_grace(&record->MF_BCalX,sizeof(record->MF_BCalX),1,dst) != 1)
  { 
    printf("MAG1X: Error writing field 'MF_BCalX' \n");
    return Failure;
  }
  
  if (fwrite_grace(&record->MF_BCalY,sizeof(record->MF_BCalY),1,dst) != 1)
  {
    printf("MAG1X: Error writing field 'MF_BCalY' \n");
    return Failure;
  }

  if (fwrite_grace(&record->MF_BCalZ,sizeof(record->MF_BCalZ),1,dst) != 1)
  {
    printf("MAG1X: Error writing field 'MF_BCalZ' \n");
    return Failure;
  }
  
  if (fwrite_grace(&record->torque_cal,sizeof(record->torque_cal),1,dst) != 1)
  {
    printf("MAG1X: Error writing field 'torque_cal' \n");
    return Failure;
  }

  if (fwrite_grace(&record->qualflg,sizeof(record->qualflg),1,dst) != 1)
  {
    printf("MAG1X: Error writing field 'qualflg' \n");
    return Failure;
  }


  return True;

}
