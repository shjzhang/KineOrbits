#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


static char SccsId[] = "$Id: PrintMAG1XFRecord.c,v 1.5 2004/08/30 21:03:34 wib Exp $";

void  PrintMAG1XFRecord(FILE *dst, MAG1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Print a detailed ascii description of Magnetic Torquer Level 1B 
/          Data Format record to file pointer dst
/
/ coded by: J. E. Patterson                  06/21/00
/
/ modified name of routine and structure     02/15/01
/
/ input:  *dst    Pointer to MAG1X Data Format File
/         *record Pointer to MAG1X Data struct (MAG1X_t)
<-----------------------------------------------------------------------------*/
{

 char string[3];

 strcpy(string,"-");

 
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/

  fprintf(dst," %-20s = %ld\n","record->time_intg",record->time_intg);
  fprintf(dst," %-20s = %ld\n","record->time_frac",record->time_frac);
  string[0] = record->time_ref;
  fprintf(dst," %-20s = %s\n","record->time_ref",string);
  strcpy(string,"-");
  string[0] = record->GRACE_id;
  fprintf(dst," %-20s = %s\n","record->GRACE_id",string);
  fprintf(dst," %-20s = %.16g\n","record->MfvX_RAW",record->MfvX_RAW);
  fprintf(dst," %-20s = %.16g\n","record->MfvY_RAW",record->MfvY_RAW);
  fprintf(dst," %-20s = %.16g\n","record->MfvZ_RAW",record->MfvZ_RAW);
  fprintf(dst," %-20s = %.16g\n","record->torque1A",record->torque1A);
  fprintf(dst," %-20s = %.16g\n","record->torque2A",record->torque2A);
  fprintf(dst," %-20s = %.16g\n","record->torque3A",record->torque3A);
  fprintf(dst," %-20s = %.16g\n","record->torque1B",record->torque1B);
  fprintf(dst," %-20s = %.16g\n","record->torque2B",record->torque2B);
  fprintf(dst," %-20s = %.16g\n","record->torque3B",record->torque3B);
  fprintf(dst," %-20s = %.16g\n","record->MF_BCalX",record->MF_BCalX);
  fprintf(dst," %-20s = %.16g\n","record->MF_BCalY",record->MF_BCalY);
  fprintf(dst," %-20s = %.16g\n","record->MF_BCalZ",record->MF_BCalZ);
  fprintf(dst," %-20s = %.16g\n","record->torque_cal",record->torque_cal);
  fprintf(dst," %-20s = %d\n","record->qualflg",record->qualflg);


}
