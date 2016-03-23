#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


static char SccsId[] = "$Id: PrintACC1BFRecord.c,v 1.7 2004/08/30 21:03:34 wib Exp $";


void PrintACC1BFRecord(FILE *dst, ACC1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Print a detailed ascii description of Accelerometer Level 1B 
/          Data Format record to file pointer dst
/
/ coded by: J. E. Patterson                         06/21/00
/ modified: Gerhard L.H. Kruizinga                  01/03/02
/
/ input:  *dst    Pointer to ACC1B Data Format File
/         *record Pointer to ACC1B Data struct (ACC1B_t)
<-----------------------------------------------------------------------------*/
{
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/
 fprintf(dst," %-20s = %ld\n","record->gps_time",record->gps_time);
 fprintf(dst," %-20s = %c\n","record->GRACE_id",record->GRACE_id);
 fprintf(dst," %-20s = %le\n","record->lin_accl_x",record->lin_accl_x);
 fprintf(dst," %-20s = %le\n","record->lin_accl_y",record->lin_accl_y);
 fprintf(dst," %-20s = %le\n","record->lin_accl_z",record->lin_accl_z);
 fprintf(dst," %-20s = %le\n","record->ang_accl_x",record->ang_accl_x);
 fprintf(dst," %-20s = %le\n","record->ang_accl_y",record->ang_accl_y);
 fprintf(dst," %-20s = %le\n","record->ang_accl_z",record->ang_accl_z);
 fprintf(dst," %-20s = %le\n","record->acl_x_res",record->acl_x_res);
 fprintf(dst," %-20s = %le\n","record->acl_y_res",record->acl_y_res);
 fprintf(dst," %-20s = %le\n","record->acl_z_res",record->acl_z_res);
 fprintf(dst," %-20s = %d\n","record->qualflg",record->qualflg);


}
