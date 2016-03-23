#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Success 1 
#define Failure 0


static char SccsId[] = "$Id: WrAsciiACC1BFRecord.c,v 1.8 2004/08/30 21:03:37 wib Exp $";


boolean WrAsciiACC1BFRecord(FILE *dst, ACC1B_t *record)
/*-----------------------------------------------------------------------------'
/ purpose: Dump ascii records of SuperSTAR Acceleromenter data 
/          from the file pointed to by dst 
/
/ coded by: J. E. Patterson                         07/18/00
/ modified: Gerhard L.H. Kruizinga                  01/03/02
/
/ input:  *dst    Pointer to ACC1BF Data Format File
/         *record Pointer to ACC1BF Data struct (ACC1B_t)
'-----------------------------------------------------------------------------*/
{
 char string[3];
 char bits8[8];

 long i;

 GetCharBits(record->qualflg,bits8);

 strcpy(string,"-");
 string[0] = record->GRACE_id;

 fprintf(dst,"%d %s %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g", 
         record->gps_time, string, 
         record->lin_accl_x, record->lin_accl_y, record->lin_accl_z, 
         record->ang_accl_x, record->ang_accl_y, record->ang_accl_z,
         record->acl_x_res, record->acl_y_res, record->acl_z_res); 

 fprintf(dst,"  ");
 loop(i,8)fprintf(dst,"%d",bits8[7-i]);
 fprintf(dst,"\n");
  
 return Success;
}
