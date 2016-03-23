#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Success 1 
#define Failure 0

static char SccsId[] = "$Id: WrAsciiIOA1BFRecord.c,v 1.5 2004/08/30 21:03:37 wib Exp $";


boolean WrAsciiIOA1BFRecord(FILE *dst, IOA1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Dump ascii records of the Inertial Orientation of the
/          SuperSTAR Accelerometer from the file pointed to by dst 
/
/ coded by: J. E. Patterson                  07/18/00
/
/ modified name of routine and structure     02/15/01
/
/ input:  *dst    Pointer to IOA1B Data Format File
/         *record Pointer to IOA1B Data struct (IOA1B_t)
<-----------------------------------------------------------------------------*/
{
 char string[3];
 char bits8[8];

 long i;

 GetCharBits(record->qualflg,bits8);

 strcpy(string,"-");
 string[0] = record->GRACE_id;                                                          

 fprintf(dst,"%d %s %.16g %.16g %.16g %.16g", 
         record->gps_time, string, record->quatangle, 
         record->quaticoeff, record->quatjcoeff, record->quatkcoeff);

 fprintf(dst,"  ");
 loop(i,8)fprintf(dst,"%d",bits8[7-i]);
 fprintf(dst,"\n");
 
 return Success;
}
