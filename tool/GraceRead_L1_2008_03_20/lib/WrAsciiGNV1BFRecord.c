#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Success 1 
#define Failure 0


static char SccsId[] = "$Id: WrAsciiGNV1BFRecord.c,v 1.7 2004/08/30 21:03:37 wib Exp $";     


boolean WrAsciiGNV1BFRecord(FILE *dst, GNV1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Dump ascii records of GPS Navigation Level 1B data 
/          from the file pointed to by dst 
/
/ coded by: J. E. Patterson                         07/18/00
/
/ input:  *dst    Pointer to GNV1BF Data Format File
/         *record Pointer to GNV1BF Data struct (GNV1B_t)
<-----------------------------------------------------------------------------*/
{
 char string1[3];
 char string2[3];
 char bits8[8];

 long i;

 GetCharBits(record->qualflg,bits8);

 strcpy(string1,"-");
 string1[0] = record->GRACE_id;                                                          

 strcpy(string2,"-");
 string2[0] = record->coord_ref;                                                          

 fprintf(dst,"%d %s %s %.16g %.16g %.16g %.16g %.16g"
        " %.16g %.16g %.16g %.16g %.16g %.16g %.16g",
         record->gps_time, string1, string2, record->xpos, 
         record->ypos, record->zpos, record->xpos_err, 
         record->ypos_err, record->zpos_err, record->xvel, 
         record->yvel, record->zvel, record->xvel_err, record->yvel_err, 
         record->zvel_err);

 fprintf(dst,"  ");
 loop(i,8)fprintf(dst,"%d",bits8[7-i]);
 fprintf(dst,"\n");
 
  return Success;
}
