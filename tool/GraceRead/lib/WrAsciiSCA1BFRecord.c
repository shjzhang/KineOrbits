#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Success 1
#define Failure 0


static char SccsId[] = "$Id: WrAsciiSCA1BFRecord.c,v 1.8 2004/08/30 21:03:37 wib Exp $";


boolean WrAsciiSCA1BFRecord(FILE *dst, SCA1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Dump ascii records from the Star Camera Assembly 
/          file pointed to by dst 
/
/ coded by: J. E. Patterson                         07/18/00
/
/ input:  *dst    Pointer to SCA1B Data Format File
/         *record Pointer to SCA1B Data struct (SCA1B_t)
<-----------------------------------------------------------------------------*/
{
 char string[3];
 char bits8[8];                                                                         

 long i;

 GetCharBits(record->qualflg,bits8);                                                    

 strcpy(string,"-");
 string[0] = record->GRACE_id;                                                          
 fprintf(dst,"%d %s %d %.16g %.16g %.16g %.16g %.16g ", 
         record->gps_time, string, record->sca_id, 
         record->quatangle, record->quaticoeff, record->quatjcoeff, 
         record->quatkcoeff, record->qual_rss);

 loop(i,8)fprintf(dst,"%d",bits8[7-i]);                                                 
 fprintf(dst,"\n");
 
 return Success ;
}
