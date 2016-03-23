#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Success 1
#define Failure 0

static char SccsId[] = "$Id: WrAsciiSCA1AFRecord.c,v 1.4 2004/08/30 21:03:37 wib Exp $";

boolean WrAsciiSCA1AFRecord(FILE *dst, SCA1A_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Dump ascii records from the Star Camera Assembly  Level 1A
/          file pointed to by dst 
/
/ coded by: J. E. Patterson                         09/20/01
/
/ input:  *dst    Pointer to SCA1A Data Format File
/         *record Pointer to SCA1A Data struct (SCA1B_t)
<-----------------------------------------------------------------------------*/
{
 char bits8[8];                                                                         

 long i;


 fprintf(dst,"%d %c %d %c %.16g %.16g %.16g %.16g %d %d", 
         record->rcv_time, record->GRACE_id, record->sca_id,record->sca_desig, 
         record->quatangle, record->quaticoeff, record->quatjcoeff, 
         record->quatkcoeff, record->nlocks, record->nstars);

 loop(i,NSCACONF)fprintf(dst," %d",record->sca_config[i]);

 fprintf(dst," ");

 GetCharBits(record->sca_mode,bits8);                                                    
 loop(i,8)fprintf(dst,"%d",bits8[7-i]);                                                 
 fprintf(dst," ");

 GetCharBits(record->qualflg,bits8);                                                    
 loop(i,8)fprintf(dst,"%d",bits8[7-i]);                                                 
 fprintf(dst,"\n");
 
 return Success ;
}
