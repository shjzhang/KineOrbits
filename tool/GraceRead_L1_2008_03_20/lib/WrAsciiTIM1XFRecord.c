#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"
#include "GRACEio_utils.h"

#define Success 1
#define Failure 0

static char SccsId[] = "$Id: WrAsciiTIM1XFRecord.c,v 1.2 2004/08/30 21:03:37 wib Exp $";

boolean WrAsciiTIM1XFRecord(FILE *dst, TIM1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Dump ascii records from the OBDH time mapping to GPS time records to
/          file pointed to by dst 
/
/ coded by: Gerhard Kruizinga                       10/19/01
/
/ input:  *dst    Pointer to TIM1X Data Format File
/         *record Pointer to TIM1X Data struct (TIM1X_t)
<-----------------------------------------------------------------------------*/
{
 char bits8[8];                                                                         

 long i;

 GetCharBits(record->qualflg,bits8);                                                    

 fprintf(dst,"%d %c %d %d %d %d %d ", 
         record->obdh_time, record->GRACE_id, record->TS_suppid, 
         record->gpstime_intg, record->gpstime_frac, 
         record->first_icu_blknr, record->final_icu_blknr);

 loop(i,8)fprintf(dst,"%d",bits8[7-i]);                                                 
 fprintf(dst,"\n");
 
 return Success ;
}
