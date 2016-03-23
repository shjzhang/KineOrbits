#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


#define Success 1 
#define Failure 0


static char SccsId[] = "$Id: WrAsciiKBR1BFRecord.c,v 1.7 2004/08/30 21:03:37 wib Exp $";


boolean WrAsciiKBR1BFRecord(FILE *dst, KBR1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Dump ascii records of the KBR Level 1B data 
/          from file pointer dst 
/
/ coded by: J. E. Patterson                         07/18/00
/
/ input:  *dst    Pointer to KBR1B Data Format File
/         *record Pointer to KBR1B Data struct (KBR1B_t)
<-----------------------------------------------------------------------------*/
{
 char bits8[8];
 
 long i;

 GetCharBits(record->qualflg,bits8);


 fprintf(dst,"%d %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %.16g %2d %2d"
         " %2d %2d", record->gps_time, record->biased_range, 
         record->range_rate, record->range_accl, record->iono_corr, 
         record->lighttime_corr, record->lighttime_rate, record->lighttime_accl,
         record->ant_centr_corr, record->ant_centr_rate, record->ant_centr_accl,
         record->K_A_SNR, record->Ka_A_SNR, record->K_B_SNR, record->Ka_B_SNR);

 fprintf(dst,"  ");
 loop(i,8)fprintf(dst,"%d",bits8[7-i]);
 fprintf(dst,"\n");
 
 return Success;
}
