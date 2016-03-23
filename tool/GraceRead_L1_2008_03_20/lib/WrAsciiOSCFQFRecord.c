#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Success 1
#define Failure 0


static char SccsId[] = "$Id: WrAsciiOSCFQFRecord.c,v 1.7 2004/08/30 21:03:37 wib Exp $";


boolean WrAsciiOSCFQFRecord(FILE *dst, OSCFQ_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Dump ascii records from the Ultra Stable Oscillator 
/          data from the file pointed to by dst 
/
/ coded by: J. E. Patterson                  09/07/00
/
/ modified name of routine and structure     02/15/01
/
/ input:  *dst    Pointer to OSCFQ Data Format File
/         *record Pointer to OSCFQ Data struct (OSCFQ_t)
<-----------------------------------------------------------------------------*/
{
  char string[3];
  char bits8[8];

  long i;

  GetCharBits(record->qualflg,bits8);

  strcpy(string,"-");
  string[0] = record->GRACE_id;                                                          

  fprintf(dst,"%d %s %d %.16g %.16g %.16g", record->gps_time,
      string, record->uso_id, record->uso_freq, record->K_freq,record->Ka_freq);

  fprintf(dst,"  ");
  loop(i,8)fprintf(dst,"%d",bits8[7-i]);
  fprintf(dst,"\n");
 
  return Success;
}
