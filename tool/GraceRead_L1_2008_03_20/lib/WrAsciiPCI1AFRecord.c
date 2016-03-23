#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


#define Success 1 
#define Failure 0


static char SccsId[] = "$Id: WrAsciiPCI1AFRecord.c,v 1.2 2004/08/30 21:03:37 wib Exp $";


boolean WrAsciiPCI1AFRecord(FILE *dst, PCI1A_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Dump ascii records of the PCI Level 1A data 
/          from file pointer dst 
/
/ coded by: Gerhard Kruizinga                       03/18/03
/
/ input:  *dst    Pointer to PCI1A Data Format File
/         *record Pointer to PCI1A Data struct (PCI1A_t)
<-----------------------------------------------------------------------------*/
{
 char bits8[8];
 
 long i;

 GetCharBits(record->qualflg,bits8);


 fprintf(dst,"%d %c %.16g %.16g %.16g", record->gps_time, record->GRACE_id,
         record->ant_centr_corr, record->ant_centr_rate, record->ant_centr_accl);

 fprintf(dst,"  ");
 loop(i,8)fprintf(dst,"%d",bits8[7-i]);
 fprintf(dst,"\n");
 
 return Success;
}
