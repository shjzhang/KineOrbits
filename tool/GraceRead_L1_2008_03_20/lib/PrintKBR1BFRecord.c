#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


static char SccsId[] = "$Id: PrintKBR1BFRecord.c,v 1.8 2004/08/30 21:03:34 wib Exp $";



void PrintKBR1BFRecord(FILE *dst, KBR1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Print a detailed ascii description of KBR Level 1B Data Format 
/          record to file pointer dst
/
/ coded by: J. Patterson                            06/21/00
/
/ input:  *dst    Pointer to KBR Level 1B Flight Data Format File
/         *record Pointer to KBR Level 1B Flight Data struct (KBR1B_t)
<-----------------------------------------------------------------------------*/
{

 
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/
  fprintf(dst," %-20s = %ld\n","record->gps_time",record->gps_time);
  fprintf(dst," %-20s = %le\n","record->biased_range",record->biased_range);
  fprintf(dst," %-20s = %le\n","record->range_rate",record->range_rate);
  fprintf(dst," %-20s = %le\n","record->range_accl",record->range_accl);
  fprintf(dst," %-20s = %le\n","record->iono_corr",record->iono_corr);
  fprintf(dst," %-20s = %le\n","record->lighttime_corr",record->lighttime_corr);
  fprintf(dst," %-20s = %le\n","record->lighttime_rate",record->lighttime_rate);
  fprintf(dst," %-20s = %le\n","record->lighttime_accl",record->lighttime_accl);
  fprintf(dst," %-20s = %le\n","record->ant_centr_corr",record->ant_centr_corr);
  fprintf(dst," %-20s = %le\n","record->ant_centr_rate",record->ant_centr_rate);
  fprintf(dst," %-20s = %le\n","record->ant_centr_accl",record->ant_centr_accl);
  fprintf(dst," %-20s = %d\n","record->K_A_SNR",record->K_A_SNR);
  fprintf(dst," %-20s = %d\n","record->Ka_A_SNR",record->Ka_A_SNR);
  fprintf(dst," %-20s = %d\n","record->K_B_SNR",record->K_B_SNR);
  fprintf(dst," %-20s = %d\n","record->Ka_B_SNR",record->Ka_B_SNR);
  fprintf(dst," %-20s = %d\n","record->qualflg",record->qualflg);
}
