#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define Failure 0

static char SccsId[] = "$Id: WriteKBR1BFRecord.c,v 1.7 2004/08/30 21:03:38 wib Exp $";


boolean WriteKBR1BFRecord(FILE *dst, KBR1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: write KBR Level 1B Data Format record to file pointer dst
/
/ coded by: J. Patterson                     06/21/00
/
/ input:  *dst    Pointer to KBR Level 1B Data Format File
/         *record Pointer to KBR Level 1B Data struct (KBR1B_t)
<-----------------------------------------------------------------------------*/
{
 
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/
  if (fwrite_grace(&record->gps_time,sizeof(record->gps_time),1,dst) != 1)
  {
    printf("KBR1B: Error writing field 'gps_time' \n");
    return Failure;
  }

  if (fwrite_grace(&record->biased_range,sizeof(record->biased_range),1,dst) != 1)
  {
    printf("KBR1B: Error writing field 'biased_range' \n");
    return Failure;
  }

  if (fwrite_grace(&record->range_rate,sizeof(record->range_rate),1,dst) != 1)
  {
    printf("KBR1B: Error writing field 'range_rate' \n");
    return Failure;
  }

  if (fwrite_grace(&record->range_accl,sizeof(record->range_accl),1,dst) != 1)
  {
    printf("KBR1B: Error writing field 'range_accl' \n");
    return Failure;
  }

  if (fwrite_grace(&record->iono_corr,sizeof(record->iono_corr),1,dst) != 1)
  {
    printf("KBR1B: Error writing field 'iono_corr' \n");
    return Failure;
  }

  if (fwrite_grace(&record->lighttime_corr,sizeof(record->lighttime_corr),1,dst) != 1)
  {
    printf("KBR1B: Error writing field 'lighttime_corr' \n");
    return Failure;
  }

  if (fwrite_grace(&record->lighttime_rate,sizeof(record->lighttime_rate),1,dst) != 1)
  {
    printf("KBR1B: Error writing field 'lighttime_rate' \n");
    return Failure;
  }

  if (fwrite_grace(&record->lighttime_accl,sizeof(record->lighttime_accl),1,dst) != 1)
  {
    printf("KBR1B: Error writing field 'lighttime_accl' \n");
    return Failure;
  }

  if (fwrite_grace(&record->ant_centr_corr,sizeof(record->ant_centr_corr),1,dst) != 1)
  {
    printf("KBR1B: Error writing field 'ant_centr_corr' \n");
    return Failure;
  }

  if (fwrite_grace(&record->ant_centr_rate,sizeof(record->ant_centr_rate),1,dst) != 1)
  {
    printf("KBR1B: Error writing field 'ant_centr_rate' \n");
    return Failure;
  }

  if (fwrite_grace(&record->ant_centr_accl,sizeof(record->ant_centr_accl),1,dst) != 1)
  {
    printf("KBR1B: Error writing field 'ant_centr_accl' \n");
    return Failure;
  }

  if (fwrite_grace(&record->K_A_SNR,sizeof(record->K_A_SNR),1,dst) != 1)
  {
    printf("KBR1B: Error writing field 'K_A_SNR' \n");
    return Failure;
  }

  if (fwrite_grace(&record->Ka_A_SNR,sizeof(record->Ka_A_SNR),1,dst) != 1)
  {
    printf("KBR1B: Error writing field 'Ka_A_SNR' \n");
    return Failure;
  }

  if (fwrite_grace(&record->K_B_SNR,sizeof(record->K_B_SNR),1,dst) != 1)
  {
    printf("KBR1B: Error writing field 'K_B_SNR' \n");
    return Failure;
  }

  if (fwrite_grace(&record->Ka_B_SNR,sizeof(record->Ka_B_SNR),1,dst) != 1)
  {
    printf("KBR1B: Error writing field 'Ka_B_SNR' \n");
    return Failure;
  }

  if (fwrite_grace(&record->qualflg,sizeof(record->qualflg),1,dst) != 1)
  {
    printf("KBR1B: Error writing field 'qualflg' \n");
    return Failure;
  }

  
  return True;
}
