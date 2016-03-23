#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


#define Success 1
#define Failure 0

static char SccsId[] = "$Id: ReadKBR1BFRecord.c,v 1.6 2004/08/30 21:03:36 wib Exp $";



boolean ReadKBR1BFRecord(FILE *src, KBR1B_t *record)
/*----------------------------------------------------------------------------->
/ purpose: read KBR Level 1B Data Format record from file pointer src
/
/ coded by: J. Patterson                   06/21/00
/
/ input:  *src    Pointer to KBR Level 1B Flight Data Format File
/ output: *record Pointer to KBR Level 1B Flight Data struct (KBR1B_t)
/
/ return:      1       normal return
/              0       End Of File reached
<-----------------------------------------------------------------------------*/
{
  unsigned long gps_time;

  int    retrn;


/*----------------------------------------------------------------------------->
/ Test for EOF
<-----------------------------------------------------------------------------*/

  retrn = fread_grace(&gps_time,sizeof(gps_time),1,src);
  if (feof(src) != 0) return False;
  if (retrn != 1)
  {
    printf("KBR1B: Error on reading field 'gsp_time' \n");
    return Failure;
  }
  record->gps_time = gps_time;
  
/*----------------------------------------------------------------------------->
/ Read Header from src
<-----------------------------------------------------------------------------*/
  if (fread_grace(&record->biased_range,sizeof(record->biased_range),1,src) != 1)
  {
    printf("KBR1B: Error on reading field 'biased_range' \n");
    return Failure;
  }
  
  if (fread_grace(&record->range_rate,sizeof(record->range_rate),1,src) != 1)
  {
    printf("KBR1B: Error on reading field 'range_rate' \n");
    return Failure;
  }
  
  if (fread_grace(&record->range_accl,sizeof(record->range_accl),1,src) != 1)
  {
    printf("KBR1B: Error on reading field 'range_accl' \n");
    return Failure;
  }
  
  if (fread_grace(&record->iono_corr,sizeof(record->iono_corr),1,src) != 1)
  {
    printf("KBR1B: Error on reading field 'iono_corr' \n");
    return Failure;
  }
  
  if (fread_grace(&record->lighttime_corr,sizeof(record->lighttime_corr),1,src) != 1)
  {
    printf("KBR1B: Error on reading field 'lighttime_corr' \n");
    return Failure;
  }
  
  if (fread_grace(&record->lighttime_rate,sizeof(record->lighttime_rate),1,src) != 1)
  {
    printf("KBR1B: Error on reading field 'lighttime_rate' \n");
    return Failure;
  }
  
  if (fread_grace(&record->lighttime_accl,sizeof(record->lighttime_accl),1,src) != 1)
  {
    printf("KBR1B: Error on reading field 'lighttime_accl' \n");
    return Failure;
  }
  
  if (fread_grace(&record->ant_centr_corr,sizeof(record->ant_centr_corr),1,src) != 1)
  {
    printf("KBR1B: Error on reading field 'ant_centr_corr' \n");
    return Failure;
  }
  
  if (fread_grace(&record->ant_centr_rate,sizeof(record->ant_centr_rate),1,src) != 1)
  {
    printf("KBR1B: Error on reading field 'ant_centr_rate' \n");
    return Failure;
  }
  
  if (fread_grace(&record->ant_centr_accl,sizeof(record->ant_centr_accl),1,src) != 1)
  {
    printf("KBR1B: Error on reading field 'ant_centr_accl' \n");
    return Failure;
  }
  
  if (fread_grace(&record->K_A_SNR,sizeof(record->K_A_SNR),1,src) != 1)
  {
    printf("KBR1B: Error on reading field 'K_A_SNR' \n");
    return Failure;
  }
  
  if (fread_grace(&record->Ka_A_SNR,sizeof(record->Ka_A_SNR),1,src) != 1)
  {
    printf("KBR1B: Error on reading field 'Ka_A_SNR' \n");
    return Failure;
  }
  
  if (fread_grace(&record->K_B_SNR,sizeof(record->K_B_SNR),1,src) != 1)
  {
    printf("KBR1B: Error on reading field 'K_B_SNR' \n");
    return Failure;
  }
  
  if (fread_grace(&record->Ka_B_SNR,sizeof(record->Ka_B_SNR),1,src) != 1)
  {
    printf("KBR1B: Error on reading field 'Ka_B_SNR' \n");
    return Failure;
  }
  
  if (fread_grace(&record->qualflg,sizeof(record->qualflg),1,src) != 1)
  {
    printf("KBR1B: Error on reading field 'qualflg' \n");
    return Failure;
  }
  

  return True;

}
