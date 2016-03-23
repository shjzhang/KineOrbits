#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"


#define Success 1
#define Failure 0


static char SccsId[] = "$Id: ReadPCI1AFRecord.c,v 1.2 2004/08/30 21:03:36 wib Exp $";


boolean ReadPCI1AFRecord(FILE *src, PCI1A_t *record)
/*----------------------------------------------------------------------------->
/ purpose: read PCI1A Level 1A Data Format record from file pointer src
/
/ coded by: Gerhard L.H. Kruizinga         03/18/03
/
/ input:  *src    Pointer to PCI Level 1A Flight Data Format File
/ output: *record Pointer to PCI Level 1A Flight Data struct (PCI1A_t)
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
    printf("PCI1A: Error on reading field 'gsp_time' \n");
    return Failure;
  }
  record->gps_time = gps_time;
  
/*----------------------------------------------------------------------------->
/ Read Header from src
<-----------------------------------------------------------------------------*/
  if (fread_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,src) != 1)
  {
    printf("PCI1A: Error reading field 'GRACE_id' \n");
    return Failure;
  }

  if (fread_grace(&record->ant_centr_corr,sizeof(record->ant_centr_corr),1,src) != 1)
  {
    printf("PCI1A: Error on reading field 'ant_centr_corr' \n");
    return Failure;
  }
  
  if (fread_grace(&record->ant_centr_rate,sizeof(record->ant_centr_rate),1,src) != 1)
  {
    printf("PCI1A: Error on reading field 'ant_centr_rate' \n");
    return Failure;
  }
  
  if (fread_grace(&record->ant_centr_accl,sizeof(record->ant_centr_accl),1,src) != 1)
  {
    printf("PCI1A: Error on reading field 'ant_centr_accl' \n");
    return Failure;
  }
  
  if (fread_grace(&record->qualflg,sizeof(record->qualflg),1,src) != 1)
  {
    printf("PCI1A: Error on reading field 'qualflg' \n");
    return Failure;
  }
  

  return True;

}
