#include <string.h>
#include "GRACEiolib.h"
#include "GRACEio_utils.h"
#include "GRACEio_prototypes.h"

#define Failure 0
   
#define NBITSMAX 16

static char SccsId[] = "$Id: ReadGFD1XFRecord.c,v 1.8 2004/08/30 21:03:35 wib Exp $";


boolean ReadGFD1XFRecord(FILE *src, GFD1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: read GPS Flight Data Format record from file pointer src
/
/ coded by: Gerhard L.H. Kruizinga                  06/08/00
/ modified by: J. E. Patterson                      08/29/00
/   Fixed order of checking for EOF or misread of record field 
/   Failure/Success conditions
/
/ input:  *src    pointer to GPS Flight Data Format File
/ output: *record Pointer to GPS Flight Data struct (GFD1X_t)
/
/ return:      1       normal return
/              0       End Of File reached
<-----------------------------------------------------------------------------*/
{
  unsigned long rcvtime_intg;

  int           retrn;

  long          i;

  char          bits[NBITSMAX];

/*----------------------------------------------------------------------------->
/ Test for EOF
<-----------------------------------------------------------------------------*/


  retrn = fread_grace(&rcvtime_intg,sizeof(rcvtime_intg),1,src); 
  if (feof(src) != 0) return False;
  if (retrn != 1)
  {
    printf("GFD1X: Error reading field 'rcvtime_intg' \n");
    return Failure;
  }

  record->rcvtime_intg = rcvtime_intg;
  
/*----------------------------------------------------------------------------->
/ Read Header from src
<-----------------------------------------------------------------------------*/

  if (fread_grace(&record->rcvtime_frac,sizeof(record->rcvtime_frac),1,src) != 1)
  {
    printf("GFD1X: Error reading field 'rcvtime_frac' \n");
    return (Failure);
  }

  if (fread_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,src) != 1)
  {
    printf("GFD1X: Error reading field 'GRACE_id' \n");
    return Failure;
  }

  if (fread_grace(&record->prn_id,sizeof(record->prn_id),1,src) != 1)
  {
    printf("GFD1X: Error reading field 'prn_id' \n");
    return Failure;
  }

  if (fread_grace(&record->ant_id,sizeof(record->ant_id),1,src) != 1)
  {
    printf("GFD1X: Error reading field 'ant_id' \n");
    return Failure;
  }

  if (fread_grace(&record->prod_flag,sizeof(record->prod_flag),1,src) != 1)
  {
    printf("GFD1X: Error reading field 'prod_flag' \n");
    return Failure;
  }

  if (fread_grace(&record->qualflg,sizeof(record->qualflg),1,src) != 1)
  {
    printf("GFD1X: Error reading field 'qualflg' \n");
    return Failure;
  }


/*----------------------------------------------------------------------------->
/ Decode Product flag 
<-----------------------------------------------------------------------------*/

  GetShortBits(record->prod_flag,bits);

/*----------------------------------------------------------------------------->
/ Read all product specified by prod_flag from src
<-----------------------------------------------------------------------------*/

  loop(i,NBITSMAX)
  {
    if (bits[i] == 1)
    {
      switch(i)
      {
       case  0:
          if (fread_grace(&record->CA_range,sizeof(record->CA_range),1,src) != 1)
          {
            printf("GFD1X: Error reading field 'CA_range' \n");
            return Failure;
          }
          break;
       case  1:
          if (fread_grace(&record->L1_range,sizeof(record->L1_range),1,src) != 1)
          {
            printf("GFD1X: Error reading field 'L1_range' \n");
            return Failure;
          }
          break;
       case  2:
          if (fread_grace(&record->L2_range,sizeof(record->L2_range),1,src) != 1)
          {
            printf("GFD1X: Error reading field 'L2_range' \n");
            return Failure;
          }
          break;
       case  3:
          if (fread_grace(&record->CA_phase,sizeof(record->CA_phase),1,src) != 1)
          {
            printf("GFD1X: Error reading field 'CA_phase' \n");
            return Failure;
          }
          break;
       case  4:
          if (fread_grace(&record->L1_phase,sizeof(record->L1_phase),1,src) != 1)
          {
            printf("GFD1X: Error reading field 'L1_phase' \n");
            return Failure;
          }
          break;
       case  5:
          if (fread_grace(&record->L2_phase,sizeof(record->L2_phase),1,src) != 1)
          {
            printf("GFD1X: Error reading field 'L2_phase' \n");
            return Failure;
          }
          break;
       case  6:
          if (fread_grace(&record->CA_SNR,sizeof(record->CA_SNR),1,src) != 1)
          {
            printf("GFD1X: Error reading field 'CA_SNR' \n");
            return Failure;
          }
          break;
       case  7:
          if (fread_grace(&record->L1_SNR,sizeof(record->L1_SNR),1,src) != 1)
          {
            printf("GFD1X: Error reading field 'L1_SNR' \n");
            return Failure;
          }
          break;
       case  8:
          if (fread_grace(&record->L2_SNR,sizeof(record->L2_SNR),1,src) != 1)
          {
            printf("GFD1X: Error reading field 'L2_SNR' \n");
            return Failure;
          }
          break;
       case  9:
          if (fread_grace(&record->CA_chan,sizeof(record->CA_chan),1,src) != 1)
          {
            printf("GFD1X: Error reading field 'CA_chan' \n");
            return Failure;
          }
          break;
       case 10:
          if (fread_grace(&record->L1_chan,sizeof(record->L1_chan),1,src) != 1)
          {
            printf("GFD1X: Error reading field 'L1_chan' \n");
            return Failure;
          }
          break;
       case 11:
          if (fread_grace(&record->L2_chan,sizeof(record->L2_chan),1,src) != 1)
          {
            printf("GFD1X: Error reading field 'L2_chan' \n");
            return Failure;
          }
          break;
       case 12:
          if (fread_grace(&record->K_phase,sizeof(record->K_phase),1,src) != 1)
          {
            printf("GFD1X: Error reading field 'K_phase' \n");
            return Failure;
          }
          break;
       case 13:
          if (fread_grace(&record->Ka_phase,sizeof(record->Ka_phase),1,src) != 1)
          {
            printf("GFD1X: Error reading field 'Ka_phase' \n");
            return Failure;
          }
          break;
       case 14:
          if (fread_grace(&record->K_SNR,sizeof(record->K_SNR),1,src) != 1)
          {
            printf("GFD1X: Error reading field 'K_SNR' \n");
            return Failure;
          }
          break;
       case 15:
          if (fread_grace(&record->Ka_SNR,sizeof(record->Ka_SNR),1,src) != 1)
          {
            printf("GFD1X: Error reading field 'Ka_SNR' \n");
            return Failure;
          }
          break;
       default:
          fprintf(stderr,"\n Product Flag index %d is invalid!!!\n\n",i);
          exit(0);
      }
    }
  }

  /*>>>> check if architecture is little endian, if so swap bytes <<<<*/

  return True;

}
