#include "GRACEiolib.h"
#include "GRACEio_utils.h"
#include "GRACEio_prototypes.h"

#define Failure 0


#define NBITSMAX 16


static char SccsId[] = "$Id: WriteGFD1XFRecord.c,v 1.5 2004/08/30 21:03:38 wib Exp $";


boolean WriteGFD1XFRecord(FILE *dst, GFD1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: write GPS Flight Data Format record to file pointer dst
/
/ coded by: Gerhard L.H. Kruizinga                  06/08/00
/ modified by: J. E. Patterson                      08/30/00
/    changed definition and name of gps time variables
/ 
/ input:  *dst   pointer to GPS Flight Data Format File
/         *record Pointer to GPS Flight Data struct (GFD1X_t)
<-----------------------------------------------------------------------------*/
{

 long i;
 char bits[NBITSMAX];
 
/*----------------------------------------------------------------------------->
/ Write Header to dst
<-----------------------------------------------------------------------------*/
  if (fwrite_grace(&record->rcvtime_intg,sizeof(record->rcvtime_intg),1,dst) != 1)
  {
    printf("GFD1X: Error writing field 'rcvtime_intg' \n");
    return Failure;
  }

  if (fwrite_grace(&record->rcvtime_frac,sizeof(record->rcvtime_frac),1,dst) != 1)
  {
    printf("GFD1X: Error writing field 'rcvtime_frac' \n");
    return Failure;
  }

  if (fwrite_grace(&record->GRACE_id,sizeof(record->GRACE_id),1,dst) != 1)
  {
    printf("GFD1X: Error writing field 'GRACE_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->prn_id,sizeof(record->prn_id),1,dst) != 1)
  {
    printf("GFD1X: Error writing field 'prn_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->ant_id,sizeof(record->ant_id),1,dst) != 1)
  {
    printf("GFD1X: Error writing field 'ant_id' \n");
    return Failure;
  }

  if (fwrite_grace(&record->prod_flag,sizeof(record->prod_flag),1,dst) != 1)
  {
    printf("GFD1X: Error writing field 'prod_flag' \n");
    return Failure;
  }

  if (fwrite_grace(&record->qualflg,sizeof(record->qualflg),1,dst) != 1)
  {
    printf("GFD1X: Error writing field 'qualflg' \n");
    return Failure;
  }


/*----------------------------------------------------------------------------->
/ Decode Product flag 
<-----------------------------------------------------------------------------*/

  GetShortBits(record->prod_flag,bits);

/*----------------------------------------------------------------------------->
/ Write all product specified by prod_flag to dst
<-----------------------------------------------------------------------------*/

  loop(i,NBITSMAX)
  {
    if (bits[i] == 1)
    {
      switch(i)
      {
       case  0:
          if (fwrite_grace(&record->CA_range,sizeof(record->CA_range),1,dst) != 1)
          {
            printf("GFD1X: Error writing field 'CA_range' \n");
            return Failure;
          }
          break;
       case  1:
          if (fwrite_grace(&record->L1_range,sizeof(record->L1_range),1,dst) != 1)
          {
            printf("GFD1X: Error writing field 'L1_range' \n");
            return Failure;
          }
          break;
       case  2:
          if (fwrite_grace(&record->L2_range,sizeof(record->L2_range),1,dst) != 1)
          {
            printf("GFD1X: Error writing field 'L2_range' \n");
            return Failure;
          }
          break;
       case  3:
          if (fwrite_grace(&record->CA_phase,sizeof(record->CA_phase),1,dst) != 1)
          {
            printf("GFD1X: Error writing field 'CA_phase' \n");
            return Failure;
          }
          break;
       case  4:
          if (fwrite_grace(&record->L1_phase,sizeof(record->L1_phase),1,dst) != 1)
          {
            printf("GFD1X: Error writing field 'L1_phase' \n");
            return Failure;
          }
          break;
       case  5:
          if (fwrite_grace(&record->L2_phase,sizeof(record->L2_phase),1,dst) != 1)
          {
            printf("GFD1X: Error writing field 'L2_phase' \n");
            return Failure;
          }
          break;
       case  6:
          if (fwrite_grace(&record->CA_SNR,sizeof(record->CA_SNR),1,dst) != 1)
          {
            printf("GFD1X: Error writing field 'CA_SNR' \n");
            return Failure;
          }
          break;
       case  7:
          if (fwrite_grace(&record->L1_SNR,sizeof(record->L1_SNR),1,dst) != 1)
          {
            printf("GFD1X: Error writing field 'L1_SNR' \n");
            return Failure;
          }
          break;
       case  8:
          if (fwrite_grace(&record->L2_SNR,sizeof(record->L2_SNR),1,dst) != 1)
          {
            printf("GFD1X: Error writing field 'L2_SNR' \n");
            return Failure;
          }
          break;
       case  9:
          if (fwrite_grace(&record->CA_chan,sizeof(record->CA_chan),1,dst) != 1)
          {
            printf("GFD1X: Error writing field 'CA_chan' \n");
            return Failure;
          }
          break;
       case 10:
          if (fwrite_grace(&record->L1_chan,sizeof(record->L1_chan),1,dst) != 1)
          {
            printf("GFD1X: Error writing field 'L1_chan' \n");
            return Failure;
          }
          break;
       case 11:
          if (fwrite_grace(&record->L2_chan,sizeof(record->L2_chan),1,dst) != 1)
          {
            printf("GFD1X: Error writing field 'L2_chan' \n");
            return Failure;
          }
          break;
       case 12:
          if (fwrite_grace(&record->K_phase,sizeof(record->K_phase),1,dst) != 1)
          {
            printf("GFD1X: Error writing field 'K_phase' \n");
            return Failure;
          }
          break;
       case 13:
          if (fwrite_grace(&record->Ka_phase,sizeof(record->Ka_phase),1,dst) != 1)
          {
            printf("GFD1X: Error writing field 'Ka_phase' \n");
            return Failure;
          }
          break;
       case 14:
          if (fwrite_grace(&record->K_SNR,sizeof(record->K_SNR),1,dst) != 1)
          {
            printf("GFD1X: Error writing field 'K_SNR' \n");
            return Failure;
          }
          break;
       case 15:
          if (fwrite_grace(&record->Ka_SNR,sizeof(record->Ka_SNR),1,dst) != 1)
          {
            printf("GFD1X: Error writing field 'Ka_SNR' \n");
            return Failure;
          }
          break;
       default:
          fprintf(stderr,"\n Product Flag index %d is invalid!!!\n\n",i);
          exit(0);
      }
    }
  }

  return True;
}
