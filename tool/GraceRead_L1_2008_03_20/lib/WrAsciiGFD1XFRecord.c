#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"

#define NBITSMAX 16
#define Success 1 
#define Failure 0


static char SccsId[] = "$Id: WrAsciiGFD1XFRecord.c,v 1.6 2004/08/30 21:03:37 wib Exp $";


boolean WrAsciiGFD1XFRecord(FILE *dst, GFD1X_t *record)
/*----------------------------------------------------------------------------->
/ purpose: Dump ascii records of flight data from the file pointed to by dst 
/
/ coded by: J. E. Patterson                         07/18/00
/
/ input:  *dst    Pointer to GFD1X Data Format File
/         *record Pointer to GFD1X Data struct (GFD1X_t)
<-----------------------------------------------------------------------------*/
{
  char bits[NBITSMAX];
  char string[3];

  long i;

  char bits8[8];

  GetCharBits(record->qualflg,bits8);


  strcpy(string,"-");
  string[0] = record->GRACE_id;                                                          


  fprintf(dst,"%d",record->rcvtime_intg);
  fprintf(dst,"  %d",record->rcvtime_frac);
  fprintf(dst,"  %s",string);
  fprintf(dst,"  %d",record->prn_id);
  fprintf(dst,"  %d",record->ant_id);

  GetShortBits(record->prod_flag,bits);

  fprintf(dst,"  ");
  loop(i,NBITSMAX)fprintf(dst,"%d",bits[15-i]);
  fprintf(dst," ");

  fprintf(dst,"  ");
  loop(i,8)fprintf(dst,"%d",bits8[7-i]);
  fprintf(dst," ");

/*
  fprintf(dst,"  %d", record->qualflg);
*/
 
  loop(i,NBITSMAX)
  {
    if (bits[i] == 1)
    {
      switch(i)
      {
        case  0:
                 fprintf(dst,"  %.16g",record->CA_range);
                 break;

        case  1:
                 fprintf(dst,"  %.16g",record->L1_range);
                 break;

        case  2:
                 fprintf(dst,"  %.16g",record->L2_range);
                 break;

        case  3:
                 fprintf(dst,"  %.16g",record->CA_phase);
                 break;

        case  4:
                 fprintf(dst,"  %.16g",record->L1_phase);
                 break;

        case  5:
                 fprintf(dst,"  %.16g",record->L2_phase);
                 break;

        case  6:
                 fprintf(dst,"  %d",record->CA_SNR);
                 break;

        case  7:
                 fprintf(dst,"  %d",record->L1_SNR);
                 break;

        case  8:
                 fprintf(dst,"  %d",record->L2_SNR);
                 break;

        case  9:
                 fprintf(dst,"  %d",record->CA_chan);
                 break;

        case 10:
                 fprintf(dst,"  %d",record->L1_chan);
                 break;

        case 11:
                 fprintf(dst,"  %d",record->L2_chan);
                 break;

        case 12:
                 fprintf(dst,"  %.16g",record->K_phase);
                 break;

        case 13:
                 fprintf(dst,"  %.16g",record->Ka_phase);
                 break;

        case 14:
                 fprintf(dst,"  %d",record->K_SNR);
                 break;

        case 15:
                 fprintf(dst,"  %d",record->Ka_SNR);
                 break;

        default:
           fprintf(stderr,"\n GFD1XF Product Flag index %d is invalid!!!\n\n",i);
      }
    }
  }

  fprintf(dst,"\n");
    
  return Success;
}
