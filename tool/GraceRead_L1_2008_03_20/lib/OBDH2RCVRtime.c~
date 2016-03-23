#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"
#include "GRACEio_utils.h"

#define NOBLOCK -1

static char SccsId[] = "$Id: OBDH2RCVRtime.c,v 1.8 2004/08/30 21:03:34 wib Exp $";

long OBDH2RCVRtime(long obdh_intg, long obdh_frac, long *gps_intg, long *gps_frac,
                  long icu_blk_nr, FILE *tim1b)
/*----------------------------------------------------------------------------->
/ purpose:  Return GPS time based on OBDH Time input and ACC BlockNr
/
/ coded by: Gerhard L.H. Kruizinga                10/30/01
/ modified: Gerhard L.H. Kruizinga                02/18/02
/ modified: Gerhard L.H. Kruizinga                09/09/03
/
/ input:
/        obdh_intg     obdh time (integer seconds)
/        obdh_frac     obdh time fraction (micro seconds)
/        icu_blk_nr    icu data block number associate with obdh time
/        tim1b         file pointer to TIM1B file
/ output:
/        gps_intg      gps time (integer seconds)
/        gps_frac      obdh time fraction (micro seconds)
/ return 
/        0L if mapping succesful
/        1L request time after TIM file range
/        21L GPS mapping not determined (no tim1b record available)
/        22L GPS mapping not defined
/        23L GPS mapping not determined (no start icu block nr available)
/        24L GPS mapping not determined (no final icu block nr available)
/        25L GPS mapping not determined (icu block nr != tim1b block nr)
/        26L GPS mapping not determined (icu block nr outside tim1b block nr range)
/        27L GPS mapping not determined (cannot compute GPS mapping)
/        3L problem reading tim1b header
/        4L tim1b file is not a tim1b file or tim1b file is an ascii file
/       -1L request time before TIM file range
<-----------------------------------------------------------------------------*/
{
  static long         first = 1,first_obs = 1;
  static long         Tim1bStart,Tim1bFinal,tim1b_nrecs;
  static long         block_time_diff, prev_block_time_diff  = -999999;

  long                ndx,i;
 
  double              Time;

  static TIM1X_t      **tim1b_recs,tim1b_record;

  FileHeader_t        tim1b_header;

  char                bits8[8];

  *gps_intg = obdh_intg;
  *gps_frac = obdh_frac;

  /*>>>> if file pointer is not set return input as output <<<<*/

  if (tim1b == NULL) return 0L;

  /*>>>> on first call load tim1b file into memory <<<<*/

  if (first)
  {
    InitializeHeaderStruct(&tim1b_header);

    if (ReadFileHeader(tim1b,&tim1b_header) == false)
    {
      fprintf(stderr,"\n Problem reading tim1b file header in OBDH2RCVRtime\n");
      fprintf(stderr," See message above for problem\n\n");      
      return 3L;
    }
        
    if (tim1b_header.filetype != GetFileType("ipTIM1XF") || tim1b_header.formattype != 0)
    {       
      fprintf(stderr,"\n TIM1A file in OBDH2RCVRtime is not a TIM mapping file or \n");
      fprintf(stderr," TIM1A file is an ascii TIM mapping file . Check Input file!\n\n");
      return 4L;
    } 

    while(ReadTIM1XFRecord(tim1b,&tim1b_record) == true)
    {
      if (first_obs) {Tim1bStart = tim1b_record.obdh_time; first_obs = 0;}
      Tim1bFinal = tim1b_record.obdh_time;
    } 

    tim1b_nrecs = Tim1bFinal -Tim1bStart + 1;
    tim1b_recs  = (TIM1X_t **) malloc ((size_t)tim1b_nrecs * sizeof(TIM1X_t *));

    loop(i,tim1b_nrecs) tim1b_recs[i] = NULL;

    rewind(tim1b);

    if (ReadFileHeader(tim1b,&tim1b_header) == false)
    {
      fprintf(stderr,"\n Problem second reading tim1b file header in OBDH2RCVRtime\n");
      fprintf(stderr," See message above for problem\n\n");      
      return 3L;
    }

    while(ReadTIM1XFRecord(tim1b,&tim1b_record) == true)
    {
      if (first) {Tim1bStart = tim1b_record.obdh_time; first = 0;}
      ndx = tim1b_record.obdh_time - Tim1bStart;
      if (ndx < 0 || ndx > tim1b_nrecs-1)
      {
        fprintf(stderr," Data point in OBDH2RCVRtime at %d not within time_span %d <-> %d\n",
                         tim1b_record.obdh_time,Tim1bStart,Tim1bFinal);
        continue;
      } 

      tim1b_recs[ndx] = (TIM1X_t *) malloc(sizeof(TIM1X_t));
      
      *tim1b_recs[ndx] = tim1b_record;
    } 

    if (icu_blk_nr != NOBLOCK) prev_block_time_diff = obdh_intg - icu_blk_nr;

    first = 0;
  }

  block_time_diff = obdh_intg - icu_blk_nr;

  ndx = obdh_intg - Tim1bStart;

  /* correct for one second offset applied to ACC data in EncodeICUpacket */

  if (icu_blk_nr != NOBLOCK) ndx++;

  if ((icu_blk_nr != NOBLOCK) &&
      (block_time_diff != prev_block_time_diff) &&
      (obdh_frac > 1000000))
  {
    ndx++;
  }

  /*>>> check for valid ndx range <<<<*/

  if (ndx < 0) return -1L;
  if (ndx > tim1b_nrecs-1) return 1L;

  /*>>> check if tim1b record is in memory <<<<*/

  if (tim1b_recs[ndx] == NULL) return 21L;

  /*>>> check if GPS mapping is defined    <<<<*/

  GetCharBits(tim1b_recs[ndx]->qualflg,bits8);

  if (bits8[3]) return 22L;
  if (bits8[6]) return 27L;

  Time =      (double) tim1b_recs[ndx]->gpstime_intg + 
         1e-6*(double) tim1b_recs[ndx]->gpstime_frac +
         1e-6*(double) obdh_frac;

  *gps_intg = (long) Time;
  *gps_frac = roundint( 1e6*(Time - (double) *gps_intg));

  /*>>>> check if icu_blk_nr matches or for multiple block compute proper offset <<<<*/

  if (icu_blk_nr >= 0)
  {

    if (tim1b_recs[ndx]->first_icu_blknr == NOBLOCK) 
    { 
      prev_block_time_diff = block_time_diff;
      *gps_intg = obdh_intg;
      *gps_frac = obdh_frac;
      return 23L;
    }
    if (tim1b_recs[ndx]->final_icu_blknr == NOBLOCK) 
    {
      prev_block_time_diff = block_time_diff;
      *gps_intg = obdh_intg;
      *gps_frac = obdh_frac;
      return 24L;
    }

    if (tim1b_recs[ndx]->first_icu_blknr == tim1b_recs[ndx]->final_icu_blknr)
    {
      if (icu_blk_nr != tim1b_recs[ndx]->first_icu_blknr)
      {
        fprintf(stderr," Input ICU block number = %d != tim1b ICU blocknumber %d\n",
                        icu_blk_nr,tim1b_recs[ndx]->first_icu_blknr);
        prev_block_time_diff = block_time_diff;
        *gps_intg = obdh_intg;
        *gps_frac = obdh_frac;
        return 25L;
      }
    }
    else
    {
      if (icu_blk_nr <  tim1b_recs[ndx]->first_icu_blknr ||
          icu_blk_nr >  tim1b_recs[ndx]->final_icu_blknr)
      {
        fprintf(stderr," Input ICU block number = %d is out of tim1b block number",
                        icu_blk_nr);
        fprintf(stderr," range %d <-> %d\n",tim1b_recs[ndx]->first_icu_blknr,
                                            tim1b_recs[ndx]->final_icu_blknr);
        prev_block_time_diff = block_time_diff;
        *gps_intg = obdh_intg;
        *gps_frac = obdh_frac;
        return 26L;
      }

      *gps_intg = tim1b_recs[ndx]->gpstime_intg + 
                  icu_blk_nr - tim1b_recs[ndx]->first_icu_blknr;

    }


    prev_block_time_diff = block_time_diff;

  }

  /* correct new gps time for 1 second delay in ACC data time tag */

  if (icu_blk_nr != NOBLOCK) *gps_intg = *gps_intg - 1;

  return 0L;
}
