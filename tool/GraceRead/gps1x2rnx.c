#include <string.h>
#define _mk_extern_
#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"
#include "GRACEio_utils.h"
#include "TimeLib.h"
#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"
#include "GRACEio_utils.h"
#include "TimeLib.h"
#include "GRACEgpslib.h"
#include <math.h>

static char SccsId[] = "$Id: gps1x2rnx.c,v 1.8 2004/08/30 21:15:55 wib Exp $";

#define MAXCHAR   1000
#define NBITSMAX    16
#define NBITSMAXCHAR 8


#define loop1(I,N) for(I=1;I<N;I++)

int snr2n(short s);

main (int argc, char *argv[])
/*----------------------------------------------------------------------------->
/ purpose: read a GPS1A,GPS1B or KBR1A file and convert data into a standard
/          rinex file
/
/ coded by: G.L.H. Kruizinga        04/12/01
/ Based on: BJfmt.c by Larry Romans
/
/-----------------------------------------------------------------------------*/
{
 FILE         *gps1x,*rnx;

 char         gps1x_filename[MAXCHAR],rnx_filename[MAXCHAR];
 char         bits[NBITSMAX],qualbits[NBITSMAXCHAR];

 GFD1X_t      record;

 FileHeader_t header;

 rnx_header_t rnx_head;

 Prn_t        rnxblock[MAX_NSATELLITES];

 long         do_snrs,do_ca,i,nobs,first_block=1,arg_ndx;

 double       last_time_tag,current_time_tag;
 
/*----------------------------------------------------------------------------->
/ check usage
<-----------------------------------------------------------------------------*/
 
 if (argc < 2)
   {
    fprintf(stderr,"\n usage: %s  -gps1x GPS1X_File -rnx Rinex_File [-no_snrs] [-no_ca]\n",
            argv[0]);
    fprintf(stderr,"\n");
    exit(1);
   }

/*----------------------------------------------------------------------------->
/ extract all all filenames from input arguments and open files
<-----------------------------------------------------------------------------*/

 strcpy (gps1x_filename,   GetStrArgv(argc,argv,"-gps1x" ));

 gps1x = fopen(gps1x_filename, "rb");

 if (gps1x == NULL)
 {
  fprintf(stderr,"\n GPS1X file %s cannot be opened !! \n\n",gps1x_filename);
  exit(2);
 }

 strcpy (rnx_filename,   GetStrArgv(argc,argv,"-rnx" ));

 rnx = fopen(rnx_filename, "w+");

 if (rnx == NULL)
 {
  fprintf(stderr,"\n Rinex File file %s cannot be opened !! \n\n",rnx_filename);
  exit(3);
 }

 do_snrs = 1;
 arg_ndx = FindOpt(argc,argv, "-no_snrs");
 if (arg_ndx != -1) do_snrs  = 0;

 do_ca = 1;
 arg_ndx = FindOpt(argc,argv, "-no_ca");
 if (arg_ndx != -1) do_ca  = 0;

/*----------------------------------------------------------------------------->
/ Intialize Header information and Determine if file is a binary SCA1B file
<-----------------------------------------------------------------------------*/

  InitializeHeaders();

  if (ReadFileHeader(gps1x,&header) == false)
  {   
    fprintf(stderr,"\n Problem reading file header for file %s\n",gps1x_filename);
    fprintf(stderr," See message above for problem\n\n");
    exit(1);
  }

  if (header.filetype != GetFileType("ipGFD1XF") || header.formattype != 0)
  {
    fprintf(stderr,"\n GPS1X file %s is not a GPS observation file or \n",gps1x_filename);
    fprintf(stderr," GPS1X file is an ascii GPS observation file. Check Input file!\n\n");
    exit(5);
  }

/*----------------------------------------------------------------------------->
/ Setup Rinex header and write header to rinex file
<-----------------------------------------------------------------------------*/

  strncpy(rnx_head.pgm, "gps1x2rnx", 20);
  strncpy(rnx_head.marker, header.SatelliteName, 60);
  strncpy(rnx_head.observer, header.SatelliteName, 20);
  strncpy(rnx_head.agency, header.ProducerAgency, 40);
  strncpy(rnx_head.recnum, "RECNUM", 20);
  strncpy(rnx_head.rectype, "RECTYPE", 20);
  strncpy(rnx_head.recvers, "RECVERS", 20);
  strncpy(rnx_head.antnum, "ANTNUM", 20);
  strncpy(rnx_head.anttype, "ANTTYPE", 20);
  loop(i,3) rnx_head.pos[i] = rnx_head.ant[i] = 0.0;

  rnx_head.StartTime = header.TimeFirstObs;
  rnx_head.FinalTime = header.TimeLastObs;

/*----------------------------------------------------------------------------->
/ Initialize rnxblock struct
<-----------------------------------------------------------------------------*/

  loop1(i,MAX_NSATELLITES)
  {
    rnxblock[i].flg      = 0;
    rnxblock[i].flg_prev = 0;
  }


  nobs = 0;
  while(ReadGFD1XFRecord(gps1x,&record) == true)
  { 
    GetShortBits(record.prod_flag,bits);
    GetCharBits(record.qualflg,qualbits);
 
    if (record.prn_id < MIN_PRN_NUMBER || record.prn_id > MAX_PRN_NUMBER)
    {
      fprintf(stderr,"\n Invalid prn number = %d!!\n",record.prn_id);
      fprintf(stderr," Check input file %s\n\n",gps1x_filename);
      exit(6);
    }

    current_time_tag = (double) record.rcvtime_intg +
                       (double) record.rcvtime_frac*1e-6;
    if (nobs == 0)
    {
      rnxblock[record.prn_id].ttag_last = current_time_tag;
      last_time_tag                     = current_time_tag;
    }

    if (current_time_tag != last_time_tag)
    { 
      if (first_block == 1)
      {
        rnx_head.Interval  = record.rcvtime_intg - last_time_tag;
        WriteRnxHeader(rnx,do_snrs,do_ca,&rnx_head);
        first_block = 0;
      }
      PurgeRnxBlock(rnx,rnxblock,MAX_NSATELLITES,do_snrs,do_ca);
      last_time_tag = current_time_tag;
    }
    rnxblock[record.prn_id].flg       = 1;
    rnxblock[record.prn_id].flg_prev  = qualbits[0];
    rnxblock[record.prn_id].ttag = current_time_tag;
 
    rnxblock[record.prn_id].tu[0] = 0.0;
    rnxblock[record.prn_id].tu[1] = 0.0;
    rnxblock[record.prn_id].tu[2] = 0.0;

    rnxblock[record.prn_id].fz[0] = 0.0;
    rnxblock[record.prn_id].fz[1] = 0.0;
    rnxblock[record.prn_id].fz[2] = 0.0;

    rnxblock[record.prn_id].snr[0] = 0;
    rnxblock[record.prn_id].snr[1] = 0;
    rnxblock[record.prn_id].snr[2] = 0;

    rnxblock[record.prn_id].ss[0] = 0;
    rnxblock[record.prn_id].ss[1] = 0;
    rnxblock[record.prn_id].ss[2] = 0;

    loop(i,NBITSMAX)
    {
      if (bits[i] == 1)
      {
        switch(i)
        {
          case CA_RANGE_NDX:
                 rnxblock[record.prn_id].tu[0] = record.CA_range;
                 break;
          case L1_RANGE_NDX:
                 rnxblock[record.prn_id].tu[1] = record.L1_range;
                 break;
          case L2_RANGE_NDX:
                 rnxblock[record.prn_id].tu[2] = record.L2_range;
                 break; 
          case CA_PHASE_NDX:
                 rnxblock[record.prn_id].fz[0] = record.CA_phase/LAMBDA_1/1000;
                 break;
          case L1_PHASE_NDX:   
                 rnxblock[record.prn_id].fz[1] = record.L1_phase/LAMBDA_1/1000;
                 break;
          case L2_PHASE_NDX:
                 rnxblock[record.prn_id].fz[2] = record.L2_phase/LAMBDA_2/1000;
                 break;
          case CA_SNR_NDX:
                 rnxblock[record.prn_id].snr[0] = record.CA_SNR;
                 rnxblock[record.prn_id].ss[0]  = snr2n(record.CA_SNR);
                 break;
          case L1_SNR_NDX:   
                 rnxblock[record.prn_id].snr[1] = record.L1_SNR;
                 rnxblock[record.prn_id].ss[1]  = snr2n(record.L1_SNR);
                 break;
          case L2_SNR_NDX:
                 rnxblock[record.prn_id].snr[2] = record.L2_SNR;
                 rnxblock[record.prn_id].ss[2]  = snr2n(record.L2_SNR);
                 break;
          default:
                 break;
        }
      }

      nobs++;

    }
  }

  PurgeRnxBlock(rnx,rnxblock,MAX_NSATELLITES,do_snrs,do_ca);

  fclose(gps1x);
  fclose(rnx);

  exit(0);

}
int snr2n(short s)
{ 
  if (s > 10) {
    if (s > 100) return( (s > 500) ? 9 : 8 );
    else return( (s > 50) ? 7 : 6 );    
  }
  else {
    if (s > 3) return( (s > 5) ? 5 : 4 );
    else return( (s > 1) ? 3 : 2 );
  }
}
