#include <stdio.h>
#include <stdlib.h> 
#include <string.h>
#include <time.h>
#include "GRACEiolib.h"
#include "GRACEgpslib.h"

#define MAXCHAR 1000
#define loop1(I,N) for(I=1;I<N;I++)

static char SccsId[] = "$Id: WriteRnxHeader.c,v 1.4 2004/08/30 21:03:32 wib Exp $";

long Roundint(double x);

long WriteRnxHeader(FILE *dst,long do_snrs,long do_ca,rnx_header_t *rnxhead)
/*----------------------------------------------------------------------------->
/ purpose: write a rinex header to file dst based on arguments
/
/ coded by: G.L.H. Kruizinga        04/12/01
/
/ input:  *dst      File pointer to destination file           
/         StartTime Time of first observation
/         do_snrs   indicator for addition of snr data to header
/         do_ca     indicator for addition of LA and SA to header
/
/-----------------------------------------------------------------------------*/
{   
  time_t       tt;
  struct       tm *pT;

  int          yr100,yr;

  double       seconds;
  double       sec_frac;

  int          month,day,year,hour,min,sec;

  char   runby[MAXCHAR],date[MAXCHAR];

  tt = (time_t) (time(&tt));
  pT = localtime(&tt);
  yr = (pT->tm_year % 100) + 1900;
  if (yr < 1980) yr += 100;

  strncpy(runby, getenv("USER"), 20);
  sprintf(date, "%04d-%02d-%02d %02d:%02d:%02.0f",
      yr, pT->tm_mon + 1, pT->tm_mday,
      pT->tm_hour, pT->tm_min, (double)pT->tm_sec);

  seccal(rnxhead->StartTime,&year,&month,&day,&hour,&min,&sec,&sec_frac);

  seconds = (double)sec + (double)sec_frac;
    
  fprintf(dst,"     2.20           OBSERVATION DATA    GPS               ");
  fprintf(dst,"  RINEX VERSION / TYPE\n");

  fprintf(dst,"%-20s%-20s%-20sPGM / RUN BY / DATE\n", rnxhead->pgm,runby,date);

  fprintf(dst,"%-60sMARKER NAME\n", rnxhead->marker);

  fprintf(dst,"%-20s%-40sOBSERVER / AGENCY\n", rnxhead->observer, 
                                               rnxhead->agency);

  fprintf(dst,"%-20s%-20s%-20sREC # / TYPE / VERS\n", rnxhead->recnum, 
                                                      rnxhead->rectype, 
                                                      rnxhead->recvers);

  fprintf(dst,"%-20s%-20s                    ANT # / TYPE\n", rnxhead->antnum, 
                                                              rnxhead->anttype);

  fprintf(dst,"%14.4f%14.4f%14.4f                  APPROX POSITION XYZ\n", 
              rnxhead->pos[0], rnxhead->pos[1], rnxhead->pos[2]);

  fprintf(dst,"%14.4f%14.4f%14.4f                  ANTENNA: DELTA H/E/N\n", 
              rnxhead->ant[0], rnxhead->ant[1], rnxhead->ant[2]);

  fprintf(dst,"     1     1     0                                        ");
  fprintf(dst,"  WAVELENGTH FACT L1/2\n");
  
  if (do_snrs) 
  {
    if (do_ca)
    {
      fprintf(dst,"     9    L1    L2    C1    P1    P2    LA    SA    S1    S2");
    }
    else
    {
      fprintf(dst,"     7    L1    L2    C1    P1    P2    S1    S2            ");
    }
  }
  else 
  {
    if (do_ca)
    {
      fprintf(dst,"     6    L1    L2    C1    P1    P2    LA                  ");
    }
    else
    {
      fprintf(dst,"     5    L1    L2    C1    P1    P2                        ");
    }
  }
  fprintf(dst,"# / TYPES OF OBSERV\n");

  fprintf(dst,"%6d                                                    ",
              Roundint(rnxhead->Interval));
  fprintf(dst,"  INTERVAL\n");
  fprintf(dst,"  %04d    %02d    %02d    %02d    %02d   %09.6f      GPS     ",
               year, month, day,hour,min,seconds);
  fprintf(dst,"    TIME OF FIRST OBS\n");

  fprintf(dst,
    "SNR is mapped to signal strength [0,1,2-9]                  COMMENT\n");
  fprintf(dst,
    " SNR: >500 >100  >50  >10   >5   >3  >1  >0  bad  n/a       COMMENT\n");
  fprintf(dst,
    " sig:    9    8    7    6    5    4   3   2    1    0       COMMENT\n");
  fprintf(dst,
    " Loss of Lock Indicator is set for all phase observations   COMMENT\n");
  fprintf(dst,
    " in case a phase break/cycle slip is detected.              COMMENT\n");
  fprintf(dst,
    " Loss of Lock Indicator is also set at acquisition of a PRN COMMENT\n");
  fprintf(dst,
    " Undetected cycle slips may remain in this file.            COMMENT\n");
  fprintf(dst,
    "                                                            END OF HEADER\n");
  
  return(0);
}
/******************************************************************
* FUNCTION: ROUNDINT
* Purpose:
* Rounds a double precision number to the nearest integer
*
* Input:
*    x    - Double to be rounded to integer
*
* Function returns x rounded to the nearest integer
*
* Written by: Shailen Desai
* Date      : 03/24/97
* Modified  : Gerhard L.H. Kruizinga
* Date      : 08/10/98
******************************************************************/

long Roundint(double x)
{
  double rem, ipart;

  rem = modf(x, &ipart);
  if (rem >= 0.5) {
    ipart += 1.0;
  }
  if (rem <= -0.5) {
    ipart -= 1.0;
  }
  return ((long) ipart);
}

