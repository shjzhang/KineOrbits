#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "GRACEiolib.h"
#include "GRACEgpslib.h"
#include "TimeLib.h"

static char SccsId[] = "$Id: gps2prnh.c,v 1.3 2004/08/30 21:03:32 wib Exp $";


#define MAXCHAR   1000
#define MAXGPS2PRN  10 
#define MAXGPSSAT  101 
#define NOGPSID     -1

long gps2prnh(long gps, double T2000)
/*----------------------------------------------------------------------------->
/ purpose: return prn number based on gps number and time tag 2000
/
/ coded by: G.L.H. Kruizinga        10/14/01
/
/ input:  long gps     gps number 
/         double T2000 time in sec past 2000 for which gps number is requested
/
/ output: Return prn number if valid requestion otherwise 0 is returned.
/
/ Note: Enviroment variable PRN_GPS must be set. (at JPL: /goa/etc/PRN_GPS)
/-----------------------------------------------------------------------------*/
{
  FILE             *gpsprn;

  static long      first = 1;

  char             filename[MAXCHAR],line[MAXCHAR];
  char             start_date[MAXCHAR],deact_date[MAXCHAR];
  char             *p;

  long             prn_id,gps_id;
  long             i,j,prn_out,ndx;

  int              year,month,day;

  static double    sec_frac = 0.0,No_time = -999999999.0;

  static double    start_time[MAXGPSSAT][MAXGPS2PRN];
  static double    final_time[MAXGPSSAT][MAXGPS2PRN];

  static long      gps2prn_count[MAXGPSSAT]; 
  static long      gps2prn[MAXGPSSAT][MAXGPS2PRN];

  static int       hour= 0,minute = 0,second = 0;

  if (first == 1)
  {
    loop(i,MAXGPSSAT)
    {
      loop(j,MAXGPS2PRN) 
      { 
        start_time[i][j] = No_time;
        final_time[i][j] = No_time;
        gps2prn[i][j] = NOGPSID;
      }
      gps2prn_count[i] = 0;
    }

    strcpy(filename,"/goa/etc/PRN_GPS");
/*
    strcpy(filename,getenv("PRN_GPS"));
*/
    gpsprn = fopen(filename,"r");
    if (gpsprn == NULL)
    {
     fprintf(stderr,"\n PRN_GPS file  %s cannot be opened !! \n\n", filename);
     return 0L;
    }

    if (fgets(line,MAXCHAR,gpsprn) == NULL)
    {
     fprintf(stderr,"\n PRN_GPS file  %s is empty !! \n\n", filename);
     return 0L;
    }

    while (fgets(line,MAXCHAR,gpsprn) != NULL)
    {
      sscanf(line,"%s %s %d %d",start_date,deact_date,&gps_id,&prn_id);

      if (gps_id < 0 || gps_id > MAXGPSSAT-1)
      {
        fprintf(stderr,"\n GPS = %d is out of range. should be 0<->100\n\n",
                       gps_id);
        exit(1);
      }
      if (gps2prn_count[gps_id] + 1 > MAXGPS2PRN) 
      {
        fprintf(stderr,"\n Number of gps2prn's exceeds %d in file %s !!\n",
                        MAXGPS2PRN,filename);
        fprintf(stderr,
                " Change MAXGPS2PRN in gps2prnh.c to accomodate entries\n\n");
        exit(1);
      }

      sscanf(start_date,"%4d-%2d-%2d",&year,&month,&day);
 
      gps2prn[gps_id][gps2prn_count[gps_id]] = prn_id;

      start_time[gps_id][gps2prn_count[gps_id]] = 
                         calsec(year,month,day,hour,minute,second,sec_frac);
      
      if (strcmp(deact_date,"0000") != 0) 
      {
        sscanf(deact_date,"%4d-%2d-%2d",&year,&month,&day);
        final_time[gps_id][gps2prn_count[gps_id]] = 
                         calsec(year,month,day,hour,minute,second,sec_frac);
      }

      gps2prn_count[gps_id]++;
    }

    first = 0;
    fclose(gpsprn);

/*
    loop(i,MAXGPSSAT)
    {
      if (gps2prn_count[i] != 0)
      {
        loop(j,gps2prn_count[i])
        {
          fprintf(stderr,"prn ts tf : %d %f %f %d\n",i,start_time[i][j],
                  final_time[i][j],gps2prn[i][j]);
        }
        fprintf(stderr,"\n");
      }
    }
*/
  }

  if (gps < 0 || gps > MAXGPSSAT-1) 
  {
    fprintf(stderr,"\n GPS = %d is out of range. should be 0<->100\n\n",
                    gps_id);
    return 0L;
  }

  prn_out = 0;


/*
  ndx = 0;

  while (T2000 > start_time[gps][ndx] && ndx < gps2prn_count[gps])
  {
    prn_out = gps2prn[gps][ndx]; 
    if (final_time[gps][ndx] != No_time)
      if (T2000 > final_time[gps][ndx]) prn_out = 0;
    ndx++;
  }
*/

  prn_out = 0;

  loop(ndx,gps2prn_count[gps])
  {
    if (start_time[gps][ndx] == No_time)
    {
      if (final_time[gps][ndx] == No_time)
        prn_out =  gps2prn[gps][ndx];
      else
        if (T2000 < final_time[gps][ndx]) prn_out =  gps2prn[gps][ndx];
    }  
    else
    {
      if (final_time[gps][ndx] == No_time)
      {
        if (T2000 >= start_time[gps][ndx]) prn_out =  gps2prn[gps][ndx];
      }
      else
      {
        if ( (T2000 < final_time[gps][ndx]) &&
             (T2000 >= start_time[gps][ndx]) ) prn_out =  gps2prn[gps][ndx];
      }
    }
  }

  return prn_out;
}
