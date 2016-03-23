#include <stdio.h>
#include <string.h>
#include <time.h>
#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"
#include "GRACEio_utils.h"
#include "GRACEgpslib.h"
#include "TimeLib.h"

#define MAXCHAR 1000

static char SccsId[] = "$Id: Write_CMT_command.c,v 1.9 2004/08/30 21:03:39 wib Exp $";

void GetUTCTag(char *time_tag, char *time_tag2);

long Write_CMT_command(CMT_command_t *record)
/*----------------------------------------------------------------------------->
/ purpose:  write data in CMT_command struct in to file dst
/
/ coded by: Gerhard L.H. Kruizinga                01/10/02
/
/ return  0L succesful command written
/        -1L cannot open command file for writing
/
/ note: sample for MTE 1 output of this routine
/ note: sample for MTE 2 output of this routine
<-----------------------------------------------------------------------------*/
{
  FILE   *dst;

  double Xcg_change,Ycg_change,Zcg_change,dsteps;
  double Trim_Mass,MicronPerStep;
  double frac,secs,NewYearTime,CreateTime;

  long   xstep,ystep,zstep;
  long   MTE_number[2],GRACE_number;
  
  char   MTE_name[MAXCHAR],filename[MAXCHAR];
  char   TimeTagUTC[MAXCHAR];
  char   TimeTagUTC2[MAXCHAR];
  char   DirX[MAXCHAR],DirY[MAXCHAR],DirZ[MAXCHAR];
  char   Mass_Time[MAXCHAR];

  int    year,month,day,hour,minute,second,doy,year_doy;

  MTE_number[0] = 236;
  MTE_number[1] = 237;

  Trim_Mass     = 4.8; /* kg */
  MicronPerStep = 2.5; 
  frac          = 0.0;

  sprintf(MTE_name,"MTE%1d",record->MTE_id);

  GRACE_number = 1;
  if (record->GRACE_id == 'B') GRACE_number = 2;

  GetUTCTag(TimeTagUTC,TimeTagUTC2);

  sscanf(TimeTagUTC,"%d %d %d %d %d %d",&year,&month,&day,&hour,&minute,&second);

  NewYearTime = calsec (year,1,1,0,0,0,frac);
  CreateTime  = calsec(year,month,day,hour,minute,second,frac);

  year_doy    = year-2000;
  doy         = (CreateTime - NewYearTime)/86400.0;
  doy++;

  Xcg_change = -1e6*record->x_cg;   
  Ycg_change = -1e6*record->y_cg;   
  Zcg_change = -1e6*record->z_cg;   

  dsteps = (Xcg_change/MicronPerStep)*(record->Mass/Trim_Mass);
  xstep  = roundint(dsteps);
  dsteps = (Ycg_change/MicronPerStep)*(record->Mass/Trim_Mass);
  ystep  = roundint(dsteps);
  dsteps = (Zcg_change/MicronPerStep)*(record->Mass/Trim_Mass);
  zstep  = roundint(dsteps);

  strcpy(DirX,"POS");
  strcpy(DirY,"POS");
  strcpy(DirZ,"POS");

  if (xstep < 0) {strcpy(DirX,"NEG");xstep = -1 *xstep;}
  if (ystep < 0) {strcpy(DirY,"NEG");ystep = -1 *ystep;}
  if (zstep < 0) {strcpy(DirZ,"NEG");zstep = -1 *zstep;}

  if (xstep > 12000) 
  {
    fprintf(stderr,"\n Number of steps (%d) exceeds distance (0.125 mm/sec) that can be travelled in 4 minutes for x-axis\n\n",xstep);
    fprintf(stderr," CG trim needs to be broken up in multiple parts\n\n");
    exit(1);
  }
  if (ystep > 12000) 
  {
    fprintf(stderr,"\n Number of steps (%d) exceeds distance (0.125 mm/sec) that can be travelled in 4 minutes for y-axis\n\n",ystep);
    fprintf(stderr," CG trim needs to be broken up in multiple parts\n\n");
    exit(1);
  }
  if (zstep > 12000) 
  {
    fprintf(stderr,"\n Number of steps (%d) exceeds distance (0.125 mm/sec) that can be travelled in 4 minutes for z-axis\n\n",zstep);
    fprintf(stderr," CG trim needs to be broken up in multiple parts\n\n");
    exit(1);
  }

  seccal(record->Mass_time,&year,&month,&day,&hour,&minute,&second,&frac);

  sprintf(Mass_Time,"%4d/%02d/%02d %02d:%02d:%02d",year,month,day,hour,minute,second);

  sprintf(filename,"GR%1d_CMT_%s_TT_%02d%03d_%1d.rod",
          GRACE_number,MTE_name,year_doy,doy,record->version);

  if ((dst = fopen(filename,"w")) == NULL)
  {
    fprintf(stderr,"\n Cannot open filename %s for writing\n\n",filename);
    return -1L;
  }

  fprintf(dst,"!\n");
  fprintf(dst,"! FOP:     N_CMT_TRIM_%s_TT using\n",MTE_name);
  fprintf(dst,"!          %s\n",SccsId);
  fprintf(dst,"!\n");
  fprintf(dst,"! Generated: %s (yyyy/mo/dd hh_mm_ss UTC) at JPL\n",TimeTagUTC2);
  fprintf(dst,"!\n");
  fprintf(dst,"! Input product filename                 = %s\n",record->prd_name);
  fprintf(dst,"! Input product file creation time (UTC) = %s\n",record->prd_ttag);
  fprintf(dst,"!\n");
  fprintf(dst,"! Commands provided for %s on GRACE %d\n",MTE_name,GRACE_number);
  fprintf(dst,"!\n");
  fprintf(dst,"! Command CG change in X-axis = %7.2f (micron) = %s %5d (steps)\n",
                Xcg_change,DirX,xstep);
  fprintf(dst,"! Command CG change in Y-axis = %7.2f (micron) = %s %5d (steps)\n",
                Ycg_change,DirY,ystep);
  fprintf(dst,"! Command CG change in Z-axis = %7.2f (micron) = %s %5d (steps)\n",
                Zcg_change,DirZ,zstep);
  fprintf(dst,"!\n");
  fprintf(dst,"! Absolute distance travelled prior to CG Trim by %s-X = %8.2f (mm)\n",MTE_name,
                 record->abs_dist_x*1e3);
  fprintf(dst,"! Absolute distance travelled prior to CG Trim by %s-Y = %8.2f (mm)\n",MTE_name,
                 record->abs_dist_y*1e3);
  fprintf(dst,"! Absolute distance travelled prior to CG Trim by %s-Z = %8.2f (mm)\n",MTE_name,
                 record->abs_dist_z*1e3);
  fprintf(dst,"!\n");
  fprintf(dst,"! note: Absolute distance origin is assumed to be the MTM start position\n");
  fprintf(dst,"!       which is assumed to be in the middle of the total range\n");
  fprintf(dst,"!       for which the MTM can operate!\n");
  fprintf(dst,"!\n");
  fprintf(dst,"! Spacecraft mass = %7.3f Kg at %s (GPS time)\n",record->Mass,Mass_Time);
  fprintf(dst,"!\n");

  /*>>>> start writing commands <<<<*/ 

  fprintf(dst,"BREAKPOINT CO=\"! N_CMT_TRIM_%s_TT\"\n",MTE_name);
  fprintf(dst,"XMT [?1;PW_ON,%s]\n",MTE_name);
  fprintf(dst,"XMT [+00:00:10;MMU_AP_COLLR,%d,1,10]\n",MTE_number[record->MTE_id-1]);
  fprintf(dst,"XMT [+00:00:02;RT_AP_COLLR,%d,1,10]\n",MTE_number[record->MTE_id-1]);
  fprintf(dst,"XMT [+00:00:03;IC_HP_SETR,%d,1,1]\n",MTE_number[record->MTE_id-1]);

  if (ystep != 0)
    fprintf(dst,"XMT [+00:00:45;MTM_Y,%s,%s,%d]\n",MTE_name,DirY,ystep);
  else
    fprintf(dst,"!XMT [+00:00:45;MTM_Y,%s,%s,%d]\n",MTE_name,DirY,ystep);

  if (xstep != 0)
    fprintf(dst,"XMT [+00:04:00;MTM_X,%s,%s,%d]\n",MTE_name,DirX,xstep);
  else
    fprintf(dst,"!XMT [+00:04:00;MTM_X,%s,%s,%d]\n",MTE_name,DirX,xstep);

  if (zstep != 0)
    fprintf(dst,"XMT [+00:04:00;MTM_Z,%s,%s,%d]\n",MTE_name,DirZ,zstep);
  else
    fprintf(dst,"!XMT [+00:4:00;MTM_Z,%s,%s,%d]\n",MTE_name,DirZ,zstep);

  fprintf(dst,"XMT [+00:05:00;MMU_AP_COLLR,%d,1,60]\n",MTE_number[record->MTE_id-1]);
  fprintf(dst,"XMT [+00:05:00;PW_OFF,%s]\n",MTE_name);
  fprintf(dst,"XMT [+00:00:10;MMU_AP_COLLR,%d,1,0]\n",MTE_number[record->MTE_id-1]);
  fprintf(dst,"XMT [+00:00:02;RT_AP_COLLR,%d,1,0]\n",MTE_number[record->MTE_id-1]);
  fprintf(dst,"XMT [+00:00:01;IC_HP_SETR,%d,1,0]\n",MTE_number[record->MTE_id-1]);
  fprintf(dst,"XMT [+00:00:02;ST_DMP_IDR,RAM,TM]\n");
  fprintf(dst,"! EOF\n");
  
  fclose(dst);

  return 0L;
}
void GetUTCTag(char *time_tag, char *time_tag2)
/*----------------------------------------------------------------------------->
/ purpose: produce UTC time tag at time of evaluation of this routine
/          in FileHeader_t struct
/
/ coded by: Gerhard L.H. Kruizinga                           08/15/2000
/ modified: Gerhard L.H. Kruizinga                           01/10/2002
/
/ output: *time_tag  Pointer to time tag string
/
<-----------------------------------------------------------------------------*/
{
  time_t   now;
  int len;

  char time_tag1[HEADERMAXCHAR];

  now = time(NULL);

  strftime(time_tag,HEADERMAXCHAR,"%Y %m %d %H %M %S",gmtime(&now));
  strftime(time_tag2,HEADERMAXCHAR,"%Y/%m/%d %H:%M:%S",gmtime(&now));
}
