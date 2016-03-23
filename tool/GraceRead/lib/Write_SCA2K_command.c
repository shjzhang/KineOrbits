#include <stdio.h>
#include <string.h>
#include <time.h>
#include "GRACEiolib.h"
#include "GRACEio_prototypes.h"
#include "GRACEio_utils.h"
#include "GRACEgpslib.h"
#include "TimeLib.h"

#define MAXCHAR 1000

static char SccsId[] = "$Id: Write_SCA2K_command.c,v 1.9 2004/08/30 21:03:39 wib Exp $";

void GetUTCTag1(char *time_tag, char *time_tag2);

long Write_SCA2K_command(SCA2K_command_t *record)
/*----------------------------------------------------------------------------->
/ purpose:  write data in SCA2K_command struct in to file dst
/
/ coded by: Gerhard L.H. Kruizinga                01/11/02
/
/ return  0L succesful command written
/        -1L cannot open command file for writing
/
/ note: sample output
/ ! EOF 
<-----------------------------------------------------------------------------*/
{
  FILE   *dst;

  double frac,secs,NewYearTime,CreateTime;

  long   GRACE_number;
  
  char   filename[MAXCHAR];
  char   TimeTagUTC[MAXCHAR];
  char   TimeTagUTC2[MAXCHAR];

  int    year,month,day,hour,minute,second,doy,year_doy;


  GRACE_number = 1;
  if (record->GRACE_id == 'B') GRACE_number = 2;

  GetUTCTag1(TimeTagUTC,TimeTagUTC2);

  sscanf(TimeTagUTC,"%d %d %d %d %d %d",&year,&month,&day,&hour,&minute,&second);

  NewYearTime = calsec (year,1,1,0,0,0,0.0);
  CreateTime  = calsec (year,month,day,hour,minute,second,0.0);
  year_doy    = year-2000;

  doy         = (CreateTime - NewYearTime)/86400.0;
  doy++;

  sprintf(filename,"GR%1d_AO_SCA2K_CNF_%02d%03d_%1d.odl",
          GRACE_number,year_doy,doy,record->version);

  if ((dst = fopen(filename,"w")) == NULL)
  {
    fprintf(stderr,"\n Cannot open filename %s for writing\n\n",filename);
    return -1L;
  }

  fprintf(dst,"!\n");
  fprintf(dst,"! FOP:     N_AO_SCA2K_CNF_GR%1d using\n",GRACE_number);
  fprintf(dst,"!          %s\n",SccsId);
  fprintf(dst,"!\n");
  fprintf(dst,"! Generated: %s (yyyy/mo/dd hh_mm_ss UTC) at JPL\n",TimeTagUTC2);
  fprintf(dst,"!\n");
  fprintf(dst,"! Input product filename                 = %s\n",record->prd_name);
  fprintf(dst,"! Input product file creation time (UTC) = %s\n",record->prd_ttag);
  fprintf(dst,"!\n");
  fprintf(dst,"! Commands provided for GRACE %d\n",GRACE_number);
  fprintf(dst,"!\n");
  fprintf(dst,"! Command SCA1 SCA2K quaternion\n");
  fprintf(dst,"!         q1_sca1 = %.16g\n",record->q1_sca1);
  fprintf(dst,"!         q2_sca1 = %.16g\n",record->q2_sca1);
  fprintf(dst,"!         q3_sca1 = %.16g\n",record->q3_sca1);
  fprintf(dst,"!         q4_sca1 = %.16g\n",-record->q0_sca1); /* note inverse quat!! */
  fprintf(dst,"! Command SCA2 SCA2K quaternion\n");
  fprintf(dst,"!         q1_sca2 = %.16g\n",record->q1_sca2);
  fprintf(dst,"!         q2_sca2 = %.16g\n",record->q2_sca2);
  fprintf(dst,"!         q3_sca2 = %.16g\n",record->q3_sca2);
  fprintf(dst,"!         q4_sca2 = %.16g\n",-record->q0_sca2); /* note inverse quat!! */
  fprintf(dst,"!\n");

  /*>>>> start writing commands <<<<*/ 

  fprintf(dst,"BREAKPOINT CO=\"! N_AO_SCA2K_CNF_GR%1d\"\n",GRACE_number);
  fprintf(dst,"XMT [ST_DMP_ADR,RAM,3]\n");
  fprintf(dst,"XMT [AO_S_KSCAM,%.16g,%.16g-\n,%.16g,%.16g-\n,%.16g,%.16g-\n,%.16g,%.16g]\n",
                     record->q1_sca1,
                     record->q2_sca1,
                     record->q3_sca1,
                     -record->q0_sca1, /* note inverse quat!! */
                     record->q1_sca2,
                     record->q2_sca2, 
                     record->q3_sca2,
                     -record->q0_sca2); /* note inverse quat!! */
  fprintf(dst,"XMT [ST_DMP_ADR,RAM,3]\n");
  fprintf(dst,"! EOF\n");
  
  fclose(dst);

  return 0L;
}
void GetUTCTag1(char *time_tag, char *time_tag2)
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
