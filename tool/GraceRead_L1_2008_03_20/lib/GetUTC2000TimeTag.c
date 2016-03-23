#include <time.h>
#include <string.h>
#include <stdlib.h>
#include "GRACEiolib.h"
#include "TimeLib.h"

static char SccsId[] = "$Id: GetUTC2000TimeTag.c,v 1.2 2004/08/30 21:03:33 wib Exp $";

double GetUTC2000TimeTag()
/*----------------------------------------------------------------------------->
/ purpose: produce sec past 2000 UTC time tag at time of evaluation of 
/          this routine
/          
/
/ coded by: Gerhard L.H. Kruizinga                           08/15/2000
/ modified: Gerhard L.H. Kruizinga                           05/07/2001
/
/ return: time_tag  time tag string in seconds past 2000
/
<-----------------------------------------------------------------------------*/
{
  double  time_tag;
  time_t   now;
  int len;
  int year,month,day,hour,minute,second;

  char time_tag1[HEADERMAXCHAR];
 
  now = time(NULL);

  strftime(time_tag1,HEADERMAXCHAR,"%Y %m %d %H %M %S",gmtime(&now));

  sscanf(time_tag1,"%d %d %d %d %d %d",&year,&month,&day,&hour,&minute,&second);

  time_tag = calsec(year,month,day,hour,minute,second,0.0);

  return time_tag;
}
