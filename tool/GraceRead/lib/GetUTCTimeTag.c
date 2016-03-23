#include <time.h>
#include <string.h>
#include <stdlib.h>
#include "GRACEiolib.h"

static char SccsId[] = "$Id: GetUTCTimeTag.c,v 1.4 2004/08/30 21:03:33 wib Exp $";


void GetUTCTimeTag(char *time_tag)
/*----------------------------------------------------------------------------->
/ purpose: produce UTC time tag at time of evaluation of this routine
/          in FileHeader_t struct
/
/ coded by: Gerhard L.H. Kruizinga                           08/15/2000
/
/ output: *time_tag  Pointer to time tag string
/
/
<-----------------------------------------------------------------------------*/
{
  time_t   now;
  int len;

  char time_tag1[HEADERMAXCHAR];
 
  now = time(NULL);

  strftime(time_tag,HEADERMAXCHAR,"%Y-%m-%d %H:%M:%S",gmtime(&now));
}
