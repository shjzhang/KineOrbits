#include "TimeLib.h"

  /* Purpose:  This function will return the tdt time corresponding to
     the UTC time */

real utc2gps(
		real utc,             /* Input: utc is in seconds past J2000 */
                table  leap)          /* Input: leap second table */
  /* Output: real (return val)   GPS in seconds */
{
  /* leap seconds table contains TAI-UTC */
   static char SccsId[] = "@(#) utc2gps.c       1.1 01/08/01";

   real r;
 
   r = (taiutc(utc, leap) + utc - 19.0);   /* This returns gps time. */
   return r;

} 


