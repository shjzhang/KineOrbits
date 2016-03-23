/* 

Purpose:

   This subroutine (Julian Date 2 CALendar date) takes an input Julian d
   and returns the various components of the corresponding calendar date
   The components of the calendar date are all returned as numbers to al
   for use in computation. For instance, the month is returned as the
   integer month number rather than as a character string.

   Adapted from timetrans JD2CAL.F
   Bruce Haines 3/5/96

*/

#include "TimeLib.h"
#include <math.h>

void jd2cal (

/* Input */
		real jd,	/* Julian date       */
/* Output */
		int *year,  	/* Year               */
		int *month,	/* Month Number       */
		int *day,	/* Day                */
		int *hour,	/* Hour               */
		int *minute,	/* Minute             */
		int *second,	/* Second             */
		real *frac 	/* Fractional Seconds */
            )
{

/* Local Variables */

   static char SccsId[] = "@(#) jd2cal.c       1.1 01/08/01";

   real jdplus;
   int jdint;
   real dsec;
   int isec;

/* Method:   
 
    Compute jdint, the integer Julian date at noon of the current day
    compute dsec, the number of seconds elapsed since the start of the
    current day.

*/ 
   jdplus = jd + 0.5;
   jdint = (int) jdplus;
   dsec = sec_per_day * ( jdplus - jdint );
   if (dsec > sec_per_day) {
      jdint += 1;
      dsec = dsec - sec_per_day;
   }
   isec = (int) dsec;
 
/* call j2date with jdint to compute the year, month, and day of the
   calendar date. */

   j2date( jdint, year, month, day);

/* compute hour */

   *hour = isec/3600;
   isec = (int) (isec - 3600 * *hour);
 
/* compute minute */

   *minute = isec/60;
   isec = (int) ( isec - 60 * *minute );
 
/* compute second */

   *second = isec;
 
/* compute frac */

   *frac = dsec - ( (int) dsec );
}
