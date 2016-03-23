/* Purpose:  to return the UTC time in secons past J2000
             given gps time. The time is given in gps seconds past J2000. */

  /* Input:  
     real gps_time        gps time given in seconds past J2000
     table table_leap     table containing leapseconds (table_leap.x[]), 
                          the time of the leapseconds (table_leap.y[]),
                          and the number of entries(table_leap.size)
   
     Output:
     real (return value)  utc time given in seconds past J2000  */

#include <stdio.h>
#include "TimeLib.h" 

real gps2utc(real gps_time, table table_leap)
{
  static char SccsId[] = "@(#) gps2utc.c       1.1 01/08/01";

  /* Local variables: */ 
  real utc_t1;            /* utc time in seconds past J2000 */ 
  real b4lp;              /* time of leapsecond before input time */
  int lpsec;              /* number of gps leapseconds */

/* Call gpslpsec twice to avoid being off by one second when */
/* the utc time is less than gpslpsec before a leap second   */

  gpslpsec(gps_time,&lpsec,&b4lp,table_leap);
  utc_t1 = gps_time - lpsec;

  /* Without the following fix, the following would happen: 

  1-JUL-1993 00:00:10 GPS TIME --->  1-JUL-1993 00:00:01 UTC TIME
  1-JUL-1993 00:00:09 GPS TIME --->  1-JUL-1993 00:00:01 UTC TIME
  1-JUL-1993 00:00:08 GPS TIME --->  1-JUL-1993 00:00:00 UTC TIME
  1-JUL-1993 00:00:07 GPS TIME ---> 30-JUN-1993 23:59:59 UTC TIME

  The fix allows the following conversion: 

  1-JUL-1993 00:00:10 GPS TIME --->  1-JUL-1993 00:00:01 UTC TIME
  1-JUL-1993 00:00:09 GPS TIME --->  1-JUL-1993 00:00:00 UTC TIME
  1-JUL-1993 00:00:08 GPS TIME --->  1-JUL-1993 00:00:00 UTC TIME
  1-JUL-1993 00:00:07 GPS TIME ---> 30-JUN-1993 23:59:59 UTC TIME */

  if( utc_t1 == b4lp )
  {
    return (gps_time - lpsec);
  }

  gpslpsec(utc_t1,&lpsec,&b4lp,table_leap);

  return(gps_time-lpsec);
}
