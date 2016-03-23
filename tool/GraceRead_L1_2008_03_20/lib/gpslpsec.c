/****************************************************************************
*      RTG Source Code,                                                     *
*      Copyright (C) 1996, California Institute of Technology               *
*      U.S. Government Sponsorship under NASA Contract NAS7-1260            *
*                    (as may be time to time amended)                       *
*                                                                           *
*      RTG is a trademark of the California Institute of Technology.        *
*                                                                           *
*                                                                           *
*      written by Yoaz Bar-Sever, Willy Bertiger, Bruce Haines,             *
*                 Angelyn Moore, Ron Muellerschoen, Tim Munson,             *
*                 Larry Romans, and Sien Wu                                 *
****************************************************************************/
/* Purpose:  to determine the number of GPS leap seconds given utc time. 
             The utc time is given in UTC seconds past J2000.

     Input:
	   real utc_time        sec past j2000 
           table table_leap     table_leap.x[] UTC sec past J2000 time of 
                                table_leap.y[] contains TAI - UTC at the 

     Output:
	   int lpsec           number of gps leapseconds at the given time
	   real before	       time of leapsecond before input time

*/

#include "TimeLib.h" 

void gpslpsec(real utc_time, int *lpsec, real *before, table leapsecs)
{
  static char SccsId[] = "@(#) gpslpsec.c       1.1 01/08/01";

  int j; 

  *lpsec  = 0;
  *before = -1577880000.0; 

  for(j=0;j<leapsecs.size;j++)
  {
    if (utc_time >= leapsecs.x[j]) {
       *before= leapsecs.x[j];
    }
  }

/* The following is tai-utc + gps-tai */
   *lpsec = (int) taiutc( utc_time, leapsecs ) - 19;

}
