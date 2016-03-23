/* 

Purpose: 
 
   This subroutine (Julian date 2 calendar DATE) converts the input inte
   Julian date to the corresponding Gregorian calendar date. Since the J
   date is an integer, this correspondence is exact for noon of the cale
   date.
 
   The algorithm for this conversion is taken from the following article
   Tantzen,R.T., "Communications of the ACM", Volume 6, Number 8, August
   Algorithm 199, page 444.  

   Adapted from timetrans J2DATE.F
   Bruce Haines 3/5/96

*/

#include "TimeLib.h"

void j2date(

/* Input: */
		int jd,      /* is the integer julian data */
/* Output */

                int *year,
		int *month,
		int *day
	   )
{

/* Local: */
  static char SccsId[] = "@(#) j2date.c       1.1 01/08/01";

  int j;
  int y;
  int m;
  int d;

  j = jd;
  j = j - 1721119;
  y = (4*j-1)/146097;
  j = 4*j - 1 - 146097*y;
  d = j/4;
  j = (4*d+3)/1461;
  d = 4*d + 3 -1461*j;
  d = (d+4)/4;
  m = (5*d-3)/153;
  d = 5*d - 3 - 153*m;
  d = (d+5)/5;
  y = 100*y + j;
  if (m < 10) {
     m = m + 3;
  } else {
     m = m - 9;
     y = y + 1;
  }
  *year   = y;
  *month  = m;
  *day    = d;

}
