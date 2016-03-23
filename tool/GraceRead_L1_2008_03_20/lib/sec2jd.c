/* 

   Purpose:
          Convert seconds past J2000.0 to Julian Date

   11/07/95 Willy Bertiger

*/

#include "TimeLib.h" 

real sec2jd( 

/* Input: */
            real sec 
/* Output: function value */
            )

{

/* Local: */
  static char SccsId[] = "@(#) sec2jd.c       1.1 01/08/01";

  return sec/sec_per_day + jdref ;

}






