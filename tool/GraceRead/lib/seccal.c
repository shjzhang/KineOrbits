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
/* 
 
Purpose:
 
   This subroutine (SEConds to CALendar date) takes an input seconds past
   the Julian reference date (JDREF) for this library and returns the 
   components of the corresponding calendar date. The components of the
   calendar date are all returned as numbers to allow for use in computations
   For instance, the month is returned as the integer month number rather
   than as a character string.

   Adapted from timetrans SECCAL.F
   Bruce Haines  3/5/95

*/
#include "TimeLib.h"
#include <math.h>

void seccal(

/* Input: */

		real sec,   /* seconds past ref date */

/* Output */ 

                int *year,  
      		int *month,
		int *day,
		int *hour,
		int *minute,
		int *second,
		real *frac 
            )
{

/* Local Variables */

   static char SccsId[] = "@(#) seccal.c       1.1 01/08/01";
   real jd;
   real temp; 

/* Method:
 
   Extract the fraction seconds (frac) from sec. Note that care must
   taken if sec < 0 since frac must be a non-negative number < 1. The o
   fraction seconds of the calendar representation is simply the frac
   part of the input seconds and can be computed immediately. The rem
   integral seconds is converted to the rest of the calendar date. Th
   done to avoid round off error that could be introduced by the
   intermediate conversion to Julian date. */

   *frac = sec - ((int) sec);
   if ( *frac < 0.0 ) {
      *frac += 1;
   }

/* call sec2jd to convert the integral seconds to the Julian date. */

   jd = sec2jd( sec - *frac + 0.5);

/* call jd2cal to convert the Julian date to calandar date. */

   jd2cal( jd, year, month, day, hour, minute, second, &temp);
   
}
