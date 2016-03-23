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
     This function will return the difference
     TAI-UTC in seconds for the specified utc time.  */

#include "TimeLib.h"

  /* Purpose:  This function will return the difference
     TAI-UTC in seconds for the specified utc time.  */

real taiutc(
  /* Input: */
	    real utc,		/* The value at which TAI-UTC is to be */
				/* determined.  utc is in seconds past J2000*/
	    table  leap)	/* leap second table */
{


/* Output:
     real (return val)   TAI-UTC in seconds */

/* Local variables */
  static char SccsId[] = "@(#) taiutc.c       1.1 01/08/01";
  int i;



  if (leap.x[0] > utc) { return 0.0;}

  for(i=0; i<leap.size && leap.x[i] <= utc; i++);   /* leap.x is never tested 
                                                       outside its range */

  return (leap.y[i-1]);

} 
