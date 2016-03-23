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

/*  %Z% %M% %I% %G%  */ 

#include "GRACEdefs.h"

#ifndef _TimeLib_h_
#define _TimeLib_h_

#define jdref (2451545.0)      /* J2000.0 (January 1, 2000, 12 hours) */
#define sec_per_day (86400.0)

#define gps_strt (-630763200.0) /* seconds past J2000 
                                   on 06-Jan-1980 00:00:00.000 
                                   gps time is counted from here*/


void cal2ch(
/* Inputs*/ int year,
            int month,
            int day,
            int hour,
            int minute,
            int second,
            real frac_second,
            short option_code, 
/* Outputs */
            char *cal_str);

real calsec(int year, int month, int day, int hour, int minute, int second, 
  real frac );

void ch2cal(char *string, int *year, int *month, int *day, int *hour, 
  int *minute, int *second, real *frac);

real ch2sec(char *string);

int date2j( int year, int month, int day );

int dayoyr(

/* Input: */
		int year, 	/* The Year Number      */
		int month, 	/* The Month Number     */
		int day		/* The Day of the Month */
/* Output:  function return                             */
          );

real gps2tdt( real gps );

real gps2utc(real gps_time, table table_leap);

void gpslpsec(real utc_time, int *lpsec, real *before, table leapsecs);

double gpsmin2sec( unsigned int gps_minutes, unsigned short gps_millisecs );

double gpsws2sec(
  /* Input: */
	    int gpsweek,	/* the gps week number */
	    double tow)	        /* seconds of GPS week */;

void j2date(

/* Input: */
		int jd,      /* is the integer julian data */
/* Output */

                int *year,
		int *month,
		int *day
	   );

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
            );

real jd2sec(

/* Input */
		real jd	/* Julian date       */
/* Output */
/* Return value:
   real   - seconds past J2000 */
            );

void sec2ch(
/* Inputs */
           real tsec,        /* time, in seconds */
           short opt_code,   /*0x0000: string is "DD-MMM HH:MM:SS.SSS" */
                             /*0x0001: string is "DD-MMM-YYYY HH:MM:SS.SSS"*/
/* Outputs */
           char *cal_str)  /* calendar character string */;

void sec2gpsmin( double gpstime, 
                 unsigned int *gps_minutes, unsigned short *gps_millisecs );

void sec2gpsws ( double sec, int *week, double *tow );

real sec2jd( 
/* Input: */
            real sec 
/* Output: function value */
            );

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
            );

char *stamper( 

/* Input: */
	      short option_code,   /* option code */
/* Output: */
              char *timestamp_str  /* note that stamper() returns a ptr*/
                                   /* to this string as well */ 
	      );

real taiutc(
  /* Input: */
	    real utc,		/* The value at which TAI-UTC is to be */
				/* determined.  utc is in seconds past J2000*/
	    table  leap)	/* leap second table */;

real tdt2et( real tdt );

real tdt2gps( real tdt );

real tdt2utc(
		real tdt,             /* Input: tdt is in seconds past J2000 */
                table  leap)          /* Input: leap second table */;

real utc2gps(
		real utc,             /* Input: utc is in seconds past J2000 */
                table  leap)          /* Input: leap second table */;

real utc2tdt(
		real utc,             /* Input: utc is in seconds past J2000 */
                table  leap)          /* Input: leap second table */;

table table_create(
                int size );           /*                                     */

table LoadLeapSeconds();              /* load leap second table              */

#endif /* _TimeLib_h_ */
