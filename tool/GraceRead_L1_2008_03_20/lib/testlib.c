#include <stdio.h>
#include <string.h>
#include "TimeLib.h"

#define MAXCHAR 1000

main ( )

{

 real frac_second,frac_out,gps_time,gps_time_out;
 real et,et_out,tdt,tdt_out;
 real utc_time,utc_time_out,Tbefore,gps_week_sec,gps_week_sec_out;
 real JulianDateReal,JulianDateReal_out;
 real tai,tai_out;

 int year, month, day, hour, minute, second;
 int year_out, month_out, day_out, hour_out, minute_out, second_out;

 int Doy,Doy_out,JulianDate,JulianDate_out;

 int leap_sec,gps_week,gps_week_out;

 unsigned int gps_minutes,gps_minutes_out;

 unsigned short gps_millisecs,gps_millisecs_out;

 short option_code;

 char cal_str[MAXCHAR],cal_str_out[MAXCHAR];

 table leap_table;

 leap_table = LoadLeapSeconds();

 year        = 2001;
 month       = 1;
 day         = 8;
 hour        = 11;
 minute      = 23;
 second      = 13;
 frac_second = 0.12345;

 option_code = 0;

 gps_time    = 32224993.1234500;
 utc_time    = 32224980.1234500; 
 
 gps_week    = 1096;
 gps_week_sec = 127393.123450;

 strcpy(cal_str,"8-JAN-2001 11:23:13.1235");

 seccal(gps_time,&year_out,&month_out,&day_out,&hour_out,
        &minute_out,&second_out,&frac_out);

 fprintf(stderr," seccal %d %d %d %d %d %d %f\n",year,month,day,hour,
                  minute,second,frac_second);
 fprintf(stderr," seccal %d %d %d %d %d %d %f\n",year_out,month_out,
                  day_out,hour_out,minute_out,second_out,frac_out);

 gps_time_out = calsec(year,month,day,hour,minute,second,frac_second);

 fprintf(stderr," calsec %f %f %f\n",gps_time,gps_time_out,gps_time-gps_time_out);

 utc_time_out = gps2utc(gps_time, leap_table);

 fprintf(stderr," gps2utc %f %f %f\n",utc_time,utc_time_out,utc_time-utc_time_out);

 gps_time_out = utc2gps(utc_time,leap_table);

 fprintf(stderr," utc2gps %f %f %f\n",gps_time,gps_time_out,gps_time-gps_time_out);

/*-----------------------------------------------------------------------
 cal2ch(year,month,day,hour,minute,second,frac_second,
        option_code,cal_str_out);

 fprintf(stderr," cal2ch %s %s\n",cal_str,cal_str_out);


 ch2cal(cal_str, &year_out, &month_out, &day_out, &hour_out, 
        &minute_out, &second_out, &frac_out);

 fprintf(stderr," ch2cal %d %d %d %d %d %d %d %f\n",year,month,day,hour,
                  minute,second,frac_second);
 fprintf(stderr," ch2cal %d %d %d %d %d %d %d %f\n",year_out,month_out,
                  day_out,hour_out,minute_out,second_out,frac_out);

 gps_time_out = ch2sec(cal_str);

 fprintf(stderr," ch2sec %f %f %f\n",gps_time,gps_time_out,gps_time-gps_time_out);

 JulianDate_out = date2j(year,month,day);

 fprintf(stderr," date2j %d\n",JulianDate_out);

 Doy_out = dayoyr(year,month,day);

 fprintf(stderr," dayoyr %d\n",Doy_out);

 tdt_out      =  gps2tdt(gps_time);

 tdt = tdt_out;

 fprintf(stderr," gps2tdt %f %f\n",tdt_out);


 gpslpsec(utc_time, &leap_sec, &Tbefore, leap_table);

 fprintf(stderr," gpslpsec %d %f\n",leap_sec,Tbefore);

 gps_time_out = gpsmin2sec(gps_minutes,gps_millisecs);

 fprintf(stderr," gpsmin2sec %f %f %f\n",gps_time,gps_time_out,gps_time-gps_time_out);

 gps_time_out = gpsws2sec(gps_week,gps_week_sec);

 fprintf(stderr," gpsws2sec %f %f %f\n",gps_time,gps_time_out,gps_time-gps_time_out);

 j2date(JulianDate,&year_out,&month_out,&day_out);

 fprintf(stderr," j2date %d %d %d\n",year_out,month_out,day_out);

 jd2cal(JulianDateReal,&year_out,&month_out,&day_out,&hour_out,
        &minute_out,&second_out,&frac_out);

 fprintf(stderr," jd2cal %d %d %d %d %d %d %d %f\n",year,month,day,hour,
                  minute,second,frac_second);
 fprintf(stderr," jd2cal %d %d %d %d %d %d %d %f\n",year_out,month_out,
                  day_out,hour_out,minute_out,second_out,frac_out);

 JulianDateReal_out = sec2jd(gps_time);
 JulianDateReal     = JulianDateReal;
 fprintf(stderr," sec2jd %f\n",JulianDateReal_out);

 gps_time_out = jd2sec(JulianDateReal);

 fprintf(stderr," jd2sec %f %f %f\n",gps_time,gps_time_out,gps_time-gps_time_out);

 sec2ch(gps_time,option_code,cal_str_out);

 fprintf(stderr," sec2ch %s %s\n",cal_str,cal_str_out);

 sec2gpsmin(gps_time,&gps_minutes_out,&gps_millisecs_out);

 fprintf(stderr," sec2gpsmin %d %d\n",gps_minutes_out,gps_millisecs_out);

 sec2gpsws(gps_time, &gps_week_out,&gps_week_sec_out);

 fprintf(stderr," sec2gpsmin %d %f\n",gps_week_out,gps_week_sec_out);


 tai_out = taiutc(utc_time,leap_table);

 fprintf(stderr," taiutc %f\n",tai_out);

 et = tdt2et(tdt);

 fprintf(stderr," et %f\n",et);

 gps_time_out = tdt2gps (tdt);

 fprintf(stderr," tdt2gps %f %f %f\n",gps_time,gps_time_out,gps_time-gps_time_out);

 utc_time_out = tdt2utc (tdt, leap_table);

 fprintf(stderr," tdt2utc %f %f %f\n",utc_time,utc_time_out,utc_time-utc_time_out);


 tdt_out      = utc2tdt(utc_time,leap_table);

 fprintf(stderr," tdt %f %f %f\n",tdt,tdt_out,tdt-tdt_out);

-----------------------------------------------------------------------*/

 exit(0);
}
