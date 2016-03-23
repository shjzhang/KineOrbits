#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "TimeLib.h" 


#define CFILNMAX   80
#define MAXCHAR  1000

static char SccsId[] = "@(#) leap.c       1.1 01/08/01";

table LoadLeapSeconds()

{
/*----------------------------------------------------------------------------->
/
/   purpose: load all leap seconds into table from file
/
/   output:   leap.x[]   time of leap second occurance [seconds past J2000 UTC]
/             leap.y[]   leap seconds offset
/
<-----------------------------------------------------------------------------*/

  FILE    *leap_file;

  double   sec_frac,seconds;

  char     filename[CFILNMAX],line[MAXCHAR];
  char     *ref_month[] = {"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG",
                           "SEP","OCT","NOV","DEC"};
  char     month[MAXCHAR],dummy[MAXCHAR];

  int      nleap;
  long     mon,day,year,hour,min;
  table    leap;

/*----------------------------------------------------------------------------->
/ Open leap second file
<-----------------------------------------------------------------------------*/

 strcpy(filename,"/time-pole/LEAPSECS");

 leap_file = fopen(filename, "rb");

 if (leap_file == NULL)
 {
  fprintf(stderr,"\n Leap Second file %s cannot be opened !! \n\n",filename);
  exit(1);
 }

/*----------------------------------------------------------------------------->
/ Load leap seconds into table
<-----------------------------------------------------------------------------*/

 nleap = 0;

 while (fgets(line,MAXCHAR,leap_file) != NULL) nleap++;

 rewind(leap_file);

 leap = table_create(nleap);

 nleap = 0;

 while (fgets(line,MAXCHAR,leap_file) != NULL) 
 {
  sscanf((line),"%2d%1s%3s%1s%4d%3d%1s%2d%1s%7lf",&day,&dummy,month,&dummy,
                    &year,&hour,&dummy,&min,&dummy,&seconds);

  mon      = 1;
  while ( strncmp(month,ref_month[mon-1],3) != 0) mon++;

  sec_frac = seconds - (double)( (int)seconds);

  leap.x[nleap] = calsec (year,mon,day,hour,min,(int) seconds,sec_frac);
  leap.y[nleap] = -atof((line+38));

  nleap++;
 }

 fclose(leap_file);
 return leap;

}
