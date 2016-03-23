ctitle systime
 
      subroutine systime( date, secs)
 
*     This routine will return the current system date and seconds.
*     The date is a five element array with year, month, day, hour, minute.
*
*   Note: time is returned to nearest second at the moment.
 
* Variables
 
*   date(5)     - Year, month, day, hour, min, sec
 
      integer*4 date(5)
 
*   secs        - Seconds part of the time.
 
      real*8 secs
 
* Functions used:
*   IDATE       ! Returns date as day, month, year
*   ITIME       ! Returns time in hr, min, seconds
 
* LOCAL VARIABLES
 
*   jdate(3)    - Return from idate
*   jtime(3)    - Return from itime.
 
      integer*4 jdate(3), jtime(3)

*      external idate
 
****  Get the date
      call idate(jdate)
      date(1) = jdate(3)
      date(2) = jdate(2)
      date(3) = jdate(1)

cifc If you are using the intel fortran compiler prior to version 8 use the idate calls below      
cifc      call idate(jdate(1),jdate(2),jdate(3))
cifc      date(1) = jdate(3)
cifc      date(2) = jdate(1)
cifc      date(3) = jdate(2)
 
****  Get the hours, min and seconds
      call itime(jtime)
      date(4) = jtime(1)
      date(5) = jtime(2)
 
      secs = jtime(3)
 
****  Thats all
      return
      end
 
