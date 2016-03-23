*
*  procedure     GPS2UTC
*
      double precision Function GPS2UTC(GPS)
*********1*********2*********3*********4*********5*********6*********7**
*      
*                           Purpose
*
*  Return the UTC time in secons past J2000 given gps time. The time is
*  given in gps seconds past J2000.
*
*                           Input_Output Auguments
*
*  Name     Type    I/O     Descriptin
*  ----     ----    ---     --------------------------------------------
*  GPS      REAL*8  I       gps time given in seconds past J2000
*  UTC      REAL*8  O       utc time given in seconds past J2000 
*      
*  History:
*
*  Vesion 1.0
*    
*  Time         Author      Description
*  ----         ------      --------------------------------------------
*  07.06.01     S.J.Zhang   build this subroutine    
*
*********1*********2*********3*********4*********5*********6*********7**
c
c     Declaration_of_Varialbes
c
      IMPLICIT NONE
C
C     Declaration_of_Constants
C
      INTEGER       MAX_LEAP
      PARAMETER(    MAX_LEAP = 20 )
c
c     Declaration_of_Input_Output_Varialbles
c
      REAL*8        GPS, UTC
C
C     Declaration_of_External_Functions
C
      REAL*8        JD2SEC
      REAL*8        gpslpsec
C
C     Declaration_of_Local_Variables
C
      INTEGER       NLEAP
      INTEGER       I, J
      REAL*8        LEAP(MAX_LEAP,2)
      REAL*8        UTC_t1
      REAL*8        UTC_leapsec
c
c     Call gpslpsec twice to avoid being off by one second when 
c     the utc time is less than gpslpsec before a leap second 
c
      call leapsec(nleap, leap)
c
c     Get the leap second time before the GPS time
c
      do i=1, nleap
        if(GPS.ge.leap(i,1))then
          UTC_leapsec = leap(i,1)
        endif 
      enddo
c
      UTC_T1 = GPS - gpslpsec(GPS)
c
c     Without the following fix, the following would happen: 
******
c     1-JUL-1993 00:00:10 GPS TIME --->  1-JUL-1993 00:00:01 UTC TIME
c     1-JUL-1993 00:00:09 GPS TIME --->  1-JUL-1993 00:00:01 UTC TIME
c     1-JUL-1993 00:00:08 GPS TIME --->  1-JUL-1993 00:00:00 UTC TIME
c     1-JUL-1993 00:00:07 GPS TIME ---> 30-JUN-1993 23:59:59 UTC TIME
******
c     The fix allows the following conversion: 
******
c     1-JUL-1993 00:00:10 GPS TIME --->  1-JUL-1993 00:00:01 UTC TIME
c     1-JUL-1993 00:00:09 GPS TIME --->  1-JUL-1993 00:00:00 UTC TIME
c     1-JUL-1993 00:00:08 GPS TIME --->  1-JUL-1993 00:00:00 UTC TIME
c     1-JUL-1993 00:00:07 GPS TIME ---> 30-JUN-1993 23:59:59 UTC TIME 
c
      UTC = GPS - gpslpsec(UTC_T1)
c
      GPS2UTC = UTC
c
      return
c
      end

