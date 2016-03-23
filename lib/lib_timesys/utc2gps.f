*
*  procedure     UTC2GPS
*
      DOUBLE PRECISION FUNCTION UTC2GPS(UTC)
*********1*********2*********3*********4*********5*********6*********7**
*      
*                           Purpose
*
*  Return the GPS time in secons past J2000 given UTC time. The UTC time is
*  given in seconds past J2000.
*
*                           Input_Output Auguments
*
*  Name     Type    I/O     Descriptin
*  ----     ----    ---     --------------------------------------------
*  UTC      REAL*8  I       utc time given in seconds past J2000 
*  GPS      REAL*8  O       gps time given in seconds past J2000
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
      REAL*8        UTC, GPS
C
C     Declaration_of_External_Functions
C
      REAL*8        TAIUTC
c
c     return
c
      GPS = UTC + TAIUTC(UTC) - 19.0d0
c
c     FUNCTION VALUE
c
      UTC2GPS = GPS
c
      return
c
      end

