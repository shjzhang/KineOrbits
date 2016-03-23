*
*  Procedure diff_tai2utc
*
      DOUBLE PRECISION FUNCTION diff_tai2utc(UTC)
*********1*********2*********3*********4*********5*********6*********7**
*      
*                           Purpose
*
*  Subroutine to return the difference TAI-UTC in seconds for the 
*  specified utc time, which is in seconds past J2000
*
*                           Input_Output Auguments
*
*  Name      Type    I/O    Descriptin
*  ----      ----    ---    --------------------------------------------
*  UTC       REAL*8  I      UTC time givn in seconds past J2000
*  diff_leap REAL*8  O      difference of TAI - UTC at given UTC time
*
*                           Notes
*
*  The leap seconds can be attracted from the file
*  tai-utc.dat
*  which can be downloaded form the ftp site:
*    ftp://oceans.gsfc.nasa.gov/COMMON/leapsec.dat
*
*  File Exapmle:
*    1980 JAN  1 =JD 2444239.5  TAI-UTC=  19.0000000 S
*    1981 JUL  1 =JD 2444786.5  TAI-UTC=  20.0000000 S
*    1982 JUL  1 =JD 2445151.5  TAI-UTC=  21.0000000 S
*    1983 JUL  1 =JD 2445516.5  TAI-UTC=  22.0000000 S
*    1985 JUL  1 =JD 2446247.5  TAI-UTC=  23.0000000 S
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
      PARAMETER(    MAX_LEAP = 20)
C
C     Declaration_of_Input_Output_Variables
C
      REAL*8        UTC
      REAL*8        diff_leap
C      
C     Declaration_of_Local_Variables
C
      INTEGER       nleap
      INTEGER       i, j
      REAL*8        leap(MAX_LEAP, 2)
c
c     load leap from leapsec 
c
      do i=1, max_leap
        leap(i,1) = 0.0d0
        leap(i,2) = 0.0d0
      enddo
c
      call leapsec(nleap, leap)
c
c     Judge the difference of TAI-UTC
c
      do i=1, nleap
        if(leap(i,1).le.utc)then
          diff_leap = leap(i,2)
        endif
      enddo
c
c     RETURN
c
      diff_TAI2UTC = diff_leap
c
      return
c
      end
