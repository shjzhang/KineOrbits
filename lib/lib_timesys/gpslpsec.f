*
*  procedure gpslpsec
*
      double precision function gpslpsec(UTC)
********1*********2*********3*********4*********5*********6*********7**
*
*                       Purpose
*
*  Subroutine to determine the number of GPS leap seconds 
*  given UTC time. The UTC time is given in seconds past J2000
*  
*                       Input_Arguments
*
*  Name     Type        I/O     Description
*  ----     ----        ---     ------------------------------
*  UTC      Real*8      I       UTC time in seconds past J2000
*                               
*                       Notes
*
*  The leap seconds can be attracted from the file
*  tai-utc.dat
*  The ftp site are:
*    ftp://oceans.gsfc.nasa.gov/COMMON/leapsec.dat
*
*                       File Format
*
*  1980 JAN  1 =JD 2444239.5  TAI-UTC=  19.0s
*  2006 JAN  1 =JD 2453736.5  TAI-UTC=  33.0s
*
*  Author: S.J.Zhang
*
********1*********2*********3*********4*********5*********6*********7**
c
c     Declarations_of_Variables
c 
      IMPLICIT NONE
c
c     Declarations_of_External_Function
c
      DOUBLE PRECISION diff_tai2utc
c
c     Declarations_of_Input_and_Output_Arguments
c
      real*8           UTC
c
c     Declarations_of_Local_Variables
c
      real*8           leapsec
c
c     UTC time have equal values with GPS time at 1980.01.06.00.00.00
c     TAI and GPS time's relation are :
c       TAI - GPS = 19              
c     UTC and GPS time's relation are :
c       GPS - UTC = diff - 19
c     TAI and UTC time's relatino are :
c       TAI - UTC = diff
c   
      leapsec = diff_tai2utc(UTC) - 19.0d0
c
c     Result
c
      gpslpsec = leapsec
c
      return
c
      end
