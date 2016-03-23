*
*  subroutine earth_rotation
*
      subroutine earth_rotation(transmit_time, gps_pos)
c
c=======================================================================
c     ****f* SmartPPP/earth_rotation.f
c
c   FUNCTION   
c   
*     calculate the earth rotation effects.
c
c   INPUTS
c
*     transmit_time   real*8       signal transmitting time from GPS to LEO
*     gps_pos(3)      real*8       gps postion after the earth rotation
c
c   OUTPUT
c
*     gps_pos(3)      real*8       gps postion after the earth rotation
c                                  corrections
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c   REVISION
c
c     2007/09/16                   programmed
c     2009/08/03                   modifiy the header
c
c     ***
c
C     $Id: earth_rotation.f.f,v 1.0 2009/07/28 $
c=======================================================================
* 
      implicit none 
c
c     earth rotation omega [rad/s]   Aoki 1982, NIMA 199
c
      real*8    omega_Earth
      parameter(omega_Earth = 7.2921151467D-5 )
*
*     input/output 
*
      real*8    transmit_time
      real*8    gps_pos(3)
*
*     local
*
      real*8    alfa
      real*8    gps_pos_new(3)
*
*     earth rotation omega during the signal transmitting time
*
      alfa = omega_Earth*transmit_time
*
*     ITRF rotation owing the earth rotation
*
      gps_pos_new(1) =  gps_pos(1)*dcos(alfa) + gps_pos(2)*dsin(alfa)
      gps_pos_new(2) = -gps_pos(1)*dsin(alfa) + gps_pos(2)*dcos(alfa)
      gps_pos_new(3) =  gps_pos(3)  
c    
c     store the result   
c
      gps_pos(1) = gps_pos_new(1)
      gps_pos(2) = gps_pos_new(2)
      gps_pos(3) = gps_pos_new(3)
c
      return
c
      end
