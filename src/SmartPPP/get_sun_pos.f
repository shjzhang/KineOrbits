*
*  subroutine get_sun_pos
*      
      subroutine get_sun_pos(time, xsat)
c
c=======================================================================
c     ****f* SmartPPP/get_sun_pos
c
c   FUNCTION   
c   
c     read IGS antex file, which is used to correct:
c     1)  the receiver's PCO and PCV
c     2)  the GNSS's PCO and PCV
c
c   INPUTS
c
c     NONE
c
c   OUTPUT
c
c     NONE
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: get_sun_pos.f,v 1.0 2009/07/31 $
c=======================================================================
*         
      implicit none
c
      include       '../../include/igs.h'
      include       '../../include/atx.h'
      include       '../../include/rinex.h'
      include       '../../include/rinex.conf.h'
      include       '../../include/SmartPPP.h'
      include       '../../include/SmartPPP.conf.h'
c     function
      real*8        sec2jd
c     input/output
      real*8        time
      real*8        xsat(3)
c
      real*8        time_jd
      real*8        ntarg, ncent, sun_state_icrs(6), sun_state_itrs(6)
c
      time_JD = sec2jd(time)
c
c     sun's state vector in ICRS
c
      ntarg = 11
      ncent = 3
c
      call planet_state(time_JD,ntarg,ncent,sun_state_icrs)
c
c     sun's state vector in ITRS
c
      call ICRS2ITRS(time,sun_state_icrs,sun_state_itrs)
c
      xsat(1) = sun_state_itrs(1)
      xsat(2) = sun_state_itrs(2)
      xsat(3) = sun_state_itrs(3)
c
      end

