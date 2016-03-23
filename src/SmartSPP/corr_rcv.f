c
c  subroutine corr_rcv.f
c
      subroutine corr_rcv(Time,xrcv)
c
c=======================================================================
c     ****f* SmartPPP/corr_rcv.f
c
c   FUNCTION   
c
c     compute the recever's corrections: receiver phase center offset,
c     solid earth tides, ocean loading, polar tides, etc.
c
c   INPUTS
c
c     xrcv(3)        real*8        receiver parameter
c
c   OUTPUT
c
c     xrcv(3)        real*8        receiver parameter after adding
c                                  corrections
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c   REVISION
c
c     2009/7/28                    programmed
c
c     ***
c
C     $Id: corr_rcv.f.f,v 1.0 2009/07/28 $
c=======================================================================
c
      implicit none
c
c     include
c
      include      '../../include/rinex.h'
      include      '../../include/rinex.conf.h'
      include      '../../include/SmartPPP.h'
      include      '../../include/SmartPPP.conf.h'
c
c     input/output variable
c
c     input
      real*8        Time
      real*8        xrcv(3)
c
c     local
c
      real*8        x,y,z
      real*8        dx_stide(3)
      real*8        dx_otide(3)
      real*8        dx_ptide(3)
      integer       i
c
      if(MARKER_TYPE.EQ.'SPACEBORNE')then
c
c        receiver antenna offset
         call  corr_ant_off(Time,xrcv)
c        receiver antenna phase center offset correction
         call  corr_rcv_pco(Time,xrcv)
c     
      else
c
c        receiver antenna offset
         call  corr_ant_off(Time,xrcv)
c
c        receiver's antenna phase center offset
         call  corr_rcv_pco(Time,xrcv)
c     
c        solid earth tides
         call  solid_earth_tides(Time,xrcv,dx_stide)
c   
c        0rite(*,*) 'stide', (dx_stide(i),i=1,3)
         xrcv(1) = xrcv(1) + dx_stide(1)
         xrcv(2) = xrcv(2) + dx_stide(2)
         xrcv(3) = xrcv(3) + dx_stide(3)
c
c        ocean loading
         call  otl(Time,xrcv,dx_otide)
c
c        write(*,*) 'otide', (dx_otide(i),i=1,3)
         xrcv(1) = xrcv(1) + dx_otide(1)
         xrcv(2) = xrcv(2) + dx_otide(2)
         xrcv(3) = xrcv(3) + dx_otide(3)
c
c        polar tides
         call pole_tide(Time,xrcv,dx_ptide)
c
c        write(*,*) 'ptide', (dx_ptide(i),i=1,3)
         xrcv(1) = xrcv(1) + dx_ptide(1)
         xrcv(2) = xrcv(2) + dx_ptide(2)
         xrcv(3) = xrcv(3) + dx_ptide(3)
c
      endif
c
      return
c
      end
