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
c     xrcv          real*8        receiver parameter
c
c   OUTPUT
c
c     xrcv1          real*8        receiver parameter after adding
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
c
      if(MARKER_TYPE.EQ.'SPACEBORNE')then
c
c        receiver phase center offset correction     
c
         call  corr_rcv_pco(Time,xrcv)
c     
      else
c
c        receiver phase center correction     
c
c        call  corr_rcv_pco()
c     
c        solid earth tides
c
c        call  solid_earth_tides()
c     
c        ocean loading
c
c        call  ocean_loading()
c
c        polar tides
c
c        call  polar_tides()
c
         write(*,*) 'SmartPPP/corr_rcv'
         write(*,*) 'please modified'
         stop
c
      endif
c
      return
c
      end
