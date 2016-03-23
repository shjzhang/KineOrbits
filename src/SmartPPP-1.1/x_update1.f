c
c  subroutine x_update1
c
      subroutine x_update1(ndim,x0,dx0)
c
c=======================================================================
c     ****f* SmartPPP/x_update1
c
c   FUNCTION   
c   
c     update the parameter array   
c
c   INPUTS
c
c      x0        real*8         parameter
c     dx0        real*8         parameter
c     ndim       integer        observable number
c
c   OUTPUT
c
c      x0        real*8         parameter
c     dx0        real*8         parameter
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: x_update1.f,v 1.0 2009/07/27 $
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
c     input/output
c
      integer       ndim
c
      real*8         x0(ndim)
      real*8        dx0(ndim)
c
c     local
c
      integer       i,j,k
c
c
      if(MARKER_TYPE.EQ.'SPACEBORNE')then
         do i=1,4
            x0(i) = x0(i) + dx0(i)
         enddo
      elseif(MARKER_TYPE.EQ.'GEODETIC')then
c        including trop parameters      
         do i=1,5
            x0(i) = x0(i) + dx0(i)
         enddo
      endif
c
      return
c
      end
