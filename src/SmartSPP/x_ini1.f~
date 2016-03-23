c
c  subroutine x_ini1
c
      subroutine x_ini1(ndim,x1,x0,dx0)
c
c=======================================================================
c     ****f* SmartPPP/x_ini1
c
c   FUNCTION   
c   
c     initial the parameter array   
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
C     $Id: x_ini1.f,v 1.0 2009/07/27 $
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
      real*8        x1(*)
c
c     local
c
      integer       i,j,k
c
c     radius of the receiver 
c
      do i=1,3
       x0(i) = x1(i)
      enddo
c
      return
c
      end
