c
c  subroutine x_convgd1
c
      subroutine x_convgd1(ndim,dx0,convgd)
c
c=======================================================================
c     ****f* SmartPPP/x_convgd1
c
c   FUNCTION   
c   
c     determine the parameter dimension for Point Positioning   
c
c   INPUTS
c
c     dx0        real*8         parameter increment
c     ndim       integer        observable number
c
c   OUTPUT
c
c     convgd     logical        converged
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: x_convgd1.f,v 1.0 2009/07/27 $
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
      real*8        dx0(6)
      integer       ndim
      logical       convgd
c
c     local
c
      integer       i,j,k
      real*8        dx_max
c
      convgd = .false.
c
      dx_max = 0.0d0
c
c     position
c
      do i=1, 4
         if(dabs(dx0(i)).gt.dx_max)then
            dx_max = dabs(dx0(i))
         endif
      enddo
c
      if(dx_max.lt.1.0D-3)then
         convgd = .true.
      endif
c
      return
c
      end
