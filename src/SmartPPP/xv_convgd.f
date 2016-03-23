c
c  subroutine xv_convgd
c
      subroutine xv_convgd(ndim,dxv0,convgd)
c
c=======================================================================
c     ****f* SmartPPP/xv_convgd
c
c   FUNCTION   
c   
c     determine the parameter dimension for Point Positioning   
c
c   INPUTS
c
c     dxv0       real*8         parameter increment
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
C     $Id: xv_convgd.f,v 1.0 2009/08/21 $
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
      real*8        dxv0(6)
      integer       ndim
      logical       convgd
c
c     local
c
      integer       i,j,k
      real*8        dxv_max
c
      convgd = .false.
c
      dxv_max = 0.0d0
c
c     position
c
      do i=1, 4
         if(dabs(dxv0(i)).gt.dxv_max)then
            dxv_max = dabs(dxv0(i))
         endif
      enddo
c
      if(dxv_max.lt.1.0D-3)then
         convgd = .true.
      endif
c
      return
c
      end
