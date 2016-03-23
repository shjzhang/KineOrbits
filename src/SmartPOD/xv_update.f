c
c  subroutine xv_update
c
      subroutine xv_update(ndim,xv0,dxv0)
c
c=======================================================================
c     ****f* SmartPPP/xv_update
c
c   FUNCTION   
c   
c     update the parameter array   
c
c   INPUTS
c
c      xv0       real*8         parameter
c     dxv0       real*8         parameter
c     ndim       integer        observable number
c
c   OUTPUT
c
c      xv0       real*8         parameter
c     dxv0       real*8         parameter
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: xv_update.f,v 1.0 2009/08/23 $
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
      real*8         xv0(ndim)
      real*8        dxv0(ndim)
c
c     local
c
      integer       i,j,k
c
c
      if(MARKER_TYPE.EQ.'SPACEBORNE')then
c
      do i=1,4
         xv0(i) = xv0(i) + dxv0(i)
      enddo
c
      endif
c
      return
c
      end
