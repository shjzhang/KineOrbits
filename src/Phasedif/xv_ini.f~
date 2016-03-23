c
c  subroutine xv_ini
c
      subroutine xv_ini(ndim,time0,time1,xv0,dxv0)
c
c=======================================================================
c     ****f* SmartPPP/xv_ini
c
c   FUNCTION   
c   
c     initial the parameter array   
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
C     $Id: xv_ini.f,v 1.0 2009/08/23 $
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
      real*8        time0, time1
c
      real*8         xv0(ndim)
      real*8        dxv0(ndim)
c
c     local
c
      integer       i,j,k
c
      real*8        x0,y0,z0
      real*8        x1,y1,z1
c
      real*8        t(MAX_EPO)
      real*8        x(MAX_EPO)
      real*8        y(MAX_EPO)
      real*8        z(MAX_EPO)
c
c     common
c
      real*8        POS(MAX_EPO,4)
      integer       nrec_pos
c
      common /ini/  pos, NREC_POS
c
      do i=1, NREC_POS
         t(i) = POS(i,1)
         x(i) = POS(i,2)
         y(i) = POS(i,3)
         z(i) = POS(i,4)
      enddo
c
      call lagrange(t,x,NREC_POS,time0,x0)
      call lagrange(t,y,NREC_POS,time0,y0)
      call lagrange(t,z,NREC_POS,time0,z0)
c
      call lagrange(t,x,NREC_POS,time1,x1)
      call lagrange(t,y,NREC_POS,time1,y1)
      call lagrange(t,z,NREC_POS,time1,z1)
c
      xv0(1) = x1 - x0
      xv0(2) = y1 - y0
      xv0(3) = z1 - z0
      xv0(4) = 0.0d0
c
      return
c
      end
