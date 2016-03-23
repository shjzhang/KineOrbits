c
c  subroutine x_ini
c
      subroutine x_ini(ndim,x0,dx0)
c
c=======================================================================
c     ****f* SmartPPP/x_ini
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
C     $Id: x_ini.f,v 1.0 2009/08/10 $
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
      real*8         x0(MAX_PMS)
      real*8        dx0(MAX_PMS)
c
c     local
c
      integer       i,j,k
c
      real*8        t(MAX_EPO)
      real*8        x(MAX_EPO)
      real*8        y(MAX_EPO)
      real*8        z(MAX_EPO)
c
      real*8        t1,x1,y1,z1
c
c     common
c
      real*8        POS(MAX_EPO,4)
      integer       nrec_pos
c
      common /ini/  POS,NREC_POS
c
      integer       NSAT, NEPO
      integer       NAMB, NREC
      integer       iSAT_iPRN(MAX_PRN), iPRN_iSAT(MAX_PRN)
      integer       NAMB_iPRN(MAX_PRN), NAMB_iSAT(MAX_PRN)
      integer       NREC_iPRN(MAX_PRN), NREC_iSAT(MAX_PRN)
c
      character*3   cPRN_iSAT(MAX_PRN)
c
      real*8        TIME_SPAN(2)
      real*8        aEPO(MAX_EPO)
c
      common /obs/  NSAT,      NEPO,     
     &              NAMB,      NREC, 
     &              iPRN_iSAT, iSAT_iPRN,
     &              NREC_iPRN, NREC_iSAT,
     &              NAMB_iPRN, NAMB_iSAT, 
     &              cPRN_iSAT, TIME_SPAN,
     &              aEPO
c
      do i=1, ndim
         x0(i) = 0.0d0
      enddo
c
      do i=1, NREC_POS
         t(i) = POS(i,1)
         x(i) = POS(i,2)
         y(i) = POS(i,3)
         z(i) = POS(i,4)
      enddo
c
      do i=1, nepo
c
         t1 = aEPO(i)
c
         call lagrange(t,x,NREC_POS,t1,x1)
         call lagrange(t,y,NREC_POS,t1,y1)
         call lagrange(t,z,NREC_POS,t1,z1)
c
         x0((i-1)*4+1) = x1 
         x0((i-1)*4+2) = y1
         x0((i-1)*4+3) = z1
         x0((i-1)*4+4) = 0.0d0

c
      enddo
c
      return
c
      end
