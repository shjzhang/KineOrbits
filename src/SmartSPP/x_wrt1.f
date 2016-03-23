c
c  subroutine x_wrt1
c
      subroutine x_wrt1(nobs,time,iPRN,res,ndim,x0,sig,Qxx)
c
c=======================================================================
c     ****f* SmartPPP/x_wrt1
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
C     $Id: x_wrt1.f,v 1.0 2009/07/27 $
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
      integer       nobs
      real*8        time
      integer       iPRN(MAX_SAT_NUM)
      real*8        res(MAX_SAT_NUM,1)
c
      integer       ndim
      real*8        x0(ndim)
      real*8        sig
      real*8        Qxx(5,5)
c
c     local
c
      real*8        sigx,sigy,sigz,sigt
      real*8        GDOP
c
      integer       i,j,k
c
c     position
c     ********
c
      write(201,'(F14.3,4F14.3)') time,(x0(i),i=1,4)
      write( * ,'(F14.3,4F14.3)') time,(x0(i),i=1,4)
c
c     infomation of residual 
c     **********************
c
      write(301,'(F14.3,I6)') time,nobs
c
      do i=1, nobs
c
      write(301,'(I3,F8.3)')  iPRN(i), res(i,1)
c
      enddo
c
c     statis
c     ******
c
      sigx    = sig*dsqrt( Qxx(1,1) )
      sigy    = sig*dsqrt( Qxx(2,2) ) 
      sigz    = sig*dsqrt( Qxx(3,3) )
      sigt    = sig*dsqrt( Qxx(4,4) )
c
      GDOP  = dsqrt( Qxx(1,1)+Qxx(2,2)+Qxx(3,3)+Qxx(4,4) )
c
      write(303,'(F14.3,6F8.3)') time,sig,sigx,sigy,sigz,sigt,GDOP
c
      return
c
      end
