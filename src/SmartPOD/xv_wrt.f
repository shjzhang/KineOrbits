c
c  subroutine xv_wrt
c
      subroutine xv_wrt(nobs,aPRN,time0,time1,v,ndim,xv0,sig,Qxvxv)
c
c=======================================================================
c     ****f* SmartPPP/xv_wrt
c
c   FUNCTION   
c   
c     initial the parameter array   
c
c   INPUTS
c
c      xv0        real*8         parameter
c     dxv0        real*8         parameter
c     ndim       integer        observable number
c
c   OUTPUT
c
c      xv0        real*8         parameter
c     dxv0        real*8         parameter
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: xv_wrt.f,v 1.0 2009/08/21 $
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
      real*8        time0,time1
      integer       aPRN(MAX_SAT_NUM)
      real*8        v(MAX_SAT_NUM,1)
c
      integer       ndim
      real*8        xv0(ndim)
      real*8        sig
      real*8        Qxvxv(5,5)
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
      write(201,'(2F14.3,4F14.3)') time0,time1,(xv0(i),i=1,4)
c
c     infomation of residual 
c     **********************
c
      write(301,'(2F14.3,I6    )') time0,time1,nobs
c
      do i=1, nobs
c
      write(301,'(I3,F8.3      )') aPRN(i), v(i,1)
c
      enddo
c
c     statis
c     ******
c
      sigx    = sig*dsqrt( Qxvxv(1,1) )
      sigy    = sig*dsqrt( Qxvxv(2,2) ) 
      sigz    = sig*dsqrt( Qxvxv(3,3) )
      sigt    = sig*dsqrt( Qxvxv(4,4) )
c
      GDOP  = dsqrt( Qxvxv(1,1)+Qxvxv(2,2)+Qxvxv(3,3)+Qxvxv(4,4) )
c
      write(303,'(2F14.3,6F8.3 )') time0,time1,
     &                             sig,sigx,sigy,sigz,sigt,GDOP
c
      return
c
      end
