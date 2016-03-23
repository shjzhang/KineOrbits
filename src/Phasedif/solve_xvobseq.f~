c
c  subroutine solve_xvobseq
c
      subroutine solve_xvobseq(nobs,ndim,A0,P0,L0,dxv0,sig,Qxvxv)
c
c=======================================================================
c     ****f* SmartPPP/solve_xvobseq.f
c
c   FUNCTION   
c   
c     compose observation equation for PPOS
c
c   INPUTS
c
c     A0          real*8           coefficent
c     P0          real*8           weight
c     L0          real*8           OMC
c
c   OUTPUT
c
c     dxv0        real*8           parameter adjustment
c
c   REVISION
c
c     2009/08/21                   programmed
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c     ***
c
C     $Id: solve_xvobseq.f.f,v 1.0 2009/08/21 $
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
      integer       nobs, ndim
c
      real*8        A0 (MAX_SAT_NUM,6)
      real*8        P0 (MAX_SAT_NUM,MAX_SAT_NUM)
      real*8        L0 (MAX_SAT_NUM,1)
      real*8        Qxvxv(5,5)
c
      real*8        dxv0(6)
c
      real*8        A  (nobs,ndim)
      real*8        P  (nobs,nobs)
      real*8        L  (nobs,1)
c
c     local
c
      real*8        AT  (ndim,nobs)
      real*8        ATP (ndim,nobs)
      real*8        ATPA(ndim,ndim)
      real*8        ATPL(ndim,1)
c
      real*8        V   (nobs,1)
      real*8        VT  (1,nobs)
      real*8        VTP (1,nobs)
      real*8        VTPV
      real*8        sig
c
      real*8        W   (ndim,ndim)
c
      real*8        dxv1 (ndim,1)
c
      integer       i,j,k
c
      do i=1,nobs
      do k=1,ndim
         A(i,k) = A0(i,k)
      enddo
      enddo
c
      do i=1,nobs
      do k=1,nobs
         P(i,k) = P0(i,k)
      enddo
      enddo
c
      do i=1,nobs
         L(i,1) = L0(i,1)
      enddo
c
      do i=1,ndim
      do j=1,nobs
         AT (i,j) = 0.0d0
         ATP(i,j) = 0.0d0
      enddo
      enddo
c
      do i=1,ndim
      do j=1,ndim
         ATPA(i,j) = 0.0d0
      enddo
      enddo
c
      do i=1,ndim
         ATPL(i,1) = 0.0d0
         dxv1 (i,1) = 0.0d0
      enddo
c
      do i=1, 5
      do k=1, 5
         Qxvxv(i,k) = 0.0d0
      enddo
      enddo
c
C     write(*,*) 'A'
C     do i=1,nobs
C        write(*,*) (A(i,k),k=1,ndim)
C     enddo
C
C     write(*,*) 'L'
C     do i=1,nobs
C        write(*,*) L(i,1)
C     enddo
C
C     write(*,*) 'P'
C     do i=1,nobs
C        write(*,*) (P(i,k),k=1,nobs)
C     enddo
c
c     AT       
c
      call mtxtrs(A,AT,nobs,ndim)
c
c     ATP       
      call mtxmul(AT,P,ATP,ndim,nobs,nobs)
c     ATPA
      call mtxmul(ATP,A,ATPA,ndim,nobs,ndim)
c     ATPL
      call mtxmul(ATP,L,ATPL,ndim,nobs,1)
c     inv(ATPA)
      call mtxinv(ATPA,ndim)
c     dxv1
      call mtxmul(ATPA,ATPL,dxv1,ndim,ndim,1)
c
      do i=1, ndim
         dxv0(i) = dxv1(i,1)
      enddo
c
c     sigma
c     *****
c
      do i=1, nobs
         V(i,1) = L(i,1)
      enddo
c
      call mtxtrs(V,VT,nobs,1)
c
      call mtxmul(VT,P,VTP,1,nobs,nobs)
c
      call mtxmul(VTP,V,VTPV,1,nobs,1)
c
      sig = dsqrt(VTPV/(nobs-ndim))
c
c     Qxvxv
c     ***
c
      do i=1, ndim
      do k=1, ndim
         Qxvxv(i,k) = ATPA(i,k)
      enddo
      enddo
c
      return
c
      end
