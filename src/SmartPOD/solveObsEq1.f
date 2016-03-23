c
c  subroutine solveObsEq1
c
      subroutine solveObsEq1(nobs,ndim,A0,P0,L0,dx0,sig,Qxx)
c
c=======================================================================
c     ****f* SmartPPP/solveObsEq1.f
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
c     dx0         real*8           parameter adjustment
c
c   REVISION
c
c     2009/08/05                   programmed
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c     ***
c
C     $Id: solveObsEq1.f.f,v 1.0 2009/07/28 $
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
      real*8        Qxx(6,6)
c
      real*8        dx0(6)
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
      real*8        dx1 (ndim,1)
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
         dx1 (i,1) = 0.0d0
      enddo
c
      do i=1, 6
      do k=1, 6
         Qxx(i,k) = 0.0d0
      enddo
      enddo
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
c     dx1
      call mtxmul(ATPA,ATPL,dx1,ndim,ndim,1)
c
      do i=1, ndim
         dx0(i) = dx1(i,1)
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
c     Qxx
c     ***
c
      do i=1, ndim
      do k=1, ndim
         Qxx(i,k) = ATPA(i,k)
      enddo
      enddo
c
      return
c
      end
