c
c  subroutine chlinv
c
      subroutine chlinv(araw,N)
c*********************************************************************72
c
cc    SYMINV computes the inverse of a symmetric matrix.
c
c  Modified:
c
c     01 February 2008
c
c  Author:
c
c     Michael Healy
c     Modifications by John Burkardt
c
c  Reference:
c
c     Michael Healy,
c     Algorithm AS 7:
c     Inversion of a Positive Semi-Definite Symmetric Matrix,
c     Applied Statistics,
c     Volume 17, Number 2, 1968, pages 198-199.
c
c  Parameters:
c
cccccccccccccccccccccccccccccccccccccccccc
c
      implicit none
c
      integer   N
      real*8    araw(N,N),arawinv(N,N)
c
      real*8    avec   (( N*(N+1) )/2)
      real*8    avecinv(( N*(N+1) )/2)
c
      real*8    work(N)
      integer   nullty,ifault
      integer   i,j,k
c
      call mtxvec(N,araw,avec)
c
      call syminv(avec,N,avecinv,work,nullty,ifault)
c
      if(nullty.NE.0)then
         write(*,*) 'chlinv'
         write(*,*) 'nullty.NE.0'
         stop
      endif
      if(ifault.NE.0)then
         write(*,*) 'chlinv'
         write(*,*) 'ifault.NE.0'
         stop
      endif
c
      call vecmtx(N,avecinv,arawinv)
c
      do i=1,N
      do k=1,N
         araw(i,k) = arawinv(i,k)
      enddo
      enddo
c
      return
c
      end
