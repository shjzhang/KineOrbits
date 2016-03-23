c
c   subroutine diff
c
      subroutine diff(A, M, N, B)
c
c=======================================================================
c     ****f* qualicontr/diff
c
c   FUNCTION   
c
c     caculate the N-th order difference for input array A. 
c
c   Notes
c   
c     the dimension of A must be less than the difference order N   
c
c   INPUTS
c
c     A            (R)          input array
c     m            (I)          dimension of A
c     n            (I)          difference order
c
c   OUTPUT
c
c     B            (R)          array to store the partly reversed
c                               array
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: diff.f,v 1.0 2009/07/11 $
c=======================================================================
c
      implicit none
c
c     input/output variables
c
      integer   M
      integer   N
c
      real*8    A(M)
      real*8    B(M)
c
c     local
c
      integer   i, j, k
      real*8    A_fore(M)
      real*8    A_this(M)
c
      do i=1, M
         B(i) = 0.0d0
      enddo
c
c     Initial
c
      do i=1,M
         A_fore(i) = A(i)
      enddo
c
      do i=1,N
c
         do j=1, i
            A_this(j) = 0
         enddo
c
         do j=i+1, M
            A_this(j) = A_fore(j)-A_fore(j-1)
         enddo
c
         do j=1, M
            A_fore(j) = A_this(j)
         enddo
c
      enddo
c
      do j=1,M
         B(j) = A_this(j)
      enddo
c
      return
c
      end

