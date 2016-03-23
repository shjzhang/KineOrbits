c
c   subroutine diff1_rcv
c
      subroutine diff1_rcv(A, M, B)
c
c=======================================================================
c     ****f* qualicontr/diff1_rcv
c
c   FUNCTION   
c
c     rebuild 1st order difference from 2nd order difference
c
c   INPUTS
c
c     A            (R)          2nd array
c     m            (I)          dimension of A
c
c   OUTPUT
c
c     B            (R)          1st order difference
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: diff1_rcv.f,v 1.0 2009/07/11 $
c=======================================================================
c
      implicit none
c
c     input/output variables
c
      integer   M
      real*8    A(M)
      real*8    B(M)
c
c     local 
c
      integer   i, j, k
c
      do i=1, M
         B(i) = 0.0d0
      enddo
c
      do i=3, M
c
         do j=3,i
            B(i) = B(i) + A(j)
         enddo
c
      enddo
c
      return
c
      end

