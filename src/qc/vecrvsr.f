c
c   subroutine vecrvsr
c
      subroutine vecrvsr(A, m, n, B)
c
c=======================================================================
c     ****f* qualicontr/vecrvsr
c
c   FUNCTION   
c
c     reverse part of the real vector A, 
c     store the partly reversed array into array B
c
c   Notes
c   
c     only part of A, i.e.dimension 1 to n, to be reversed, so n must be
c     equal or less than m 
c
c   INPUTS
c
c     A            (R)          input array
c     m            (I)          dimension of A
c     n            (I)          dimension to be inversed( n.le.m)
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
C     $Id: vecrvsr.f,v 1.0 2009/07/11 $
c=======================================================================
c
      implicit none
c
c     input/output variables
c
      integer   m
      integer   n
c
      real*8    A(m)
      real*8    B(m)
c
c     local variables
c
      integer   i, j, k
      real*8    C(n), D(n)
c
c     initial
c
      do i=1, m
         B(i) = A(i)
      enddo
c
      do i=1, n
         C(i) = 0.0d0
         D(i) = 0.0d0
      enddo
c
c     copy part of A to be reversed
c
      do i=1, n
         C(i) = A(i)
      enddo
c
c     reversed c and stored into d
c
      do i=1, n
         D(i) = c(n-i+1)
      enddo
c
c     copy d to B
c
      do i=1, n
         B(i) = D(i)
      enddo
c
      return
c
      end
