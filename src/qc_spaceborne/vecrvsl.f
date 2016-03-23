c
c   subroutine vecrvsl
c
      subroutine vecrvsl(A, m, n, B)
c
c=======================================================================
c     ****f* qualicontr/vecrvsl
c
c   FUNCTION   
c
c     reverse part of the logical vector A, 
c     store the partly reversed array into array B
c
c   Notes
c   
c     only part of A, i.e.dimension 1 to n, to be reversed, so n must be
c     equal or less than m 
c
c   INPUTS
c
c     A            (L)          input array
c     m            (I)          dimension of A
c     n            (I)          dimension to be inversed( n.le.m)
c
c   OUTPUT
c
c     B            (L)          array to store the partly reversed
c                               array
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: vecrvsl.f,v 1.0 2009/07/11 $
c=======================================================================
c
c
      implicit none
c
c     input/output variables
c
      integer   m
      integer   n
c
      logical   A(m)
      logical   B(m)
c
c     local variables
c
      integer   i, j, k
      logical   C(n), D(n)
c
c     initial
c
      do i=1, m
         B(i) = A(i)
      enddo
c
      do i=1, n
         C(i) = .false.
         D(i) = .false.
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
         D(i) = C(n-i+1)
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
