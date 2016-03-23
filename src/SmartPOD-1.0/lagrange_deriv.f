*
*  subroutine lagrange_deriv
*
      subroutine lagrange_deriv( xa, ya, n, x, z)
c
c=======================================================================
c     ****f* SmartPPP/lagrange_deriv
c
c   FUNCTION   
c   
*     lagrange function derivative interpolation 
c
c   INPUTS
c
*     n            integer      node number
*     xa           real*8       array store the node value
*     ya           real*8       array store the funtion value
*     x            real*8       interpolate independet value
c
c   OUTPUT
c
*      z           real*8       interpolated value
c
c   REVISION
c
c     2009/07/29                modified the header 
c     2009/08/10                binary search for the left neareast
c                               point of the interpolated x.
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: lagrange_deriv.f,v 1.0 2009/08/10 $
c=======================================================================
*
*     evironment variables
*
      implicit none
c
c     input/output
c
      integer   n
      real*8    xa(n), ya(n), x, z, dy
c
c     local
c
      integer   i, j, k,l,M
      real*8    a, b, c
      logical   find
c
c     interpolate the derivative value of the lagrange function
c
      find=.false.
c
      call binary_search(x,xa,N,M,find)
c
      if(M.LT.5) M=5
      if(M.GT.n-5) M=n-5
c
      z=0.0d0
      do i = M-4, M+5
        b = 1.0d0
        a = 0.0d0
        do j = M-4, M+5
          if(j.ne.i) then
            b = b*(xa(i)-xa(j))
            c = 1.0d0
            do k = M-4, M+5
              if(k.ne.j .and. k.ne.i) then
                c = c*(x-xa(k))
              endif
            enddo
            a = a + c
          endif
        enddo   
        z = z + ya(i)*a/b
      enddo
c
      return
c
      end
