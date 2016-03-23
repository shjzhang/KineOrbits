*
*  procedure mtxvec
*      
      subroutine mtxvec(N, araw, avec)
c
c=======================================================================
c     ****f* SmartPPP/maxbxc.f
c
c   FUNCTION   
c   
c     change raw matrix into vector-stored format
c
c   INPUTS
c   
c     N, araw
c
c   OUTPUT
c
c     avec
c
c   REVISION
c
c     2009/08/29                   build
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c     ***
c
C     $Id: maxbxc.f,v 1.0 2009/08/29 $
c=======================================================================
c
      implicit none
c
c     input variables
c
      integer   N
      real*8    araw(N,N)
c
c     output variables       
c       
      real*8    avec( ( N*(N+1) )/2)
c
c     local variables       
c       
      integer   i, j, k
c
      k = 0
      do i = 1, n
         do j = 1, i 
            k = k + 1
            avec(k) = araw(i,j)
         end do
      end do
c
      return
c
      end
