*
*  procedure vecmtx
*      
      subroutine vecmtx(N, avec, araw)
c
c=======================================================================
c     ****f* SmartPPP/maxbxc.f
c
c   FUNCTION   
c   
c     change vector-stored format into raw matrix 
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
c
      implicit none
c
c     input variables
c
      integer   N
      real*8    avec( ( N*(N+1) )/2)
c
c     output variables       
c       
      real*8    araw(N,N)
c
c     local variables       
c       
      integer   i, j, k
c
      k = 0
      do j = 1, n
         do i = 1, j - 1
           k = k + 1
           araw(i,j) = avec(k)
           araw(j,i) = avec(k)
         end do
         k = k + 1
         araw(j,j) = avec(k)
      end do
c
      return
c
      end
