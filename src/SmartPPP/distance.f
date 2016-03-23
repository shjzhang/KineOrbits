*
*  subroutine distance
*      
      subroutine distance(x1, x2, dis)
c
c=======================================================================
c     ****f* SmartPPP/distance.f
c
c   FUNCTION   
c   
*     distance between x1(3) and x2(3)   
c
c   INPUTS
c
c     x1        real*8             coordinates
c     x2        real*8             coordinates
c
c   OUTPUT
c
c     dis       real*8             distance between x1 and x2
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c   REVISION
c
c     2007/09/16                   programmed
c     2009/08/03                   modifiy the header
c
c     ***
c
C     $Id: distance.f.f,v 1.0 2009/07/28 $
c=======================================================================
* 
      implicit none
c
c     input/output variable
c
      real*8 x1(3), x2(3), dis
c
c     local variable
c
      integer i
c
c     initialization
c
      dis = 0.0d0
c
c     calculate the distance
c
      do i=1,3
         dis = dis + (x1(i)-x2(i))**2
      enddo
c
      dis = dsqrt(dis)
c
      return
c
      end
