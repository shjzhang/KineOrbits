c
c   subroutine x_elv.f
c
      subroutine x_elv(x1,x2,elv)
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
c     elv       real*8             elevation [arc]
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
      real*8    x1(3), x2(3), elv
c
c     loop
c
      integer   i,j,k
      real*8    NEU(3)
c
      call xyz2neu(x1,x2,NEU)
c
      elv = atan2(NEU(3),sqrt(NEU(1)**2+NEU(2)**2))
c
      return
c
      end
