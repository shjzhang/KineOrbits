c
c   subroutine maxbxc
c
      subroutine maxbxc(A,B,C,ABC,NROWA,NCOLA,NCOLB,NCOLC)
c
c=======================================================================
c     ****f* SmartPPP/maxbxc.f
c
c   FUNCTION   
c   
c     Input A,B,C, and compute A*B*C   
c
c   INPUTS
c
c     A,B,C,NROWA,NCOLA,NCOLB,NCOLC
c
c   OUTPUT
c
c     ABC
c
c   REVISION
c
c     2009/08/10                   build
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c     ***
c
C     $Id: maxbxc.f,v 1.0 2009/08/10 $
c=======================================================================
c
      implicit none
c
      integer   NROWA,NCOLA,NCOLB,NCOLC
c
      real*8    A  (NROWA,NCOLA)
      real*8    B  (NCOLA,NCOLB)
      real*8    C  (NCOLB,NCOLC)
c
      real*8    ABC(NROWA,NCOLC)
c
c     local
c
      real*8    AB (NROWA,NCOLB)
      integer   i,j,k
c
      do i=1,NROWA
      do k=1,NCOLC
         ABC(i,k) = 0.0d0
      enddo
      enddo
      do i=1,NROWA
      do k=1,NCOLB
         AB (i,k) = 0.0d0
      enddo
      enddo
c
      call mtxmul(A, B, AB,NROWA,NCOLA,NCOLB)
c
      call mtxmul(AB,C,ABC,NROWA,NCOLB,NCOLC)
c
      return
c
      end
