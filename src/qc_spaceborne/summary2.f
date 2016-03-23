c
c   subroutine summary2
c
      subroutine summary2()
c
c=======================================================================
c     ****f* qualicontr/summary2
c
c   FUNCTION   
c
c     write summary including: iPRN, nrec, namb, notr, otr_ratio 
c
c   INPUTS
c
c     NONE
c
c   OUTPUT
c
c     NONE
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: summary2.f,v 1.0 2009/07/11 $
c=======================================================================
c
      implicit none
c
c     include
c
      include      '../../include/rinex.h'
      include      '../../include/qualicontr.h'
      include      '../../include/qualicontr.conf.h'
c
      real*8        avrotr_ratio
c
c     common
c
      integer       NPRN, allrec, allamb, allotr
c
      common /summary/ NPRN, allrec, allamb, allotr
c
c     average outlier ratio
c
      avrotr_ratio = dble(allotr)/dble(allrec)
c
      write(unit=67,fmt=1000) '*',NPRN,allrec,allamb,allotr,avrotr_ratio
c   
1000  format(A1,4(x,I6),(x,F6.2))
c
      return
c
      end
