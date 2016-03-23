c
c   subroutine summary1
c
      subroutine summary1(nrec,iPRN,iflag)
c
c=======================================================================
c     ****f* qualicontr/summary1
c
c   FUNCTION   
c
c     write summary including: iPRN, nrec, namb, notr, otr_ratio
c     into summry file
c
c   INPUTS
c
c     iPRN        (I)           PRN number
c     nrec        (I)           record number
c     iflag       (R)           cycle slip and outlier flags
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
C     $Id: summary1.f,v 1.0 2009/07/11 $
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
c     input/output
c
c     input
      integer       nrec
      integer       iPRN
      integer       iflag (MAX_OBS_REC)
c
c     local
c
      integer       i, j, k
      integer       irec
      integer       namb, notr
c
      real*8        otr_ratio
c
c     common
c
      integer       NPRN, allrec, allamb, allotr
c
      common /summary/ NPRN, allrec, allamb, allotr
c
      namb = 0
      notr = 0
      do irec=1, nrec
         if(iflag(irec).NE.9)then
c
            if(iflag(irec).EQ.1)then
               namb = namb + 1
            endif
        else
            notr = notr + 1
        endif
      enddo
c
      otr_ratio = dble(notr)/dble(nrec)
c
      write(unit=67,fmt=1000) iPRN, nrec, namb, notr, otr_ratio
c   
1000  format(x,4(x,I6),(x,F6.2))
c
      NPRN   = NPRN   + 1
      allrec = allrec + nrec
      allamb = allamb + namb
      allotr = allotr + notr
c
      return
c
      end
