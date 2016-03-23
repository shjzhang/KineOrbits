c
c   subroutine codescr
c
      subroutine codescr(nrec,EPOCH,iPRN,P4,lotlr)
c
c=======================================================================
c     ****f* qualicontr/codescr
c
c   FUNCTION   
c
c     If the snr of observable is less than MIN_SNR, which is set in 
c     the etc/qualicontr.conf file, then set the lotlr=.true. for this
c     observable.
c
c   INPUTS
c
c     nrec        (I)           record number
c     EPOCH       (R)           observation EPOCH in seconds past J2000.0
c     iPRN        (I)           PRN number
c     P4          (R)           P2 - P1
c
c   OUTPUT
c
c     lotlr      (R)           flags for otlrs
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: codescr.f,v 1.0 2009/07/12 $
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
c     input/output variables
c
      integer       nrec
      integer       iPRN
c
      real*8        EPOCH(MAX_OBS_REC)
c
      real*8        P4(MAX_OBS_REC)
c
      logical       lotlr(MAX_OBS_REC)
c
c     output
      integer       NGOOD
c
c     common
c
      integer       outlier(MAX_PRN)
      integer       amb(MAX_PRN)
c
      common /statistic/ outlier, amb
c
c     local variables
c
      integer  i, j, k
      integer  year, mon, day, hour, min, sec_int
      real*8   sec_dec
c
c     Deleting observables with code combination
c
      do i=1, nrec
c
         if(.not.lotlr(i))then
c
            if(P4(i).lt.0.or.P4(i).gt.50)then
c
              lotlr(i) = .true.
              outlier(iPRN) = outlier(iPRN) + 1
c
c++           calendar
c
              call sec2cal(EPOCH(i),
     &                     year,mon,day,hour,min,sec_int,sec_dec)
c           
c++           information output       
c           
              write(65, fmt=1000) 
     &             'code_range     ', EPOCH(i), iPRN, 'P4', P4(i)
c
1000          format((2x,A15),(x,F14.3),(x,I3),(X,A,X,F8.2))
c
            endif
c
         endif
c
      enddo
c
      return    
c
      end
