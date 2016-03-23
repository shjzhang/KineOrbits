c
c   subroutine snrscr
c
      subroutine snrscr(nrec,EPOCH,iPRN,S1,S2,lotlr,NGOOD)
c
c=======================================================================
c     ****f* qualicontr/snrscr
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
c     S1          (R)           signal to noise for L1
c     S2          (R)           signal to noise for L2
c
c   OUTPUT
c
c     NGOOD       (R)           number of good observables
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: snrscr.f,v 1.0 2009/07/11 $
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
      real*8        S1(MAX_OBS_REC)
      real*8        S2(MAX_OBS_REC)
      real*8        Nw(MAX_OBS_REC)
      real*8        L3(MAX_OBS_REC)
      real*8        P3(MAX_OBS_REC)
      real*8        L4(MAX_OBS_REC)
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
c     Deleting observables, if the SNR less than MIN_SNR
c
      NGOOD = 0
c
      do i=1, nrec
c
        if(S1(i).lt.MIN_SNR.or.S2(i).lt.MIN_SNR)then
c
          lotlr(i)      = .true.
          outlier(iPRN) = outlier(iPRN) + 1
c
c++       calendar
c
          call sec2cal(EPOCH(i),year,mon,day,hour,min,sec_int,sec_dec)
c       
c++       information output       
c       
          write(65, fmt=1000) 
     &         'low_snr        ',EPOCH(i),iPRN,'S1',S1(i),'S2',S2(i)
c
1000      format((2x,A15),(x,F14.3),(x,I3),2(X,A,X,F8.2))
c
        else
c
          NGOOD = NGOOD + 1
c
        endif
c
      enddo
c
      return    
c
      end
