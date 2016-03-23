c
c   subroutine qualelib
c
      subroutine qualelib(nrec,iPRN,EPOCH,L1,L2,P1,P2,S1,S2,
     &                                         Nw,L4,P4,L3,P3,Lw,
     &                                         Pw,iflag)
c
c=======================================================================
c     ****f* QUALE/qualelib
c
c   FUNCTION   
c
c     Detect the cycle-slips and otlrs existed in the rinex files with: 
c   
c     a) SNR screening
c     b) code screening
c     c) Widelane amb filtering, 
c     d) L1-L2 1st order difference filter
c     e) L3 5th order difference filter
c
c   INPUTS
c
c     nrec        (I)           record number
c     iPRN        (I)           PRN number
c     EPOCH       (R)           observation epoch in seconds past J2000.0
c     S1          (R)           signal to noise for L1
c     S2          (R)           signal to noise for L2
c     L4          (R)           L4
c     Nw          (R)           MW    widelane        combination
c     L3          (R)           phase Ionoshpere-free combination
c     P3          (R)           code  Ionoshpere-free combination
c     L4          (R)           phase Geometry-free   combination
c     P4          (R)           code  Geometry-free   combination
c
c   OUTPUT
c
c     iflag       (R)           cycle slip and otlr flags
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: qualelib.f,v 1.0 2009/07/11 $
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
c
      real*8        EPOCH(MAX_OBS_REC)
c
      real*8        L1(MAX_OBS_REC)
      real*8        L2(MAX_OBS_REC)
      real*8        P1(MAX_OBS_REC)
      real*8        P2(MAX_OBS_REC)
      real*8        S1(MAX_OBS_REC)
      real*8        S2(MAX_OBS_REC)
      real*8        N2(MAX_OBS_REC)
      real*8        Nw(MAX_OBS_REC)
      real*8        L3(MAX_OBS_REC)
      real*8        P3(MAX_OBS_REC)
      real*8        L4(MAX_OBS_REC)
      real*8        P4(MAX_OBS_REC)
      real*8        Lw(MAX_OBS_REC)
      real*8        Pw(MAX_OBS_REC)
c     output
      integer       iflag(MAX_OBS_REC)
c
c     local
c
c     widelane filter
c
      logical       lotlr(MAX_OBS_REC)
      logical       lflag(MAX_OBS_REC)
      logical       wflag(MAX_OBS_REC)
c
      real*8        EPOCH_rvs(MAX_OBS_REC)
      real*8        Nw_rvs   (MAX_OBS_REC)
      logical       lotlr_rvs(MAX_OBS_REC)
      logical       lflag_rvs(MAX_OBS_REC)
      logical       wflag_rvs(MAX_OBS_REC)
c
      real*8        NwMean   (MAX_ARC)
      real*8        NwMeanStd(MAX_ARC)
c
      real*8        NwCL(MAX_OBS_REC)
c
c     difference filter
c
c     another filter method
c
      real*8        N12fit(MAX_OBS_REC)
      logical       N12flag(MAX_OBS_REC)
c
      real*8        N12Mean   (MAX_ARC)
      real*8        N12MeanStd(MAX_ARC)
c
      real*8        N12CL(MAX_OBS_REC)
c
c     arcsta, arcdel
c
      integer       narc
      integer       xpts (MAX_ARC)
c
c     snrscr
c
      integer       NGOOD
c
c     loop      
c
      integer       i, j, k
      integer       irec
c
c     common
c
      integer       outlier(MAX_PRN)
      integer       amb(MAX_PRN)
c
      common /statistic/ outlier, amb
c
c     initialization
c
      do i=1, MAX_OBS_REC
         lotlr(i)  = .false.
         lflag(i)  = .false.
         wflag(i)  = .false.
         N12flag(i)  = .false.
      enddo
c
      do i=1, MAX_OBS_REC
         iflag(i)     = 0
         Nw_rvs(i)    = 0.0d0
         EPOCH_rvs(i) = 0.0d0
         lotlr_rvs(i) = .false.
         lflag_rvs(i) = .false.
         wflag_rvs(i) = .false.
      enddo
c
      do i=1, MAX_ARC
         xpts(i)      = 0
         NwMean(i)      = 0.0d0
         NwMeanStd(i)   = 0.0d0
      enddo
c
      do i=1, MAX_OBS_REC
         N12fit(i) = 0.0d0
      enddo
c
      NGOOD = 0
c
c     Signal to Noise Ratio (SNR) screening 
c
      call snrscr(nrec,EPOCH,iPRN,S1,S2,lotlr,NGOOD)
c
c     write(*,*) 'SNR'
c     do i=1,nrec
c        write(*,*) S1(i),S2(i)
c     enddo
c
cif   NGOOD <= MIN_ARC_PNT ???
c     ************************
c
      if(NGOOD.LE.MIN_ARC_PNT)then
c
      write(*,*) 'QUALE/qualelib.f'
      write(*,*) 'Number of good observables of PRN',iPRN,' is ', 
     &            NGOOD, 
     &           'which is less than minimum arc point number:',
     &            MIN_ARC_PNT  
      do i=1, nrec
         iflag(i) = 9 
      enddo
c
c     write flags into iflag 
c
      call write_iflag(nrec,iPRN,EPOCH,lotlr,lflag,iflag)
c
c     return
c     ======
      return
c
      endif
c
c     pre-screening
c
      call prescr(nrec,EPOCH,iPRN,L1,L2,P1,P2,P4,lotlr)
c
c     write(*,*) 'bpre-screening'
c     do i=1,nrec
c        write(*,*) EPOCH(i),lotlr(i)
c     enddo
c
c     Nw filter
c     *********
c
      call wlamb_filter(nrec,iPRN,EPOCH,Nw,lotlr,wflag)
c
c     widelane statis 
c
      call arc_info(nrec,iPRN,EPOCH,Nw,lotlr,wflag, 
     +              narc,xpts,NwMean,NwMeanStd)
c
c     widelane arc conjoin; conjunction
c
      call arc_conj(nrec,iPRN,EPOCH,narc,xpts,NwMean,NwMeanStd,
     &              lotlr,wflag,NwCL)
c
c     ionospheric combination detection: L4 - Polyfit(P4)
c     **************************************************
c
      call ionos_N12fit(nrec,iPRN,EPOCH,wflag,lotlr,L4,P4,S1,S2,N12fit)
c
c     filtering N12fit
c
      call ionos_filter(nrec,iPRN,EPOCH,wflag,lotlr,NwCL,N12fit,N12flag)
c
c     N12 arc statis 
c
      call arc_info(nrec,iPRN,EPOCH,N12fit,lotlr,N12flag, 
     +              narc,xpts,N12Mean,N12MeanStd)
c
c     N12 arc conjoin; conjunction
c
      call arc_conj(nrec,iPRN,EPOCH,narc,xpts,N12Mean,N12MeanStd,
     &              lotlr,N12flag,N12CL)
c
c     merge wflag, N12flag into lflag
c     =============================
c
      call merge_lflag(nrec,iPRN,EPOCH,lotlr,wflag,N12flag,lflag)
c
c     write flags into iflag 
c     ======================
c
      call write_iflag(nrec,iPRN,EPOCH,lotlr,lflag,iflag)
c
      return
c
      end
