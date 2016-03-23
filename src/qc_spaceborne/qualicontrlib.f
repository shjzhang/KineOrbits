c
c   subroutine qualicontrlib
c
      subroutine qualicontrlib(nrec,iPRN,EPOCH,S1,S2,N2,Nw,L4,P4,L3,P3,
     &                                          Lw,Pw,Nw1,NwSig1,iflag)
c
c=======================================================================
c     ****f* qualicontr/qualicontrlib
c
c   FUNCTION   
c
c     Detect the cycle-slips and otlrs existed in the rinex files with: 
c   
c     a) SNR screening
c     a) Widelane amb filtering, 
c     b) Geometry free combination (L1-L2)
c
c   INPUTS
c
c     nrec        (I)           record number
c     iPRN        (I)           PRN number
c     EPOCH       (R)           observation epoch in seconds past J2000.0
c     S1          (R)           signal to noise for L1
c     S2          (R)           signal to noise for L2
c     N2          (R)           N2
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
C     $Id: qualicontrlib.f,v 1.0 2009/07/11 $
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
      real*8        Nw1(MAX_OBS_REC)
      real*8        NwSig1(MAX_OBS_REC)
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
      real*8        NwMean (MAX_ARC)
      real*8        NwMeanSig  (MAX_ARC)
c
c     difference filter
c
      real*8        diff2Lw(MAX_OBS_REC)
      real*8        diff2Pw(MAX_OBS_REC)
      real*8        df1N2(MAX_OBS_REC)
      real*8        df2N2(MAX_OBS_REC)
      real*8        df1N2Mean(MAX_ARC)
      real*8        df1N2MeanSig(MAX_ARC)
c
      real*8        df1N21(MAX_OBS_REC)
      real*8        df1N2Sig1(MAX_OBS_REC)
c
      real*8        N2Mean (MAX_ARC)
      real*8        N2Sig  (MAX_ARC)
c
      real*8        df5L3(MAX_OBS_REC)
c
c     another filter method
c
      logical       mflag(MAX_OBS_REC)
c
c     arcsta, arcdel
c
      integer       narc
      real*8        points (MAX_ARC)
c
c     snrscr
c
      integer       NGOOD
c
c     loop      
c
      integer       i, j, k
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
         mflag(i)  = .false.
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
         points(i)      = 0
         NwMean(i)      = 0.0d0
         NwMeanSig(i)   = 0.0d0
         df1N2Mean(i)   = 0.0d0
         df1N2MeanSig(i)= 0.0d0
      enddo
c
      do i=1, MAX_OBS_REC
         Nw1(i)  = 0.0d0
         NwSig1(i)  = 0.0d0
      enddo
c
      NGOOD = 0
c
c     Signal to Noise Ratio (SNR) screening 
c     =====================================
c
      call snrscr(nrec,EPOCH,iPRN,S1,S2,lotlr,NGOOD)
c
cif   NGOOD <= MIN_ARC_PNT ???
c     ************************
c
      if(NGOOD.LE.MIN_ARC_PNT)then
c
      write(*,*) 'qualicontr/qualicontrlib.f'
      write(*,*) 'Number of good observables of PRN',iPRN,' is ', 
     &            NGOOD, 
     &           'which is less than minimum arc point number:',
     &            MIN_ARC_PNT  
      do i=1, nrec
         iflag(i) = 9 
      enddo
c
c     write flags into iflag 
c     ======================
c
      call write_iflag(nrec,iPRN,EPOCH,lotlr,lflag,iflag)
c
c     return
c     ======
      return
c
      endif
c
c     code screening
c     ==============
c
      call codescr(nrec,EPOCH,iPRN,P4,lotlr)
c
cif   NGOOD > MIN_ARC_PNT, then continue ....
c     ***************************************
c
c     reverse Nw, lotlr, EPOCH
c     ===========================
c
      call vecrvsr(Nw,   MAX_OBS_REC,nrec,Nw_rvs   )
      call vecrvsr(EPOCH,MAX_OBS_REC,nrec,EPOCH_rvs)
      call vecrvsl(lotlr,MAX_OBS_REC,nrec,lotlr_rvs)
c
c     Nw Backward widelane filter
c     ===========================
c     Function  
c
c     delete outliers in the beginning records, which can not 
c     be detected by foreward widelane filter. 
c
c     Notes     
c
c     only store outlier flags in the lotlr_rvs array.
c     cycle slip will be detected in the following foreward filter.
c
      call wlamb_filter(nrec,iPRN,EPOCH_rvs,Nw_rvs,
     +                            lotlr_rvs,wflag_rvs)
c
c++   recover the right order and store the reversed detected otlrs
c
      call vecrvsl(lotlr_rvs,MAX_OBS_REC,nrec,lotlr)
c
c     Nw Foreward widelane filter
c     ===========================
c
      call wlamb_filter(nrec,iPRN,EPOCH,Nw,lotlr,wflag)
c
c     scheme1: widelane filter + 1st difference equation of L1-L2
c     ***********************************************************
c
      if(    ischeme.EQ.1)then
c
c     1st order difference filtering
c
      call diff1_filter(nrec,iPRN,EPOCH,N2,lotlr,mflag)
c
c     scheme2: widelane filter + 5-rd difference equation of L3
c     *********************************************************
c
      elseif(ischeme.EQ.2)then
c
c     cycleslip with high-order difference equation
c     =============================================
c
      call diff5_filter(nrec,iPRN,EPOCH,L3,lotlr,mflag)
c
      endif
c
c     merge wflag, mflag into lflag
c     =============================
c
      call merge_lflag(nrec,iPRN,EPOCH,lotlr,wflag,mflag,lflag)
c
c     statis the arc number and points for every arc
c     ==============================================
c
c++   delete the widelane arc, if the number is less than MIN_ARC_PNT 
c
      call arcsta(nrec,iPRN,EPOCH,Nw,lotlr,lflag, 
     +            narc,points,NwMean,NwMeanSig,Nw1,NwSig1)
c
c     write flags into iflag 
c     ======================
c
      call write_iflag(nrec,iPRN,EPOCH,lotlr,lflag,iflag)
c
      return
c
      end
