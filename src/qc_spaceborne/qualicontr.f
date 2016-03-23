c
c   program qualicontr
c
      program qualicontr
c
c=======================================================================
c     ****f* qualicontr/qualicontr
c
c   FUNCTION   
c   
c     This toolkit is used to detect the cycle-slips and outliers 
c     existed in the rinex files with: 
c   
c     a) SNR screening
c     a) Widelane ambiguity filtering, 
c     b) Geometry free combination (L1-L2)
c
c   Notes
c
c     The standard input format is HPRNX, not RINEX. 
c
c   Reference
c
c     1. RINEX 3.00 
c     2. Montenbruck, O. 2003, In flight performance analysis of the 
c        CHAMP BlackJack GPS receiver. 
c     3. Blewitt, 1990, An automatic GPS data editing tool: turobEdit
c
c   USAGE
c
c     qualicontr  -i  input_file  -o  output_file    
c
C   ARGUMENTS
C
C     -i   Argument                    input rinex observation file
C     -o   Argument                    output hprnx observation file
c
c   COPYRIGHT
c
c     Copyright(c) 2006-               Shoujian Zhang,
c                                      School of Geodesy and Geomatics,
c                                      Wuhan University.
c     ***
c
C     $Id: qualicontr.f,v 1.0 2009/07/07 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
      include      '../../include/qualicontr.h'
      include      '../../include/qualicontr.conf.h'
c
c     variables in getopts     
c
      integer       MAX_OPTS
      integer       nopts
      integer       stat
      parameter    (MAX_OPTS = 20)
      parameter    (nopts    =  4)
c
c     variables from getargvs
c
      integer       nargv
      character*200 argvs(MAX_OPTS)
c
      character*200 ifile
      character*200 ofile
c
      integer       iargv
      integer       ios
c
c     variables for rinex 3.00 processing
c  
c     rinex header
c   
      real*8        rnxvsn
      integer       nsat_sys
      character*3   csat_sys(MAX_SAT_SYS)
      integer       nobs_typ(MAX_SAT_SYS)
      character*3   cobs_typ(MAX_SAT_SYS, MAX_OBS_TYP)
c
c     hprnx data block
c
      integer       EPOCH_flag
      integer       EPOCH_nsat
c
      integer       EPOCH_iPRN(MAX_SAT_NUM)
      character*3   EPOCH_cPRN(MAX_SAT_NUM)
c
      integer       EPOCH_LLI(MAX_OBS_TYP), EPOCH_SNR(MAX_OBS_TYP)
      real*8        EPOCH_OBS(MAX_OBS_TYP)
c
c     qualicontr returned
c
      integer       nsat
      integer       pntr_isat(MAX_SAT_NUM)
      integer       nrec_isat(MAX_SAT_NUM)
c
c     rhpxobs and whpxobs variable
c
      integer       nobs
      integer       lvl2_PNTR(MAX_OBS_REC)
      real*8        lvl2_Time(MAX_OBS_REC)
      integer       lvl2_iPRN(MAX_OBS_REC)
      integer       lvl2_flag(MAX_OBS_REC)
c
      real*8        lvl2_OBS(MAX_OBS_REC, MAX_OBS_TYP)
      integer       lvl2_LLI(MAX_OBS_REC, MAX_OBS_TYP)
      integer       lvl2_SNR(MAX_OBS_REC, MAX_OBS_TYP)
c
c     merge hpxobs
c
      real*8        lvl2_Nw(MAX_OBS_REC)
      real*8        lvl2_Sw(MAX_OBS_REC)
c
c     extrobs variable
c
      real*8        L1(MAX_OBS_REC)
      real*8        L2(MAX_OBS_REC)
      real*8        LA(MAX_OBS_REC)
      real*8        C1(MAX_OBS_REC)
      real*8        P1(MAX_OBS_REC)
      real*8        P2(MAX_OBS_REC)
c
      real*8        S1(MAX_OBS_REC)
      real*8        S2(MAX_OBS_REC)
      real*8        SA(MAX_OBS_REC)
c
      integer       LLI  (MAX_OBS_REC)
      real*8        EPOCH(MAX_OBS_REC)
c
c     qualicontrlib
c
      integer       iflag (MAX_OBS_REC)
      real*8        Nw1   (MAX_OBS_REC)
      real*8        NwSig1(MAX_OBS_REC)
c
c     linear combination
c
      real*8        N2 (MAX_OBS_REC)
      real*8        Nw (MAX_OBS_REC)
      real*8        L3 (MAX_OBS_REC)
      real*8        P3 (MAX_OBS_REC)
      real*8        L4 (MAX_OBS_REC)
      real*8        P4 (MAX_OBS_REC)
      real*8        Lw (MAX_OBS_REC)
      real*8        Pw (MAX_OBS_REC)
c
      real*8        dL1(MAX_OBS_REC)
      real*8        MC1(MAX_OBS_REC)
      real*8        MP1(MAX_OBS_REC)
      real*8        MP2(MAX_OBS_REC)
c
c     information file
c
      character*80  line
      character*100 file_arc     
      character*100 file_outlier 
      character*100 file_summary 
c
c     loop
c
      integer       i, j, k
      integer       nepo, iPRN, nrec
      integer       isat
      character*3   cPRN
c
c     system path
c
      character*100 HOME_PATH
      character*100 QUALICONTR_CONF
c
c     common
c
      integer       NPRN, allrec, allamb, allotr
      integer       outlier(MAX_PRN)
      integer       amb(MAX_PRN)
c
      common /summary/ NPRN, allrec, allamb, allotr
      common /statistic/ outlier, amb
c
      NPRN   = 0
      allrec = 0
      allamb = 0
      allotr = 0
c
      do i=1, MAX_PRN
         outlier(i) = 0
         amb(i) = 0
      enddo
c
      do i=1, MAX_OBS_REC
c
         lvl2_PNTR(i)   = 0 
         lvl2_Time(i)   = 0.0d0
         lvl2_iPRN(i)   = 0
         lvl2_flag(i)   = 0
c
         do k=1, MAX_OBS_TYP
         lvl2_OBS(i, k) = 0.0d0
         lvl2_LLI(i, k) = 0
         lvl2_SNR(i, k) = 0
         enddo
c
         lvl2_Nw(i)     = 0.0d0
         lvl2_Sw(i)     = 0.0d0
c
      enddo
c
c     read arguments from command line
c     ================================
c
      call getargvs(nargv, argvs, stat)
c
      if(stat /=0 )then
         write(*,*)
         write(*,*) 
     &'USAGES'
         write(*,*)
         write(*,*)
     &'     qualicontr -i file -o file'
         write(*,*)
         write(*,*) 
     &'OPTIONS'
         write(*,*)
         write(*,*)
     &'     The options are as follows:'
         write(*,*)
         write(*,*)
     &'     -i file   input observation file with hprnx format.'
         write(*,*)
         write(*,*) 
     &'     -o file   output observation file with hprnx format.',
     &' Any existing file with that name will be overwritten.'
         write(*,*) 
         write(*,*)
     &'AUTHOR       '
         write(*,*)
         write(*,*)
     &'     Programmed by Shoujian Zhang'
         write(*,*)
c
         stop
c
      endif
c
      if(nargv /= nopts)then
         write(*,*) 'qualicontr/qualicontr.f'
         write(*,*) 'input argments not correct'
         stop
      endif
c
      iargv = 1
      do while(iargv <= nargv) 
         if(    argvs(iargv)=='-i')then
            iargv = iargv + 1
            ifile = argvs(iargv)
         elseif(argvs(iargv)=='-o')then
            iargv = iargv + 1
            ofile = argvs(iargv)
         endif
         iargv = iargv + 1
      end do   
c
c     qualicontr confiugre file name
c     =========================
c
      QUALICONTR_CONF = 'etc/qualicontr.conf'
c
      call getenv('HOPES_HOME',HOME_PATH)
c
      QUALICONTR_CONF = trim(HOME_PATH)//trim(QUALICONTR_CONF)
c
c     parse the parameters in qualicontr.conf
c
      call parse_conf(QUALICONTR_CONF)
c
c     open rinex file
c     ===============
c
      open(unit=101,file=ifile,status='old',iostat=ios)
c
      if(ios.ne.0)then
         write(*,*) 'qualicontr/qualicontr.f'
         write(*,*) 'open rinex file error'
         stop
      endif
c
c     open hprnx file
c     ===============
c
      open(unit=102,file=ofile,status='replace',iostat=ios)
c
      if(ios.ne.0)then
         write(*,*) 'qualicontr/qualicontr.f'
         write(*,*) 'open hprnx file error'
         stop
      endif
c
c     open internal formatted file 
c     ============================
c
      open(unit=111,status='scratch',iostat=ios)
c
      if(ios.NE.0)then
         write(*,*) 'qualicontr/qualicontr.f'
         write(*,*) '  open lvl1 files error!'
         stop
      endif
c
      open(unit=112,status='scratch',iostat=ios)
c
      if(ios.NE.0)then
         write(*,*) 'qualicontr/qualicontr.f'
         write(*,*) '  open lvl1 hdr files error!'
         stop
      endif
c
      open(unit=211,status='scratch',iostat=ios)
 
      if(ios.NE.0)then
         write(*,*) 'qualicontr/qualicontr.f'
         write(*,*) '  open lvl2 files error!'
         stop
      endif
c
      open(unit=311,status='scratch',iostat=ios)
c
      if(ios.NE.0)then
         write(*,*) 'qualicontr/qualicontr.f'
         write(*,*) '  open lvl1 files error!'
         stop
      endif
c
      open(unit=411,status='scratch',iostat=ios)
 
      if(ios.NE.0)then
         write(*,*) 'qualicontr/qualicontr.f'
         write(*,*) '  open lvl2 files error!'
         stop
      endif
c
c     ambiguity and outlier information file 
c
      file_arc          = trim(ofile)//'.arc'
      file_outlier      = trim(ofile)//'.outlier'
      file_summary      = trim(ofile)//'.summary'
c
c     infomation file including cycleslip and outlier 
c     ===============================================
c
      open(unit=66, file=file_arc,     status='replace', iostat=ios)
      open(unit=65, file=file_outlier, status='replace', iostat=ios)
      open(unit=67, file=file_summary, status='replace', iostat=ios)
c
      if(ios.NE.0)then
         write(*,*) 'qualicontr/qualicontr.f'
         write(*,*) '  open lvl2 files error!'
         stop
      endif
c
c     outlier header
c
      line = ' '
c
      write(line( 1:40),fmt=3000) 
     &     'Column 1:          OUTLIER CAUSES       '
      write(unit=65,    fmt=5000) line(1:60)
c
      write(line( 1:40),fmt=3000) 
     &     'Column 2:          EPOCH                '
      write(unit=65,    fmt=5000) line(1:60)
c
      write(line( 1:40),fmt=3000) 
     &     'Column 3:          PRN NUMBER           '
      write(unit=65,    fmt=5000) line(1:60)
c
      write(line( 1:40),fmt=3000) 
     &     'Line   *:          SUMMATION            '
      write(unit=65,    fmt=5000) line(1:60)
c
      write(line(1:20), fmt=4000) 
     &     'END OF HEADER       '
      write(unit=65,    fmt=4000) line(1:20)
c
c     summary header   
c
3000  format(A40)
4000  format(A20)
5000  format(A60)

c
      line = ' '
c
      write(line( 1:40),fmt=3000) 
     &     'Column 1:          iPRN                 '
      write(unit=67,    fmt=5000) line(1:60)
c
      write(line( 1:40),fmt=3000) 
     &     'Column 2:          Number of records    '
      write(unit=67,    fmt=5000) line(1:60)
c
      write(line( 1:40),fmt=3000) 
     &     'Column 3:          Number of ambiguities'
      write(unit=67,    fmt=5000) line(1:60)
c
      write(line( 1:40),fmt=3000) 
     &     'Column 4:          Number of ouliters   '
      write(unit=67,    fmt=5000) line(1:60)
c
      write(line( 1:40),fmt=3000) 
     &     'Column 5:          Ratio  of ouliters   '
      write(unit=67,    fmt=5000) line(1:60)
c
      write(line( 1:40),fmt=3000) 
     &     'Line   *:          Summary              '
      write(unit=67,    fmt=5000) line(1:60)
c
      write(line(1:20), fmt=4000) 
     &     'END OF HEADER       '
c
      write(unit=67,    fmt=4000) line(1:20)
c
c     ==========================
c     ==========================
c
      write(*,*)
      write(*,*) 'processing ...'
      write(*,*)
c
c     hprnx2inter 
c     ===========
c
      call hpx2itr(rnxvsn,
     &             nsat_sys, csat_sys, nobs_typ, cobs_typ,
     &             nsat, pntr_isat, nrec_isat)
c
      write(*,*)           'satellite system number:'
      write(*,'( (x,I6))')  nsat_sys
c
      do i=1, nsat_sys
c
      write(*,*)           'satellite system:'
      write(*,'( (x,A6))')  trim(csat_sys(i))
      write(*,*)           'observation types number:'
      write(*,'( (x,I6))')  nobs_typ(i)
      write(*,*)           'observation types:'
      write(*,'(9(x,A3))') (cobs_typ(i,k),k=1,nobs_typ(i))
c
      enddo
c
c     read hprnx observation from lvl2 file
c     =======================================
c
      call rhpxobs(rnxvsn, 
     &             nsat_sys, csat_sys, nobs_typ, cobs_typ, nobs,
     &             lvl2_PNTR,lvl2_Time,lvl2_iPRN,lvl2_OBS, 
     &             lvl2_LLI, lvl2_SNR)             
c
      write(*,'(x,a)')
     &     'number of observations for each observation types:'
      write(*,'(x,I6)') 
     &      nobs 
c
c     cycleslip and outlier detection
c     ===============================
c
      do isat=1, nsat
c
c        PRN number
         iPRN = pntr_isat(isat)
c
c        PRN record number
         nrec = nrec_isat(isat)
c
c        output record number for PRNs
c
         write(*,fmt=2000) "record number for PRN", iPRN," is:", nrec
c
2000     format(X,A,I3,A,I4)
c
c++      extract observables for PRN
c   
         call extrobs(isat, nrec_isat, 
     &                iPRN, rnxvsn, nobs_typ, cobs_typ,
     &                lvl2_Time, lvl2_OBS, lvl2_LLI, lvl2_SNR,
     &                EPOCH, L1, L2, LA, C1, P1, P2, S1, S2, SA, lli)
c
c++      linear combination          
c
c        L3 = (f1^2*L1-f2^2*L2)/(f1^2-f2^2)
c        P3 = (f1^2*P1-f2^2*P2)/(f1^2-f2^2)
c        L4 =  L1 - L2 =  dion + lam1*N1 - lam2*N2
c        P4 =  P1 - P2 = +dion
c
         call linear_combine(nrec,L1,L2,LA,C1,P1,P2,
     &                       N2,Nw,L3,P3,L4,P4,Lw,Pw,dL1,MC1,MP1,MP2)
c
c++      cycle slip and outlier detection
c
c        Method1 : SNR screen
c        Method2 : Modified Melbourne-Wubberna combination 
c        Method3 : geometry-free (L4) high-order difference
c
         call qualicontrlib(nrec,iPRN,EPOCH,S1,S2,N2,Nw,L4,P4,L3,P3,
     &                                      Lw,Pw,Nw1,NwSig1,iflag)
c
c        write summary including: 
c        arc points, outlier number, outlier ratio, etc.
c
         call summary1(nrec,iPRN,iflag)
c
c++      merge flags into level2 file
c
         call merge_hpxobs(rnxvsn,nobs_typ,cobs_typ,isat,nrec_isat,
     &                     iflag,Nw1,NwSig1,lvl2_LLI,lvl2_Nw,lvl2_Sw)
c
      enddo
c
c     summary
c     =======
c
      call summary2()
c
c     write hprnx observation with cycleslip and outlier flags
c     ========================================================
c
      call whpxobs(rnxvsn,nsat_sys,csat_sys,nobs_typ,cobs_typ,nobs, 
     &             lvl2_PNTR,lvl2_Time,lvl2_iPRN,lvl2_OBS,lvl2_LLI, 
     &             lvl2_SNR,lvl2_Nw,lvl2_Sw)   
c
c     itr2hpx
c     =======
c
      call itr2hpx(rnxvsn,nsat_sys,csat_sys,nobs_typ,cobs_typ,nobs,
     &             lvl2_PNTR,lvl2_Time,lvl2_iPRN,lvl2_OBS,lvl2_LLI,
     &             lvl2_SNR,lvl2_Nw,lvl2_Sw)
c
c     close 
c     =====
c
      close(101)
      close(102)
c
      close(111)
      close(112)
      close(211)
      close(311)
      close(411)
c
      close(65)
      close(66)
      close(67)
c
      write(*,*) 'Stop Normally'
c
      stop
c
      end
