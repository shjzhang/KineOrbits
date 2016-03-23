c
c   program newcode
c
      program newcode
c
c=======================================================================
c     ****f* newcode/newcode
c
c   FUNCTION   
c   
c     refine code with high-order difference equation method     
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
c     newcode  -ihpx  input_file -ires input_residul file -ohpx output_file    
c
C   ARGUMENTS
C
C     -ihpx   Argument                 input  hprnx observation file
c     -ires   Argument                 input  hprnx residual    file of P3
C     -ohpx   Argument                 output hprnx observation file
c
c   COPYRIGHT
c
c     Copyright(c) 2006-               Shoujian Zhang,
c                                      School of Geodesy and Geomatics,
c                                      Wuhan University.
c     ***
c
C     $Id: newcode.f,v 1.0 2009/08/07 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
c
c     variables in getopts     
c
      integer       MAX_OPTS
      integer       nopts
      integer       stat
      parameter    (MAX_OPTS = 20)
      parameter    (nopts    =  6)
c
c     variables from getargvs
c
      integer       nargv
      character*200 argvs(MAX_OPTS)
c
      character*200 ihpx_file
      character*200 ires_file
      character*200 ohpx_file
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
c     newcode returned
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
      integer       lvl2_nsat(MAX_OBS_REC)
      integer       lvl2_flag(MAX_OBS_REC)
c
      real*8        lvl2_OBS(MAX_OBS_REC, MAX_OBS_TYP)
      integer       lvl2_LLI(MAX_OBS_REC, MAX_OBS_TYP)
      integer       lvl2_SNR(MAX_OBS_REC, MAX_OBS_TYP)
c
      real*8        lvl2_rres(MAX_OBS_REC)
      integer       nres
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
c     newcodelib
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
      real*8        res(MAX_OBS_REC)
      integer       lsat(MAX_OBS_REC)
      real*8        true_err(MAX_OBS_REC)
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
      do i=1, MAX_SAT_SYS
      csat_sys(i) = ' '
      nobs_typ(i) = 0
      do k=1, MAX_OBS_TYP
      cobs_typ(i, k) = ' '
      enddo
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
         lvl2_rres(i)    = 0.0d0
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
     &'     newcode -ihpx file -ires file -ohpx file'
         write(*,*)
         write(*,*) 
     &'OPTIONS'
         write(*,*)
         write(*,*)
     &'     The options are as follows:'
         write(*,*)
         write(*,*)
     &'     -ihpx file   input  observation file with hprnx format.'
         write(*,*)
     &'     -ires file   input  residual    file with hprnx format.'
         write(*,*)
         write(*,*) 
     &'     -ohpx file   output observation file with hprnx format.',
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
         write(*,*) 'newcode/newcode.f'
         write(*,*) 'input argments not correct'
         stop
      endif
c
      iargv = 1
      do while(iargv <= nargv) 
         if(    argvs(iargv)=='-ihpx')then
            iargv = iargv + 1
            ihpx_file = argvs(iargv)
            write(*,*) ihpx_file
         elseif(argvs(iargv)=='-ires')then
            iargv = iargv + 1
            ires_file = argvs(iargv)
            write(*,*) ires_file
         elseif(argvs(iargv)=='-ohpx')then
            iargv = iargv + 1
            ohpx_file = argvs(iargv)
            write(*,*) ohpx_file
         endif
         iargv = iargv + 1
      end do   
c
c     open rinex file
c     ===============
c
      open(unit=101,file=ihpx_file,status='old',iostat=ios)
c
      if(ios.ne.0)then
         write(*,*) 'newcode/newcode.f'
         write(*,*) 'open hpx file error'
         stop
      endif
c
      open(unit=201,file=ires_file,status='old',iostat=ios)
c
      if(ios.ne.0)then
         write(*,*) 'newcode/newcode.f'
         write(*,*) 'open res file error'
         stop
      endif
c
c     open hprnx file
c     ===============
c
      open(unit=102,file=ohpx_file,status='replace',iostat=ios)
c
      if(ios.ne.0)then
         write(*,*) 'newcode/newcode.f'
         write(*,*) 'open hpx file error'
         stop
      endif
c
c     open internal formatted file 
c     ============================
c
      open(unit=111,file='hpx.lvl1',status='replace',iostat=ios)
c     open(unit=111,                status='scratch',iostat=ios)
c
      if(ios.NE.0)then
         write(*,*) 'newcode/newcode.f'
         write(*,*) '  open lvl1 files error!'
         stop
      endif
c
      open(unit=112,status='scratch',iostat=ios)
c
      if(ios.NE.0)then
         write(*,*) 'newcode/newcode.f'
         write(*,*) '  open lvl1 hdr files error!'
         stop
      endif
c
      open(unit=211,file='hpx.lvl2',status='replace',iostat=ios)
c     open(unit=211,                status='scratch',iostat=ios)
 
      if(ios.NE.0)then
         write(*,*) 'newcode/newcode.f'
         write(*,*) '  open lvl2 files error!'
         stop
      endif
c
      open(unit=311,status='scratch',iostat=ios)
c
      if(ios.NE.0)then
         write(*,*) 'newcode/newcode.f'
         write(*,*) '  open lvl1 files error!'
         stop
      endif
c
      open(unit=411,status='scratch',iostat=ios)
 
      if(ios.NE.0)then
         write(*,*) 'newcode/newcode.f'
         write(*,*) '  open lvl2 files error!'
         stop
      endif
c
c     residual file 
c
      open(unit=121,file='res.lvl1',status='replace',iostat=ios)
c     open(unit=121,                status='scratch',iostat=ios)
c
      if(ios.NE.0)then
         write(*,*) 'newcode/newcode.f'
         write(*,*) '  open res lvl1 files error!'
         stop
      endif
c
      open(unit=122,status='scratch',iostat=ios)
c
      if(ios.NE.0)then
         write(*,*) 'newcode/newcode.f'
         write(*,*) '  open res lvl1 hdr files error!'
         stop
      endif
c
      open(unit=221,file='res.lvl2',status='replace',iostat=ios)
c     open(unit=221,                status='scratch',iostat=ios)
 
      if(ios.NE.0)then
         write(*,*) 'newcode/newcode.f'
         write(*,*) '  open res lvl2 files error!'
         stop
      endif
c
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
      write(*,*)            'satellite system number:'
      write(*,'(  (x,I6))')  nsat_sys
c
      do i=1, nsat_sys
c
      write(*,*)            'satellite system:'
      write(*,'(  (x,A6))')  trim(csat_sys(i))
      write(*,*)            'observation types number:'
      write(*,'(  (x,I6))')  nobs_typ(i)
      write(*,*)            'observation types:'
      write(*,'(11(x,A3))') (cobs_typ(i,k),k=1,nobs_typ(i))
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
c     res2itr 
c     =======
c
      call res2itr()
c
c     read residual of P3
c     ===================
c
      call rhpxres(lvl2_nsat,lvl2_rres,nres)
c
      write(*,'(x,a)')
     &     'number of residual     for P3:'
      write(*,'(x,I6)') 
     &      nres
c
      if(nobs.NE.nres)then
         write(*,*) 'code_refine/newcode.f'
         write(*,*)  nobs,'ne', nres
         stop
      endif
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
         call extrobs(isat,nrec_isat, 
     &                iPRN,rnxvsn,nobs_typ,cobs_typ,
     &                lvl2_Time,lvl2_OBS,lvl2_rres,lvl2_nsat,
     &                EPOCH,L1,L2,LA,C1,P1,P2,S1,S2,SA,res,lsat)
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
c**      recover real error with high-order difference
c
         call newcodelib(nrec,iPRN,EPOCH,L3,P3,res,lsat,true_err)
c
c++      merge residual into observables
c
         call merge_hpxobs(rnxvsn,nobs_typ,cobs_typ,isat,nrec_isat,
     &                     lvl2_OBS,true_err)
c
      enddo
c
c     write hprnx observation 
c     =======================
c
      call whpxobs(rnxvsn,nsat_sys,csat_sys,nobs_typ,cobs_typ,nobs, 
     &             lvl2_PNTR,lvl2_Time,lvl2_iPRN,lvl2_OBS,lvl2_LLI, 
     &             lvl2_SNR)   
c
c     itr2hpx
c     =======
c
      call itr2hpx(rnxvsn,nsat_sys,csat_sys,nobs_typ,cobs_typ,nobs,
     &             lvl2_PNTR,lvl2_Time,lvl2_iPRN,lvl2_OBS,lvl2_LLI,
     &             lvl2_SNR)
c
c     close 
c     =====
c
      close(101)
      close(102)
c
      close(201)
c
      close(111)
      close(112)
      close(211)
      close(311)
      close(411)
c
      close(121)
      close(122)
      close(221)
c
      write(*,*) 'Stop Normally'
c
      stop
c
      end
