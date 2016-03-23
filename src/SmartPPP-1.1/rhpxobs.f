c
c  subroutine rhpxobs
c
      subroutine rhpxobs(rnxvsn,nsat_sys,csat_sys,nobs_typ,cobs_typ) 
c
c=======================================================================
c     ****f* qualicontr/rhpxobs
c
c   FUNCTION   
c   
c     read hprnx level1 data, and return nobs, nepo, namb   
c
c   INPUTS
c
c     rnxvsn     real*8         rinex version
c     csat_sys   characeter     returned satellite systems in rinex 
c                               file
c     nobs_typ   integer        observation types number for existed 
c                               satellite systems.
c     cobs_typ   character      observation types for existed 
c                               satellite systems.
c
c   OUTPUT
c
c     nobs       integer        observable number
c     nepo       integer        epoch number
c     namb       integer        ambiguity number
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: rhpxobs.f,v 1.0 2009/07/27 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
c
c     input
c
      real*8        rnxvsn
      integer       nsat_sys
      character*3   csat_sys(MAX_SAT_SYS)
      integer       nobs_typ(MAX_SAT_SYS)
      character*3   cobs_typ(MAX_SAT_SYS, MAX_OBS_TYP)
c
c     output
c
      integer       nsat, nobs, nepo, namb
c
c     local
c
      real*8        EPOCH
      integer       EPOCH_nsat
      integer       EPOCH_flag
      integer       iPRN
      integer       EPOCH_iPRN
      character*3   EPOCH_cPRN
      real*8        EPOCH_OBS(MAX_OBS_TYP)       
      integer       EPOCH_LLI(MAX_OBS_TYP)
      integer       EPOCH_SNR(MAX_OBS_TYP)
c
      integer       idx_L1,idx_L2
c
      integer       tmp_nsat
c
      real*8        tmp_time
      real*8        tmp_time_fore
      integer       tmp_flag(MAX_SAT_NUM)
      integer       tmp_iPRN(MAX_SAT_NUM)
c
      real*8        tmp_OBS(MAX_SAT_NUM,MAX_OBS_TYP)
      integer       tmp_LLI(MAX_SAT_NUM,MAX_OBS_TYP)
      integer       tmp_SNR(MAX_SAT_NUM,MAX_OBS_TYP)
c
      logical       more
c
c     loop
      integer       i, k, irec, iobs
      integer       isat, iamb, iepo
c
c     common
c
      character*60  MARKER_NAME
c
      common /hpx/  MARKER_NAME 
c
      integer       iSAT_iPRN(MAX_PRN), iPRN_iSAT(MAX_PRN)
      integer       NAMB_iPRN(MAX_PRN), NAMB_iSAT(MAX_PRN)
      integer       NREC_iPRN(MAX_PRN), NREC_iSAT(MAX_PRN)
c
      character*3   cPRN_iSAT(MAX_PRN)
c
      real*8        TIME_SPAN(2)
c
      do i=1, MAX_PRN
c
         iSAT_iPRN(i) = 0
         iPRN_iSAT(i) = 0
         NAMB_iPRN(i) = 0
         NAMB_iSAT(i) = 0
         NREC_iPRN(i) = 0
         NREC_iSAT(i) = 0
c
      enddo
c
      idx_L1 = 0
      idx_L2 = 0
c
      if(rnxvsn.lt.3.00)then
c
         do i=1, nobs_typ(1)
            if(    trim(cobs_typ(1,i)).eq.'L1')then
               idx_L1 = i
            elseif(trim(cobs_typ(1,i)).eq.'L2')then
               idx_L2 = i
            endif
         enddo
c
c        double-frequency receiver ??
         if(idx_L1.EQ.0.or.idx_L2.EQ.0)then
            write(*,*) 'qualicontr/extrobs'
            write(*,*) '  not double-frequency GPS receiver'
            stop
         endif
c
      else
c
         write(*,*) "qualicontr/extrobs"
         write(*,*) '  Please modifiy to process rinex 3.00' 
         stop
c
      endif
c
c     read observation data block
c     ===========================
c
      nobs = 0
      nepo = 0
      namb = 0
      more = .true.
      tmp_nsat = 0
      tmp_time = 0.0d0
      tmp_time_fore = 0.0d0
c
 100  continue
c
      read(111, fmt=2000, end=300)
     +      irec, 
     +      EPOCH,       EPOCH_iPRN, (EPOCH_OBS(k),
     +      EPOCH_LLI(k),EPOCH_SNR(k),k=1,nobs_typ(1))
c
 2000 format(I6, F18.7, (X,I3), 12(F14.3,I1,I1))
c
c     Starting Time
      if(irec.EQ.1)then
         TIME_SPAN(1) = EPOCH
      endif
c
      tmp_time = EPOCH
c
c     continue reading observables at the same epoch
c
 110  continue
c
c     finish this epoch reading ???
c
      if(tmp_time.gt.tmp_time_fore.and.tmp_nsat.gt.0)then
c
         goto 200
c
      endif
c
c     pass the outliers
c
      if(EPOCH_LLI(idx_L1).EQ.9)then
c
         goto 100
c
      endif
c
      tmp_nsat           = tmp_nsat + 1
      tmp_flag(tmp_nsat) = EPOCH_LLI(idx_L1)
      tmp_iPRN(tmp_nsat) = EPOCH_iPRN
c
c     observables at the same EPOCH
c
      do k=1, nobs_typ(1)
         tmp_OBS(tmp_nsat,k) = EPOCH_OBS(k)
         tmp_LLI(tmp_nsat,k) = EPOCH_LLI(k)
         tmp_SNR(tmp_nsat,k) = EPOCH_SNR(k)
      enddo
c
c     store the last epoch time
c
      tmp_time_fore = tmp_time
c
      goto 100
c
c     END read EPOCH data block
c     *************************
c
 200  continue
c
c     processing last EPOCH data block
c     ================================
c
      if(tmp_nsat.GE.5)then
c
         do i=1, tmp_nsat
c
c        observable increase
         nobs = nobs + 1
c
         iPRN = tmp_iPRN(i)
c        new arc: ambiugity increase
         if(tmp_flag(i).EQ.1)then
         namb            = namb + 1
         NAMB_iPRN(iPRN) = NAMB_iPRN(iPRN) + 1
         endif
c
         NREC_iPRN(iPRN) = NREC_iPRN(iPRN) + 1
c
         enddo
c
         nepo = nepo + 1
c
      endif
c
      tmp_nsat = 0
c
c     END of DATA
c
      if(.not.more)then
c
         goto 400
c
      endif
c
      goto 110
c
  300 continue
c
      more = .false.
c
c     read the last EPOCH observation data block
      goto 200
c
  400 continue
c
      TIME_SPAN(2) = EPOCH
c
c     aggregate the total satellite number
c     ====================================
c
      isat = 0
      do iPRN=1,MAX_PRN
         if(NREC_iPRN(iPRN).GT.0)then
            isat           = isat + 1
            iSAT_iPRN(iPRN)= isat
            iPRN_iSAT(isat)= iPRN
            NREC_iSAT(isat)= NREC_iPRN(iPRN)
            NAMB_iSAT(isat)= NAMB_iPRN(iPRN)
         endif
      enddo
c
      nsat=isat
c
      return
c
      end
