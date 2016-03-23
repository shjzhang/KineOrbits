C
c   subroutine hpx2itr
c
      subroutine hpx2itr(rnxvsn,
     +                   nsat_sys, csat_sys, nobs_typ, cobs_typ,
     +                   nsat, pntr_isat, nrec_isat)
c
c=======================================================================
c     ****f* qualicontr/hpx2itr
c
c   FUNCTION   
c   
c     Transform the observation data file from HPRNX format to 
c     internal used formats, which are: 
c     
c     1) HPRNX Level1 format:
c
c        irec, EPOCH, cPRN, (obs, lli, snr)
c         *
c
c     2) HPRNX Level2 format:
c
c        irec, EPOCH, cPRN, (obs, lli, snr)
c                       *
c
c   Notes
c   =====
c
c     Level1 and Level2 are similiar, the difference are:
c     Level1 is arranged according to the record        number.
c     Level2 is arranged according to the satellite PRN number.
c
c   INPUTS
c
c     NONE
c
c   OUTPUT
c
c     rnxvsn     real*8           rinex version
c     csat_sys   characeter       returned satellite systems in rinex 
c                                 file
c     nobs_typ   integer          observation types number for existed 
c                                 satellite systems.
c     cobs_typ   character        observation types for existed 
c                                 satellite systems.
c     nsat       integer          number of satellites in HPRNX
c     pntr_isat  character        PRN number for isat 
c     nrec_isat  integer          record number for isat 
c
c   COPYRIGHT
c
c     Copyright(c) 2006-          Shoujian Zhang,
c                                 School of Geodesy and Geomatics,
c                                 Wuhan University.
c     ***
c
C     $Id: hpx2itr.f,v 1.0 2009/07/10 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
c
c     input/output
c
      integer       nsat
      integer       pntr_isat(MAX_SAT_NUM)
      integer       nrec_isat(MAX_SAT_NUM)
c
c     local 
c
      integer       isat
      integer       iepo, nepo, irec, nrec
      integer       i, j, k
      integer       ios
c
c     hprnx header file
c
      real*8        rnxvsn
      integer       nsat_sys
      character*3   csat_sys(MAX_SAT_SYS)
      integer       nobs_typ(MAX_SAT_SYS)
      character*3   cobs_typ(MAX_SAT_SYS, MAX_OBS_TYP)
c
      integer       nrec_iPRN(MAX_PRN)
c
c     hprnx observation data block
c
      character*1   REC_ID
      real*8        EPOCH
      integer       EPOCH_nsat
      integer       EPOCH_flag
      integer       iPRN
      character*3   cPRN
      integer       EPOCH_iPRN
      character*3   EPOCH_cPRN
      real*8        EPOCH_OBS(MAX_OBS_TYP)       
      integer       EPOCH_LLI(MAX_OBS_TYP)
      integer       EPOCH_SNR(MAX_OBS_TYP)
c
c     LVL1
c
      integer       LVL1_PNTR(MAX_OBS_REC)
      real*8        LVL1_Time(MAX_OBS_REC)
      integer       LVL1_iPRN(MAX_OBS_REC)
c
      real*8        LVL1_OBS(MAX_OBS_REC,MAX_OBS_TYP)
      integer       LVL1_LLI(MAX_OBS_REC,MAX_OBS_TYP)
      integer       LVL1_SNR(MAX_OBS_REC,MAX_OBS_TYP)
c
c     LVL2
c
      integer       LVL2_PNTR(MAX_PRN, MAX_OBS_REC)
c
      do i=1, MAX_PRN
         nrec_iPRN(i) = 0
      enddo
c
      do i=1, MAX_SAT_NUM
         nrec_isat(i) = 0
         pntr_isat(i) = 0
      enddo
c
c     read HPRNX header data
c     ======================
c
      call rhpxhdr(rnxvsn, nsat_sys, csat_sys, nobs_typ, cobs_typ)
c
c     HPRNX to hprnx_level1
c     =====================
c
      irec = 0    
      nrec = 0
      iepo = 0
      nepo = 0
      EPOCH_flag = 0
c
 100  continue
c
c++   read observ data block header 
c
      read(101, *, end=200, iostat=ios) 
     &     REC_ID, EPOCH, EPOCH_flag, EPOCH_nsat
c
c     EPOCH number increment
      iepo = iepo + 1
c
      if(rnxvsn .lt. 3.00)then
c
         do i=1, EPOCH_nsat
c
c           write(*,*) nobs_typ(1)
c           read a record
            read(101, fmt=1000) 
     +           EPOCH_cPRN, 
     +          (EPOCH_OBS(k),EPOCH_LLI(k),EPOCH_SNR(k),k=1,nobs_typ(1))
c           write(*,fmt=1000) 
c    +           EPOCH_cPRN, 
c    +          (EPOCH_OBS(k),EPOCH_LLI(k),EPOCH_SNR(k),k=1,nobs_typ(1))
c
 1000       format((A3),10(F14.3, I1, I1))
c
            cPRN = EPOCH_cPRN
c
            if(    EPOCH_cPRN(1:1).EQ.'G')then
c
               read(cPRN(2:3),'(I2)') EPOCH_iPRN
c
            elseif(EPOCH_cPRN(1:1).EQ.'E')then
c
               read(cPRN(2:3),'(I2)') EPOCH_iPRN
c
               EPOCH_iPRN   = EPOCH_iPRN + 100
c
               goto 222
c
            elseif(EPOCH_cPRN(1:1).EQ.'R')then
c
               read(cPRN(2:3),'(I2)') EPOCH_iPRN
c
               EPOCH_iPRN   = EPOCH_iPRN + 200
c
               goto 222
c
            elseif(EPOCH_cPRN(1:1).EQ.'S')then
c
               read(cPRN(2:3),'(I2)') EPOCH_iPRN
c
               EPOCH_iPRN   = EPOCH_iPRN + 300
c
               goto 222
c
            endif

            iPRN = EPOCH_iPRN
c
            nrec_iPRN(iPRN) = nrec_iPRN(iPRN) + 1
c
c           record number increment
            irec = irec + 1
c
c++         write HPRNX into LVL1 format
c
            write(111, fmt=2000)
     +            irec, 
     +            EPOCH,       EPOCH_cPRN, (EPOCH_OBS(k),
     +            EPOCH_LLI(k),EPOCH_SNR(k),k=1,nobs_typ(1))
c
 2000       format(I6, F18.7, (X,A3), 10(F14.3,I1,I1))
c
c
c++         save LVL1 observation data into array, which
c++         is used to create LVL2 format data file.
c
            LVL1_iPRN(irec)  = iPRN
            LVL1_PNTR(irec)  = irec
            LVL1_Time(irec)  = EPOCH
c
            do k=1, nobs_typ(1)
               LVL1_OBS(irec,k) = EPOCH_OBS(k)
               LVL1_LLI(irec,k) = EPOCH_LLI(k)
               LVL1_SNR(irec,k) = EPOCH_SNR(k)
            enddo
c
            LVL2_PNTR(iPRN,nrec_iPRN(iPRN)) = irec
c
c           !!! Pass the observations of certain satellite systems
c
 222        continue
c
         enddo
c
c        read a new observation data block
c
         goto 100
c
      elseif(rnxvsn.EQ.3.00)then
         write(*,*) 'qualicontr/hpx2itr.f'
         write(*,*) '  Please make extension for RINEX 3.00'
         stop
      endif
c
 200  continue
c
      nrec = irec
      nepo = iepo
c
      write(*,*)        'number of epoch:'
      write(*,'(x,I6)')  nepo
c
c++   satellite PRNs 
c   
      isat = 0
      do iPRN=1, MAX_PRN
         if(nrec_iPRN(iPRN).gt.0)then
            isat            = isat + 1
            pntr_isat(isat) = iPRN
            nrec_isat(isat) = nrec_iPRN(iPRN)
         endif
      enddo
c
c++   satellite number
c
      nsat = isat
c
      if(nsat.gt.40)then
         write(*,*) 'qualicontr/hpx2itr'
         write(*,*) '  satellite number are more than', nsat
         write(*,*) '  please modify the MAX_PRN in include/rinex.h'
         stop
      endif
c
c     hprnx_level1 to hprnx_level2
c     ============================
c
      if(rnxvsn.lt.3.00)then
c
         do isat=1, nsat
c
            iPRN = pntr_isat(isat)
            nrec = nrec_isat(isat)
c
            do i=1, nrec
c
               irec = LVL2_PNTR(iPRN,i)
c
               write(211, fmt=3000)
     +         LVL1_PNTR(irec),  LVL1_Time(irec),  LVL1_iPRN(irec),
     +        (LVL1_OBS( irec,k),LVL1_LLI( irec,k),LVL1_SNR( irec,k),
     +         k=1,nobs_typ(1))      
c
 3000          format(I6, F18.7, (X,I3), 10(F14.3,I1,I1))
c
            enddo
c
         enddo
c
      endif
c
      return
c
      end
