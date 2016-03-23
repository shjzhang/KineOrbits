C
c   subroutine hpx2itr
c
      subroutine hpx2itr(rnxvsn,nsat_sys,csat_sys,nobs_typ,cobs_typ)
c
c=======================================================================
c     ****f* SmartPPP/hpx2itr
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
c   Notes
c
c     Level1 and Level2 are similiar, the difference are:
c     Level1 is arranged according to the record number.
c     
c     !!!
c   
c     SmartPPP/hpx2itr is different from SmartPPP/hpx2itr
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
c     iPRN_isat  character        PRN number for isat 
c     NREC_isat  integer          record number for isat 
c
c   COPYRIGHT
c
c     Copyright(c) 2006-          Shoujian Zhang,
c                                 School of Geodesy and Geomatics,
c                                 Wuhan University.
c   REVISION
c
c     2009/08/10                  only read good observables
c
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
c     hprnx header file
c
      real*8        rnxvsn
      integer       nsat_sys
      character*3   csat_sys(MAX_SAT_SYS)
      integer       nobs_typ(MAX_SAT_SYS)
      character*3   cobs_typ(MAX_SAT_SYS, MAX_OBS_TYP)
      integer       isat
      integer       iepo, irec, iamb
      integer       idx_L1, idx_L2
      integer       i, j, k
      integer       ios
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
c     lvl1
c
      integer       tmp_iPRN(MAX_SAT_NUM)
      real*8        tmp_OBS (MAX_SAT_NUM, MAX_OBS_TYP)
      integer       tmp_LLI (MAX_SAT_NUM, MAX_OBS_TYP)
      integer       tmp_SNR (MAX_SAT_NUM, MAX_OBS_TYP)
      integer       tmp_nsat
c
c     common
c     ******
c
      integer       NSAT, NEPO
      integer       NAMB, NREC
      integer       iSAT_iPRN(MAX_PRN), iPRN_iSAT(MAX_PRN)
      integer       NAMB_iPRN(MAX_PRN), NAMB_iSAT(MAX_PRN)
      integer       NREC_iPRN(MAX_PRN), NREC_iSAT(MAX_PRN)
c
      character*3   cPRN_iSAT(MAX_PRN)
c
      real*8        TIME_SPAN(2)
      real*8        HPX_EPO(MAX_EPO)
c
      integer       NSAT_iEPO(MAX_EPO)
      integer       iPRN_iEPO(MAX_EPO,MAX_SAT_NUM)
c
      common /obs/  NSAT,      NEPO,     
     &              NAMB,      NREC, 
     &              iPRN_iSAT, iSAT_iPRN,
     &              NREC_iPRN, NREC_iSAT,
     &              NAMB_iPRN, NAMB_iSAT, 
     &              cPRN_iSAT, TIME_SPAN,
     &              HPX_EPO,
     &              NSAT_iEPO, iPRN_iEPO
c
      do i=1, MAX_PRN
         iSAT_iPRN(i) = 0
         iPRN_iSAT(i) = 0
         NAMB_iPRN(i) = 0
         NAMB_iSAT(i) = 0
         NREC_iPRN(i) = 0
         NREC_iSAT(i) = 0
      enddo
c
c
c     read HPRNX header data
c     ======================
c
      call rhpxhdr(rnxvsn, nsat_sys, csat_sys, nobs_typ, cobs_typ)
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
            write(*,*) 'SmartPPP/hpx2itr'
            write(*,*) '  not double-frequency GPS receiver'
            stop
         endif
c
      else
c
         write(*,*) "SmartPPP/hpx2itr"
         write(*,*) '  Please modifiy to process rinex 3.00' 
         stop
c
      endif
c
c     HPRNX to hprnx_level1
c     =====================
c
      irec = 0    
      nrec = 0
      iepo = 0
      nepo = 0
      iamb = 0
      namb = 0
      EPOCH_flag = 0
c
 100  continue
c
c++   read observ data block header 
c
c     !!!! RINEX 3.00 format, not used recently
c
c     read(101, *, end=200, iostat=ios) 
c    &     REC_ID, EPOCH, EPOCH_flag, EPOCH_nsat
c
      read(101, *, end=200, iostat=ios) 
     &     EPOCH, EPOCH_nsat
c
c
      if(rnxvsn .lt. 3.00)then
c
         do i=1, EPOCH_nsat
c
c           read a record
            read(101, fmt=1000) 
     +           EPOCH_cPRN, 
     +          (EPOCH_OBS(k),EPOCH_LLI(k),EPOCH_SNR(k),k=1,nobs_typ(1))
c
 1000       format((x,A3),12(F14.3, I1, I1))
c
            cPRN = EPOCH_cPRN
c
c           GPS
            if(    EPOCH_cPRN(1:1).EQ.'G')then
            read(cPRN(2:3),'(I2)') EPOCH_iPRN
c           GPS
            elseif(EPOCH_cPRN(1:1).EQ.' ')then
            read(cPRN(2:3),'(I2)') EPOCH_iPRN
c           Galieo
            elseif(EPOCH_cPRN(1:1).EQ.'E')then
            read(cPRN(2:3),'(I2)') EPOCH_iPRN
            EPOCH_iPRN   = EPOCH_iPRN + 100
c           Glonass
            elseif(EPOCH_cPRN(1:1).EQ.'R')then
            read(cPRN(2:3),'(I2)') EPOCH_iPRN
            EPOCH_iPRN   = EPOCH_iPRN + 200
c           SBAS
            elseif(EPOCH_cPRN(1:1).EQ.'S')then
            read(cPRN(2:3),'(I2)') EPOCH_iPRN
            EPOCH_iPRN   = EPOCH_iPRN + 300
            endif
c
            iPRN = EPOCH_iPRN
c
c           record number for iPRN
            NREC_iPRN(iPRN) = NREC_iPRN(iPRN) + 1
c
c           ambiguity number
            if(EPOCH_LLI(idx_L1).EQ.1)then
               iamb = iamb + 1
               NAMB_iPRN(iPRN) = NAMB_iPRN(iPRN) + 1
            endif
c
            if(EPOCH_LLI(idx_L1).NE.9)then
c
               tmp_nsat = tmp_nsat + 1
c
               do k=1, nobs_typ(1)
               tmp_OBS (tmp_nsat,k) = EPOCH_OBS(k)
               tmp_LLi (tmp_nsat,k) = EPOCH_LLI(k)
               tmp_SNR (tmp_nsat,k) = EPOCH_SNR(k)
               enddo
               tmp_iPRN(tmp_nsat) = iPRN
c
            endif
c
         enddo
c
c        GOOD observable must .ge. 5
c
         if(tmp_nsat.ge.5)then
c
c           read one by one
c
            do i=1,tmp_nsat
c
               iPRN = tmp_iPRN(i)
c
               nrec_iPRN(iPRN) = nrec_iPRN(iPRN) + 1
c
c              record number increment
               irec = irec + 1
c
c++            write HPRNX into LVL1 format
c
               write(111, fmt=2000)
     +               irec, 
     +               EPOCH,       tmp_iPRN(i), 
     &              (tmp_OBS(i,k),tmp_LLI (i,k),tmp_SNR(i,k),
     &                                          k=1,nobs_typ(1))
c
 2000          format(I6, F18.7, (X,I3), 12(F14.3,I1,I1))
c
            enddo
c
            iepo = iepo + 1
c
c           EPOCH time for common
c
            if(iEPO.EQ.1)then
               TIME_SPAN(1) = EPOCH
            endif
            HPX_EPO(iepo) = EPOCH
c
            NSAT_iEPO(iepo) = tmp_nsat
c
            do i=1, tmp_nsat
            iPRN_iEPO(iepo,i) = tmp_iPRN(i)
            enddo
c
         endif
c
         tmp_nsat = 0
c
c        read a new observation data block
c
         goto 100
c
      elseif(rnxvsn.EQ.3.00)then
         write(*,*) 'SmartPPP/hpx2itr.f'
         write(*,*) '  Please make extension for RINEX 3.00'
         stop
      endif
c
 200  continue
c
      nrec = irec
      nepo = iepo
      namb = iamb
c
      TIME_SPAN(2) = EPOCH
c
      write(*,*)           'number of epochs:'
      write(*,'(x,I6)')     nepo
c
      write(*,*)           'number of records:'
      write(*,'(x,I6)')     nrec
c
      write(*,*)           'number of ambiguities:'
      write(*,'(x,I6)')     namb
c
      write(*,*)           'The first EPOCH: '
      write(*,'(x,F14.3)')  TIME_SPAN(1)
c
      write(*,*)           'The last  EPOCH: '
      write(*,'(x,F14.3)')  TIME_SPAN(2)
c
c++   satellite PRNs 
c   
      isat = 0
      do iPRN=1, MAX_PRN
         if(NREC_iPRN(iPRN).gt.0)then
            isat            = isat + 1
            iSAT_iPRN(iPRN) = isat
            iPRN_iSAT(isat) = iPRN
            NREC_iSAT(isat) = NREC_iPRN(iPRN)
            NAMB_iSAT(isat) = NAMB_iPRN(iPRN)
         endif
      enddo
c
c++   satellite number
c
      nsat = isat
c
      if(nsat.gt.40)then
         write(*,*) 'SmartPPP/hpx2itr'
         write(*,*) '  satellite number are more than', nsat
         write(*,*) '  please modify the MAX_PRN in include/rinex.h'
         stop
      endif
c
      do i=1, nsat
c
         iPRN = iPRN_iSAT(i)
c        GPS
         if(iPRN.gt.0.and.iPRN.lt.100)then
c
            write(cPRN_iSAT(i),'(A,I2.2)') 'G',iPRN
c
         else
c
            write(*,*) 'SmartPPP/hpx2itr'
            write(*,*) 'Please modify for RINEX 3.0'
            stop
c
         endif
c
      enddo
c
      return
c
      end
