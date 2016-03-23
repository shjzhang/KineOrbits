C
c   subroutine res2itr
c
      subroutine res2itr()
c
c=======================================================================
c     ****f* qualicontr/res2itr
c
c   FUNCTION   
c   
c     transform the residual data file from HPRNX format to 
c     internal used formats, which are: 
c     
c     1) HPRNX Level1 format:
c
c        irec, EPOCH, cPRN, res
c         *
c
c     2) HPRNX Level2 format:
c
c        irec, EPOCH, cPRN, res
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
c     NONE
c
c   COPYRIGHT
c
c     Copyright(c) 2006-          Shoujian Zhang,
c                                 School of Geodesy and Geomatics,
c                                 Wuhan University.
c     ***
c
C     $Id: res2itr.f,v 1.0 2009/07/10 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
c
c     input/output
c
c     local 
c
      integer       isat
      integer       iepo, nepo, irec, nrec
      integer       i, j, k
      integer       ios
      integer       idx_L1,idx_L2
c 
      integer       nsat
      integer       pntr_isat(MAX_SAT_NUM)
      integer       nrec_isat(MAX_SAT_NUM)
c
c     hprnx header file
c
      integer       nrec_iPRN(MAX_PRN)
c
c     hprnx observation data block
c
      real*8        EPOCH
      integer       EPOCH_nsat
      integer       iPRN
      character*3   cPRN
      integer       EPOCH_iPRN
      character*3   EPOCH_cPRN
      real*8        EPOCH_rRES
c
c     LVL1
c
      integer       LVL1_PNTR(MAX_OBS_REC)
      real*8        LVL1_Time(MAX_OBS_REC)
      integer       LVL1_iPRN(MAX_OBS_REC)
      integer       LVL1_nsat(MAX_OBS_REC)
c
      real*8        LVL1_rRES(MAX_OBS_REC)
c
      integer       tmp_iPRN(MAX_SAT_NUM)
      real*8        tmp_rres (MAX_SAT_NUM)
      integer       tmp_nsat
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
      do i=1, MAX_SAT_NUM
         tmp_iPRN(i) = 0
      enddo
c
      do i=1, MAX_SAT_NUM
         tmp_rres(i) = 0.0d0
      enddo
c
c     HPRNX to hprnx_level1
c     =====================
c
      irec = 0    
      nrec = 0
      iepo = 0
      nepo = 0
      tmp_nsat = 0
c
 100  continue
c
c++   read observ data block header 
c
c!!!! RINEX 3.00 format, not used recently
c
      read(201, *, end=200, iostat=ios)
     &     EPOCH, EPOCH_nsat
c
c     EPOCH number increment
      iepo = iepo + 1
c
      do i=1, EPOCH_nsat
c
c        read a record
         read(201, fmt=1000) 
     +        EPOCH_cPRN, EPOCH_rRES
c
 1000    format((A3),F8.3)
c
         cPRN = EPOCH_cPRN
c
         if(    EPOCH_cPRN(1:1).EQ.'G'.or.EPOCH_cPRN(1:1).EQ.' ')then
c
         read(cPRN(2:3),'(I2)') EPOCH_iPRN
c
         elseif(EPOCH_cPRN(1:1).EQ.'E')then
c
         read(cPRN(2:3),'(I2)') EPOCH_iPRN
c
         EPOCH_iPRN   = EPOCH_iPRN + 100
c
         elseif(EPOCH_cPRN(1:1).EQ.'R')then
c
         read(cPRN(2:3),'(I2)') EPOCH_iPRN
c
         EPOCH_iPRN   = EPOCH_iPRN + 200
c
         elseif(EPOCH_cPRN(1:1).EQ.'S')then
c
         read(cPRN(2:3),'(I2)') EPOCH_iPRN
c
         EPOCH_iPRN   = EPOCH_iPRN + 300
c
         endif

         iPRN               = EPOCH_iPRN
         tmp_nsat           = tmp_nsat + 1
         tmp_iPRN(tmp_nsat) = iPRN
         tmp_rres(tmp_nsat) = EPOCH_rRES
c
      enddo
c
c     GOOD observable must .ge. 5
c
      if(tmp_nsat.ge.5)then
c
c     read one by one
c
      do i=1,tmp_nsat
c
         iPRN = tmp_iPRN(i)
c
         nrec_iPRN(iPRN) = nrec_iPRN(iPRN) + 1
c
c        record number increment
         irec = irec + 1
c
c++      write HPRNX into LVL1 format
c
         write(121, fmt=2000)
     +         irec, tmp_nsat,
     +         EPOCH,tmp_iPRN(i),tmp_rres(i)
c
 2000    format(2I6, F18.7, (X,I3), F8.3)
c
c++      save LVL1 observation data into array, which
c++      is used to create LVL2 format data file.
c
         LVL1_iPRN(irec)                 = iPRN
         LVL1_nsat(irec)                 = tmp_nsat
         LVL1_PNTR(irec)                 = irec
         LVL1_Time(irec)                 = EPOCH
         LVL1_rRES(irec)                 = tmp_rres(i)
c
         LVL2_PNTR(iPRN,nrec_iPRN(iPRN)) = irec
c
      enddo
c
      endif
c
      tmp_nsat = 0
c
c     read a new observation data block
c
      goto 100
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
         write(*,*) 'qualicontr/res2itr'
         write(*,*) '  satellite number are more than', nsat
         write(*,*) '  please modify the MAX_PRN in include/rinex.h'
         stop
      endif
c
c     hprnx_level1 to hprnx_level2
c     ============================
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
            write(221, fmt=3000)
     &      LVL1_PNTR(irec),LVL1_nsat(irec),LVL1_Time(irec),
     &      LVL1_iPRN(irec),LVL1_rRES(irec)
c
 3000       format(2I6, F18.7, (X,I3), F8.3)
c
         enddo
c
      enddo
c
      return
c
      end
