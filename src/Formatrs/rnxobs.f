c
c   subroutine rnxobs
c
      subroutine rnxobs(rnxvsn,nsat_sys,csat_sys,
     &                         nobs_typ,cobs_typ)
c
c=======================================================================
c     ****f* fmttrs/rnxobs
c
c   FUNCTION   
c   
c     According to the input RINEX header information, read the RINEX
c     observation data, and then transform it to the internal HPRNX
c     format, which is convenient for the data processing later.
c
C   ARGUMENTS
C   
C     rnxvsn        real       (i) returned rinex rnxvsn from rinex
C                                  header
c     csat_sys(*)   characeter (i) returned satellite systems in rinex 
c                                  file
c     nobs_typ(*)   integer    (i) observation types number for existed 
c                                  satellite systems.
c     cobs_typ(*,*) character  (i) observation types for existed 
c                                  satellite systems.
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c   History
c
c     2009/12/25
c
c+++++
c           jump the following lines 2009/12/25
c
c           write(102,'(A80)') line
c+++++
c
c     ***
c
C     $Id: rnxobs.f,v 1.0 2009/07/04 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
      include      '../../include/rinex.conf.h'
c
c     external funtion declaration
c
      real*8        cal2sec
c
c     variable for rinex header
c
c     RINEX VERSION / TYPE
c
      real*8        rnxvsn
      integer       nsat_sys
      character*3   csat_sys(MAX_SAT_SYS)
      integer       nobs_typ(MAX_SAT_SYS)
      character*3   cobs_typ(MAX_SAT_SYS, MAX_OBS_TYP)
c
c     local variable
c
c     RINEX data block
      character*80  line
      character*1   REC_ID
      integer       year, month, day, hour, min, sec_int
      real*8        sec
      real*8        sec_dec, sec_GPS, sec_J2000, sec_offset
      integer       EPOCH_flag
      integer       EPOCH_nsat
c
      integer       EPOCH_iPRN(MAX_PRN)
      character*3   EPOCH_cPRN(MAX_PRN)
c
      integer       ncline
c
      integer       EPOCH_LLI(MAX_OBS_TYP), EPOCH_SNR(MAX_OBS_TYP)
      integer             LLI(MAX_OBS_TYP),       SNR(MAX_OBS_TYP)
      real*8        EPOCH_OBS(MAX_OBS_TYP)
      real*8              OBS(MAX_OBS_REC, MAX_OBS_TYP)
c
      integer       pntr_isat(MAX_PRN)
      integer       nrec_iPRN(MAX_PRN)
      integer       nrec_isat(MAX_PRN)
c
      integer       iPRN,jPRN
      character*3   cPRN
c
      integer       i, j, k
      integer       nepo, nrec, isat, nsat, ios
c
c     initialization
c
      do i=1, MAX_PRN
         pntr_isat(i) = 0
         nrec_iPRN(i) = 0
         EPOCH_iPRN(i) = 0
         EPOCH_cPRN(i) = ' '
      enddo
c
      do i=1, MAX_OBS_TYP
         EPOCH_OBS(i) = 0.0d0
         EPOCH_LLI(i) = 0
         EPOCH_SNR(i) = 0
      enddo
c
      REC_ID = '>'
c
      nrec = 0
      nepo = 0
c
 200  continue
c
c     read data record block
c     ======================
c
      read(101, '(A80)', end=444, iostat=ios) line
c
      nepo = nepo + 1
c
c**   different treatment for different RINEX rnxvsn
c
      if(rnxvsn.lt.2.00 .and. rnxvsn.gt.3.00)then
         write(*,*) 'rnxtrs/rnxobs.f'
         write(*,*) 'not support this rinex version'
         stop
      endif
c   
c**   read observation header information   
c   
      read(line( 2: 3), '(I2   )') year
      read(line( 5: 6), '(I2   )') month
      read(line( 8: 9), '(I2   )') day            
      read(line(11:12), '(I2   )') hour           
      read(line(14:15), '(I2   )') min            
      read(line(16:26), '(F11.7)') sec            
      read(line(29:29), '(I1   )') EPOCH_flag     
      read(line(30:32), '(I3   )') EPOCH_nsat     
c
      sec_int  = DINT(sec)
      sec_dec  = sec - DINT(sec)

c  
c**   deal with the EPOCH flags 
c
c     EPOCH_flag 0 : OK
c     EPOCH_flag 6 : cycle slip
      
      sec_J2000 = 0.0d0
c
      if(EPOCH_flag.EQ.0.or.EPOCH_flag.EQ.1.or.EPOCH_flag.EQ.6)then
c
         sec_J2000 = cal2sec(year,month,day,hour,min,sec_int,sec_dec)
c
c        satellite number greater than 12 ? 
c
         if(    EPOCH_nsat.LE.12)then
c
            do i=1, EPOCH_nsat
c
c              for GPS satellite system
               if(    line(32+3*(i-1)+1:32+3*(i-1)+1).EQ.'G')then
c
c                 read PRNS for GPS system 2.10 
                  cPRN            = line(32+3*(i-1)+1:32+3*(i-1)+3)
                  EPOCH_cPRN(i)   = cPRN
c
                  read(cPRN(2:3),'(I2.2)') EPOCH_iPRN(i)
c
               elseif(line(32+3*(i-1)+1:32+3*(i-1)+1).EQ.' ')then
c
c                 read PRNS for GPS system with rnxvsn 2.20
                  cPRN            = line(32+3*(i-1)+1:32+3*(i-1)+3)
                  cPRN(1:1)       = 'G'
                  EPOCH_cPRN(i)   = cPRN
c
                  read(cPRN(2:3),'(I2.2)') EPOCH_iPRN(i)
c
               elseif(line(32+3*(i-1)+1:32+3*(i-1)+1).EQ.'E')then
c
                  cPRN            = line(32+3*(i-1)+1:32+3*(i-1)+3)
                  EPOCH_cPRN(i)   = cPRN
c
                  read(cPRN(2:3),'(I2.2)') EPOCH_iPRN(i)
                  EPOCH_iPRN(i)   = EPOCH_iPRN(i) + 100
c
               elseif(line(32+3*(i-1)+1:32+3*(i-1)+1).EQ.'R')then
c
                  cPRN            = line(32+3*(i-1)+1:32+3*(i-1)+3)
                  EPOCH_cPRN(i)   = cPRN
c
                  read(cPRN(2:3),'(I2.2)') EPOCH_iPRN(i)
                  EPOCH_iPRN(i)   = EPOCH_iPRN(i) + 200
c
               elseif(line(32+3*(i-1)+1:32+3*(i-1)+1).EQ.'S')then
c
                  cPRN            = line(32+3*(i-1)+1:32+3*(i-1)+3)
                  EPOCH_cPRN(i)   = cPRN
c
                  read(cPRN(2:3),'(I2.2)') EPOCH_iPRN(i)
                  EPOCH_iPRN(i)   = EPOCH_iPRN(i) + 300
c
               endif
c
            enddo
c
c        12~14
c
         elseif(EPOCH_nsat.GT.12.and.EPOCH_nsat.le.24)then
c
            do i=1,12
c
c              for the GPS satellite system
               if(    line(32+3*(i-1)+1:32+3*(i-1)+1).EQ.'G')then
c
c                 read PRNS for GPS system
                  cPRN            = line(32+3*(i-1)+1:32+3*(i-1)+3)
                  EPOCH_cPRN(i)   = cPRN
c
                  read(cPRN(2:3),'(I2.2)') EPOCH_iPRN(i)
c
               elseif(line(32+3*(i-1)+1:32+3*(i-1)+1).EQ.'E')then
c
c                 read PRNS for GALIEO system
                  cPRN            = line(32+3*(i-1)+1:32+3*(i-1)+3)
                  EPOCH_cPRN(i)   = cPRN
c
                  read(cPRN(2:3),'(I2.2)') EPOCH_iPRN(i)
                  EPOCH_iPRN(i)   = EPOCH_iPRN(i) + 100
c
               elseif(line(32+3*(i-1)+1:32+3*(i-1)+1).EQ.'R')then
c
c                 read PRNS for GLONASS system
                  cPRN            = line(32+3*(i-1)+1:32+3*(i-1)+3)
                  EPOCH_cPRN(i)   = cPRN
c
                  read(cPRN(2:3),'(I2.2)') EPOCH_iPRN(i)
                  EPOCH_iPRN(i)   = EPOCH_iPRN(i) + 200
c
               elseif(line(32+3*(i-1)+1:32+3*(i-1)+1).EQ.'S')then
c
c                 read PRNS for SBAS system
                  cPRN            = line(32+3*(i-1)+1:32+3*(i-1)+3)
                  EPOCH_cPRN(i)   = cPRN
c
                  read(cPRN(2:3),'(I2.2)') EPOCH_iPRN(i)
                  EPOCH_iPRN(i)   = EPOCH_iPRN(i) + 300
c
               endif
c
            enddo
c
c           read a cotinuation line for more than 12 satellites
c
            read(101, '(A80)', end=444, iostat=ios) line
c
            do i=13, EPOCH_nsat
c
c              for GPS satellite system
               if(    line(32+3*(i-13)+1:32+3*(i-13)+1).EQ.'G')then
c
c                 read PRNS for GPS system 2.10 
                  cPRN            = line(32+3*(i-13)+1:32+3*(i-13)+3)
                  EPOCH_cPRN(i)   = cPRN
c
                  read(cPRN(2:3),'(I2.2)') EPOCH_iPRN(i)
c
               elseif(line(32+3*(i-13)+1:32+3*(i-13)+1).EQ.' ')then
c
c                 read PRNS for GPS system with rnxvsn 2.20
                  cPRN            = line(32+3*(i-13)+1:32+3*(i-13)+3)
                  cPRN(1:1)       = 'G'
                  EPOCH_cPRN(i)   = cPRN
c
                  read(cPRN(2:3),'(I2.2)') EPOCH_iPRN(i)
c
               elseif(line(32+3*(i-13)+1:32+3*(i-13)+1).EQ.'E')then
c
                  cPRN            = line(32+3*(i-13)+1:32+3*(i-13)+3)
                  EPOCH_cPRN(i)   = cPRN
c
                  read(cPRN(2:3),'(I2.2)') EPOCH_iPRN(i)
                  EPOCH_iPRN(i)   = EPOCH_iPRN(i) + 100
c
               elseif(line(32+3*(i-13)+1:32+3*(i-13)+1).EQ.'R')then
c
                  cPRN            = line(32+3*(i-13)+1:32+3*(i-13)+3)
                  EPOCH_cPRN(i)   = cPRN
c
                  read(cPRN(2:3),'(I2.2)') EPOCH_iPRN(i)
                  EPOCH_iPRN(i)   = EPOCH_iPRN(i) + 200
c
               elseif(line(32+3*(i-13)+1:32+3*(i-13)+1).EQ.'S')then
c
                  cPRN            = line(32+3*(i-13)+1:32+3*(i-13)+3)
                  EPOCH_cPRN(i)   = cPRN
c
                  read(cPRN(2:3),'(I2.2)') EPOCH_iPRN(i)
                  EPOCH_iPRN(i)   = EPOCH_iPRN(i) + 300
c
               endif
c
            enddo
c
         elseif(EPOCH_nsat.GT.24.and.EPOCH_nsat.le.36)then
c
c           more than 24 satellites? 
c   
            write(*,*) "format/rnxobs.f"
            write(*,*) "This program have to be modified to process"
            write(*,*) 'more than 24 satellites at the same epoch!'
            stop 
c
         endif
c
c        modify for GOCE, the receivers' clock have offset
c   
         
c        sec_offset = 0.0d0
c        read(line(69:80), '(F12.9)') sec_offset            

c        write(*,'(F20.9)') sec_J2000, sec_offset

c        sec_J2000 = sec_J2000 - sec_offset

c        write(*,'(F20.9)') sec_J2000, sec_offset
c
c**      write data block header into hprnx file
c
         write(102,fmt=5000) 
     &         REC_ID, sec_J2000, EPOCH_flag, EPOCH_nsat
c
 5000    format(A1,F20.9,(2X,I1),(1X,I4))
c
c        read data for the PRN reading from the data block header
c        ========================================================
c
         do i=1, EPOCH_nsat
c
c           read observables according to different nobs_type
c
c           one line or two line?
            if(nobs_typ(1).le.5)then
c    
c              read data block record    
               read(101, '(A80)', end=444, iostat=ios) line
c
c              read observables
               read(line, fmt=1000) 
     &             (EPOCH_OBS(k),EPOCH_LLI(k),EPOCH_SNR(k), 
     &                                                  k=1,nobs_typ(1))
 1000          format(5(f14.3, 2i1))
c
               nrec = nrec + 1
c
c           more than 5 observables ?
            elseif(nobs_typ(1).gt.5 .and.nobs_typ(1).le.10)then
c    
c              read data block record    
               read(101, '(A80)', end=444, iostat=ios) line
c
c              read observables
               read(line, fmt=2000) 
     &             (EPOCH_OBS(k), EPOCH_LLI(k),EPOCH_SNR(k), k=1,5)
c
 2000          format(5(f14.3, 2i1))
c    
c              read continuation line     
               read(101, '(A80)', end=444, iostat=ios) line
c
c              read observables
               read(line, fmt=3000) 
     &             (EPOCH_OBS(k),EPOCH_LLI(k),EPOCH_SNR(k), 
     &                                                  k=6,nobs_typ(1))
c
 3000          format(5(f14.3, 2i1))
c
               nrec = nrec + 1
c
c           more than 10 observables? multiple GNSS system?
            elseif(nobs_typ(1).gt.10.and.nobs_typ(1).le.15)then
c
               write(*,*) "fmttrs/rnxobs.f"
               write(*,*) '  This program does not process obeservation'
               write(*,*) '  Types more than 10 !!!'
               stop
c
            endif
c
c           PRN for this loop i
c
            iPRN = EPOCH_iPRN(i)
            cPRN = EPOCH_cPRN(i)
c
            read (cPRN(2:3),'(I2.2)') jPRN
            write(cPRN(2:3),'(I2.2)') jPRN
c
c           pass the EPOCH flag = 6, i.e. cycle slip record will not be
c           written into internal file. it means we don't trust the flags
c         
            if(EPOCH_flag.eq.6)then
c         
               goto 300
c         
            endif
c
c           accumulate the total numbers for the iPRN 
c
            nrec_iPRN(iPRN) = nrec_iPRN(iPRN) + 1    
c
c           write record into HPRNX data format file 
c
            write(102,fmt=4000) 
     &            cPRN, (EPOCH_OBS(k), 
     &                   EPOCH_LLI(k), EPOCH_SNR(k), k=1, nobs_typ(1))
c
 4000       format((A3), 10(F14.3,I1,I1))
c
c           end of reading an EPOCH
c
 300        continue
c
         enddo
c
c     EPOCH_flag 2,3,4,5: header information follows 
c
      else
c
         ncline = EPOCH_nsat
c
         do i=1, ncline
c
            read (101, '(A80)', end=444, iostat=ios) line
c
c           jump the following lines 2009/12/25
c
c           write(102,'(A80)') line
c
         enddo
c
      endif
c
c     read a new data block
c
      goto 200
c
c     End of reading observation data record
c     ======================================
c
 444  continue
c
c     write PRNs of Satellite into lvl1A.header file for
c     usage in reading lvl1A file
c   
      isat = 0
      do iPRN=1, MAX_PRN
         if(nrec_iPRN(iPRN).gt.0)then
            isat = isat + 1
            pntr_isat(isat) = iPRN
            nrec_isat(isat) = nrec_iPRN(iPRN)
         endif
      enddo
c
c     total satellite number
c
      nsat = isat
c
      if(nsat.gt.40)then
c
         write(*,*) 'fmttrs/rnxobs.f'
         write(*,*) '  Satellite number more than 40!!'
         write(*,*) '  Program does not process this instance'
         stop
c
      endif

      return
c
      end
