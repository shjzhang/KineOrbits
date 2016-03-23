c
c  subroutine rphxobs
c
      subroutine itr2hpx(rnxvsn, 
     &                   nsat_sys,csat_sys,nobs_typ,cobs_typ,nobs, 
     &                   lvl2_PNTR,lvl2_Time,lvl2_iPRN,lvl2_OBS,
     &                   lvl2_LLI, lvl2_SNR,lvl2_Nw,lvl2_Sw)   
c
c=======================================================================
c     ****f* qualicontr/itr2hpx
c
c   FUNCTION   
c   
c     write lvl2 format observation data with outlier and cycleslip
c     flags into lvl1 format
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
c     lvl2_PNTR  integer        record pointer in lvl2. 
c     lvl2_Time  real*8         time in seconds past J2000.0
c     lvl2_iPRN  integer        iPRN for satellite, 
c                               if sat.system = GPS,     iPRN = PRN
c                               if sat.system = Galieo,  iPRN = PRN+100
c                               if sat.system = Glonass, iPRN = PRN+200
c                               if sat.system = SBAS,    iPRN = PRN+300
c   
c     lvl2_OBS                  observations
c     lvl2_LLI                  lli
c     lvl2_SNR                  signal to noise 
c     lvl2_Nw                   widelane ambiguity
c     lvl2_Sw                   widelane ambiguity sigma
c
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: itr2hpx.f,v 1.0 2009/07/10 $
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
      integer       nobs
      integer       lvl2_PNTR(MAX_OBS_REC)
      real*8        lvl2_Time(MAX_OBS_REC)
      integer       lvl2_iPRN(MAX_OBS_REC)
c
      real*8        lvl2_OBS(MAX_OBS_REC, MAX_OBS_TYP)
      integer       lvl2_LLI(MAX_OBS_REC, MAX_OBS_TYP)
      integer       lvl2_SNR(MAX_OBS_REC, MAX_OBS_TYP)
c
      real*8        lvl2_Nw (MAX_OBS_REC)
      real*8        lvl2_Sw (MAX_OBS_REC)
c
c     local
c
      real*8        lvl1_Time(MAX_OBS_REC)
      integer       lvl1_iPRN(MAX_OBS_REC)
      integer       lvl1_PNTR(MAX_OBS_REC)
c
      real*8        lvl1_OBS(MAX_OBS_REC, MAX_OBS_TYP)
      integer       lvl1_LLI(MAX_OBS_REC, MAX_OBS_TYP)
      integer       lvl1_SNR(MAX_OBS_REC, MAX_OBS_TYP)
c
      integer       lvl1_rec, lvl2_rec
c
      real*8        tmp_time
      real*8        tmp_time_fore
c
      integer       tmp_flag     (MAX_SAT_NUM)
      integer       tmp_flag_fore(MAX_SAT_NUM)
c
      character*3   tmp_cPRN(MAX_SAT_NUM)
      integer       tmp_iPRN(MAX_SAT_NUM)
c
      real*8        tmp_OBS(MAX_SAT_NUM, MAX_OBS_TYP)
      integer       tmp_LLI(MAX_SAT_NUM, MAX_OBS_TYP)
      integer       tmp_SNR(MAX_SAT_NUM, MAX_OBS_TYP)
c
      integer       nsat_time
      integer       nsat_good
c
      integer       idx_L1, idx_L2
c
      logical       more 
c
c     loop
c
      integer       irec, iPRN, nrec
      integer       i, j, k
c
      do i=1, MAX_OBS_REC
         do k=1, MAX_OBS_TYP
         lvl1_OBS(i,k) = 0.0d0
         lvl1_LLI(i,k) = 0
         lvl1_SNR(i,k) = 0
         enddo
      enddo
c
      do i=1, MAX_OBS_REC
         lvl1_PNTR(i) = 0
         lvl1_Time(i) = 0.0d0
         lvl1_iPRN(i) = 0
      enddo
c
      do i=1, MAX_SAT_NUM
         tmp_flag     (i) = 0
         tmp_flag_fore(i) = 0
      enddo
c
      do i=1, MAX_SAT_NUM
         tmp_cPRN(i) = ' '
      enddo
c
      do i=1, MAX_SAT_NUM
         do k=1, MAX_OBS_TYP
         tmp_OBS(i,k) = 0.0d0
         tmp_LLi(i,k) = 0
         tmp_SNR(i,k) = 0
         enddo
      enddo
c
c     calculate the position of lvl2 record in the lvl1 record
c
      do irec=1, nobs
         lvl1_rec           = lvl2_PNTR(irec)
         lvl1_PNTR(lvl1_rec)= irec
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
c     rinex version ??
c
      if(rnxvsn.lt.3.00)then
c
c     write lvl1 observation data
c     ===========================
c
      write(*,*) 'EPOCHS with good satellite number less than 5:'
c
      do irec=1,nobs
c
         lvl2_rec = lvl1_PNTR(irec)
c
         if(nobs_typ(1).LE.11)then
c
            write(411, fmt=1000) 
     &      lvl2_PNTR(lvl2_rec),  lvl2_Time(lvl2_rec),  
     &      lvl2_iPRN(lvl2_rec), 
     &     (lvl2_OBS (lvl2_rec,k),lvl2_LLI (lvl2_rec,k),
     &      lvl2_SNR (lvl2_rec,k),k=1,nobs_typ(1))
c
 1000       format(I6, F18.7, (X,I3), 11(F14.3,I1,I1))
c
         else
c
            write(*,*) 'qualicontr/itr2hpx.f'  
            write(*,*) 'please modify to more observations'
            stop
c
         endif
c
      enddo
c
      endif
c
c     rewind
c     ======
      rewind(112)
c
c     write hprnx header 
c     ==================
c
      call whpxhdr(rnxvsn, nsat_sys, csat_sys, nobs_typ, cobs_typ)
c
c     rewind
c     ======
      rewind(411)
c
c     lvl1 to hprnx
c     =============
c
      irec = 0
      more = .true.
      nsat_time = 0
      nsat_good = 0
      tmp_time  = 0.0d0
      tmp_time_fore = 0.0d0
c
 100  continue
c
      irec = irec + 1
c
      read(411, fmt=1000, end=300)
     &     lvl1_PNTR(irec), lvl1_Time(irec),  
     &     lvl1_iPRN(irec),(lvl1_OBS (irec,k), lvl1_LLI (irec,k),
     &                      lvl1_SNR (irec,k), k=1,nobs_typ(1))
c
c     determine the same time sat accordig to the lvl1_Time
c
      tmp_time = lvl1_Time(irec)
c
c     read the fore satellite observables 
c
 110  continue
c
c     write observation data block
c
      if(tmp_time.gt.tmp_time_fore.and.nsat_time.gt.0)then
c
         goto 200
c
      endif
c
c     store observables of the same time into corresponding temporary
c     variables to be written together
c
      nsat_time            = nsat_time + 1
c
      tmp_time             = lvl1_Time(irec)
      tmp_iPRN(nsat_time)  = lvl1_iPRN(irec)
      iPRN                 = lvl1_iPRN(irec)
c
      if(    iPRN.GE.000.and.iPRN.LT.100)then
         write(tmp_cPRN(nsat_time),'(I3)') iPRN
         if(iPRN.lt.10)then
         tmp_cPRN(nsat_time)(1:1) = 'G'
         tmp_cPRN(nsat_time)(2:2) = '0'
         endif
         tmp_cPRN(nsat_time)(1:1) = 'G'
      elseif(iPRN.GE.100.and.iPRN.LT.200)then
         iPRN = iPRN - 100
         write(tmp_cPRN(nsat_time),'(I3)') iPRN
         if(iPRN.lt.10)then
         tmp_cPRN(nsat_time)(1:1) = 'E'
         tmp_cPRN(nsat_time)(2:2) = '0'
         endif
         tmp_cPRN(nsat_time)(1:1) = 'E'
      elseif(iPRN.GE.200.and.iPRN.LT.300)then
         iPRN = iPRN - 200
         write(tmp_cPRN(nsat_time),'(I3)') iPRN
         if(iPRN.lt.10)then
         tmp_cPRN(nsat_time)(1:1) = 'R'
         tmp_cPRN(nsat_time)(2:2) = '0'
         endif
         tmp_cPRN(nsat_time)(1:1) = 'R'
      else
         write(*,*) 'qualicontr/itr2hpx'
         write(*,*) '  More systems?'
         stop
      endif
c
      tmp_flag(nsat_time)  = lvl1_LLI (irec,idx_L1)
c
      if(tmp_flag_fore(iPRN).EQ.1.and.tmp_flag(nsat_time).NE.9)then
         tmp_flag     (nsat_time) = tmp_flag_fore(iPRN)
         tmp_flag_fore(iPRN)      = 0
      endif
c
      if(tmp_flag(nsat_time).NE.9)then
         nsat_good = nsat_good + 1
      endif
c
      do k=1, nobs_typ(1)
         tmp_OBS(nsat_time, k)= lvl1_OBS(irec, k)
         tmp_LLI(nsat_time, k)= lvl1_LLI(irec, k)
         tmp_SNR(nsat_time, k)= lvl1_SNR(irec, k)
      enddo
c
      tmp_time_fore = tmp_time
c
c     goto read a new line
c
      goto 100
c
c     output the observables at the same epoch time
c
*******************************************************************************
c
  200 continue   
c
c     write observations block if satellite number is .ge. 4
c
      if(    nsat_good.lt.5)then
c
c        write observables block header(time and satellie number)
c
         write(*,'(F18.7,x,I3)') tmp_time_fore,nsat_good
c
c        write observables block header(time and satellie number)
c
         write(102, fmt=2000) tmp_time_fore, nsat_time
c
         do i=1, nsat_time
c
c           satellite PRN number 
c
            iPRN = tmp_iPRN(i)
c
c           store the cycle-slips from the epoch, at which the observed
c           satellite number is less than 5, then the cycle-slip will 
c           be passed to the observable of the epoch with observed
c           satellite number is greater than 5. 
c
            if(tmp_flag(i).EQ.1)then
c
               tmp_flag_fore(iPRN) = tmp_flag(i)
c
            endif
c
            tmp_LLI(i,idx_L1) = 9
c           
            write(102, fmt=3000) 
     &      tmp_cPRN(i),(tmp_OBS(i,k),tmp_LLI(i,k),
     &                                tmp_SNR(i,k), k=1,nobs_typ(1))
c
         enddo
c
      elseif(nsat_good.ge.5)then
c
c        write observables block header(time and satellie number)
c
         write(102, fmt=2000) tmp_time_fore, nsat_time
c
c        write observables 
c
         do i=1, nsat_time
c
            tmp_LLI(i,idx_L1) = tmp_flag(i)
c           
            write(102, fmt=3000) 
     &            tmp_cPRN(i),(tmp_OBS(i,k),tmp_LLI(i,k),
     &                         tmp_SNR(i,k),k=1,nobs_typ(1))
c
         enddo
c
 2000    format( F18.7, I4)
 3000    format((X,A3),11(F14.3, I1, I1))
c
      endif
c
      nsat_time = 0
      nsat_good = 0
c
c     end of file
c
      if(.not.more)then
c
         goto 400
c
      endif
c
c     goto read the current satellite observables(tmp_time)
c
      goto 110
c
c     flag the last epoch and goto output the last epoch observables
c
*******************************************************************************
c
  300 continue
c
      more = .false. 
c
c     goto output the last epoch observables
      goto 200
c
  400 continue
c
      return
c
      end
