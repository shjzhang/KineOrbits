c
c   program ambfix_UHD
c
      program ambfix_UHD
c
c=======================================================================
c     ****f* ambfix_UHD/ambfix_UHD
c
c   FUNCTION   
c   
c     Estimation of the UHD(uncalibrated phase delays with PPP)
c   
c   Notes
c
c     1. Ge M.           2007, Resolution of GPS carrier-phase
c                              ambiguities in Precise Point
c                              Positioning(PPP) with daily observations
c
c   USAGE
c
c     ambfix_UHD -iamb Argument -ouhd Argument 
c
C   ARGUMENTS
C
C     -iamb                    input  floating ambiguities with PPP(LSQ)
c     -ouhd                    output uncalibrated hardware delays
c
c   COPYRIGHT
c
c     Copyright(c) 2006-       Shoujian Zhang,
c                              School of Geodesy and Geomatics,
c                              Wuhan University.
c     ***
c
C     $Id: ambfix_UHD.f,v 1.0 2009/09/12 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
      include      '../../include/rinex.conf.h'
c
      integer       MAX_ARC
      parameter    (MAX_ARC = 500)
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
      character*200 iamb_file
      character*200 ouhd_file
c
      integer       iargv
      integer       ios
c
c     amb2itr returned
c
      integer       nsat
      integer       pntr_isat(MAX_SAT_NUM)
      integer       nrec_isat(MAX_SAT_NUM)
c
      integer       iPRN
      integer       isat
      integer       NREC
c     rhpxamb
      integer       nobs
c
      integer       lvl2_PNTR(MAX_PRN,MAX_OBS_REC)
      real*8        lvl2_Time(MAX_OBS_REC)
      integer       lvl2_iPRN(MAX_OBS_REC)
c
      real*8        lvl2_Nw  (MAX_OBS_REC)
      real*8        lvl2_N3  (MAX_OBS_REC)
      integer       lvl2_flag(MAX_OBS_REC)
c
      integer       flag (MAX_OBS_REC)
      real*8        mNw  (MAX_OBS_REC) ! Nw in meters
      real*8        mN3  (MAX_OBS_REC) ! N3 in meters
      real*8        mNn  (MAX_OBS_REC)
      real*8        EPOCH(MAX_OBS_REC)
      real*8        cNw  (MAX_OBS_REC) ! Nw in cycles
      real*8        cN3  (MAX_OBS_REC) ! N3 in cycles
      real*8        cNn  (MAX_OBS_REC) ! N3 in cycles
c
      integer       iarc,narc
      integer       points(MAX_ARC)
      real*8        bwfrac(MAX_ARC)
      real*8        mean_bwfrac(MAX_ARC)
      real*8        vv_bwfrac(MAX_ARC)
      real*8        std_bwfrac(MAX_ARC)
      real*8        mean_bwfrac1
c
      real*8        bnfrac(MAX_ARC)
      real*8        mean_bnfrac(MAX_ARC)
      real*8        vv_bnfrac(MAX_ARC)
      real*8        std_bnfrac(MAX_ARC)
c
      real*8        bias
      real*8        mean_Nw(MAX_ARC)
      real*8        sigma2_Nw(MAX_ARC)
      real*8        EPOCH_Nw(MAX_ARC)
c
      real*8        mean_Nn(MAX_ARC)
      real*8        sigma2_Nn(MAX_ARC)
      real*8        EPOCH_Nn(MAX_ARC)
c
      integer       year, mon, day, hour, min, sec
      real*8        frac
      real*8        dy
c
      logical       first
      integer       n
c
c     information file
c
      character*200 line
c
c     loop
c
      integer       i, j, k
c
c     read arguments from command line
c     ================================
c
      call getargvs(nargv, argvs, stat)
c
      if(stat /=0 )then
c
         write(*,*)
         write(*,*)'Usages: ambfix -iamb f -ouhd f '
         write(*,*)
         write(*,*)'Arguments:'
         write(*,*)
         write(*,*)'  -iamb input  floating ambiguity file'
         write(*,*)'  -ouhd output UHD file'
         write(*,*)
         write(*,*)'AUTHOR'
         write(*,*)
         write(*,*)'  Programmed by Shoujian Zhang'
         write(*,*)
c
         stop
c
      endif
c
      if(nargv.gt. nopts)then
         write(*,*) 'ambfix_UHD/ambfix_UHD.f'
         write(*,*) 'input argments not correct'
         stop
      endif
c
      iargv = 1
      do while(iargv <= nargv) 
         if(    argvs(iargv)=='-iamb')then
            iargv     = iargv + 1
            iamb_file = argvs(iargv)
         elseif(argvs(iargv)=='-ouhd')then
            iargv     = iargv + 1
            ouhd_file = argvs(iargv)
         endif
         iargv = iargv + 1
      enddo   
c
c     open rinex file
c     ===============
c
      open(unit=101,file=iamb_file,status='old',iostat=ios)
c
      if(ios.ne.0)then
         write(*,*) 'ambfix_UHD/ambfix_UHD.f'
         write(*,*) 'open input floating ambiguity file error'
         stop
      endif
c
c     open hprnx file
c     ===============
c
      open(unit=102,file=ouhd_file,status='replace',iostat=ios)
c
      if(ios.ne.0)then
         write(*,*) 'ambfix_UHD/ambfix_UHD.f'
         write(*,*) 'open output UHD file error'
         stop
      endif
c
c     open internal formatted file 
c     ============================
c
      open(unit=111,file='111.txt',status='replace',iostat=ios)
c
      if(ios.NE.0)then
         write(*,*) 'ambfix_UHD/ambfix_UHD.f'
         write(*,*) '  open lvl1 files error!'
         stop
      endif
c
      open(unit=211,file='211.txt',status='replace',iostat=ios)
c
      if(ios.NE.0)then
         write(*,*) 'ambfix_UHD/ambfix_UHD.f'
         write(*,*) '  open lvl2 files error!'
         stop
      endif
c
c     amb2itr
c     =======
c
      call amb2itr(nsat, pntr_isat, nrec_isat)
c
c     read floating ambiguities from files
c
      call rhpxamb(nobs,lvl2_Time,lvl2_iPRN,lvl2_flag,lvl2_Nw,lvl2_N3)
c
c     UHD calculation for widelane and narrow line for L3 combination
c
      do isat=1,nsat
c
c     PRN number
      iPRN = pntr_isat(isat)
c
c     PRN record number
      nrec = nrec_isat(isat)
c
c     output record number for PRNs
c
      write(*  ,fmt=2000) "UHD for PRN", iPRN," is:", nrec
      write(102,fmt=2000) "UHD for PRN", iPRN," is:", nrec
c
2000  format(X,A,I3,A,I6)
c
c++   extract ambiguities for PRN
c
      call extramb(isat,nrec_isat,
     &             lvl2_Time,lvl2_flag,lvl2_Nw,lvl2_N3,
     &             EPOCH,flag,mNw,mN3)
c
c++   ionosphere-free ambiguity combination          
c
c     cNw = mNw/lam_Nw
c     cN3 = mN3/lam_N1
c
      do i=1, nrec
         cNw(i) = mNw(i)/ lam_Nw_GPS        
         cN3(i) = mN3(i)/ lam_L1_GPS
      enddo
c
c     do i=1,nrec
c        write(*,'(I3,I3,2F14.3)') iPRN,flag(i),cNw(i),cN3(i)
c     enddo
c
      do i=1,MAX_ARC
         points(i) = 0  
c
         bwfrac(i) = 0.0d0
         std_bwfrac(i) = 0.0d0
         mean_bwfrac(i) = 0.0d0
         mean_Nw(i) = 0.0d0
         sigma2_Nw(i) = 0.0d0
c
         bnfrac(i) = 0.0d0
         std_bnfrac(i) = 0.0d0
         mean_bnfrac(i) = 0.0d0
         mean_Nn(i) = 0.0d0
         sigma2_Nn(i) = 0.0d0
      enddo
c
c     estimate widelane UHD
c     =====================
c
      iarc = 0
      do i=1,nrec
         if(flag(i).EQ.1)then
            iarc = iarc + 1
            points(iarc) = 1
            mean_Nw(iarc) = cNw(i)
            sigma2_Nw(iarc) = 0.1**2
            bias = cNw(i) 
            EPOCH_Nw(iarc) = EPOCH(i)
         else
            points(iarc) = points(iarc) + 1  ! number of bw of continuous arc
            bias = cNw(i) - mean_Nw(iarc)
            if(bias.lt.3*dsqrt(sigma2_Nw(iarc)))then
               mean_Nw(iarc)  = mean_Nw(iarc)+bias/dfloat(points(iarc))
               sigma2_Nw(iarc)=(points(iarc)-2)
     &                        * sigma2_Nw(iarc)/dfloat(points(iarc)-1)
     &                        +(bias**2 )/dfloat(points(iarc))
            endif
         endif
      enddo
c
      narc = iarc
c
      do i=1,narc
         mean_bwfrac(i) = mean_Nw(i) - FLOOR(mean_Nw(i))
         std_bwfrac(i)  = dsqrt(sigma2_Nw(i))
      enddo
c
      do i=1,narc
         if(i.NE.1)then
            n = NINT( mean_bwfrac(i-1) - mean_bwfrac(i))
            if(n.NE.0)then
               mean_bwfrac(i) = (n + mean_bwfrac(i))
               mean_Nw(i)     = (n + mean_Nw(i))
            endif
         endif
      enddo
c
      mean_bwfrac1 = 0.0d0
      do i=1,narc
      mean_bwfrac1 = mean_bwfrac1 + mean_bwfrac(i)
      enddo
      mean_bwfrac1 = mean_bwfrac1/narc
c
c     estimate narrow-lane UHD
c     ========================
c
      iarc = 0
      do i=1,nrec
         if(flag(i).EQ.1)then
         iarc=iarc + 1
         endif
c
c        cNn(i)=(f1_GPS+f2_GPS)/(f1_GPS       )*cN3(i) 
c    &         -(f2_GPS       )/(f1_GPS-f2_GPS)*floor(mean_Nw(iarc))
         cNn(i)=(f1_GPS+f2_GPS)/(f1_GPS       )*cN3(i) 
     &         -(f2_GPS)/(f1_GPS-f2_GPS)*(mean_Nw(iarc))
c        cNn(i)=(f1_GPS+f2_GPS)/(f1_GPS       )*cN3(i) 
c    &         -(f2_GPS       )/(f1_GPS-f2_GPS)
c    &         *(floor(mean_Nw(iarc))+mean_bwfrac1)
      enddo
c
      iarc = 0
      do i=1,nrec
         if(flag(i).EQ.1)then
            iarc = iarc + 1
            points(iarc) = 1
            mean_Nn(iarc) = cNn(i)
            sigma2_Nn(iarc) = 0.1**2
            bias = cNn(i) 
            EPOCH_Nn(iarc) = EPOCH(i)
         else
            points(iarc) = points(iarc) + 1  ! number of bw of continuous arc
            bias = cNn(i) - mean_Nn(iarc)
            if(bias.lt.3*dsqrt(sigma2_Nn(iarc)))then
               mean_Nn(iarc)  = mean_Nn(iarc)+bias/dfloat(points(iarc))
               sigma2_Nn(iarc)=(points(iarc)-2)
     &                        * sigma2_Nn(iarc)/dfloat(points(iarc)-1)
     &                        +(bias**2)/dfloat(points(iarc))
            endif
         endif
      enddo
c
      narc = iarc
c
      do i=1,narc
         mean_bnfrac(i) = mean_Nn(i) - FLOOR(mean_Nn(i))
         std_bnfrac(i)  = dsqrt(sigma2_Nn(i))
      enddo
c
      do i=1,narc
         if(i.NE.1)then
            n = NINT( mean_bnfrac(i-1) - mean_bnfrac(i))
            if(n.NE.0)then
               mean_bnfrac(i) = (n + mean_bnfrac(i))
               mean_Nn(i)     = (n + mean_Nn(i))
            endif
         endif
      enddo
c
      do i=1,narc
c
         call  sec2cal(EPOCH_Nw(i),year,mon,day,hour,min,sec,frac)
c
         dy = day + (hour + min/60.0d0 + (sec+frac)/3600.0d0)/24.0d0
c
         write(102,'(I3,x,I3,x,I6,x,F8.3,x,4F8.3)') 
     &         iPRN, i, points(i),dy,
     &         mean_bwfrac(i),std_bwfrac(i),
     &         mean_bnfrac(i),std_bnfrac(i)
      enddo
c
c++   ionosphere-free ambiguity combination          
c
c     call WAmbUHD()
c
      enddo
c
      write(*,*) 'Stop Normally'
      write(*,*)
c
c     END time 
c     ========
c
      call timestamp()
c
      close(101)
      close(102)
      close(111)
      close(211)
c
      stop
c
      end
