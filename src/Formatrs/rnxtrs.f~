c
c   program rnxtrs
c
      program rnxtrs
c
c=======================================================================
c     ****f* fmttrs/rnxtrs
c
c   FUNCTION   
c   
c     Transform the GNSS data from rinex format to the internal 'hprnx'
c     format. The main processing is to transform the time format from
c     Gregorian calendar date to GPS time in second past J2000.0. Which
c     will be very useful for the following data editing, precise point 
c     positioning and precise orbit determination.
c
c   USAGE
c
c     rnxtrs  -i  input_file  -o  output_file    
c
C   ARGUMENTS
C
C     -i Argument                 input rinex observation file
C     -o Argument                 output hprnx observation file
c
c   COPYRIGHT
c
c     Copyright(c) 2006-          Shoujian Zhang,
c                                 School of Geodesy and Geomatics,
c                                 Wuhan University.
c     ***
c
C     $Id: rnxtrs.f,v 1.0 2009/06/26 $
c=======================================================================
c
      implicit none
c
      include      '../../include/rinex.h'
      include      '../../include/rinex.conf.h'
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
      integer       i, k, iargv
      integer       ios
c
c     variables for rinex 3.00 processing
c
      character*3   yes
c  
c     rinex header
c   
      real*8        rnxvsn
      integer       nsat_sys
      character*3   csat_sys(MAX_SAT_SYS)
      integer       nobs_typ(MAX_SAT_SYS)
      character*3   cobs_typ(MAX_SAT_SYS, MAX_OBS_TYP)
c
c     system path
c
      character*100 HOPES_HOME
      character*100 RINEX_CONF
c
c     read arguments from command line
c     ================================
c
      call getargvs(nargv, argvs, stat)
c
      if(stat /=0 )then
         write(*,*)
         write(*,*) 
     &'Usages: rnxtrs [options] ...'
         write(*,*)
         write(*,*)
     &'  Transform the GNSS data from rinex format to the internal ',
     &'hprnx format. The main processing is to transform the time ',
     &'format from Gregorian calendar date to GPS time in second past ',
     &'J2000.0, Which will be very useful for the following data ',
     &'editing, precise point positioning and precise orbit ',
     &'determination.'
         write(*,*)
         write(*,*) 
     &'OPTIONS'
         write(*,*)
         write(*,*)
     &'  -i Arg    input  observation file with rinex format'
         write(*,*)
         write(*,*) 
     &'  -o Arg    output observation file with hprnx format.',
     &'Any existing file with that name will be ',
     &'overwritten.'
         write(*,*) 
         write(*,*)
     &'AUTHOR'
         write(*,*)
         write(*,*)
     &'  Programmed by Shoujian Zhang'
         write(*,*)
c
         stop
c
      endif
c
      if(nargv /= nopts)then
         write(*,*) 'rnxtrs'
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
c     open rinex file
c     ===============
c
      open(unit=101,file=ifile,status='old',iostat=ios)
c
      if(ios.ne.0)then
         write(*,*) 'fmttrs/rnxtrs.f'
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
         write(*,*) 'fmttrs/rnxtrs.f'
         write(*,*) 'open hprnx file error'
         stop
      endif
c
c     rinex confiugre file name
c     =========================
c
      RINEX_CONF = 'etc/rinex.conf'
c
      call getenv('HOPES_HOME',HOPES_HOME)
c
      RINEX_CONF = trim(HOPES_HOME)//trim(RINEX_CONF)
c
c     parse the parameters in rinex.conf
c
      call parse_conf(RINEX_CONF)
c
c     read rinex header and relating information
c     ==========================================
c
      call rnxhdr(rnxvsn, nsat_sys, csat_sys, nobs_typ, cobs_typ)
c
c     special consideration for SPACEBORNE receiver
c
      if(trim(MARKER_TYPE).EQ.'SPACEBORNE')then
c
         write(*,*)
         write(*,*)              'Marker Type:'
         write(*,'((x, A )   )')  trim(MARKER_TYPE)
         write(*,*)              'ARP coordinates:'
         write(*,'((x, 3F6.2))')  ARP_DX, ARP_DY, ARP_DZ
         write(*,*)              'MASS_CENTER coordinates:'
         write(*,'((x, 3F6.2))')  MAS_DX, MAS_DY, MAS_DZ 
c
      endif
c
c     read and transform the observation data format 
c     ==============================================
c
      write(*,*) 'Transforming, please wait ...'
c
      call rnxobs(rnxvsn, nsat_sys, csat_sys, nobs_typ, cobs_typ)
c
      write(*,*) 'Stop Normally'
c
      stop
c
      end

