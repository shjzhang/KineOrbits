      program   sp3trs       
c
c=======================================================================
c     ****f* SmartPPP/read_sp3
c
c   FUNCTION   
c   
c     read IGS SP3 precise ephemeris data from file.
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
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: read_sp3.f,v 1.0 2009/07/27 $
c=======================================================================
c
      implicit none
c
      integer       MAX_OPTS
      integer       nopts
      integer       stat
      parameter(    MAX_OPTS=4)
      parameter(    nopts   =4)
c   
      character*200 isp3_file
      character*200 opos_file
c
c     variables from getargvs
c
      integer       nargv
      character*200 argvs(MAX_OPTS)
c
      integer       iargv
      integer       ios
c
c     read arguments from command line
c     ================================

      write(*,*) 'nargv'
c
      call getargvs(nargv, argvs, stat)
c
      write(*,*) 'nargv'
c
      if(stat /= 0)then

          write(*,*)
          write(*,*) 'Usages: sp3trs -isp3 -f -opos f '
          write(*,*) 
          write(*,*) 'Arguments:'
          write(*,*)
          write(*,*) ' -isp3 input IGS sp3 file'
          write(*,*) ' -opos output position file'

      endif
c

      if(nargv.gt. nopts)then
         write(*,*) 'SmartPPP/SmartPPP.f'
         write(*,*) 'input argments not correct'
         stop
      endif
c
      iargv = 1
      do while(iargv <= nargv) 
         if(    argvs(iargv)=='-isp3')then
            iargv = iargv + 1
            isp3_file = argvs(iargv)
         elseif(argvs(iargv)=='-opos')then
            iargv = iargv + 1
            opos_file = argvs(iargv)
         endif
         iargv = iargv + 1
      end do   
c
c     open rinex file
c     ===============
c
      open(unit=102,file=isp3_file,status='old',iostat=ios)
c
      if(ios.ne.0)then
         write(*,*) 'SmartPPP/SmartPPP.f'
         write(*,*) 'open sp3 file error'
         stop
      endif
c
c     open output position file
c     =========================
c
      open(unit=201,file=opos_file,status='replace',iostat=ios)
c
      if(ios.ne.0)then
         write(*,*) 'SmartPPP/SmartPPP.f'
         write(*,*) 'open position file error'
         stop
      endif
c
c     BEGIN time 
c     ==========
c
      write(*,*)
      write(*,*) 'PROCESSING ... ...'
      write(*,*) '=================='
c
      write(*,*)
      call timestamp()
      write(*,*)
c
      call read_sp3()
c
      call write_pos()
c
      end
