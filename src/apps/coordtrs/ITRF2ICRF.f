C
C   program ITRF2ICRF
C
      program ITRF2ICRF
C
C=======================================================================
C     ****f* apps/ITRF2ICRF
C
C   FUNCTION
C
C     Transform the coordinate from International Terrestrial Reference
C     System(ITRF) to International Celestial Reference System(ICRF).
C
C   ARGUMENTS
C
C     -i Arg       input file, which consists of time flag and ITRF coordinates,
c                  should have the following formats:
c
C                  Time X_Coord Y_Coord Z_Coord
C                  ....
c
c                  Time   : GPS second past J2000.0
c                  X_Coord: X coordinate in ITRF 
c                  Y_Coord: Y coordinate in ITRF 
c                  Z_Coord: Z coordinate in ITRF 
C
C     -o Arg       output file, which consists of time flag and transformed ICRF coordinates.
C                  Any existing file with that name will be 
C                  overwritten.
C     
C   USAGE
C
C     ITRF2ICRF -i ifile1 -o ofile2
C
C   COPYRIGHT
C
C     Copyright(c) 2006-2009    Shoujian Zhang, SGG of Wuhan University.
C
C     ***
C
C   $Id: ITRF2ICRF.f,v 1.0 2009/06/04 $
C=======================================================================
C
      implicit none
c
c     Variables in getopts     
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
      integer       n
      integer       irec
      integer       ios
c
      character*100 line
c
      real*8        sec_GPS
      real*8        state_itrf(6)
      real*8        state_icrf(6)
c
      call getargvs(nargv, argvs, stat)
c
      if(stat /=0 )then
         write(*,*)
         write(*,*) 
     & ' ITRF2ICRF 2009.6.11 by S.J. Zhang'
         write(*,*)
         write(*,*) 
     & ' Usages: ITRF2ICRF [options] ...'
         write(*,*)
         write(*,*)
     & '   Transform coordinate from ITRF to ICRF. Input time flag '
         write(*,*)
     & '   and coordinate should be stored in file.'
         write(*,*)
     & '   The time flag along with transformed ICRF coordinates are',
     &   ' also stored in an'
         write(*,*)
     & '   output file. Any existing file with the same name will be'
         write(*,*)
     & '   overwritten.'
         write(*,*)
         write(*,*) 
     & ' Required Arguments:'
         write(*,*)
         write(*,*)
     & '   -i Arg    input file, which consists of time flag and',
     &             ' ITRF coordinate,'
         write(*,*)
     & '             have the following formats:'
         write(*,*) 
     & '             Time X_Coord Y_Coord Z_Coord'
         write(*,*) 
     & '             ....'

         write(*,*) 
         write(*,*) 
     & '             Time   : GPS second past J2000.0'
         write(*,*) 
     & '             X_Coord: X coordinate in ITRF '
         write(*,*)                        
     & '             Y_Coord: Y coordinate in ITRF '
         write(*,*)                        
     & '             Z_Coord: Z coordinate in ITRF '
         write(*,*) 
         write(*,*) 
     & '   -o Arg    output file, which consists of time flag and',
     &             ' transformed ICRF coordinates'
         write(*,*) 
     & '             Any existing file with that name will be',
     &             ' overwritten.'
         write(*,*) 
c
         stop
c
      endif
c
      if(nargv /= nopts)then
         write(*,*) 'ITRF2ICRF'
         write(*,*) 'input argments not correct'
         stop
      endif
c
      iargv = 1
      do while(iargv <= nargv) 
         if(    argvs(iargv)=='-i')then
            iargv = iargv + 1
            ifile = argvs(iargv)
            write(*,*) ifile
         elseif(argvs(iargv)=='-o')then
            iargv = iargv + 1
            ofile = argvs(iargv)
            write(*,*) ofile
         endif
         iargv = iargv + 1
      enddo   
c
      open(unit=101,file=ifile,status="old",iostat=ios)
c
      if(ios.ne.0)then
         write(*,*) 'ITRF2ICRF'
         write(*,*) 'open ifile error'
         stop
      endif
c
      open(unit=102,file=ofile,status="replace",iostat=ios)
c
      if(ios.ne.0)then
         write(*,*) 'ITRF2ICRF'
         write(*,*) 'open ofile error'
         stop
      endif
c
      irec=1
c
100   continue
c
      do i=1,6
         state_itrf(i) = 0.0d0
         state_icrf(i) = 0.0d0
      enddo
c
      read(101, '(A80)', end=444)  line
c
      irec = irec + 1
c
      read(line,'(4F14.3)')sec_GPS,
     &                     state_itrf(1),state_itrf(2),state_itrf(3)
c
      call ITRS2ICRS(sec_GPS, state_itrf, state_icrf)
c
      write(102,'(4F14.3)') sec_GPS, 
     &                      state_icrf(1), state_icrf(2), state_icrf(3) 
c
      goto 100
c
444   continue
c
      stop 'Normally'
c
      end
