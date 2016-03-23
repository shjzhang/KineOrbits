c
c   program merge_sp3
c
      program merge_sp3
c
c=======================================================================
c     ****f* Formatrs/merge_sp3
c
c   FUNCTION   
c   
c     Merge several IGS SP3 format ephemeris file into one file. which
c     is used to avoid extrapolating, which introduce large interpolated
c     error.
c
c   USAGE
c
c     merge_sp3  -i  input_file  -o  output_file    
c
C   ARGUMENTS
C
C     -i Argument                 input sp3 file
C     -o Argument                 output sp3 file
c
c   COPYRIGHT
c
c     Copyright(c) 2006-          Shoujian Zhang,
c                                 School of Geodesy and Geomatics,
c                                 Wuhan University.
c     ***
c
C     $Id: merge_sp3.f,v 1.0 2009/06/26 $
c=======================================================================
c
      implicit none
c
      integer       MAX_PRN
      parameter    (MAX_PRN = 100)
c
c     variables in getopts     
c
      integer       MAX_OPTS
      integer       nopts
      integer       stat
      parameter    (MAX_OPTS = 20)
      parameter    (nopts    = 20)
c
c     variables from getargvs
c
      integer       nargv
      character*200 argvs(MAX_OPTS)
c
      character*200 ifile_sp3(10)
      character*200 ofile_sp3
c
      integer       ifid, n
      integer       ifile,nfile
c
      integer        PRN(MAX_PRN)
      character*3   cPRN(MAX_PRN)
c
      character*2   flag
      character*1   sys
c
      integer       isat, nsat, iPRN
      integer       idx_iPRN
      logical       find
c
      character*60  line
c
      integer       i, k, iargv
      integer       ios
c
      do i=1, MAX_PRN
          PRN(i) = 0
         cPRN(i) = ' '
      enddo
c     read arguments from command line
c     ================================
c
      call getargvs(nargv, argvs, stat)
c
      if(stat /=0 )then
         write(*,*)
         write(*,*) 
     &'Usages: merge_sp3 [options] ...'
         write(*,*)
         write(*,*)
     &'  Merge several IGS SP3 format ephemeris file into one file,',
     &'which is used to avoid extrapolating, which introduce large ',
     &'interpolated error'
         write(*,*)
         write(*,*) 
     &'OPTIONS'
         write(*,*)
         write(*,*)
     &'  -i Arg    input  sp3 file'
         write(*,*)
         write(*,*) 
     &'  -o Arg    output sp3 file.Any existing file with that ',
     &'name will be overwritten.'
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
      if(nargv >= nopts)then
         write(*,*) 'merge_sp3'
         write(*,*) 'input argments not correct'
         stop
      endif
c
      ifile = 0
      iargv = 1
      do while(iargv <= nargv) 
         if(    argvs(iargv)=='-i')then
            ifile            = ifile + 1
            iargv            = iargv + 1
            ifile_sp3(ifile) = argvs(iargv)
            write(*,*) ifile_sp3(ifile)
         elseif(argvs(iargv)=='-o')then
            iargv            = iargv + 1
            ofile_sp3        = argvs(iargv)
            write(*,*) ofile_sp3
         endif
         iargv = iargv + 1
      end do   
c
      nfile = ifile
c
      write(*,*) nfile
c
c     open output sp3 file
c     ====================
c
      open(unit=201,file=ofile_sp3,status='replace',iostat=ios)
c
      if(ios.ne.0)then
         write(*,*) 'Formatrs/merge_sp3.f'
         write(*,*) 'open output sp3 file error'
         stop
      endif
c
c     merge multiple file headers
c
************************************************************
c
      isat = 0
c
      do ifile=1, nfile
c   
         write(*,*) 'Merging the header of  file:', ifile_sp3(ifile)
c
c        file ID
c
         ifid = ifile + 100
c
c        open files and read header to store the PRN list in ephemeris file
c
         open(ifid, file=ifile_sp3(ifile), status='old', iostat=ios)
c
         if(ios.NE.0)then
            write(*,*) '<merge_sp3> error'
            write(*,*) ' open files error'
            stop
         endif
c
  300    continue
c
c        read a new line
c
         read(ifid, '(A60)', end=400) line
c
c        flag
c
         flag = line(1:2)
c
c        read PRN list
c
         if(flag.EQ.'+ ')then
c
c           read satellite number
c
c           read(line, 102) nsat
c
c           loop all the satellite numbers
            do i=1, 17
*              c.f. IGS ephemeris file conventions
               sys = line((9+3*(i-1)+1):(9+3*(i-1)+1))
c              only for GPS satellites
               if(sys.eq.'G')then
                  flag = line( (9+3*(i-1)+2):(9+3*(i-1)+3) )      
                  read(flag, '(I2)') iPRN
c                 search iPRN in PRN array
                  find = .false.
                  call linear_search(iPRN,MAX_PRN,PRN,idx_iPRN,find)
c                 if iPRN is not found in the PRN list, then write it
c                 into PRN array 
                  if(.NOT.find)then
                     isat = isat + 1
                     PRN(isat) = iPRN
                  endif
               endif
             enddo
*            store the number
             nsat = isat 
             if(nsat.gt.MAX_PRN)then
                write(*,*) '<merge_sp3>'
                write(*,*) ' please modify MAX_PRN'
                stop
             endif
c
c            read a new line
c
             goto 300
c
         endif
c
         if(flag.EQ.'* ')then
c           goto the end of this file
            goto 400
         endif
c
c        read a new line
c
         goto 300
c
c        formats in reading ephemeris file
c
 101     format(2X,  x, I4, X, I2, X, I2, 47x)
 102     format(2X, 2x, I2, 54x)
 103     format(2x, 1x, I4, 4(1x, I2),1x, F11.8)
 104     format(A2, I2, 3F14.6, 14x)
c
  400    continue
c
         close(ifid)
c
      enddo
c
c     sort PRN array 
c
      call sort(nsat, PRN)
c
      do i=1, nsat
         write(cPRN(i),'(A1,I2.2)') 'G',PRN(i)
         write(*,*) cPRN(i)
      enddo
c
c     write PRN list into header, other information are omitted.
c
      write(201,1000) '+ ', ' ', nsat, ' ', (cPRN(i),i= 1,17)
      write(201,2000) '+ ', ' ', '  ', ' ', (cPRN(i),i=18,34)
      write(201,2000) '+ ', ' ', '  ', ' ', (cPRN(i),i=35,51)
      write(201,2000) '+ ', ' ', '  ', ' ', (cPRN(i),i=52,68)
      write(201,2000) '+ ', ' ', '  ', ' ', (cPRN(i),i=69,85)
 1000 format(A2, A2, I2, A3, 17(A3))
 2000 format(A2, A2, A2, A3, 17(A3))
c
c     merge multiple file ephemeris
c
************************************************************
c
      do ifile=1, nfile
c   
c        write(*,*) 'Merging the ephemeris of file:', ifile_sp3(i)
c
c        file ID
c
         ifid = ifile+ 100
c
c        open files and read header to store the PRN list in ephemeris file
c
         open(ifid, file=ifile_sp3(ifile), status='old', iostat=ios)
c
         if(ios.NE.0)then
            write(*,*) '<merge_sp3> error'
            write(*,*) ' open files error'
            stop
         endif
c
  500    continue
c
c        read a new line
c
         read(ifid, '(A60)', end=600) line
c
c        flag
c
         flag = line(1:2)
c
c        read PRN list
c
         if(flag.NE.'* '.and.flag.NE.'PG'.and.flag.NE.'P ')then
c           read a new line
            goto 500
         else
            write(201,'(A60)') line
c           read a new line
            goto 500
         endif
c
  600    continue
c
c        close files
c
         close(ifid)
c
      enddo
c
      write(*,*) 'Stop Normally'
c
      close(201)
c
      stop
c
      end

