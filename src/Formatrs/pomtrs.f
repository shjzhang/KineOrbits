*
*  program pomtrs
*      
      program pomtrs
c
c=======================================================================
c     ****f* fmttrs/pomtrs
c
c   FUNCTION   
c   
c     Extracting polar motion parameters from IERS bulletin B files.   
c
c   USAGE
c
c     pomtrs < inputfile > outputfile    
c
c   COPYRIGHT
c
c     Copyright(c) 2006-          Shoujian Zhang,
c                                 School of Geodesy and Geomatics,
c                                 Wuhan University.
c     ***
c
C     $Id: pomtrs.f,v 1.0 2010/02/08 $
c=======================================================================
c         
      IMPLICIT NONE
c
c     local variables
c
      real*8        pi
      parameter(    pi=3.1415926535897932d0)
c
c     input/output file variables 
c 
      integer       i,  j, ios, irec
      integer       date
      integer       day
c
      character*2   section
      character*3   month
      character*90  line
      character*20  header
      character*30  end_sec
      character*10  year_head, Mjd_head, x_head, y_head
      character*10  ut12utc_head, UT12UT1R_head, D_head
      character*10  dx_head, dy_head
c
      integer       NPOM
      integer       Mjd_Int
c
      real*8        Mjd(imax), x(imax), y(imax), UT12UTC(imax)
      real*8        UT12UTR(imax), D(imax), dx(imax), dy(imax)
      real*8        Mjd_last
      real*8        Mjd_this
c
c     initial
c
      do i=1, imax
         Mjd(i)     = 0.0d0
         x(i)       = 0.0d0
         y(i)       = 0.0d0
         UT12UTC(i) = 0.0d0
         UT12UTR(i) = 0.0d0
         D(i)       = 0.0d0
         dx(i)      = 0.0d0
         dy(i)      = 0.0d0
      enddo
***
      Mjd_Int      = 0
c
c     read section 1
c
100   continue
c
      read(*,'(A90)',end=444) line
c
      if(index(line(1:4), ' 2 -').eq.0)then
         goto 100
      else
         goto 300
      endif
c
c     read section 2
c
      header = "MJD       x        y"
c
300   continue
c      
      read(*,'(A90)',end=444) line
c
      write(*,*) line
c
      if(index(line, 'UT1-UT1R').ne.0)then
c
c        read data block header and unit
         irec = 0
c
         read(*,'(A90)') line
c
c        read data block and stored in array
c
400      continue
c
         read(*,'(A90)',end=444) line
c
         if(ichar(line(3:3)).gt.64)then
c
            irec = irec + 1
            read(line,*)
     &      Month, day, 
     &      Mjd_Int, x(irec), y(irec),
     &      UT12UTC(irec), UT12UTR(irec),
     &      D(irec), dx(irec), dy(irec)
c
            Mjd(irec) = dfloat(Mjd_Int)
            Mjd_this  = Mjd(irec)
            write(*, 2000) 
     &      Mjd(irec), x(irec), y(irec),
     &      UT12UTC(irec), UT12UTR(irec),
     &      D(irec), dx(irec), dy(irec)
***
            if(Mjd_this.le.Mjd_last)then
              goto 400
            endif
***
            write(*, 2000) 
     &      Mjd(irec), x(irec), y(irec),
     &      UT12UTC(irec), UT12UTR(irec),
     &      D(irec), dx(irec), dy(irec)
2000        format(f7.1,2f9.5,f10.6,2f7.3,2f7.2)
c
c           store for next file writing begining
c            
            Mjd_last = Mjd(irec)
c
            goto 400
c
         else 
c
            goto 444
c
         endif
      else 
c
         goto 300
c
      endif
c
444   continue
c
      NPOM = irec
c
c     close files
c
      end
