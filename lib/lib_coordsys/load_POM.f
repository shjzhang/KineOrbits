*
*  Procedure load_POM
*      
      subroutine load_POM(POM_File_Name)
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*
*  Load Polar Motion data from the IERS Bulletin B files.
*
*  Input_Output_Auguments
*  ======================
*
*  Name         Type    I/O     Description
*  ----         ----    ---     ---------------------------------------
*  NPOM         I       O       Record Number of the Polar Motion data
*  MJD          R       O       Modified Julian date 
*  X            R       O       Polar Motion x
*  Y            R       O       Polar Motion y
*  UT12UTC      R       O       UT1 - UTC
*
*  History
*  =======
*
*  Notes
*  =====
*
*  File polar_motion.txt internal file which can be obained from IERS
*  Bulletin B, please update the data if necessary.
*
*  End line of polar_motion is :
*  
*  2005 MAY  27   53152  -.09018   .45981  -.468468  1.816   .263  -50.4 -4.6
*
*  Vesion 1.0
*    
*  Time         Author          Description
*  ----         ------          ----------------------------------------
*  07.06.01     S.J.Zhang       build this subroutine    
*
*********1*********2*********3*********4*********5*********6*********7**
*
*   Declaration_of_the_Evironment_Variables
*         
      IMPLICIT NONE
c
c   Declaration_of_the_Local_Variables
c
      integer       imax
      parameter(    imax=10000)
***
      REAL*8        pi
      parameter(    pi=3.1415926535897932d0)
c
c   Declaration_of_the_Input_Output_Variables
c      
      integer       NPOM
***
      real*8        Mjd(imax), x(imax), y(imax), UT12UTC(imax)
c
c   Declaration_of_the_Local_Variables
c
      integer       i, j, ios, irec
      integer       day
***
      real*8        UT12UTR(imax)
      real*8        D(imax), dx(imax), dy(imax)
***
      character*10  month
      character*100 POM_File_Name
      character*90  line
c
      logical       alive
c
c  common
c
      common /polar_motion/  MJD, x, y, ut12utc, NPOM
c
c  initial
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
c
c  check file id 44 exist ?
c
c     inquire(unit=44, exist=alive)
c     if(alive)then
c       write(*,*) '<load_POM>'
c       write(*,*) ' file ID 44, which is used for polar motion file'
c       write(*,*) ' has existed, please modify the file ID to '
c       write(*,*) ' another one'
c       stop
c     endif
c
c     open file
c
c     close file ID 44. 
      close(44)
c

      open(unit=44, file=POM_File_Name, status='old', iostat=ios)
***
      if(ios.ne.0)then
        write(*,*) 'lib_coordsys/load_pom.f'
        write(*,*) 'open polar motion file error!'
        write(*,*) 'please set correct HOPES_HOME environment variable'
        write(*,*) 'in the ~/.bashrc file'
        stop
      endif
c
c  Read datas
c      
***
      irec = 0
***
100   continue
      read(44,'(a90)',end=444) line
c
      irec = irec + 1
      read(line,   *)  
*    &                  Month, day,
     &                  Mjd(irec), x(irec), y(irec),
     &                  UT12UTC(irec), UT12UTR(irec),
     &                  D(irec), dx(irec), dy(irec)
c     write(*  ,2000)   Month, day,
c    &                  Mjd(irec), x(irec), y(irec),
c    &                  UT12UTC(irec), UT12UTR(irec),
c    &                  D(irec), dx(irec), dy(irec)
1000  format(A7,I2,f5.0,2f9.5,f10.6,2f7.3,2f7.1)
2000  format(A7,I2,f8.1,2f9.5,f10.6,2f7.3,2f7.1)
c
***
      goto 100
***
c
444   continue
c
      NPOM = irec
c
c  Close files
c
      close(44)
c
      return
c
      end
      
      
     
