*
*  Procedure     LEAPSEC
*
      Subroutine leapsec(nleap, leap)
*********1*********2*********3*********4*********5*********6*********7**
*      
*                           Purpose
*
*  Store the leap seconds and leap second occurance time, which is given
*  seconds past J2000 UTC, in the arry leap
*
*                           Input_Output Auguments
*
*  Name     Type    I/O     Descriptin
*  ----     ----    ---     --------------------------------------------
*  leap     REAL*8  O       leap(1) is used to store the leap second
*                           occurance time,given in seconds past J2000
*                           UTC.
*                           leap(2) is used to store the leap seconds 
*                           number
*
*                           Notes
*
*  The leap seconds can be attracted from the file tai-utc.dat
*  which can be downloaded form the ftp site:
*    ftp://oceans.gsfc.nasa.gov/COMMON/leapsec.dat
*
*  File Exapmle:
*    1980 JAN  1 =JD 2444239.5  TAI-UTC=  19.0000000 S
*    1981 JUL  1 =JD 2444786.5  TAI-UTC=  20.0000000 S
*    1982 JUL  1 =JD 2445151.5  TAI-UTC=  21.0000000 S
*    1983 JUL  1 =JD 2445516.5  TAI-UTC=  22.0000000 S
*    1985 JUL  1 =JD 2446247.5  TAI-UTC=  23.0000000 S
*    1988 JAN  1 =JD 2447161.5  TAI-UTC=  24.0000000 S
*    1990 JAN  1 =JD 2447892.5  TAI-UTC=  25.0000000 S
*    1991 JAN  1 =JD 2448257.5  TAI-UTC=  26.0000000 S
*    1992 JUL  1 =JD 2448804.5  TAI-UTC=  27.0000000 S
*    1993 JUL  1 =JD 2449169.5  TAI-UTC=  28.0000000 S
*    1994 JUL  1 =JD 2449534.5  TAI-UTC=  29.0000000 S
*    1996 JAN  1 =JD 2450083.5  TAI-UTC=  30.0000000 S
*    1997 JUL  1 =JD 2450630.5  TAI-UTC=  31.0000000 S
*    1999 JAN  1 =JD 2451179.5  TAI-UTC=  32.0000000 S
*    2006 JAN  1 =JD 2453736.5  TAI-UTC=  33.0000000 S
*                   
*  History:
*
*  Vesion 1.0
*    
*  Time         Author      Description
*  ----         ------      --------------------------------------------
*  07.06.01     S.J.Zhang   build this subroutine    
*
*********1*********2*********3*********4*********5*********6*********7**
c
c     Declaration_of_Varialbes
c
      IMPLICIT NONE
C
C     Declaration_of_External_Functions
C
      REAL*8 JD2SEC
C
C     Declaration_of_Constants
C
      INTEGER       MAX_LEAP
      PARAMETER(    MAX_LEAP = 20 )
c
c     Declaration_of_Input_Output_Varialbles
c
      INTEGER       NLEAP
      REAL*8        LEAP(MAX_LEAP,2)
c
c     Declaration_of_Local_Varialbles
c
      INTEGER       i, j, ileap
      INTEGER       ios
      REAL*8        JD
      REAL*8        SEC
c
      CHARACTER*90  leap_file_name
      CHARACTER*90  line
c
      logical       alive
c
      character*100 installpath
c
      alive = .false.
c
c     Initialization
c
      call getenv("HOPES_HOME",installpath)
c
      leap_file_name=
     & trim(installpath)//trim('share/tables/leapsec.txt')

***      
c     inquire(file=leap_file_name, exist=alive)
c     if(alive)then
c       write(*,*) '<leapsec>'
c       write(*,*) ' file ID 444, which is used for leapsec file'
c       write(*,*) ' has existed, please modify the file ID to '
c       write(*,*) ' another one'
c       stop
c     endif
c
c     Load leap seconds into leap(nleap,2)
c   
c     close file ID 444
      close(444)
c
      open(unit=444,file=leap_file_name,status='old',iostat=ios)
c
c     Open error
c      
      if(ios.ne.0)then
        write(*,*) 'lib_timesys/leapsec.f'
        write(*,*) 'open leapsec.dat file error!!!'
        write(*,*) 'Please set correct HOPES_HOME environment variable'
        write(*,*) 'in the ~/.bashrc'
        stop
      endif
c
c     Load leapsec
c
      ileap = 0
100   continue
c
      read(444, '(A90)', end=200) line
c
c     read leap sec occurance time and leap second from line
c     
      ileap = ileap + 1
      read(line(18:26), '(f9.1)') leap(ileap,1)  
      read(line(39:48), '(f4.1)') leap(ileap,2)
c
c     Convert JD to seconds past J2000
c
      JD            = leap(ileap,1)
      leap(ileap,1) = JD2SEC(JD)     
c
c     read next record
c      
      goto 100
c
c     End of file
c      
200   continue
c
c     leap occurance times
c
      nleap = ileap
c
c     close file
c
      close(444)
c
      return
c
      end
