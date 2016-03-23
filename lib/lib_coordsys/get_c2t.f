*
*  Procedure get_c2t
*
      subroutine get_c2t(Sec_GPS, C2T)
*      
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*  
*  Transformation Matrix from celestial to terrestrial  coordinates: 
*  
*  [TRS] =  Rot_C2T * [CRS]
*  
*  where [CRS] is a vector in the Geocentric Celestial Reference
*  System and [TRS] is a vector in the International Terrestrial
*  Reference System (see IERS Conventions 2003)
*  
*  Input_Output_Auguments
*  ======================
*
*  Name       Type   I/O    Descriptin
*  ----       ----   ---    --------------------------------------------
*  Sec_GPS    R      I      Time in seconds past J2000
*  C2T        R      O      transformation rotation matrix
*
*  Notes
*  =====
*
*  History
*  =======
*
*  Vesion 1.0
*  ----------
*    
*  Time         Author      Description
*  ----         ------      --------------------------------------------
*  07/06/22     S.J.Zhang   build this subroutine    
*
*********1*********2*********3*********4*********5*********6*********7**
c
c  Declaration_of_Varialbes
c
      IMPLICIT NONE
C
C  Declaration_of_Function
C
      REAL*8    sec2Mjd
      REAL*8    cal2sec
      REAL*8    GPS2UTC
C
C  Declaration_of_Constants
C
      integer   IMAX
      parameter(IMAX = 10000)
***
      real*8    PI
      parameter(PI = 3.1415926535897932384626433832795d0)
***
      real*8    Mjd_ref
      parameter(Mjd_Ref = 2400000.5d0)
***
      real*8    AS2R
      parameter(As2R = 4.8481368110953599358991410235795D-6)
            
C
C  Declaration_of_Input_Output_Variables
C
      REAL*8    Sec_GPS
      REAL*8    C2T(3,3)
C      
C  Declaration_of_Local_Variables
C
      integer   NPOM
      integer   i, j
***
      REAL*8    Sec_TT1, Sec_TT2, Sec_UT11, Sec_UT12
      REAL*8    Sec_UTC,   Sec_UT1, Sec_TT
      REAL*8    MJD_UTC,   MJD_UT1, MJD_TT
      REAL*8    JD_TT1,    JD_TT2,  JD_UT11, JD_UT12
      REAL*8    MJD(IMAX), X(IMAX), Y(IMAX), UT12UTC(IMAX)
      REAL*8    MJD_Int,   x_int,   y_int,   ut12utc_int
C      
C  Declaration_of_Test_Variables
C
      integer   Year, Month, Day, Hour, Minu, Second
***
      real*8    Frac
c
      character*100 POM_File
      character*100 installpath
c
c  Common
c
      common /polar_motion/   MJD, x, y, ut12utc, NPOM
c
c  Transform Second to MJD
c
      Sec_UTC = GPS2UTC(Sec_GPS)
      Mjd_UTC = SEC2MJD(Sec_UTC)
      Mjd_Int = Mjd_UTC
c
c  Obtain the x, y, ut12utc at the given time MJD
c
c     Load Polar Motion file
c
      call getenv('HOPES_HOME',installpath)
c
      POM_File = 
     &trim(installpath)//trim('share/tables/polar_motion_IAU2000.txt')
c
      call load_POM(POM_File)
***
      if(Mjd_Int.lt.Mjd(1).or.Mjd_Int.gt.Mjd(NPOM))then
        write(*,*) 'fatal error << get_c2t.f'
        write(*,*) '>> range overflow in polar_motion.txt,
     +              please extend the file'
        stop
      endif
***
      call interp_POM(Mjd,     x,     y,     UT12UTC,   NPOM, 
     +                Mjd_int, x_int, y_int, UT12UTC_int     )
c
c     Unit transform
c
      x_Int = x_Int*PI/3600.0d0/180.0d0
      y_Int = y_Int*PI/3600.0d0/180.0d0
c
c     UT1, TT, in second past J2000.0
c
c
      Sec_TT1  = Sec_GPS 
      Sec_TT2  = 32.184d0 + 19.0d0
      Sec_UT11 = Sec_UTC 
      Sec_UT12 = UT12UTC_Int
c
c
c     UT1, TT, in JD
c 
      call sec2jdd(Sec_TT1,  Sec_TT2,  JD_TT1,  JD_TT2)
      call sec2jdd(Sec_UT11, Sec_UT12, JD_UT11, JD_UT12)
c
c  Call IAU Subrotuine iau_c2t00a
c
      call IAU_C2T00A(JD_TT1, JD_TT2, JD_UT11, JD_UT12, 
     +                x_Int,  y_Int,  C2T)
c
      return
c
      end
