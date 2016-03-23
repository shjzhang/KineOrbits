*
*  Procedure ITRS2ICRS
*
      subroutine ITRS2ICRS(Sec_GPS, State_ITRS, State_ICRS)
*      
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*  
*  Transform from terrestrial to celestial coordinates: 
*  
*  [CRS] =  Rot_T2C * [TRS]
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
*  State_ITRS R      I      State vector in International Terrestrial 
*                           Reference
*  State_ICRS R      O      State vector in International Celestial 
*                           Reference
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
C
C  Declaration_of_Input_Output_Variables
C
      REAL*8    Sec_GPS
      REAL*8    State_ICRS(6), State_ITRS(6)
C      
C  Declaration_of_Local_Variables
C
      integer   i, j
***
      REAL*8    C2T(3,3), T2C(3,3)
      REAL*8    C2T_Deriv(3,3), T2C_Deriv(3,3)
      REAL*8    Coord_ICRS(3), Coord_ITRS(3)
      REAL*8    vel_ICRS(3), vel_ITRS(3)
      real*8    vel_ICRS_one(3)
      real*8    vel_ICRS_two(3)
c     coordinate
      do i=1, 3
        Coord_ITRS(i) = State_ITRS(i)
        Coord_ICRS(i) = State_ICRS(i)
      enddo
c     velocity
      do i=1,3
        vel_ITRS(i) = State_ITRS(i+3)
        vel_ICRS(i) = State_ICRS(i+3)
      enddo
c
c     transformation rotation matrix
c
      call get_C2T_CIO(Sec_GPS, C2T)
c
c     Transform C2T and store in T2C
c      
      call mtxtrs(C2T, T2C, 3, 3)
c
*     Now do Coordiate transformation
c
      call mtxmul(T2C, Coord_ITRS, Coord_ICRS, 3, 3, 1)
c
c     tranformation matrix derivative 
c
      call get_C2T_CIO_Deriv(Sec_GPS, C2T_Deriv)
c
c     transpose of C2T_Deriv
c
      call mtxtrs(C2T_Deriv, T2C_Deriv, 3, 3)
c
c     now do the velociy transformation
c
      call mtxmul(T2C,       vel_ITRS,   vel_ICRS_one, 3, 3, 1)
      call mtxmul(T2C_Deriv, Coord_ITRS, vel_ICRS_two, 3, 3, 1)
c
      do i=1, 3
        vel_ICRS(i) = vel_ICRS_one(i) + vel_ICRS_two(i)
      enddo
c
c     Store the state in ICRS
c
      do i=1, 3
        State_ICRS(i)    = Coord_ICRS(i)
        State_ICRS(i+3)  = vel_ICRS(i)
      enddo
c
      return
c
      end
