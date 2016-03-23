*
*  Procedure ICRS2ITRS
*
      subroutine ICRS2ITRS(Sec_GPS, state_ICRS, state_ITRS)
*      
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*  
*  Transform from celestial to terrestrial coordinates:  
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
*  state_ICRS R      I      state vector in International Celestial 
*                           Reference
*  state_ITRS R      O      state vector in International Terrestrial 
*                           Reference
*
*  Reference
*  =========
*
*  Satellite Orbits theory, p191.
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
C  Declaration_of_Input_Output_Variables
C
      REAL*8    Sec_GPS
      REAL*8    state_ICRS(6), state_ITRS(6)
C      
C  Declaration_of_Local_Variables
C
      integer   i, j
c
      REAL*8    C2T(3,3), C2T_deriv(3,3)
      REAL*8    Coord_ICRS(3), Coord_ITRS(3)
      Real*8    vel_icrs(3), vel_itrs(3)
      real*8    vel_itrs_deriv(3)
*
*  Coordinate transformation
*
      do i=1, 3
        coord_icrs(i) = state_icrs(i)
        vel_icrs(i)   = state_icrs(i+3)
c
        coord_itrs(i) = state_itrs(i)
        vel_itrs(i)   = state_itrs(i+3)
      enddo
c
      call get_C2T_CIO(Sec_GPS, C2T)
c
      call mtxmul(C2T, Coord_ICRS, Coord_ITRS, 3, 3, 1)
c
c  velocity transformation
c
      call get_C2T_CIO_deriv(Sec_GPS, C2T_deriv)
c
      call mtxmul(C2T, vel_icrs, vel_itrs, 3, 3, 1)
      call mtxmul(C2T_deriv, coord_icrs, vel_itrs_deriv, 3, 3, 1)
c
      do i=1, 3
        vel_itrs(i) = vel_itrs(i) + vel_itrs_deriv(i)
      enddo
c
      do i=1, 3
        state_itrs(i)   = coord_itrs(i)
        state_itrs(i+3) = vel_itrs(i)
      enddo
C
      return
c
      end
