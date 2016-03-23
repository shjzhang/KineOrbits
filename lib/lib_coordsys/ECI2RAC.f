*
*  Procedure ECI2RAC
*
      subroutine ECI2RAC(state, coord_eci, coord_rac)
*      
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*  
*  Transform from Earth center inertial coordinate to Oribt Radial 
*  Along-track cross-track coordinate
*  
*  [RAC] =  Rot_eci2rac * [Eci]
*  
*  where [ECI] is a vector in the Geocentric Celestial Reference System , 
*        [RAC] is the vecotor in the Radial, Along-track, and Cross-track
*              coordinate correpongding to the vector in the [ECI]
*  
*  Input_Output_Auguments
*  ======================
*
*  Name       Type   I/O    Descriptin
*  ----       ----   ---    --------------------------------------------
*  state      R      I      orbit state vector
*  coord_eci  R      I      the coordinate vector in the ECI coordinate
*  coord_rac  R      I      the coordinate vector in the RAC coordinate
*
*
*  Notes
*  =====
*
*  orbit state is used to calculate the Orbit element of
*  the Sat, and then determine the transformation Rotation matrix  from
*  the ECI to RAC.
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
C  Declaration_of_Input_Output_Variables
C
      REAL*8    state(6)
      REAL*8    coord_eci(3), coord_rac(3)
C      
C  Declaration_of_Local_Variables
C
      REAL*8    rot_eci_rac(3,3)
*
*  Coordinate transformation
*
      call rotation_eci_rac(state, rot_eci_rac)
c
      call mtxmul(rot_eci_rac, coord_eci, coord_rac, 3, 3, 1)
C
      return
c
      end
