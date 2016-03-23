*
*  Procedure rotation_eci_rac
*
      subroutine rotation_eci_rac(state, rot_eci_rac)
*      
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*  
*  Transformation Matrix from celestial to orbit radial-along-cross 
*  coordinates: 
*  
*  [RAC] =  Rot_eci_rac * [ECI]
*  
*  where [ECI] is a vector in the Geocentric Celestial Reference System,
*        [RAC] is the vecotor in the Radial-Along-Cross coordinate  
*              correpongding to the vector in the [ECI]
*  
*  Input_Output_Auguments
*  ======================
*
*  Name       Type   I/O    Descriptin
*  ----       ----   ---    --------------------------------------------
*  state       R      I     orbit state vector
*  rot_eci_rac R      O     rotatation matrix from ECI to RAC
*
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
c     declaration of enviroment
c
      implicit none
c
      real*8    pi
      parameter(pi     =3.1415926535897932)
      real*8    two_pi
      parameter(two_pi =6.2831853071795864)
c
c     declaration of input/output variables
c
      real*8    state(6)
      real*8    rot_eci_rac(3, 3)
c
c     declaration of local variables
c
      integer   i
c
      real*8    element_a, element_e, element_i
      real*8    omega_node, omege_peri, M, E, nu
      real*8    kepler(6)
c
c     caculate the orbit element from orbit state 
c
      call state2element(state, kepler)
c
c     Mean anomaly
c
      element_a  = kepler(1)
      element_e  = kepler(2)
      element_i  = kepler(3)
      omega_node = kepler(4)
      omege_peri = kepler(5)
      M          = kepler(6)
c
c     solve the kepler' eqation     
c
      call solve_kepler_equation(M, element_e, E)
c
c     calculate the true anomaly
c
      nu = atan2(sqrt(1.0d0-element_e**2)*dsin(E), (dcos(E)-element_e))
c
      nu = modulo(nu, two_pi)
c
c     Matrix initialization
c
      call iau_ir(rot_eci_rac)
c
c     rotate omega_node around z-axis of celestial coordiante
c
      call iau_rz(omega_node, rot_eci_rac)
c
c     rotate the inclination aroud new x-axis of intermediate-1 coordinate
c
      call iau_rx(element_i, rot_eci_rac)
c
c     rotate the nu aroud the cross-trak axis of the intermediate-2 coordinate
c
      call iau_rz(nu, rot_eci_rac)
c
      return
c
      end
