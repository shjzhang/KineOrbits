*
*  procedure rot_y
*      
      subroutine rot_y(theta, rot_R)
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*  The matrix can be used to rotate the reference frame of a vector.  
*  Calling this routine with positive PHI incorporates in the matrix an 
*  additional rotation, about the y-axis, anticlockwise as seen looking
*  towards the origin from positive y.
*         
*  Input_Output Auguments
*  ======================
*         
*  Name       Type      I/O     Description
*  ----       ----      ---     ---------------------------------------
*  theta      Real*8    I       rotation angle given in Radians
*  rot_R      Real*8    I/O     rotation matrix
*
*     
*  Eample
*  ======
*
*  History
*  =======
*
*  Vesion 1.0
*    
*  Time         Author          Description
*  ----         ------          ----------------------------------------
*  07.06.01     S.J.Zhang       build this subroutine    
*
*********1*********2*********3*********4*********5*********6*********7**
*
*  Declaration_of_Enviroment
*
      IMPLICIT NONE
*
*  Declaration_of_Input_Output_Variables
*
      REAL*8    theta
      REAL*8    rot_R(3,3)
*
*  Declaration_of_Local_Variables
*
      INTEGER   i, j
***
      REAL*8    c, s
      REAL*8    w(3,3)
      REAL*8    rot_A(3,3)
c
c  Initial new rotation matrix rot_A to identity matrix 
c
      call rot_ini(rot_A)
c
      s = dsin(theta)
      c = dcos(theta)
c
c  Initial new rotation matrix rot_A 
c
      rot_A(1,1) = +c
      rot_A(1,3) = -s
      rot_A(3,1) = +s
      rot_A(3,3) = +c
c
c  Rotation matrix multiply
c
      call rot_dot_rot(rot_A, rot_R, w)
c
c  Copy w to rot_R
c
      call rot_CR(w, rot_R)
c
      return
c
      end
