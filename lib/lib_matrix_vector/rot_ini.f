*
*  procedure rot_cr
*      
      subroutine rot_ini(rot_R)
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*
*  Initialize an rotation matrix to the identity matrix
*
*  Input_Output Auguments
*  ======================
*
*  Name       Type      I/O     Description
*  ----       ----      ---     ---------------------------------------
*  rot_R(3,3) REAL*8    I/O     real rotation matrix
*
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
*  Declaration_of_Local_Varialbes
*
      IMPLICIT NONE
*
*  Declaration_of_Input_Output_Varialbes
*
      REAL*8    rot_R(3,3)
*
*  Declaration_of_Local_Varialbes
*
      INTEGER   i, j
c
c  Initial the matrix to zero-matrix
c
      do i=1, 3
        do j=1, 3
          rot_R(i,j) = 0.0d0
        enddo
      enddo
c
c  Initial the matrix to identity matrix
c
      do i=1,3
        rot_R(i,i) = 1.0d0
      enddo
c
      return
c
      end
      
      







      
