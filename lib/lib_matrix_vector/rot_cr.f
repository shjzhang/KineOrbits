*
*  procedure rot_cr
*      
      subroutine rot_cr(A, B)
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*
*  Routine to copy the Rotation matrix A to Rotation matrix B
*
*  Input_Output Auguments
*  ======================
*
*  Name       Type      I/O     Description
*  ----       ----      ---     ---------------------------------------
*  A(3,3)     REAL*8    I       real rotation matrix
*  B(3,3)     REAL*8    O       real rotation matrix
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
*  Declaration_of_Local_Varialbes
*
      IMPLICIT NONE
*
*  Declaration_of_Input_Output_Varialbes
*
      REAL*8    A(3,3), B(3,3)
*
*  Declaration_of_Local_Varialbes
*
      INTEGER   i, j
c
c  Copy matrix
c
      do i=1, 3
        do j=1, 3
          B(i,j) = A(i,j)
        enddo
      enddo
c
      return
c
      end
  
