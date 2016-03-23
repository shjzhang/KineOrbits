*
*  procedure rot_cr
*      
      subroutine rot_dot_rot(A, B, C)
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*
*  Multiply two rotation matrix.
*
*  Input_Output Auguments
*  ======================
*
*  Name       Type      I/O     Description
*  ----       ----      ---     ---------------------------------------
*  A(3,3)     REAL*8    I       real rotation matrix
*  B(3,3)     REAL*8    I       real rotation matrix
*  C(3,3)     REAL*8    O       real rotation matrix
*
*  Eample
*  ======
*  C = A*B
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
      REAL*8    A(3,3), B(3,3), C(3,3)
*
*  Declaration_of_Local_Varialbes
*
      INTEGER   i, j, k
c
      do i=1, 3
        do j=1, 3
          C(i,j) = 0.0d0
          do k=1, 3
            C(i,j) = C(i,j) + A(i,k)*B(k,j)
          enddo
        enddo
      enddo
c
      return
c
      end

      

