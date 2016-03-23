*
*  procedure vector_norm
*      
      subroutine vector_minus(a, b, c)
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*
*  Routine to compute the norm of the vector
*
*  Input_Output Auguments
*  ======================
*
*  Name       Type      I/O     Description
*  ----       ----      ---     ----------------------------------------
*  A(3)       REAL*8    I       vector
*  B(3)       REAL*8    I       vector
*  C(3)       REAL*8    O       vector
*
*
*  Usage
*  =====
*
*  Example
*  =======
*  A = [ 1,  2, 3], B = [9, 3, 2]
*  C = A - B
*  C = [-8, -1, 1]
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
      implicit none
c
c  Declaration_of_Input_Output_Varialbles
c
c
      real*8 A(3), B(3)
      real*8 C(3)
c      
      integer n
      integer i      
c
      do i=1, 3
        C(i) = A(i) - B(i)
      enddo
c
      return
c
       end
