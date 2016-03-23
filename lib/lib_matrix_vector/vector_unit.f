*
*  procedure vector_unit
*      
 
      SUBROUTINE vector_unit(A, B)
*********1*********2*********3*********4*********5*********6*********7**
*      
*                               Purpose
*
*  Routine to creates a unit vector B colinear with input vector A
*
*                               Input_Output Auguments
*
*  Name         Type    I/O     Description
*  ----         ----    ---     ----------------------------------------
*  A(3)         REAL*8  I       vector A
*  B(3)         REAL*8  I       vector B
*     
*  History:
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
      DOUBLE PRECISION A(3), B(3), TEMP
c      
      TEMP = A(1)*A(1) + A(2)*A(2) + A(3)*A(3)
      TEMP = SQRT(TEMP)
      if (TEMP .eq. 0.0d0) then
        B(1)  = 0.0d0
        B(2)  = 0.0d0
        B(3)  = 0.0d0
      else
        B(1)  = A(1)/TEMP
        B(2)  = A(2)/TEMP
        B(3)  = A(3)/TEMP
      endif
c      
      return
c
      end
