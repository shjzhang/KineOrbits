*
*  procedure vector_cross
*      
      subroutine vector_cross(A, B, C)
*********1*********2*********3*********4*********5*********6*********7**
*      
*                               Purpose
*
*  Routine to compute cross product of two vectors A and B, the results
*  are stored in vector C
*
*                               Input_Output Auguments
*
*  Name         Type    I/O     Description
*  ----         ----    ---     ---------------------------------------
*  A(3)         REAL*8  I       vector A
*  B(3)         REAL*8  I       vector B
*  C(3)         REAL*8  O       vector C
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
      REAL*8 A(3),B(3),C(3)
C
      C(1)=A(2)*B(3)-A(3)*B(2)
      C(2)=A(3)*B(1)-A(1)*B(3)
      C(3)=A(1)*B(2)-A(2)*B(1)
C
      RETURN
c
      END
