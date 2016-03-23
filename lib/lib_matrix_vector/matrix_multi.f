*
*  procedure mtxmulply
*      
      SUBROUTINE mtxmul(A,B,C,NROW,NCOLA,NCOLB)
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*
*  PREMULTIPLY MATRIX B BY MATRIX A WITH RESULTS IN MATRIX C
*
*  Auguments
*  =========
*
*  Name       Type      I/O     Description
*  ----       ----      ---     ---------------------------------------
*  A          REAL*8    I       Matrix 
*  B          REAL*8    I       Matrix
*  C          REAL*8    O       Matrix
*
*  Notes
*  =====
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
c
c     Environment_Variables
c    
      IMPLICIT NONE
c
c     Input/Output Varialbles
C
C     A       I  INPUT MATRIX
C     B       I  INPUT MATRIX
C     C       O  OUTPUT MATRIX
C     NROW    I  NUMBER OF ROWS    OF MATRIX A
C     NCOLA   I  NUMBER OF COLUMNS OF MATRIX A
C     NCOLB   I  NUMBER OF COLUMNS OF MATRIX B
C
      INTEGER*4 NROW,NCOLA,NCOLB
      INTEGER*4 I,J,N
***      
      REAL*8    SUM
      REAL*8    A(NROW,NCOLA),B(NCOLA,NCOLB),C(NROW,NCOLB)
C
      do I=1,NROW
        do J=1,NCOLB
          SUM = 0.D0
          do N=1,NCOLA
            SUM = SUM + A(I,N)*B(N,J)
          enddo
          C(I,J) = SUM
        enddo
      enddo
C
      RETURN
C
      END

