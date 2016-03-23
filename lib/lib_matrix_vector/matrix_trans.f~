*
*  procedure mtxtrs
*      
      SUBROUTINE mtxtrs(A,B,NROW,NCOL)
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*
*  Transpose the matrix A and returned in the matrix B
*
*  Auguments
*  =========
*
*  Name       Type      I/O     Description
*  ----       ----      ---     ---------------------------------------
*  A          REAL*8    I       Matrix 
*  B          REAL*8    O       Matrix
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
c     Enviroment
c    
      IMPLICIT NONE
c
c     Input/Output variables
C
C     A       I  INPUT MATRIX
C     B       I  INPUT MATRIX
C     C       O  OUTPUT MATRIX
C     NROW    I  NUMBER OF ROWS    OF MATRIX A
C     NCOLA   I  NUMBER OF COLUMNS OF MATRIX A
C     NCOLB   I  NUMBER OF COLUMNS OF MATRIX B
C
      INTEGER*4 NROW, NCOL
      INTEGER*4 I, J, k
***      
      REAL*8    A(NROW,NCOL),B(NCOL,NROW)
      REAL*8    CELL
c
      do i=1,NCOL
      do k=1,NROW
         B(i,k) = 0.0d0
      enddo
      enddo
C
      do I=1,NROW
        do J=1,NCOL
          CELL      = 0.0d0
          CELL      = A(I, J)
          B(J, I)   = CELL
        enddo
      enddo
C
      RETURN
C
      END

