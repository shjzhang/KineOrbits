*
*  procedure matrix_inverse
*      
      subroutine matrix_inverse_lapack(A, N)
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*  Returns the inverse of the square matrix X, a warning message is
*  printed if X is badly scaled or nearly singular.
*
*  Input_Output Auguments
*  ======================
*
*  Name       Type      I/O     Description
*  ----       ----      ---     ---------------------------------------
*  X(N,N)     REAL*8    I/O     input square matirx
*  N          Integer   I       the dimension of square matrix X
*
*  Algorithm
*  =========
*  uses LAPACK routines to compute the matrix inverse
*  ----------------------------+----------------------------------------
*  real Matrix inverse         |DLANGE, DGETRF, DGECON, DGETRI
*  ----------------------------+----------------------------------------
*  complex Matrix inverse      |ZLANGE, ZGETRF, ZGECON, ZGETRI
*  ----------------------------+----------------------------------------
*
*  Reference
*  =========
*  [1] LAPACK User's Guide, Third Edition
*      http://www.netlib.org/lapack/lug/lapack_lug.html
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
*  Declaration_of_Enviroment
*
      implicit none
c
c  Declaration_of_Input_Output_Varialbles
c
      INTEGER   N
***
      REAL*8    A(N,N)
c
c  Declaration_of_Local_Varialbes
c
      INTEGER   M, LDA, INFO, LWORK
      INTEGER   IPIV(N)
*** 
      REAL*8    work(N)
c
***START 
c
      M     = N
      LDA   = N
      lwork = N
c
c  Calling DGETRF to apply LU factorization
c  calling DGETRI to apply inversion of the matrix
c
      write(*,*) A
      pause
      call DGETRF( M, N, A, LDA, IPIV, INFO )
      if(info.ne.0)then
        write(*,*) 'fatal error'
        write(*,*) '    matrix inverse failed!'
        pause
      endif
      write(*,*) A
      pause
      call DGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )
c
      if(info.ne.0)then
        write(*,*) 'fatal error'
        write(*,*) '    matrix inverse failed!'
        pause
      endif
c  
      return
c
      end
