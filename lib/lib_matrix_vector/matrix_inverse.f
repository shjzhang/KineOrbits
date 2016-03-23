*
*  procedure mtxinv
*      
      subroutine mtxinv(A, N)
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*
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
*
*  GUASSIAN_JORDAN elimination methods
*
*  Notes
*  =====
*
*  THE INTPUT of the argument of this subroutine is different from the
*  original subroutine BRINV, c.f. Xu's book.
*   
*  Reference
*  =========
*
*  Fotran common algorithm's library. by Xu Shiliang  
*  Modifed from BRINV.for  
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
c  Declaration_of_Input_Output_Varialbles
c
      DIMENSION         A(N,N),IS(N),JS(N)
      DOUBLE PRECISION  A,T,D
c
      L=1
      DO 100 K=1,N
        D=0.0
        DO 10 I=K,N
        DO 10 J=K,N
          IF (ABS(A(I,J)).GT.D) THEN
            D=ABS(A(I,J))
            IS(K)=I
            JS(K)=J
          END IF
10      CONTINUE
c          
        IF (D+1.0.EQ.1.0) THEN
          L=0
          WRITE(*,20) 
          stop
          RETURN
        END IF
c
c   Error information
c
20      FORMAT(1X,'ERR**NOT INV')
        DO 30 J=1,N
          T=A(K,J)
          A(K,J)=A(IS(K),J)
          A(IS(K),J)=T
30      CONTINUE
        DO 40 I=1,N
          T=A(I,K)
          A(I,K)=A(I,JS(K))
          A(I,JS(K))=T
40      CONTINUE
        A(K,K)=1/A(K,K)
        DO 50 J=1,N
          IF (J.NE.K) THEN
            A(K,J)=A(K,J)*A(K,K)
          END IF
50      CONTINUE
        DO 70 I=1,N
          IF (I.NE.K) THEN
            DO 60 J=1,N
              IF (J.NE.K) THEN
                A(I,J)=A(I,J)-A(I,K)*A(K,J)
              END IF
60          CONTINUE
          END IF
70      CONTINUE
        DO 80 I=1,N
          IF (I.NE.K) THEN
            A(I,K)=-A(I,K)*A(K,K)
          END IF
80      CONTINUE
100   CONTINUE
c          
      DO 130 K=N,1,-1
        DO 110 J=1,N
          T=A(K,J)
          A(K,J)=A(JS(K),J)
          A(JS(K),J)=T
110     CONTINUE
        DO 120 I=1,N
          T=A(I,K)
          A(I,K)=A(I,IS(K))
          A(I,IS(K))=T
120     CONTINUE
130   CONTINUE
c
      RETURN
c
      END

