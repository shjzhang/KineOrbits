C
      SUBROUTINE lagrange(X,Y,n,xint,yout)
C 
C     This subroutine performs lagrangian interpolation
C     within a set of (X,Y) pairs to give the y
C     value corresponding to xint. This program uses a
C     window of 4 data points to perform the interpolation.
C     if the window size needs to be changed, this can be
C     done by changing the indices in the do loops for
C     variables m and j.
C
C     PARAMETERS ARE :
C     X     - array of values of the independent variable
C     Y     - array of function values corresponding to x
C     n     - number of points
C     xint  - the x-value for which estimate of y is desired
C     yout  - the y value returned to caller

      REAL*8  X(N),Y(N),xint,yout,term
      INTEGER N,I,J
      logical find
C
      YOUT = 0.0D0
c
      find = .false.
      call binary_search(xint,x,N,K,find)
C
      IF ( K .LT. 5 ) K = 5
      IF ( K .GT. N-5 ) K = N-5
      DO 20 M = K-4,K+5
        TERM = Y(M)
        DO 10 J = K-4,K+5
          IF ( M .NE. J ) THEN
            TERM = TERM * (XINT - X(J))/(X(M) - X(J))
          END IF
   10   CONTINUE
        YOUT = YOUT + TERM
   20 CONTINUE
      RETURN
      END
