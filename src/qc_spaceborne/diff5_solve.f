c
c   subroutine diff5_solve
c
      subroutine diff5_solve(m,n,df5Ts,Ts)
c
c=======================================================================
c     ****f* qualicontr/diff5_solve
c
c   FUNCTION   
c
c     sovle the 5th-order difference equation forward.
c
c   INPUTS
c     
c     m             (I)         first record of df5Ts
c     n             (I)         last  record of df5Ts
c     df5Ts         (R)         5-th order difference of Ts
c
c   OUTPUT
c
c     Ts            (R)         raw time series
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: diff5_solve.f,v 1.0 2009/07/20 $
c=======================================================================
c
      implicit none
c
c     include
c
      include      '../../include/rinex.h'
      include      '../../include/qualicontr.h'
      include      '../../include/qualicontr.conf.h'
c
c     input/output
c
      integer       m, n
c
      real*8        df5Ts(MAX_OBS_REC)
c
      real*8        Ts(MAX_OBS_REC)
c
      integer       i,j,k
c
c     nobs X nparams
c     nobs    = n- m    + 1 + 9(initial) = n-m+10
c     nparams = n-(m-5) + 1              = n-m+6
c
      real*8        A   ( N-M+10,N-M+ 6)
      real*8        P   ( N-M+10,N-M+10)
      real*8        L   ( N-M+10,     1)
      real*8        A1  ( N-M+ 6,N-M+10)
      real*8        A1P ( N-M+ 6,N-M+10)
      real*8        A1PA( N-M+ 6,N-M+ 6)
      real*8        A1PL( N-M+ 6,     1)
      real*8        X   ( N-M+ 6,     1)
c
      integer       nobs, npms
      integer       ncol, nrow
c
      do i=1,N-M+10
      do j=1,N-M+ 6
         A(i,j)   = 0.0d0
      enddo
      enddo
c
      do i=1,N-M+10
      do j=1,N-M+10
         P(i,j)   = 1.0d0
      enddo
      enddo
c
      do i=1,N-M+10
         L(i,1)   = 0.0d0
      enddo
c
      do i=1,N-M+ 6
      do j=1,N-M+10
         A1(i,j)  = 1.0d0
      enddo
      enddo
c
      do i=1,N-M+ 6
      do j=1,N-M+10
         A1P(i,j) =0.0d0
      enddo
      enddo
c
      do i=1,N-M+6
      do j=1,N-M+6
         A1PA(i,j)=0.0d0
      enddo
      enddo
c
      do i=1,N-M+6
         A1PL(i,1)=0.0d0
      enddo
c
      do i=1,N-M+6
         X(i,1)   =0.0d0
      enddo
c
c     nobs : actual obseravtion
c
      nobs = n-m+1
      npms = n-m+6
c
      do i=1,nobs
         do j=1,npms
c
         A(i,i  )   =  -1
         A(i,i+1)   =   5
         A(i,i+2)   = -10
         A(i,i+3)   =  10
         A(i,i+4)   =  -5
         A(i,i+5)   =   1
c
         L(i,1)     =  df5Ts(i+m-1)
         P(i,i)     = (0.006*sqrt(252.0d0))**2
c
         enddo
      enddo
c
c     initial for (m-5) to (m-2),(n) to (n-3))
c     (m-1) to (n-4) is the parameter to be estimated
c
      do i=nobs+1,nobs+5
         A(i,i-nobs)= 1
         L(i,1)     = 0.0
         P(i,i)     =(0.006)**2
      enddo
c
      do i=nobs+6,nobs+9
         A(i,i-4)   = 1
         A(i,i-5)   =-1
         L(i,1)     = 0.0
         P(i,i)     = 2*(0.006)**2
      enddo
c
      nrow = n-m+10
      ncol = n-m+ 6
c
      call matrix_trans(A,  A1,      nrow,ncol)
c
      call matrix_multi(A1, P, A1P,  ncol,nrow,nrow)
      call matrix_multi(A1P,A, A1PA, ncol,nrow,ncol)
      call matrix_multi(A1P,L, A1PL, ncol,nrow,1)
c
      call matrix_inverse(A1PA,ncol)
c
      call matrix_multi(A1PA,A1PL,X,ncol,ncol,1)
c
      do i=1,N-M+6
         Ts(M-6+i) = X(i,1)
      enddo
c
      return
c
      end
