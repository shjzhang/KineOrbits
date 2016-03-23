      subroutine LAMBDA (log, n, Q, a, cands, disall, L, D, Z, Zt, v1, 
     +  v2, v3, v4, v5, v6, ak, num) 

c*ver version 1.2, dd. 19-10-96 (13-02-96)
c*aut Delft Geodetic Computing Centre/LGR, Paul de Jonge 
c*aut copyright by Delft University of Technology, Faculty of Geodesy

c*rol integer estimation with the LAMBDA method

c     name   i/o      parameter description
c     ------ -------- --------------------------------------------------
c*par log    input    logical unit number for log-file
c*par n      input    dimension of matrix
c*par Q      input    variance-covariance matrix. This matrix is symmetric,
c*                    and only the lower triangular part is stored and
c*                    accessed. For the same reasons as below, a 
c*                    2-dimensional matrix is used    
c*           output   destroyed (in FMFAC5)
c*par a      input    the vector with real valued estimates \hat{a} (float
c*                    solution) 
c*           output   the vector with integer valued estimates \check{a}
c*                    (fixed solution)
c*par cands  work     | 2-dimensional array to store the candidates 
c*par disall output   | according squared norms \hat{a}-\check{a}
c*                    (sorted in increasing order)    *
c*par L      work     | lower triangular matrix: Q = L D L
c*                    | although L is lower triangular, in this example
c*                    | program L is stored column-wise in a 2-dimensional 
c*                    | array, to avoid the necessity of a dedicated 
c*                    | storage scheme. 
c*par D      work     | diagonal matrix
c*par Z      work     Z-matrix           
c*par Zt     work     transpose of Z-matrix
c*par v1     work     double precision work array with length=n
c*par v2     work     double precision work array with length=n+1
c*par v3     work     double precision work array with length=n+1
c*par v4     work     double precision work array with length=n
c*par v5     work     double precision work array with length=n
c*par v6     work     double precision work array with length=n
c*par ak     work     contains the (integer) increments of the original 
c*                    float ambiguities
c*par num    work     integer work array with length=n

      implicit double precision (a-h, o-z)
      parameter (MaxCan=2)

      double precision 
     +  Q (n,n), L (n,n), D (n), a (n), Z (n,n), disall (MaxCan), 
     +  cands (n, MaxCan)
      double precision 
     +  v1 (n), v2 (n+1), v3 (n+1), v4 (n), v5 (n), v6 (n), ak (n), 
     +  Zt (n,n)
      integer 
     +  num (n)

c*    initialize Z=unit matrix
      do i=1,n
         do j=1,n
            Z(i,j)=0d0
         end do
         Z(i,i)=1d0 
      end do

c*    make estimates in 'a' between -1 and +1 by subtracting an 
c*    integer number, store the increments in ak (=shift the centre 
c*    of the ellipsoid over the grid by an integer translation)
c*    (see section 4.6)
      call DOUT (log, 'original float ambiguities', a, 1, n)
      do i=1,n
         v1(i)=mod(a(i),1d0)
         ak(i)=a(i)-v1(i)
         a(i)=v1(i)
      end do
      call DOUT (log, '''reduced'' ambiguities', a, 1, n)
      call DOUT (log, 'increments', ak, 1, n)

c*                -*    -1    -1
c*    make the L_1   D_1   L_1    decomposition of the variance-covariance
c*    matrix Q_hat{a} (equation 3.8, case 3) (section 3.3)
      call FMFAC5 (Q, L, D, n)
      call DOUT (log, 'conditional variances (before)', D, 1, n)

c*    compute the Z-transformation based on L and D of Q, ambiguities
c*    are transformed according to \hat{z}=Z^* \hat{a}
      call SRC1 (n, L, D, a, Z)
      call DOUT (log, 'conditional variances (after)', D, 1, n)
      write (log,'(a)') ' Z-matrix:'
      do i=1,n
         write (log,'(6g12.1)') (Z(i,j),j=1,n)
      end do
      call DOUT (log, 'transformed ''reduced'' float ambiguities', 
     +  a, 1, n)

c*    For the search we need L and D of Q^{-1}, see section 4.1, or
c*    L^{-1} and D^{-1} of Q here (in our case we use of course
c*    the LAMBDA-transformed L and D as they came from SRC1)
      call INVLT2d (n, L, L, v1)
c*    ... and D_1
      do i=1,n
         D(i)=1d0/D(i)
      end do
        
c*    find a suitable Chi^2 such that we have two candidates at minimum
c*    use an eps to make sure the two candidates are inside the ellipsoid 
c*    (section 4.11)
      eps=1d-6
      call CHIstrt4 (n, D, L, v1, v2, a, v3, v4)
      Chi_1=v3(2)+eps

c*    find the two candidates with minimum norm (section 4.5)
      call FI71 (Chi_1, MaxCan, n, a, D, L, v1, v2, v3, v4, v5, v6, 
     +  ncan, disall, cands, ipos) 

      write (log,'(i3,a,g14.4)') 
     +  ncan, ' candidates found, with a Chi^* of ', Chi_1
      call DOUT (log, 'fixed transformed ''reduced'' ambiguities', 
     +  cands(1,ipos), 1, n)
      
c*    transpose Z
      do i=1,n
         do j=1,n
            Zt(i,j)=Z(j,i)
         end do
      end do

c*    LU decomposition of Z (Gaussian elimination with partial pivoting)
      call LU3 (Zt, n, num)

c*    solve Z^* a = z
      call LUSOL2 (Zt, n, cands(1,ipos), num)
c*    ...and add the increment to them
      do i=1,n 
         a(i)=cands(i,ipos)+ak(i)
      end do
      call DOUT (log, 'a_check', a, 1, n)
 
c*    'sort' the vector of squared norms in increasing order
c*    (if ipos equals 2, the best candidate is in the second place: 
c*    so reverse disall) 
      if (ipos.eq.2) then
         h=disall(1)
         disall(1)=disall(2)
         disall(2)=h
      endif
   
      return
      end
