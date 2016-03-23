      subroutine LU3 (Z, n, num) 

c*ver version 1, dd. 29-11-94
c*aut Delft Geodetic Computing Centre/LGR, Paul de Jonge

c*rol compute LU-decompostion of a square matrix, (not necessarily 
c*rol symmetric): Z=LU where L is a unit-lower triangular matrix, and U 
c*rol is an upper triangular matrix, using partial pivoting.
c*rol L (except the unit-diagonal) and U over-write Z. from: Golub and
c*rol van Loan: Matrix Computations, second edition, algorithm 3.4.3 
c*rol G.E. with Partial Pivoting: Gaxpy Version

c     name   i/o      parameter description
c     ------ -------- --------------------------------------------------
c*par Z      in/out   on input the matrix to be factored, on output the
c*                    L and U factor
c*par n      input    dimension of Z
c*par num    output   permutation vector

      implicit double precision (a-h, o-z)

      double precision 
     +  Z (n,n) 
      integer 
     +  num (n)

      do j=1,n
c*       re-shuffle part of column j
         do k=1,j-1
            help=Z(k,j)
            Z(k,j)=Z(num(k),j)
            Z(num(k),j)=help
         end do
         do k=1,j-1
            do i=k+1,j-1
               Z(i,j)=Z(i,j)-Z(i,k)*Z(k,j)
            end do
         end do
         do k=1,j-1
            do i=j,n
               Z(i,j)=Z(i,j)-Z(i,k)*Z(k,j)
            end do
         end do
c*       find the largest entry in column j (in absolute value)
         pmax=0d0
         do k=j,n
            if (abs(Z(k,j)).gt.pmax) then
               pmax=abs(Z(k,j))
               num(j)=k
            endif
         end do
c*       swap rows due to pivoting
         do k=1,j
            help=Z(j,k)
            Z(j,k)=Z(num(j),k)
            Z(num(j),k)=help
         end do
         if (Z(j,j).ne.0d0) then
            do i=j+1,n
               Z(i,j)=Z(i,j)/Z(j,j)
            end do
         endif
      end do

      return
      end
