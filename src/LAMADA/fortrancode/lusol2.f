      subroutine LUSOL2 (Z, n, b, num)

c*ver version 1, dd. 29-11-94
c*aut Delft Geodetic Computing Centre/LGR, Paul de Jonge

c*rol Solution of the system LUx=b: First forward substitution Lv=b, 
c*rol v overwrites b, then backward substitution Ux=v 
c*rol from Golub and van Loan: Matrix Computations, second edition,
c*rol algorithm 3.1.1
c*rol permutation of right hand side according to num (pivot vector)

c     name   i/o      parameter description
c     ------ -------- --------------------------------------------------
c*par Z      input    L and U factor (from subroutine LU3)
c*par n      input    dimension of L and U
c*par b      in/out   right hand side on input, solution at output
c*par num    input    permutation vector (from subroutine LU3)

      implicit double precision (a-h,o-z)

      double precision 
     +  Z (n,n), b (n)
      integer
     +  num (n) 

c*    permute right hand side
      do i=1,n
         help=b(i)
         b(i)=b(num(i))
         b(num(i))=help
      end do

c*    forward substitution with lower UNIT triangular matrix
      do i=2,n
         do j=1,i-1
            b(i)=b(i)-Z(i,j)*b(j)
         end do
      end do

c*    backward substitution with upper triangular matrix
      b(n)=b(n)/Z(n,n)
      do i=n-1,1,-1
         do j=i+1,n
            b(i)=b(i)-Z(i,j)*b(j)
         end do
         b(i)=b(i)/Z(i,i)
      end do

      return
      end
