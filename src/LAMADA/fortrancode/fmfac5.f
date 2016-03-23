      subroutine FMFAC5 (Q, L, D, n)

c*ver version 1.1, dd. 13-02-96 
c*aut Delft Geodetic Computing Centre/LGR, Paul de Jonge
c*aut copyright by Delft University of Technology, Faculty of Geodesy

c*rol transpose(L) D L factorization of Q, L may over write Q

c     name   i/o      parameter description
c     ------ -------- --------------------------------------------------
c*par Q      input    symmetric lower triangular matrix to be factored
c*par        output   destroyed on output
c*par L      output   factor   
c*par D      output   diagonal 
c*par n      input    dimension of the matrix

      implicit double precision (a-h, o-z)

      double precision 
     +  Q (n,n), L (n,n), D (n)

      do i = n, 1, -1
         D(i) = Q(i,i)
         do j = 1, i
            L(i,j) = Q(i,j) / sqrt(Q(i,i))
         end do

         do j = 1, i-1
            do k = 1, j
               Q(j,k) = Q(j,k) -  L(i,k)*L(i,j)
            end do
         end do

         do j = 1, i
            L(i,j) = L(i,j) / L(i,i)
         end do
      end do

      return
      end
