      subroutine INVLT2d (n, L, Lm, vec)

c*ver version 1, dd. 21-11-95, originates from INVLT
c*aut Delft Geodetic Computing Centre/LGR, Paul de Jonge
c*aut copyright by Delft University of Technology, Faculty of Geodesy

c*rol compute the inverse of a lower triangular matrix
c*rol Lm may over write L

c     name   i/o      parameter description
c     ------ -------- --------------------------------------------------
c*par n      input    dimension of the matrix
c*par L      input    lower triangular matrix
c*par Lm     output   inverse of L (also lower triangular)
c*par vec    work     double precision work array with lenght n

      implicit double precision (a-h, o-z)

      double precision 
     +  L (n,n), Lm (n,n), vec (n)

      do i = 1,n
         do j = 1, i-1
            vec(j) = L(i,j)
         end do
         do j = 1, i-1
            aiude = 0d0
            do k = j, i-1
               aiude = aiude + Lm(k,j)*vec(k) 
            end do
            Lm(i,j) = -aiude/L(i,i)
         end do
         Lm(i,i) = 1d0/L(i,i)
      end do

      return
      end

