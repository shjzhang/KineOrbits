      subroutine FMFAC6 (L, D, n, eps, cfail)

c*ver version 1, dd. 30-12-95 
c*aut Delft Geodetic Computing Centre/LGR, Christian Tiberius
c*aut copyright by Delft University of Technology, Faculty of Geodesy

c*rol transpose(L) D L factorization of Q, L over-writes Q 
c*rol bordering method, computation of Googe number (section 3.3)

c     name   i/o      parameter description
c     ------ -------- --------------------------------------------------
c*par L      input    symmetric lower triangular matrix Q to be factored
c*par        output   factor   
c*par D      output   diagonal 
c*par n      input    dimension of the matrix
c*par eps    input    if the Googe number is smaller than eps the matrix
c*                    is considered to be singular
c*par cfail  output   error message

      implicit double precision (a-h, o-z)

      double precision 
     +  L (n,n), D (n)
      character
     +  cfail*(*)

      gomi = 1d20

      do j = n, 1, -1
         do i = n, j+1, -1
            sum = 0d0
            do k = i+1, n
               sum = sum + L(k,j)*L(k,i)
            end do
            L(i,j) = (L(i,j)-sum) / L(i,i)
         end do
         sum = 0d0
         do k = j+1, n
            sum = sum + L(k,j)*L(k,j)
         end do
         t = L(j,j) - sum
         if (t .le. L(j,j)*eps) then
            cfail (1:46) =
     +        ' error FMFAC6: matrix singular at pivot xxxx  '
            write (cfail(41:44),'(i4)') j
            goto 999
         endif
         googe = t/L(j,j)
         if (googe .lt. gomi) gomi=googe
         L(j,j) = sqrt(t)
      end do

      do i = 1, n
         do j = 1, i-1
            L(i,j) = L(i,j)/L(i,i)
         end do
         D(i) = L(i,i)*L(i,i)
         L(i,i) = 1d0
      end do

      cfail (1:46) = ' FMFAC6: smallest googe number: '
      write (cfail(33:46), '(g14.4)') gomi

999   return
      end
