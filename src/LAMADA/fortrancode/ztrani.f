      subroutine ZTRANi (first, last, n, L, a, Zti)

c*ver version 1.0, dd. 15-02-96
c*aut Paul de Jonge, Delft Geodetic Computing Centre (LGR)
c*aut copyright by Delft University of Technology, Faculty of Geodesy

c*rol Updates integral Z (transposed inverse); only column `first' until 
c*rol `last'. see: the LAMBDA method for integer ambiguity estimation: 
c*rol implementation aspects (section 3.5 & 3.9)

c     name   i/o      parameter description
c     ------ -------- --------------------------------------------------
c*par first  input    first column to be updated
c*par last   input    last column to be updated
c*par n      input    dimension of the system
c*par L      in/out   lower triangular matrix L 
c*par a      in/out   Z (transposed) a, with a the vector of unknowns
c*par Zti    in/out   Z (transposed inverse) matrix

      implicit double precision (a-h, o-z)

      double precision 
     +  L (n,n), Zti (n,n), a (n)
      integer 
     +  first
      
      do i = last, first, -1
         do j = i+1, n
            mu = nint(L(j,i))               
            if (mu .ne. 0) then
               do k = j, n
                  L(k,i) = L(k,i) - mu * L(k,j)  
               end do
               do k = 1, n
                  Zti(k,j) = Zti(k,j) + mu * Zti(k,i)  
               end do
               a(i) = a(i) - mu * a(j)
            endif
         end do
      end do

      return
      end
