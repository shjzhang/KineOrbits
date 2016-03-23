      subroutine SRC1i (n, L, D, a, Zti)

c*ver version 1.0, dd. 15-02-96
c*aut Delft Geodetic Computing Centre/LGR, Paul de Jonge
c*aut copyright by Delft University of Technology, Faculty of Geodesy

c*rol                     -*
c*rol Computation of the Z   matrix 
c*rol see: the LAMBDA method for integer ambiguity estimation: 
c*rol implementation aspects, section 3.7 and 3.9

c     name   i/o      parameter description
c     ------ -------- --------------------------------------------------
c*par n      input    dimension of the system
c*par L      in/out   lower triangular matrix L 
c*par D      in/out   diagonal matrix D stored as a one-dimensional 
c                     array
c*par a      in/out   Z (transposed) a, with a the original vector of 
c                     unknowns
c*par Zti    in/out   Z (transposed inverse) transformation matrix

      implicit double precision (a-h, o-z)

      double precision 
     +  L (n,n), Zti (n,n), D (n), a (n), lambda (3)
      logical 
     +  swap

      i1 = n - 1
      swap = .true.
10    continue
      if (swap) then
         i = n
         swap = .false.
20       continue
         if (.not. swap  .and.  i. gt. 1) then
            i = i - 1
            if (i .le. i1) call ZTRANi (i, i, n, L, a, Zti)
            delta = D(i) + L(i+1,i)**2 * D(i+1)
            if (delta .lt. D(i+1)) then
               lambda(3) = D(i+1) * L(i+1,i) / delta 
               eta = D(i) / delta
               D(i) = eta * D(i+1)
               D(i+1) = delta 
               do j = 1, i-1
                  lambda(1) = L(i,j)
                  lambda(2) = L(i+1,j)
                  L(i,j)   = lambda(2) - L(i+1,i) * lambda(1)
                  L(i+1,j) = lambda(3) * lambda(2) + eta * lambda(1)
               end do
               L(i+1,i) = lambda(3)
               do j = i+2, n
                  help = L(j,i)              
                  L(j,i) = L(j,i+1)              
                  L(j,i+1) = help
               end do
               do j = 1, n
                  help = Zti(j,i)              
                  Zti(j,i) = Zti(j,i+1)              
                  Zti(j,i+1) = help
               end do
               help = a(i)
               a(i) = a(i+1)
               a(i+1) = help
               i1 = i
               swap = .true.
            endif
            goto 20
         endif
         goto 10
      endif 

      return
      end
