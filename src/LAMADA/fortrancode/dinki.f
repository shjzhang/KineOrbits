      double precision function DINKi (n, L, a, icol)

c*ver version 1, dd. 21-05-95
c*aut Delft Geodetic Computing Centre/LGR, Paul de Jonge
c*aut copyright by Delft University of Technology, Faculty of Geodesy

c*rol takes inproduct of column 'icol' of triangular unit matrix L and 
c*rol vector a (exclusive the diagonal element)

c     name   i/o      parameter description
c     ------ -------- --------------------------------------------------
c*par n      input    length of a
c*par L      input    triangular matrix L
c*par a      input    vector a 
c*par icol   input    see rol

      implicit double precision (a-h, o-z)

      double precision 
     +  L (n,n), a (n)

      DINKi = 0d0
      do i = icol+1, n
         DINKi = DINKi + L(i,icol) * a(i)
      end do
      
      return
      end
