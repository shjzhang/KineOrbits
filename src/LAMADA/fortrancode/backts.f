      subroutine BACKTs (n, i, end, dist, lef, left, ende)

c*ver version 1, dd. 01-09-93
c*aut Delft Geodetic Computing Centre/LGR, Paul de Jonge
c*aut copyright by Delft University of Technology, Faculty of Geodesy

c*rol backtrack in the search tree, internal subroutine for FI71

c     name   i/o      parameter description
c     ------ -------- --------------------------------------------------
c*par n      input    dimension of matrix
c*par i      in/out   level in the tree
c*par end    input    dp work vector 
c*par dist   in/out   difference between the integer tried and \hat{a}_i
c*par lef    input    dp work vector 
c*par left   output   dp work vector 
c*par ende   output   if .true., then search is done

      implicit double precision (a-h, o-z)

      double precision 
     +  dist (n), left (n), lef (n), end (n) 
      logical 
     +  ende
      
      j = i+1
      do i = j, n
         if (dist(i) .le. end(i)) then
            dist(i) = dist(i) + 1d0
            left(i) = (dist(i) + lef(i))**2
            goto 10
         endif
      end do
      ende = .true.
10    continue

      return
      end
