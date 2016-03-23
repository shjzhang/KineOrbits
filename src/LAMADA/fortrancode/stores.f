      subroutine STOREs (ican, ipos, imax, t, tmax, dist, disall, cands, 
     +  MaxCan, n)

c*ver version 1, dd. 19-05-95
c*aut Delft Geodetic Computing Centre/LGR, Paul de Jonge
c*aut copyright by Delft University of Technology, Faculty of Geodesy

c*rol stores integer vectors and corresponding distances

c     name   i/o      parameter description
c     ------ -------- --------------------------------------------------
c*par ican   input    Min (number of vectors found until now, MaxCan)
c*par ipos   input    position in disall/cands to put the new found vector
c*par imax   output   position in disall/cands of the vector with the
c*                    largest distance of the ican vectors with minimum 
c*                    distance found until now
c*par t      input    distance of the new found vector    
c*par tmax   output   the largest distance of the ican vectors with 
c*                    minimum distance found until now 
c*par dist   input    difference between the integer tried and \hat{a}_n
c*par cands  in/out   2d-array to store the integer vectors 
c*par disall in/out   distance of the MaxCan integer vectors
c*par MaxCan input    number of integer vectors required
c*par n      input    dimension of the system (number of DD ambiguities)

      implicit double precision (a-h, o-z)

      double precision
     +  dist (n), disall (MaxCan), cands (n,MaxCan)
                      
      do i = 1, n
         cands (i, ipos) = dist (i)
      end do

      disall (ipos) = t
      tmax = t
      imax = ipos

      do i = 1, ican
         if (disall(i).gt.tmax) then
            imax = i
            tmax = disall(i)
         endif
      end do

      return
      end
