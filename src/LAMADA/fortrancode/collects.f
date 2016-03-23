      subroutine COLLECTs (n, MaxCan, D_1, lef_1, left_1, right_1, Chic, 
     +  dist, end_1, ncan, disall, cands, tmax, imax)

c*ver version 1, dd. 19-05-95
c*aut Delft Geodetic Computing Centre/LGR, Paul de Jonge
c*aut copyright by Delft University of Technology, Faculty of Geodesy

c*rol collects integer vectors and corresponding squared distances

c     name   i/o      parameter description
c     ------ -------- --------------------------------------------------
c*par n      input    dimension of the system
c*par MaxCan input    number of minimum integer vectors requiered
c*par D_1    input    first element of diagonal matrix D
c*par lef_1  input    first element of dp work vector lef
c*par left_1 input    first element of dp work vector left
c*par right_1input    first element of dp work vector right
c*par Chic   input    Chi squared
c*par dist   in/out   difference between the integer tried and \hat{a}_n
c*par end_1  in/out   first element of dp work vector end 
c*par ncan   in/out   number of integer vectors found
c*par cands  in/out   | 2-dimensional array to store the candidates
c*par disall in/out   | according squared norms \hat{a}-\check{a}
c*par tmax   in/out   the largest distance of the Min (ncan,MaxCan) 
c*                    vectors with minimum distance found until now 
c*par imax   in/out   position in disall/cands of the vector with the
c*                    largest distance of the Min (ncan,MaxCan) vectors
c*                    with minimum distance found until now

      implicit double precision (a-h, o-z)

      double precision
     +  dist (n), disall (MaxCan), cands (n,MaxCan), lef_1, left_1
                           
      t = Chic - (right_1-left_1) * D_1
      end_1 = end_1 + 1d0

10    continue
         ncan = ncan + 1
         if (ncan .le. MaxCan) then
            call STOREs (ncan, ncan, imax, t, tmax, dist, disall, cands, 
     +        MaxCan, n)
         else
            if (t .lt. tmax) then
               call STOREs (MaxCan, imax, imax, t, tmax, dist, disall,
     +           cands, MaxCan, n)
            endif
         endif
         t = t + (2 * (dist(1)+lef_1) + 1) * D_1
         dist(1) = dist(1) + 1d0
      if (dist(1) .le. end_1) goto 10

      return
      end
