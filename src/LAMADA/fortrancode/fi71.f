      subroutine FI71 (Chic, MaxCan, n, a, D, L, lef, left, right,
     +  dist, end, dq, ncan, disall, cands, ipos)

c*ver version 2.1, dd. 19-09-93
c*aut Delft Geodetic Computing Centre/LGR, Paul de Jonge
c*aut copyright by Delft University of Technology, Faculty of Geodesy

c*rol finds 'MaxCan' integer vectors whose distance to the the real
c*rol vector 'a' is minimal in the metric of Q=transpose(L) D L. Only
c*rol integer vectors with a distance less than sqrt(Chic) are regarded.

c     name   i/o      parameter description
c     ------ -------- --------------------------------------------------
c*par Chic   input    Chi squared
c*par MaxCan input    number of minimum integer vectors requiered
c*par n      input    dimension of matrix
c*par a      input    the vector with real valued estimates \hat{a} (float
c*                    solution) 
c*par D      input    | diagonal matrix           -1        *
c*par L      input    | lower triangular matrix: Q   = L D L
c*                    | although L is lower triangular, in this example
c*                    | program L is stored column-wise in a 2-dimensional 
c*                    | array, to avoid the necessity of a dedicated 
c*                    | storage scheme. 
c*par lef    work     dp work vector with length = n
c*par left   work     dp work vector with length = n+1
c*par right  work     dp work vector with length = n+1
c*par dist   work     difference between the integer tried and \hat{a}_i,
c*par                 length = n
c*par end    work     dp work vector with length = n
c*par dq     work     dp work vector with length = n
c*par ncan   output   number of integer vectors found
c*par cands  output   | 2-dimensional array to store the candidates
c*par disall output   | according squared norms \hat{a}-\check{a}
c*par ipos   output   column number in 'cands' where the candidate 
c*                    belonging to the minimum distance is stored

      implicit double precision (a-h, o-z)

      double precision
     +  L (n,n), D (n), a (n), lef (n), left (n+1), right (n+1), 
     +  dist (n), end (n), dq (n), disall (MaxCan), cands (n,MaxCan)
      logical 
     +  ende

      if (MaxCan.lt.1) then
         stop 'ERROR in FI71: number of requested candidates < 1'
      else if (n.lt.2) then                             
         stop 'ERROR in FI71: dimension of system < 2'
      endif
      
      ende=.false.
      right (n+1) = Chic
      left (n+1) = 0d0
      do i = 1, n-1
         dq(i) = D(i+1) / D(i)                !  d_i^2/d_i-1^2
      end do
      dq (n) = 1d0 / D(n)

      ncan = 0
      i = n+1
      iold = i

10    continue
         i = i-1                              ! go a level deeper
         if (iold .le. i) then                ! we were here before
            lef(i) = lef(i) + L(i+1,i)        ! only one dist is one bigger
         else
            lef(i) = DINKi (n, L, dist, i)
         endif
         iold = i                             ! keep track
         
         right(i) = (right(i+1)-left(i+1)) * dq(i)
         reach = Sqrt (right(i)) 
         
c*       delta=a(i)-reach-lef(i) is the left border 
c*       JNT2 (delta) is the left most integer in the interval
c*       dist(i) is the distance of this left most integer to the a_hat

         delta = a(i) - reach - lef(i)
         dist(i) = JNT2 (delta) - a(i)  

         if (dist(i) .gt. reach-lef(i)) then  ! there is nothing at this level
            call BACKTs (n, i, end, dist, lef, left, ende) ! so, ... back track
         else
            end(i) = reach - lef(i) - 1d0     ! set the right 'border'
            left(i) = (dist(i)+lef(i))**2
         endif
         
         if (i .eq. 1) then
            call COLLECTs (n, MaxCan, D(1), lef(1), left(1), right(1), 
     +        Chic, dist, end(1), ncan, disall, cands, tmax, imax)
            call BACKTs (n, i, end, dist, lef, left, ende)
         endif
      if (.not. ende) goto 10

c*    the candidate vectors are computed
      dminn=1d20
      do i = 1, MIN (ncan, maxcan)
         if (disall(i).lt.dminn) then
            dminn=disall(i)
            ipos=i
         endif
         do j = 1, n
            cands(j,i) = cands(j,i) + a(j)
         end do
      end do 

      return
      end
