      subroutine CHIstrt4 (n, D, L, dist, e, a, tm, t)

c*ver version 2, dd. 09-10-95
c*aut Delft Geodetic Computing Centre/LGR, Paul de Jonge
c*aut copyright by Delft University of Technology, Faculty of Geodesy

c*rol computes squared distance of partially rounded float vectors to
c*rol the float vector in the metric of the variance-covariance
c*rol matrix (see `The LAMBDA method for integer ambiguity estimation: 
c*rol implementation aspects', 5.8: The volume of the ellipsoidal
c*rol region.

c     name   i/o      parameter description
c     ------ -------- --------------------------------------------------
c*par n      input    number of ambiguities 
c*                                                -1        *
c*par L      input    | lower triangular matrix: Q   = L D L
c*                    | although L is lower triangular, in this example
c*                    | program L is stored column-wise in a 2-dimensional 
c*                    | array, to avoid the necessity of a dedicated 
c*                    | storage scheme. 
c*par D      input    | diagonal matrix
c*par dist   work     double precision vector of length n
c*par e      work     double precision vector of length n
c*par a      input    float solution
c*par tm     output   tm(1) is smallest norm, tm(2) one-but-smallest
c*par t      work     double precision vector of length n 

      implicit double precision (a-h, o-z)

      double precision 
     +  D (n), L (n ,n), dist (n), a (n), t (n), tm (2), e (n), nabla
      data
     +  big /1d10/

c*    a-\hat{a} with a the nearest integer to \hat{a}
      do i=1,n
         dist(i)=nint(a(i))-a(i)
      end do 
 
c*    sum_j=i+1^n l_ji (a_i-\hat{a}_i)
      do i=1,n
         e(i)=0d0
         do j=i,n
            e(i)=e(i)+L(j,i)*dist(j)
         end do
      end do

c*    compute t_0 and partial norms
      t_0=0d0
      do i=1,n
         t_0=t_0+d(i)*e(i)*e(i)
      end do

      tm(1)=t_0
      tm(2)=big
c*    for 1 to n take the second nearest integer to a_i
      do i=1,n
         if (dist(i).lt.0d0) then
            nabla=1d0 
            t(i)=t_0
            do j=1,i
               t(i)=t(i) + d(j) * L(i,j) * (2*e(j)+L(i,j))
            end do
         else 
            nabla=-1d0
            t(i)=t_0
            do j=1,i
               t(i)=t(i) - d(j) * L(i,j) * (2*e(j)-L(i,j))
            end do
         endif

c*       and compute the squared norm
c        t(i)=t_0
c        do j=1,i
c           t(i)=t(i)+d(j) * L(i,j)*nabla * (2*e(j)+L(i,j)*nabla)
c        end do

c*       find the second smallest squared norm
         if (t(i).lt.tm(1)) then
            tm(2)=tm(1)
            tm(1)=t(i)
         else if (t(i).lt.tm(2)) then
            tm(2)=t(i)
         endif
      end do

      return
      end
