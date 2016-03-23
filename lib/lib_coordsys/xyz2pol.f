*
*  procedure xyz2pol
*   
      subroutine xyz2pol(xyz, pol)
*********1*********2*********3*********4*********5*********6*********7**
*      
*  Purpose
*  =======
*
*  Given a point with (X, Y, Z) coordinate in cartesian coordinate,
*  the polar coordinate (range, azimuth, elevation) is returned.
*
*
*  Auguments
*  =========
*
*  Name         Type    I/O     Descriptin
*  ----         ----    ---     ---------------------------------------
*  xyz(3)       REAL*8  I       XYZ coordinates (m)
*  pol(3)       REAL*8  I       polar coordinates 
*
*
*  Algorithm
*  =========
*
*  suppose the coordinate of point is (X,Y,Z), the polar coordinate of
*  the point is (range, azimuth, elevation) can be calculated by:
*
*  range     = sqrt(X^2+Y^2+Z^2)
*  azimuth   = arc tan(Y/X)
*  elevation = arc tan(Z/(X^2+Y^2)^(1/2))
*
*  Notes!!!
*
*  the azimuth and elevation is return with unit (radian)
*
*  History:
*  ========
*
*  Vesion 1.0
*  ==========
*    
*  Time         Author          Description
*  ----         ------          ----------------------------------------
*  08.09.18     S.J.Zhang       build this subroutine    
*
*********1*********2*********3*********4*********5*********6*********7**
c
      IMPLICIT NONE
c
c     parameter
c
      real*8    PI
      parameter(PI=3.1415926535897932D0)
c
c     Input/Output variables
c
      real*8    XYZ(3), pol(3)
c
c     Local variables
c
      real*8    X, Y, Z
      real*8    R, A, E
c
      X = XYZ(1)
      Y = XYZ(2)
      Z = XYZ(3)
c
c     range from the origin to the point(X,Y,Z)
c
      R = dsqrt(X**2+Y**2+Z**2)
c
c     azimuth (range from 0 to 2*PI)
c
      A = atan2(Y,X)
c
      if(A.LT.0.0d0)then
         A = A + 2*PI
      endif
c
c     elevation(can be negative)
c
      E = atan2(Z,sqrt(X**2+Y**2))
c
      pol(1) = R
      pol(2) = A
      pol(3) = E
c
      return
c
      end
