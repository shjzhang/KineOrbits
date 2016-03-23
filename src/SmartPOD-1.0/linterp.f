*
*  subroutine linear_interp
*
      subroutine linear_interp(x,y,N,xint,yint)      
c
c=======================================================================
c     ****f* SmartPPP/linear_interp
c
c   FUNCTION   
c   
*     linear interpolation
c
c   INPUTS
c
*     x            real*8       array store the node value
*     y            real*8       array store the funtion value
c     N            integer      dimension of X, and Y
*     xint         real*8       the x-value for which estimate of y is
*                               desired
c
c   OUTPUT
c
*     yint         real*8       interpolate value
c
c   REVISION
c
c     2009/08/01                processing points more than 2
c
c   COPYRIGHT
c
c     Copyright(c) 2006-        Shoujian Zhang,
c                               School of Geodesy and Geomatics,
c                               Wuhan University.
c     ***
c
C     $Id: linear_interp.f,v 1.0 2007/06/01 $
c=======================================================================
*        
      implicit none
*
*     input/output variable
*
      integer       N
      real*8        x(N), y(N)
      real*8        xint, yint
c
c     local
c
      integer       i,j,k
      integer       idx_Min, idx_Mid, idx_Max
      logical       find
c      
      real*8        xx(2), yy(2)
      real*8        ca,    cb
c
      real*8        xint1
c
      find = .false.
c
      call binary_search(xint,x,N,idx_Mid,find)
c
      idx_Min = idx_Mid
c
      if(     idx_Min.lt.1   )then
         idx_Min = 1
      elseif((idx_Min+1).gt.N)then
         idx_Min = N - 1
      endif
c
      do i=1,2
         xx(i) = x(idx_Min+i-1) - x(idx_Min)
         yy(i) = y(idx_Min+i-1)
      enddo
c
      xint1 = xint - x(idx_Min)
c
c     beeline function is y = ca*x + cb
c
      ca = ( yy(2)-yy(1) )/( xx(2)-xx(1) )
      cb = ( yy(1) - ca*xx(1) )
c
      yint    = ca*xint1 + cb
c
      return
c
      end
