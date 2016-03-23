*
*  subroutine coeff2
*      
      subroutine coeff2 (time,xrcv,crcv,xtrs,vtrs,ctrs,elv,iPRN,
     &                   irec,A)
c
c=======================================================================
c     ****f* SmartPPP/coeff2.f
c
c   FUNCTION   
c   
*     coeff2 of the observation equation
c
c   INPUTS
c
c     xrcv        real*8           coordinates
c     xtrs        real*8           coordinates
c
c   OUTPUT
c
c     A           real*8           coefficents of the observ equation
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c   REVISION
c
c     2009/08/03                   build
c
c     ***
c
C     $Id: coeff2.f.f,v 1.0 2009/07/28 $
c=======================================================================
c
      implicit none
c
c     include
c
      include      '../../include/rinex.h'
      include      '../../include/rinex.conf.h'
      include      '../../include/SmartPPP.h'
      include      '../../include/SmartPPP.conf.h'
c
c     input/output
c
      real*8        time

      real*8        xrcv(3),crcv
      real*8        xtrs(3),vtrs(3),ctrs
      real*8        elv
c
      integer       irec
      integer       iPRN
c
c     local
c
      real*8        A(MAX_SAT_NUM,6)
      real*8        dis
      real*8        drou_dx,drou_dy,drou_dz,drou_dT,drou_dN,drou_dTrop
c
c     loop
c
      integer       i,j,k
      integer       iobs
      integer       ivar
c
c     distance from receiver to transmitter
c
      call distance(xrcv,xtrs,dis)
c
c     partial derivative with respect to the receiver
c
      drou_dx    = -(xtrs(1)-xrcv(1))/dis
      drou_dy    = -(xtrs(2)-xrcv(2))/dis
      drou_dz    = -(xtrs(3)-xrcv(3))/dis
c
c     partial derivative with respect to the receiver clock
c
      drou_dT    = 1
c
c     partial derivative with respect to the wet tropspheric
c
      drou_dTrop = 1
c
c     partial derivative with respect to ambiguity parameters
c
      drou_dN    = 1
c
c     coefficent
c     ==========
c     SPACEBORNE
c
      if(    MARKER_TYPE.EQ.'SPACEBORNE')then
c
         if(observ_model.EQ.1)then
c
c++         partial derivatives for P3
c
            iobs         = (irec-1)*2 + 1
c
c           dx, dy, dz
            A(iobs,1)    = drou_dx
            A(iobs,2)    = drou_dy
            A(iobs,3)    = drou_dz
c
c           dT
            A(iobs,4)    = drou_dT
c
c++         partial derivatives for L3
c
            iobs         =(irec-1)*2 + 2
c
c           dx, dy, dz   
            A(iobs,1)    = drou_dx
            A(iobs,2)    = drou_dy
            A(iobs,3)    = drou_dz
c
c           dT
            A(iobs,4)    = drou_dT
c
c           dN
            ivar         = iPRN + 4
            A(iobs,ivar) = drou_dN
c
         elseif(observ_model.EQ.2)then
c
c++         partial derivatives for L3
c
            iobs         = irec
c
c           dx, dy, dz   
            A(iobs,1)    = drou_dx
            A(iobs,2)    = drou_dy
            A(iobs,3)    = drou_dz
c
c           dT
            A(iobs,4)    = drou_dT
c
c           dN
            ivar         = iPRN + 4
            A(iobs,ivar) = drou_dN
c
         else
c
            write(*,*) 'SmartPPP/coeff2'
            write(*,*) 'do not support this observ_model'
            stop
c
         endif
c
c     GEODETIC
      elseif(MARKER_TYPE.EQ.'GEODETIC')then
c
         if(observ_model.EQ.1)then
c
c++         partial derivatives for P3
c
            iobs         = (irec-1)*2 + 1
c
c           dx, dy, dz
            A(iobs,1)    = drou_dx
            A(iobs,2)    = drou_dy
            A(iobs,3)    = drou_dz
c
c           dT
            A(iobs,4)    = drou_dT
c
c++         partial derivatives for L3
c
            iobs         =(irec-1)*2 + 2
c
c           dx, dy, dz   
            A(iobs,1)    = drou_dx
            A(iobs,2)    = drou_dy
            A(iobs,3)    = drou_dz
c
c           dT
            A(iobs,4)    = drou_dT
c
c           dN
            ivar         = iPRN + 4
            A(iobs,ivar) = drou_dN
c
         elseif(observ_model.EQ.2)then
c
c++         partial derivatives for L3
c
            iobs         = irec
c
c           dx, dy, dz   
            A(iobs,1)    = drou_dx
            A(iobs,2)    = drou_dy
            A(iobs,3)    = drou_dz
c
c           dT
            A(iobs,4)    = drou_dT
c
c           dN
            ivar         = iPRN + 4
            A(iobs,ivar) = drou_dN
c
         else
c
            write(*,*) 'SmartPPP/coeff2'
            write(*,*) 'do not support this observ_model'
            stop
c
         endif
c
      else
c
         write(*,*) 'SmartPPP/coeff2'
         write(*,*) 'Please make modification for tropospheric deriv'
c
      endif
c
      return
c
      end
