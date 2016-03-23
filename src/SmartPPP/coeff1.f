*
*  subroutine coeff1
*      
      subroutine coeff1(xrcv,xtrs,iobs,ndim,A,elv,time)
c
c=======================================================================
c     ****f* SmartPPP/coeff1.f
c
c   FUNCTION   
c   
*     coeff1 of the observation equation
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
C     $Id: coeff1.f.f,v 1.0 2009/07/28 $
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
c     function
      real*8        sec2mjd
c
c     input/output
c
      real*8        xrcv(3), xtrs(3)
      real*8        elv
      real*8        time
      integer       iobs, ndim
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
c
      real*8        pi
      character*10  MF  ! Mapping Fucntion Type
      real*8        MJD ! Modified Julian Date
c
      real*8        zd  ! zenith distance in radian
      real*8        site_geod(3)
      real*8        slat, slon, shgt
      real*8        gmfh, gmfw
c
c     distance from receiver to transmitter
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
c     default value
      MF = 'gmf'
c     Modified Julian Date
      MJD = sec2mjd(time)
c     zenith distance in radian
      pi = 3.14159265359d0
      zd = pi/2 - elv
c     geodetic coordinate
      call xyz2geod(xrcv,site_geod)
      slat = site_geod(1)
      slon = site_geod(2)
      shgt = site_geod(3)
c     Global Mapping Function
      if(    trim(MF).EQ.'gmf')then
        call gmf(mjd,slat,slon,shgt,zd,gmfh,gmfw)
      elseif(trim(MF).EQ.'vmf1')then
        write(*,*) 'Not support the vmf1 now'
      endif
      drou_dTrop = gmfw
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
c        dx, dy, dz
         A(iobs,1) = drou_dx
         A(iobs,2) = drou_dy
         A(iobs,3) = drou_dz
c        dT
         A(iobs,4) = drou_dT
c
c     GEODETIC
      elseif(MARKER_TYPE.EQ.'GEODETIC')then
c        dx,dy,dz
         A(iobs,1) = drou_dx
         A(iobs,2) = drou_dy
         A(iobs,3) = drou_dz
c        dT
         A(iobs,4) = drou_dT
c        dtrop
         A(iobs,5) = drou_dTrop
c        write(*,*) (A(iobs,k),k=1,5)
      else
          write(*,*) 'not supported Marker Type'
          stop
      endif
c
      return
c
      end
