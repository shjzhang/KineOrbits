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
c     xrcv        real*8           receiver's coordinates
c     xtrs        real*8           GPS satellites' coordinates
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
      integer       nmax
      parameter    (nmax=MAX_SAT_NUM+5)
c
c     external function
      real*8        sec2mjd
c
c     input/output
      real*8        time
      real*8        xrcv(3),crcv
      real*8        xtrs(3),vtrs(3),ctrs
      real*8        elv
      integer       irec
      integer       iPRN
c
c     local
      real*8        A(MAX_SAT_NUM,nmax)
      real*8        dis
      real*8        drou_dx,drou_dy,drou_dz,drou_dT,drou_dN,drou_dTrop
c
c     loop
      integer       i,j,k
      integer       ii,jj,kk
      integer       iobs
      integer       ivar
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
c     default value
      MF = 'gmf'
c
c     distance from receiver to transmitter
      call distance(xrcv,xtrs,dis)
c
c     Modified Julian Date
      MJD = sec2mjd(time)
c
c     zenith distance in radian
      pi = 3.14159265359d0
      zd = pi/2 - elv
c
c     geodetic coordinate
      call xyz2geod(xrcv,site_geod)
c
      slat = site_geod(1)
      slon = site_geod(2)
      shgt = site_geod(3)
c
c     SPACEBORNE
      if(MARKER_TYPE.EQ.'SPACEBORNE')then
         if(observ_model.EQ.1)then
c        
c++         partial derivatives for P3
            iobs         = (irec-1)*2 + 1
c           dx, dy, dz
            A(iobs,1)    = -(xtrs(1)-xrcv(1))/dis
            A(iobs,2)    = -(xtrs(2)-xrcv(2))/dis
            A(iobs,3)    = -(xtrs(3)-xrcv(3))/dis
c           drou/dT
            A(iobs,4)    = 1
c
c++         partial derivatives for L3
            iobs         =(irec-1)*2 + 2
c           dx, dy, dz   
            A(iobs,1)    = -(xtrs(1)-xrcv(1))/dis
            A(iobs,2)    = -(xtrs(2)-xrcv(2))/dis
            A(iobs,3)    = -(xtrs(3)-xrcv(3))/dis
c           drou/dT
            A(iobs,4)    = 1
c           drou/dN
            ivar         = iPRN + 4
            A(iobs,ivar) = 1
         elseif(observ_model.EQ.2)then
c++         partial derivatives for L3
            iobs         = irec
c           dx, dy, dz   
            A(iobs,1)    = -(xtrs(1)-xrcv(1))/dis
            A(iobs,2)    = -(xtrs(2)-xrcv(2))/dis
            A(iobs,3)    = -(xtrs(3)-xrcv(3))/dis
c           dT
            A(iobs,4)    = 1
c           dN
            ivar         = iPRN + 4
            A(iobs,ivar) = 1
         else
            write(*,*) 'SmartPPP/coeff2'
            write(*,*) 'do not support this observ_model'
            stop
         endif
c
c     GEODETIC
      elseif(MARKER_TYPE.EQ.'GEODETIC')then
         if(observ_model.EQ.1)then
c
c++         partial derivatives for P3
            iobs         = (irec-1)*2 + 1
c           dx, dy, dz
            A(iobs,1)    = -(xtrs(1)-xrcv(1))/dis
            A(iobs,2)    = -(xtrs(2)-xrcv(2))/dis
            A(iobs,3)    = -(xtrs(3)-xrcv(3))/dis
c           drou/dT
            A(iobs,4)    = 1
c           write(*,*) iPRN, (A(iobs,kk),kk=1,nmax)
c
c++         partial derivatives for L3
            iobs         =(irec-1)*2 + 2
c           dx, dy, dz   
            A(iobs,1)    = -(xtrs(1)-xrcv(1))/dis
            A(iobs,2)    = -(xtrs(2)-xrcv(2))/dis
            A(iobs,3)    = -(xtrs(3)-xrcv(3))/dis
c           drou/dT
            A(iobs,4)    = 1
c
c*          drou/dtrop
c           Global Mapping Function
            if(    trim(MF).EQ.'gmf')then
                call gmf(mjd,slat,slon,shgt,zd,gmfh,gmfw)
            elseif(trim(MF).EQ.'vmf1')then
                write(*,*) 'Not support the vmf1 now'
            endif
            A(iobs,5)    = gmfw
c           drou/dN
            ivar         = iPRN + 5
            A(iobs,ivar) = 1
c           write(*,*) iPRN, (A(iobs,kk),kk=1,nmax)
         elseif(observ_model.EQ.2)then
c
c++         partial derivatives for L3
            iobs         = irec
c           dx, dy, dz   
            A(iobs,1)    = -(xtrs(1)-xrcv(1))/dis
            A(iobs,2)    = -(xtrs(2)-xrcv(2))/dis
            A(iobs,3)    = -(xtrs(3)-xrcv(3))/dis
c           dT
            A(iobs,4)    = 1
c
c*          drou/dtrop
c           Global Mapping Function
            if(    trim(MF).EQ.'gmf')then
                call gmf(mjd,slat,slon,shgt,zd,gmfh,gmfw)
            elseif(trim(MF).EQ.'vmf1')then
                write(*,*) 'Not support the vmf1 now'
            endif
            A(iobs,5)    = gmfw
c           dN
            ivar         = iPRN + 5
            A(iobs,ivar) = 1
         else
            write(*,*) 'SmartPPP/coeff2'
            write(*,*) 'do not support this observ_model'
            stop
         endif
      else
         write(*,*) 'SmartPPP/coeff2'
         write(*,*) 'Please make modification for tropospheric deriv'
      endif
c
      return
c
      end
