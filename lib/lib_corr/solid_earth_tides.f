c
c  subroutine solid_earth_tides.f
c
      subroutine solid_earth_tides(Time,xrcv,dxtide)
c
c=======================================================================
c     ****f* lib_corr/solid_earth_tides.f
c
c   FUNCTION   
c   
c     compute the solid earth tides.   
c
c   INPUTS
c
c     Time              real*8      input time in GPS seconds past J2000.0
c     xrcv(3)           real*8      receiver parameter
c
c   OUTPUT
c
c     dxtide(3)         real*8      solid earth tides value in xyz [m]
c
c   COPYRIGHT
c
c     Copyright(c) 2006-            Shoujian Zhang,
c                                   School of Geodesy and Geomatics,
c                                   Wuhan University.
c   REVISION
c
c     2010/01/23                    programmed
c
c     ***
c
C     $Id: solid_earth_tides.f,v 1.0 2010/01/23 $
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
c     external function
      real*8        sec2jd
c
c     input/output
      real*8        time, xrcv(3)
c
c     local
      integer       iyr,imonth,iday,ihour,iminute,isecond
      integer       i,j,k
      integer       ntarg,ncent ! for position of moon and sun 
c
c     sun and moon's position
      real*8        time_JD
      real*8        sun_state_icrs(6)
      real*8        sun_state_itrs(6)
      real*8        mon_state_icrs(6)
      real*8        mon_state_itrs(6)
c
      real*8        XSTA(3), FHR, XSUN(3),XMON(3),DXTIDE(3),XCORSTA(3)
      real*8        frac
c
c     Julian date
      time_JD = sec2jd(time)
c
      ncent = 3     
      ntarg = 11
c
      do i=1,6
         sun_state_icrs(i) = 0.0d0
         sun_state_itrs(i) = 0.0d0
      enddo
c
c     sun's position in icrs
      call planet_state(time_JD,ntarg,ncent,sun_state_icrs)
c
c     sun's position in itrs
      call ICRS2ITRS(time,sun_state_icrs,sun_state_itrs)
c
c*********************************************************
c
      ncent = 3     
      ntarg = 10
c
      do i=1,6
         mon_state_icrs(i) = 0.0d0
         mon_state_itrs(i) = 0.0d0
      enddo
c
c     moon's position in icrs
      call planet_state(time_JD,ntarg,ncent,mon_state_icrs)
c
c     moon's position in itrs
      call ICRS2ITRS(time,mon_state_icrs,mon_state_itrs)
c
c*********************************************************
c     sun's position
      XSUN(1) = sun_state_itrs(1)*10**3
      XSUN(2) = sun_state_itrs(2)*10**3
      XSUN(3) = sun_state_itrs(3)*10**3
C
C     moon's position
      XMON(1) = mon_state_itrs(1)*10**3 
      XMON(2) = mon_state_itrs(2)*10**3
      XMON(3) = mon_state_itrs(3)*10**3
c
c     station's position
      xsta(1) = xrcv(1)
      xsta(2) = xrcv(2)
      xsta(3) = xrcv(3)
c
      call sec2cal(time,iyr,imonth,iday,ihour,iminute,isecond,frac)
c
      FHR = ihour + iminute/60.0d0 + isecond/3600.0d0 + frac/3600.0d0
c
C     INPUT  :  XSTA(I),I=1,2,3: GEOCENTRIC POSITION OF THE STATION (ITRF,
C                                CO-ROTATING FRAME) -- UNITS = METERS
C               XSUN(I),I=1,2,3: GEOC. POSITION OF THE SUN (ECEF FRAME) --
C                                UNITS = METERS
C               XMON(I),I=1,2,3: GEOC. POSITION OF THE MOON (ECEF FRAME) --
C                                UNITS = METERS
C               IYR            : YEAR (UTC TIMESCALE)
C               IMONTH         : MONTH (UTC TIMESCALE)
C               IDAY           : DAY (UTC TIMESCALE)
C               FHR=hr+zmin/60.+sec/3600. : HR IN THE DAY 
C     OUTPUT :  DXTIDE(I),I=1,2,3: DISPLACEMENT VECTOR (GEOCENTRIC ITRF FRAME) --
C                                  UNITS = METERS
c
c*********************************************************
c
      CALL TIDE(XSTA,iYR,iMONTH,iDAY,FHR,XSUN,XMON,DXTIDE)
c
      return
c
      end
