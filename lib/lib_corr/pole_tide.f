c
c   subroutine pole_tide
c
      subroutine pole_tide(SEC_GPS,site_xyz,delta)

c   Subroutine to compute the pole tide component
c
c   code based on IERS Technical Note 3
c   written by peterm 1-oct-93
c
c   INPUT PARAMETERS (NONE CHANGED)
c   *******************************  
c
c   lmptid        logical = .true. if mean tide of 2000 to be removed
c   SEC_GPS       input time in GPS time system.
c   site_xyz      site position [XYZ in ITRF]
c
c   OUTPUT PARAMETERS (NONE DEFINED ON INPUT)
c   *****************************************
c
c   delta         the X,Y,Z Cartesian displacements due to the pole tide.
c                 The units are transformed to metersin the terrestrial frame.
c                 
c   NOTES         THE IERS Tech. NOTES unit is millimeters.                 
c
c
      implicit none
      integer       IMAX
      parameter(    IMAX = 10000)
c             
c     INPUTS
      logical       lmptid
      real*8        SEC_GPS
      real*8        site_xyz(3)
c
c     OUTPUT
      real*8        delta(3)
c
      real *8       r(3,3), s(3), pi
      real*8        xp, yp, t, fract, along, colat, jdr, dt
c
      real*8        site_geod(3)
c 
      REAL*8        Sec_UTC
      REAL*8        MJD_UTC
c
      real*8        MJD(IMAX), X(IMAX), Y(IMAX), UT12UTC(IMAX)
      real*8        MJD_Int,   x_int,   y_int,   ut12utc_int
C      
      integer       Year, Month, Day, Hour, Minu, Second
      integer       i
***
      character*100 POM_file
      character*100 installpath
c
c     External Function
      real*8        GPS2UTC, sec2MJD
c
c  Common
c
      integer       NPOM
      common /polar_motion/   MJD, x, y, ut12utc, NPOM
c
c     !!! should change the default value in the configure file
c
      lmptid = .false.
c
      pi = 4.0d0*datan(1.d0)
c
c  Transform Second to MJD
c
      Sec_UTC = GPS2UTC(Sec_GPS)
      Mjd_UTC = sec2MJD(Sec_UTC)
      Mjd_Int = Mjd_UTC
c
c  Obtain the x, y, ut12utc at the given time MJD
c
c     Load Polar Motion file
c
      call getenv('HOPES_HOME',installpath)
c
      POM_file = 
     &trim(installpath)//trim('share/tables/polar_motion_IAU2000.txt')
c
      call load_POM(POM_file)
c  
      if(Mjd_Int.lt.Mjd(1).or.Mjd_Int.gt.Mjd(NPOM))then
         write(*,*) 'lib_coordsys/get_c2t.f'
         write(*,*) 'range overflow in polar_motion.txt '
         write(*,*) 'please extend the file'
         stop
      endif
c
c     Read the pole-position table and compute pole position
c     in arc-seconds.
c
      call interp_POM(Mjd,     x,     y,     UT12UTC,   NPOM, 
     +                Mjd_int, x_int, y_int, UT12UTC_int     )
c
      xp = x_int
      yp = y_int
c
c     Reomve the mean pole position for 2000.0

      if(lmptid) then   
         dt  = (Mjd_Int+2400000.5d0-2451545.0d0)/365.25d0   ! time in years
*        Linear trend from IERS Conventions 2000.
         xp  = xp  - (0.054d0+0.00083d0*dt)
         yp  = yp  - (0.357d0+0.00395d0*dt) 
      endif 
c
c     NOTE the distinction between geodetic and geocentric
c     latitude has not been maintained. The error is less
c     than 0.1 mm.
c
      call xyz2geod(site_xyz,site_geod)
c
c     unit:[rad]
      colat = pi/2.0d0 - site_geod(1)

c     reassign the longitude[rad]
      along = site_geod(2)

c     compute the rotation matrix in its transposed position
      r(1,1) =  dcos(colat)*dcos(along)
      r(2,1) =  dcos(colat)*dsin(along)
      r(3,1) = -dsin(colat)
      r(1,2) = -dsin(along)
      r(2,2) =  dcos(along)
      r(3,2) =  0.0d0
      r(1,3) =  dsin(colat)*dcos(along)
      r(2,3) =  dsin(colat)*dsin(along)
      r(3,3) =  dcos(colat)
c
c     compute the s vector. caution parameters are in millimeters and
c     arcseconds.
c**   pjm code
c     s(1)  = -32.d0*dsin(2.d0*colat)*(xp*dcos(along) - yp*dsin(along))
c     s(2)  =  -9.d0*dcos(2.d0*colat)*(xp*dcos(along) - yp*dsin(along))
c     s(3)  =   9.d0*dcos(colat)     *(xp*dsin(along) + yp*dcos(along))
c**   end pjm code

c**   Dong correction -- assumes S, E, U; rotation matrix tranforms correctly to X Y Z 
      s(1)  =  -9.d0*dcos(2.d0*colat)*(xp*dcos(along) - yp*dsin(along))
      s(2)  =   9.d0*dcos(colat)     *(xp*dsin(along) + yp*dcos(along))
      s(3)  = -32.d0*dsin(2.d0*colat)*(xp*dcos(along) - yp*dsin(along)) 
c
      call mtxmul (r,s,delta,3,3,1)
c
c     Transform the unit: from millimeters to meters
c
      do i=1,3
         delta(i) = delta(i)/1000.0d0
      enddo
c
c     the array delta contains the pole tide corrections
c     values are in the terrestrial system in cartesian coordinates
c
c     write (iscrn, 10) (delta(i),i=1,3)
c10   format (1x, 'The pole tide components are (mm): ',3f9.3 )
c
      return
c
      end
