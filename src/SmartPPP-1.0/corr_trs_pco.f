c
c subroutine corr_trs_pco.f
c
      subroutine corr_trs_pco(Time,iPRN,xtrs)
c
c=======================================================================
c     ****f* SmartPPP/corr_trs_pco.f
c
c   FUNCTION   
c   
c     transmitter phase center offset for GPS satellites        
c
c   INPUTS
c
c     time           real*8        second past J2000.0 in GPS
c     iPRN           integer       PRN number
c
c   OUTPUT
c
c     xtrs           real*8        transmitter position
c
c   COPYRIGHT
c
c     Copyright(c) 2006-           Shoujian Zhang,
c                                  School of Geodesy and Geomatics,
c                                  Wuhan University.
c   REVISION
c
c     2009/08/03                   programmed
c
c     ***
c
C     $Id: corr_trs_pco.f.f,v 1.0 2009/08/02 $
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
      include      '../../include/igs.h'
      include      '../../include/atx.h'
c
      real*8        sec2jd
c
c     input/output variable
c
c     input
      real*8        Time
      integer       iPRN
c
c     output
      real*8        xtrs(3)
c
c     local variables
c
      integer       i,j,k
      real*8        ANT_PCO1(3)
c
c     Sun Position
      real*8        time_JD
      real*8        sun_state_icrs(6)
      real*8        sun_state_itrs(6)
c
      integer       ntarg, ncent
c
      real*8        x,y,z,x_sun,y_sun,z_sun
      real*8        r_sat_sun,r_sat_earth
      real*8        s
      real*8        ex(3),ey(3),ez(3)
c
      real*8        GPS_PCO(3)
c
c     common
c     ******
      real*8        rcv_pco  (MAX_FREQ,3)
      real*8        dpco     (3)
      real*8        xZEN     (MAX_REC_ZEN)
      real*8        yant_pcv1(MAX_FREQ,MAX_REC_ZEN)
      real*8        yant_pcv2(MAX_FREQ,MAX_REC_AZI,MAX_REC_ZEN)
      integer       nrec_zen
c
      real*8        trs_pco  (MAX_PRN,3)
c*
      common /atx/  rcv_pco,dpco,xzen,yant_pcv1,
     &                                yant_pcv2,nrec_zen,trs_pco
c
c     save!!!
c
      real*8        time_fore
c
      save          time_fore
      save          sun_state_itrs
c
      data          time_fore  /0.0d0/
      data          sun_state_itrs   /0.0, 0.0, 0.0, 0.0, 0.0, 0.0/
c
      do i=1,3
         GPS_PCO(i) = 0.0d0
      enddo
c
      ncent = 3     
      ntarg = 11
c
      do i=1,6
      sun_state_icrs(i) = 0.0d0
      enddo
c
      if(dabs(time-time_fore).GE.60.0d0)then
c
         time_JD = sec2jd(time)
c
c        sun's state vector in ICRS
c
         call planet_state(time_JD,ntarg,ncent,sun_state_icrs)
c
c        sun's state vector in ITRS
c
         call ICRS2ITRS(time,sun_state_icrs,sun_state_itrs)
c
         time_fore = time
c
      endif
c
c     calculate the phase center offset of GPS     
c     ========================================
c
      x     = xtrs(1)
      y     = xtrs(2)
      z     = xtrs(3)
c
      x_sun = sun_state_itrs(1)
      y_sun = sun_state_itrs(2)
      z_sun = sun_state_itrs(3)
c
c     radius from satellite to earth center and to the sun
c
      r_sat_earth = dsqrt(       x **2 +        y **2 +        z **2)
      r_sat_sun   = dsqrt((x_sun-x)**2 + (y_sun-y)**2 + (z_sun-z)**2)
c
      s           = dsqrt((y*z_sun-y_sun*z)**2 + 
     +                    (z*x_sun-z_sun*x)**2 +
     +                    (x*y_sun-x_sun*y)**2 ) 
c
c     error
c
      if(r_sat_earth.eq.0.0d0 )then
         write(*,*) 'eclips ocurred ?'
         return
      endif
c
      if(r_sat_sun  .eq.0.0d0.or.
     +   s          .eq.0.0d0    )then
         write(*,*) 'r_sat_sun', r_sat_sun
         write(*,*) 's', s
         write(*,*) 'SmartPPP/corr_trs_pco'
         write(*,*) 'Divided by zero'
         stop
      endif
c
c     satellite fixed coordinate unit vector
c
      ez(1) = -x/r_sat_earth
      ez(2) = -y/r_sat_earth
      ez(3) = -z/r_sat_earth
c
      ey(1) = -(y*z_sun-y_sun*z)/s
      ey(2) = -(z*x_sun-z_sun*x)/s
      ey(3) = -(x*y_sun-x_sun*y)/s
c
      ex(1) = ((z*x_sun-z_sun*x)*z-(x*y_sun-x_sun*y)*y)/(s*r_sat_earth)
      ex(2) = ((x*y_sun-x_sun*y)*x-(y*z_sun-y_sun*z)*z)/(s*r_sat_earth)
      ex(3) = ((y*z_sun-y_sun*z)*y-(z*x_sun-z_sun*x)*x)/(s*r_sat_earth) 
c
      do i=1,3
c
         xtrs(i) = xtrs(i) + ex(i)*trs_pco(iPRN,1) 
     &                     + ey(i)*trs_pco(iPRN,2)
     &                     + ez(i)*trs_pco(iPRN,3)
c
      enddo
c
      return
c
      end
