c   
c   subroutine trop_hydro_zpd   
c   
      subroutine trop_hydro_zpd(site_xyz,hydro_zpd)
c
c=======================================================================
c     ****f* lib_corr/trop_hydro_zpd.f
c
c   FUNCTION   
c
c     compute the hydrostatic tropospheric zenith path delay (zpd) 
c     using saastamoinen model
c
c   INPUTS
c
c     site_xyz(3)   real*8          site coordinate in ITRF (unit:m)
c
c   OUTPUT
c
c     hydro_zpd     real*8          zenith path delay of hydrostatic[m]
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
C     $Id: trop_hydro_zpd.f,v 1.0 2010/01/23 $
c=======================================================================
c
      implicit none
c
      real*8    ffun      
c
c..   inputs
      real*8    site_xyz(3)
      real*8    pi
c
c     outputs
      real*8    hydro_zpd
c
c     local
      real*8    site_geod(3)
      real*8    site_lat, site_lon,site_ht
      real*8    P, T, E
      logical   debug
c
c     standard reference atmospheric parameters      
      real*8    P0, E0, T0
c
      parameter(P0=1013.25)  !unit:[mbar]
      parameter(E0=11.691 )  !unit:[mbar]
      parameter(T0=288.15 )  !unit:[kelvin] 
      parameter(pi=3.1415926535897932d0) 
c
c     compute the latitude,longitude and height for site
      call xyz2geod(site_xyz,site_geod)
c
      site_lat = site_geod(1) ![rad]
      site_lon = site_geod(2) ![rad]
      site_ht  = site_geod(3) ![m]
c
      debug = .false.
      if(debug)then
         site_lat = 0.731772172731938d0
         site_ht  = 98.75250704595179d0
      endif
c
c     write(*,*)'lat,lon,ht' 
c     write(*,*) site_xyz(1),site_xyz(2),site_xyz(3)
c     write(*,*) site_lat,site_lon, site_ht
c
c     Reference:
c     Chinese Astronomy and Astrophysics 32(2008) 429–438
c     Evaluation of the Precision of Three Tropospheric Delay Correction Models.
c
c     ALSO can reference the CHINESE EDITION 
c
c     calculate the site atmospheric parameters, used in saastamoinen model
c     ground atmospheric pressure (mbar), 
c     ground temperature (deg C), 
c     ground water vapor pressure (mbar), 
c
      P = P0*(1-0.0068D0/T0*site_ht)**5
      T = T0-0.0068*site_ht
      if(site_ht.lt.11000.0d0)then
      E = E0*(1-0.0068D0/T0*site_ht)**4
      else
      E = 0.0d0
      endif
c     write(*,*) 'P,T,E'
c     write(*,*)  P,T,E
c
c     the troposphere is below 50 km height.
      if(site_ht.lt.50000.0d0)then
c..      calculate the hydrostatic delay using saastamoinen model
         hydro_zpd = 0.2277D-02 * (P + (0.1255D+04/T+0.05D0)*E)
     &                          /  ffun(site_lat,site_ht)
      else
         hydro_zpd = 0.0d0
      endif
c
c..   calculate the hydrostatic delay using hopfield     model
c
      return
c
      end
